// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::Write as _;

use crate::compiler::*;
use crate::definitions::*;
use crate::graph::{EdgeId, NodeId};
use crate::handles::HandleVec;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Register {
    Zero,
    ProgramCounter,
    ProcedureStart,
    InputOffset,
    HighlightStart,
    HighlightKind,
}

impl Register {
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::ProgramCounter => "pc",
            Register::ProcedureStart => "ps",
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::HighlightKind => "hk",
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct Registers {
    pub zero: u32, // Zero
    pub pc: u32,   // ProgramCounter
    pub ps: u32,   // ProcedureStart
    pub off: u32,  // InputOffset
    pub hs: u32,   // HighlightStart
    pub hk: u32,   // HighlightKind
}

#[derive(Debug, Clone, Copy)]
enum RelocationTarget {
    State(NodeId),
    Transition(EdgeId),
}

// General encoding:
// * Instructions are 32 bit
// * The opcode is 4 bits at opcode[3:0]
// * Register constants are 4 bits
// *
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Instruction {
    // Encoding:
    //   imm[31:12] | src[11:8] | dst[7:4] | 0000
    //
    // NOTE: This allows for jumps as well by manipulating REG_PROGRAM_COUNTER.
    Add { dst: Register, src: Register, imm: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0001
    //
    // NOTE: The VM takes care of saving the return address.
    Call { dst: usize },

    // Encoding:
    //                                       0010
    Return,

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0011
    //
    // Jumps to `dst` if the test fails.
    JumpIfNotMatchCharset { idx: CharsetHandle, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0100
    JumpIfNotMatchPrefix { idx: StringHandle, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0101
    JumpIfNotMatchPrefixInsensitive { idx: StringHandle, dst: usize },

    // Encoding:
    //                                       0110
    //
    // Flushes the current HighlightKind to the output.
    FlushHighlight,

    // Encoding:
    //                                       0111
    //
    // Checks if we're at the end and exit if so.
    SuspendOpportunity,
}

impl Instruction {
    const IMM_MAX: usize = (1 << 20) - 1;

    #[allow(clippy::identity_op)]
    pub fn encode(&self) -> u32 {
        match *self {
            Instruction::Add { dst, src, imm } => {
                Self::cast_imm(imm) | (src as u32) << 8 | (dst as u32) << 4 | 0b0000
            }
            Instruction::Call { dst } => Self::cast_imm(dst) | 0b0001,
            Instruction::Return => 0b0010,
            Instruction::JumpIfNotMatchCharset { idx, dst } => {
                Self::cast_imm(dst) | (idx.0 as u32) << 4 | 0b0011
            }
            Instruction::JumpIfNotMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | (idx.0 as u32) << 4 | 0b0100
            }
            Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst } => {
                Self::cast_imm(dst) | (idx.0 as u32) << 4 | 0b0101
            }
            Instruction::FlushHighlight => 0b0110,
            Instruction::SuspendOpportunity => 0b0111,
        }
    }

    pub fn mnemonic(&self) -> String {
        match *self {
            Instruction::Add { dst, src, imm } => {
                let mut str = String::with_capacity(48);
                _ = write!(str, "add   {}, {}, ", dst.mnemonic(), src.mnemonic());
                if imm > 1024 * 1024 {
                    _ = write!(str, "{:#x}", imm & Self::IMM_MAX);
                } else {
                    _ = write!(str, "{imm}");
                }
                str
            }
            Instruction::Call { dst } => format!("call  {dst}"),
            Instruction::Return => "ret".to_string(),
            Instruction::JumpIfNotMatchCharset { idx, dst } => {
                format!("jnc   {:?}, {dst}", idx.0)
            }
            Instruction::JumpIfNotMatchPrefix { idx, dst } => {
                format!("jnp   {:?}, {dst}", idx.0)
            }
            Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst } => {
                format!("jnpi  {:?}, {dst}", idx.0)
            }
            Instruction::FlushHighlight => "flush".to_string(),
            Instruction::SuspendOpportunity => "susp".to_string(),
        }
    }

    fn cast_imm(imm: usize) -> u32 {
        if imm == usize::MAX {
            (Self::IMM_MAX << 12) as u32
        } else {
            assert!(imm <= Self::IMM_MAX);
            (imm << 12) as u32
        }
    }
}

struct Backend<'a> {
    states: &'a mut HandleVec<NodeId, GraphState>,
    transitions: &'a mut HandleVec<EdgeId, GraphTransition>,
    instructions: Vec<Instruction>,
    relocations: Vec<(usize, RelocationTarget)>,
}

impl Backend<'_> {
    fn compile(mut self) -> Vec<Instruction> {
        for th in self.transitions.indices() {
            let th_next = EdgeId(th.0 + 1);
            let t = self.transitions[th].clone();

            self.transitions[th].instruction_offset = self.instructions.len();
            if self.states[t.src].instruction_offset == usize::MAX {
                self.states[t.src].instruction_offset = self.instructions.len();
            }

            match t.test {
                GraphTest::Chars(0) => {}
                GraphTest::Chars(usize::MAX) => {
                    self.assign(Register::InputOffset, usize::MAX);
                }
                GraphTest::Chars(n) => {
                    self.add_assign(Register::InputOffset, n);
                }
                GraphTest::Charset(h) => {
                    self.jump_if_not_match_charset(h, RelocationTarget::Transition(th_next));
                }
                GraphTest::Prefix(h) => {
                    self.jump_if_not_match_prefix(h, RelocationTarget::Transition(th_next));
                }
                GraphTest::PrefixInsensitive(h) => {
                    self.jump_if_not_match_prefix_insensitive(
                        h,
                        RelocationTarget::Transition(th_next),
                    );
                }
            }

            match t.kind {
                HighlightKindOp::None => {}
                HighlightKindOp::Some(kind) => {
                    self.assign(Register::HighlightKind, kind.as_usize());
                }
            }

            match t.dst {
                GraphAction::Jump(dst) => {
                    self.jump(RelocationTarget::State(dst));
                }
                GraphAction::Push(dst) => {
                    self.flush_highlight();
                    self.call(RelocationTarget::State(dst));
                }
                GraphAction::Pop(0) => {
                    self.flush_highlight();
                    self.suspend_opportunity();
                    self.assign(Register::HighlightKind, HighlightKind::Other.as_usize());
                    self.copy(Register::ProgramCounter, Register::ProcedureStart);
                }
                GraphAction::Pop(1) => {
                    self.flush_highlight();
                    self.ret();
                }
                GraphAction::Loop => {
                    self.suspend_opportunity();
                    self.jump(RelocationTarget::State(t.src));
                }
                _ => unreachable!(),
            }
        }

        for &(off, dst) in &self.relocations {
            let instruction_offset = match dst {
                RelocationTarget::State(h) => self.states[h].instruction_offset,
                RelocationTarget::Transition(h) => self.transitions[h].instruction_offset,
            };
            match &mut self.instructions[off] {
                Instruction::Add { dst: Register::ProgramCounter, src: Register::Zero, imm } => {
                    *imm = instruction_offset;
                }
                Instruction::Call { dst }
                | Instruction::JumpIfNotMatchCharset { dst, .. }
                | Instruction::JumpIfNotMatchPrefix { dst, .. }
                | Instruction::JumpIfNotMatchPrefixInsensitive { dst, .. } => {
                    *dst = instruction_offset;
                }
                i => panic!("Unexpected relocation target: {i:?}"),
            }
        }

        self.instructions
    }

    fn add_assign(&mut self, dst: Register, imm: usize) {
        self.instructions.push(Instruction::Add { dst, src: dst, imm });
    }

    fn assign(&mut self, dst: Register, imm: usize) {
        self.instructions.push(Instruction::Add { dst, src: Register::Zero, imm });
    }

    fn copy(&mut self, dst: Register, src: Register) {
        self.instructions.push(Instruction::Add { dst, src, imm: 0 });
    }

    fn jump(&mut self, dst: RelocationTarget) {
        let dst = self.resolve_relocation(dst);
        self.assign(Register::ProgramCounter, dst);
    }

    fn jump_if_not_match_charset(&mut self, idx: CharsetHandle, dst: RelocationTarget) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchCharset { idx, dst });
    }

    fn jump_if_not_match_prefix(&mut self, idx: StringHandle, dst: RelocationTarget) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchPrefix { idx, dst });
    }

    fn jump_if_not_match_prefix_insensitive(&mut self, idx: StringHandle, dst: RelocationTarget) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst });
    }

    fn call(&mut self, dst: RelocationTarget) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::Call { dst });
    }

    fn ret(&mut self) {
        self.instructions.push(Instruction::Return);
    }

    fn flush_highlight(&mut self) {
        self.instructions.push(Instruction::FlushHighlight);
    }

    fn suspend_opportunity(&mut self) {
        self.instructions.push(Instruction::SuspendOpportunity);
    }

    fn resolve_relocation(&mut self, dst: RelocationTarget) -> usize {
        let instruction_offset = match dst {
            RelocationTarget::State(h) => self.states[h].instruction_offset,
            RelocationTarget::Transition(h) => self.transitions[h].instruction_offset,
        };

        if instruction_offset != usize::MAX {
            instruction_offset
        } else {
            self.relocations.push((self.instructions.len(), dst));
            0
        }
    }
}
