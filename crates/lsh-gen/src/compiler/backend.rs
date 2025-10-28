// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::HashMap;
use std::fmt::Write as _;

use crate::compiler::*;
use crate::definitions::*;

#[derive(Debug, Clone, Copy)]
enum RelocationTarget<'a> {
    Node(*const NodeCell<'a>),
    Edge(*const Edge<'a>),
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
    JumpIfNotMatchCharset { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0100
    JumpIfNotMatchPrefix { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0101
    JumpIfNotMatchPrefixInsensitive { idx: usize, dst: usize },

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
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0011
            }
            Instruction::JumpIfNotMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0100
            }
            Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0101
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
                format!("jnc   {idx:?}, {dst}")
            }
            Instruction::JumpIfNotMatchPrefix { idx, dst } => {
                format!("jnp   {idx:?}, {dst}")
            }
            Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst } => {
                format!("jnpi  {idx:?}, {dst}")
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

pub struct Backend<'a> {
    instructions: Vec<Instruction>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
    relocations: Vec<(usize, RelocationTarget<'a>)>,
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            charsets: Vec::new(),
            strings: Vec::new(),
            relocations: Vec::new(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn charsets(&self) -> &[&'a Charset] {
        &self.charsets
    }

    pub fn strings(&self) -> &[&'a str] {
        &self.strings
    }

    pub fn compile(&mut self, frontend: &Frontend<'a>) {
        let mut offsets = HashMap::new();
        let mut charsets_seen = HashMap::new();
        let mut strings_seen = HashMap::new();

        frontend.visit_graph(|src| {
            let Node::Intra(n) = &*src.borrow() else {
                return;
            };

            offsets.insert(src as *const _ as *const (), self.instructions.len());

            let mut iter = n.edges.iter().peekable();
            while let Some(t) = iter.next() {
                offsets.insert(t as *const _ as *const (), self.instructions.len());

                match t.test {
                    GraphTest::Chars(0) => {}
                    GraphTest::Chars(usize::MAX) => {
                        self.assign(Register::InputOffset, usize::MAX);
                    }
                    GraphTest::Chars(n) => {
                        self.add_assign(Register::InputOffset, n);
                    }
                    GraphTest::Charset(h) => {
                        let h = *charsets_seen.entry(h as *const _).or_insert_with(|| {
                            let idx = self.charsets.len();
                            self.charsets.push(h);
                            idx
                        });
                        self.jump_if_not_match_charset(
                            h,
                            RelocationTarget::Edge(*iter.peek().unwrap() as *const _),
                        );
                    }
                    GraphTest::Prefix(h) => {
                        let h = *strings_seen.entry(h as *const _).or_insert_with(|| {
                            let idx = self.strings.len();
                            self.strings.push(h);
                            idx
                        });
                        self.jump_if_not_match_prefix(
                            h,
                            RelocationTarget::Edge(*iter.peek().unwrap() as *const _),
                        );
                    }
                    GraphTest::PrefixInsensitive(h) => {
                        let h = *strings_seen.entry(h as *const _).or_insert_with(|| {
                            let idx = self.strings.len();
                            self.strings.push(h);
                            idx
                        });
                        self.jump_if_not_match_prefix_insensitive(
                            h,
                            RelocationTarget::Edge(*iter.peek().unwrap() as *const _),
                        );
                    }
                }

                match t.kind {
                    HighlightKindOp::None => {}
                    HighlightKindOp::Some(kind) => {
                        self.assign(Register::HighlightKind, kind.as_usize());
                    }
                }

                match &*t.dst.borrow() {
                    Node::Intra(..) => {
                        self.jump(RelocationTarget::Node(t.dst as *const _));
                    }
                    Node::Leaf(LeafNode::Jump(dst)) => {
                        self.jump(RelocationTarget::Node(*dst as *const _));
                    }
                    Node::Leaf(LeafNode::Push(dst)) => {
                        self.flush_highlight();
                        self.call(RelocationTarget::Node(*dst as *const _));
                    }
                    Node::Leaf(LeafNode::Pop(0)) => {
                        self.flush_highlight();
                        self.suspend_opportunity();
                        self.assign(Register::HighlightKind, HighlightKind::Other.as_usize());
                        self.copy(Register::ProgramCounter, Register::ProcedureStart);
                    }
                    Node::Leaf(LeafNode::Pop(1)) => {
                        self.flush_highlight();
                        self.ret();
                    }
                    Node::Leaf(LeafNode::Loop) => {
                        self.suspend_opportunity();
                        self.jump(RelocationTarget::Node(src));
                    }
                    _ => unreachable!(),
                }
            }
        });

        for &(off, dst) in &self.relocations {
            let instruction_offset = *offsets
                .get(&match dst {
                    RelocationTarget::Node(h) => h as *const _ as *const (),
                    RelocationTarget::Edge(h) => h as *const _ as *const (),
                })
                .unwrap();
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

    fn jump(&mut self, dst: RelocationTarget<'a>) {
        let dst = self.resolve_relocation(dst);
        self.assign(Register::ProgramCounter, dst);
    }

    fn jump_if_not_match_charset(&mut self, idx: usize, dst: RelocationTarget<'a>) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchCharset { idx, dst });
    }

    fn jump_if_not_match_prefix(&mut self, idx: usize, dst: RelocationTarget<'a>) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchPrefix { idx, dst });
    }

    fn jump_if_not_match_prefix_insensitive(&mut self, idx: usize, dst: RelocationTarget<'a>) {
        let dst = self.resolve_relocation(dst);
        self.instructions.push(Instruction::JumpIfNotMatchPrefixInsensitive { idx, dst });
    }

    fn call(&mut self, dst: RelocationTarget<'a>) {
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

    fn resolve_relocation(&mut self, dst: RelocationTarget<'a>) -> usize {
        self.relocations.push((self.instructions.len(), dst));
        0
    }
}
