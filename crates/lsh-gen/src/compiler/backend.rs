// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::HashMap;
use std::fmt::Write as _;

use super::*;

#[derive(Debug, Clone, Copy)]
enum Relocation<'a> {
    ByName(usize, &'a str),
    ByNode(usize, IRCell<'a>),
}

// General encoding:
// * Instructions are 32 bit
// * The opcode is 4 bits at opcode[3:0]
// * Register constants are 4 bits
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Instruction {
    // Encoding:
    //   imm[31:12] | src[11:8] | dst[7:4] | 0000
    //
    // NOTE: This allows for jumps by manipulating Register::ProgramCounter.
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
    // Jumps to `dst` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0100
    JumpIfMatchPrefix { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0101
    JumpIfMatchPrefixInsensitive { idx: usize, dst: usize },

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
            Instruction::JumpIfMatchCharset { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0011
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0100
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
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
            Instruction::JumpIfMatchCharset { idx, dst } => {
                format!("jnc   {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                format!("jnp   {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
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
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self { instructions: Vec::new(), charsets: Vec::new(), strings: Vec::new() }
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

    pub fn compile(&mut self, compiler: &Compiler<'a>) {
        let mut relocations = Vec::new();

        let mut charsets_seen = HashMap::new();
        let mut visit_charset = |h: &'a Charset| {
            *charsets_seen.entry(h as *const _).or_insert_with(|| {
                let idx = self.charsets.len();
                self.charsets.push(h);
                idx
            })
        };
        let mut strings_seen = HashMap::new();
        let mut visit_string = |s: &'a str| {
            *strings_seen.entry(s as *const _).or_insert_with(|| {
                let idx = self.strings.len();
                self.strings.push(s);
                idx
            })
        };

        for function in &compiler.functions {
            let mut stack = vec![function.body];

            while let mut cell = stack.pop()
                && cell.is_some()
            {
                while let Some(ir) = cell {
                    let mut ir = ir.borrow_mut();
                    ir.offset = self.instructions.len();

                    match ir.instr {
                        IRI::Add { dst, src, imm } => {
                            self.instructions.push(Instruction::Add { dst, src, imm });
                        }
                        IRI::If { condition, then } => {
                            stack.push(then);
                            relocations.push(Relocation::ByNode(self.instructions.len(), then));
                            match condition {
                                Condition::Charset(h) => {
                                    let idx = visit_charset(h);
                                    self.instructions
                                        .push(Instruction::JumpIfMatchCharset { idx, dst: 0 });
                                }
                                Condition::Prefix(s) => {
                                    let idx = visit_string(s);
                                    self.instructions
                                        .push(Instruction::JumpIfMatchPrefix { idx, dst: 0 });
                                }
                                Condition::PrefixInsensitive(s) => {
                                    let idx = visit_string(s);
                                    self.instructions.push(
                                        Instruction::JumpIfMatchPrefixInsensitive { idx, dst: 0 },
                                    );
                                }
                            }
                        }
                        IRI::Call { name } => {
                            relocations.push(Relocation::ByName(self.instructions.len(), name));
                            self.instructions.push(Instruction::Call { dst: 0 });
                        }
                        IRI::Return => {
                            self.instructions.push(Instruction::Return);
                        }
                        IRI::Flush => {
                            self.instructions.push(Instruction::FlushHighlight);
                        }
                    }

                    cell = ir.next;
                }
            }
        }

        for reloc in relocations {
            let (off, resolved) = match reloc {
                Relocation::ByName(off, name) => {
                    let resolved = compiler
                        .functions
                        .iter()
                        .find(|f| f.name == name)
                        .map(|f| f.body.borrow().offset)
                        .expect("Undefined function in relocation");
                    (off, resolved)
                }
                Relocation::ByNode(off, node) => {
                    let resolved = node.borrow().offset;
                    (off, resolved)
                }
            };

            match &mut self.instructions[off] {
                Instruction::Add { dst: Register::ProgramCounter, src: Register::Zero, imm } => {
                    *imm = resolved;
                }
                Instruction::Call { dst }
                | Instruction::JumpIfMatchCharset { dst, .. }
                | Instruction::JumpIfMatchPrefix { dst, .. }
                | Instruction::JumpIfMatchPrefixInsensitive { dst, .. } => {
                    *dst = resolved;
                }
                i => panic!("Unexpected relocation target: {i:?}"),
            }
        }
    }
}
