// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::{HashMap, VecDeque};

use super::*;

#[derive(Debug, Clone, Copy)]
enum Relocation<'a> {
    ByName(usize, &'a str),
    ByNode(usize, IRCell<'a>),
}

pub struct Backend<'a> {
    assembly: Assembly<'a>,

    stack: VecDeque<IRCell<'a>>,
    relocations: Vec<Relocation<'a>>,
    next_instruction_label: &'a str,

    functions_seen: HashMap<&'a str, usize>,
    charsets_seen: HashMap<*const Charset, usize>,
    strings_seen: HashMap<*const str, usize>,
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self {
            assembly: Assembly {
                instructions: Vec::new(),
                charsets: Vec::new(),
                strings: Vec::new(),
            },

            stack: VecDeque::new(),
            relocations: Vec::new(),
            next_instruction_label: "",

            functions_seen: HashMap::new(),
            charsets_seen: HashMap::new(),
            strings_seen: HashMap::new(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        for function in &compiler.functions {
            self.next_instruction_label = function.name;
            self.functions_seen.insert(function.name, self.assembly.instructions.len());
            self.stack.push_back(function.body);

            while let Some(mut ir) = self.stack.pop_front() {
                let mut ir = ir.borrow_mut();

                if ir.offset != usize::MAX {
                    // Already serialized: self.dst_by_node() and the relocation code will insert jumps as needed.
                    continue;
                }

                loop {
                    ir.offset = self.assembly.instructions.len();

                    match ir.instr {
                        IRI::Add { dst, src, imm } => {
                            self.push_instruction(Instruction::Add { dst, src, imm });
                        }
                        IRI::If { condition, then } => {
                            self.stack.push_back(then);
                            let dst = self.dst_by_node(then);
                            let instr = match condition {
                                Condition::Charset(h) => {
                                    let idx = self.visit_charset(h);
                                    Instruction::JumpIfMatchCharset { idx, dst }
                                }
                                Condition::Prefix(s) => {
                                    let idx = self.visit_string(s);
                                    Instruction::JumpIfMatchPrefix { idx, dst }
                                }
                                Condition::PrefixInsensitive(s) => {
                                    let idx = self.visit_string(s);
                                    Instruction::JumpIfMatchPrefixInsensitive { idx, dst }
                                }
                            };
                            self.push_instruction(instr);
                        }
                        IRI::Call { name } => {
                            let dst = self.dst_by_name(name);
                            self.push_instruction(Instruction::Call { dst });
                        }
                        IRI::Return => {
                            self.push_instruction(Instruction::Return);
                        }
                        IRI::Flush => {
                            self.push_instruction(Instruction::FlushHighlight);
                        }
                        IRI::Loop { dst } => {
                            let dst = self.dst_by_node(dst);
                            self.push_instruction(Instruction::Loop { dst });
                        }
                    }

                    ir = match ir.next {
                        Some(next) => next.borrow_mut(),
                        None => break,
                    };

                    // If the tail end of this IR chain is already compiled, we jump there.
                    if ir.offset != usize::MAX {
                        self.push_instruction(match ir.instr {
                            // If the target instruction is a jump (or loop) itself, we can just duplicate it.
                            IRI::Add {
                                dst: Register::ProgramCounter, src: Register::Zero, ..
                            }
                            | IRI::Loop { .. } => self.assembly.instructions[ir.offset].instr,
                            // Otherwise, a regular jump.
                            _ => Instruction::Add {
                                dst: Register::ProgramCounter,
                                src: Register::Zero,
                                imm: ir.offset,
                            },
                        });
                        break;
                    }
                }
            }

            self.process_relocations();
        }

        if !self.relocations.is_empty() {
            let names: String = self
                .relocations
                .iter()
                .filter_map(|reloc| match reloc {
                    Relocation::ByName(_, name) => Some(*name),
                    Relocation::ByNode(_, _) => None,
                })
                .collect::<Vec<&str>>()
                .join(", ");

            if !names.is_empty() {
                return Err(CompileError {
                    line: 0,
                    column: 0,
                    message: format!("Unresolved function call names: {names}"),
                });
            }

            return Err(CompileError {
                line: 0,
                column: 0,
                message: "Unresolved IR nodes".to_string(),
            });
        }

        Ok(self.assembly)
    }

    fn visit_charset(&mut self, h: &'a Charset) -> usize {
        *self.charsets_seen.entry(h as *const _).or_insert_with(|| {
            let idx = self.assembly.charsets.len();
            self.assembly.charsets.push(h);
            idx
        })
    }

    fn visit_string(&mut self, s: &'a str) -> usize {
        *self.strings_seen.entry(s as *const _).or_insert_with(|| {
            let idx = self.assembly.strings.len();
            self.assembly.strings.push(s);
            idx
        })
    }

    fn dst_by_node(&mut self, ir: IRCell<'a>) -> usize {
        let off = ir.borrow().offset;
        if off != usize::MAX {
            off
        } else {
            self.relocations.push(Relocation::ByNode(self.assembly.instructions.len(), ir));
            0
        }
    }

    fn dst_by_name(&mut self, name: &'a str) -> usize {
        match self.functions_seen.get(name) {
            Some(&dst) => dst,
            None => {
                self.relocations.push(Relocation::ByName(self.assembly.instructions.len(), name));
                0
            }
        }
    }

    fn process_relocations(&mut self) {
        self.relocations.retain_mut(|reloc| {
            let (off, resolved) = match *reloc {
                Relocation::ByName(off, name) => match self.functions_seen.get(name) {
                    None => return true,
                    Some(&resolved) => (off, resolved),
                },
                Relocation::ByNode(off, node) => match node.borrow().offset {
                    usize::MAX => return true,
                    resolved => (off, resolved),
                },
            };

            match &mut self.assembly.instructions[off].instr {
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

            false
        });
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.assembly
            .instructions
            .push(AnnotatedInstruction { instr, label: self.next_instruction_label });
        self.next_instruction_label = "";
    }
}
