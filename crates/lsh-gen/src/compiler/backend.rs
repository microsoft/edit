// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::HashMap;

use super::*;

#[derive(Debug, Clone, Copy)]
enum Relocation<'a> {
    ByName(usize, &'a str),
    ByNode(usize, IRCell<'a>),
}

pub struct Backend<'a> {
    assembly: Assembly<'a>,

    stack: Vec<IRCell<'a>>,
    relocations: Vec<Relocation<'a>>,

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

            stack: Vec::new(),
            relocations: Vec::new(),

            functions_seen: HashMap::new(),
            charsets_seen: HashMap::new(),
            strings_seen: HashMap::new(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        for function in &compiler.functions {
            self.functions_seen.insert(function.name, self.assembly.instructions.len());
            self.stack.push(function.body);

            while let mut cell = self.stack.pop()
                && cell.is_some()
            {
                while let Some(ir) = cell {
                    let mut ir = ir.borrow_mut();

                    if ir.offset != usize::MAX {
                        // Jump to the already compiled instruction.
                        self.assembly.instructions.push(match ir.instr {
                            // If the target instruction is a jump (or loop) itself, we can just duplicate it.
                            IRI::Add {
                                dst: Register::ProgramCounter, src: Register::Zero, ..
                            }
                            | IRI::Loop { .. } => self.assembly.instructions[ir.offset],
                            // Otherwise, a regular jump.
                            _ => Instruction::Add {
                                dst: Register::ProgramCounter,
                                src: Register::Zero,
                                imm: ir.offset,
                            },
                        });
                        break;
                    }

                    ir.offset = self.assembly.instructions.len();

                    match ir.instr {
                        IRI::Add { dst, src, imm } => {
                            self.assembly.instructions.push(Instruction::Add { dst, src, imm });
                        }
                        IRI::If { condition, then } => {
                            self.stack.push(then);
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
                            self.assembly.instructions.push(instr);
                        }
                        IRI::Call { name } => {
                            let dst = self.dst_by_name(name);
                            self.assembly.instructions.push(Instruction::Call { dst });
                        }
                        IRI::Return => {
                            self.assembly.instructions.push(Instruction::Return);
                        }
                        IRI::Flush => {
                            self.assembly.instructions.push(Instruction::FlushHighlight);
                        }
                        IRI::Loop { dst } => {
                            let dst = self.dst_by_node(dst);
                            self.assembly.instructions.push(Instruction::Loop { dst });
                        }
                    }

                    cell = ir.next;
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

            match &mut self.assembly.instructions[off] {
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
}
