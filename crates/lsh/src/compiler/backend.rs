// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::transmute;

use stdext::arena::scratch_arena;

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
    functions_seen: HashMap<&'a str, usize>,
    charsets_seen: HashMap<*const Charset, usize>,
    strings_seen: HashMap<*const str, usize>,
    registers: RegisterAllocator,
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self {
            assembly: Assembly {
                instructions: Default::default(),
                entrypoints: Default::default(),
                charsets: Default::default(),
                strings: Default::default(),
                highlight_kinds: Default::default(),
            },
            stack: Default::default(),
            relocations: Default::default(),
            functions_seen: Default::default(),
            charsets_seen: Default::default(),
            strings_seen: Default::default(),
            registers: Default::default(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        use Instruction::*;

        compiler.count_register_uses();

        for function in &mut compiler.functions.clone() {
            self.registers = Default::default();

            let entrypoint_offset = self.assembly.instructions.len();
            self.functions_seen.insert(function.name, entrypoint_offset);
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
                        IRI::Noop => {}
                        IRI::Add { dst, src, imm } => {
                            // NOTE: read/write call order is crucial. See `read_reg`.
                            let src = self.read_reg(src)?;
                            let dst = self.write_reg(dst)?;
                            match (src, imm) {
                                (Register::Zero, _) => {
                                    self.push_instruction(MovImm { dst, imm });
                                }
                                (_, 0) => {
                                    self.push_instruction(Mov { dst, src });
                                }
                                _ => {
                                    self.push_instruction(Mov { dst, src });
                                    self.push_instruction(AddImm { dst, imm });
                                }
                            };
                        }
                        IRI::If { condition, then } => {
                            self.stack.push_back(then);

                            match condition {
                                Condition::Cmp { lhs, rhs, op } => {
                                    let lhs = self.read_reg(lhs)?;
                                    let rhs = self.read_reg(rhs)?;
                                    let off = self.dst_by_node(then) as u32;

                                    match op {
                                        ComparisonOp::Eq => {
                                            self.push_instruction(JumpEQ { lhs, rhs, tgt: off });
                                        }
                                        ComparisonOp::Ne => {
                                            self.push_instruction(JumpNE { lhs, rhs, tgt: off });
                                        }
                                        ComparisonOp::Lt => {
                                            self.push_instruction(JumpLT { lhs, rhs, tgt: off });
                                        }
                                        ComparisonOp::Gt => {
                                            self.push_instruction(JumpGT { lhs, rhs, tgt: off });
                                        }
                                        ComparisonOp::Le => {
                                            self.push_instruction(JumpLE { lhs, rhs, tgt: off });
                                        }
                                        ComparisonOp::Ge => {
                                            self.push_instruction(JumpGE { lhs, rhs, tgt: off });
                                        }
                                    }
                                }
                                Condition::EndOfLine => {
                                    let off = self.dst_by_node(then) as u32;
                                    self.push_instruction(JumpIfEndOfLine { tgt: off });
                                }
                                Condition::Charset(h) => {
                                    let idx = self.visit_charset(h) as u32;
                                    let off = self.dst_by_node(then) as u32;
                                    self.push_instruction(JumpIfMatchCharset { idx, tgt: off });
                                }
                                Condition::Prefix(s) => {
                                    let idx = self.visit_string(s) as u32;
                                    let off = self.dst_by_node(then) as u32;
                                    self.push_instruction(JumpIfMatchPrefix { idx, tgt: off });
                                }
                                Condition::PrefixInsensitive(s) => {
                                    let idx = self.visit_string(s) as u32;
                                    let off = self.dst_by_node(then) as u32;
                                    self.push_instruction(
                                        Instruction::JumpIfMatchPrefixInsensitive { idx, tgt: off },
                                    );
                                }
                            }
                        }
                        IRI::Call { name } => {
                            let off = self.dst_by_name(name) as u32;
                            self.push_instruction(Call { tgt: off });
                        }
                        IRI::Return => {
                            self.push_instruction(Return);
                        }
                        IRI::Flush => {
                            self.push_instruction(FlushHighlight);
                        }
                        IRI::AwaitInput => {
                            self.push_instruction(AwaitInput);
                        }
                    }

                    let Some(next) = ir.next else {
                        break;
                    };

                    // The current IR node may be fully circular, like
                    //   loop {
                    //     foo();
                    //   }
                    // so we need to drop the borrow before re-borrowing.
                    drop(ir);

                    ir = next.borrow_mut();

                    // If the tail end of this IR chain is already compiled, we jump there.
                    if ir.offset != usize::MAX {
                        self.push_instruction(MovImm {
                            dst: Register::ProgramCounter,
                            imm: ir.offset as u32,
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
                    path: String::new(),
                    line: 0,
                    column: 0,
                    message: format!("unresolved function call names: {names}"),
                });
            }

            return Err(CompileError {
                path: String::new(),
                line: 0,
                column: 0,
                message: "unresolved IR nodes".to_string(),
            });
        }

        self.assembly.entrypoints = compiler
            .functions
            .iter()
            .filter(|f| f.public)
            .map(|f| {
                let mut path_suffixes = Vec::new();
                let mut path_patterns = Vec::new();

                for path in &f.attributes.paths {
                    if let Some(suffix) =
                        path.strip_prefix("**/*").or_else(|| path.strip_prefix("**"))
                        && !suffix.is_empty()
                        && !suffix.contains(['?', '*', '[', ']', '{', '}'])
                    {
                        path_suffixes.push(suffix.to_string());
                    } else {
                        path_patterns.push(path.to_string());
                    }
                }

                Entrypoint {
                    name: f.name.to_string(),
                    display_name: f.attributes.display_name.unwrap_or(f.name).to_string(),
                    path_suffixes,
                    path_patterns,
                    address: f.body.borrow().offset,
                }
            })
            .collect();
        self.assembly.highlight_kinds = compiler.highlight_kinds.clone();
        Ok(self.assembly)
    }

    fn push_instruction(&mut self, instr: Instruction) {
        // If we have a pending relocation for the instruction we're about to write,
        // we can now calculate the exact byte offset of the immediate value that
        // needs to be patched and update the relocation accordingly.
        //
        // This allows `process_relocations` to simply overwrite the bytes at the
        // stored offset without having to decode specific instructions.
        if let Some(reloc) = self.relocations.last_mut() {
            let offset = match reloc {
                Relocation::ByName(off, _) => off,
                Relocation::ByNode(off, _) => off,
            };

            if *offset == self.assembly.instructions.len()
                && let Some(delta) = instr.address_offset()
            {
                *offset += delta;
            }
        }

        let scratch = scratch_arena(None);
        self.assembly.instructions.extend(instr.encode(&scratch));
    }

    /// Prepares a virtual register from IR for write-use in an instruction.
    ///
    /// # Warnings
    ///
    /// *ALWAYS* call [`Backend::read_reg`] *BEFORE* [`Backend::write_reg`].
    /// This is because the former frees physical registers for use by the latter.
    fn write_reg(&mut self, reg: IRRegCell<'a>) -> CompileResult<Register> {
        let mut reg = reg.borrow_mut();

        if let Some(r) = reg.physical {
            Ok(r)
        } else if let Some(r) = self.registers.alloc() {
            reg.physical = Some(r);
            Ok(r)
        } else {
            Err(CompileError {
                path: String::new(),
                line: 0,
                column: 0,
                message: "out of physical registers".to_string(),
            })
        }
    }

    /// Prepares a virtual register from IR for read-use in an instruction.
    ///
    /// # Warnings
    ///
    /// *ALWAYS* call [`Backend::read_reg`] *BEFORE* [`Backend::write_reg`].
    /// This is because the former frees physical registers for use by the latter.
    fn read_reg(&mut self, reg: IRRegCell<'a>) -> CompileResult<Register> {
        let mut reg = reg.borrow_mut();

        let Some(r) = reg.physical else {
            return Err(CompileError {
                path: String::new(),
                line: 0,
                column: 0,
                message: "reading from unallocated register".to_string(),
            });
        };

        reg.read_count -= 1;
        assert!(reg.read_count >= 0);

        if reg.read_count == 0 {
            self.registers.dealloc(r);
        }

        Ok(r)
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

            let range = off..off + 4;
            if let Some(target) = self.assembly.instructions.get_mut(range) {
                target.copy_from_slice(&(resolved as u32).to_le_bytes());
            } else {
                panic!("Unexpected relocation target offset: {off}");
            }

            false
        });
    }
}
