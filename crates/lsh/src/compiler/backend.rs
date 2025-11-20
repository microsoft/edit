// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::{HashMap, HashSet, VecDeque};

use super::*;

/// Information about a virtual register's live range and allocation
#[derive(Clone)]
struct VRegInfo {
    /// First instruction where this vreg is defined or used
    first_use: usize,
    /// Last instruction where this vreg is used
    last_use: usize,
    /// Allocated physical register (if any)
    physical: Option<Register>,
}

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

    // Register allocation state
    vreg_info: HashMap<VRegId, VRegInfo>,
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self {
            assembly: Assembly {
                instructions: Vec::new(),
                entrypoints: Vec::new(),
                charsets: Vec::new(),
                strings: Vec::new(),
                highlight_kinds: Vec::new(),
            },

            stack: VecDeque::new(),
            relocations: Vec::new(),

            functions_seen: HashMap::new(),
            charsets_seen: HashMap::new(),
            strings_seen: HashMap::new(),
            vreg_info: HashMap::new(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        for function in &mut compiler.functions.clone() {
            // First, perform register allocation for this function
            self.allocate_registers(compiler, function)?;

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
                        IRI::SetHighlightKind { kind } => {
                            self.push_instruction(Instruction::Add {
                                dst: Register::HighlightKind,
                                src: Register::Zero,
                                imm: kind,
                            });
                        }
                        IRI::IncOffset { amount } => {
                            if amount == usize::MAX {
                                self.push_instruction(Instruction::Add {
                                    dst: Register::InputOffset,
                                    src: Register::Zero,
                                    imm: usize::MAX,
                                });
                            } else {
                                self.push_instruction(Instruction::Add {
                                    dst: Register::InputOffset,
                                    src: Register::InputOffset,
                                    imm: amount,
                                });
                            }
                        }
                        IRI::If { condition, then } => {
                            self.stack.push_back(then);
                            let dst = self.dst_by_node(then);
                            let instr = match condition {
                                Condition::EndOfLine => Instruction::JumpIfEndOfLine { dst },
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
                        IRI::AwaitInput => {
                            self.push_instruction(Instruction::AwaitInput);
                        }
                        // Virtual register instructions - lower to physical instructions
                        IRI::LoadImm { dst, value } => {
                            if let Some(phys) = self.get_physical_reg(dst) {
                                self.push_instruction(Instruction::Add {
                                    dst: phys,
                                    src: Register::Zero,
                                    imm: value as usize,
                                });
                            } else {
                                panic!("LoadImm: vreg {dst} not allocated");
                            }
                        }
                        IRI::AddImm { dst, src, imm } => {
                            match (self.get_physical_reg(dst), self.get_physical_reg(src)) {
                                (Some(dst_phys), Some(src_phys)) => {
                                    self.push_instruction(Instruction::Add {
                                        dst: dst_phys,
                                        src: src_phys,
                                        imm: imm as usize,
                                    });
                                }
                                _ => panic!("AddImm: vregs not allocated"),
                            }
                        }
                        IRI::CopyFromPhys { dst, src } => {
                            if let Some(dst_phys) = self.get_physical_reg(dst) {
                                self.push_instruction(Instruction::Add {
                                    dst: dst_phys,
                                    src,
                                    imm: 0,
                                });
                            } else {
                                panic!("CopyFromPhys: vreg {dst} not allocated");
                            }
                        }
                        IRI::CopyToPhys { dst, src } => {
                            if let Some(src_phys) = self.get_physical_reg(src) {
                                self.push_instruction(Instruction::Add {
                                    dst,
                                    src: src_phys,
                                    imm: 0,
                                });
                            } else {
                                panic!("CopyToPhys: vreg {src} not allocated");
                            }
                        }
                        IRI::Push { mask } => {
                            self.push_instruction(Instruction::Push { mask });
                        }
                        IRI::Pop { mask } => {
                            self.push_instruction(Instruction::Pop { mask });
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
                        self.push_instruction(Instruction::Add {
                            dst: Register::ProgramCounter,
                            src: Register::Zero,
                            imm: ir.offset,
                        });
                        break;
                    }
                }
            }

            self.process_relocations();

            self.assembly.instructions[entrypoint_offset].label = function.name;
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
            .map(|f| (f.name, f.body.borrow().offset))
            .collect();
        self.assembly.highlight_kinds = compiler.highlight_kinds.clone();
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
                | Instruction::JumpIfEndOfLine { dst }
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

    fn allocate_registers(
        &mut self,
        _compiler: &Compiler<'a>,
        function: &mut Function<'a>,
    ) -> CompileResult<()> {
        // Clear per-function state
        self.vreg_info.clear();

        // Collect all virtual registers and their live ranges
        let mut instruction_count = 0;
        self.analyze_liveness(function.body, &mut instruction_count);

        // Allocate physical registers using simple greedy allocation
        // Available registers: X6-X15 (10 registers total)
        let available_regs = vec![
            Register::X6,
            Register::X7,
            Register::X8,
            Register::X9,
            Register::X10,
            Register::X11,
            Register::X12,
            Register::X13,
            Register::X14,
            Register::X15,
        ];

        // Sort virtual registers by first use (simple linear scan allocation)
        let mut vregs: Vec<(VRegId, VRegInfo)> =
            self.vreg_info.iter().map(|(&id, info)| (id, info.clone())).collect();
        vregs.sort_by_key(|(_, info)| info.first_use);

        let mut used_registers = 0u16;

        for (vreg_id, info) in vregs {
            let mut allocated = false;

            // Try to find a free physical register
            for &phys_reg in &available_regs {
                // Check if this physical register is free at the current vreg's live range
                let conflicts = self.vreg_info.values().any(|other_info| {
                    if let Some(other_phys) = other_info.physical
                        && other_phys == phys_reg
                    {
                        // Check for overlap: other starts before vreg ends AND other ends after vreg starts
                        other_info.first_use <= info.last_use
                            && other_info.last_use >= info.first_use
                    } else {
                        false
                    }
                });

                if !conflicts {
                    // Allocate this physical register
                    let vreg_info = self.vreg_info.get_mut(&vreg_id).unwrap();
                    vreg_info.physical = Some(phys_reg);
                    used_registers |= 1 << (phys_reg as u16);
                    allocated = true;
                    break;
                }
            }

            // Error if no physical register available
            if !allocated {
                return Err(CompileError {
                    path: String::new(),
                    line: 0,
                    column: 0,
                    message: format!("too many live variables in function '{}'", function.name),
                });
            }
        }

        // Store which registers this function uses
        function.used_registers = used_registers;

        Ok(())
    }

    fn analyze_liveness(&mut self, root: IRCell<'a>, instruction_count: &mut usize) {
        let mut visited = HashSet::new();
        let mut to_visit = VecDeque::new();
        to_visit.push_back((root, *instruction_count));

        while let Some((cell, position)) = to_visit.pop_front() {
            if !visited.insert(cell.as_ptr()) {
                continue;
            }

            let ir = cell.borrow();
            *instruction_count = position + 1;

            // Record vreg usage
            match ir.instr {
                IRI::LoadImm { dst, .. } => {
                    self.record_vreg_use(dst, position);
                }
                IRI::AddImm { dst, src, .. } => {
                    self.record_vreg_use(src, position);
                    self.record_vreg_use(dst, position);
                }
                IRI::CopyFromPhys { dst, .. } => {
                    self.record_vreg_use(dst, position);
                }
                IRI::CopyToPhys { src, .. } => {
                    self.record_vreg_use(src, position);
                }
                IRI::If { then, .. } => {
                    to_visit.push_back((then, *instruction_count));
                }
                _ => {}
            }

            if let Some(next) = ir.next {
                to_visit.push_back((next, *instruction_count));
            }
        }
    }

    fn record_vreg_use(&mut self, vreg: VRegId, position: usize) {
        self.vreg_info
            .entry(vreg)
            .and_modify(|info| {
                info.first_use = info.first_use.min(position);
                info.last_use = info.last_use.max(position);
            })
            .or_insert(VRegInfo { first_use: position, last_use: position, physical: None });
    }

    fn get_physical_reg(&self, vreg: VRegId) -> Option<Register> {
        self.vreg_info.get(&vreg).and_then(|info| info.physical)
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.assembly.instructions.push(AnnotatedInstruction { instr, label: "" });
    }
}
