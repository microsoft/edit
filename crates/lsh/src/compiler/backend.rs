// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::{HashMap, HashSet, VecDeque};

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

    // Register allocation state for eager allocation
    available_regs: Vec<Register>,
    used_registers_mask: u16,
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
            available_regs: Vec::new(),
            used_registers_mask: 0,
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        for function in &mut compiler.functions.clone() {
            // Count uses of vregs and initialize register pool
            self.allocate_registers(compiler, function)?;
            self.init_available_registers();

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
                                Condition::Eq { lhs, rhs } => {
                                    // Track uses
                                    if let RegId::Virtual(v) = lhs {
                                        self.handle_vreg_use(v);
                                    }
                                    if let RegId::Virtual(v) = rhs {
                                        self.handle_vreg_use(v);
                                    }

                                    let Some(lhs) = self.get_physical_reg(lhs) else {
                                        panic!("Condition::Eq: vregs {lhs:?} not allocated");
                                    };
                                    let Some(rhs) = self.get_physical_reg(rhs) else {
                                        panic!("Condition::Eq: vregs {rhs:?} not allocated");
                                    };
                                    Instruction::JumpIfEq { lhs, rhs, dst }
                                }
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
                            // Allocate a physical register on first def
                            if let RegId::Virtual(v) = dst {
                                if v.physical.get().is_none() {
                                    if self.allocate_physical_register(v).is_none() {
                                        panic!("LoadImm: out of physical registers for {dst:?}");
                                    }
                                }
                            }

                            if let Some(phys) = self.get_physical_reg(dst) {
                                self.push_instruction(Instruction::Add {
                                    dst: phys,
                                    src: Register::Zero,
                                    imm: value as usize,
                                });
                            } else {
                                panic!("LoadImm: vreg {dst:?} not allocated");
                            }
                        }
                        IRI::AddImm { dst, src, imm } => {
                            // Mark src as used
                            if let RegId::Virtual(v) = src {
                                self.handle_vreg_use(v);
                            }
                            // Mark dst as used (for read-modify-write)
                            if let RegId::Virtual(v) = dst {
                                self.handle_vreg_use(v);
                                // Allocate if not yet allocated
                                if v.physical.get().is_none() {
                                    if self.allocate_physical_register(v).is_none() {
                                        panic!("AddImm: out of physical registers for {dst:?}");
                                    }
                                }
                            }

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
                            // Allocate a physical register on first def
                            if let RegId::Virtual(v) = dst {
                                if v.physical.get().is_none() {
                                    if self.allocate_physical_register(v).is_none() {
                                        panic!(
                                            "CopyFromPhys: out of physical registers for {dst:?}"
                                        );
                                    }
                                }
                            }

                            if let Some(dst_phys) = self.get_physical_reg(dst) {
                                self.push_instruction(Instruction::Add {
                                    dst: dst_phys,
                                    src,
                                    imm: 0,
                                });
                            } else {
                                panic!("CopyFromPhys: vreg {dst:?} not allocated");
                            }
                        }
                        IRI::CopyToPhys { dst, src } => {
                            // Mark src as used
                            if let RegId::Virtual(v) = src {
                                self.handle_vreg_use(v);
                            }

                            if let Some(src_phys) = self.get_physical_reg(src) {
                                self.push_instruction(Instruction::Add {
                                    dst,
                                    src: src_phys,
                                    imm: 0,
                                });
                            } else {
                                panic!("CopyToPhys: vreg {src:?} not allocated");
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

            function.used_registers = self.used_registers_mask;
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
                | Instruction::JumpIfMatchPrefixInsensitive { dst, .. }
                | Instruction::JumpIfEq { dst, .. } => {
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
        // Count how many times each virtual register is used
        fn count_vreg_uses<'a>(root: IRCell<'a>) {
            let mut visited = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back(root);

            while let Some(cell) = queue.pop_front() {
                if !visited.insert(cell.as_ptr()) {
                    continue;
                }

                let ir = cell.borrow();

                // Count uses (reads) of vregs
                match ir.instr {
                    IRI::AddImm { src, dst, .. } => {
                        if let RegId::Virtual(v) = src {
                            v.use_count.set(v.use_count.get() + 1);
                        }
                        if let RegId::Virtual(v) = dst {
                            v.use_count.set(v.use_count.get() + 1);
                        }
                    }
                    IRI::CopyToPhys { src, .. } => {
                        if let RegId::Virtual(v) = src {
                            v.use_count.set(v.use_count.get() + 1);
                        }
                    }
                    IRI::If { condition: Condition::Eq { lhs, rhs }, .. } => {
                        if let RegId::Virtual(v) = lhs {
                            v.use_count.set(v.use_count.get() + 1);
                        }
                        if let RegId::Virtual(v) = rhs {
                            v.use_count.set(v.use_count.get() + 1);
                        }
                    }
                    _ => {}
                }

                if let Some(next) = ir.next {
                    queue.push_back(next);
                }
                if let IRI::If { then, .. } = ir.instr {
                    queue.push_back(then);
                }
            }
        }

        count_vreg_uses(function.body);

        // Now during compile(), we'll allocate registers eagerly
        function.used_registers = 0;
        Ok(())
    }

    fn get_physical_reg(&self, reg: RegId<'a>) -> Option<Register> {
        match reg {
            RegId::Physical(p) => Some(p),
            RegId::Virtual(v) => v.physical.get(),
        }
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.assembly.instructions.push(AnnotatedInstruction { instr, label: "" });
    }

    fn allocate_physical_register(&mut self, vreg: &'a VReg) -> Option<Register> {
        if let Some(reg) = self.available_regs.pop() {
            vreg.physical.set(Some(reg));
            self.used_registers_mask |= 1 << (reg as u16);
            Some(reg)
        } else {
            None
        }
    }

    fn free_physical_register(&mut self, vreg: &'a VReg) {
        if let Some(reg) = vreg.physical.get() {
            self.available_regs.push(reg);
        }
    }

    fn handle_vreg_use(&mut self, vreg: &'a VReg) {
        let remaining = vreg.use_count.get().saturating_sub(1);
        vreg.use_count.set(remaining);
        if remaining == 0 {
            self.free_physical_register(vreg);
        }
    }

    fn init_available_registers(&mut self) {
        self.available_regs = vec![
            Register::X15,
            Register::X14,
            Register::X13,
            Register::X12,
            Register::X11,
            Register::X10,
            Register::X9,
            Register::X8,
            Register::X7,
            Register::X6,
        ];
        self.used_registers_mask = 0;
    }
}
