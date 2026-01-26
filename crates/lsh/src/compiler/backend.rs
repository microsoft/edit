// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Backend: bytecode generation with liveness analysis and linear scan register allocation.
//!
//! The algorithm follows the Cornell CS 4120 lecture notes:
//! 1. Linearize IR nodes using DFS to assign instruction indices
//! 2. Compute liveness using backward dataflow analysis
//! 3. Compute live intervals from liveness sets
//! 4. Run linear scan register allocation
//! 5. Generate bytecode using pre-allocated registers
//!
//! ## Why DFS ordering is essential
//!
//! The linearization step must use depth-first traversal. Live intervals are computed
//! as `[min_index, max_index]` across all instructions where a vreg appears in liveness
//! sets. With BFS, instructions from independent if/else branches get interleaved,
//! causing vregs local to one branch to appear live across unrelated branches.
//!
//! For example, with code like:
//! ```text
//! if /a/ { backup = off; ... restore off from backup }
//! else if /b/ { backup2 = off; ... restore off from backup2 }
//! ```
//!
//! BFS might number them: `[0: if /a/] [1: if /b/] [2: backup=off] [3: backup2=off] ...`
//! This makes `backup` appear live from index 2 through later indices in both branches,
//! conflicting with `backup2` even though they're in separate branches.
//!
//! DFS numbers each branch contiguously, so `backup` and `backup2` have non-overlapping
//! intervals and can share a register.

use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

use stdext::arena::scratch_arena;

use super::*;

/// A live interval represents the range of instructions where a vreg is live.
/// We use instruction indices (not byte offsets).
#[derive(Debug, Clone, Copy)]
struct LiveInterval {
    vreg_id: u32,
    start: usize, // First instruction where this vreg is live
    end: usize,   // Last instruction where this vreg is live (inclusive)
}

#[derive(Debug, Clone, Copy)]
enum Relocation<'a> {
    ByName(usize, &'a str),
    ByNode(usize, IRCell<'a>),
}

pub struct Backend<'a> {
    assembly: Assembly<'a>,
    relocations: Vec<Relocation<'a>>,
    functions_seen: HashMap<&'a str, usize>,
    charsets_seen: HashMap<*const Charset, usize>,
    strings_seen: HashMap<*const str, usize>,
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
            relocations: Default::default(),
            functions_seen: Default::default(),
            charsets_seen: Default::default(),
            strings_seen: Default::default(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        for function in &compiler.functions {
            // Reset physical register assignments for each function
            self.reset_physical_registers(compiler);

            // Step 1-4: Liveness analysis and register allocation
            self.allocate_registers_for_function(compiler, function)?;

            // Step 5: Code generation
            let entrypoint_offset = self.assembly.instructions.len();
            self.functions_seen.insert(function.name, entrypoint_offset);
            self.generate_code_for_function(compiler, function)?;

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
            .map(|f| Entrypoint {
                name: f.name.to_string(),
                display_name: f.attributes.display_name.unwrap_or(f.name).to_string(),
                paths: f.attributes.paths.iter().map(|s| s.to_string()).collect(),
                address: f.body.borrow().offset,
            })
            .collect();
        self.assembly.highlight_kinds = compiler.highlight_kinds.clone();
        Ok(self.assembly)
    }

    /// Reset all physical register assignments before processing a function.
    fn reset_physical_registers(&self, compiler: &Compiler<'a>) {
        // Physical registers (those with id < Register::COUNT) keep their physical mapping.
        // Virtual registers get their physical assignment cleared.
        // Note: we don't need to do anything here because register allocation
        // will set the physical field for each vreg.
    }

    /// Perform liveness analysis and register allocation for a function.
    fn allocate_registers_for_function(
        &mut self,
        compiler: &Compiler<'a>,
        function: &Function<'a>,
    ) -> CompileResult<()> {
        // Step 1: Linearize IR nodes, build CFG, and collect all vregs
        let (nodes, node_to_idx, successors, vreg_cells) =
            self.linearize_and_build_cfg(compiler, function);

        if nodes.is_empty() {
            return Ok(());
        }

        // Step 2: Compute liveness using backward dataflow analysis
        let (live_in, live_out) = self.compute_liveness(&nodes, &successors);

        // Step 3: Compute live intervals for each vreg
        let intervals = self.compute_live_intervals(&nodes, &live_in, &live_out);

        // Step 4: Linear scan register allocation
        let allocation = self.linear_scan_allocation(intervals)?;

        // Step 5: Apply the allocation to the IRReg cells
        for (vreg_id, reg) in allocation {
            if let Some(cell) = vreg_cells.get(&vreg_id) {
                cell.borrow_mut().physical = Some(reg);
            }
        }

        Ok(())
    }

    /// Linearize IR nodes into a vector and build the CFG (successor relationships).
    /// Also collects all unique vreg cells.
    /// Uses DFS order to keep related instructions close together in numbering.
    /// Returns: (nodes, node_to_idx mapping, successors for each node, vreg_cells)
    fn linearize_and_build_cfg(
        &self,
        compiler: &Compiler<'a>,
        function: &Function<'a>,
    ) -> (
        Vec<IRCell<'a>>,
        HashMap<*const RefCell<IR<'a>>, usize>,
        Vec<Vec<usize>>,
        HashMap<u32, IRRegCell<'a>>,
    ) {
        let mut nodes: Vec<IRCell<'a>> = Vec::new();
        let mut node_to_idx: HashMap<*const RefCell<IR<'a>>, usize> = HashMap::new();
        let mut vreg_cells: HashMap<u32, IRRegCell<'a>> = HashMap::new();
        let mut visited = HashSet::new();

        // DFS is essential here, not just an optimization. See module docs for details.
        fn dfs<'a>(
            cell: IRCell<'a>,
            nodes: &mut Vec<IRCell<'a>>,
            node_to_idx: &mut HashMap<*const RefCell<IR<'a>>, usize>,
            vreg_cells: &mut HashMap<u32, IRRegCell<'a>>,
            visited: &mut HashSet<*const RefCell<IR<'a>>>,
        ) {
            if !visited.insert(cell as *const _) {
                return;
            }

            let idx = nodes.len();
            node_to_idx.insert(cell as *const _, idx);
            nodes.push(cell);

            let ir = cell.borrow();

            // Collect vregs from this instruction
            match ir.instr {
                IRI::Add { dst, src, .. } => {
                    let dst_reg = dst.borrow();
                    if dst_reg.physical.is_none() {
                        vreg_cells.insert(dst_reg.id, dst);
                    }
                    let src_reg = src.borrow();
                    if src_reg.physical.is_none() {
                        vreg_cells.insert(src_reg.id, src);
                    }
                }
                IRI::If { condition: Condition::Cmp { lhs, rhs, .. }, then } => {
                    let lhs_reg = lhs.borrow();
                    if lhs_reg.physical.is_none() {
                        vreg_cells.insert(lhs_reg.id, lhs);
                    }
                    let rhs_reg = rhs.borrow();
                    if rhs_reg.physical.is_none() {
                        vreg_cells.insert(rhs_reg.id, rhs);
                    }
                }
                _ => {}
            }

            // For If nodes, visit the "then" branch first (DFS into branches)
            // before continuing with the "next" (fallthrough) path.
            // This keeps the inner branch instructions contiguous.
            if let IRI::If { then, .. } = ir.instr {
                drop(ir);
                dfs(then, nodes, node_to_idx, vreg_cells, visited);
                // Re-borrow to get next
                let ir = cell.borrow();
                if let Some(next) = ir.next {
                    drop(ir);
                    dfs(next, nodes, node_to_idx, vreg_cells, visited);
                }
            } else if let Some(next) = ir.next {
                drop(ir);
                dfs(next, nodes, node_to_idx, vreg_cells, visited);
            }
        }

        dfs(function.body, &mut nodes, &mut node_to_idx, &mut vreg_cells, &mut visited);

        // Second pass: build successor relationships
        let mut successors: Vec<Vec<usize>> = vec![Vec::new(); nodes.len()];
        for (idx, cell) in nodes.iter().enumerate() {
            let ir = cell.borrow();
            if let Some(next) = ir.next {
                if let Some(&next_idx) = node_to_idx.get(&(next as *const _)) {
                    successors[idx].push(next_idx);
                }
            }
            if let IRI::If { then, .. } = ir.instr {
                if let Some(&then_idx) = node_to_idx.get(&(then as *const _)) {
                    successors[idx].push(then_idx);
                }
            }
        }

        (nodes, node_to_idx, successors, vreg_cells)
    }

    /// Compute liveness using backward dataflow analysis.
    /// Returns: (live_in sets, live_out sets) for each instruction index.
    fn compute_liveness(
        &self,
        nodes: &[IRCell<'a>],
        successors: &[Vec<usize>],
    ) -> (Vec<HashSet<u32>>, Vec<HashSet<u32>>) {
        let n = nodes.len();

        // Initialize live_in and live_out sets
        let mut live_in: Vec<HashSet<u32>> = vec![HashSet::new(); n];
        let mut live_out: Vec<HashSet<u32>> = vec![HashSet::new(); n];

        // Build predecessor relationships (needed for backward analysis)
        let mut predecessors: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (idx, succs) in successors.iter().enumerate() {
            for &succ in succs {
                predecessors[succ].push(idx);
            }
        }

        // Worklist algorithm for backward dataflow analysis
        // For liveness: in[n] = use[n] ∪ (out[n] - def[n])
        //               out[n] = ∪_{n'∈succ(n)} in[n']
        let mut worklist: VecDeque<usize> = (0..n).collect();
        let mut in_worklist: Vec<bool> = vec![true; n];

        while let Some(idx) = worklist.pop_front() {
            in_worklist[idx] = false;

            let ir = nodes[idx].borrow();

            // Compute out[n] = ∪_{n'∈succ(n)} in[n']
            let mut new_out = HashSet::new();
            for &succ in &successors[idx] {
                new_out.extend(live_in[succ].iter().cloned());
            }

            // Compute use[n] and def[n]
            let (use_set, def_set) = Self::compute_use_def(&ir);

            // Compute in[n] = use[n] ∪ (out[n] - def[n])
            let mut new_in = use_set;
            for &vreg in &new_out {
                if !def_set.contains(&vreg) {
                    new_in.insert(vreg);
                }
            }

            // If in[n] changed, add predecessors to worklist
            if new_in != live_in[idx] {
                live_in[idx] = new_in;
                for &pred in &predecessors[idx] {
                    if !in_worklist[pred] {
                        worklist.push_back(pred);
                        in_worklist[pred] = true;
                    }
                }
            }

            live_out[idx] = new_out;
        }

        (live_in, live_out)
    }

    /// Compute use and def sets for an IR instruction.
    /// Returns: (use_set, def_set) of vreg IDs.
    fn compute_use_def(ir: &IR<'a>) -> (HashSet<u32>, HashSet<u32>) {
        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        match ir.instr {
            IRI::Noop => {}
            IRI::Add { dst, src, .. } => {
                // src is used (read)
                let src_reg = src.borrow();
                if src_reg.physical.is_none() || src_reg.id >= Register::COUNT as u32 {
                    use_set.insert(src_reg.id);
                }
                // dst is defined (written)
                let dst_reg = dst.borrow();
                if dst_reg.physical.is_none() || dst_reg.id >= Register::COUNT as u32 {
                    def_set.insert(dst_reg.id);
                }
            }
            IRI::If { condition: Condition::Cmp { lhs, rhs, .. }, .. } => {
                // lhs and rhs are used
                let lhs_reg = lhs.borrow();
                if lhs_reg.physical.is_none() || lhs_reg.id >= Register::COUNT as u32 {
                    use_set.insert(lhs_reg.id);
                }
                let rhs_reg = rhs.borrow();
                if rhs_reg.physical.is_none() || rhs_reg.id >= Register::COUNT as u32 {
                    use_set.insert(rhs_reg.id);
                }
            }
            IRI::If { .. } | IRI::Call { .. } | IRI::Return | IRI::Flush | IRI::AwaitInput => {}
        }

        (use_set, def_set)
    }

    /// Compute live intervals from liveness sets.
    /// A live interval is [start, end] where start is the first instruction
    /// where the vreg is live, and end is the last.
    fn compute_live_intervals(
        &self,
        nodes: &[IRCell<'a>],
        live_in: &[HashSet<u32>],
        live_out: &[HashSet<u32>],
    ) -> Vec<LiveInterval> {
        // First, collect all vreg IDs and their live ranges
        let mut vreg_ranges: HashMap<u32, (usize, usize)> = HashMap::new();

        for (idx, _node) in nodes.iter().enumerate() {
            // A vreg is live at instruction idx if it's in live_in[idx] or live_out[idx]
            for &vreg_id in live_in[idx].iter().chain(live_out[idx].iter()) {
                vreg_ranges
                    .entry(vreg_id)
                    .and_modify(|(start, end)| {
                        *start = (*start).min(idx);
                        *end = (*end).max(idx);
                    })
                    .or_insert((idx, idx));
            }
        }

        // Convert to LiveInterval structs, sorted by start position
        let mut intervals: Vec<LiveInterval> = vreg_ranges
            .into_iter()
            .map(|(vreg_id, (start, end))| LiveInterval { vreg_id, start, end })
            .collect();

        intervals.sort_by_key(|i| i.start);
        intervals
    }

    /// Linear scan register allocation.
    /// Assigns physical registers to vregs based on their live intervals.
    /// Returns a map from vreg_id to allocated physical register.
    fn linear_scan_allocation(
        &mut self,
        intervals: Vec<LiveInterval>,
    ) -> CompileResult<HashMap<u32, Register>> {
        let mut vreg_to_physical: HashMap<u32, Register> = HashMap::new();

        if intervals.is_empty() {
            return Ok(vreg_to_physical);
        }

        // Active intervals (currently allocated), sorted by end position
        let mut active: Vec<LiveInterval> = Vec::new();

        // Available registers (user registers only)
        let mut available: Vec<Register> =
            (Register::FIRST_USER_REG..Register::COUNT).map(|i| Register::from_usize(i)).collect();

        for interval in intervals {
            // Expire old intervals: free registers for intervals that ended before this one starts
            active.retain(|active_interval| {
                if active_interval.end < interval.start {
                    // This interval has expired, free its register
                    if let Some(&reg) = vreg_to_physical.get(&active_interval.vreg_id) {
                        // Only add back to available if it's a user register
                        if reg as usize >= Register::FIRST_USER_REG {
                            available.push(reg);
                        }
                    }
                    false // Remove from active
                } else {
                    true // Keep in active
                }
            });

            // Try to allocate a register for this interval
            if let Some(reg) = available.pop() {
                vreg_to_physical.insert(interval.vreg_id, reg);
                active.push(interval);
                // Keep active sorted by end position
                active.sort_by_key(|i| i.end);
            } else {
                // Spill: pick the interval with the furthest end point
                // If it ends after the current interval, spill that one and use its register
                // Otherwise, spill the current interval (no register for it)
                if let Some(last_active) = active.last() {
                    if last_active.end > interval.end {
                        // Spill the last active interval, use its register for current
                        let spilled = active.pop().unwrap();
                        if let Some(&reg) = vreg_to_physical.get(&spilled.vreg_id) {
                            vreg_to_physical.remove(&spilled.vreg_id);
                            vreg_to_physical.insert(interval.vreg_id, reg);
                            active.push(interval);
                            active.sort_by_key(|i| i.end);
                            // Note: spilled interval doesn't get a register
                            // In a real compiler, we'd generate spill code
                        }
                    }
                    // else: current interval ends later, it gets spilled (no allocation)
                } else {
                    return Err(CompileError {
                        path: String::new(),
                        line: 0,
                        column: 0,
                        message: "out of physical registers".to_string(),
                    });
                }
            }
        }

        Ok(vreg_to_physical)
    }

    /// Generate code for a function.
    fn generate_code_for_function(
        &mut self,
        compiler: &Compiler<'a>,
        function: &Function<'a>,
    ) -> CompileResult<()> {
        use Instruction::*;

        let mut stack: VecDeque<IRCell<'a>> = VecDeque::new();
        stack.push_back(function.body);

        while let Some(ir_cell) = stack.pop_front() {
            let mut ir = ir_cell.borrow_mut();

            if ir.offset != usize::MAX {
                // Already serialized
                continue;
            }

            loop {
                ir.offset = self.assembly.instructions.len();

                match ir.instr {
                    IRI::Noop => {}
                    IRI::Add { dst, src, imm } => {
                        let src_reg = src.borrow();
                        let dst_reg = dst.borrow();

                        let src_phys = src_reg.physical.ok_or_else(|| CompileError {
                            path: String::new(),
                            line: 0,
                            column: 0,
                            message: format!(
                                "vreg v{} not allocated a physical register (src)",
                                src_reg.id
                            ),
                        })?;

                        let dst_phys = dst_reg.physical.ok_or_else(|| CompileError {
                            path: String::new(),
                            line: 0,
                            column: 0,
                            message: format!(
                                "vreg v{} not allocated a physical register (dst)",
                                dst_reg.id
                            ),
                        })?;

                        match (src_phys, imm) {
                            (Register::Zero, _) => {
                                self.push_instruction(MovImm { dst: dst_phys, imm });
                            }
                            (_, 0) => {
                                self.push_instruction(Mov { dst: dst_phys, src: src_phys });
                            }
                            _ => {
                                self.push_instruction(Mov { dst: dst_phys, src: src_phys });
                                self.push_instruction(AddImm { dst: dst_phys, imm });
                            }
                        }
                    }
                    IRI::If { condition, then } => {
                        stack.push_back(then);

                        match condition {
                            Condition::Cmp { lhs, rhs, op } => {
                                let lhs_phys =
                                    lhs.borrow().physical.ok_or_else(|| CompileError {
                                        path: String::new(),
                                        line: 0,
                                        column: 0,
                                        message: "lhs vreg not allocated".to_string(),
                                    })?;
                                let rhs_phys =
                                    rhs.borrow().physical.ok_or_else(|| CompileError {
                                        path: String::new(),
                                        line: 0,
                                        column: 0,
                                        message: "rhs vreg not allocated".to_string(),
                                    })?;
                                let tgt = self.dst_by_node(then) as u32;

                                match op {
                                    ComparisonOp::Eq => self.push_instruction(JumpEQ {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                    ComparisonOp::Ne => self.push_instruction(JumpNE {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                    ComparisonOp::Lt => self.push_instruction(JumpLT {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                    ComparisonOp::Gt => self.push_instruction(JumpGT {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                    ComparisonOp::Le => self.push_instruction(JumpLE {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                    ComparisonOp::Ge => self.push_instruction(JumpGE {
                                        lhs: lhs_phys,
                                        rhs: rhs_phys,
                                        tgt,
                                    }),
                                }
                            }
                            Condition::EndOfLine => {
                                let tgt = self.dst_by_node(then) as u32;
                                self.push_instruction(JumpIfEndOfLine { tgt });
                            }
                            Condition::Charset(h) => {
                                let idx = self.visit_charset(h) as u32;
                                let tgt = self.dst_by_node(then) as u32;
                                self.push_instruction(JumpIfMatchCharset { idx, tgt });
                            }
                            Condition::Prefix(s) => {
                                let idx = self.visit_string(s) as u32;
                                let tgt = self.dst_by_node(then) as u32;
                                self.push_instruction(JumpIfMatchPrefix { idx, tgt });
                            }
                            Condition::PrefixInsensitive(s) => {
                                let idx = self.visit_string(s) as u32;
                                let tgt = self.dst_by_node(then) as u32;
                                self.push_instruction(JumpIfMatchPrefixInsensitive { idx, tgt });
                            }
                        }
                    }
                    IRI::Call { name } => {
                        let tgt = self.dst_by_name(name) as u32;
                        self.push_instruction(Call { tgt });
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

                drop(ir);
                ir = next.borrow_mut();

                if ir.offset != usize::MAX {
                    self.push_instruction(MovImm {
                        dst: Register::ProgramCounter,
                        imm: ir.offset as u32,
                    });
                    break;
                }
            }
        }

        Ok(())
    }

    fn push_instruction(&mut self, instr: Instruction) {
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
