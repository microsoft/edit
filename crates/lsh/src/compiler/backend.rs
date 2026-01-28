// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Backend: IR graph -> bytecode
//!
//! ## Algorithm
//!
//! Source: <https://www.cs.cornell.edu/courses/cs4120/2022sp/notes/>
//!
//! - Linearize IR nodes using DFS to assign instruction indices
//! - Compute liveness using backward dataflow analysis
//! - Compute live intervals from liveness sets
//! - Linear scan register allocation
//! - Generate bytecode with physical registers
//!
//! ## Relocation system
//!
//! Jump targets aren't known during code generation (forward references). The backend
//! emits placeholder zeros and records `Relocation` entries. After each function,
//! `process_relocations()` patches the bytecode with resolved addresses.
//!
//! Two relocation types:
//! - `ByName` - cross-function calls, resolved when the target function is compiled
//! - `ByNode` - intra-function jumps to IR nodes not yet serialized
//!
//! ## Quirks
//!
//! - `IR.offset` starts as `usize::MAX` (unvisited). Codegen sets it to the bytecode
//!   address. If we encounter a node with `offset != MAX`, it's a backward reference (loop)
//!   We need to then emit a jump to the already-assigned address.
//! - Physical registers have `physical = Some(...)`.
//!   Liveness analysis ignores them (they're always "live").
//!
//! ### DFS
//!
//! Live intervals are `[min_index, max_index]` across all instructions where a vreg is live.
//! BFS interleaves independent branches, making branch-local vregs appear to span the whole function.
//!
//! Example: `if /a/ { x = off } else if /b/ { y = off }` with BFS numbering:
//! ```text
//! [0: if /a/] [1: if /b/] [2: x=off] [3: y=off] [4: use x] [5: use y]
//! ```
//! Here `x` appears live `[2,4]` and `y` appears live `[3,5]`, overlapping at `[3,4]`.
//!
//! DFS numbers each branch contiguously: `[0: if /a/] [1: x=off] [2: use x] [3: if /b/] ...`
//! Now `x` is live `[1,2]` and `y` is live `[4,5]` - no overlap & can share a register.
//!
//! ## TODO
//!
//! - The liveness analysis is a much later addition so it doesn't fit into existing structures very well.
//! - The linear scan allocator has spill logic but doesn't generate spill code.

use std::collections::{HashMap, HashSet, VecDeque};

use stdext::arena::scratch_arena;

use super::*;
use crate::runtime::Instruction;

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
            self.allocate_registers(function)?;

            let entrypoint_offset = self.assembly.instructions.len();
            self.functions_seen.insert(function.name, entrypoint_offset);
            self.generate_code(function)?;
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
            return Err(CompileError {
                path: String::new(),
                line: 0,
                column: 0,
                message: if !names.is_empty() {
                    format!("unresolved function call names: {names}")
                } else {
                    "unresolved IR nodes".to_string()
                },
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

    /// Perform liveness analysis and register allocation for a function.
    fn allocate_registers(&mut self, function: &Function<'a>) -> CompileResult<()> {
        let mut analysis = LivenessAnalysis::new(function);
        if analysis.is_empty() {
            return Ok(());
        }

        analysis.compute_liveness();
        let intervals = analysis.compute_intervals();
        let allocation = self.linear_scan_allocation(intervals)?;
        analysis.apply_allocation(&allocation);

        Ok(())
    }

    /// Linear scan register allocation (Poletto-Sarkar).
    ///
    /// Processes intervals in order of start position, expiring old intervals
    /// and allocating registers greedily. When out of registers, spills the
    /// interval with the furthest end point.
    fn linear_scan_allocation(
        &mut self,
        intervals: Vec<LiveInterval>,
    ) -> CompileResult<HashMap<u32, Register>> {
        let mut allocation: HashMap<u32, Register> = HashMap::new();

        if intervals.is_empty() {
            return Ok(allocation);
        }

        // Active intervals sorted by end position
        let mut active: Vec<LiveInterval> = Vec::new();

        // Available user registers
        let mut available: Vec<Register> =
            (Register::FIRST_USER_REG..Register::COUNT).rev().map(Register::from_usize).collect();

        for interval in intervals {
            // Expire intervals that ended before this one starts
            active.retain(|active_interval| {
                if active_interval.end < interval.start {
                    if let Some(&reg) = allocation.get(&active_interval.vreg_id)
                        && reg as usize >= Register::FIRST_USER_REG
                    {
                        available.push(reg);
                    }
                    false
                } else {
                    true
                }
            });

            // Allocate or spill
            if let Some(reg) = available.pop() {
                allocation.insert(interval.vreg_id, reg);
                active.push(interval);
                active.sort_by_key(|i| i.end);
            } else if let Some(last) = active.last() {
                if last.end > interval.end {
                    // Spill the longest-living active interval
                    let spilled = active.pop().unwrap();
                    if let Some(&reg) = allocation.get(&spilled.vreg_id) {
                        allocation.remove(&spilled.vreg_id);
                        allocation.insert(interval.vreg_id, reg);
                        active.push(interval);
                        active.sort_by_key(|i| i.end);
                    }
                }
            } else {
                // TODO: current interval gets spilled
                return Err(CompileError {
                    path: String::new(),
                    line: 0,
                    column: 0,
                    message: "out of physical registers".to_string(),
                });
            }
        }

        Ok(allocation)
    }

    /// Generate bytecode for a function (assumes registers already allocated).
    fn generate_code(&mut self, function: &Function<'a>) -> CompileResult<()> {
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
                    IRI::Mov { dst, src } => {
                        // NOTE: Liveness analysis doesn't assign physical registers for dead stores.
                        // In practice this shouldn't hit because optimizer.rs also removes dead stores.
                        // In essence, optimizer.rs is a bit redundant and it may be worth checking if they can be unified.
                        if let (Some(dst), Some(src)) =
                            (dst.borrow().physical, src.borrow().physical)
                        {
                            self.push_instruction(Mov { dst, src });
                        }
                    }
                    IRI::MovImm { dst, imm } => {
                        if let Some(dst) = dst.borrow().physical {
                            self.push_instruction(MovImm { dst, imm });
                        }
                    }
                    IRI::MovKind { dst, kind } => {
                        if let Some(dst) = dst.borrow().physical {
                            self.push_instruction(MovImm { dst, imm: kind });
                        }
                    }
                    IRI::AddImm { dst, imm } => {
                        if let Some(dst) = dst.borrow().physical {
                            self.push_instruction(AddImm { dst, imm });
                        }
                    }
                    IRI::If { condition, then } => {
                        stack.push_back(then);

                        debug_assert!(!std::ptr::eq(ir_cell, then));

                        match condition {
                            Condition::Cmp { lhs, rhs, op } => {
                                let lhs_phys = lhs.borrow().physical.unwrap();
                                let rhs_phys = rhs.borrow().physical.unwrap();
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
                            Condition::Charset { cs, min, max } => {
                                let idx = self.visit_charset(cs) as u32;
                                let tgt = self.dst_by_node(then) as u32;
                                self.push_instruction(JumpIfMatchCharset { idx, min, max, tgt });
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
                    IRI::Flush { kind } => {
                        let kind = kind.borrow().physical.unwrap();
                        self.push_instruction(FlushHighlight { kind });
                    }
                    IRI::AwaitInput => {
                        self.push_instruction(AwaitInput);
                    }
                }

                let Some(next) = ir.next else {
                    break;
                };

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

/// A live interval represents the range of instructions where a vreg is live.
#[derive(Debug, Clone, Copy)]
struct LiveInterval {
    vreg_id: u32,
    start: usize,
    end: usize,
}

/// Encapsulates liveness analysis state for a single function.
///
/// Performs DFS linearization, builds the CFG, and computes liveness sets.
/// The analysis owns all intermediate data structures, exposing only what's
/// needed for register allocation.
struct LivenessAnalysis<'a> {
    /// IR nodes in DFS order (instruction indices correspond to positions here).
    nodes: Vec<IRCell<'a>>,
    /// CFG: `successors[i]` contains indices of nodes that can follow node i.
    successors: Vec<Vec<usize>>,
    /// Map from vreg ID to its [`IRRegCell`] (for applying allocation results).
    vreg_cells: HashMap<u32, IRRegCell<'a>>,
    /// Liveness sets: `live_in[i]` = vregs live at entry to instruction i.
    live_in: Vec<HashSet<u32>>,
    /// Liveness sets: `live_out[i]` = vregs live at exit from instruction i.
    live_out: Vec<HashSet<u32>>,
}

impl<'a> LivenessAnalysis<'a> {
    /// Create a new liveness analysis for a function.
    ///
    /// This linearizes the IR using DFS and builds the CFG. Call `compute_liveness()`
    /// to fill in the liveness sets, then `compute_intervals()` to get live intervals.
    fn new(function: &Function<'a>) -> Self {
        let mut nodes = Vec::new();
        let mut node_to_idx = HashMap::new();
        let mut vreg_cells = HashMap::new();
        let mut visited = HashSet::new();

        // DFS is essential for correctness. See module docs for details.
        Self::dfs(function.body, &mut nodes, &mut node_to_idx, &mut vreg_cells, &mut visited);

        // Build successor relationships from the node_to_idx map
        let successors = Self::build_successors(&nodes, &node_to_idx);

        let n = nodes.len();
        Self {
            nodes,
            successors,
            vreg_cells,
            live_in: vec![HashSet::new(); n],
            live_out: vec![HashSet::new(); n],
        }
    }

    fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// DFS traversal to linearize IR nodes.
    ///
    /// Visits "then" branches before "next" (fallthrough) to keep branch
    /// instructions contiguous in the numbering.
    fn dfs(
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

        match ir.instr {
            IRI::Mov { dst, src } => {
                if dst.borrow().physical.is_none() {
                    vreg_cells.insert(dst.borrow().id, dst);
                }
                if src.borrow().physical.is_none() {
                    vreg_cells.insert(src.borrow().id, src);
                }
            }
            IRI::MovImm { dst, .. } => {
                if dst.borrow().physical.is_none() {
                    vreg_cells.insert(dst.borrow().id, dst);
                }
            }
            IRI::MovKind { dst, .. } => {
                if dst.borrow().physical.is_none() {
                    vreg_cells.insert(dst.borrow().id, dst);
                }
            }
            IRI::AddImm { dst, .. } => {
                if dst.borrow().physical.is_none() {
                    vreg_cells.insert(dst.borrow().id, dst);
                }
            }
            IRI::If { condition: Condition::Cmp { lhs, rhs, .. }, .. } => {
                if lhs.borrow().physical.is_none() {
                    vreg_cells.insert(lhs.borrow().id, lhs);
                }
                if rhs.borrow().physical.is_none() {
                    vreg_cells.insert(rhs.borrow().id, rhs);
                }
            }
            IRI::Flush { kind, .. } => {
                if kind.borrow().physical.is_none() {
                    vreg_cells.insert(kind.borrow().id, kind);
                }
            }
            _ => {}
        }

        // Visit "then" branch first (DFS into branches), then "next" (fallthrough).
        if let IRI::If { then, .. } = ir.instr {
            Self::dfs(then, nodes, node_to_idx, vreg_cells, visited);
            let ir = cell.borrow();
            if let Some(next) = ir.next {
                Self::dfs(next, nodes, node_to_idx, vreg_cells, visited);
            }
        } else if let Some(next) = ir.next {
            Self::dfs(next, nodes, node_to_idx, vreg_cells, visited);
        }
    }

    /// Build CFG successor relationships from the linearized nodes.
    fn build_successors(
        nodes: &[IRCell<'a>],
        node_to_idx: &HashMap<*const RefCell<IR<'a>>, usize>,
    ) -> Vec<Vec<usize>> {
        let mut successors = vec![Vec::new(); nodes.len()];
        for (idx, cell) in nodes.iter().enumerate() {
            let ir = cell.borrow();
            if let Some(next) = ir.next
                && let Some(&next_idx) = node_to_idx.get(&(next as *const _))
            {
                successors[idx].push(next_idx);
            }
            if let IRI::If { then, .. } = ir.instr
                && let Some(&then_idx) = node_to_idx.get(&(then as *const _))
            {
                successors[idx].push(then_idx);
            }
        }
        successors
    }

    /// Compute liveness using backward dataflow analysis (worklist algorithm).
    ///
    /// Dataflow equations:
    /// - `in[n] = use[n] ∪ (out[n] - def[n])`
    /// - `out[n] = ∪_{s ∈ succ(n)} in[s]`
    fn compute_liveness(&mut self) {
        let n = self.nodes.len();
        if n == 0 {
            return;
        }

        // Build predecessors from successors (needed for worklist propagation)
        let mut predecessors: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (idx, succs) in self.successors.iter().enumerate() {
            for &succ in succs {
                predecessors[succ].push(idx);
            }
        }

        // Worklist: start with all nodes
        let mut worklist: VecDeque<usize> = (0..n).collect();
        let mut in_worklist = vec![true; n];

        while let Some(idx) = worklist.pop_front() {
            in_worklist[idx] = false;

            // out[n] = ∪_{s ∈ succ(n)} in[s]
            let mut new_out = HashSet::new();
            for &succ in &self.successors[idx] {
                new_out.extend(self.live_in[succ].iter().copied());
            }

            // in[n] = use[n] ∪ (out[n] - def[n])
            let (use_set, def_set) = Self::compute_use_def(&self.nodes[idx].borrow());
            let mut new_in = use_set;
            for &vreg in &new_out {
                if !def_set.contains(&vreg) {
                    new_in.insert(vreg);
                }
            }

            // If in[n] changed, propagate to predecessors
            if new_in != self.live_in[idx] {
                self.live_in[idx] = new_in;
                for &pred in &predecessors[idx] {
                    if !in_worklist[pred] {
                        worklist.push_back(pred);
                        in_worklist[pred] = true;
                    }
                }
            }

            self.live_out[idx] = new_out;
        }
    }

    /// Compute use and def sets for a single IR instruction.
    fn compute_use_def(ir: &IR<'a>) -> (HashSet<u32>, HashSet<u32>) {
        let mut use_set = HashSet::new();
        let mut def_set = HashSet::new();

        match ir.instr {
            IRI::Mov { dst, src } => {
                if let dst_reg = dst.borrow()
                    && dst_reg.physical.is_none()
                {
                    def_set.insert(dst_reg.id);
                }
                let src_reg = src.borrow();
                if src_reg.physical.is_none() {
                    use_set.insert(src_reg.id);
                }
            }
            IRI::MovImm { dst, .. } => {
                if let dst_reg = dst.borrow()
                    && dst_reg.physical.is_none()
                {
                    def_set.insert(dst_reg.id);
                }
            }
            IRI::MovKind { dst, .. } => {
                if let dst_reg = dst.borrow()
                    && dst_reg.physical.is_none()
                {
                    def_set.insert(dst_reg.id);
                }
            }
            IRI::AddImm { dst, .. } => {
                if let dst_reg = dst.borrow()
                    && dst_reg.physical.is_none()
                {
                    def_set.insert(dst_reg.id);
                }
            }
            IRI::If { condition: Condition::Cmp { lhs, rhs, .. }, .. } => {
                let lhs_reg = lhs.borrow();
                if lhs_reg.physical.is_none() {
                    use_set.insert(lhs_reg.id);
                }
                let rhs_reg = rhs.borrow();
                if rhs_reg.physical.is_none() {
                    use_set.insert(rhs_reg.id);
                }
            }
            IRI::Flush { kind, .. } => {
                let kind_reg = kind.borrow();
                if kind_reg.physical.is_none() {
                    use_set.insert(kind_reg.id);
                }
            }
            _ => {}
        }

        (use_set, def_set)
    }

    /// Compute live intervals from liveness sets, sorted by start position.
    fn compute_intervals(&self) -> Vec<LiveInterval> {
        let mut vreg_ranges: HashMap<u32, (usize, usize)> = HashMap::new();

        for idx in 0..self.nodes.len() {
            for &vreg_id in self.live_in[idx].iter().chain(self.live_out[idx].iter()) {
                vreg_ranges
                    .entry(vreg_id)
                    .and_modify(|(start, end)| {
                        *start = (*start).min(idx);
                        *end = (*end).max(idx);
                    })
                    .or_insert((idx, idx));
            }
        }

        let mut intervals: Vec<LiveInterval> = vreg_ranges
            .into_iter()
            .map(|(vreg_id, (start, end))| LiveInterval { vreg_id, start, end })
            .collect();

        intervals.sort_by_key(|i| i.start);
        intervals
    }

    /// Apply register allocation results to the IRReg cells.
    fn apply_allocation(&self, allocation: &HashMap<u32, Register>) {
        for (&vreg_id, &reg) in allocation {
            if let Some(cell) = self.vreg_cells.get(&vreg_id) {
                cell.borrow_mut().physical = Some(reg);
            }
        }
    }
}
