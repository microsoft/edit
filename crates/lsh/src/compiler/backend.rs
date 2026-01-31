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
//! - Generate instructions with symbolic relocations
//! - Iterative relaxation to compute final addresses (for varint encoding)
//! - Emit final bytecode
//!
//! ## Relocation system
//!
//! Jump targets use symbolic references during code generation. After all
//! functions are compiled, iterative relaxation resolves addresses:
//!
//! 1. Start with pessimistic (max-size) varint encoding
//! 2. Compute byte offsets for all instructions
//! 3. Re-encode with actual addresses (may shrink varints)
//! 4. Repeat until no sizes change (guaranteed to converge)
//!
//! Two relocation types:
//! - `ByName` - cross-function calls, resolved when the target function is compiled
//! - `ByNode` - intra-function jumps to IR nodes
//!
//! ## Quirks
//!
//! - `IR.offset` starts as `usize::MAX` (unvisited). Codegen sets it to the
//!   *instruction index* (not byte offset). After relaxation, it holds the byte offset.
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

/// Symbolic reference to a jump target, resolved during relaxation.
#[derive(Debug, Clone, Copy)]
enum JumpTarget<'a> {
    /// Reference to a named function (cross-function call).
    ByName(&'a str),
    /// Reference to an IR node (intra-function jump).
    ByNode(IRCell<'a>),
}

/// An instruction with its symbolic jump target (if any).
struct PendingInstruction<'a> {
    /// The instruction with a placeholder target address (0).
    instr: Instruction,
    /// If this instruction has a jump target, how to resolve it.
    target: Option<JumpTarget<'a>>,
    /// Current computed byte offset (updated during relaxation).
    offset: usize,
}

impl<'a> PendingInstruction<'a> {
    fn new(instr: Instruction) -> Self {
        Self { instr, target: None, offset: 0 }
    }

    fn with_target(instr: Instruction, target: JumpTarget<'a>) -> Self {
        Self { instr, target: Some(target), offset: 0 }
    }
}

pub struct Backend<'a> {
    /// Pending instructions awaiting address resolution.
    pending: Vec<PendingInstruction<'a>>,
    /// Map from function name to its starting instruction index.
    functions_seen: HashMap<&'a str, usize>,
    /// Map from IR node pointer to its instruction index.
    nodes_seen: HashMap<*const RefCell<IR<'a>>, usize>,
}

impl<'a> Backend<'a> {
    pub fn new() -> Self {
        Self {
            pending: Default::default(),
            functions_seen: Default::default(),
            nodes_seen: Default::default(),
        }
    }

    pub fn compile(mut self, compiler: &Compiler<'a>) -> CompileResult<Assembly<'a>> {
        // Phase 1: Generate instructions with symbolic relocations
        for function in &compiler.functions {
            self.allocate_registers(function)?;

            let entrypoint_idx = self.pending.len();
            self.functions_seen.insert(function.name, entrypoint_idx);
            self.generate_code(function)?;
        }

        // Verify all function references are resolved
        for pi in &self.pending {
            if let Some(JumpTarget::ByName(name)) = pi.target
                && !self.functions_seen.contains_key(name)
            {
                return Err(CompileError {
                    path: String::new(),
                    line: 0,
                    column: 0,
                    message: format!("unresolved function call: {name}"),
                });
            }
        }

        // Phase 2: Iterative relaxation to compute final byte offsets
        self.relax();

        // Update IR node offsets from instruction index to byte offset
        for (node_ptr, &instr_idx) in &self.nodes_seen {
            let byte_offset = self.pending[instr_idx].offset;
            // SAFETY: We hold the only reference to these nodes during compilation
            unsafe { (**node_ptr).borrow_mut().offset = byte_offset };
        }

        // Phase 3: Emit final bytecode
        let scratch = scratch_arena(None);
        let mut instructions = Vec::new();
        for pi in &self.pending {
            instructions.extend(pi.instr.encode(&scratch));
        }

        // Build entrypoints from function offsets
        let entrypoints = compiler
            .functions
            .iter()
            .filter(|f| f.public)
            .map(|f| {
                let instr_idx = self.functions_seen[f.name];
                Entrypoint {
                    name: f.name.to_string(),
                    display_name: f.attributes.display_name.unwrap_or(f.name).to_string(),
                    paths: f.attributes.paths.iter().map(|s| s.to_string()).collect(),
                    address: self.pending[instr_idx].offset,
                }
            })
            .collect();

        let charsets =
            compiler.charsets.iter().filter(|h| h.usage_count != 0).map(|h| h.value).collect();
        let strings =
            compiler.strings.iter().filter(|h| h.usage_count != 0).map(|h| h.value).collect();
        let highlight_kinds = compiler
            .highlight_kinds
            .iter()
            .filter(|h| h.usage_count != 0)
            .map(|h| h.value)
            .collect();

        Ok(Assembly { instructions, entrypoints, charsets, strings, highlight_kinds })
    }

    /// Iterative relaxation: compute byte offsets until stable.
    ///
    /// Addresses can only decrease when instructions shrink (smaller varints),
    /// so this is guaranteed to converge.
    fn relax(&mut self) {
        loop {
            // Resolve targets and compute new instruction sizes
            let mut changed = false;
            for i in 0..self.pending.len() {
                if let Some(target) = self.pending[i].target {
                    let target_offset = self.resolve_target(target);
                    let old_instr = self.pending[i].instr;
                    self.pending[i].instr = Self::patch_target(old_instr, target_offset as u32);
                }
            }

            // Recompute byte offsets
            let mut offset = 0;
            for pi in &mut self.pending {
                if pi.offset != offset {
                    pi.offset = offset;
                    changed = true;
                }
                offset += pi.instr.encoded_size();
            }

            if !changed {
                break;
            }
        }
    }

    /// Resolve a symbolic jump target to its current byte offset.
    fn resolve_target(&self, target: JumpTarget<'a>) -> usize {
        match target {
            JumpTarget::ByName(name) => {
                let instr_idx = self.functions_seen[name];
                self.pending[instr_idx].offset
            }
            JumpTarget::ByNode(node) => {
                let instr_idx = self.nodes_seen[&(node as *const _)];
                self.pending[instr_idx].offset
            }
        }
    }

    /// Patch an instruction's jump target field with a resolved address.
    fn patch_target(instr: Instruction, tgt: u32) -> Instruction {
        use Instruction::*;
        match instr {
            MovImm { dst, .. } if dst == Register::ProgramCounter => MovImm { dst, imm: tgt },
            Call { .. } => Call { tgt },
            JumpEQ { lhs, rhs, .. } => JumpEQ { lhs, rhs, tgt },
            JumpNE { lhs, rhs, .. } => JumpNE { lhs, rhs, tgt },
            JumpLT { lhs, rhs, .. } => JumpLT { lhs, rhs, tgt },
            JumpLE { lhs, rhs, .. } => JumpLE { lhs, rhs, tgt },
            JumpGT { lhs, rhs, .. } => JumpGT { lhs, rhs, tgt },
            JumpGE { lhs, rhs, .. } => JumpGE { lhs, rhs, tgt },
            JumpIfEndOfLine { .. } => JumpIfEndOfLine { tgt },
            JumpIfMatchCharset { idx, min, max, .. } => JumpIfMatchCharset { idx, min, max, tgt },
            JumpIfMatchPrefix { idx, .. } => JumpIfMatchPrefix { idx, tgt },
            JumpIfMatchPrefixInsensitive { idx, .. } => JumpIfMatchPrefixInsensitive { idx, tgt },
            other => other,
        }
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

    /// Generate instructions for a function (assumes registers already allocated).
    fn generate_code(&mut self, function: &Function<'a>) -> CompileResult<()> {
        use Instruction::*;

        let mut stack: VecDeque<IRCell<'a>> = VecDeque::new();
        stack.push_back(function.body);

        while let Some(start_cell) = stack.pop_front() {
            if start_cell.borrow().offset != usize::MAX {
                // Already serialized
                continue;
            }

            // Track the current node as we follow the chain
            let mut current_cell = start_cell;

            loop {
                let mut ir = current_cell.borrow_mut();

                // Track this IR node's instruction index
                let instr_idx = self.pending.len();
                ir.offset = instr_idx;
                self.nodes_seen.insert(current_cell as *const _, instr_idx);

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
                            let imm = kind.id;
                            self.push_instruction(MovImm { dst, imm });
                        }
                    }
                    IRI::AddImm { dst, imm } => {
                        if let Some(dst) = dst.borrow().physical {
                            self.push_instruction(AddImm { dst, imm });
                        }
                    }
                    IRI::If { condition, then } => {
                        stack.push_back(then);

                        debug_assert!(!std::ptr::eq(current_cell, then));

                        let target = JumpTarget::ByNode(then);
                        match condition {
                            Condition::Cmp { lhs, rhs, op } => {
                                let lhs_phys = lhs.borrow().physical.unwrap();
                                let rhs_phys = rhs.borrow().physical.unwrap();

                                let instr = match op {
                                    ComparisonOp::Eq => {
                                        JumpEQ { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                    ComparisonOp::Ne => {
                                        JumpNE { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                    ComparisonOp::Lt => {
                                        JumpLT { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                    ComparisonOp::Gt => {
                                        JumpGT { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                    ComparisonOp::Le => {
                                        JumpLE { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                    ComparisonOp::Ge => {
                                        JumpGE { lhs: lhs_phys, rhs: rhs_phys, tgt: 0 }
                                    }
                                };
                                self.push_instruction_with_target(instr, target);
                            }
                            Condition::EndOfLine => {
                                self.push_instruction_with_target(
                                    JumpIfEndOfLine { tgt: 0 },
                                    target,
                                );
                            }
                            Condition::Charset { cs, min, max } => {
                                let idx = cs.id;
                                self.push_instruction_with_target(
                                    JumpIfMatchCharset { idx, min, max, tgt: 0 },
                                    target,
                                );
                            }
                            Condition::Prefix(s) => {
                                let idx = s.id;
                                self.push_instruction_with_target(
                                    JumpIfMatchPrefix { idx, tgt: 0 },
                                    target,
                                );
                            }
                            Condition::PrefixInsensitive(s) => {
                                let idx = s.id;
                                self.push_instruction_with_target(
                                    JumpIfMatchPrefixInsensitive { idx, tgt: 0 },
                                    target,
                                );
                            }
                        }
                    }
                    IRI::Call { name } => {
                        self.push_instruction_with_target(
                            Call { tgt: 0 },
                            JumpTarget::ByName(name),
                        );
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

                drop(ir);

                if next.borrow().offset != usize::MAX {
                    // Backward jump to already-visited node
                    self.push_instruction_with_target(
                        MovImm { dst: Register::ProgramCounter, imm: 0 },
                        JumpTarget::ByNode(next),
                    );
                    break;
                }

                // Continue following the chain
                current_cell = next;
            }
        }

        Ok(())
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.pending.push(PendingInstruction::new(instr));
    }

    fn push_instruction_with_target(&mut self, instr: Instruction, target: JumpTarget<'a>) {
        self.pending.push(PendingInstruction::with_target(instr, target));
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
