// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Backend2: basic-block SSA IR -> bytecode
//!
//! Pipeline:
//! 1. Linearize blocks (DFS order from entry)
//! 2. Compute value liveness across the linear order
//! 3. Linear scan register allocation
//! 4. Phi elimination (insert copies in predecessors)
//! 5. Emit bytecode

#![allow(clippy::collapsible_if)]

use std::collections::{HashMap, HashSet};

use stdext::arena::scratch_arena;

use super::ir::{BlockId, CondKind, FuncBody, InstKind, Term, Value};
use super::{
    Assembly, ComparisonOp, CompileError, CompileResult, Compiler, Entrypoint, FunctionAttributes,
};
use crate::runtime::{Instruction, Register};

// -----------------------------------------------------------------------
// Public entry point
// -----------------------------------------------------------------------

pub struct Backend2<'a> {
    assembly: Assembly<'a>,
    relocations: Vec<Relocation>,
    block_relocs: Vec<(usize, BlockId)>,
    block_offsets: HashMap<BlockId, usize>,
    functions_seen: HashMap<&'a str, usize>,
    charsets_seen: HashMap<*const super::Charset, usize>,
    strings_seen: HashMap<*const str, usize>,
}

#[derive(Debug)]
enum Relocation {
    ByName(usize, String),
}

impl<'a> Default for Backend2<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Backend2<'a> {
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
            block_relocs: Default::default(),
            block_offsets: Default::default(),
            functions_seen: Default::default(),
            charsets_seen: Default::default(),
            strings_seen: Default::default(),
        }
    }

    pub(crate) fn compile(
        mut self,
        compiler: &Compiler<'a>,
        functions: Vec<(&'a str, FuncBody<'a>, FunctionAttributes<'a>, bool)>,
    ) -> CompileResult<Assembly<'a>> {
        for (name, func, _attrs, _public) in &functions {
            let entrypoint_offset = self.assembly.instructions.len();
            self.functions_seen.insert(name, entrypoint_offset);
            self.compile_function(func)?;
            self.process_relocations(func);
        }

        if !self.relocations.is_empty() {
            let names: String = self
                .relocations
                .iter()
                .map(|r| {
                    let Relocation::ByName(_, name) = r;
                    name.as_str()
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Err(CompileError {
                path: String::new(),
                line: 0,
                column: 0,
                message: if !names.is_empty() {
                    format!("unresolved function call names: {names}")
                } else {
                    "unresolved relocations".to_string()
                },
            });
        }

        self.assembly.entrypoints = functions
            .iter()
            .filter(|(_, _, _, public)| *public)
            .map(|(name, _func, attrs, _)| Entrypoint {
                name: name.to_string(),
                display_name: attrs.display_name.unwrap_or(name).to_string(),
                paths: attrs.paths.iter().map(|s| s.to_string()).collect(),
                address: *self.functions_seen.get(name).unwrap(),
            })
            .collect();
        self.assembly.highlight_kinds = compiler.highlight_kinds.clone();

        Ok(self.assembly)
    }

    fn compile_function(&mut self, func: &FuncBody<'a>) -> CompileResult<()> {
        if func.blocks.is_empty() {
            return Ok(());
        }

        // 1. Linearize blocks (DFS from entry).
        let order = linearize_blocks(func);
        let block_index: HashMap<BlockId, usize> =
            order.iter().enumerate().map(|(i, &b)| (b, i)).collect();

        // 2. Collect all values and compute liveness.
        let used_values = collect_used_values(func, &order);
        let (live_start, live_end) = compute_liveness(func, &order, &block_index);

        // 3. Allocate registers.
        let allocation = allocate_registers(func, &order, &block_index, &live_start, &live_end)?;

        // 4. Track block offsets for relocation and record pre-emit cursor.
        let func_start = self.assembly.instructions.len();
        self.block_relocs.clear();
        self.block_offsets.clear();

        // 5. Emit bytecode.
        for (linear_idx, &block_id) in order.iter().enumerate() {
            self.block_offsets.insert(block_id, self.assembly.instructions.len());
            let block = func.block(block_id);

            // Emit instructions.
            for inst in &block.insts {
                if !used_values.contains(&inst.dst) {
                    // Dead value — skip emission.
                    // Exception: side-effecting instructions must still be emitted.
                    match &inst.kind {
                        InstKind::Flush(_)
                        | InstKind::Call(_)
                        | InstKind::AwaitInput
                        | InstKind::ReadOff
                        | InstKind::WriteOff(_)
                        | InstKind::WriteHs(_)
                        | InstKind::AdvanceOff(_)
                        | InstKind::SetOffImm(_) => {} // emit anyway
                        _ => continue,
                    }
                }

                let dst_reg = allocation.get(&inst.dst).copied();

                match &inst.kind {
                    InstKind::Imm(v) => {
                        if let Some(dst) = dst_reg {
                            self.push_instruction(Instruction::MovImm { dst, imm: *v });
                        }
                    }
                    InstKind::Copy(src) => {
                        if let (Some(dst), Some(&src)) = (dst_reg, allocation.get(src)) {
                            if dst != src {
                                self.push_instruction(Instruction::Mov { dst, src });
                            }
                        }
                    }
                    InstKind::AddImm(src, imm) => {
                        if let Some(dst) = dst_reg {
                            let src_reg = allocation.get(src).copied().unwrap_or(dst);
                            if dst != src_reg {
                                self.push_instruction(Instruction::Mov { dst, src: src_reg });
                            }
                            self.push_instruction(Instruction::AddImm { dst, imm: *imm });
                        }
                    }
                    InstKind::HlKind(k) => {
                        if let Some(dst) = dst_reg {
                            self.push_instruction(Instruction::MovImm { dst, imm: *k });
                        }
                    }
                    InstKind::Flush(val) => {
                        let kind = allocation.get(val).copied().unwrap();
                        self.push_instruction(Instruction::FlushHighlight { kind });
                    }
                    InstKind::Call(name) => {
                        let tgt = self.dst_by_name(name);
                        self.push_instruction(Instruction::Call { tgt: tgt as u32 });
                    }
                    InstKind::AwaitInput => {
                        self.push_instruction(Instruction::AwaitInput);
                    }
                    InstKind::ReadOff => {
                        if let Some(dst) = dst_reg {
                            if dst != Register::InputOffset {
                                self.push_instruction(Instruction::Mov {
                                    dst,
                                    src: Register::InputOffset,
                                });
                            }
                        }
                    }
                    InstKind::WriteOff(val) => {
                        let src = allocation.get(val).copied().unwrap();
                        if src != Register::InputOffset {
                            self.push_instruction(Instruction::Mov {
                                dst: Register::InputOffset,
                                src,
                            });
                        }
                    }
                    InstKind::WriteHs(val) => {
                        let src = allocation.get(val).copied().unwrap();
                        self.push_instruction(Instruction::Mov {
                            dst: Register::HighlightStart,
                            src,
                        });
                    }
                    InstKind::AdvanceOff(n) => {
                        self.push_instruction(Instruction::AddImm {
                            dst: Register::InputOffset,
                            imm: *n,
                        });
                    }
                    InstKind::SetOffImm(n) => {
                        self.push_instruction(Instruction::MovImm {
                            dst: Register::InputOffset,
                            imm: *n,
                        });
                    }
                }
            }

            // Emit terminator.
            let next_block = order.get(linear_idx + 1).copied();
            match block.term.as_ref() {
                Some(Term::Jump { target, args }) => {
                    // Phi elimination: emit parallel copies for block params.
                    let target_params = &func.block(*target).params;
                    self.emit_parallel_copies(target_params, args, &allocation);

                    if Some(*target) != next_block {
                        let tgt = self.dst_by_block(*target) as u32;
                        self.push_instruction(Instruction::MovImm {
                            dst: Register::ProgramCounter,
                            imm: tgt,
                        });
                        self.record_block_reloc(*target);
                    }
                }
                Some(Term::CondBranch { kind, then_block, then_args, else_block, else_args }) => {
                    // If then_args is non-empty, we need a trampoline: the condition
                    // jumps to a trampoline that does the copies then jumps to then_block.
                    // We'll emit the trampoline after the else path.
                    let need_then_trampoline = {
                        let then_params = &func.block(*then_block).params;
                        then_args.iter().enumerate().any(|(i, arg)| {
                            if let (Some(&dst), Some(&src)) = (
                                then_params.get(i).and_then(|p| allocation.get(p)),
                                allocation.get(arg),
                            ) {
                                dst != src
                            } else {
                                false
                            }
                        })
                    };

                    // If we need a trampoline, the condition jumps there instead.
                    // We'll record where to patch the target after emitting the trampoline.
                    let cond_tgt = if need_then_trampoline {
                        0u32 // placeholder, will be patched
                    } else {
                        self.dst_by_block(*then_block) as u32
                    };

                    match kind {
                        CondKind::Cmp { lhs, rhs, op } => {
                            let lhs = allocation[lhs];
                            let rhs = allocation[rhs];
                            match op {
                                ComparisonOp::Eq => self.push_instruction(Instruction::JumpEQ {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                                ComparisonOp::Ne => self.push_instruction(Instruction::JumpNE {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                                ComparisonOp::Lt => self.push_instruction(Instruction::JumpLT {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                                ComparisonOp::Gt => self.push_instruction(Instruction::JumpGT {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                                ComparisonOp::Le => self.push_instruction(Instruction::JumpLE {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                                ComparisonOp::Ge => self.push_instruction(Instruction::JumpGE {
                                    lhs,
                                    rhs,
                                    tgt: cond_tgt,
                                }),
                            }
                        }
                        CondKind::EndOfLine => {
                            self.push_instruction(Instruction::JumpIfEndOfLine { tgt: cond_tgt });
                        }
                        CondKind::Charset { cs, min, max } => {
                            let idx = self.visit_charset(cs) as u32;
                            self.push_instruction(Instruction::JumpIfMatchCharset {
                                idx,
                                min: *min,
                                max: *max,
                                tgt: cond_tgt,
                            });
                        }
                        CondKind::Prefix(s) => {
                            let idx = self.visit_string(s) as u32;
                            self.push_instruction(Instruction::JumpIfMatchPrefix {
                                idx,
                                tgt: cond_tgt,
                            });
                        }
                        CondKind::PrefixInsensitive(s) => {
                            let idx = self.visit_string(s) as u32;
                            self.push_instruction(Instruction::JumpIfMatchPrefixInsensitive {
                                idx,
                                tgt: cond_tgt,
                            });
                        }
                    }

                    if !need_then_trampoline {
                        self.record_block_reloc(*then_block);
                    }
                    // Record where the condition instruction's tgt field is, for trampoline patching.
                    let cond_tgt_patch_offset = self.assembly.instructions.len() - 4;

                    // Else path: emit else_args copies (fallthrough).
                    {
                        let else_params = &func.block(*else_block).params;
                        self.emit_parallel_copies(else_params, else_args, &allocation);
                    }

                    if Some(*else_block) != next_block {
                        let tgt = self.dst_by_block(*else_block) as u32;
                        self.push_instruction(Instruction::MovImm {
                            dst: Register::ProgramCounter,
                            imm: tgt,
                        });
                        self.record_block_reloc(*else_block);
                    }

                    // Then trampoline: emit then_args copies + jump to then_block.
                    if need_then_trampoline {
                        let trampoline_offset = self.assembly.instructions.len();
                        // Patch the condition's jump target to point here.
                        self.assembly.instructions
                            [cond_tgt_patch_offset..cond_tgt_patch_offset + 4]
                            .copy_from_slice(&(trampoline_offset as u32).to_le_bytes());

                        let then_params = &func.block(*then_block).params;
                        self.emit_parallel_copies(then_params, then_args, &allocation);

                        let tgt = self.dst_by_block(*then_block) as u32;
                        self.push_instruction(Instruction::MovImm {
                            dst: Register::ProgramCounter,
                            imm: tgt,
                        });
                        self.record_block_reloc(*then_block);
                    }
                }
                Some(Term::Return) => {
                    self.push_instruction(Instruction::Return);
                }
                None => {
                    self.push_instruction(Instruction::Return);
                }
            }
        }

        // Resolve block relocations.
        for (offset, block_id) in &self.block_relocs {
            if let Some(&target) = self.block_offsets.get(block_id) {
                let range = *offset..*offset + 4;
                if let Some(target_bytes) = self.assembly.instructions.get_mut(range) {
                    let bytes = (target as u32).to_le_bytes();
                    target_bytes.copy_from_slice(&bytes);
                }
            }
        }
        self.block_relocs.clear();
        self.block_offsets.clear();

        // Peephole: remove identity movs (mov rX, rX) from the function's bytecode.
        peephole_remove_identity_movs(&mut self.assembly.instructions, func_start);

        Ok(())
    }

    fn push_instruction(&mut self, instr: Instruction) {
        let scratch = scratch_arena(None);
        self.assembly.instructions.extend(instr.encode(&scratch));
    }

    fn visit_charset(&mut self, cs: &'a super::Charset) -> usize {
        let ptr = cs as *const _;
        if let Some(&idx) = self.charsets_seen.get(&ptr) {
            return idx;
        }
        let idx = self.assembly.charsets.len();
        self.assembly.charsets.push(cs);
        self.charsets_seen.insert(ptr, idx);
        idx
    }

    fn visit_string(&mut self, s: &'a str) -> usize {
        let ptr = s as *const _;
        if let Some(&idx) = self.strings_seen.get(&ptr) {
            return idx;
        }
        let idx = self.assembly.strings.len();
        self.assembly.strings.push(s);
        self.strings_seen.insert(ptr, idx);
        idx
    }

    fn dst_by_name(&mut self, name: &str) -> usize {
        if let Some(&offset) = self.functions_seen.get(name) {
            offset
        } else {
            self.relocations
                .push(Relocation::ByName(self.assembly.instructions.len(), name.to_string()));
            0
        }
    }

    fn dst_by_block(&mut self, block: BlockId) -> usize {
        if let Some(&offset) = self.block_offsets.get(&block) {
            return offset;
        }
        // Forward reference — caller must call record_block_reloc() after push_instruction.
        0
    }

    /// Record a relocation for the last 4 bytes emitted (the `tgt` field of the just-pushed instruction).
    fn record_block_reloc(&mut self, block: BlockId) {
        if !self.block_offsets.contains_key(&block) {
            self.block_relocs.push((self.assembly.instructions.len() - 4, block));
        }
    }

    fn process_relocations(&mut self, _func: &FuncBody<'a>) {
        self.relocations.retain(|reloc| {
            let Relocation::ByName(offset, name) = reloc;
            if let Some(&target) = self.functions_seen.get(name.as_str()) {
                let range = *offset..*offset + 4;
                if let Some(target_bytes) = self.assembly.instructions.get_mut(range) {
                    let bytes = (target as u32).to_le_bytes();
                    target_bytes.copy_from_slice(&bytes);
                }
                false
            } else {
                true
            }
        });
    }

    /// Emit a set of parallel copies `params[i] ← args[i]` in a correct sequential order.
    ///
    /// Naively emitting `mov dst, src` for each pair can fail when copies form
    /// cycles (e.g. `x3←x5, x5←x3`). This implements a proper sequentialization:
    ///
    /// 1. Build the set of copies `(dst_reg, src_reg)`, filtering identity copies.
    /// 2. Emit copies whose destination is not anyone's source first (leaves).
    /// 3. For remaining cycles, break with a scratch register (`mov tmp, src; ...`).
    fn emit_parallel_copies(
        &mut self,
        params: &[Value],
        args: &[Value],
        allocation: &HashMap<Value, Register>,
    ) {
        // Collect non-identity copies as (dst_reg, src_reg).
        let mut copies: Vec<(Register, Register)> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            if let (Some(&dst), Some(&src)) =
                (params.get(i).and_then(|p| allocation.get(p)), allocation.get(arg))
            {
                if dst != src {
                    copies.push((dst, src));
                }
            }
        }

        if copies.is_empty() {
            return;
        }

        // Repeatedly emit copies whose dst is not used as a src by any remaining copy.
        let mut emitted = true;
        while emitted {
            emitted = false;
            let mut i = 0;
            while i < copies.len() {
                let (dst, _) = copies[i];
                let dst_is_src = copies.iter().any(|&(_, s)| s == dst);
                if !dst_is_src {
                    let (dst, src) = copies.remove(i);
                    self.push_instruction(Instruction::Mov { dst, src });
                    emitted = true;
                } else {
                    i += 1;
                }
            }
        }

        // Any remaining copies form cycles. Break each cycle with a temp.
        // Find a scratch register not involved in any copy.
        while !copies.is_empty() {
            let (first_dst, _first_src) = copies[0];

            let scratch = (Register::FIRST_USER_REG..Register::COUNT)
                .map(Register::from_usize)
                .find(|r| !copies.iter().any(|&(d, s)| d == *r || s == *r))
                .expect("no scratch register available for cycle breaking");

            // Save the first destination into scratch.
            self.push_instruction(Instruction::Mov { dst: scratch, src: first_dst });

            // Walk the cycle: A←B, B←C, ..., Z←A → emit B←C, ..., Z←scratch
            let mut current_dst = first_dst;
            loop {
                let idx = copies.iter().position(|&(d, _)| d == current_dst);
                let Some(idx) = idx else { break };
                let (_, src) = copies.remove(idx);
                let actual_src = if src == first_dst { scratch } else { src };
                self.push_instruction(Instruction::Mov { dst: current_dst, src: actual_src });
                if src == first_dst {
                    break; // cycle complete
                }
                current_dst = src;
            }
        }
    }
}

// -----------------------------------------------------------------------
// Peephole optimizations
// -----------------------------------------------------------------------

/// Remove `mov rX, rX` identity instructions from bytecode and fix up all jump targets.
///
/// Only identity movs are safe to remove without control-flow analysis — they are
/// no-ops regardless of how execution reaches them.
fn peephole_remove_identity_movs(bytecode: &mut Vec<u8>, start: usize) {
    let end = bytecode.len();

    // Phase 1: Find identity movs.
    let mut removals: Vec<(usize, usize)> = Vec::new();
    let mut off = start;
    while off < end {
        let (instr, len) = Instruction::decode(&bytecode[off..]);
        if instr.is_none() {
            break;
        }
        if let Some(Instruction::Mov { dst, src }) = instr {
            if dst == src {
                removals.push((off, len));
            }
        }
        off += len;
    }

    if removals.is_empty() {
        return;
    }

    // Phase 2: Build an offset remap table for the function region.
    // For each byte position in [start, end), compute how many bytes were removed before it.
    let mut removed_before = vec![0usize; end - start + 1];
    let mut cum = 0usize;
    let mut removal_idx = 0;
    for pos in start..end {
        if removal_idx < removals.len() && pos == removals[removal_idx].0 {
            cum += removals[removal_idx].1;
            removal_idx += 1;
        }
        removed_before[pos - start] = cum;
    }
    removed_before[end - start] = cum;

    let remap = |old_offset: usize| -> usize {
        if old_offset >= start && old_offset <= end {
            old_offset - removed_before[old_offset - start]
        } else {
            old_offset // outside this function
        }
    };

    // Phase 3: Fix up all jump targets in the bytecode.
    off = start;
    while off < end {
        let (instr, len) = Instruction::decode(&bytecode[off..]);
        if instr.is_none() {
            break;
        }

        // Patch the target field of jump instructions.
        // The target is always the last 4 bytes of the instruction encoding.
        let has_target = matches!(
            instr,
            Some(
                Instruction::MovImm { dst: Register::ProgramCounter, .. }
                    | Instruction::Call { .. }
                    | Instruction::JumpEQ { .. }
                    | Instruction::JumpNE { .. }
                    | Instruction::JumpLT { .. }
                    | Instruction::JumpLE { .. }
                    | Instruction::JumpGT { .. }
                    | Instruction::JumpGE { .. }
                    | Instruction::JumpIfEndOfLine { .. }
                    | Instruction::JumpIfMatchCharset { .. }
                    | Instruction::JumpIfMatchPrefix { .. }
                    | Instruction::JumpIfMatchPrefixInsensitive { .. }
            )
        );

        if has_target {
            let tgt_off = off + len - 4;
            let old_target =
                u32::from_le_bytes(bytecode[tgt_off..tgt_off + 4].try_into().unwrap()) as usize;
            let new_target = remap(old_target);
            bytecode[tgt_off..tgt_off + 4].copy_from_slice(&(new_target as u32).to_le_bytes());
        }

        off += len;
    }

    // Phase 4: Remove the identity mov bytes (in reverse order to preserve indices).
    for &(offset, len) in removals.iter().rev() {
        bytecode.drain(offset..offset + len);
    }
}

// -----------------------------------------------------------------------
// Block linearization
// -----------------------------------------------------------------------

fn linearize_blocks(func: &FuncBody<'_>) -> Vec<BlockId> {
    let mut order = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = vec![BlockId(0)];

    while let Some(block_id) = stack.pop() {
        if !visited.insert(block_id) {
            continue;
        }
        order.push(block_id);

        let block = func.block(block_id);
        match block.term.as_ref() {
            Some(Term::Jump { target, .. }) => {
                stack.push(*target);
            }
            Some(Term::CondBranch { then_block, else_block, .. }) => {
                // Push then first so else gets visited first (DFS).
                // This ensures else_block is the fallthrough (immediately after the condition),
                // which matches the bytecode layout: the jump instruction targets then_block,
                // and we fall through to else_block.
                stack.push(*then_block);
                stack.push(*else_block);
            }
            _ => {}
        }
    }

    order
}

// -----------------------------------------------------------------------
// Liveness & register allocation
// -----------------------------------------------------------------------

fn collect_used_values(func: &FuncBody<'_>, order: &[BlockId]) -> HashSet<Value> {
    let mut used = HashSet::new();

    for &block_id in order {
        let block = func.block(block_id);
        for inst in &block.insts {
            match &inst.kind {
                InstKind::Copy(v)
                | InstKind::WriteOff(v)
                | InstKind::WriteHs(v)
                | InstKind::Flush(v) => {
                    used.insert(*v);
                }
                InstKind::AddImm(v, _) => {
                    used.insert(*v);
                }
                _ => {}
            }
        }
        if let Some(term) = &block.term {
            match term {
                Term::Jump { args, .. } => {
                    for v in args {
                        used.insert(*v);
                    }
                }
                Term::CondBranch { kind, then_args, else_args, .. } => {
                    if let CondKind::Cmp { lhs, rhs, .. } = kind {
                        used.insert(*lhs);
                        used.insert(*rhs);
                    }
                    for v in then_args.iter().chain(else_args.iter()) {
                        used.insert(*v);
                    }
                }
                Term::Return => {}
            }
        }
    }

    used
}

fn compute_liveness(
    func: &FuncBody<'_>,
    order: &[BlockId],
    _block_index: &HashMap<BlockId, usize>,
) -> (HashMap<Value, usize>, HashMap<Value, usize>) {
    let mut start: HashMap<Value, usize> = HashMap::new();
    let mut end: HashMap<Value, usize> = HashMap::new();

    for (linear_idx, &block_id) in order.iter().enumerate() {
        let block = func.block(block_id);

        // Block params are defined at block entry.
        for &val in &block.params {
            start.entry(val).or_insert(linear_idx);
            end.insert(val, end.get(&val).copied().unwrap_or(linear_idx).max(linear_idx));
        }

        // Instructions.
        for inst in &block.insts {
            start.entry(inst.dst).or_insert(linear_idx);
            end.insert(inst.dst, end.get(&inst.dst).copied().unwrap_or(linear_idx).max(linear_idx));

            // Uses extend the live range of the source.
            for &v in inst_uses(&inst.kind) {
                end.insert(v, end.get(&v).copied().unwrap_or(linear_idx).max(linear_idx));
            }
        }

        // Terminator uses.
        if let Some(term) = &block.term {
            for v in term_uses(term) {
                end.insert(v, end.get(&v).copied().unwrap_or(linear_idx).max(linear_idx));
            }
            // Args must be live at least until the block that passes them.
            // For back-edges (target is earlier in linear order), the value must
            // survive from its definition to this block — not to the target.
            match term {
                Term::Jump { args, .. } => {
                    for v in args {
                        end.insert(*v, end.get(v).copied().unwrap_or(linear_idx).max(linear_idx));
                    }
                }
                Term::CondBranch { then_args, else_args, .. } => {
                    for v in then_args.iter().chain(else_args.iter()) {
                        end.insert(*v, end.get(v).copied().unwrap_or(linear_idx).max(linear_idx));
                    }
                }
                _ => {}
            }
        }
    }

    (start, end)
}

fn inst_uses<'a>(kind: &'a InstKind<'_>) -> &'a [Value] {
    // Return a slice of values used by this instruction.
    // We use a small trick: for variants with one Value, return a slice of length 1.
    match kind {
        InstKind::Copy(v) | InstKind::WriteOff(v) | InstKind::WriteHs(v) | InstKind::Flush(v) => {
            std::slice::from_ref(v)
        }
        InstKind::AddImm(v, _) => std::slice::from_ref(v),
        _ => &[],
    }
}

fn term_uses(term: &Term<'_>) -> Vec<Value> {
    let mut uses = Vec::new();
    if let Term::CondBranch { kind: CondKind::Cmp { lhs, rhs, .. }, .. } = term {
        uses.push(*lhs);
        uses.push(*rhs);
    }
    uses
}

struct LiveInterval {
    value: Value,
    start: usize,
    end: usize,
}

fn allocate_registers(
    _func: &FuncBody<'_>,
    _order: &[BlockId],
    _block_index: &HashMap<BlockId, usize>,
    live_start: &HashMap<Value, usize>,
    live_end: &HashMap<Value, usize>,
) -> CompileResult<HashMap<Value, Register>> {
    let mut allocation: HashMap<Value, Register> = HashMap::new();

    // Collect all values that need registers.
    let mut intervals: Vec<LiveInterval> = Vec::new();

    for (&val, &start) in live_start {
        let end = live_end.get(&val).copied().unwrap_or(start);
        intervals.push(LiveInterval { value: val, start, end });
    }

    // Sort by start position.
    intervals.sort_by_key(|i| (i.start, i.value.0));

    // Available user registers.
    let mut available: Vec<Register> =
        (Register::FIRST_USER_REG..Register::COUNT).rev().map(Register::from_usize).collect();
    let mut active: Vec<LiveInterval> = Vec::new();

    for interval in intervals {
        // Check if this value is produced by ReadOff — it can use InputOffset directly
        // if it's immediately consumed by a WriteOff or comparison.
        // For simplicity, always allocate a user register.

        // Expire intervals that ended before this one starts.
        active.retain(|active_interval| {
            if active_interval.end < interval.start {
                if let Some(&reg) = allocation.get(&active_interval.value) {
                    if reg as usize >= Register::FIRST_USER_REG {
                        available.push(reg);
                    }
                }
                false
            } else {
                true
            }
        });

        if let Some(reg) = available.pop() {
            allocation.insert(interval.value, reg);
            active.push(interval);
            active.sort_by_key(|i| (i.end, i.value.0));
        } else {
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
