// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! IR optimization passes, run before register allocation.
//!
//! ## Quirks
//!
//! - Pass order matters
//!
//! ## TODO
//!
//! - Could do copy propagation: if `vreg1 = vreg2`, replace all uses of vreg1 with vreg2.
//! - Could merge consecutive `Add { off, off, 1 }` instructions.
//! - Could eliminate unreachable code after `Return`.

use std::collections::HashSet;

use super::ir::*;
use crate::runtime::Register;

pub fn optimize(program: &mut Program) {
    // Remove noops first, such that analyzing instruction chains becomes easier for the other passes.
    optimize_noop(program);
    optimize_redundant_offset_backup_restore(program);
    optimize_highlight_kind_values(program);
}

/// Removes no-op instructions from the IR.
fn optimize_noop(program: &mut Program) {
    fn skip_noops(graph: &Graph, mut target: NodeId) -> NodeId {
        while matches!(graph[target].instr, Op::Noop)
            && let Some(next) = graph[target].next
        {
            target = next;
        }
        target
    }

    // Remove noops from the function entrypoint (the trunk of the tree).
    for function in &mut program.functions {
        function.body = skip_noops(&program.graph, function.body);
    }

    for function in &program.functions {
        let mut visitor = program.visit_nodes_from(function.body);
        while let Some(current) = visitor.next(&program.graph) {
            // First, filter down to nodes that are not no-ops.
            if matches!(program.graph[current].instr, Op::Noop) {
                continue;
            }

            if let Some(next) = program.graph[current].next {
                let next = skip_noops(&program.graph, next);
                program.graph[current].next = Some(next);
            }

            // `Op::If` nodes have an additional "then" branch.
            if let Op::If { then, .. } = program.graph[current].instr {
                let target = skip_noops(&program.graph, then);
                if let Op::If { then, .. } = &mut program.graph[current].instr {
                    *then = target;
                }
            }
        }
    }
}

// Conditions in the VM advance the offset only if they match. The frontend doesn't
// care about this and emits pointless backup/restore instructions for the offset.
// This code is responsible for turning this chain of `.next` pointers:
//   Op::Add { off -> backup }
//   Op::If { .. }
//   Op::Add { backup -> off }
//   Op::If { .. }
//   Op::Add { backup -> off }
//   Op::If { .. }
//   Op::Add { backup -> off }
// into this:
//   Op::Add { off -> backup }
//   Op::If { .. }
//   Op::If { .. }
//   Op::If { .. }
fn optimize_redundant_offset_backup_restore(program: &mut Program) {
    let off_reg = program.get_reg(Register::InputOffset);

    // Remove pointless offset restore chains.
    for function in &program.functions {
        let mut visitor = program.visit_nodes_from(function.body);
        while let Some(current_cell) = visitor.next(&program.graph) {
            // First, filter down to nodes that assign the `off` to a virtual register.
            if let save = &program.graph[current_cell]
                && let Op::Mov { dst: backup_reg, src } = save.instr
                && src == off_reg
                && !backup_reg.is_physical()
            {
                let mut next_cond = save.next;

                // Next optimize an entire chain of `if` conditions that pointlessly restore `off`.
                while let Some(cond_id) = next_cond
                    && let cond = &program.graph[cond_id]
                    && matches!(cond.instr, Op::If { .. })
                    && let Some(restore) = cond.next
                    && let restore = &program.graph[restore]
                    && matches!(restore.instr, Op::Mov { dst, src } if dst == off_reg && src == backup_reg)
                {
                    let next = restore.next;
                    program.graph[cond_id].next = next;
                    next_cond = next;
                }
            }
        }
    }

    // Remove pointless offset backups.
    // A backup is pointless if the destination vreg is never read.
    for function in &program.functions {
        // First, collect all vregs that are read anywhere in the function.
        let mut used_vregs = HashSet::new();
        let mut visitor = program.visit_nodes_from(function.body);
        while let Some(current_cell) = visitor.next(&program.graph) {
            let current = &program.graph[current_cell];
            match current.instr {
                Op::Mov { src, .. } => {
                    used_vregs.insert(src);
                }
                Op::If { condition: Condition::Cmp { lhs, rhs, .. }, .. } => {
                    used_vregs.insert(lhs);
                    used_vregs.insert(rhs);
                }
                _ => {}
            }
        }

        // Now remove dead stores (assignments to vregs that are never read).
        let mut visitor = program.visit_nodes_from(function.body);
        while let Some(current_cell) = visitor.next(&program.graph) {
            // First, filter down to nodes that assign the `off` to a virtual register.
            if let cell = &program.graph[current_cell]
                && let Op::Mov { dst, src } = cell.instr
                // TODO: Technically we could also optimize vreg --> vreg assignments, but for that we
                // need to be able to call `count_register_uses` multiple times, so that the count is
                // accurate after removing an assignment. Physical registers don't care about that.
                && src.is_physical()
                // We can't optimize physical register --> physical register assignments.
                && !dst.is_physical()
                && !used_vregs.contains(&dst)
            {
                program.graph[current_cell].instr = Op::Noop;
            }
        }
    }
    optimize_noop(program);
}

/// This isn't an optimization for the VM, it's one for my pedantic side.
/// I like it if the identifiers are sorted and the values contiguous.
fn optimize_highlight_kind_values(program: &mut Program) {
    program.highlight_kinds.sort_unstable_by(|a, b| {
        let a = a.identifier.as_str();
        let b = b.identifier.as_str();

        // Global identifiers without a dot come first.
        let nested_a = a.contains('.');
        let nested_b = b.contains('.');
        let cmp = nested_a.cmp(&nested_b);
        if cmp != std::cmp::Ordering::Equal {
            return cmp;
        }

        // Among globals, "other" comes first. Due to the above,
        // `nested_a == false` implies `nested_b == false`.
        if !nested_a {
            if a == "other" {
                return std::cmp::Ordering::Less;
            }
            if b == "other" {
                return std::cmp::Ordering::Greater;
            }
        }

        // Otherwise, sort by dot-separated components.
        a.split('.').cmp(b.split('.'))
    });

    let mut mapping = vec![u32::MAX; program.highlight_kinds.len()];
    for (idx, hk) in program.highlight_kinds.iter_mut().enumerate() {
        let idx = idx as u32;
        mapping[hk.value as usize] = idx;
        hk.value = idx;
    }

    for function in &program.functions {
        let mut visitor = program.visit_nodes_from(function.body);
        while let Some(current) = visitor.next(&program.graph) {
            let current = &mut program.graph[current];

            if let Op::MovKind { kind, .. } = &mut current.instr {
                *kind = mapping[*kind as usize];
            }
        }
    }
}
