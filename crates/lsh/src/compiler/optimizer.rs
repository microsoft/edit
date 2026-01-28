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

use std::ptr;

use stdext::arena::scratch_arena;

use super::*;

pub fn optimize<'a>(compiler: &mut Compiler<'a>) {
    // Remove noops first, such that analyzing instruction chains becomes easier for the other passes.
    optimize_noop(compiler);
    optimize_redundant_offset_backup_restore(compiler);
    optimize_highlight_kind_values(compiler);
}

/// Removes no-op instructions from the IR.
fn optimize_noop<'a>(compiler: &mut Compiler<'a>) {
    // Remove noops from the function entrypoint (the trunk of the tree).
    for function in &mut compiler.functions {
        while let body = function.body.borrow()
            && let Some(next) = body.next
            && matches!(body.instr, IRI::Noop)
        {
            function.body = next;
        }
    }

    for function in &compiler.functions {
        for current_cell in compiler.visit_nodes_from(function.body) {
            // First, filter down to nodes that are not no-ops.
            if let mut current = current_cell.borrow_mut()
                && !matches!(current.instr, IRI::Noop)
            {
                // `IRI::If` nodes have an additional "next" pointer.
                let current = &mut *current;
                let nexts = [
                    current.next.as_mut(),
                    match &mut current.instr {
                        IRI::If { then, .. } => Some(then),
                        _ => None,
                    },
                ];

                // Now, "pop_front" no-ops from the next pointer, until it
                // points to a real op (or None, but that shouldn't happen).
                for next_ref in nexts.into_iter().flatten() {
                    while !ptr::eq(*next_ref, current_cell)
                        && let next = next_ref.borrow()
                        && matches!(next.instr, IRI::Noop)
                        && let Some(skip_next) = next.next
                    {
                        *next_ref = skip_next;
                    }
                }
            }
        }
    }
}

// Conditions in the VM advance the offset only if they match. The frontend doesn't
// care about this and emits pointless backup/restore instructions for the offset.
// This code is responsible for turning this chain of `.next` pointers:
//   IRI::Add { off -> backup }
//   IRI::If { .. }
//   IRI::Add { backup -> off }
//   IRI::If { .. }
//   IRI::Add { backup -> off }
//   IRI::If { .. }
//   IRI::Add { backup -> off }
// into this:
//   IRI::Add { off -> backup }
//   IRI::If { .. }
//   IRI::If { .. }
//   IRI::If { .. }
fn optimize_redundant_offset_backup_restore<'a>(compiler: &mut Compiler<'a>) {
    let off_reg = compiler.get_reg(Register::InputOffset);

    // Remove pointless offset restore chains.
    for function in &compiler.functions {
        for current_cell in compiler.visit_nodes_from(function.body) {
            // First, filter down to nodes that assign the `off` to a virtual register.
            if let save = current_cell.borrow()
                && let IRI::Mov { dst: backup_reg, src } = save.instr
                && ptr::eq(src, off_reg)
                && backup_reg.borrow().physical.is_none()
            {
                let mut next_cond = save.next;

                // Next optimize an entire chain of `if` conditions that pointlessly restore `off`.
                while let Some(cond) = next_cond
                    && let mut cond = cond.borrow_mut()
                    && matches!(cond.instr, IRI::If { .. })
                    && let Some(restore) = cond.next
                    && let restore = restore.borrow()
                    && matches!(restore.instr, IRI::Mov { dst, src } if ptr::eq(dst, off_reg) && ptr::eq(src, backup_reg))
                {
                    cond.next = restore.next;
                    next_cond = restore.next;
                }
            }
        }
    }

    // Remove pointless offset backups.
    // A backup is pointless if the destination vreg is never read.
    for function in &compiler.functions {
        // First, collect all vregs that are read anywhere in the function.
        let mut used_vregs = HashSet::new();
        for current_cell in compiler.visit_nodes_from(function.body) {
            let current = current_cell.borrow();
            match current.instr {
                IRI::Mov { src, .. } => {
                    let id = src.borrow().id;
                    used_vregs.insert(id);
                }
                IRI::If { condition: Condition::Cmp { lhs, rhs, .. }, .. } => {
                    used_vregs.insert(lhs.borrow().id);
                    used_vregs.insert(rhs.borrow().id);
                }
                _ => {}
            }
        }

        // Now remove dead stores (assignments to vregs that are never read).
        for current_cell in compiler.visit_nodes_from(function.body) {
            // First, filter down to nodes that assign the `off` to a virtual register.
            if let mut cell = current_cell.borrow_mut()
                && let IRI::Mov { dst, src } = cell.instr
                && let src = src.borrow()
                && let dst = dst.borrow()
                // TODO: Technically we could also optimize vreg --> vreg assignments, but for that we
                // need to be able to call `count_register_uses` multiple times, so that the count is
                // accurate after removing an assignment. Physical registers don't care about that.
                && src.physical.is_some()
                // We can't optimize physical register --> physical register assignments.
                && dst.physical.is_none()
                && !used_vregs.contains(&dst.id)
            {
                cell.instr = IRI::Noop;
            }
        }
    }
    optimize_noop(compiler);
}

/// This isn't an optimization for the VM, it's one for my autistic side.
/// I like it if the identifiers are sorted and the values contiguous.
fn optimize_highlight_kind_values<'a>(compiler: &mut Compiler<'a>) {
    let scratch = scratch_arena(None);
    let mut mapping = Vec::new_in(&*scratch);

    compiler.highlight_kinds.sort_unstable_by(|a, b| {
        let a = a.identifier;
        let b = b.identifier;

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

    mapping.resize(compiler.highlight_kinds.len(), u32::MAX);
    for (idx, hk) in compiler.highlight_kinds.iter_mut().enumerate() {
        let idx = idx as u32;
        mapping[hk.value as usize] = idx;
        hk.value = idx;
    }

    for function in &compiler.functions {
        for current in compiler.visit_nodes_from(function.body) {
            let mut current = current.borrow_mut();

            if let IRI::MovKind { kind, .. } = &mut current.instr {
                *kind = mapping[*kind as usize];
            }
        }
    }
}
