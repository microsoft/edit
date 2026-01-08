// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::RefMut;
use std::iter::repeat_n;
use std::ptr;

use stdext::arena::scratch_arena;

use super::*;

pub fn optimize<'a>(compiler: &mut Compiler<'a>) {
    optimize_noop(compiler);
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

    // Remove noops from the rest of the tree.
    for function in &compiler.functions {
        for current_cell in compiler.visit_nodes_from(function.body) {
            // First, filter down to nodes that are not no-ops.
            if let mut current = current_cell.borrow_mut()
                && !matches!(current.instr, IRI::Noop)
            {
                // `IRI::If` nodes have an additional "next" pointer.
                let current = &mut *current;
                let mut nexts = [
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

    let hk_dst = compiler.get_reg(Register::HighlightKind);

    for function in &compiler.functions {
        for current in compiler.visit_nodes_from(function.body) {
            let mut current = current.borrow_mut();

            if let IRI::Add { dst, src, imm } = &mut current.instr
                && ptr::eq(*dst, hk_dst)
            {
                *imm = mapping[*imm as usize];
            }
        }
    }
}
