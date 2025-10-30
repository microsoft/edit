use std::cell::RefMut;

use stdext::arena::scratch_arena;

use super::*;

pub fn optimize<'a>(compiler: &mut Compiler<'a>) {
    optimize_noop(compiler);
}

fn optimize_noop<'a>(compiler: &mut Compiler<'a>) {
    let scratch = scratch_arena(None);
    let mut candidates = Vec::new_in(&*scratch);

    // Remove noops from the function entrypoint (the trunk of the tree).
    for function in &mut compiler.functions {
        while let body = function.body.borrow()
            && let Some(next) = body.next
            && matches!(body.instr, IRI::Add { dst: Register::Zero, .. })
        {
            function.body = next;
        }
    }

    for function in &compiler.functions {
        for cell in compiler.visit_nodes_from(function.body) {
            if let node = cell.borrow()
                && !matches!(node.instr, IRI::Add { dst: Register::Zero, .. })
            {
                candidates.push(cell);
            }
        }
    }

    for cell in candidates {
        let (mut next, mut instr) =
            RefMut::map_split(cell.borrow_mut(), |n| (&mut n.next, &mut n.instr));
        let slots = [
            next.as_mut(),
            match &mut *instr {
                IRI::If { then, .. } => Some(then),
                _ => None,
            },
        ];

        for next in slots.into_iter().flatten() {
            while let n = next.borrow()
                && let Some(noop_next) = n.next
                && matches!(n.instr, IRI::Add { dst: Register::Zero, .. })
            {
                *next = noop_next;
            }
        }
    }
}
