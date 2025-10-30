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
        while let Node::Add { next: Some(next), dst: Register::Zero, .. } = *function.body.borrow()
        {
            function.body = next;
        }
    }

    for function in &compiler.functions {
        for cell in compiler.visit_nodes_from(function.body) {
            if let node = cell.borrow()
                && !matches!(*node, Node::Add { dst: Register::Zero, .. })
            {
                candidates.push(cell);
            }
        }
    }

    for cell in candidates {
        let mut node = cell.borrow_mut();
        let slots = match &mut *node {
            Node::Add { next: Some(next), .. } => [Some(next), None],
            Node::If { next: Some(next), then, .. } => [Some(next), Some(then)],
            Node::Call { next: Some(next), .. } => [Some(next), None],
            Node::Flush { next: Some(next) } => [Some(next), None],
            Node::Add { .. }
            | Node::If { .. }
            | Node::Call { .. }
            | Node::Return
            | Node::Flush { .. } => [None, None],
        };

        for next in slots.into_iter().flatten() {
            while let Node::Add { next: Some(noop_next), dst: Register::Zero, .. } = *next.borrow()
            {
                *next = noop_next;
            }
        }
    }
}
