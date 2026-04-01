// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! On-the-fly SSA construction (Braun et al. 2013).
//!
//! Reference: "Simple and Efficient Construction of Static Single Assignment Form"
//! <https://link.springer.com/content/pdf/10.1007/978-3-642-37051-9_6.pdf>
//!
//! ## Usage
//!
//! While building the CFG:
//! - Call [`SSABuilder::declare_block`] for each new block.
//! - Call [`SSABuilder::def_var`] whenever a variable is assigned.
//! - Call [`SSABuilder::use_var`] whenever a variable is read.
//! - Call [`SSABuilder::seal_block`] once all predecessors of a block are known.
//!
//! The builder inserts block parameters (phis) as needed and removes trivial
//! ones on the fly.

use std::collections::HashMap;

use super::ir::{BlockId, FuncBody, Value, Variable};

/// Per-block sealing state.
#[derive(Debug, Clone)]
enum Sealed {
    /// Not yet sealed. Contains incomplete phi entries: `(variable, placeholder_value)`.
    No(Vec<(Variable, Value)>),
    /// All predecessors are known.
    Yes,
}

impl Default for Sealed {
    fn default() -> Self {
        Sealed::No(Vec::new())
    }
}

/// Per-block SSA metadata.
#[derive(Debug, Clone, Default)]
struct BlockData {
    sealed: Sealed,
}

pub struct SSABuilder {
    /// `current_def[(var, block)]` = the Value that `var` holds at the end of `block`.
    current_def: HashMap<(Variable, BlockId), Value>,
    /// Per-block metadata.
    block_data: HashMap<BlockId, BlockData>,
    /// Set of Values that are known to be block parameters (phis).
    /// Maps `phi_value -> block` so we can find operands for trivial-phi removal.
    phi_blocks: HashMap<Value, BlockId>,
}

impl Default for SSABuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SSABuilder {
    pub fn new() -> Self {
        Self { current_def: HashMap::new(), block_data: HashMap::new(), phi_blocks: HashMap::new() }
    }

    /// Declare a new basic block. Must be called before the block is used.
    pub fn declare_block(&mut self, block: BlockId) {
        self.block_data.entry(block).or_default();
    }

    /// Record a definition of `var` in `block`.
    pub fn def_var(&mut self, var: Variable, val: Value, block: BlockId) {
        self.current_def.insert((var, block), val);
    }

    /// Look up the current SSA value for `var` in `block`.
    ///
    /// If `var` isn't defined locally, the algorithm walks predecessors
    /// (inserting phis as necessary) per Braun et al. Algorithm 2.
    pub fn use_var(&mut self, func: &mut FuncBody<'_>, var: Variable, block: BlockId) -> Value {
        // Algorithm 1: Local Value Numbering.
        if let Some(&val) = self.current_def.get(&(var, block)) {
            return val;
        }
        // Algorithm 2: Global Value Numbering.
        self.use_var_recursive(func, var, block)
    }

    /// All predecessors of `block` are now known. Resolve any incomplete phis.
    pub fn seal_block(&mut self, func: &mut FuncBody<'_>, block: BlockId) {
        let incomplete = match std::mem::replace(
            &mut self.block_data.entry(block).or_default().sealed,
            Sealed::Yes,
        ) {
            Sealed::No(v) => v,
            Sealed::Yes => return, // already sealed
        };

        for (var, phi_val) in incomplete {
            self.add_phi_operands(func, var, phi_val, block);
        }
    }

    /// Seal all currently declared blocks at once.
    pub fn seal_all_blocks(&mut self, func: &mut FuncBody<'_>) {
        let blocks: Vec<BlockId> = self.block_data.keys().copied().collect();
        for b in blocks {
            self.seal_block(func, b);
        }
    }

    // -----------------------------------------------------------------------
    // Internal
    // -----------------------------------------------------------------------

    fn use_var_recursive(
        &mut self,
        func: &mut FuncBody<'_>,
        var: Variable,
        block: BlockId,
    ) -> Value {
        let data = self.block_data.entry(block).or_default();
        match &data.sealed {
            Sealed::No(_) => {
                // Block not yet sealed – add a placeholder phi (block param).
                let phi = func.add_block_param(block);
                // We need to re-borrow because add_block_param borrows func,
                // and we need to borrow block_data again.
                match &mut self.block_data.get_mut(&block).unwrap().sealed {
                    Sealed::No(incomplete) => incomplete.push((var, phi)),
                    Sealed::Yes => unreachable!(),
                }
                self.phi_blocks.insert(phi, block);
                self.current_def.insert((var, block), phi);
                phi
            }
            Sealed::Yes => {
                let preds: Vec<BlockId> = func.block(block).preds.clone();
                if preds.len() == 1 {
                    // Exactly one predecessor – no phi needed, just look there.
                    let val = self.use_var(func, var, preds[0]);
                    self.current_def.insert((var, block), val);
                    val
                } else {
                    // Multiple predecessors – insert a phi.
                    let phi = func.add_block_param(block);
                    self.phi_blocks.insert(phi, block);
                    // Define before recursing to break cycles.
                    self.current_def.insert((var, block), phi);
                    self.add_phi_operands(func, var, phi, block);
                    // After adding operands, try to simplify.
                    let result = self.try_remove_trivial_phi(func, phi, block);
                    self.current_def.insert((var, block), result);
                    result
                }
            }
        }
    }

    /// For each predecessor of `block`, look up `var` and pass it as a
    /// jump argument corresponding to `phi`'s parameter slot.
    fn add_phi_operands(
        &mut self,
        func: &mut FuncBody<'_>,
        var: Variable,
        _phi: Value,
        block: BlockId,
    ) {
        let preds: Vec<BlockId> = func.block(block).preds.clone();
        for pred in preds {
            let val = self.use_var(func, var, pred);
            func.append_pred_arg(pred, block, val);
        }
    }

    /// If all operands of the phi are the same value (ignoring self-references),
    /// remove the phi and return that value. Otherwise return the phi unchanged.
    fn try_remove_trivial_phi(
        &mut self,
        func: &mut FuncBody<'_>,
        phi: Value,
        block: BlockId,
    ) -> Value {
        let b = func.block(block);
        let param_idx = match b.params.iter().position(|&v| v == phi) {
            Some(i) => i,
            None => return phi, // phi was already removed
        };

        // Collect operands: for each predecessor, the argument at `param_idx`.
        let mut same: Option<Value> = None;
        for &pred in &b.preds.clone() {
            let arg = self.get_branch_arg(func, pred, block, param_idx);
            let Some(arg) = arg else { continue };
            if arg == phi {
                continue; // self-reference
            }
            match same {
                None => same = Some(arg),
                Some(s) if s == arg => {} // same as before
                Some(_) => return phi,    // non-trivial phi
            }
        }

        // Trivial: all operands are the same value (or only self-refs).
        let replacement = same.unwrap_or(phi);
        if replacement == phi {
            return phi;
        }

        // Remove the block parameter and adjust predecessor arguments.
        func.block_mut(block).params.retain(|&v| v != phi);
        // Remove the corresponding argument from each predecessor's terminator.
        for &pred in &func.block(block).preds.clone() {
            self.remove_branch_arg(func, pred, block, param_idx);
        }
        self.phi_blocks.remove(&phi);

        // Replace all uses: update current_def entries that point to phi.
        let keys: Vec<(Variable, BlockId)> =
            self.current_def.iter().filter(|&(_, &v)| v == phi).map(|(&k, _)| k).collect();
        for k in keys {
            self.current_def.insert(k, replacement);
        }

        replacement
    }

    /// Get the argument that `pred`'s terminator passes to `target` at position `idx`.
    fn get_branch_arg(
        &self,
        func: &FuncBody<'_>,
        pred: BlockId,
        target: BlockId,
        idx: usize,
    ) -> Option<Value> {
        use super::ir::Term;
        let term = func.block(pred).term.as_ref()?;
        match term {
            Term::Jump { target: t, args } if *t == target => args.get(idx).copied(),
            Term::CondBranch { then_block, then_args, else_block, else_args, .. } => {
                if *then_block == target {
                    then_args.get(idx).copied()
                } else if *else_block == target {
                    else_args.get(idx).copied()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Remove the argument at `idx` from `pred`'s terminator targeting `target`.
    fn remove_branch_arg(
        &self,
        func: &mut FuncBody<'_>,
        pred: BlockId,
        target: BlockId,
        idx: usize,
    ) {
        use super::ir::Term;
        let term = func.block_mut(pred).term.as_mut().unwrap();
        match term {
            Term::Jump { target: t, args } if *t == target && idx < args.len() => {
                args.remove(idx);
            }
            Term::CondBranch { then_block, then_args, else_block, else_args, .. } => {
                if *then_block == target && idx < then_args.len() {
                    then_args.remove(idx);
                }
                if *else_block == target && *then_block != *else_block && idx < else_args.len() {
                    else_args.remove(idx);
                }
            }
            _ => {}
        }
    }
}
