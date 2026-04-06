// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Basic-block IR with SSA values.
//!
//! Every instruction produces at most one [`Value`]. Control flow is expressed
//! via block terminators ([`Term`]). Phi functions are represented as block
//! parameters: a [`Term::Jump`] or [`Term::CondBranch`] passes values to the
//! target block's parameter list, and those values are available as [`Value`]s
//! inside the target block.

// Re-export so downstream users of `ir` can name the type.
pub use super::ComparisonOp;
use super::charset::Charset;

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

/// An SSA value – immutable once defined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub u32);

/// Index into `FuncBody::blocks`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

/// Source-level variable (the SSA builder maps these to `Value`s).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(pub u32);

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

/// A non-terminator instruction inside a basic block.
#[derive(Debug, Clone)]
pub struct Inst<'a> {
    pub dst: Value,
    pub kind: InstKind<'a>,
}

/// Instruction kinds. Every variant that carries values refers to them by
/// [`Value`] (SSA), never by physical register.
#[derive(Debug, Clone)]
pub enum InstKind<'a> {
    /// `dst = imm`
    Imm(u32),
    /// `dst = src`  (copy / move)
    Copy(Value),
    /// `dst = src + imm`
    AddImm(Value, u32),
    /// `dst = <highlight kind constant>`
    HlKind(u32),
    /// Emit a highlight span. `kind` is the highlight-kind value.
    Flush(Value),
    /// Call another LSH function by name.
    Call(&'a str),
    /// Yield to the runtime until more input is available.
    AwaitInput,
    /// Read the physical `off` register into an SSA value.
    /// Emitted after a regex NFA succeeds to capture the post-match offset,
    /// and inside capture groups to save start/end positions.
    ReadOff,
    /// Write an SSA value into the physical `off` register.
    /// Emitted before a regex NFA to sync the physical register,
    /// and in `yield $N as kind` to temporarily set highlight bounds.
    WriteOff(Value),
    /// Write an SSA value into the physical `highlight_start` register.
    WriteHs(Value),
    /// Advance physical offset by `n`. Used inside regex NFA for `.` (dot).
    AdvanceOff(u32),
    /// Set physical offset to immediate. Used inside regex NFA for `.*` (skip to end of line).
    SetOffImm(u32),
}

// ---------------------------------------------------------------------------
// Terminators
// ---------------------------------------------------------------------------

/// How a basic block ends.
#[derive(Debug, Clone)]
pub enum Term<'a> {
    /// Unconditional jump. `args` are passed to the target block's parameters.
    Jump { target: BlockId, args: Vec<Value> },
    /// Conditional branch.
    CondBranch {
        kind: CondKind<'a>,
        then_block: BlockId,
        then_args: Vec<Value>,
        else_block: BlockId,
        else_args: Vec<Value>,
    },
    /// Return from function.
    Return,
}

/// The condition tested by a [`Term::CondBranch`].
#[derive(Debug, Clone)]
pub enum CondKind<'a> {
    /// Compare two SSA values.
    Cmp { lhs: Value, rhs: Value, op: ComparisonOp },
    /// True if at end of line.
    EndOfLine,
    /// Match a character set (min..=max repetitions).
    Charset { cs: &'a Charset, min: u32, max: u32 },
    /// Match a literal prefix.
    Prefix(&'a str),
    /// Match a literal prefix (case-insensitive).
    PrefixInsensitive(&'a str),
}

// ---------------------------------------------------------------------------
// Basic blocks & functions
// ---------------------------------------------------------------------------

/// A basic block: a straight-line sequence of instructions ending in a
/// terminator.
#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    /// Block parameters (phi values). Predecessors pass arguments via their
    /// terminator's `args` lists, one per parameter, in order.
    pub params: Vec<Value>,
    /// Non-terminator instructions.
    pub insts: Vec<Inst<'a>>,
    /// Terminator (control-flow).
    pub term: Option<Term<'a>>,
    /// Predecessor block IDs (filled during construction).
    pub preds: Vec<BlockId>,
}

/// A function body consisting of basic blocks.
#[derive(Debug)]
pub struct FuncBody<'a> {
    pub blocks: Vec<BasicBlock<'a>>,
    next_value: u32,
}

impl<'a> Default for FuncBody<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> FuncBody<'a> {
    pub fn new() -> Self {
        Self { blocks: Vec::new(), next_value: 0 }
    }

    /// Allocate a fresh [`Value`].
    pub fn next_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
        v
    }

    /// Create an empty block and return its ID.
    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock {
            params: Vec::new(),
            insts: Vec::new(),
            term: None,
            preds: Vec::new(),
        });
        id
    }

    /// Add a block parameter (phi input) and return the corresponding value.
    pub fn add_block_param(&mut self, block: BlockId) -> Value {
        let val = self.next_value();
        self.blocks[block.0 as usize].params.push(val);
        val
    }

    /// Append a non-terminator instruction to `block`. Returns the produced value.
    pub fn push_inst(&mut self, block: BlockId, kind: InstKind<'a>) -> Value {
        let dst = self.next_value();
        self.blocks[block.0 as usize].insts.push(Inst { dst, kind });
        dst
    }

    /// Set the terminator for `block` and register predecessors on target blocks.
    pub fn set_term(&mut self, block: BlockId, term: Term<'a>) {
        // Register predecessors.
        match &term {
            Term::Jump { target, .. } => {
                self.blocks[target.0 as usize].preds.push(block);
            }
            Term::CondBranch { then_block, else_block, .. } => {
                self.blocks[then_block.0 as usize].preds.push(block);
                if *then_block != *else_block {
                    self.blocks[else_block.0 as usize].preds.push(block);
                }
            }
            Term::Return => {}
        }
        self.blocks[block.0 as usize].term = Some(term);
    }

    /// Access a block.
    pub fn block(&self, id: BlockId) -> &BasicBlock<'a> {
        &self.blocks[id.0 as usize]
    }

    /// Access a block mutably.
    pub fn block_mut(&mut self, id: BlockId) -> &mut BasicBlock<'a> {
        &mut self.blocks[id.0 as usize]
    }

    /// Find the block-param index for a given value, if it is a param of `block`.
    pub fn block_param_index(&self, block: BlockId, val: Value) -> Option<usize> {
        self.blocks[block.0 as usize].params.iter().position(|&v| v == val)
    }

    /// Append an argument to every predecessor's terminator that targets `block`.
    /// Used by the SSA builder when resolving phis.
    pub fn append_pred_arg(&mut self, pred: BlockId, target: BlockId, val: Value) {
        let term =
            self.blocks[pred.0 as usize].term.as_mut().expect("predecessor has no terminator");
        match term {
            Term::Jump { target: t, args } if *t == target => {
                args.push(val);
            }
            Term::CondBranch { then_block, then_args, else_block, else_args, .. } => {
                if *then_block == target {
                    then_args.push(val);
                }
                if *else_block == target && *then_block != *else_block {
                    else_args.push(val);
                }
            }
            _ => {}
        }
    }
}
