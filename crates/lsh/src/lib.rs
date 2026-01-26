// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Leonard's Syntax Highlighter (LSH) - a DSL compiler for syntax highlighting.
//!
//! ## Architecture
//!
//! DSL source → `frontend` (parse) → IR → `optimizer` → IR → `backend` (regalloc + codegen) → bytecode → `engine` (execute)
//!
//! The IR is a graph of `RefCell<IR>` nodes, not a vector. Each node has a `.next` pointer and
//! `If` nodes have a `.then` pointer. This makes CFG manipulation trivial but means you can't
//! iterate in program order without linearization (see `backend::LivenessAnalysis`).
//!
//! ## Gotchas
//!
//! - **Arena lifetime**: All IR nodes and interned strings live in a single `Arena`. The compiler
//!   holds `&'a Arena` and everything derives lifetime `'a` from it. Don't try to outlive it.
//!
//! - **Physical vs virtual registers**: `IRReg.physical` being `Some` means it's pre-colored
//!   (e.g., `off`, `hs`, `hk`). The backend must preserve these assignments.
//!
//! - **Single assignment form (sort of)**: The frontend emits IR where each vreg is written once,
//!   but physical registers like `off` are mutated repeatedly. The optimizer relies on this.
//!
//! ## TODO
//!
//! - The regex compiler (`regex.rs`) panics on unsupported patterns instead of returning errors.
//!   Should bubble up `CompileError` instead.
//!
//! - No support for spill code generation. If you run out of 11 user registers, you're dead.
//!   The current definition files don't hit this, but complex patterns could.
//!
//! - Incremental compilation: currently recompiles everything. Would need dependency tracking.

#![feature(allocator_api)]
#![allow(irrefutable_let_patterns, unused, clippy::upper_case_acronyms)]

pub mod compiler;
pub mod engine;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use stdext::arena::*;

pub use crate::compiler::CompileResult;

fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    ArenaString::from_str(arena, s).leak()
}

trait Intern<'a, T: ?Sized> {
    fn intern(&mut self, arena: &'a Arena, item: &T) -> &'a T;
}

impl<'a> Intern<'a, str> for Vec<&'a str> {
    fn intern(&mut self, arena: &'a Arena, value: &str) -> &'a str {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena_clone_str(arena, value);
            self.push(s);
            s
        }
    }
}
