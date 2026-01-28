// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Welcome to Leonard's Syntax Highlighter (LSH), otherwise known as
//! Leonard's Shitty Highlighter, which is really what it is.
//!
//! ## Architecture
//!
//!   DSL source
//! → `frontend` (parse) → IR
//! → `optimizer` → IR
//! → `backend` (regalloc + codegen) → bytecode
//! → `runtime` (execute)
//!
//! The IR is a graph of `RefCell<IR>` nodes. Each node has a `.next` pointer and `If` nodes
//! in particular have a `.then` pointer. This makes CFG manipulation trivial but means you can't
//! iterate in program order without linearization (see `backend::LivenessAnalysis`).
//!
//! ## Register semantics
//!
//! - `off` advances only on successful regex matches. Failed matches leave it alone.
//!   This is why the frontend emits backup/restore pairs around regex chains.
//! - `hs` (highlight start) is used to track the input offset of the last yield statement,
//!   which permits the runtime to highlight everything inbetween with the next yield's highlight kind.
//!
//! ## Charset encoding
//!
//! Charsets are 256-bit bitmaps stored as `[u16; 16]`. The encoding is transposed for SIMD:
//! `bitmap[lo_nibble] & (1 << hi_nibble)` tests if byte `(hi_nibble << 4) | lo_nibble` is set.
//! See `in_set()`. This layout allows parallel lookup of multiple bytes using pshufb.
//!
//! ## Gotchas
//!
//! - Physical VS virtual registers:
//!   `IRReg.physical` being `Some` means it's pre-colored (e.g., `off` register).
//!   The backend must preserve these assignments.
//! - Semi-SSA:
//!   The frontend emits IR where each vreg is written once,
//!   but physical registers like `off` are mutated repeatedly.
//!   The optimizer relies on this.
//!
//! ## TODO
//!
//! - Most importantly, the architecture is A GODDAMN MESS.
//! - The regex compiler (`regex.rs`) panics on unsupported patterns instead of returning errors.
//!   Should bubble up `CompileError` instead.
//! - No support for spill code generation. If you run out of user registers, you're dead.
//!   The current definition files don't hit this, but complex patterns could.
//! - No include statements, and all functions across all files share a single namespace.

#![feature(allocator_api)]
#![allow(irrefutable_let_patterns, clippy::upper_case_acronyms)]

pub mod compiler;
pub mod runtime;
