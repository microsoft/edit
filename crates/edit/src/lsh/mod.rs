// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Welcome to Leonard's Syntax Highlighter,
//! otherwise known as Leonard's Shitty Highlighter.
//!
//! This module provides the VM that runs the bytecode produced by the lsh crate.

pub mod cache;
mod definitions;
mod highlighter;

pub use definitions::*;
pub use highlighter::*;
