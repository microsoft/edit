// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Welcome to Leonard's Syntax Highlighter,
//! otherwise known as Leonard's Shitty Highlighter.
//!
//! This module provides the compiler that produces the bytecode for edit.

#![feature(allocator_api)]
#![allow(irrefutable_let_patterns, unused, clippy::upper_case_acronyms)]

pub mod compiler;
pub mod engine;
pub mod glob;

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
