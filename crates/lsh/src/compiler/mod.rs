// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! The LSH compiler
//!
//! See crate documentation.

mod backend;
mod charset;
mod frontend;
mod generator;
mod index_vec;
mod ir;
mod optimizer;
mod regex;

use std::fmt;
use std::path::Path;

use self::backend::Assembly;
pub use self::charset::{Charset, SerializedCharset};
pub use self::generator::Generator;

pub fn builtin_definitions_path() -> &'static Path {
    #[cfg(windows)]
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "\\definitions");
    #[cfg(not(windows))]
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/definitions");
    Path::new(path)
}

#[derive(Debug)]
pub struct CompileError {
    pub path: String,
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl std::error::Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}:{}: {}", self.path, self.line, self.column, self.message)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Default)]
pub struct Compiler {
    program: ir::Program,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse<'src>(&mut self, path: &'src str, src: &'src str) -> CompileResult<()> {
        frontend::Parser::new(&mut self.program, path, src).run()
    }

    pub fn assemble(&mut self) -> CompileResult<Assembly> {
        optimizer::optimize(&mut self.program);
        backend::Backend::new(&self.program).compile()
    }
}
