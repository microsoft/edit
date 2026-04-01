// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! The LSH compiler
//!
//! See crate documentation.

mod backend;
mod charset;
mod frontend;
mod generator;
pub mod ir;
mod regex;
pub mod ssa;

use std::fmt;
use std::fmt::Write as _;
use std::path::Path;

use stdext::arena::Arena;
use stdext::collections::BString;

pub use self::charset::{Charset, SerializedCharset};
pub use self::generator::Generator;

pub fn builtin_definitions_path() -> &'static Path {
    #[cfg(windows)]
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "\\definitions");
    #[cfg(not(windows))]
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/definitions");
    Path::new(path)
}

pub type CompileResult<T> = Result<T, CompileError>;

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

pub struct Compiler<'a> {
    arena: &'a Arena,
    parsed_functions: Vec<(&'a str, ir::FuncBody<'a>, FunctionAttributes<'a>, bool)>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
    highlight_kinds: Vec<HighlightKind<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            arena,
            parsed_functions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            highlight_kinds: vec![HighlightKind { identifier: "other", value: 0 }],
        }
    }

    pub fn parse<'src>(&mut self, path: &'src str, src: &'src str) -> CompileResult<()> {
        let mut parser = frontend::Parser2::new(self, path, src);
        parser.run()?;
        let functions = std::mem::take(&mut parser.functions);
        drop(parser);
        self.parsed_functions.extend(functions);
        Ok(())
    }

    pub fn assemble(&mut self) -> CompileResult<Assembly<'a>> {
        let functions = std::mem::take(&mut self.parsed_functions);
        backend::Backend2::new().compile(self, functions)
    }

    fn intern_charset(&mut self, charset: &Charset) -> &'a Charset {
        self.charsets.intern(self.arena, charset)
    }

    fn intern_string(&mut self, s: &str) -> &'a str {
        self.strings.intern(self.arena, s)
    }

    fn intern_highlight_kind(&mut self, identifier: &str) -> &HighlightKind<'a> {
        let idx = match self.highlight_kinds.binary_search_by(|hk| hk.identifier.cmp(identifier)) {
            Ok(idx) => idx,
            Err(idx) => {
                let identifier = arena_clone_str(self.arena, identifier);
                let value = self.highlight_kinds.len() as u32;
                self.highlight_kinds.insert(idx, HighlightKind { identifier, value });
                idx
            }
        };
        &self.highlight_kinds[idx]
    }

    pub fn as_mermaid(&self) -> String {
        use ir::*;

        let mut output = String::new();
        output.push_str("flowchart TB\n");

        for (name, func, _, _) in &self.parsed_functions {
            _ = writeln!(output, "  subgraph {name}");
            _ = writeln!(output, "    direction TB");
            if !func.blocks.is_empty() {
                _ = writeln!(output, "    {name}_start@{{shape: start}} --> B0");
            }

            for (i, block) in func.blocks.iter().enumerate() {
                let bid = format!("B{i}");

                // Block header with params.
                if block.params.is_empty() {
                    _ = write!(output, "    {bid}[\"B{i}");
                } else {
                    let params: Vec<String> =
                        block.params.iter().map(|v| format!("v{}", v.0)).collect();
                    _ = write!(output, "    {bid}[\"B{i}({})", params.join(", "));
                }

                // Instructions.
                for inst in &block.insts {
                    _ = write!(output, "<br/>v{} = ", inst.dst.0);
                    match &inst.kind {
                        InstKind::Imm(v) => _ = write!(output, "imm {v}"),
                        InstKind::Copy(src) => _ = write!(output, "copy v{}", src.0),
                        InstKind::AddImm(src, imm) => _ = write!(output, "v{} + {imm}", src.0),
                        InstKind::HlKind(k) => _ = write!(output, "hlkind {k}"),
                        InstKind::Flush(v) => _ = write!(output, "flush v{}", v.0),
                        InstKind::Call(name) => _ = write!(output, "call {name}"),
                        InstKind::AwaitInput => _ = write!(output, "await_input"),
                        InstKind::ReadOff => _ = write!(output, "read_off"),
                        InstKind::WriteOff(v) => _ = write!(output, "write_off v{}", v.0),
                        InstKind::WriteHs(v) => _ = write!(output, "write_hs v{}", v.0),
                        InstKind::AdvanceOff(n) => _ = write!(output, "advance_off {n}"),
                        InstKind::SetOffImm(n) => _ = write!(output, "set_off_imm {n}"),
                    }
                }
                _ = writeln!(output, "\"]");

                // Edges from terminator.
                match &block.term {
                    Some(Term::Jump { target, args }) => {
                        let args_str = if args.is_empty() {
                            String::new()
                        } else {
                            let a: Vec<String> = args.iter().map(|v| format!("v{}", v.0)).collect();
                            format!(" [{}]", a.join(", "))
                        };
                        _ = writeln!(output, "    {bid} --> B{}{args_str}", target.0);
                    }
                    Some(Term::CondBranch {
                        kind,
                        then_block,
                        then_args,
                        else_block,
                        else_args,
                    }) => {
                        let cond_str = match kind {
                            CondKind::Cmp { lhs, rhs, op } => {
                                let op_str = match op {
                                    ComparisonOp::Eq => "==",
                                    ComparisonOp::Ne => "!=",
                                    ComparisonOp::Lt => "<",
                                    ComparisonOp::Gt => ">",
                                    ComparisonOp::Le => "<=",
                                    ComparisonOp::Ge => ">=",
                                };
                                format!("v{} {op_str} v{}", lhs.0, rhs.0)
                            }
                            CondKind::EndOfLine => "eol".to_string(),
                            CondKind::Charset { cs, min, max } => {
                                format!("charset {cs:?} {{{min},{max}}}")
                            }
                            CondKind::Prefix(s) => format!("prefix {s:?}"),
                            CondKind::PrefixInsensitive(s) => format!("iprefix {s:?}"),
                        };
                        _ = writeln!(
                            output,
                            "    {bid} -->|\"yes: {cond_str}\"| B{}",
                            then_block.0
                        );
                        _ = writeln!(output, "    {bid} -->|no| B{}", else_block.0);
                        let _ = (then_args, else_args); // TODO: render args if needed
                    }
                    Some(Term::Return) => {
                        _ = writeln!(output, "    {bid} --> {name}_end@{{shape: stop}}");
                    }
                    None => {
                        _ = writeln!(output, "    {bid} --> {name}_end@{{shape: stop}}");
                    }
                }
            }

            _ = writeln!(output, "  end");
        }

        output
    }
}

pub struct Assembly<'a> {
    pub instructions: Vec<u8>,
    pub entrypoints: Vec<Entrypoint>,
    pub charsets: Vec<&'a Charset>,
    pub strings: Vec<&'a str>,
    pub highlight_kinds: Vec<HighlightKind<'a>>,
}

pub struct Entrypoint {
    pub name: String,
    pub display_name: String,
    pub paths: Vec<String>,
    pub address: usize,
}

#[derive(Clone)]
pub struct HighlightKind<'a> {
    pub identifier: &'a str,
    pub value: u32,
}

impl<'a> HighlightKind<'a> {
    pub fn fmt_camelcase(&self) -> HighlightKindCamelcaseFormatter<'a> {
        HighlightKindCamelcaseFormatter { identifier: self.identifier }
    }
}

pub struct HighlightKindCamelcaseFormatter<'a> {
    identifier: &'a str,
}

impl<'a> fmt::Display for HighlightKindCamelcaseFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut capitalize_next = true;
        for c in self.identifier.chars() {
            if c == '.' {
                capitalize_next = true;
            } else if capitalize_next {
                capitalize_next = false;
                f.write_char(c.to_ascii_uppercase())?;
            } else {
                f.write_char(c)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct FunctionAttributes<'a> {
    pub(crate) display_name: Option<&'a str>,
    pub(crate) paths: Vec<&'a str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    BString::from_str(arena, s).leak()
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
