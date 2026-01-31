// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! The LSH compiler
//!
//! See crate documentation.

mod backend;
mod charset;
mod frontend;
mod generator;
mod optimizer;
mod regex;

use std::cell::{Cell, RefCell};
use std::collections::{HashSet, VecDeque};
use std::fmt;
use std::fmt::Write as _;
use std::mem::zeroed;
use std::path::Path;

use stdext::arena::{Arena, ArenaString};

pub use self::charset::{Charset, SerializedCharset};
use self::frontend::*;
pub use self::generator::Generator;
use crate::runtime::Register;

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

#[derive(Debug)]
pub struct CharsetHandle<'a> {
    pub id: u32,
    pub value: &'a Charset,
    pub usage_count: u32,
}

#[derive(Debug)]
pub struct StringHandle<'a> {
    pub id: u32,
    pub value: &'a str,
    pub usage_count: u32,
}

#[derive(Debug)]
pub struct HighlightKindHandle<'a> {
    pub id: u32,
    pub value: &'a str,
    pub usage_count: u32,
}

pub struct Compiler<'a> {
    arena: &'a Arena,
    physical_registers: [IRRegCell<'a>; Register::COUNT],
    functions: Vec<Function<'a>>,
    charsets: Vec<&'a CharsetHandle<'a>>,
    strings: Vec<&'a StringHandle<'a>>,
    highlight_kinds: Vec<&'a HighlightKindHandle<'a>>,
    next_vreg_id: Cell<u32>,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        #[allow(invalid_value)]
        let mut s = Self {
            arena,
            physical_registers: unsafe { zeroed() },
            functions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            highlight_kinds: Default::default(),
            next_vreg_id: Cell::new(0),
        };

        // The `other` highlight kind is always present.
        s.intern_string("other");

        for i in 0..Register::COUNT {
            let reg = s.alloc_vreg();
            reg.borrow_mut().physical = Some(Register::from_usize(i));
            s.physical_registers[i] = reg;
        }

        s
    }

    pub fn parse<'src>(&mut self, path: &'src str, src: &'src str) -> CompileResult<()> {
        let mut parser = Parser::new(self, path, src);
        parser.run()?;
        Ok(())
    }

    pub fn assemble(&mut self) -> CompileResult<Assembly<'a>> {
        optimizer::optimize(self);
        backend::Backend::new().compile(self)
    }

    fn alloc_ir(&self, ir: IR<'a>) -> IRCell<'a> {
        self.arena.alloc_uninit().write(RefCell::new(ir))
    }

    fn alloc_iri(&self, instr: IRI<'a>) -> IRCell<'a> {
        self.arena.alloc_uninit().write(RefCell::new(IR { next: None, instr, offset: usize::MAX }))
    }

    fn alloc_noop(&self) -> IRCell<'a> {
        self.alloc_iri(IRI::Noop)
    }

    fn build_chain<'s>(&'s self) -> IRChainBuilder<'a, 's> {
        IRChainBuilder { compiler: self, span: None }
    }

    fn get_reg(&self, reg: Register) -> IRRegCell<'a> {
        self.physical_registers[reg as usize]
    }

    fn alloc_vreg(&self) -> IRRegCell<'a> {
        let id = self.next_vreg_id.get();
        self.next_vreg_id.set(id + 1);
        self.arena.alloc_uninit().write(RefCell::new(IRReg::new(id)))
    }

    fn intern_charset(&mut self, charset: &Charset) -> &'a CharsetHandle<'a> {
        if let Some(&h) = self.charsets.iter().find(|&&v| v.value == charset) {
            h
        } else {
            let id = self.charsets.len() as u32;
            let value = self.arena.alloc_uninit().write(charset.clone());
            let h = self.arena.alloc_uninit().write(CharsetHandle { id, value, usage_count: 0 });
            self.charsets.push(h);
            h
        }
    }

    fn intern_string(&mut self, s: &str) -> &'a StringHandle<'a> {
        if let Some(&h) = self.strings.iter().find(|&&v| v.value == s) {
            h
        } else {
            let id = self.strings.len() as u32;
            let value = arena_clone_str(self.arena, s);
            let h = self.arena.alloc_uninit().write(StringHandle { id, value, usage_count: 0 });
            self.strings.push(h);
            h
        }
    }

    fn intern_highlight_kind(&mut self, s: &str) -> &'a HighlightKindHandle<'a> {
        if let Some(&h) = self.highlight_kinds.iter().find(|&&v| v.value == s) {
            h
        } else {
            let id = self.highlight_kinds.len() as u32;
            let value = arena_clone_str(self.arena, s);
            let h =
                self.arena.alloc_uninit().write(HighlightKindHandle { id, value, usage_count: 0 });
            self.highlight_kinds.push(h);
            h
        }
    }

    fn visit_nodes_from(&self, root: IRCell<'a>) -> TreeVisitor<'a> {
        let mut stack = VecDeque::new();
        stack.push_back(root);
        TreeVisitor { current: None, stack, visited: Default::default() }
    }

    /// Collect all "interesting" characters from conditions in a loop body.
    /// Returns a charset where true = interesting character that should be checked.
    fn collect_interesting_charset(&self, loop_body: IRCell<'a>) -> Charset {
        let mut iter = self.visit_nodes_from(loop_body);
        let mut charset = Charset::no();

        #[allow(clippy::while_let_loop)]
        loop {
            // Can't use `while let`, because that borrows `iter`
            // and that prevents us from calling `skip_node()`.
            let Some(node) = iter.next() else {
                break;
            };

            if let IRI::If { condition, then } = node.borrow().instr {
                // For the purpose of computing fast-skips the contents of if conditions are irrelevant,
                // so skip the subtree. This is actually quite important. This this as an example:
                //   loop {
                //     if /a/ {
                //       loop {
                //         if /b/ {
                //         }
                //       }
                //     }
                //   }
                // The inverted charset of the inner /b/ includes "a". If we merge that into the outer
                // loop's charset we get one that covers all characters, making fast-skips impossible.
                iter.skip_node(then);

                match condition {
                    Condition::Cmp { .. } => {}
                    Condition::EndOfLine => {}
                    Condition::Charset { cs, .. } => {
                        // Merge this charset
                        charset.merge(&cs.value);
                    }
                    Condition::Prefix(s) | Condition::PrefixInsensitive(s) => {
                        // First character of the prefix is interesting
                        if let Some(&b) = s.value.as_bytes().first() {
                            charset.set(b, true);
                            if matches!(condition, Condition::PrefixInsensitive(_)) {
                                charset.set(b.to_ascii_uppercase(), true);
                                charset.set(b.to_ascii_lowercase(), true);
                            }
                        }
                    }
                }
            }
        }

        charset
    }

    pub fn as_mermaid(&self) -> String {
        let mut output = String::new();

        // ---
        // config:
        // layout: elk
        // elk:
        //   considerModelOrder: NONE
        // ---
        output.push_str("flowchart TB\n");

        for func in &self.functions {
            _ = writeln!(output, "  subgraph {}", func.name);
            _ = writeln!(output, "    direction TB");
            _ = writeln!(
                output,
                "    {}_start@{{shape: start}} --> {}",
                func.name,
                func.body.borrow()
            );

            let mut visited = HashSet::new();
            let mut to_visit = vec![func.body];

            while let Some(node_cell) = to_visit.pop() {
                if !visited.insert(node_cell.as_ptr()) {
                    continue;
                }

                let node = node_cell.borrow();
                let offset = node.offset;
                _ = write!(output, "    {}", node);

                match node.instr {
                    IRI::Noop => {
                        _ = write!(output, "[{offset}: noop]");
                    }
                    IRI::Mov { dst, src } => {
                        let src = src.borrow();
                        let dst = dst.borrow();
                        _ = write!(output, "[\"{offset}: {dst:?} = {src:?}\"]");
                    }
                    IRI::MovImm { dst, imm } => {
                        let dst = dst.borrow();
                        _ = write!(output, "[\"{offset}: {dst:?} = {imm}\"]");
                    }
                    IRI::MovKind { dst, kind } => {
                        let dst = dst.borrow();
                        _ = write!(output, "[\"{offset}: {dst:?} = `{}`\"]", kind.value);
                    }
                    IRI::AddImm { dst, imm } => {
                        let dst = dst.borrow();
                        _ = write!(output, "[\"{offset}: {dst:?} += {imm}\"]");
                    }
                    IRI::If { condition, then } => {
                        _ = write!(output, "{{\"{offset}: ");
                        _ = match condition {
                            Condition::Cmp { lhs, rhs, op } => {
                                let lhs = lhs.borrow();
                                let rhs = rhs.borrow();
                                let op_str = match op {
                                    ComparisonOp::Eq => "==",
                                    ComparisonOp::Ne => "!=",
                                    ComparisonOp::Lt => "<",
                                    ComparisonOp::Gt => ">",
                                    ComparisonOp::Le => "<=",
                                    ComparisonOp::Ge => ">=",
                                };
                                write!(output, "{lhs:?} {op_str} {rhs:?}")
                            }
                            Condition::EndOfLine => write!(output, "eol"),
                            Condition::Charset { cs, min, max } => match (min, max) {
                                (0, 1) => write!(output, "charset: {cs:?}?"),
                                (0, u32::MAX) => write!(output, "charset: {cs:?}*"),
                                (1, u32::MAX) => write!(output, "charset: {cs:?}+"),
                                _ => write!(output, "charset: {cs:?}{{{min},{max}}}"),
                            },
                            Condition::Prefix(s) => write!(output, "match: {}", s.value),
                            Condition::PrefixInsensitive(s) => {
                                write!(output, "imatch: {}", s.value)
                            }
                        };
                        _ = writeln!(output, "\"}}");
                        _ = writeln!(output, "    {} -->|yes| {}", node, then.borrow());
                        to_visit.push(then);
                    }
                    IRI::Call { name } => {
                        _ = write!(output, "[\"{offset}: call {name}\"]");
                    }
                    IRI::Return => {
                        _ = write!(output, "[{offset}: return]");
                    }
                    IRI::Flush { kind } => {
                        let kind = kind.borrow();
                        _ = write!(output, "[{offset}: flush {kind:?}]");
                    }
                    IRI::AwaitInput => {
                        _ = write!(output, "[{offset}: await input]");
                    }
                }

                match node.instr {
                    IRI::If { .. } => {
                        if let Some(next) = node.next {
                            _ = writeln!(output, "    {} -->|no| {}", node, next.borrow());
                        }
                    }
                    _ => {
                        if let Some(next) = node.next {
                            _ = writeln!(output, " --> {}", next.borrow());
                        } else {
                            _ = writeln!(output);
                        }
                    }
                }

                if let Some(next) = node.next {
                    to_visit.push(next);
                }
            }

            _ = writeln!(output, "  end");
        }

        output
    }
}

struct TreeVisitor<'a> {
    current: Option<IRCell<'a>>,
    stack: VecDeque<IRCell<'a>>,
    visited: HashSet<*const RefCell<IR<'a>>>,
}

impl<'a> TreeVisitor<'a> {
    fn skip_node(&mut self, node: IRCell<'a>) {
        self.visited.insert(node as *const _);
    }
}

impl<'a> Iterator for TreeVisitor<'a> {
    type Item = IRCell<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cell) = self.current.take() {
            {
                let ir = cell.borrow();
                if let IRI::If { then, .. } = ir.instr {
                    self.stack.push_back(then);
                }
                if let Some(next) = ir.next {
                    self.stack.push_back(next);
                }
            }
        }

        while let Some(cell) = self.stack.pop_front() {
            if self.visited.insert(cell) {
                self.current = Some(cell);
                return self.current;
            }
        }

        None
    }
}

pub struct Assembly<'a> {
    pub instructions: Vec<u8>,
    pub entrypoints: Vec<Entrypoint>,
    pub charsets: Vec<&'a Charset>,
    pub strings: Vec<&'a str>,
    pub highlight_kinds: Vec<&'a str>,
}

pub struct Entrypoint {
    pub name: String,
    pub display_name: String,
    pub paths: Vec<String>,
    pub address: usize,
}

pub struct HighlightKindCamelcaseFormatter<'a>(&'a str);

impl<'a> fmt::Display for HighlightKindCamelcaseFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut capitalize_next = true;
        for c in self.0.chars() {
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
struct FunctionAttributes<'a> {
    display_name: Option<&'a str>,
    paths: Vec<&'a str>,
}

#[derive(Debug, Clone)]
struct Function<'a> {
    name: &'a str,
    attributes: FunctionAttributes<'a>,
    body: IRCell<'a>,
    public: bool,
}

// To be honest, I don't think this qualifies as an "intermediate representation",
// if we compare this to popular compilers. But whatever. It's still intermediate to us.
#[derive(Debug)]
struct IR<'a> {
    next: Option<IRCell<'a>>,
    instr: IRI<'a>,
    offset: usize,
}

impl<'a> fmt::Display for IR<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "i{self:p}")
    }
}

type IRCell<'a> = &'a RefCell<IR<'a>>;

// IRI = Immediate Representation Instruction
#[derive(Debug, Clone, Copy)]
enum IRI<'a> {
    Noop,
    Mov { dst: IRRegCell<'a>, src: IRRegCell<'a> },
    MovImm { dst: IRRegCell<'a>, imm: u32 },
    MovKind { dst: IRRegCell<'a>, kind: &'a HighlightKindHandle<'a> },
    AddImm { dst: IRRegCell<'a>, imm: u32 },
    If { condition: Condition<'a>, then: IRCell<'a> },
    Call { name: &'a str },
    Return,
    Flush { kind: IRRegCell<'a> },
    AwaitInput,
}

#[derive(Default)]
struct IRReg {
    id: u32,
    physical: Option<Register>,
}

type IRRegCell<'a> = &'a RefCell<IRReg>;

impl IRReg {
    fn new(id: u32) -> Self {
        IRReg { id, physical: None }
    }
}

impl fmt::Debug for IRReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(p) = self.physical
            && self.id < Register::COUNT as u32
        {
            write!(f, "{}", p.mnemonic())
        } else {
            write!(f, "v{}", self.id)
        }
    }
}

#[derive(Clone, Copy)]
struct IRSpan<'a> {
    first: IRCell<'a>,
    last: IRCell<'a>,
}

impl<'a> IRSpan<'a> {
    fn single(node: IRCell<'a>) -> Self {
        Self { first: node, last: node }
    }
}

struct IRChainBuilder<'a, 's> {
    compiler: &'s Compiler<'a>,
    span: Option<IRSpan<'a>>,
}

impl<'a, 's> IRChainBuilder<'a, 's> {
    fn append(&mut self, instr: IRI<'a>) -> &mut Self {
        let node = self.compiler.alloc_iri(instr);
        if let Some(span) = &mut self.span {
            span.last.borrow_mut().set_next(node);
            span.last = node;
        } else {
            self.span = Some(IRSpan::single(node));
        }
        self
    }

    fn build(&self) -> IRSpan<'a> {
        self.span.unwrap_or_else(|| IRSpan::single(self.compiler.alloc_noop()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComparisonOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy)]
enum Condition<'a> {
    Cmp { lhs: IRRegCell<'a>, rhs: IRRegCell<'a>, op: ComparisonOp },
    EndOfLine,
    Charset { cs: &'a CharsetHandle<'a>, min: u32, max: u32 },
    Prefix(&'a StringHandle<'a>),
    PrefixInsensitive(&'a StringHandle<'a>),
}

impl<'a> IR<'a> {
    fn wants_next(&self) -> bool {
        if self.next.is_some() {
            return false;
        }

        match self.instr {
            IRI::Mov { dst, .. } | IRI::MovImm { dst, .. }
                if dst.borrow().id == Register::ProgramCounter as u32 =>
            {
                false
            }
            IRI::Return => false,
            _ => true,
        }
    }

    fn set_next(&mut self, n: IRCell<'a>) {
        debug_assert!(self.wants_next());
        self.next = Some(n);
    }
}

fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    ArenaString::from_str(arena, s).leak()
}
