// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Compiler core: IR definitions, instruction encoding, and shared infrastructure.
//!
//! ## IR representation
//!
//! The IR is a graph of `RefCell<IR>` nodes linked via `next` and `then` pointers.
//! This isn't a traditional SSA or basic block representation - it's more like a
//! flowchart where each node is one instruction. The `offset` field is set during
//! codegen to the final bytecode address.
//!
//! ## Register architecture
//!
//! The `IRReg.physical` field starts as `None` for vregs and gets filled in by regalloc.
//! For physical registers, it's set during `Compiler::new()`.
//!
//! ## Charset representation
//!
//! `Charset` is a 256-bit bitmap (`[bool; 256]`). The engine uses a transposed `[u16; 16]`
//! layout for SIMD reasons - see `generator.rs` where the conversion happens.
//!
//! ## Instruction encoding
//!
//! Variable-length encoding, 1-9 bytes per instruction. See `Instruction::encode/decode`.
//! The `address_offset()` method returns where within an instruction the jump target lives,
//! used by the backend's relocation system.
//!
//! ## Quirks
//!
//! - `TreeVisitor` does BFS, which is fine for iteration but *not* for linearization.
//!   The backend has its own DFS traversal for liveness analysis.

mod backend;
mod frontend;
mod generator;
mod helpers;
mod optimizer;
mod regex;
mod tokenizer;

use std::cell::{Cell, RefCell};
use std::collections::{HashSet, VecDeque};
use std::fmt::Write as _;
use std::marker::PhantomData;
use std::mem::{MaybeUninit, transmute, zeroed};
use std::ops::{Index, IndexMut};
use std::path::Path;
use std::{fmt, ptr};

use stdext::arena::{Arena, ArenaString};

use self::frontend::*;
pub use self::generator::Generator;
use crate::compiler::helpers::{Intern, arena_clone_str};

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
    physical_registers: [IRRegCell<'a>; Register::COUNT],
    functions: Vec<Function<'a>>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
    highlight_kinds: Vec<HighlightKind<'a>>,
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
            highlight_kinds: vec![HighlightKind { identifier: "other", value: 0 }],
            next_vreg_id: Cell::new(0),
        };

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

    fn visit_nodes_from(&self, root: IRCell<'a>) -> TreeVisitor<'a> {
        let mut stack = VecDeque::new();
        stack.push_back(root);
        TreeVisitor { current: None, stack, visited: Default::default() }
    }

    /// Collect all "interesting" characters from conditions in a loop body.
    /// Returns a charset where true = interesting character that should be checked.
    fn collect_interesting_charset(&self, loop_body: IRCell<'a>) -> Charset {
        let mut charset = Charset::no();
        let mut visited = HashSet::new();

        for node in self.visit_nodes_from(loop_body) {
            let node_ptr = node as *const _;
            if !visited.insert(node_ptr) {
                continue;
            }

            if let IRI::If { condition, .. } = node.borrow().instr {
                match condition {
                    Condition::Cmp { .. } => {}
                    Condition::EndOfLine => {}
                    Condition::Charset(cs) => {
                        // Merge this charset
                        charset.merge(cs);
                    }
                    Condition::Prefix(s) | Condition::PrefixInsensitive(s) => {
                        // First character of the prefix is interesting
                        if let Some(&b) = s.as_bytes().first() {
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
                        let kind = self
                            .highlight_kinds
                            .iter()
                            .find(|hk| hk.value == kind)
                            .map_or("???", |hk| hk.identifier);
                        _ = write!(output, "[\"{offset}: {dst:?} = {kind}\"]");
                    }
                    IRI::AddImm { dst, imm } => {
                        let dst = dst.borrow();
                        _ = write!(output, "[\"{offset}: {dst:?} += {imm}\"]");
                    }
                    IRI::If { condition, then } => {
                        _ = write!(output, "{{\"{offset}: ");
                        _ = match condition {
                            Condition::Cmp { lhs, rhs, op } => {
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
                            Condition::Charset(cs) => write!(output, "charset: {cs:?}"),
                            Condition::Prefix(s) => write!(output, "match: {s}"),
                            Condition::PrefixInsensitive(s) => write!(output, "imatch: {s}"),
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
                        _ = write!(output, "[{offset}: flush]");
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
    MovKind { dst: IRRegCell<'a>, kind: u32 },
    AddImm { dst: IRRegCell<'a>, imm: u32 },
    If { condition: Condition<'a>, then: IRCell<'a> },
    Call { name: &'a str },
    Return,
    Flush { kind: IRRegCell<'a> },
    AwaitInput,
}

#[derive(Default)]
pub struct IRReg {
    id: u32,
    physical: Option<Register>,
}

pub type IRRegCell<'a> = &'a RefCell<IRReg>;

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
    pub first: IRCell<'a>,
    pub last: IRCell<'a>,
}

impl<'a> IRSpan<'a> {
    pub fn single(node: IRCell<'a>) -> Self {
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
pub enum ComparisonOp {
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
    Charset(&'a Charset),
    Prefix(&'a str),
    PrefixInsensitive(&'a str),
}

impl<'a> IR<'a> {
    fn wants_next(&self) -> bool {
        self.next.is_none() && !matches!(self.instr, IRI::Return)
    }

    fn set_next(&mut self, n: IRCell<'a>) {
        debug_assert!(self.wants_next());
        self.next = Some(n);
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Charset {
    bits: [bool; 256],
}

impl Charset {
    const fn no() -> Self {
        Charset { bits: [false; 256] }
    }

    const fn yes() -> Self {
        Charset { bits: [true; 256] }
    }

    fn fill(&mut self, value: bool) {
        self.bits.fill(value);
    }

    fn invert(&mut self) {
        for b in &mut self.bits {
            *b = !*b;
        }
    }

    fn set(&mut self, index: u8, value: bool) {
        self.bits[index as usize] = value;
    }

    fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b)
    }

    fn covers_char(&self, b: u8) -> bool {
        self.bits[b as usize]
    }

    fn covers_char_insensitive(&self, b: u8) -> bool {
        self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
    }

    fn covers_str(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| self.bits[b as usize])
    }

    fn covers_str_insensitive(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| {
            self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
        })
    }

    fn is_superset(&self, other: &Charset) -> bool {
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            if s && !o {
                return false;
            }
        }
        true
    }

    fn is_strict_superset(&self, other: &Charset) -> bool {
        let mut has_extra = false;
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            if !s && o {
                return false;
            }
            has_extra |= s && !o;
        }
        has_extra
    }
}

impl Default for Charset {
    fn default() -> Self {
        Self::no()
    }
}

impl fmt::Debug for Charset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let show_char = |f: &mut fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b == b'"' {
                write!(f, "&quot;")
            } else if b.is_ascii_graphic() {
                let b = b as char;
                write!(f, "{b}")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !self.bits[beg] {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && self.bits[end] {
                end += 1;
            }

            if !first {
                write!(f, ", ")?;
            }
            show_char(f, beg)?;
            if end - beg > 1 {
                write!(f, "-")?;
                show_char(f, end - 1)?;
            }

            beg = end;
            first = false;
        }

        write!(f, "]")
    }
}

impl<I> Index<I> for Charset
where
    [bool]: Index<I>,
{
    type Output = <[bool] as Index<I>>::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.bits.index(index)
    }
}

impl<I> IndexMut<I> for Charset
where
    [bool]: IndexMut<I>,
{
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.bits.index_mut(index)
    }
}

impl<'a> Intern<'a, Charset> for Vec<&'a Charset> {
    fn intern(&mut self, arena: &'a Arena, value: &Charset) -> &'a Charset {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena.alloc_uninit().write(value.clone());
            self.push(s);
            s
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    ProgramCounter,
    InputOffset,
    HighlightStart,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
}

impl Register {
    const FIRST_USER_REG: usize = 3; // aka x3
    const COUNT: usize = 16;

    fn from_usize(value: usize) -> Self {
        debug_assert!(value < Self::COUNT);
        unsafe { transmute::<u8, Register>(value as u8) }
    }

    fn mnemonic(&self) -> &'static str {
        match self {
            Register::ProgramCounter => "pc",
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::X3 => "x3",
            Register::X4 => "x4",
            Register::X5 => "x5",
            Register::X6 => "x6",
            Register::X7 => "x7",
            Register::X8 => "x8",
            Register::X9 => "x9",
            Register::X10 => "x10",
            Register::X11 => "x11",
            Register::X12 => "x12",
            Register::X13 => "x13",
            Register::X14 => "x14",
            Register::X15 => "x15",
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mnemonic())
    }
}

#[repr(C)]
#[derive(Default, Clone, Copy)]
pub struct Registers {
    pub pc: u32,  // x0 = ProgramCounter
    pub off: u32, // x1 = InputOffset
    pub hs: u32,  // x2 = HighlightStart
    pub x3: u32,
    pub x4: u32,
    pub x5: u32,
    pub x6: u32,
    pub x7: u32,
    pub x8: u32,
    pub x9: u32,
    pub x10: u32,
    pub x11: u32,
    pub x12: u32,
    pub x13: u32,
    pub x14: u32,
    pub x15: u32,
}

impl Registers {
    const COUNT: usize = 16;

    #[inline(always)]
    pub fn get(&self, reg: usize) -> u32 {
        debug_assert!(reg < Self::COUNT);
        unsafe { (self as *const _ as *const u32).add(reg).read() }
    }

    #[inline(always)]
    pub fn set(&mut self, reg: usize, val: u32) {
        debug_assert!(reg < Self::COUNT);
        unsafe { (self as *mut _ as *mut u32).add(reg).write(val) }
    }
}

struct RegisterAllocator(u16);

impl RegisterAllocator {
    fn new() -> Self {
        RegisterAllocator((1 << Register::FIRST_USER_REG) - 1)
    }

    fn alloc(&mut self) -> Option<Register> {
        let available = !self.0;
        if available == 0 {
            return None;
        }

        let idx = available.trailing_zeros() as usize;
        self.0 |= 1 << idx;
        Some(Register::from_usize(idx))
    }

    fn dealloc(&mut self, reg: Register) {
        let idx = reg as usize;
        if idx >= Register::FIRST_USER_REG {
            self.0 &= !(1 << idx);
        }
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

pub struct AnnotatedInstruction<'a> {
    pub instr: Instruction,
    pub label: &'a str,
}

#[allow(dead_code)]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // NOTE: This allows for jumps by manipulating Register::ProgramCounter.
    Mov { dst: Register, src: Register },
    Add { dst: Register, src: Register },
    Sub { dst: Register, src: Register },
    MovImm { dst: Register, imm: u32 },
    AddImm { dst: Register, imm: u32 },
    SubImm { dst: Register, imm: u32 },

    Call { tgt: u32 },
    Return,

    JumpEQ { lhs: Register, rhs: Register, tgt: u32 }, // ==
    JumpNE { lhs: Register, rhs: Register, tgt: u32 }, // !=
    JumpLT { lhs: Register, rhs: Register, tgt: u32 }, // <
    JumpLE { lhs: Register, rhs: Register, tgt: u32 }, // <=
    JumpGT { lhs: Register, rhs: Register, tgt: u32 }, // >
    JumpGE { lhs: Register, rhs: Register, tgt: u32 }, // >=

    // Jumps to `tgt` if we're at the end of the line.
    JumpIfEndOfLine { tgt: u32 },

    // Jumps to `tgt` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: u32, tgt: u32 },
    JumpIfMatchPrefix { idx: u32, tgt: u32 },
    JumpIfMatchPrefixInsensitive { idx: u32, tgt: u32 },

    // Flushes the current HighlightKind to the output.
    FlushHighlight { kind: Register },

    // Awaits more input to be available.
    AwaitInput,
}

impl Instruction {
    // JumpIfMatchCharset, etc., are 1 byte opcode + two u32 parameters.
    pub const MAX_ENCODED_SIZE: usize = 9;

    pub fn address_offset(&self) -> Option<usize> {
        match *self {
            Instruction::MovImm { .. }
            | Instruction::AddImm { .. }
            | Instruction::SubImm { .. } => Some(2), // opcode + dst

            Instruction::Call { .. } => Some(1), // opcode

            Instruction::JumpEQ { .. }
            | Instruction::JumpNE { .. }
            | Instruction::JumpLT { .. }
            | Instruction::JumpLE { .. }
            | Instruction::JumpGT { .. }
            | Instruction::JumpGE { .. } => Some(2), // opcode + lhs/rhs pair

            Instruction::JumpIfEndOfLine { .. } => Some(1), // opcode

            Instruction::JumpIfMatchCharset { .. }
            | Instruction::JumpIfMatchPrefix { .. }
            | Instruction::JumpIfMatchPrefixInsensitive { .. } => Some(5), // opcode + idx

            _ => None,
        }
    }

    #[allow(clippy::identity_op)]
    pub fn encode<'a>(&self, arena: &'a Arena) -> Vec<u8, &'a Arena> {
        fn enc_reg_pair(lo: Register, hi: Register) -> u8 {
            ((hi as u8) << 4) | (lo as u8)
        }

        fn enc_reg_single(lo: Register) -> u8 {
            lo as u8
        }

        fn enc_u16(val: u16) -> [u8; 2] {
            val.to_le_bytes()
        }

        fn enc_u32(val: u32) -> [u8; 4] {
            val.to_le_bytes()
        }

        let mut bytes = Vec::with_capacity_in(16, arena);
        #[allow(clippy::missing_transmute_annotations)]
        bytes.push(unsafe { std::mem::transmute(std::mem::discriminant(self)) });

        match *self {
            Instruction::Mov { dst, src }
            | Instruction::Add { dst, src }
            | Instruction::Sub { dst, src } => {
                bytes.push(enc_reg_pair(dst, src));
            }
            Instruction::MovImm { dst, imm }
            | Instruction::AddImm { dst, imm }
            | Instruction::SubImm { dst, imm } => {
                bytes.push(enc_reg_single(dst));
                bytes.extend_from_slice(&enc_u32(imm));
            }

            Instruction::Call { tgt } => {
                bytes.extend_from_slice(&enc_u32(tgt));
            }
            Instruction::Return => {}

            Instruction::JumpEQ { lhs, rhs, tgt }
            | Instruction::JumpNE { lhs, rhs, tgt }
            | Instruction::JumpLT { lhs, rhs, tgt }
            | Instruction::JumpLE { lhs, rhs, tgt }
            | Instruction::JumpGT { lhs, rhs, tgt }
            | Instruction::JumpGE { lhs, rhs, tgt } => {
                bytes.push(enc_reg_pair(lhs, rhs));
                bytes.extend_from_slice(&enc_u32(tgt));
            }

            Instruction::JumpIfEndOfLine { tgt } => {
                bytes.extend_from_slice(&enc_u32(tgt));
            }
            Instruction::JumpIfMatchCharset { idx, tgt }
            | Instruction::JumpIfMatchPrefix { idx, tgt }
            | Instruction::JumpIfMatchPrefixInsensitive { idx, tgt } => {
                bytes.extend_from_slice(&enc_u32(idx));
                bytes.extend_from_slice(&enc_u32(tgt));
            }

            Instruction::FlushHighlight { kind } => {
                bytes.push(enc_reg_single(kind));
            }
            Instruction::AwaitInput => {}
        }

        bytes
    }

    pub fn decode(bytes: &[u8]) -> (Option<Self>, usize) {
        fn dec_reg_pair(b: u8) -> (Register, Register) {
            let hi = Register::from_usize((b >> 4) as usize);
            let lo = Register::from_usize((b & 0xF) as usize);
            (lo, hi)
        }

        fn dec_reg_single(b: u8) -> Register {
            Register::from_usize(b as usize)
        }

        fn dec_u32(bytes: &[u8]) -> u32 {
            u32::from_le_bytes(bytes[..4].try_into().unwrap())
        }

        let opcode = bytes[0];
        match opcode {
            0 => {
                let (dst, src) = dec_reg_pair(bytes[1]);
                (Some(Instruction::Mov { dst, src }), 2)
            }
            1 => {
                let (dst, src) = dec_reg_pair(bytes[1]);
                (Some(Instruction::Add { dst, src }), 2)
            }
            2 => {
                let (dst, src) = dec_reg_pair(bytes[1]);
                (Some(Instruction::Sub { dst, src }), 2)
            }
            3 => {
                let dst = dec_reg_single(bytes[1]);
                let imm = dec_u32(&bytes[2..]);
                (Some(Instruction::MovImm { dst, imm }), 6)
            }
            4 => {
                let dst = dec_reg_single(bytes[1]);
                let imm = dec_u32(&bytes[2..]);
                (Some(Instruction::AddImm { dst, imm }), 6)
            }
            5 => {
                let dst = dec_reg_single(bytes[1]);
                let imm = dec_u32(&bytes[2..]);
                (Some(Instruction::SubImm { dst, imm }), 6)
            }
            6 => (Some(Instruction::Call { tgt: dec_u32(&bytes[1..]) }), 5),
            7 => (Some(Instruction::Return), 1),
            8 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpEQ { lhs, rhs, tgt }), 6)
            }
            9 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpNE { lhs, rhs, tgt }), 6)
            }
            10 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpLT { lhs, rhs, tgt }), 6)
            }
            11 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpLE { lhs, rhs, tgt }), 6)
            }
            12 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpGT { lhs, rhs, tgt }), 6)
            }
            13 => {
                let (lhs, rhs) = dec_reg_pair(bytes[1]);
                let tgt = dec_u32(&bytes[2..]);
                (Some(Instruction::JumpGE { lhs, rhs, tgt }), 6)
            }
            14 => (Some(Instruction::JumpIfEndOfLine { tgt: dec_u32(&bytes[1..]) }), 5),
            15 => {
                let idx = dec_u32(&bytes[1..]);
                let tgt = dec_u32(&bytes[5..]);
                (Some(Instruction::JumpIfMatchCharset { idx, tgt }), 9)
            }
            16 => {
                let idx = dec_u32(&bytes[1..]);
                let tgt = dec_u32(&bytes[5..]);
                (Some(Instruction::JumpIfMatchPrefix { idx, tgt }), 9)
            }
            17 => {
                let idx = dec_u32(&bytes[1..]);
                let tgt = dec_u32(&bytes[5..]);
                (Some(Instruction::JumpIfMatchPrefixInsensitive { idx, tgt }), 9)
            }
            18 => {
                let kind = dec_reg_single(bytes[1]);
                (Some(Instruction::FlushHighlight { kind }), 2)
            }
            19 => (Some(Instruction::AwaitInput), 1),
            _ => (None, 1),
        }
    }

    pub fn mnemonic<'a>(
        &self,
        arena: &'a Arena,
        config: &MnemonicFormattingConfig,
    ) -> ArenaString<'a> {
        let mut str = ArenaString::new_in(arena);
        let _i = config.instruction_prefix;
        let i_ = config.instruction_suffix;
        let _r = config.register_prefix;
        let r_ = config.register_suffix;
        let _a = config.address_prefix;
        let a_ = config.address_suffix;
        let _n = config.numeric_prefix;
        let n_ = config.numeric_suffix;

        match *self {
            Instruction::Mov { dst, src } => {
                _ = write!(str, "{_i}mov{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::Add { dst, src } => {
                _ = write!(str, "{_i}add{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::Sub { dst, src } => {
                _ = write!(str, "{_i}sub{i_}    {_r}{dst}{r_}, {_r}{src}{r_}");
            }
            Instruction::MovImm { dst, imm } => {
                if dst == Register::ProgramCounter {
                    _ = write!(str, "{_i}movi{i_}   {_r}{dst}{r_}, {_a}{imm}{a_}");
                } else {
                    _ = write!(str, "{_i}movi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
                }
            }
            Instruction::AddImm { dst, imm } => {
                _ = write!(str, "{_i}addi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
            }
            Instruction::SubImm { dst, imm } => {
                _ = write!(str, "{_i}subi{i_}   {_r}{dst}{r_}, {_n}{imm}{n_}");
            }

            Instruction::Call { tgt } => {
                _ = write!(str, "{_i}call{i_}   {_a}{tgt}{a_}");
            }
            Instruction::Return => {
                _ = write!(str, "{_i}ret{i_}");
            }

            Instruction::JumpEQ { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jeq{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpNE { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jne{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpLT { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jlt{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpLE { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jle{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpGT { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jgt{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpGE { lhs, rhs, tgt } => {
                _ = write!(str, "{_i}jge{i_}    {_r}{lhs}{r_}, {_r}{rhs}{r_}, {_a}{tgt}{a_}");
            }

            Instruction::JumpIfEndOfLine { tgt } => {
                _ = write!(str, "{_i}jeol{i_}   {_a}{tgt}{a_}");
            }
            Instruction::JumpIfMatchCharset { idx, tgt } => {
                _ = write!(str, "{_i}jc{i_}     {_n}{idx}{n_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpIfMatchPrefix { idx, tgt } => {
                _ = write!(str, "{_i}jp{i_}     {_n}{idx}{n_}, {_a}{tgt}{a_}");
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, tgt } => {
                _ = write!(str, "{_i}jpi{i_}    {_n}{idx}{n_}, {_a}{tgt}{a_}");
            }

            Instruction::FlushHighlight { kind } => {
                _ = write!(str, "{_i}flush{i_}  {_r}{kind}{r_}");
            }
            Instruction::AwaitInput => {
                _ = write!(str, "{_i}await{i_}");
            }
        }

        str
    }
}

#[derive(Default)]
pub struct MnemonicFormattingConfig<'a> {
    // Color used for highlighting the instruction.
    pub instruction_prefix: &'a str,
    pub instruction_suffix: &'a str,

    // Color used for highlighting a register name.
    pub register_prefix: &'a str,
    pub register_suffix: &'a str,

    // Color used for highlighting an immediate value.
    pub address_prefix: &'a str,
    pub address_suffix: &'a str,

    // Color used for highlighting an immediate value.
    pub numeric_prefix: &'a str,
    pub numeric_suffix: &'a str,
}
