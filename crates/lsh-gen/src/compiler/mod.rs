// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//mod backend;
mod frontend;
mod optimizer;
mod regex;
mod tokenizer;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};

use stdext::arena::{Arena, ArenaString};

use self::frontend::*;
use self::tokenizer::*;
use crate::definitions::HighlightKind;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    line: usize,
    column: usize,
    message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}: {}", self.line, self.column, self.message)
    }
}

pub struct Compiler<'a> {
    arena: &'a Arena,
    functions: Vec<Function<'a>>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            arena,
            functions: Vec::new(),
            charsets: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn parse(&mut self, src: &str) -> CompileResult<()> {
        let mut parser = Parser::new(self, src);
        parser.run()?;
        Ok(())
    }

    pub fn optimize(&mut self) {
        optimizer::optimize(self);
    }

    fn alloc_noop(&self) -> NodeCell<'a> {
        self.alloc_node(Node::Add { next: None, dst: Register::Zero, src: Register::Zero, imm: 0 })
    }

    fn alloc_node(&self, node: Node<'a>) -> NodeCell<'a> {
        self.arena.alloc_uninit().write(RefCell::new(node))
    }

    fn intern_charset(&mut self, charset: &Charset) -> &'a Charset {
        self.charsets.intern(self.arena, charset)
    }

    fn intern_string(&mut self, s: &str) -> &'a str {
        self.strings.intern(self.arena, s)
    }

    fn visit_nodes_from(&self, root: NodeCell<'a>) -> NodeIter<'a> {
        NodeIter { stack: vec![root], visited: HashSet::new() }
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
            _ = writeln!(output, "    {}_start@{{shape: start}} --> N{:p}", func.name, func.body);

            let mut visited = HashSet::new();
            let mut to_visit = vec![func.body];

            while let Some(node_cell) = to_visit.pop() {
                if !visited.insert(node_cell.as_ptr()) {
                    continue;
                }

                let node = node_cell.borrow();
                match *node {
                    Node::Add { next, dst, src, imm } => {
                        _ = write!(output, "    N{node_cell:p}");
                        if dst == Register::Zero && src == Register::Zero && imm == 0 {
                            _ = write!(output, "[noop]");
                        } else if dst == Register::HighlightKind && src == Register::Zero {
                            _ = write!(output, "[hk = {:?}]", unsafe {
                                HighlightKind::from_usize(imm)
                            });
                        } else {
                            _ = write!(output, "[{} = ", dst.mnemonic());
                            if src != Register::Zero {
                                _ = write!(output, "{} + ", src.mnemonic());
                            }
                            if imm == usize::MAX {
                                _ = write!(output, "max]");
                            } else {
                                _ = write!(output, "{imm}]");
                            }
                        }
                        if let Some(next) = next {
                            _ = writeln!(output, " --> N{next:p}");
                            to_visit.push(next);
                        } else {
                            _ = writeln!(output);
                        }
                    }
                    Node::If { next, condition, then } => {
                        match condition {
                            Condition::Charset(cs) => {
                                _ = writeln!(output, "    N{node_cell:p}{{charset: {cs:?}}}");
                            }
                            Condition::Prefix(s) => {
                                _ = writeln!(output, "    N{node_cell:p}{{match: {s}}}");
                            }
                            Condition::PrefixInsensitive(s) => {
                                _ = writeln!(output, "    N{node_cell:p}{{imatch: {s}}}");
                            }
                        }
                        _ = writeln!(output, "    N{node_cell:p} -->|yes| N{then:p}");
                        to_visit.push(then);
                        if let Some(next) = next {
                            to_visit.push(next);
                            _ = writeln!(output, "    N{node_cell:p} -->|no| N{next:p}");
                        } else {
                            _ = writeln!(output);
                        }
                    }
                    Node::Call { next, name } => {
                        _ = write!(output, "    N{node_cell:p}[Call {name}]");
                        if let Some(next) = next {
                            to_visit.push(next);
                            _ = writeln!(output, " --> N{next:p}");
                        } else {
                            _ = writeln!(output);
                        }
                    }
                    Node::Return => {
                        _ = writeln!(output, "    N{node_cell:p}[return]");
                    }
                    Node::Flush { next } => {
                        _ = write!(output, "    N{node_cell:p}[flush]");
                        if let Some(next) = next {
                            to_visit.push(next);
                            _ = writeln!(output, " --> N{next:p}");
                        } else {
                            _ = writeln!(output);
                        }
                    }
                };
            }

            _ = writeln!(output, "  end");
        }

        output
    }
}

struct NodeIter<'a> {
    stack: Vec<NodeCell<'a>>,
    visited: HashSet<*const RefCell<Node<'a>>>,
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = NodeCell<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(cell) = self.stack.pop() {
            if self.visited.insert(cell) {
                let node = cell.borrow_mut();
                if let Node::If { then, .. } = *node {
                    self.stack.push(then);
                }
                if let Some(next) = node.next() {
                    self.stack.push(next);
                }
                return Some(cell);
            }
        }

        None
    }
}

pub enum IR<'a> {
    Program(NodeCell<'a>),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Zero,
    ProgramCounter,
    ProcedureStart,
    InputOffset,
    HighlightStart,
    HighlightKind,
}

impl Register {
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::ProgramCounter => "pc",
            Register::ProcedureStart => "ps",
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::HighlightKind => "hk",
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct Registers {
    pub zero: u32, // Zero
    pub pc: u32,   // ProgramCounter
    pub ps: u32,   // ProcedureStart
    pub off: u32,  // InputOffset
    pub hs: u32,   // HighlightStart
    pub hk: u32,   // HighlightKind
}

type NodeCell<'a> = &'a RefCell<Node<'a>>;

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: NodeCell<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Condition<'a> {
    Charset(&'a Charset),
    Prefix(&'a str),
    PrefixInsensitive(&'a str),
}

#[derive(Debug)]
pub enum Node<'a> {
    Add { next: Option<NodeCell<'a>>, dst: Register, src: Register, imm: usize },
    If { next: Option<NodeCell<'a>>, condition: Condition<'a>, then: NodeCell<'a> },
    Call { next: Option<NodeCell<'a>>, name: &'a str },
    Return,
    Flush { next: Option<NodeCell<'a>> },
}

impl<'a> Node<'a> {
    fn next(&self) -> Option<NodeCell<'a>> {
        match self {
            Node::Add { next, .. } => *next,
            Node::If { next, .. } => *next,
            Node::Call { next, .. } => *next,
            Node::Flush { next, .. } => *next,
            _ => None,
        }
    }

    fn set_next(&mut self, n: NodeCell<'a>) {
        let next = match self {
            Node::Add { next, .. } => next,
            Node::If { next, .. } => next,
            Node::Call { next, .. } => next,
            Node::Flush { next, .. } => next,
            _ => panic!("Cannot set next on this node type"),
        };
        debug_assert!(next.is_none());
        *next = Some(n);
    }

    fn set_next_if_none(&mut self, n: NodeCell<'a>) {
        let next = match self {
            Node::Add { next, .. } => next,
            Node::If { next, .. } => next,
            Node::Call { next, .. } => next,
            Node::Flush { next, .. } => next,
            _ => panic!("Cannot set next on this node type"),
        };
        if next.is_none() {
            *next = Some(n);
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Charset {
    bits: [bool; 256],
}

impl Charset {
    pub const fn no() -> Self {
        Charset { bits: [false; 256] }
    }

    pub const fn yes() -> Self {
        Charset { bits: [true; 256] }
    }

    pub fn fill(&mut self, value: bool) {
        self.bits.fill(value);
    }

    pub fn invert(&mut self) {
        for b in &mut self.bits {
            *b = !*b;
        }
    }

    pub fn set(&mut self, index: u8, value: bool) {
        self.bits[index as usize] = value;
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    pub fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b)
    }

    pub fn covers_char(&self, b: u8) -> bool {
        self.bits[b as usize]
    }

    pub fn covers_char_insensitive(&self, b: u8) -> bool {
        self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
    }

    pub fn covers_str(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| self.bits[b as usize])
    }

    pub fn covers_str_insensitive(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| {
            self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
        })
    }

    pub fn is_superset(&self, other: &Charset) -> bool {
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            if s && !o {
                return false;
            }
        }
        true
    }

    pub fn is_strict_superset(&self, other: &Charset) -> bool {
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
            if b.is_ascii_graphic() {
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

trait Intern<'a, T: ?Sized> {
    fn intern(&mut self, arena: &'a Arena, item: &T) -> &'a T;
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

fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    ArenaString::from_str(arena, s).leak()
}
