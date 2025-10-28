// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//mod backend;
mod parser;
mod regex;
mod tokenizer;

use std::cell::RefCell;
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};

use stdext::arena::{Arena, ArenaString};

use self::parser::*;
use self::tokenizer::*;

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

    /// Parses the given source code into a graph-based IR.
    pub fn parse(&mut self, src: &str) -> CompileResult<()> {
        let mut parser = Parser::new(self, src);
        parser.run()?;
        Ok(())
    }

    fn alloc_noop(&self) -> NodeCell<'a> {
        self.alloc_node(Node::Add { dst: Register::Zero, src: Register::Zero, imm: 0, next: None })
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

    pub fn as_mermaid(&self) -> String {
        let mut output = String::new();
        output.push_str("graph TD\n");

        for func in &self.functions {
            _ = writeln!(output, "  subgraph Function: {}", func.name);

            let mut visited = std::collections::HashSet::new();
            let mut to_visit = vec![func.body];

            while let Some(node_cell) = to_visit.pop() {
                if !visited.insert(node_cell.as_ptr()) {
                    continue;
                }

                let node = node_cell.borrow();

                let node_id = format!("N{:p}", node_cell.as_ptr());
                let label = match &*node {
                    Node::Add { dst, src, imm, .. } => {
                        format!("Add {} = {} + {}", dst.mnemonic(), src.mnemonic(), imm)
                    }
                    Node::Return => "Return".to_string(),
                    Node::Jump { destination } => {
                        to_visit.push(*destination);
                        format!("Jump to N{:p}", destination.as_ptr())
                    }
                    Node::If { condition, then, .. } => {
                        to_visit.push(*then);
                        match condition {
                            Condition::MatchCharset(cs) => {
                                format!("If matches {:?}", cs)
                            }
                            Condition::MatchPrefix(s) => {
                                format!("If prefix == {:?}", s)
                            }
                            Condition::MatchPrefixInsensitive(s) => {
                                format!("If prefix (insensitive) == {:?}", s)
                            }
                        }
                    }
                    Node::Yield { color, .. } => format!("Yield color {}", color),
                    Node::Call { name, .. } => format!("Call {}", name),
                };

                _ = writeln!(output, "    {}[\"{}\"]", node_id, label);

                if let Some(next) = node.next() {
                    let next_id = format!("N{:p}", next.as_ptr());
                    _ = writeln!(output, "    {} --> {}", node_id, next_id);
                    to_visit.push(next);
                }
            }

            output.push_str("  end\n");
        }

        output
    }
}

pub enum IR<'a> {
    Program(NodeCell<'a>),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: NodeCell<'a>,
}

#[derive(Debug)]
pub enum Condition<'a> {
    MatchCharset(&'a Charset),
    MatchPrefix(&'a str),
    MatchPrefixInsensitive(&'a str),
}

#[derive(Debug)]
pub enum Node<'a> {
    Add { dst: Register, src: Register, imm: usize, next: Option<NodeCell<'a>> },
    Return,
    Jump { destination: NodeCell<'a> },
    If { condition: Condition<'a>, then: NodeCell<'a>, next: Option<NodeCell<'a>> },
    Yield { color: &'a str, next: Option<NodeCell<'a>> },
    Call { name: &'a str, next: Option<NodeCell<'a>> },
}

impl<'a> Node<'a> {
    fn next(&self) -> Option<NodeCell<'a>> {
        match self {
            Node::Add { next, .. } => *next,
            Node::If { next, .. } => *next,
            Node::Yield { next, .. } => *next,
            Node::Call { next, .. } => *next,
            _ => None,
        }
    }

    fn set_next(&mut self, _next: NodeCell<'a>) {
        match self {
            Node::Add { next, .. } => *next = Some(_next),
            Node::If { next, .. } => *next = Some(_next),
            Node::Yield { next, .. } => *next = Some(_next),
            Node::Call { next, .. } => *next = Some(_next),
            _ => panic!("Cannot set next on this node type"),
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
