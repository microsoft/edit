// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

mod backend;
mod frontend;
mod optimizer;
mod regex;
mod tokenizer;

use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};

use stdext::arena::Arena;

use self::frontend::*;
use crate::{Intern, arena_clone_str};

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    pub path: String,
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}:{}: {}", self.path, self.line, self.column, self.message)
    }
}

pub struct Compiler<'a> {
    arena: &'a Arena,
    functions: Vec<Function<'a>>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
    highlight_kinds: Vec<HighlightKind<'a>>,
    next_vreg_id: std::cell::Cell<u32>,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            arena,
            functions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            highlight_kinds: vec![HighlightKind { identifier: "other", value: 0 }],
            next_vreg_id: std::cell::Cell::new(0),
        }
    }

    pub fn parse<'src>(&mut self, path: &'src str, src: &'src str) -> CompileResult<()> {
        let mut parser = Parser::new(self, path, src);
        parser.run()?;
        Ok(())
    }

    pub fn optimize(&mut self) {
        optimizer::optimize(self);
    }

    pub fn assemble(&mut self) -> CompileResult<Assembly<'a>> {
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

    fn chain_iri(&self, prev: IRCell<'a>, instr: IRI<'a>) -> IRCell<'a> {
        let ir = self.arena.alloc_uninit().write(RefCell::new(IR {
            next: None,
            instr,
            offset: usize::MAX,
        }));
        prev.borrow_mut().set_next(ir);
        ir
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
                let value = self.highlight_kinds.len();
                self.highlight_kinds.insert(idx, HighlightKind { identifier, value });
                idx
            }
        };
        &self.highlight_kinds[idx]
    }

    /// Allocate a new virtual register
    pub fn alloc_vreg(&self) -> VRegId {
        let id = self.next_vreg_id.get();
        self.next_vreg_id.set(id + 1);
        VRegId::new(id)
    }

    /// Create IR to save InputOffset to a virtual register (for backtracking).
    fn save_position(&self, dst: VRegId) -> IRCell<'a> {
        self.alloc_iri(IRI::CopyFromPhys { dst, src: Register::InputOffset })
    }

    /// Create IR to restore InputOffset from a virtual register (for backtracking).
    fn restore_position(&self, src: VRegId) -> IRCell<'a> {
        self.alloc_iri(IRI::CopyToPhys { dst: Register::InputOffset, src })
    }

    fn visit_nodes_from(&self, root: IRCell<'a>) -> TreeVisitor<'a> {
        let mut stack = VecDeque::new();
        stack.push_back(root);
        TreeVisitor { current: None, stack, visited: Default::default() }
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
                    IRI::SetHighlightKind { kind } => {
                        if let Some(hk) = self.highlight_kinds.iter().find(|hk| hk.value == kind) {
                            _ = write!(
                                output,
                                "[\"{offset}: hk = {} ({})\"]",
                                hk.value, hk.identifier
                            );
                        } else {
                            _ = write!(output, "[\"{offset}: hk = {kind}\"]");
                        }
                    }
                    IRI::IncOffset { amount } => {
                        if amount == usize::MAX {
                            _ = write!(output, "[\"{offset}: off = max\"]");
                        } else {
                            _ = write!(output, "[\"{offset}: off += {amount}\"]");
                        }
                    }
                    IRI::If { condition, then } => {
                        _ = write!(output, "{{\"{offset}: ");
                        _ = match condition {
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
                    IRI::Flush => {
                        _ = write!(output, "[{offset}: flush]");
                    }
                    IRI::AwaitInput => {
                        _ = write!(output, "[{offset}: await input]");
                    }
                    IRI::LoadImm { dst, value } => {
                        _ = write!(output, "[\"{offset}: {dst} = {value}\"]");
                    }
                    IRI::AddImm { dst, src, imm } => {
                        _ = write!(output, "[\"{{offset}}: {{dst}} = {{src}} + {{imm}}\"]");
                    }
                    IRI::CopyFromPhys { dst, src } => {
                        _ = write!(output, "[\"{offset}: {dst} = {}\"]", src.mnemonic());
                    }
                    IRI::CopyToPhys { dst, src } => {
                        _ = write!(output, "[\"{offset}: {} = {src}\"]", dst.mnemonic());
                    }
                    IRI::Push { mask } => {
                        _ = write!(output, "[\"{offset}: push {mask:#06x}\"]");
                    }
                    IRI::Pop { mask } => {
                        _ = write!(output, "[\"{offset}: pop {mask:#06x}\"]");
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
    pub value: usize,
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

#[derive(Debug, Clone)]
struct Function<'a> {
    name: &'a str,
    body: IRCell<'a>,
    public: bool,
    used_registers: u16,
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
        write!(f, "i{}", self.offset)
    }
}

type IRCell<'a> = &'a RefCell<IR<'a>>;

// IRI = Immediate Representation Instruction
#[derive(Debug, Clone, Copy)]
enum IRI<'a> {
    Noop,
    SetHighlightKind { kind: usize },
    IncOffset { amount: usize },
    LoadImm { dst: VRegId, value: i64 },
    AddImm { dst: VRegId, src: VRegId, imm: i64 },
    CopyFromPhys { dst: VRegId, src: Register },
    CopyToPhys { dst: Register, src: VRegId },
    If { condition: Condition<'a>, then: IRCell<'a> },
    Push { mask: u16 },
    Pop { mask: u16 },
    Call { name: &'a str },
    Return,
    Flush,
    AwaitInput,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VRegId(u32);

impl VRegId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for VRegId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Physical(Register),
    Virtual(VRegId),
    Immediate(i64),
}

impl Operand {
    pub fn is_virtual(&self) -> bool {
        matches!(self, Operand::Virtual(_))
    }

    pub fn as_virtual(&self) -> Option<VRegId> {
        match self {
            Operand::Virtual(v) => Some(*v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Condition<'a> {
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
    pub instructions: Vec<AnnotatedInstruction<'a>>,
    pub entrypoints: Vec<(&'a str, usize)>,
    pub charsets: Vec<&'a Charset>,
    pub strings: Vec<&'a str>,
    pub highlight_kinds: Vec<HighlightKind<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    Zero,
    ProgramCounter,
    InputOffset,
    HighlightStart,
    HighlightKind,
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
    fn mnemonic(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::ProgramCounter => "pc",
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::HighlightKind => "hk",
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

#[repr(C)]
#[derive(Default, Clone, Copy)]
pub struct Registers {
    zero: u32, // x0 = Zero
    pc: u32,   // x1 = ProgramCounter
    off: u32,  // x2 = InputOffset
    hs: u32,   // z3 = HighlightStart
    hk: u32,   // x4 = HighlightKind
    x5: u32,
    x6: u32,
    x7: u32,
    x8: u32,
    x9: u32,
    x10: u32,
    x11: u32,
    x12: u32,
    x13: u32,
    x14: u32,
    x15: u32,
}

impl Registers {
    #[inline(always)]
    fn get(&self, reg: usize) -> u32 {
        debug_assert!(reg < 16);
        unsafe { (self as *const _ as *const u32).add(reg).read() }
    }

    #[inline(always)]
    fn set(&mut self, reg: usize, val: u32) {
        debug_assert!(reg < 16);
        unsafe { (self as *mut _ as *mut u32).add(reg).write(val) }
    }
}

pub struct AnnotatedInstruction<'a> {
    pub instr: Instruction,
    pub label: &'a str,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // Encoding:
    //   imm[31:12] | src[11:8] | dst[7:4] | 0000
    //
    // NOTE: This allows for jumps by manipulating Register::ProgramCounter.
    Add { dst: Register, src: Register, imm: usize },

    // Encoding:
    //   mask[31:16] |                     | 0001
    //
    // Push registers specified by bitmask to call stack.
    // Bit N set means save register N.
    Push { mask: u16 },

    // Encoding:
    //   mask[31:16] |                     | 0010
    //
    // Pop registers specified by bitmask from call stack.
    // Bit N set means restore register N.
    Pop { mask: u16 },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0011
    //
    // NOTE: The VM takes care of saving the return address.
    Call { dst: usize },

    // Encoding:
    //                                       0100
    Return,

    // Encoding:
    //   dst[31:12] |                      | 0101
    //
    // Jumps to `dst` if we're at the end of the line.
    JumpIfEndOfLine { dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0110
    //
    // Jumps to `dst` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0111
    JumpIfMatchPrefix { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 1000
    JumpIfMatchPrefixInsensitive { idx: usize, dst: usize },

    // Encoding:
    //                                       1001
    //
    // Flushes the current HighlightKind to the output.
    FlushHighlight,

    // Encoding:
    //                                       1010
    // Awaits more input to be available.
    AwaitInput,
}

impl Instruction {
    const IMM_MAX: usize = (1 << 20) - 1;

    #[allow(clippy::identity_op)]
    pub fn encode(&self) -> u32 {
        match *self {
            Instruction::Add { dst, src, imm } => {
                Self::cast_imm(imm)
                    | Self::cast_bits(src as usize, 4, 8)
                    | Self::cast_bits(dst as usize, 4, 4)
                    | 0b0000
            }
            Instruction::Push { mask } => ((mask as u32) << 16) | 0b0001,
            Instruction::Pop { mask } => ((mask as u32) << 16) | 0b0010,
            Instruction::Call { dst } => Self::cast_imm(dst) | 0b0011,
            Instruction::Return => 0b0100,
            Instruction::JumpIfEndOfLine { dst } => Self::cast_imm(dst) | 0b0101,
            Instruction::JumpIfMatchCharset { idx, dst } => {
                Self::cast_imm(dst) | Self::cast_bits(idx, 8, 4) | 0b0110
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | Self::cast_bits(idx, 8, 4) | 0b0111
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
                Self::cast_imm(dst) | Self::cast_bits(idx, 8, 4) | 0b1000
            }
            Instruction::FlushHighlight => 0b1001,
            Instruction::AwaitInput => 0b1010,
        }
    }

    pub fn mnemonic(&self) -> String {
        match *self {
            Instruction::Add { dst, src, imm } => {
                let mut str = String::with_capacity(48);
                _ = write!(str, "add   {}, {}, ", dst.mnemonic(), src.mnemonic());
                if imm > 1024 * 1024 {
                    _ = write!(str, "{:#x}", imm & Self::IMM_MAX);
                } else {
                    _ = write!(str, "{imm}");
                }
                str
            }
            Instruction::Push { mask } => format!("push  {mask:#06x}"),
            Instruction::Pop { mask } => format!("pop   {mask:#06x}"),
            Instruction::Call { dst } => format!("call  {dst}"),
            Instruction::Return => "ret".to_string(),
            Instruction::JumpIfEndOfLine { dst } => format!("jeol  {dst}"),
            Instruction::JumpIfMatchCharset { idx, dst } => {
                format!("jc    {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                format!("jp    {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
                format!("jpi   {idx:?}, {dst}")
            }
            Instruction::FlushHighlight => "flush".to_string(),
            Instruction::AwaitInput => "await".to_string(),
        }
    }

    fn cast_imm(imm: usize) -> u32 {
        if imm == usize::MAX {
            (Self::IMM_MAX << 12) as u32
        } else {
            assert!(imm <= Self::IMM_MAX);
            (imm << 12) as u32
        }
    }

    fn cast_bits(val: usize, bits: usize, shift: usize) -> u32 {
        assert!(val < (1 << bits));
        (val as u32) << shift
    }
}
