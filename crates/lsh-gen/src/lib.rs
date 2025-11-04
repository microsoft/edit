// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![feature(allocator_api)]
#![allow(irrefutable_let_patterns, unused, clippy::upper_case_acronyms)]

mod backend;
mod compiler;
mod frontend;
mod generator;
mod optimizer;
mod regex;
mod tokenizer;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write as _;
use std::ops::{Index, IndexMut};
use std::path::Path;
use std::str::FromStr;

use stdext::arena::*;

use self::compiler::*;
use self::frontend::*;
pub use self::generator::Generator;
use self::tokenizer::*;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}: {}", self.line, self.column, self.message)
    }
}

pub struct Assembly<'a> {
    pub instructions: Vec<AnnotatedInstruction<'a>>,
    pub charsets: Vec<&'a Charset>,
    pub strings: Vec<&'a str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,

    Other,
    Comment,
    Number,
    String,
    Variable,
    Operator,
    Keyword,
    Method,
}

impl HighlightKind {
    #[inline]
    pub const fn as_usize(self) -> usize {
        unsafe { std::mem::transmute::<Self, u8>(self) as usize }
    }

    /// # Safety
    /// Don't pass the wrong thing you dummy.
    #[inline]
    pub const unsafe fn from_usize(value: usize) -> Self {
        debug_assert!(value <= Self::Method.as_usize());
        unsafe { std::mem::transmute::<u8, Self>(value as u8) }
    }
}

impl FromStr for HighlightKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use HighlightKind::*;
        match s {
            "Black" => Ok(Black),
            "Red" => Ok(Red),
            "Green" => Ok(Green),
            "Yellow" => Ok(Yellow),
            "Blue" => Ok(Blue),
            "Magenta" => Ok(Magenta),
            "Cyan" => Ok(Cyan),
            "White" => Ok(White),
            "BrightBlack" => Ok(BrightBlack),
            "BrightRed" => Ok(BrightRed),
            "BrightGreen" => Ok(BrightGreen),
            "BrightYellow" => Ok(BrightYellow),
            "BrightBlue" => Ok(BrightBlue),
            "BrightMagenta" => Ok(BrightMagenta),
            "BrightCyan" => Ok(BrightCyan),
            "BrightWhite" => Ok(BrightWhite),

            "Other" => Ok(Other),
            "Comment" => Ok(Comment),
            "Number" => Ok(Number),
            "String" => Ok(String),
            "Variable" => Ok(Variable),
            "Operator" => Ok(Operator),
            "Keyword" => Ok(Keyword),
            "Method" => Ok(Method),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Zero,
    ProgramCounter,
    InputOffset,
    HighlightStart,
    HighlightKind,
}

impl Register {
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::ProgramCounter => "pc",
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
    pub off: u32,  // InputOffset
    pub hs: u32,   // HighlightStart
    pub hk: u32,   // HighlightKind
}

impl Registers {
    #[inline(always)]
    pub fn get(&self, reg: usize) -> u32 {
        debug_assert!(reg < 5);
        unsafe { (self as *const _ as *const u32).add(reg).read() }
    }

    #[inline(always)]
    pub fn set(&mut self, reg: usize, val: u32) {
        debug_assert!(reg < 5);
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
    //   dst[31:12] |      idx[11:4]       | 0001
    //
    // NOTE: The VM takes care of saving the return address.
    Call { dst: usize },

    // Encoding:
    //                                       0010
    Return,

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0011
    //
    // Jumps to `dst` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0100
    JumpIfMatchPrefix { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0101
    JumpIfMatchPrefixInsensitive { idx: usize, dst: usize },

    // Encoding:
    //                                       0110
    //
    // Flushes the current HighlightKind to the output.
    FlushHighlight,

    // Encoding:
    //   dst[31:12] |                      | 0111
    //
    // Checks if we're at the end and exit if so.
    // Otherwise, jumps to `dst`.
    Loop { dst: usize },
}

impl Instruction {
    const IMM_MAX: usize = (1 << 20) - 1;

    #[allow(clippy::identity_op)]
    pub fn encode(&self) -> u32 {
        match *self {
            Instruction::Add { dst, src, imm } => {
                Self::cast_imm(imm) | (src as u32) << 8 | (dst as u32) << 4 | 0b0000
            }
            Instruction::Call { dst } => Self::cast_imm(dst) | 0b0001,
            Instruction::Return => 0b0010,
            Instruction::JumpIfMatchCharset { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0011
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0100
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0101
            }
            Instruction::FlushHighlight => 0b0110,
            Instruction::Loop { dst } => Self::cast_imm(dst) | 0b0111,
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
            Instruction::Call { dst } => format!("call  {dst}"),
            Instruction::Return => "ret".to_string(),
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
            Instruction::Loop { dst } => {
                format!("loop  {dst}")
            }
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
