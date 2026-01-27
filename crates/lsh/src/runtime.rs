// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! LSH bytecode interpreter.
//!
//! ## Performance notes
//!
//! - The main loop is "unsafe". Profile before "cleaning" it up.
//! - `charset_gobble`, `inlined_mem(i)cmp` are hot paths.
//!
//! ## Gotchas
//!
//! - `Return` with empty stack resets the VM to entrypoint and clears registers.
//!   This is how the DSL returns to the "idle" state between tokens.
//! - `AwaitInput` only breaks the loop if `off >= line.len()`. If not at EOL, it's a no-op.
//!   This allows the DSL to say "wait for more input OR continue if there is some".
//! - The result always has a sentinel span at `line.len()`. Consumers can rely on this.

use std::fmt::{self, Debug, Write as _};
use std::mem;

use stdext::arena::{Arena, ArenaString};

/// A compiled language definition with its bytecode entrypoint.
pub struct Language {
    /// Unique identifier (e.g., "rust", "markdown").
    pub id: &'static str,
    /// Human-readable display name.
    pub name: &'static str,
    /// Bytecode address where execution begins for this language.
    pub entrypoint: u32,
}

impl PartialEq for &'static Language {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

/// A highlight span indicating that text from `start` to the next span has the given `kind`.
///
/// Spans are half-open: `[start, next.start)`. The final span in a line extends to EOL.
#[derive(Clone, PartialEq, Eq)]
pub struct Highlight<T> {
    /// Byte offset where this highlight begins.
    pub start: usize,
    /// The token/highlight type (e.g., keyword, string, comment).
    pub kind: T,
}

impl<T: Debug> Debug for Highlight<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {:?})", self.start, self.kind)
    }
}

/// The bytecode interpreter for syntax highlighting.
#[derive(Clone)]
pub struct Runtime<'pa, 'ps, 'pc> {
    assembly: &'pa [u8],
    strings: &'ps [&'ps str],
    charsets: &'pc [[u16; 16]],
    entrypoint: u32,
    stack: Vec<Registers>,
    registers: Registers,
}

/// Snapshot of the runtime state for incremental re-highlighting.
#[derive(Clone)]
pub struct RuntimeState {
    stack: Vec<Registers>,
    registers: Registers,
}

impl<'pa, 'ps, 'pc> Runtime<'pa, 'ps, 'pc> {
    pub fn new(
        assembly: &'pa [u8],
        strings: &'ps [&'ps str],
        charsets: &'pc [[u16; 16]],
        entrypoint: u32,
    ) -> Self {
        Runtime {
            assembly,
            strings,
            charsets,
            entrypoint,
            stack: Default::default(),
            registers: Registers { pc: entrypoint, ..Default::default() },
        }
    }

    pub fn snapshot(&self) -> RuntimeState {
        RuntimeState { stack: self.stack.clone(), registers: self.registers }
    }

    pub fn restore(&mut self, state: &RuntimeState) {
        self.stack = state.stack.clone();
        self.registers = state.registers;
    }

    /// Parse a single line and return highlight spans.
    ///
    /// Executes bytecode until the line is fully consumed or a `Return` resets the VM.
    /// The returned spans partition the line into highlighted regions.
    ///
    /// # Returns
    /// A vector of [`Highlight`] spans. Always contains at least two spans:
    /// one at offset 0 and one at `line.len()` as a sentinel.
    pub fn parse_next_line<'a, T: PartialEq + TryFrom<u32>>(
        &mut self,
        arena: &'a Arena,
        line: &[u8],
    ) -> Vec<Highlight<T>, &'a Arena> {
        let mut res: Vec<Highlight<T>, &Arena> = Vec::new_in(arena);

        self.registers.off = 0;
        self.registers.hs = 0;

        // By default, any line starts with HighlightKind::Other.
        // If the DSL yields anything, this will be overwritten.
        res.push(Highlight { start: 0, kind: unsafe { mem::zeroed() } });

        loop {
            instruction_decode!(self.assembly, self.registers.pc, {
                Mov { dst, src } => {
                    let s = self.registers.get(src);
                    self.registers.set(dst, s);
                }
                Add { dst, src } => {
                    let d = self.registers.get(dst);
                    let s = self.registers.get(src);
                    self.registers.set(dst, d.saturating_add(s));
                }
                Sub { dst, src } => {
                    let d = self.registers.get(dst);
                    let s = self.registers.get(src);
                    self.registers.set(dst, d.saturating_sub(s));
                }
                MovImm { dst, imm } => {
                    self.registers.set(dst, imm);
                }
                AddImm { dst, imm } => {
                    let d = self.registers.get(dst);
                    self.registers.set(dst, d.saturating_add(imm));
                }
                SubImm { dst, imm } => {
                    let d = self.registers.get(dst);
                    self.registers.set(dst, d.saturating_sub(imm));
                }

                Call { tgt } => {
                    // PC already points to the next instruction (return address)
                    self.stack.push(self.registers);
                    self.registers.pc = tgt;
                }
                Return => {
                    if let Some(registers) = self.stack.pop() {
                        self.registers = registers;
                    } else {
                        self.registers = unsafe { mem::zeroed() };
                        self.registers.pc = self.entrypoint;
                        break;
                    }
                }

                JumpEQ { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) == self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }
                JumpNE { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) != self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }
                JumpLT { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) < self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }
                JumpLE { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) <= self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }
                JumpGT { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) > self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }
                JumpGE { lhs, rhs, tgt } => {
                    if self.registers.get(lhs) >= self.registers.get(rhs) {
                        self.registers.pc = tgt;
                    }
                }

                JumpIfEndOfLine { tgt } => {
                    if (self.registers.off as usize) >= line.len() {
                        self.registers.pc = tgt;
                    }
                }

                JumpIfMatchCharset { idx, tgt } => {
                    let off = self.registers.off as usize;
                    let cs = &self.charsets[idx as usize];

                    if let Some(off) = Self::charset_gobble(line, off, cs) {
                        self.registers.off = off as u32;
                        self.registers.pc = tgt;
                    }
                }
                JumpIfMatchPrefix { idx, tgt } => {
                    let off = self.registers.off as usize;
                    let str = self.strings[idx as usize].as_bytes();

                    if Self::inlined_memcmp(line, off, str) {
                        self.registers.off = (off + str.len()) as u32;
                        self.registers.pc = tgt;
                    }
                }
                JumpIfMatchPrefixInsensitive { idx, tgt } => {
                    let off = self.registers.off as usize;
                    let str = self.strings[idx as usize].as_bytes();

                    if Self::inlined_memicmp(line, off, str) {
                        self.registers.off = (off + str.len()) as u32;
                        self.registers.pc = tgt;
                    }
                }

                FlushHighlight { kind } => {
                    let kind = self.registers.get(kind);
                    let kind = unsafe { kind.try_into().unwrap_unchecked() };
                    let start = (self.registers.hs as usize).min(line.len());

                    if let Some(last) = res.last_mut()
                        && (last.start == start || last.kind == kind)
                    {
                        last.kind = kind;
                    } else {
                        res.push(Highlight { start, kind });
                    }

                    self.registers.hs = self.registers.off;
                }
                AwaitInput => {
                    let off = self.registers.off as usize;
                    if off >= line.len() {
                        break;
                    }
                }

                _ => unreachable!(),
            });
        }

        // Ensure that there's a past-the-end highlight.
        if res.last().is_none_or(|last| last.start < line.len()) {
            res.push(Highlight { start: line.len(), kind: unsafe { mem::zeroed() } });
        }

        res
    }

    // TODO: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html#alternative-implementation
    #[inline]
    fn charset_gobble(haystack: &[u8], mut off: usize, cs: &[u16; 16]) -> Option<usize> {
        if off >= haystack.len() || !Self::in_set(cs, haystack[off]) {
            return None;
        }

        while {
            off += 1;
            off < haystack.len() && Self::in_set(cs, haystack[off])
        } {}

        Some(off)
    }

    /// A mini-memcmp implementation for short needles.
    /// Compares the `haystack` at `off` with the `needle`.
    #[inline]
    fn inlined_memcmp(haystack: &[u8], off: usize, needle: &[u8]) -> bool {
        unsafe {
            if off >= haystack.len() || haystack.len() - off < needle.len() {
                return false;
            }

            let a = haystack.as_ptr().add(off);
            let b = needle.as_ptr();
            let mut i = 0;

            while i < needle.len() {
                let a = *a.add(i);
                let b = *b.add(i);
                i += 1;
                if a != b {
                    return false;
                }
            }

            true
        }
    }

    /// Like `inlined_memcmp`, but case-insensitive.
    #[inline]
    fn inlined_memicmp(haystack: &[u8], off: usize, needle: &[u8]) -> bool {
        unsafe {
            if off >= haystack.len() || haystack.len() - off < needle.len() {
                return false;
            }

            let a = haystack.as_ptr().add(off);
            let b = needle.as_ptr();
            let mut i = 0;

            while i < needle.len() {
                // str in PrefixInsensitive(str) is expected to be lowercase, printable ASCII.
                let a = a.add(i).read().to_ascii_lowercase();
                let b = b.add(i).read();
                i += 1;
                if a != b {
                    return false;
                }
            }

            true
        }
    }

    #[inline]
    fn in_set(bitmap: &[u16; 16], byte: u8) -> bool {
        let lo_nibble = byte & 0xf;
        let hi_nibble = byte >> 4;

        let bitset = bitmap[lo_nibble as usize];
        let bitmask = 1u16 << hi_nibble;

        (bitset & bitmask) != 0
    }
}

#[repr(u8)]
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
    pub const FIRST_USER_REG: usize = 3; // aka x3
    pub const COUNT: usize = 16;

    #[inline(always)]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(value < Self::COUNT);
        unsafe { std::mem::transmute::<u8, Register>(value as u8) }
    }

    pub fn mnemonic(&self) -> &'static str {
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
    #[inline(always)]
    pub fn get(&self, reg: Register) -> u32 {
        unsafe { (self as *const _ as *const u32).add(reg as usize).read() }
    }

    #[inline(always)]
    pub fn set(&mut self, reg: Register, val: u32) {
        unsafe { (self as *mut _ as *mut u32).add(reg as usize).write(val) }
    }
}

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

macro_rules! instruction_decode {
    ($assembly:expr, $pc:expr, {
        Mov { $mov_dst:ident, $mov_src:ident } => $mov_handler:block
        Add { $add_dst:ident, $add_src:ident } => $add_handler:block
        Sub { $sub_dst:ident, $sub_src:ident } => $sub_handler:block
        MovImm { $movi_dst:ident, $movi_imm:ident } => $movi_handler:block
        AddImm { $addi_dst:ident, $addi_imm:ident } => $addi_handler:block
        SubImm { $subi_dst:ident, $subi_imm:ident } => $subi_handler:block

        Call { $call_tgt:ident } => $call_handler:block
        Return => $ret_handler:block

        JumpEQ { $jeq_lhs:ident, $jeq_rhs:ident, $jeq_tgt:ident } => $jeq_handler:block
        JumpNE { $jne_lhs:ident, $jne_rhs:ident, $jne_tgt:ident } => $jne_handler:block
        JumpLT { $jlt_lhs:ident, $jlt_rhs:ident, $jlt_tgt:ident } => $jlt_handler:block
        JumpLE { $jle_lhs:ident, $jle_rhs:ident, $jle_tgt:ident } => $jle_handler:block
        JumpGT { $jgt_lhs:ident, $jgt_rhs:ident, $jgt_tgt:ident } => $jgt_handler:block
        JumpGE { $jge_lhs:ident, $jge_rhs:ident, $jge_tgt:ident } => $jge_handler:block

        JumpIfEndOfLine { $jeol_tgt:ident } => $jeol_handler:block

        JumpIfMatchCharset { $jc_idx:ident, $jc_tgt:ident } => $jc_handler:block
        JumpIfMatchPrefix { $jp_idx:ident, $jp_tgt:ident } => $jp_handler:block
        JumpIfMatchPrefixInsensitive { $jpi_idx:ident, $jpi_tgt:ident } => $jpi_handler:block

        FlushHighlight { $flush_kind:ident } => $flush_handler:block
        AwaitInput => $await_handler:block

        _ => $bad_opcode:expr $(,)?
    }) => {{
        #[inline(always)]
        fn dec_reg_single(bytes: &[u8], off: usize) -> Register {
            let b = unsafe { *bytes.as_ptr().add(off) as usize };
            Register::from_usize(b & 0xf)
        }

        #[inline(always)]
        fn dec_reg_pair(bytes: &[u8], off: usize) -> (Register, Register) {
            let b = unsafe { *bytes.as_ptr().add(off) as usize };
            let dst = Register::from_usize(b & 0xf);
            let src = Register::from_usize(b >> 4);
            (dst, src)
        }

        #[inline(always)]
        fn dec_u32(bytes: &[u8], off: usize) -> u32 {
            unsafe { (bytes.as_ptr().add(off) as *const u32).read_unaligned() }
        }

        let __asm: &[u8] = $assembly;
        let __off = $pc as usize;

        // The use of unsafe code above boosts performance by about 10%. We rely on the code
        // generator to emit invalid 0xff opcodes at the end of the instruction stream as padding.
        // This way we can read past-the-end, even if the last non-0xff instruction is truncated.
        let __opcode = __asm[__off];

        match __opcode {
            0 => {
                $pc += 2;
                let ($mov_dst, $mov_src) = dec_reg_pair(__asm, __off + 1);
                $mov_handler
            }
            1 => {
                $pc += 2;
                let ($add_dst, $add_src) = dec_reg_pair(__asm, __off + 1);
                $add_handler
            }
            2 => {
                $pc += 2;
                let ($sub_dst, $sub_src) = dec_reg_pair(__asm, __off + 1);
                $sub_handler
            }
            3 => {
                $pc += 6;
                let $movi_dst = dec_reg_single(__asm, __off + 1);
                let $movi_imm = dec_u32(__asm, __off + 2);
                $movi_handler
            }
            4 => {
                $pc += 6;
                let $addi_dst = dec_reg_single(__asm, __off + 1);
                let $addi_imm = dec_u32(__asm, __off + 2);
                $addi_handler
            }
            5 => {
                $pc += 6;
                let $subi_dst = dec_reg_single(__asm, __off + 1);
                let $subi_imm = dec_u32(__asm, __off + 2);
                $subi_handler
            }
            6 => {
                $pc += 5;
                let $call_tgt = dec_u32(__asm, __off + 1);
                $call_handler
            }
            7 => {
                $pc += 1;
                $ret_handler
            }
            8 => {
                $pc += 6;
                let ($jeq_lhs, $jeq_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jeq_tgt = dec_u32(__asm, __off + 2);
                $jeq_handler
            }
            9 => {
                $pc += 6;
                let ($jne_lhs, $jne_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jne_tgt = dec_u32(__asm, __off + 2);
                $jne_handler
            }
            10 => {
                $pc += 6;
                let ($jlt_lhs, $jlt_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jlt_tgt = dec_u32(__asm, __off + 2);
                $jlt_handler
            }
            11 => {
                $pc += 6;
                let ($jle_lhs, $jle_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jle_tgt = dec_u32(__asm, __off + 2);
                $jle_handler
            }
            12 => {
                $pc += 6;
                let ($jgt_lhs, $jgt_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jgt_tgt = dec_u32(__asm, __off + 2);
                $jgt_handler
            }
            13 => {
                $pc += 6;
                let ($jge_lhs, $jge_rhs) = dec_reg_pair(__asm, __off + 1);
                let $jge_tgt = dec_u32(__asm, __off + 2);
                $jge_handler
            }
            14 => {
                $pc += 5;
                let $jeol_tgt = dec_u32(__asm, __off + 1);
                $jeol_handler
            }
            15 => {
                $pc += 9;
                let $jc_idx = dec_u32(__asm, __off + 1);
                let $jc_tgt = dec_u32(__asm, __off + 5);
                $jc_handler
            }
            16 => {
                $pc += 9;
                let $jp_idx = dec_u32(__asm, __off + 1);
                let $jp_tgt = dec_u32(__asm, __off + 5);
                $jp_handler
            }
            17 => {
                $pc += 9;
                let $jpi_idx = dec_u32(__asm, __off + 1);
                let $jpi_tgt = dec_u32(__asm, __off + 5);
                $jpi_handler
            }
            18 => {
                $pc += 2;
                let $flush_kind = dec_reg_single(__asm, __off + 1);
                $flush_handler
            }
            19 => {
                $pc += 1;
                $await_handler
            }
            _ => $bad_opcode,
        }
    }};
}

use instruction_decode;

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
