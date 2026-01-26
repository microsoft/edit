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

use std::fmt::Debug;
use std::mem;
use std::path::Path;

use stdext::arena::{Arena, scratch_arena};

use crate::compiler::Registers;

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
pub struct Higlight<T> {
    /// Byte offset where this highlight begins.
    pub start: usize,
    /// The token/highlight type (e.g., keyword, string, comment).
    pub kind: T,
}

impl<T: Debug> Debug for Higlight<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {:?})", self.start, self.kind)
    }
}

/// The bytecode interpreter for syntax highlighting.
#[derive(Clone)]
pub struct Engine<'pa, 'ps, 'pc> {
    assembly: &'pa [u8],
    strings: &'ps [&'ps str],
    charsets: &'pc [[u16; 16]],
    entrypoint: u32,
    stack: Vec<u32>,
    registers: Registers,
}

/// Snapshot of engine state for incremental re-highlighting.
///
/// Save with [`Engine::snapshot`], restore with [`Engine::restore`].
/// This allows re-highlighting from a known state when only part of a file changes.
#[derive(Clone)]
pub struct EngineState {
    stack: Vec<u32>,
    registers: Registers,
}

impl<'pa, 'ps, 'pc> Engine<'pa, 'ps, 'pc> {
    pub fn new(
        assembly: &'pa [u8],
        strings: &'ps [&'ps str],
        charsets: &'pc [[u16; 16]],
        entrypoint: u32,
    ) -> Self {
        Engine {
            assembly,
            strings,
            charsets,
            entrypoint,
            stack: Default::default(),
            registers: Registers { pc: entrypoint, ..Default::default() },
        }
    }

    pub fn snapshot(&self) -> EngineState {
        EngineState { stack: self.stack.clone(), registers: self.registers }
    }

    pub fn restore(&mut self, state: &EngineState) {
        self.stack = state.stack.clone();
        self.registers = state.registers;
    }

    /// Parse a single line and return highlight spans.
    ///
    /// Executes bytecode until the line is fully consumed or a `Return` resets the VM.
    /// The returned spans partition the line into highlighted regions.
    ///
    /// # Arguments
    /// * `arena` - Allocator for the result vector
    /// * `line` - The line bytes to highlight (without trailing newline)
    ///
    /// # Returns
    /// A vector of [`Higlight`] spans. Always contains at least two spans:
    /// one at offset 0 and one at `line.len()` as a sentinel.
    pub fn parse_next_line<'a, T: PartialEq + TryFrom<u32>>(
        &mut self,
        arena: &'a Arena,
        line: &[u8],
    ) -> Vec<Higlight<T>, &'a Arena> {
        let mut res: Vec<Higlight<T>, &Arena> = Vec::new_in(arena);
        let mut reset = false;

        self.registers.off = 0;
        self.registers.hs = 0;

        // By default, any line starts with HighlightKind::Other.
        // If the DSL yields anything, this will be overwritten.
        res.push(Higlight { start: 0, kind: unsafe { mem::zeroed() } });

        loop {
            let pc = self.registers.pc as usize;
            let op = self.assembly[pc];
            self.registers.pc += 1;

            match op {
                0 => {
                    // Mov { dst: Register, src: Register }
                    let (dst, src) = self.read_reg_pair();
                    let s = self.registers.get(src);
                    self.registers.set(dst, s);
                }
                1 => {
                    // Add { dst: Register, src: Register }
                    let (dst, src) = self.read_reg_pair();
                    let d = self.registers.get(dst);
                    let s = self.registers.get(src);
                    self.registers.set(dst, d.saturating_add(s));
                }
                2 => {
                    // Sub { dst: Register, src: Register }
                    let (dst, src) = self.read_reg_pair();
                    let d = self.registers.get(dst);
                    let s = self.registers.get(src);
                    self.registers.set(dst, d.saturating_sub(s));
                }
                3 => {
                    // MovImm { dst: Register, imm: u32 }
                    let (dst, _) = self.read_reg_pair();
                    let imm = self.read::<u32>();
                    self.registers.set(dst, imm);
                }
                4 => {
                    // AddImm { dst: Register, imm: u32 }
                    let (dst, _) = self.read_reg_pair();
                    let imm = self.read::<u32>();
                    let d = self.registers.get(dst);
                    self.registers.set(dst, d.saturating_add(imm));
                }
                5 => {
                    // SubImm { dst: Register, imm: u32 }
                    let (dst, _) = self.read_reg_pair();
                    let imm = self.read::<u32>();
                    let d = self.registers.get(dst);
                    self.registers.set(dst, d.saturating_sub(imm));
                }

                6 => {
                    // Call { tgt: u32 }
                    let tgt = self.read::<u32>();
                    self.stack.push(self.registers.pc);
                    self.registers.pc = tgt;
                }
                7 => {
                    // Return
                    if let Some(pc) = self.stack.pop() {
                        self.registers.pc = pc;
                    } else {
                        reset = true;
                        break;
                    }
                }

                8 => {
                    // JumpEQ { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) == self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }
                9 => {
                    // JumpNE { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) != self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }
                10 => {
                    // JumpLT { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) < self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }
                11 => {
                    // JumpLE { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) <= self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }
                12 => {
                    // JumpGT { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) > self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }
                13 => {
                    // JumpGE { lhs: Register, rhs: Register, tgt: u32 }
                    let (dst, src) = self.read_reg_pair();
                    let tgt = self.read::<u32>();
                    if self.registers.get(dst) >= self.registers.get(src) {
                        self.registers.pc = tgt;
                    }
                }

                14 => {
                    // JumpIfEndOfLine { tgt: u32 }
                    let tgt = self.read::<u32>();
                    if (self.registers.off as usize) >= line.len() {
                        self.registers.pc = tgt;
                    }
                }

                15 => {
                    // JumpIfMatchCharset { idx: u32, tgt: u32 }
                    let idx = self.read::<u32>();
                    let tgt = self.read::<u32>();
                    let off = self.registers.off as usize;
                    let cs = &self.charsets[idx as usize];

                    if let Some(off) = Self::charset_gobble(line, off, cs) {
                        self.registers.off = off as u32;
                        self.registers.pc = tgt;
                    }
                }
                16 => {
                    // JumpIfMatchPrefix { idx: u32, tgt: u32 }
                    let idx = self.read::<u32>();
                    let tgt = self.read::<u32>();
                    let off = self.registers.off as usize;
                    let str = self.strings[idx as usize].as_bytes();

                    if Self::inlined_memcmp(line, off, str) {
                        self.registers.off = (off + str.len()) as u32;
                        self.registers.pc = tgt;
                    }
                }
                17 => {
                    // JumpIfMatchPrefixInsensitive { idx: u32, tgt: u32 }
                    let idx = self.read::<u32>();
                    let tgt = self.read::<u32>();
                    let off = self.registers.off as usize;
                    let str = self.strings[idx as usize].as_bytes();

                    if Self::inlined_memicmp(line, off, str) {
                        self.registers.off = (off + str.len()) as u32;
                        self.registers.pc = tgt;
                    }
                }

                18 => {
                    // FlushHighlight
                    let (kind, _) = self.read_reg_pair();
                    let kind = self.registers.get(kind);
                    let kind = unsafe { kind.try_into().unwrap_unchecked() };
                    let start = (self.registers.hs as usize).min(line.len());

                    if let Some(last) = res.last_mut()
                        && (last.start == start || last.kind == kind)
                    {
                        last.kind = kind;
                    } else {
                        res.push(Higlight { start, kind });
                    }

                    self.registers.hs = self.registers.off;
                }
                19 => {
                    // AwaitInput
                    let off = self.registers.off as usize;
                    if off >= line.len() {
                        break;
                    }
                }

                _ => unreachable!(),
            }
        }

        // Ensure that there's a past-the-end highlight.
        if res.last().is_none_or(|last| last.start < line.len()) {
            res.push(Higlight { start: line.len(), kind: unsafe { mem::zeroed() } });
        }

        if reset {
            self.registers = unsafe { mem::zeroed() };
            self.registers.pc = self.entrypoint;
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

    #[inline]
    fn read<T: Copy>(&mut self) -> T {
        let pc = self.registers.pc as usize;
        self.registers.pc += mem::size_of::<T>() as u32;
        // The LSH backend emits extra 0xff instructions at the end, to allow
        // us to blindly read any instruction and only later check if it was valid.
        unsafe { (self.assembly.as_ptr().add(pc) as *const T).read_unaligned() }
    }

    #[inline]
    fn read_reg_pair(&mut self) -> (usize, usize) {
        let reg_pair = self.read::<u8>();
        let dst = (reg_pair & 0xf) as usize;
        let src = (reg_pair >> 4) as usize;
        (dst, src)
    }
}
