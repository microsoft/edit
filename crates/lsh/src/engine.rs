// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::Debug;
use std::mem;
use std::path::Path;

use stdext::arena::{Arena, scratch_arena};

use crate::compiler::Registers;

pub struct Language {
    pub id: &'static str,
    pub name: &'static str,
    pub entrypoint: u32,
}

impl PartialEq for &'static Language {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Higlight<T> {
    pub start: usize,
    pub kind: T,
}

impl<T: Debug> Debug for Higlight<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {:?})", self.start, self.kind)
    }
}

#[derive(Clone)]
pub struct Engine {
    assembly: &'static [u8],
    strings: &'static [&'static str],
    charsets: &'static [[u16; 16]],
    entrypoint: u32,
    stack: Vec<u32>,
    registers: Registers,
}

#[derive(Clone)]
pub struct EngineState {
    stack: Vec<u32>,
    registers: Registers,
}

impl Engine {
    pub fn new(
        assembly: &'static [u8],
        strings: &'static [&'static str],
        charsets: &'static [[u16; 16]],
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

    pub fn parse_next_line<'a, T: PartialEq + TryFrom<u32>>(
        &mut self,
        arena: &'a Arena,
        line: &[u8],
    ) -> Vec<Higlight<T>, &'a Arena> {
        let mut res: Vec<Higlight<T>, &Arena> = Vec::new_in(arena);
        let mut reset = false;

        self.registers.off = 0;
        self.registers.hs = 0;

        loop {
            unsafe {
                let pc = self.registers.pc as usize;
                let op = self.assembly[pc];
                self.registers.pc += 1;

                match op {
                    0 => {
                        // Mov { dst: Register, src: Register }
                        let (dst, src) = self.read_dst_src();
                        let s = self.registers.get(src);
                        self.registers.set(dst, s);
                    }
                    1 => {
                        // Add { dst: Register, src: Register }
                        let (dst, src) = self.read_dst_src();
                        let d = self.registers.get(dst);
                        let s = self.registers.get(src);
                        self.registers.set(dst, d.saturating_add(s));
                    }
                    2 => {
                        // Sub { dst: Register, src: Register }
                        let (dst, src) = self.read_dst_src();
                        let d = self.registers.get(dst);
                        let s = self.registers.get(src);
                        self.registers.set(dst, d.saturating_sub(s));
                    }
                    3 => {
                        // MovImm { dst: Register, imm: u32 }
                        let (dst, _) = self.read_dst_src();
                        let imm = self.read::<u32>();
                        self.registers.set(dst, imm);
                    }
                    4 => {
                        // AddImm { dst: Register, imm: u32 }
                        let (dst, _) = self.read_dst_src();
                        let imm = self.read::<u32>();
                        let d = self.registers.get(dst);
                        self.registers.set(dst, d.saturating_add(imm));
                    }
                    5 => {
                        // SubImm { dst: Register, imm: u32 }
                        let (dst, _) = self.read_dst_src();
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
                        let (dst, src) = self.read_dst_src();
                        let tgt = self.read::<u32>();
                        if self.registers.get(dst) == self.registers.get(src) {
                            self.registers.pc = tgt;
                        }
                    }
                    9 => {
                        // JumpNE { lhs: Register, rhs: Register, tgt: u32 }
                        let (dst, src) = self.read_dst_src();
                        let tgt = self.read::<u32>();
                        if self.registers.get(dst) != self.registers.get(src) {
                            self.registers.pc = tgt;
                        }
                    }
                    10 => {
                        // JumpLT { lhs: Register, rhs: Register, tgt: u32 }
                        let (dst, src) = self.read_dst_src();
                        let tgt = self.read::<u32>();
                        if self.registers.get(dst) < self.registers.get(src) {
                            self.registers.pc = tgt;
                        }
                    }
                    11 => {
                        // JumpLE { lhs: Register, rhs: Register, tgt: u32 }
                        let (dst, src) = self.read_dst_src();
                        let tgt = self.read::<u32>();
                        if self.registers.get(dst) <= self.registers.get(src) {
                            self.registers.pc = tgt;
                        }
                    }
                    12 => {
                        // JumpGT { lhs: Register, rhs: Register, tgt: u32 }
                        let (dst, src) = self.read_dst_src();
                        let tgt = self.read::<u32>();
                        if self.registers.get(dst) > self.registers.get(src) {
                            self.registers.pc = tgt;
                        }
                    }
                    13 => {
                        // JumpGE { lhs: Register, rhs: Register, tgt: u32 }
                        let (dst, src) = self.read_dst_src();
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
                        let cs = self.charsets.get_unchecked(idx as usize);

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
                        let str = self.strings.get_unchecked(idx as usize).as_bytes();

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
                        let str = self.strings.get_unchecked(idx as usize).as_bytes();

                        if Self::inlined_memicmp(line, off, str) {
                            self.registers.off = (off + str.len()) as u32;
                            self.registers.pc = tgt;
                        }
                    }

                    18 => {
                        // FlushHighlight
                        let start = self.registers.hs as usize;
                        let kind = self.registers.hk.try_into().unwrap_unchecked();

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

                    _ => std::hint::unreachable_unchecked(),
                }
            }
        }

        if res.last().is_none_or(|last| last.start < line.len()) {
            let start = line.len().min(self.registers.off as usize);
            res.push(Higlight { start, kind: unsafe { mem::zeroed() } });
        }
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
        unsafe { (self.assembly.as_ptr().add(pc) as *const T).read_unaligned() }
    }

    #[inline]
    fn read_dst_src(&mut self) -> (usize, usize) {
        let dst_src = self.read::<u8>();
        let dst = (dst_src & 0xf) as usize;
        let src = (dst_src >> 4) as usize;
        (dst, src)
    }
}
