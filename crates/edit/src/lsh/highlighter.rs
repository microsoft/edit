// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::Debug;
use std::mem;
use std::path::Path;

use stdext::arena::{Arena, scratch_arena};

use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::lsh::definitions::*;
use crate::{simd, unicode};

pub struct Language {
    pub name: &'static str,
    pub filenames: &'static [&'static str],
    entrypoint: u32,
}

impl PartialEq for &Language {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

pub const LANGUAGES: &[Language] = &[
    Language { name: "Diff", filenames: &["*.diff", "*.patch"], entrypoint: ENTRYPOINT_DIFF },
    Language {
        name: "Git Commit Message",
        filenames: &["COMMIT_EDITMSG", "MERGE_MSG"],
        entrypoint: ENTRYPOINT_GIT_COMMIT_MESSAGE,
    },
    Language {
        name: "Git Rebase Message",
        filenames: &["git-rebase-todo"], // TODO: https://github.com/microsoft/vscode/issues/156954
        entrypoint: ENTRYPOINT_GIT_REBASE_TODO,
    },
];

pub fn language_from_path(path: &Path) -> Option<&'static Language> {
    let filename = path.file_name()?.as_encoded_bytes();

    for l in LANGUAGES {
        for f in l.filenames {
            let f = f.as_bytes();
            if let Some(suffix) = f.strip_prefix(b"*") {
                if filename.ends_with(suffix) {
                    return Some(l);
                }
            } else if filename == f {
                return Some(l);
            }
        }
    }

    None
}

#[derive(Clone, PartialEq, Eq)]
pub struct Higlight {
    pub start: usize,
    pub kind: HighlightKind,
}

impl Debug for Higlight {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {:?})", self.start, self.kind)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct State {}

#[derive(Clone)]
pub struct Highlighter<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,
    stack: Vec<Registers>,
    registers: Registers,
}

#[derive(Clone)]
pub struct HighlighterState {
    offset: usize,
    logical_pos_y: CoordType,
    stack: Vec<Registers>,
    registers: Registers,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self {
            doc,
            offset: 0,
            logical_pos_y: 0,
            stack: Default::default(),
            registers: Registers {
                pc: language.entrypoint,
                hk: HighlightKind::Other.as_usize() as u32,
                ..Default::default()
            },
        }
    }

    pub fn logical_pos_y(&self) -> CoordType {
        self.logical_pos_y
    }

    /// Create a restorable snapshot of the current highlighter state
    /// so we can resume highlighting from this point later.
    pub fn snapshot(&self) -> HighlighterState {
        HighlighterState {
            offset: self.offset,
            logical_pos_y: self.logical_pos_y,
            stack: self.stack.clone(),
            registers: self.registers,
        }
    }

    /// Restore the highlighter state from a previously captured snapshot.
    pub fn restore(&mut self, snapshot: &HighlighterState) {
        self.offset = snapshot.offset;
        self.logical_pos_y = snapshot.logical_pos_y;
        self.stack = snapshot.stack.clone();
        self.registers = snapshot.registers;
    }

    pub fn parse_next_line<'a>(&mut self, arena: &'a Arena) -> Vec<Higlight, &'a Arena> {
        const MAX_LEN: usize = 32 * KIBI;

        let scratch = scratch_arena(Some(arena));
        let line_beg = self.offset;

        self.logical_pos_y += 1;

        // Accumulate a line of text into `line_buf`.
        let line = 'read: {
            let mut chunk;
            let mut line_buf;

            // Try to read a chunk and see if it contains a newline.
            // In that case we can skip concatenating chunks.
            {
                chunk = self.doc.read_forward(self.offset);
                if chunk.is_empty() {
                    break 'read chunk;
                }

                let (off, line) = simd::lines_fwd(chunk, 0, 0, 1);
                self.offset += off;

                if line == 1 {
                    break 'read &chunk[..off];
                }

                let next_chunk = self.doc.read_forward(self.offset);
                if next_chunk.is_empty() {
                    break 'read &chunk[..off];
                }

                line_buf = Vec::new_in(&*scratch);

                // Ensure we don't overflow the heap size with a 1GB long line.
                let end = off.min(MAX_LEN - line_buf.len());
                let end = end.min(chunk.len());
                line_buf.extend_from_slice(&chunk[..end]);

                chunk = next_chunk;
            }

            // Concatenate chunks until we get a full line.
            while line_buf.len() < MAX_LEN {
                let (off, line) = simd::lines_fwd(chunk, 0, 0, 1);
                self.offset += off;

                // Ensure we don't overflow the heap size with a 1GB long line.
                let end = off.min(MAX_LEN - line_buf.len());
                let end = end.min(chunk.len());
                line_buf.extend_from_slice(&chunk[..end]);

                // Start of the next line found.
                if line == 1 {
                    break;
                }

                chunk = self.doc.read_forward(self.offset);
                if chunk.is_empty() {
                    break;
                }
            }

            line_buf.leak()
        };

        let mut res = Vec::new_in(arena);

        // Empty lines can be somewhat common.
        //
        // If the line is too long, we don't highlight it.
        // This is to prevent performance issues with very long lines.
        if line.is_empty() || line.len() >= MAX_LEN {
            return res;
        }

        self.registers.off = 0;
        self.registers.hs = 0;

        let line = unicode::strip_newline(line);

        loop {
            unsafe {
                let pc = self.registers.pc;
                let op = *ASSEMBLY.get_unchecked(pc as usize);

                self.registers.pc += 1;

                match op & 0xf {
                    0 => {
                        // Add
                        let dst = ((op >> 4) & 0xf) as usize;
                        let src = ((op >> 8) & 0xf) as usize;
                        let imm = op >> 12;
                        self.registers.set(dst, self.registers.get(src) + imm);
                    }
                    1 => {
                        // Call
                        let dst = op >> 12;
                        self.stack.push(self.registers);
                        self.registers.pc = dst;
                    }
                    2 => {
                        // Return
                        if let Some(last) = self.stack.last() {
                            self.registers = *last;
                            self.stack.pop();
                        } else {
                            self.registers = mem::zeroed();
                            self.registers.hk = HighlightKind::Other.as_usize() as u32;
                            break;
                        }
                    }
                    3 => {
                        // JumpIfMatchCharset
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let off = self.registers.off as usize;
                        let cs = CHARSETS.get_unchecked(idx);

                        if let Some(off) = Self::charset_gobble(line, off, cs) {
                            self.registers.off = off as u32;
                            self.registers.pc = dst;
                        }
                    }
                    4 => {
                        // JumpIfMatchPrefix
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let off = self.registers.off as usize;
                        let str = STRINGS.get_unchecked(idx).as_bytes();

                        if Self::inlined_memcmp(line, off, str) {
                            self.registers.off = (off + str.len()) as u32;
                            self.registers.pc = dst;
                        }
                    }
                    5 => {
                        // JumpIfMatchPrefixInsensitive
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let off = self.registers.off as usize;
                        let str = STRINGS.get_unchecked(idx).as_bytes();

                        if Self::inlined_memicmp(line, off, str) {
                            self.registers.off = (off + str.len()) as u32;
                            self.registers.pc = dst;
                        }
                    }
                    6 => {
                        // FlushHighlight
                        let start = self.registers.hs as usize;
                        let kind = self.registers.hk as usize;
                        let kind = HighlightKind::from_usize(kind);

                        if let Some(last) = res.last_mut()
                            && (last.start == start || last.kind == kind)
                        {
                            last.kind = kind;
                        } else {
                            res.push(Higlight { start, kind });
                        }

                        self.registers.hs = self.registers.off;
                    }
                    7 => {
                        // Loop
                        let dst = op >> 12;
                        let off = self.registers.off as usize;

                        self.registers.pc = dst;

                        if off >= line.len() {
                            break;
                        }
                    }
                    _ => std::hint::unreachable_unchecked(),
                }
            }
        }

        res.push(Higlight { start: line.len(), kind: HighlightKind::Other });

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start = line_beg + h.start.min(line.len());
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
