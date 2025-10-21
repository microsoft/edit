// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::Debug;
use std::path::Path;
use std::slice;

use stdext::arena::{Arena, scratch_arena};

use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::lsh::definitions::*;
use crate::{simd, unicode};

pub fn language_from_path(path: &Path) -> Option<&'static Language> {
    let filename = path.file_name()?.as_encoded_bytes();

    for &l in LANGUAGES {
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
    language: &'static Language,
    offset: usize,
    logical_pos_y: CoordType,
    state_stack: Vec<(u8, HighlightKind)>,
}

#[derive(Clone)]
pub struct HighlighterState {
    pub offset: usize,
    pub logical_pos_y: CoordType,
    pub state_stack: Vec<(u8, HighlightKind)>,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self { doc, language, offset: 0, logical_pos_y: 0, state_stack: Vec::new() }
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
            state_stack: self.state_stack.clone(),
        }
    }

    /// Restore the highlighter state from a previously captured snapshot.
    pub fn restore(&mut self, snapshot: &HighlighterState) {
        self.offset = snapshot.offset;
        self.logical_pos_y = snapshot.logical_pos_y;
        self.state_stack = snapshot.state_stack.clone();
    }

    pub fn parse_next_line<'a>(&mut self, arena: &'a Arena) -> Vec<Higlight, &'a Arena> {
        const MAX_LEN: usize = 32 * KIBI;

        let scratch = scratch_arena(Some(arena));
        let line_beg = self.offset;
        let mut res = Vec::new_in(arena);

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

        // If the line is empty, we reached the end of the document.
        //
        // If the line is too long, we don't highlight it.
        // This is to prevent performance issues with very long lines.
        if line.is_empty() || line.len() >= MAX_LEN {
            return res;
        }

        let line = unicode::strip_newline(line);
        let mut off = 0usize;
        let mut start = 0usize;

        let (state, mut kind) =
            self.state_stack.last().cloned().unwrap_or((0, HighlightKind::Other));
        let mut state = state as usize;

        let mut push = |start: usize, kind: HighlightKind| {
            if let Some(last) = res.last_mut() {
                if last.start == start {
                    last.kind = kind;
                }
                if last.kind == kind {
                    return;
                }
            }
            res.push(Higlight { start, kind });
        };

        state = state.wrapping_sub(1);

        loop {
            state = state.wrapping_add(1);
            let t = unsafe { self.language.transitions.get_unchecked(state) };

            match t.test {
                Test::Chars(n) => {
                    off = off + n.min(line.len() - off);
                }
                Test::Prefix(str) => {
                    let str = unsafe { slice::from_raw_parts(str.add(1), str.read() as usize) };
                    if !Self::inlined_memcmp(line, off, str) {
                        continue;
                    }
                    off += str.len();
                }
                Test::PrefixInsensitive(str) => {
                    let str = unsafe { slice::from_raw_parts(str.add(1), str.read() as usize) };
                    if !Self::inlined_memicmp(line, off, str) {
                        continue;
                    }
                    off += str.len();
                }
                Test::Charset(cs) => {
                    // TODO: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html#alternative-implementation
                    if off >= line.len() || !Self::in_set(cs, line[off]) {
                        continue;
                    }
                    while {
                        off += 1;
                        off < line.len() && Self::in_set(cs, line[off])
                    } {}
                }
            }

            if let Some(k) = t.kind {
                kind = k;
            }

            match t.action {
                Action::Jump(dst) => {
                    state = dst as usize;
                }
                Action::Push(dst) => {
                    state = dst as usize;
                    push(start, kind);

                    self.state_stack.push((dst, kind));

                    start = off;
                }
                Action::Pop(count) => {
                    push(start, kind);

                    if count != 0 {
                        self.state_stack
                            .truncate(self.state_stack.len().saturating_sub(count as usize));
                    }

                    let v = self.state_stack.last().cloned().unwrap_or((0, HighlightKind::Other));
                    state = v.0 as usize;
                    kind = v.1;

                    start = off;

                    if count == 0 && off >= line.len() {
                        break;
                    }
                }
                Action::Loop(dst) => {
                    push(start, kind);

                    state = dst as usize;
                    start = off;

                    if off >= line.len() {
                        break;
                    }
                }
            }

            state = state.wrapping_sub(1);
        }

        push(start, kind);
        res.push(Higlight { start: line.len(), kind: HighlightKind::Other });

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start = line_beg + h.start.min(line.len());
        }

        res
    }

    /// A mini-memcmp implementation for short needles.
    /// Compares the `haystack` at `off` with the `needle`.
    #[inline]
    fn inlined_memcmp(haystack: &[u8], off: usize, needle: &[u8]) -> bool {
        unsafe {
            let needle_len = needle.len();
            if haystack.len() - off < needle_len {
                return false;
            }

            let a = haystack.as_ptr().add(off);
            let b = needle.as_ptr();
            let mut i = 0;

            while i < needle_len {
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
            let needle_len = needle.len();
            if haystack.len() - off < needle_len {
                return false;
            }

            let a = haystack.as_ptr().add(off);
            let b = needle.as_ptr();
            let mut i = 0;

            while i < needle_len {
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
