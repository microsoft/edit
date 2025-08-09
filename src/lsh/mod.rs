//! Welcome to Leonard's Shitty Highlighter.

mod definitions;

use std::ffi::OsStr;
use std::fmt::Debug;
use std::ops::RangeInclusive;
use std::path::Path;

pub use definitions::{HighlightKind, Language};

use crate::arena::{Arena, scratch_arena};
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

    state: usize,
    kind: HighlightKind,
    state_stack: Vec<(usize, HighlightKind)>,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self {
            doc,
            language,
            offset: 0,
            logical_pos_y: 0,

            state: 0,
            kind: Default::default(),
            state_stack: Default::default(),
        }
    }

    pub fn logical_pos_y(&self) -> CoordType {
        self.logical_pos_y
    }

    pub fn parse_next_line<'a>(&mut self, arena: &'a Arena) -> Vec<Higlight, &'a Arena> {
        const MAX_LEN: usize = 32 * KIBI;

        let scratch = scratch_arena(Some(arena));
        let line_beg = self.offset;
        let mut line_buf = Vec::new_in(&*scratch);
        let mut res = Vec::new_in(arena);

        if self.offset != 0 {
            self.logical_pos_y += 1;
        }

        // Accumulate a line of text into `line_buf`.
        {
            let mut chunk = self.doc.read_forward(self.offset);

            // Check if the last line was the last line in the document.
            if chunk.is_empty() {
                return res;
            }

            loop {
                let (off, line) = simd::lines_fwd(chunk, 0, 0, 1);
                self.offset += off;

                // Overly long lines are not highlighted, so we limit the line length to 32 KiB.
                // I'm worried it may run into weird edge cases.
                let end = off.min(MAX_LEN - line_buf.len());
                // If we're at it we can also help Rust understand that indexing with `end` doesn't panic.
                let end = end.min(chunk.len());

                line_buf.extend_from_slice(&chunk[..end]);

                // If the line is too long, we don't highlight it.
                // This is to prevent performance issues with very long lines.
                if line_buf.len() >= MAX_LEN {
                    return res;
                }

                // Start of the next line found.
                if line == 1 {
                    break;
                }

                chunk = self.doc.read_forward(self.offset);
                if chunk.is_empty() {
                    // End of document reached
                    break;
                }
            }
        }

        let line_buf = unicode::strip_newline(&line_buf);
        let mut off = 0usize;
        let mut start = 0usize;

        let mut root = (self.state, self.kind);
        let mut state = self.state;
        let mut kind = self.kind;

        'outer: loop {
            let beg = off;

            for t in unsafe { *self.language.states.get_unchecked(state) } {
                match t.test {
                    Test::Chars(n) => {
                        off = off + n.min(line_buf.len() - off);
                    }
                    Test::Prefix(str) => {
                        let str =
                            unsafe { std::slice::from_raw_parts(str.add(1), str.read() as usize) };
                        if !Self::inlined_memcmp(line_buf, off, str) {
                            continue;
                        }
                        off += str.len();
                    }
                    Test::PrefixInsensitive(str) => {
                        let str =
                            unsafe { std::slice::from_raw_parts(str.add(1), str.read() as usize) };
                        if !Self::inlined_memicmp(line_buf, off, str) {
                            continue;
                        }
                        off += str.len();
                    }
                    Test::Charset(cs) => {
                        // TODO: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html#alternative-implementation
                        if off >= line_buf.len() || !Self::in_set(cs, line_buf[off]) {
                            continue;
                        }
                        while {
                            off += 1;
                            off < line_buf.len() && Self::in_set(cs, line_buf[off])
                        } {}
                    }
                }

                match t.action {
                    Action::Change(to) => {
                        state = to as usize;
                        kind = t.kind.unwrap_or(kind);
                    }
                    Action::Push(to) => {
                        self.state_stack.push(root);

                        state = to as usize;
                        kind = t.kind.unwrap_or(kind);
                        res.push(Higlight { start, kind });

                        root = (state, kind);
                        start = off;
                    }
                    Action::Pop(n) => {
                        if n != 0 {
                            root = self.state_stack.last().copied().unwrap_or_default();
                            self.state_stack
                                .truncate(self.state_stack.len().saturating_sub(n as usize));
                        }

                        kind = t.kind.unwrap_or(kind);
                        res.push(Higlight { start, kind });

                        (state, kind) = root;
                        start = off;

                        if off >= line_buf.len() {
                            break 'outer;
                        }
                    }
                }

                break;
            }
        }

        if res.last().is_none_or(|h| h.start != start) {
            res.push(Higlight { start, kind });
        }
        if res.last().is_some_and(|h| h.start != line_buf.len()) {
            res.push(Higlight { start: line_buf.len(), kind });
        }

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start = line_beg + h.start.min(line_buf.len());
        }

        self.state = state;
        self.kind = kind;
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

            let mut a = haystack.as_ptr().add(off);
            let mut b = needle.as_ptr();
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

            let mut a = haystack.as_ptr().add(off);
            let mut b = needle.as_ptr();
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

/*#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_powershell() {
        let doc = r#"$response = Read-Host "Delete branch '$branch'? [y/N]""#;
        let bytes = doc.as_bytes();
        let scratch = scratch_arena(None);
        let mut parser = Highlighter::new(&bytes, &lang_powershell::LANG);

        let tokens = parser.parse_next_line(&scratch);
        assert_eq!(
            tokens,
            &[
                Higlight { start: 0, kind: HighlightKind::Variable },
                Higlight { start: 9, kind: HighlightKind::Other },
                Higlight { start: 10, kind: HighlightKind::Operator },
                Higlight { start: 11, kind: HighlightKind::Other },
                Higlight { start: 12, kind: HighlightKind::Method },
                Higlight { start: 21, kind: HighlightKind::Other },
                Higlight { start: 22, kind: HighlightKind::String },
                Higlight { start: 38, kind: HighlightKind::Variable },
                Higlight { start: 45, kind: HighlightKind::String },
                Higlight { start: 54, kind: HighlightKind::Other },
            ]
        );
    }

    #[test]
    fn test_string() {
        let doc = r#""$x";"#;
        let bytes = doc.as_bytes();
        let scratch = scratch_arena(None);
        let mut parser = Highlighter::new(&bytes, &lang_powershell::LANG);

        let tokens = parser.parse_next_line(&scratch);
        assert_eq!(
            tokens,
            &[
                Higlight { start: 0, kind: HighlightKind::String },
                Higlight { start: 1, kind: HighlightKind::Variable },
                Higlight { start: 3, kind: HighlightKind::String },
                Higlight { start: 4, kind: HighlightKind::Other },
            ]
        );
    }

    #[test]
    fn test_comment() {
        let doc = r#"<#x#>"#;
        let bytes = doc.as_bytes();
        let scratch = scratch_arena(None);
        let mut parser = Highlighter::new(&bytes, &lang_powershell::LANG);

        let tokens = parser.parse_next_line(&scratch);
        assert_eq!(
            tokens,
            &[
                Higlight { start: 0, kind: HighlightKind::Comment },
                Higlight { start: 5, kind: HighlightKind::Other },
            ]
        );
    }
}*/
