use std::fmt::Debug;
use std::path::Path;

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
    call_stack: Vec<Registers>,
    registers: Registers,
}

#[derive(Clone)]
pub struct HighlighterState {
    pub offset: usize,
    pub logical_pos_y: CoordType,
    pub state_stack: Vec<Registers>,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self {
            doc,
            language,
            offset: 0,
            logical_pos_y: 0,
            call_stack: Default::default(),
            registers: {
                let mut r = Registers::default();
                r[Register::HighlightKind as usize] = HighlightKind::Other.as_usize() as u32;
                r
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
            state_stack: self.call_stack.clone(),
        }
    }

    /// Restore the highlighter state from a previously captured snapshot.
    pub fn restore(&mut self, snapshot: &HighlighterState) {
        self.offset = snapshot.offset;
        self.logical_pos_y = snapshot.logical_pos_y;
        self.call_stack = snapshot.state_stack.clone();
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

        self.set_reg(Register::InputOffset, 0);
        self.set_reg(Register::HighlightStart, 0);

        loop {
            unsafe {
                let pc = self.get_reg(Register::ProgramCounter);
                let op = *self.language.instructions.get_unchecked(pc as usize);

                self.set_reg(Register::ProgramCounter, pc + 1);

                match op & 0xf {
                    0 => {
                        // Add
                        let dst = ((op >> 4) & 0xf) as usize;
                        let src = ((op >> 8) & 0xf) as usize;
                        let imm = op >> 12;
                        let src = *self.registers.get_unchecked(src);
                        let dst = self.registers.get_unchecked_mut(dst);
                        *dst = src + imm;
                    }
                    1 => {
                        // Call
                        let dst = op >> 12;
                        self.call_stack.push(self.registers);
                        self.set_reg(Register::ProgramCounter, dst);
                        self.set_reg(Register::ProcedureStart, dst);
                    }
                    2 => {
                        // Return
                        if let Some(last) = self.call_stack.last() {
                            self.registers = *last;
                            self.call_stack.pop();
                        } else {
                            break;
                        }
                    }
                    3 => {
                        // JumpIfNotMatchCharset
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let mut off = self.get_reg(Register::InputOffset) as usize;
                        let cs = self.language.charsets.get_unchecked(idx);

                        // TODO: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html#alternative-implementation
                        if off >= line.len() || !Self::in_set(cs, line[off]) {
                            self.set_reg(Register::ProgramCounter, dst);
                        } else {
                            while {
                                off += 1;
                                off < line.len() && Self::in_set(cs, line[off])
                            } {}
                            self.set_reg(Register::InputOffset, off as u32);
                        }
                    }
                    4 => {
                        // JumpIfNotMatchPrefix
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let off = self.get_reg(Register::InputOffset) as usize;
                        let str = self.language.strings.get_unchecked(idx).as_bytes();

                        if !Self::inlined_memcmp(line, off, str) {
                            self.set_reg(Register::ProgramCounter, dst);
                        } else {
                            self.set_reg(Register::InputOffset, (off + str.len()) as u32);
                        }
                    }
                    5 => {
                        // JumpIfNotMatchPrefixInsensitive
                        let idx = ((op >> 4) & 0xff) as usize;
                        let dst = op >> 12;
                        let off = self.get_reg(Register::InputOffset) as usize;
                        let str = self.language.strings.get_unchecked(idx).as_bytes();

                        if !Self::inlined_memicmp(line, off, str) {
                            self.set_reg(Register::ProgramCounter, dst);
                        } else {
                            self.set_reg(Register::InputOffset, (off + str.len()) as u32);
                        }
                    }
                    6 => {
                        // FlushHighlight
                        let start = self.get_reg(Register::HighlightStart) as usize;
                        let kind = self.get_reg(Register::HighlightKind) as usize;
                        let kind = HighlightKind::from_usize(kind);

                        if let Some(last) = res.last_mut()
                            && (last.start == start || last.kind == kind)
                        {
                            last.kind = kind;
                        } else {
                            res.push(Higlight { start, kind });
                        }

                        self.set_reg(Register::HighlightStart, self.get_reg(Register::InputOffset));
                    }
                    7 => {
                        // SuspendOpportunity
                        let off = self.get_reg(Register::InputOffset) as usize;
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

    /// A mini-memcmp implementation for short needles.
    /// Compares the `haystack` at `off` with the `needle`.
    #[inline]
    fn inlined_memcmp(haystack: &[u8], off: usize, needle: &[u8]) -> bool {
        unsafe {
            if haystack.len() - off < needle.len() {
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
            if haystack.len() - off < needle.len() {
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

    #[inline(always)]
    fn get_reg(&self, reg: Register) -> u32 {
        self.registers[reg as usize]
    }

    #[inline(always)]
    fn set_reg(&mut self, reg: Register, val: u32) {
        self.registers[reg as usize] = val;
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
