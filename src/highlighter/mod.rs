mod definitions;

use std::ffi::OsStr;
use std::fmt::Debug;
use std::ops::RangeInclusive;
use std::path::Path;

pub use definitions::{HighlightKind, Language};

use crate::arena::{Arena, scratch_arena};
use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::highlighter::definitions::*;
use crate::{simd, unicode};

pub fn language_from_path(path: &Path) -> Option<&'static Language> {
    let ext = path.extension()?;
    LANGUAGES.iter().copied().find(|lang| lang.extensions.iter().any(|&e| OsStr::new(e) == ext))
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

pub struct Highlighter<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,

    language: &'static Language,
    starter: Vec<[u8; 256]>,

    state: usize,
    kind: HighlightKind,
    state_stack: Vec<(usize, HighlightKind)>,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self {
            doc,
            offset: 0,
            logical_pos_y: 0,

            language,
            starter: language
                .states
                .iter()
                .map(|&transitions| {
                    let mut starter = [0; 256];
                    for t in transitions {
                        match t.0 {
                            Consume::Chars(_) => starter.fill(1),
                            Consume::Prefix(prefix) => {
                                starter[prefix.as_bytes()[0] as usize] = 1;
                            }
                            Consume::PrefixInsensitive(prefix) => {
                                let ch = prefix.as_bytes()[0];
                                starter[ch.to_ascii_uppercase() as usize] = 1;
                                starter[ch.to_ascii_lowercase() as usize] = 1;
                            }
                            Consume::Charset(cs) => {
                                for (a, b) in starter.iter_mut().zip(cs.iter()) {
                                    *a |= *b;
                                }
                            }
                        }
                    }
                    starter
                })
                .collect(),

            state: 0,
            kind: Default::default(),
            state_stack: Default::default(),
        }
    }

    pub fn logical_pos_y(&self) -> CoordType {
        self.logical_pos_y
    }

    pub fn parse_next_line<'a>(&mut self, arena: &'a Arena) -> Vec<Higlight, &'a Arena> {
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
                let end = off.min(MEBI - line_buf.len());
                // If we're at it we can also help Rust understand that indexing with `end` doesn't panic.
                let end = end.min(chunk.len());

                line_buf.extend_from_slice(&chunk[..end]);

                // If the line is too long, we don't highlight it.
                // This is to prevent performance issues with very long lines.
                if line_buf.len() >= MEBI {
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
        let mut state = self.state;
        let mut kind = self.kind;

        loop {
            let mut end = off;

            for t in self.language.states[state] {
                match t.0 {
                    Consume::Chars(n) => {
                        end = end.saturating_add(n);
                    }
                    Consume::Prefix(str) => {
                        if !line_buf[end..].starts_with(str.as_bytes()) {
                            continue;
                        }
                        end += str.len();
                    }
                    Consume::PrefixInsensitive(str) => {
                        if !line_buf[end..].starts_with_ignore_ascii_case(str) {
                            continue;
                        }
                        end += str.len();
                    }
                    Consume::Charset(cs) => {
                        // TODO: http://0x80.pl/notesen/2018-10-18-simd-byte-lookup.html#alternative-implementation
                        if end >= line_buf.len() || cs[line_buf[end] as usize] == 0 {
                            continue;
                        }
                        while {
                            end += 1;
                            end < line_buf.len() && cs[line_buf[end] as usize] != 0
                        } {}
                    }
                }

                match t.2 {
                    Action::Change(to) => {
                        state = to as usize;
                        kind = t.1.unwrap_or(kind);
                    }
                    Action::Push(to) => {
                        res.push(Higlight { start, kind });

                        self.state_stack.push((state, kind));
                        start = off;
                        state = to as usize;
                        kind = t.1.unwrap_or(kind);
                    }
                    Action::Pop => {
                        kind = t.1.unwrap_or(kind);
                        res.push(Higlight { start, kind });

                        start = end;
                        (state, kind) = self.state_stack.last().copied().unwrap_or_default();
                        self.state_stack.pop();
                    }
                }

                off = end;
                break;
            }

            if off >= line_buf.len() {
                break;
            }
        }

        res.push(Higlight { start, kind });
        res.push(Higlight { start: line_buf.len(), kind });

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start = line_beg + h.start.min(line_buf.len());
        }

        self.state = state;
        self.kind = kind;
        res
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
