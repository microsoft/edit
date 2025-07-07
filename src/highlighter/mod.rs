mod lang;
//mod lang_bash;
//mod lang_batch;
//mod lang_json;
//mod lang_powershell;
//mod lang_yaml;
//mod regex;

use std::fmt::Debug;

use crate::arena::{Arena, scratch_arena};
use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::highlighter::lang::LanguageDefinition;
use crate::{simd, unicode};

struct CharsetFormatter<'a>(&'a [bool; 256]);

impl Debug for CharsetFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let show_char = |f: &mut std::fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b.is_ascii_graphic() || b == b' ' {
                let b = b as char;
                write!(f, "'{b}'")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !self.0[beg] {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && self.0[end] {
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

// The runtime structure for highlighting, built from LanguageDef.
pub struct Language {
    #[allow(dead_code)]
    name: &'static str,
    extensions: &'static [&'static str],
    charsets: &'static [&'static [u16; 6]],
    states: &'static [&'static [Transition<'static>]],
}

// Conversion from LanguageDef to Language (stub).
impl Language {
    pub fn from_def(def: &LanguageDefinition) -> Self {
        // TODO: Build charsets and transitions from def.
        unimplemented!()
    }
}

struct Transition<'s> {
    test: Consume<'s>,
    kind: HighlightKind,
    state: Action,
}

enum Consume<'a> {
    Chars(usize),
    Prefix(&'a str),
    PrefixInsensitive(&'a str),
    Charset(&'a [bool; 256]),
    Line,
}

impl Debug for Consume<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Consume::Chars(n) => write!(f, "Chars({n})"),
            Consume::Prefix(s) => write!(f, "Prefix({s:?})"),
            Consume::PrefixInsensitive(s) => write!(f, "PrefixInsensitive({s:?})"),
            Consume::Charset(c) => write!(f, "Charset({:?})", CharsetFormatter(c)),
            Consume::Line => write!(f, "Line"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    #[default]
    Other,
    Comment,
    Number,
    String,
    Variable,
    Operator,
    Keyword,
    Method,
}

enum Action {
    Change(u8), // to
    Push(u8),   // to
    Pop(u8),    // count
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
    charsets: Vec<[bool; 256]>,
    starter: Vec<[bool; 256]>,

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
            charsets: language
                .charsets
                .iter()
                .map(|&charset| {
                    let mut word_chars = [false; 256];
                    Self::fill_word_chars(&mut word_chars, charset);
                    word_chars
                })
                .collect(),
            starter: language
                .states
                .iter()
                .map(|&transitions| {
                    let mut starter = [false; 256];
                    for t in transitions {
                        match t.test {
                            Consume::Chars(_) => starter.fill(true),
                            Consume::Prefix(prefix) => {
                                starter[prefix.as_bytes()[0] as usize] = true;
                            }
                            Consume::PrefixInsensitive(prefix) => {
                                let ch = prefix.as_bytes()[0];
                                starter[ch.to_ascii_lowercase() as usize] = true;
                                starter[ch.to_ascii_uppercase() as usize] = true;
                            }
                            Consume::Charset(i) => {
                                todo!()
                            }
                            Consume::Line => {}
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

    fn fill_word_chars(dst: &mut [bool; 256], src: &[u16; 6]) {
        for y in 0..src.len() {
            let mut r = src[y];
            while r != 0 {
                let bit = r.trailing_zeros() as usize;
                r &= !(1 << bit);
                dst[32 + y * 16 + bit] = true;
            }
        }
        dst[0x80..].fill(true);
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
        let mut off = 0;

        if self.kind != HighlightKind::Other {
            res.push(Higlight { start: 0, kind: self.kind });
        }

        loop {
            while off < line_buf.len() && !self.starter[self.state][line_buf[off] as usize] {
                off += 1;
            }

            let start = off;
            let mut hit = None;

            for t in self.language.states[self.state] {
                match t.test {
                    Consume::Chars(n) => {
                        off += n;
                        hit = Some(t);
                        break;
                    }
                    Consume::Prefix(str) => {
                        if line_buf[off..].starts_with(str.as_bytes()) {
                            off += str.len();
                            hit = Some(t);
                            break;
                        }
                    }
                    Consume::PrefixInsensitive(str) => {
                        if line_buf[off..].starts_with_ignore_ascii_case(str) {
                            off += str.len();
                            hit = Some(t);
                            break;
                        }
                    }
                    Consume::Charset(charset) => {
                        if off < line_buf.len() && charset[line_buf[off] as usize] {
                            while {
                                off += 1;
                                off < line_buf.len() && charset[line_buf[off] as usize]
                            } {}
                            hit = Some(t);
                            break;
                        }
                    }
                    Consume::Line => {
                        off = line_buf.len();
                        hit = Some(t);
                        break;
                    }
                };
            }

            if let Some(t) = hit {
                // If this transition changes the HighlightKind,
                // we need to split the current run and add a new one.
                if self.kind != t.kind {
                    if let Some(last) = res.last_mut()
                        && last.start == start
                    {
                        last.kind = t.kind;
                    } else {
                        res.push(Higlight { start, kind: t.kind });
                    }
                }

                match t.state {
                    Action::Change(to) => {
                        if let Some(last) = res.last_mut() {
                            last.kind = t.kind;
                        }
                        self.state = to as usize;
                        self.kind = t.kind;
                    }
                    Action::Push(to) => {
                        self.state_stack.push((self.state, self.kind));
                        self.state = to as usize;
                        self.kind = t.kind;
                    }
                    Action::Pop(count) => {
                        // Pop the last `count` states from the stack.
                        let to = self.state_stack.len().saturating_sub(count as usize);
                        (self.state, self.kind) =
                            self.state_stack.get(to).copied().unwrap_or_default();
                        self.state_stack.truncate(to);

                        // This may have changed the HighlightKind yet again.
                        if self.kind != t.kind {
                            if let Some(last) = res.last_mut()
                                && last.start == off
                            {
                                last.kind = self.kind;
                            } else {
                                res.push(Higlight { start: off, kind: self.kind });
                            }
                        }
                    }
                }
            } else {
                // False starter hit.
                off += 1;
            }

            if off >= line_buf.len() {
                break;
            }
        }

        if res.last().is_some_and(|last| last.start != line_buf.len()) {
            res.push(Higlight { start: line_buf.len(), kind: self.kind });
        }

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start += line_beg;
        }

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
