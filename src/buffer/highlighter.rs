#![allow(dead_code, unused_variables, unused_mut)]

use std::fmt::Debug;

use crate::arena::{Arena, scratch_arena};
use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::{simd, unicode};

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

#[derive(Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Other,
    Whitespace,
    Alpha,
    Numeric,
}

enum Consume {
    Chars(usize),
    Prefix(&'static str),
    Digits,
    Word,
    Line,
}

struct Language {
    word_chars: &'static [u16; 6],
    states: &'static [&'static [Transition]],
}

enum StateStack {
    Change(u8), // to
    Push(u8),   // to
    Pop(u8),    // count
}

struct Transition {
    test: Consume,
    kind: HighlightKind,
    state: StateStack,
}

const POWERSHELL: Language = {
    type T = Transition;
    use Consume::*;
    use HighlightKind::*;
    use StateStack::*;

    const GROUND: u8 = 0;

    const LINE_COMMENT: u8 = 1;
    const BLOCK_COMMENT: u8 = 2;

    const STRING_SINGLE: u8 = 3;
    const STRING_DOUBLE: u8 = 4;
    const STRING_ESCAPE: u8 = 5;

    const VARIABLE: u8 = 6;
    const VARIABLE_BRACE: u8 = 7;
    const VARIABLE_PAREN: u8 = 8;

    const PARAMETER: u8 = 9;
    const KEYWORD: u8 = 10;
    const METHOD: u8 = 11;

    Language {
        word_chars: &[
            // /.-,+*)('&%$#"!
            0b_1110110000101010,
            // ?>=<;:9876543210
            0b_1111011111111111,
            // ONMLKJIHGFEDCBA@
            0b_1111111111111110,
            // _^]\[ZYXWVUTSRQP
            0b_1111111111111111,
            // onmlkjihgfedcba`
            0b_1111111111111111,
            //  ~}|{zyxwvutsrqp
            0b_0100011111111111,
        ],
        states: &[
            // GROUND
            &[
                // Comments
                T { test: Prefix("#"), kind: Comment, state: Push(LINE_COMMENT) },
                T { test: Prefix("<#"), kind: Comment, state: Push(BLOCK_COMMENT) },
                // Numbers
                T { test: Digits, kind: Number, state: Pop(1) },
                // Strings
                T { test: Prefix("'"), kind: String, state: Push(STRING_SINGLE) },
                T { test: Prefix("\""), kind: String, state: Push(STRING_DOUBLE) },
                // Variables
                T { test: Prefix("$("), kind: Other, state: Push(VARIABLE_PAREN) },
                T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
                // Operators
                T { test: Prefix("-"), kind: Operator, state: Push(PARAMETER) },
                T { test: Prefix("!"), kind: Operator, state: Pop(1) },
                T { test: Prefix("*"), kind: Operator, state: Pop(1) },
                T { test: Prefix("/"), kind: Operator, state: Pop(1) },
                T { test: Prefix("%"), kind: Operator, state: Pop(1) },
                T { test: Prefix("+"), kind: Operator, state: Pop(1) },
                T { test: Prefix("<"), kind: Operator, state: Pop(1) },
                T { test: Prefix("="), kind: Operator, state: Pop(1) },
                T { test: Prefix(">"), kind: Operator, state: Pop(1) },
                T { test: Prefix("|"), kind: Operator, state: Pop(1) },
                // Keywords
                T { test: Prefix("break"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("catch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("continue"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("do"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("else"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("finally"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("foreach"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("function"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("if"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("return"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("switch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("throw"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("try"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("using"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("while"), kind: Keyword, state: Push(KEYWORD) },
                // Methods
                T { test: Word, kind: Method, state: Push(METHOD) },
            ],
            // LINE_COMMENT: # comment
            &[T { test: Line, kind: Comment, state: Pop(1) }],
            // BLOCK_COMMENT: <# comment #>
            &[T { test: Prefix("#>"), kind: Comment, state: Pop(1) }],
            // STRING_SINGLE: 'string'
            &[T { test: Prefix("'"), kind: String, state: Pop(1) }],
            // STRING_DOUBLE: "string"
            &[
                T { test: Prefix("`"), kind: String, state: Push(STRING_ESCAPE) },
                T { test: Prefix("$("), kind: Other, state: Push(VARIABLE_PAREN) },
                T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
                T { test: Prefix("\""), kind: String, state: Pop(1) },
            ],
            // STRING_ESCAPE: "`a"
            &[T { test: Chars(1), kind: String, state: Pop(1) }],
            // VARIABLE: $variable
            &[
                T { test: Prefix("{"), kind: Variable, state: Change(VARIABLE_BRACE) },
                T { test: Word, kind: Variable, state: Pop(1) },
            ],
            // VARIABLE_BRACE: ${variable}
            &[T { test: Prefix("}"), kind: Variable, state: Pop(1) }],
            // VARIABLE_PAREN: $(command)
            // This is largely a copy of the ground state.
            &[
                // Ground state Overrides
                T { test: Prefix("("), kind: Other, state: Push(VARIABLE_PAREN) },
                T { test: Prefix(")"), kind: Other, state: Pop(1) },
                // Numbers
                T { test: Digits, kind: Number, state: Pop(1) },
                // Strings
                T { test: Prefix("'"), kind: String, state: Push(STRING_SINGLE) },
                T { test: Prefix("\""), kind: String, state: Push(STRING_DOUBLE) },
                // Variables
                T { test: Prefix("$"), kind: Variable, state: Push(VARIABLE) },
                // Operators
                T { test: Prefix("-"), kind: Operator, state: Push(PARAMETER) },
                T { test: Prefix("!"), kind: Operator, state: Pop(1) },
                T { test: Prefix("*"), kind: Operator, state: Pop(1) },
                T { test: Prefix("/"), kind: Operator, state: Pop(1) },
                T { test: Prefix("%"), kind: Operator, state: Pop(1) },
                T { test: Prefix("+"), kind: Operator, state: Pop(1) },
                T { test: Prefix("<"), kind: Operator, state: Pop(1) },
                T { test: Prefix("="), kind: Operator, state: Pop(1) },
                T { test: Prefix(">"), kind: Operator, state: Pop(1) },
                T { test: Prefix("|"), kind: Operator, state: Pop(1) },
                // Keywords
                T { test: Prefix("break"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("catch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("continue"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("do"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("else"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("finally"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("foreach"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("function"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("if"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("return"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("switch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("throw"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("try"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("using"), kind: Keyword, state: Push(KEYWORD) },
                T { test: Prefix("while"), kind: Keyword, state: Push(KEYWORD) },
                // Methods
                T { test: Word, kind: Method, state: Push(METHOD) },
            ],
            // PARAMETER: -parameter
            &[
                T { test: Word, kind: Operator, state: Pop(1) },
                T { test: Chars(0), kind: Operator, state: Pop(1) },
            ],
            // KEYWORD: foreach, if, etc.
            &[
                T { test: Word, kind: Method, state: Change(METHOD) },
                T { test: Chars(0), kind: Keyword, state: Pop(1) },
            ],
            // METHOD: Foo-Bar
            &[
                T { test: Word, kind: Method, state: Change(METHOD) },
                T { test: Prefix("-"), kind: Method, state: Change(METHOD) },
                T { test: Chars(0), kind: Method, state: Pop(1) },
            ],
        ],
    }
};

pub struct Highlighter<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,

    language: &'static Language,
    word_chars: [bool; 256],
    starter: Vec<[bool; 256]>,

    state: usize,
    kind: HighlightKind,
    state_stack: Vec<(usize, HighlightKind)>,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument) -> Self {
        let language = &POWERSHELL;

        let mut word_chars = [false; 256];
        Self::fill_word_chars(&mut word_chars, language.word_chars);

        let starter = Vec::from_iter(language.states.iter().map(|&transitions| {
            let mut starter = [false; 256];
            for t in transitions {
                match t.test {
                    Consume::Chars(n) => starter.fill(true),
                    Consume::Prefix(prefix) => starter[prefix.as_bytes()[0] as usize] = true,
                    Consume::Digits => starter[b'0' as usize..=b'9' as usize].fill(true),
                    Consume::Word => Self::fill_word_chars(&mut starter, language.word_chars),
                    Consume::Line => {}
                }
            }
            starter
        }));

        Self {
            doc,
            offset: 0,
            logical_pos_y: 0,

            language,
            word_chars,
            starter,

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
                    Consume::Digits => {
                        if off < line_buf.len() && line_buf[off].is_ascii_digit() {
                            while {
                                off += 1;
                                off < line_buf.len() && line_buf[off].is_ascii_digit()
                            } {}
                            hit = Some(t);
                            break;
                        }
                    }
                    Consume::Word => {
                        if off < line_buf.len() && self.word_chars[line_buf[off] as usize] {
                            while {
                                off += 1;
                                off < line_buf.len() && self.word_chars[line_buf[off] as usize]
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
                    StateStack::Change(to) => {
                        self.state = to as usize;
                        self.kind = t.kind;
                    }
                    StateStack::Push(to) => {
                        self.state_stack.push((self.state, self.kind));
                        self.state = to as usize;
                        self.kind = t.kind;
                    }
                    StateStack::Pop(count) => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_powershell() {
        let doc = r#"$response = Read-Host "Delete branch '$branch'? [y/N]""#;
        let bytes = doc.as_bytes();
        let scratch = scratch_arena(None);
        let mut parser = Highlighter::new(&bytes);

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
        let mut parser = Highlighter::new(&bytes);

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
        let mut parser = Highlighter::new(&bytes);

        let tokens = parser.parse_next_line(&scratch);
        assert_eq!(
            tokens,
            &[
                Higlight { start: 0, kind: HighlightKind::Comment },
                Higlight { start: 5, kind: HighlightKind::Other },
            ]
        );
    }
}
