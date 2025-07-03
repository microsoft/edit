#![allow(dead_code, unused_variables, unused_mut)]

use std::fmt::Debug;
use std::ops::Range;

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
    pub range: Range<usize>,
    pub kind: HighlightKind,
}

impl Debug for Higlight {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}..{}, {:?})", self.range.start, self.range.end, self.kind)
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

enum Test {
    Consume(usize),
    ConsumePrefix(&'static str),
    ConsumeDigits,
    ConsumeWord,
    ConsumeLine,
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
    test: Test,
    kind: HighlightKind,
    state: StateStack,
}

const POWERSHELL: Language = {
    type T = Transition;
    use HighlightKind::*;
    use StateStack::*;
    use Test::*;

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
                T { test: ConsumePrefix("#"), kind: Comment, state: Push(LINE_COMMENT) },
                T { test: ConsumePrefix("<#"), kind: Comment, state: Push(BLOCK_COMMENT) },
                // Numbers
                T { test: ConsumeDigits, kind: Number, state: Pop(1) },
                // Strings
                T { test: ConsumePrefix("'"), kind: String, state: Push(STRING_SINGLE) },
                T { test: ConsumePrefix("\""), kind: String, state: Push(STRING_DOUBLE) },
                // Variables
                T { test: ConsumePrefix("$"), kind: Variable, state: Push(VARIABLE) },
                // Operators
                T { test: ConsumePrefix("-"), kind: Operator, state: Push(PARAMETER) },
                T { test: ConsumePrefix("*"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("/"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("%"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("+"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("<"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("="), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix(">"), kind: Operator, state: Pop(1) },
                T { test: ConsumePrefix("|"), kind: Operator, state: Pop(1) },
                // Keywords
                T { test: ConsumePrefix("break"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("catch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("continue"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("do"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("else"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("finally"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("foreach"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("function"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("if"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("return"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("switch"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("throw"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("try"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("using"), kind: Keyword, state: Push(KEYWORD) },
                T { test: ConsumePrefix("while"), kind: Keyword, state: Push(KEYWORD) },
                // Methods
                T { test: ConsumeWord, kind: Method, state: Push(METHOD) },
            ],
            // LINE_COMMENT: # comment
            &[T { test: ConsumeLine, kind: Comment, state: Pop(1) }],
            // BLOCK_COMMENT: <# comment #>
            &[T { test: ConsumePrefix("#>"), kind: Comment, state: Pop(1) }],
            // STRING_SINGLE: 'string'
            &[T { test: ConsumePrefix("'"), kind: String, state: Pop(1) }],
            // STRING_DOUBLE: "string"
            &[
                T { test: ConsumePrefix("`"), kind: String, state: Push(STRING_ESCAPE) },
                T { test: ConsumePrefix("$"), kind: Variable, state: Push(VARIABLE) },
                T { test: ConsumePrefix("\""), kind: String, state: Pop(1) },
            ],
            // STRING_ESCAPE: "`a"
            &[T { test: Consume(1), kind: String, state: Pop(1) }],
            // VARIABLE: $variable
            &[
                T { test: ConsumePrefix("{"), kind: Variable, state: Change(VARIABLE_BRACE) },
                T { test: ConsumePrefix("("), kind: Variable, state: Change(VARIABLE_PAREN) },
                T { test: ConsumeWord, kind: Variable, state: Pop(1) },
            ],
            // VARIABLE_BRACE: ${variable}
            &[T { test: ConsumePrefix("}"), kind: Variable, state: Pop(1) }],
            // VARIABLE_PAREN: $(command)
            &[T { test: ConsumePrefix(")"), kind: Variable, state: Pop(1) }],
            // PARAMETER: -parameter
            &[
                T { test: ConsumeWord, kind: Operator, state: Pop(1) },
                T { test: Consume(0), kind: Operator, state: Pop(1) },
            ],
            // KEYWORD: foreach, if, etc.
            &[
                T { test: ConsumeWord, kind: Method, state: Change(METHOD) },
                T { test: Consume(0), kind: Keyword, state: Pop(1) },
            ],
            // METHOD: Foo-Bar
            &[
                T { test: ConsumeWord, kind: Method, state: Change(METHOD) },
                T { test: ConsumePrefix("-"), kind: Method, state: Change(METHOD) },
                T { test: Consume(0), kind: Method, state: Pop(1) },
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
                    Test::Consume(n) => starter.fill(true),
                    Test::ConsumePrefix(prefix) => starter[prefix.as_bytes()[0] as usize] = true,
                    Test::ConsumeDigits => starter[b'0' as usize..=b'9' as usize].fill(true),
                    Test::ConsumeWord => Self::fill_word_chars(&mut starter, language.word_chars),
                    Test::ConsumeLine => {}
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
            res.push(Higlight { range: 0..line_buf.len(), kind: self.kind });
        }

        loop {
            while off < line_buf.len() && !self.starter[self.state][line_buf[off] as usize] {
                off += 1;
            }

            let start = off;
            let mut hit = None;

            for t in self.language.states[self.state] {
                match t.test {
                    Test::Consume(n) => {
                        off += n;
                        hit = Some(t);
                        break;
                    }
                    Test::ConsumePrefix(str) => {
                        if line_buf[off..].starts_with(str.as_bytes()) {
                            off += str.len();
                            hit = Some(t);
                            break;
                        }
                    }
                    Test::ConsumeDigits => {
                        if off < line_buf.len() && line_buf[off].is_ascii_digit() {
                            while {
                                off += 1;
                                off < line_buf.len() && line_buf[off].is_ascii_digit()
                            } {}
                            hit = Some(t);
                            break;
                        }
                    }
                    Test::ConsumeWord => {
                        if off < line_buf.len() && self.word_chars[line_buf[off] as usize] {
                            while {
                                off += 1;
                                off < line_buf.len() && self.word_chars[line_buf[off] as usize]
                            } {}
                            hit = Some(t);
                            break;
                        }
                    }
                    Test::ConsumeLine => {
                        off = line_buf.len();
                        hit = Some(t);
                        break;
                    }
                };
            }

            if let Some(t) = hit {
                if self.kind != t.kind {
                    self.kind = t.kind;
                    if let Some(last) = res.last_mut()
                        && last.range.end > start
                    {
                        last.range.end = start;
                    }
                    res.push(Higlight { range: start..line_buf.len(), kind: t.kind });
                }

                match t.state {
                    StateStack::Change(to) => {
                        if let Some(last) = self.state_stack.last_mut() {
                            *last = (self.state, t.kind);
                        }
                        if let Some(last) = res.last_mut() {
                            last.kind = t.kind;
                        }
                        self.state = to as usize;
                    }
                    StateStack::Push(to) => {
                        self.state_stack.push((self.state, t.kind));
                        self.state = to as usize;
                    }
                    StateStack::Pop(count) => {
                        self.state_stack
                            .truncate(self.state_stack.len().saturating_sub(count as usize));
                        (self.state, self.kind) =
                            self.state_stack.last().copied().unwrap_or_default();
                    }
                }

                if self.kind != t.kind {
                    if let Some(last) = res.last_mut() {
                        last.range.end = off;
                    }
                    if self.kind != HighlightKind::Other {
                        res.push(Higlight { range: off..line_buf.len(), kind: self.kind });
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

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.range.start += line_beg;
            h.range.end += line_beg;
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
                Higlight { range: 0..9, kind: HighlightKind::Variable },
                Higlight { range: 10..11, kind: HighlightKind::Operator },
                Higlight { range: 12..21, kind: HighlightKind::Method },
                Higlight { range: 22..38, kind: HighlightKind::String },
                Higlight { range: 38..45, kind: HighlightKind::Variable },
                Higlight { range: 45..54, kind: HighlightKind::String },
            ]
        );
    }
}
