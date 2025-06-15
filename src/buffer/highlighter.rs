#![allow(dead_code, unused_variables, unused_mut)]

use std::ops::{Range, RangeInclusive};

use crate::arena::{Arena, scratch_arena};
use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::{simd, unicode};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub range: Range<usize>,
    pub kind: TokenKind,
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
    ConsumeWord,
    ConsumeToLineEnd,
}

struct Language {
    word_chars: &'static [RangeInclusive<u8>],
    states: &'static [&'static [Transition]],
}

struct Transition {
    test: Test,
    kind: TokenKind,
    state: usize,
}

const POWERSHELL: Language = {
    type T = Transition;
    use Test::*;
    use TokenKind::*;

    const GROUND: usize = 0;

    const LINE_COMMENT: usize = 1;
    const BLOCK_COMMENT: usize = 2;

    const STRING_SINGLE: usize = 3;
    const STRING_DOUBLE: usize = 4;
    const STRING_ESCAPE: usize = 5;

    const VARIABLE: usize = 6;
    const VARIABLE_BRACE: usize = 7;
    const VARIABLE_PAREN: usize = 8;

    const KEYWORD: usize = 9;
    const METHOD: usize = 10;

    Language {
        word_chars: &[b'0'..=b'9', b'A'..=b'Z', b'a'..=b'z', b'?'..=b'?', b'_'..=b'_'],
        states: &[
            // GROUND
            &[
                // Comments
                T { test: ConsumePrefix("#"), kind: Comment, state: LINE_COMMENT },
                T { test: ConsumePrefix("<#"), kind: Comment, state: BLOCK_COMMENT },
                // Numbers
                // Strings
                T { test: ConsumePrefix("'"), kind: String, state: STRING_SINGLE },
                T { test: ConsumePrefix("\""), kind: String, state: STRING_DOUBLE },
                // Variables
                T { test: ConsumePrefix("$"), kind: Variable, state: VARIABLE },
                // Operators
                T { test: ConsumePrefix("++"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("--"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("="), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("<"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix(">"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("+"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("-"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("*"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("/"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("%"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("!"), kind: Operator, state: GROUND },
                T { test: ConsumePrefix("|"), kind: Operator, state: GROUND },
                // Keywords
                T { test: ConsumePrefix("break"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("catch"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("continue"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("do"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("else"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("finally"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("foreach"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("function"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("if"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("return"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("switch"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("throw"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("try"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("using"), kind: Keyword, state: KEYWORD },
                T { test: ConsumePrefix("while"), kind: Keyword, state: KEYWORD },
                // Methods
                T { test: ConsumeWord, kind: Method, state: METHOD },
            ],
            // LINE_COMMENT: # comment
            &[T { test: ConsumeToLineEnd, kind: Comment, state: GROUND }],
            // BLOCK_COMMENT: <# comment #>
            &[T { test: ConsumePrefix("#>"), kind: Comment, state: GROUND }],
            // STRING_SINGLE: 'string'
            &[T { test: ConsumePrefix("'"), kind: String, state: GROUND }],
            // STRING_DOUBLE: "string"
            &[
                T { test: ConsumePrefix("`"), kind: String, state: STRING_ESCAPE },
                T { test: ConsumePrefix("$"), kind: Variable, state: VARIABLE },
                T { test: ConsumePrefix("\""), kind: String, state: GROUND },
            ],
            // STRING_ESCAPE: "`a"
            &[T { test: Consume(1), kind: String, state: STRING_DOUBLE }],
            // VARIABLE: $variable
            &[
                T { test: ConsumePrefix("{"), kind: Variable, state: VARIABLE_BRACE },
                T { test: ConsumePrefix("("), kind: Variable, state: VARIABLE_PAREN },
                T { test: ConsumeWord, kind: Variable, state: GROUND },
            ],
            // VARIABLE_BRACE: ${variable}
            &[T { test: ConsumePrefix("}"), kind: Variable, state: GROUND }],
            // VARIABLE_PAREN: $(command)
            &[T { test: ConsumePrefix(")"), kind: Variable, state: GROUND }],
            // KEYWORD: foreach, if, etc.
            &[
                T { test: ConsumeWord, kind: Method, state: METHOD },
                T { test: Consume(0), kind: Keyword, state: GROUND },
            ],
            // METHOD: Foo-Bar
            &[
                T { test: ConsumeWord, kind: Method, state: METHOD },
                T { test: ConsumePrefix("-"), kind: Method, state: METHOD },
                T { test: Consume(0), kind: Method, state: GROUND },
            ],
        ],
    }
};

pub struct Parser<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,

    language: &'static Language,
    word_chars: [bool; 256],
    starter: Vec<[bool; 256]>,
}

impl<'doc> Parser<'doc> {
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
                    Test::ConsumeWord => Self::fill_word_chars(&mut starter, language.word_chars),
                    Test::ConsumeToLineEnd => {}
                }
            }
            starter
        }));

        Self { doc, offset: 0, logical_pos_y: 0, language, word_chars, starter }
    }

    fn fill_word_chars(dst: &mut [bool; 256], src: &[RangeInclusive<u8>]) {
        for r in src {
            dst[*r.start() as usize..=*r.end() as usize].fill(true);
        }
        dst[0x80..].fill(true);
    }

    pub fn logical_pos_y(&self) -> CoordType {
        self.logical_pos_y
    }

    pub fn parse_next_line<'a>(&mut self, arena: &'a Arena) -> Vec<Token, &'a Arena> {
        let scratch = scratch_arena(Some(arena));
        let line_offset = self.offset;
        let mut line_buf = Vec::new_in(&*scratch);
        let mut res = Vec::new_in(arena);

        // Accumulate a line of text into `line_buf`.
        {
            let mut chunk = self.doc.read_forward(self.offset);

            // Check if the last line was the last line in the document.
            if chunk.is_empty() {
                return res;
            }

            if self.offset != 0 {
                self.logical_pos_y += 1;
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
        let mut token_beg = 0;
        let mut state = 0;
        let mut kind = TokenKind::Other;

        loop {
            while off < line_buf.len() && !self.starter[state][line_buf[off] as usize] {
                off += 1;
            }
            if off >= line_buf.len() {
                break;
            }

            let mut hit = false;
            let beg = off;

            for t in self.language.states[state] {
                match t.test {
                    Test::Consume(n) => {
                        off += n;
                        hit = true;
                    }
                    Test::ConsumePrefix(str) => {
                        hit = line_buf[off..].starts_with(str.as_bytes());
                        if hit {
                            off += str.len();
                        }
                    }
                    Test::ConsumeWord => {
                        while off < line_buf.len() && self.word_chars[line_buf[off] as usize] {
                            off += 1;
                            hit = true;
                        }
                    }
                    Test::ConsumeToLineEnd => {
                        off = line_buf.len();
                        hit = true;
                    }
                };

                if hit {
                    if kind != t.kind {
                        token_beg = beg;
                    }
                    state = t.state;
                    kind = t.kind;
                    if state == 0 {
                        res.push(Token { range: token_beg..off, kind });
                    }
                    break;
                }
            }
        }

        if state != 0 {
            res.push(Token { range: token_beg..off, kind });
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
        let mut parser = Parser::new(&bytes);

        let tokens = parser.parse_next_line(&scratch);
        assert_eq!(
            tokens,
            &[
                Token { range: 0..9, kind: TokenKind::Variable },
                Token { range: 10..11, kind: TokenKind::Operator },
                Token { range: 12..21, kind: TokenKind::Method },
                Token { range: 22..38, kind: TokenKind::String },
                Token { range: 38..45, kind: TokenKind::Variable },
                Token { range: 45..54, kind: TokenKind::String },
            ]
        );
    }
}
