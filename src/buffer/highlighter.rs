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
    Prefix(&'static str),
    Skip(usize),
    AlphaNum,
    NonAlphaNum,
    LineEnd,
}

struct Language {
    char_classifier: [CharClass; 256],
    transitions: &'static [&'static [Transition]],
}

struct Transition {
    test: Test,
    kind: TokenKind,
    state: usize,
}

const fn const_fill<T: Copy>(dst: &mut [T], range: RangeInclusive<char>, kind: T) {
    let mut i = *range.start() as usize;
    let end = *range.end() as usize;

    while i <= end {
        dst[i] = kind;
        i += 1;
    }
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

    Language {
        char_classifier: {
            let mut lut = [CharClass::Other; 256];
            const_fill(&mut lut, '\0'..=' ', CharClass::Whitespace);
            const_fill(&mut lut, '0'..='9', CharClass::Numeric);
            const_fill(&mut lut, 'A'..='Z', CharClass::Alpha);
            const_fill(&mut lut, 'a'..='z', CharClass::Alpha);
            const_fill(&mut lut, '\u{0080}'..='\u{00FF}', CharClass::Alpha);
            lut
        },
        transitions: &[
            // GROUND
            &[
                // Comments
                T { test: Prefix("#"), kind: Comment, state: LINE_COMMENT },
                T { test: Prefix("<#"), kind: Comment, state: BLOCK_COMMENT },
                // Numbers
                // Strings
                T { test: Prefix("'"), kind: String, state: STRING_SINGLE },
                T { test: Prefix("\""), kind: String, state: STRING_DOUBLE },
                // Variables
                T { test: Prefix("$"), kind: Variable, state: VARIABLE },
                // Operators
                T { test: Prefix("++"), kind: Operator, state: GROUND },
                T { test: Prefix("--"), kind: Operator, state: GROUND },
                T { test: Prefix("="), kind: Operator, state: GROUND },
                T { test: Prefix("<"), kind: Operator, state: GROUND },
                T { test: Prefix(">"), kind: Operator, state: GROUND },
                T { test: Prefix("+"), kind: Operator, state: GROUND },
                T { test: Prefix("-"), kind: Operator, state: GROUND },
                T { test: Prefix("*"), kind: Operator, state: GROUND },
                T { test: Prefix("/"), kind: Operator, state: GROUND },
                T { test: Prefix("%"), kind: Operator, state: GROUND },
                T { test: Prefix("!"), kind: Operator, state: GROUND },
                T { test: Prefix("|"), kind: Operator, state: GROUND },
                // Keywords
                T { test: Prefix("break"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("catch"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("continue"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("do"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("else"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("finally"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("foreach"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("function"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("if"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("return"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("switch"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("throw"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("try"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("using"), kind: Keyword, state: KEYWORD },
                T { test: Prefix("while"), kind: Keyword, state: KEYWORD },
                // Methods
                T { test: NonAlphaNum, kind: Method, state: GROUND },
            ],
            // LINE_COMMENT: # comment
            &[T { test: LineEnd, kind: Comment, state: GROUND }],
            // BLOCK_COMMENT: <# comment #>
            &[T { test: Prefix("#>"), kind: Comment, state: GROUND }],
            // STRING_SINGLE: 'string'
            &[T { test: Prefix("'"), kind: String, state: GROUND }],
            // STRING_DOUBLE: "string"
            &[
                T { test: Prefix("`"), kind: String, state: STRING_ESCAPE },
                T { test: Prefix("$"), kind: Variable, state: VARIABLE },
                T { test: Prefix("\""), kind: String, state: GROUND },
            ],
            // STRING_ESCAPE: "`a"
            &[T { test: Skip(1), kind: String, state: STRING_DOUBLE }],
            // VARIABLE: $variable
            &[
                T { test: Prefix("{"), kind: Variable, state: VARIABLE_BRACE },
                T { test: Prefix("("), kind: Variable, state: VARIABLE_PAREN },
                T { test: NonAlphaNum, kind: Variable, state: GROUND },
            ],
            // VARIABLE_BRACE: ${variable}
            &[T { test: Prefix("}"), kind: Variable, state: GROUND }],
            // VARIABLE_PAREN: $(command)
            &[T { test: Prefix(")"), kind: Variable, state: GROUND }],
            // KEYWORD: foreach, if, etc.
            &[
                T { test: NonAlphaNum, kind: Keyword, state: GROUND },
                T { test: AlphaNum, kind: Method, state: GROUND },
            ],
        ],
    }
};

pub struct Parser<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,
}

impl<'doc> Parser<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument) -> Self {
        Self { doc, offset: 0, logical_pos_y: 0 }
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

        // If the line is too long, we don't highlight it.
        // This is to prevent performance issues with very long lines.
        if line_buf.len() > MEBI {
            return res;
        }

        let line_buf = unicode::strip_newline(&line_buf);
        let mut off = 0;
        let mut token_beg = 0;
        let mut char_class = CharClass::Whitespace;
        let mut state = 0;

        loop {
            let beg = off;

            while off < line_buf.len() {
                let c = POWERSHELL.char_classifier[line_buf[off] as usize];
                if c != char_class {
                    char_class = c;
                    break;
                }
                off += 1;
            }
            if off >= line_buf.len() {
                break;
            }

            for t in POWERSHELL.transitions[state] {
                let hit = match t.test {
                    Test::Prefix(str) => {
                        let hit = line_buf[off..].starts_with(str.as_bytes());
                        if hit {
                            off += str.len();
                        }
                        hit
                    }
                    Test::Skip(n) => {
                        off += n;
                        true
                    }
                    Test::AlphaNum => {
                        matches!(char_class, CharClass::Alpha | CharClass::Numeric)
                    }
                    Test::NonAlphaNum => {
                        !matches!(char_class, CharClass::Alpha | CharClass::Numeric)
                    }
                    Test::LineEnd => {
                        off = line_buf.len();
                        true
                    }
                };

                if hit {
                    if state == 0 {
                        token_beg = beg;
                    }
                    if t.state == 0 {
                        res.push(Token { range: token_beg..off, kind: t.kind });
                    }
                    state = t.state;
                    break;
                }
            }
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
