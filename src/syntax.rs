// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use crate::framebuffer::IndexedColor;

/// A token kind for the display-level generic tokenizer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Comment,
    String,
    Number,
    Identifier,
    Punctuation,
    Whitespace,
    Other,
}

/// A token within a display line measured in character columns (approximate).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    /// Start column (inclusive) within the display line.
    pub start: usize,
    /// End column (exclusive) within the display line.
    pub end: usize,
}

/// Simple, fast, single-pass tokenizer that operates on the already-processed
/// display line (tabs expanded, control glyphs replaced). It intentionally
/// keeps things minimal and avoids allocations where possible.
pub fn tokenize_display_line(line: &str) -> Vec<Token> {
    let mut out = Vec::new();
    let mut chars = line.chars().peekable();
    let mut col = 0usize;

    while let Some(&ch) = chars.peek() {
        // Determine token start at current column.
        let start = col;

        if ch.is_whitespace() {
            // Whitespace run
            let mut len = 0usize;
            while let Some(&c) = chars.peek() {
                if !c.is_whitespace() { break; }
                chars.next();
                len += 1;
            }
            out.push(Token { kind: TokenKind::Whitespace, start, end: start + len });
            col += len;
            continue;
        }

        // Line comment: starts with '#' or '//' sequence.
        if ch == '#' {
            // consume rest of line as comment
            let mut len = 0usize;
            while let Some(c) = chars.next() {
                len += 1;
            }
            out.push(Token { kind: TokenKind::Comment, start, end: start + len });
            col += len;
            break;
        }

        if ch == '/' {
            // possible // comment
            chars.next();
            if let Some(&'/') = chars.peek() {
                // consume the second '/'
                chars.next();
                let mut len = 2usize;
                while let Some(c) = chars.next() {
                    len += 1;
                }
                out.push(Token { kind: TokenKind::Comment, start, end: start + len });
                col += len;
                break;
            } else {
                // it's punctuation '/'
                out.push(Token { kind: TokenKind::Punctuation, start, end: start + 1 });
                col += 1;
                continue;
            }
        }

        // Strings: "..." or '...'
        if ch == '"' || ch == '\'' {
            let quote = ch;
            chars.next();
            let mut len = 1usize;
            let mut escaped = false;
            while let Some(c) = chars.next() {
                len += 1;
                if escaped {
                    escaped = false;
                    continue;
                }
                if c == '\\' {
                    escaped = true;
                    continue;
                }
                if c == quote {
                    break;
                }
            }
            out.push(Token { kind: TokenKind::String, start, end: start + len });
            col += len;
            continue;
        }

        // Numbers: start with digit
        if ch.is_ascii_digit() {
            let mut len = 0usize;
            while let Some(&c) = chars.peek() {
                if c.is_ascii_digit() || c == '.' || c == '_' { chars.next(); len += 1; } else { break }
            }
            out.push(Token { kind: TokenKind::Number, start, end: start + len });
            col += len;
            continue;
        }

        // Identifier: starts with letter or underscore
        if ch.is_alphabetic() || ch == '_' {
            let mut len = 0usize;
            while let Some(&c) = chars.peek() {
                if c.is_alphanumeric() || c == '_' { chars.next(); len += 1; } else { break }
            }
            out.push(Token { kind: TokenKind::Identifier, start, end: start + len });
            col += len;
            continue;
        }

        // Punctuation/other single char
        chars.next();
        out.push(Token { kind: TokenKind::Punctuation, start, end: start + 1 });
        col += 1;
    }

    out
}

/// Maps token kinds to an `IndexedColor` from the basic 8-color palette.
pub fn token_kind_color(kind: TokenKind) -> IndexedColor {
    match kind {
        TokenKind::Comment => IndexedColor::Green,
        TokenKind::String => IndexedColor::Red,
        TokenKind::Number => IndexedColor::Magenta,
        TokenKind::Identifier => IndexedColor::Cyan,
        TokenKind::Punctuation => IndexedColor::Yellow,
        TokenKind::Whitespace => IndexedColor::White,
        TokenKind::Other => IndexedColor::White,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_basic_line() {
        let s = "let x = 42; // comment";
        let toks = tokenize_display_line(s);
        let kinds: Vec<TokenKind> = toks.iter().map(|t| t.kind).collect();
        assert_eq!(kinds[0], TokenKind::Identifier); // "let"
        assert_eq!(kinds[kinds.len() - 1], TokenKind::Comment);

        // Verify spans for a couple tokens
        assert_eq!(toks[0].start, 0);
        assert_eq!(toks[0].end, 3); // "let"
        // number token should cover "42"
        let num_tok = toks.iter().find(|t| t.kind == TokenKind::Number).unwrap();
        assert_eq!(&s[num_tok.start..num_tok.end], "42");
    }

    #[test]
    fn tokenize_string_and_ident() {
        let s = "\"hello\" world";
        let toks = tokenize_display_line(s);
        assert_eq!(toks[0].kind, TokenKind::String);
        assert_eq!(&s[toks[0].start..toks[0].end], "\"hello\"");
        assert_eq!(toks[1].kind, TokenKind::Whitespace);
        assert_eq!(toks[2].kind, TokenKind::Identifier);
        assert_eq!(&s[toks[2].start..toks[2].end], "world");
    }

    #[test]
    fn tokenize_hash_comment() {
        let s = "  #hi";
        let toks = tokenize_display_line(s);
        assert_eq!(toks[0].kind, TokenKind::Whitespace);
        assert_eq!(toks[1].kind, TokenKind::Comment);
        assert_eq!(&s[toks[1].start..toks[1].end], "#hi");
    }
}
