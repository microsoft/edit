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
    let mut iter = line.chars().peekable();
    let mut char_idx = 0usize;

    while let Some(&ch) = iter.peek() {
        let start = char_idx;

        // Whitespace run
        if ch.is_whitespace() {
            iter.next(); char_idx += 1;
            while let Some(&c) = iter.peek() {
                if !c.is_whitespace() { break; }
                iter.next(); char_idx += 1;
            }
            let end = char_idx;
            out.push(Token { kind: TokenKind::Whitespace, start, end });
            continue;
        }

        // Line comment starting with '#'
        if ch == '#' {
            iter.next(); char_idx += 1;
            while iter.next().is_some() { char_idx += 1; }
            let end = char_idx;
            out.push(Token { kind: TokenKind::Comment, start, end });
            break;
        }

        // Possible '//' comment or punctuation '/'
        if ch == '/' {
            iter.next(); char_idx += 1;
            if let Some(&'/') = iter.peek() {
                iter.next(); char_idx += 1;
                while iter.next().is_some() { char_idx += 1; }
                let end = char_idx;
                out.push(Token { kind: TokenKind::Comment, start, end });
                break;
            } else {
                let end = char_idx;
                out.push(Token { kind: TokenKind::Punctuation, start, end });
                continue;
            }
        }

        // Strings
        if ch == '"' || ch == '\'' {
            let quote = ch;
            iter.next(); char_idx += 1;
            let mut escaped = false;
            while let Some(c) = iter.next() {
                char_idx += 1;
                if escaped { escaped = false; continue; }
                if c == '\\' { escaped = true; continue; }
                if c == quote { break; }
            }
            let end = char_idx;
            out.push(Token { kind: TokenKind::String, start, end });
            continue;
        }

        // Numbers
        if ch.is_ascii_digit() {
            iter.next(); char_idx += 1;
            while let Some(&c) = iter.peek() {
                if c.is_ascii_digit() || c == '.' || c == '_' { iter.next(); char_idx += 1; } else { break }
            }
            let end = char_idx;
            out.push(Token { kind: TokenKind::Number, start, end });
            continue;
        }

        // Identifier
        if ch.is_alphabetic() || ch == '_' {
            iter.next(); char_idx += 1;
            while let Some(&c) = iter.peek() {
                if c.is_alphanumeric() || c == '_' { iter.next(); char_idx += 1; } else { break }
            }
            let end = char_idx;
            out.push(Token { kind: TokenKind::Identifier, start, end });
            continue;
        }

        // Punctuation single char
        iter.next(); char_idx += 1;
        let end = char_idx;
        out.push(Token { kind: TokenKind::Punctuation, start, end });
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
        let num_text: String = s.chars().skip(num_tok.start).take(num_tok.end - num_tok.start).collect();
        assert_eq!(num_text, "42");
    }

    #[test]
    fn tokenize_string_and_ident() {
        let s = "\"hello\" world";
        let toks = tokenize_display_line(s);
        assert_eq!(toks[0].kind, TokenKind::String);
        let str_text: String = s.chars().skip(toks[0].start).take(toks[0].end - toks[0].start).collect();
        assert_eq!(str_text, "\"hello\"");
        assert_eq!(toks[1].kind, TokenKind::Whitespace);
        assert_eq!(toks[2].kind, TokenKind::Identifier);
        let id_text: String = s.chars().skip(toks[2].start).take(toks[2].end - toks[2].start).collect();
        assert_eq!(id_text, "world");
    }

    #[test]
    fn tokenize_hash_comment() {
        let s = "  #hi";
        let toks = tokenize_display_line(s);
        assert_eq!(toks[0].kind, TokenKind::Whitespace);
        assert_eq!(toks[1].kind, TokenKind::Comment);
        let c_text: String = s.chars().skip(toks[1].start).take(toks[1].end - toks[1].start).collect();
        assert_eq!(c_text, "#hi");
    }
}
