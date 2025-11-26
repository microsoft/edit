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
    let mut iter = line.char_indices().peekable();

    while let Some(&(start, ch)) = iter.peek() {
        // Whitespace run
        if ch.is_whitespace() {
            // consume run
            iter.next();
            while let Some(&(_, c)) = iter.peek() {
                if !c.is_whitespace() { break; }
                iter.next();
            }
            let end = iter.peek().map(|(i, _)| *i).unwrap_or(line.len());
            out.push(Token { kind: TokenKind::Whitespace, start, end });
            continue;
        }

        // Line comment starting with '#'
        if ch == '#' {
            let end = line.len();
            // consume remaining
            iter.next();
            while iter.next().is_some() {}
            out.push(Token { kind: TokenKind::Comment, start, end });
            break;
        }

        // Possible '//' comment or punctuation '/'
        if ch == '/' {
            iter.next();
            if let Some(&(_, '/')) = iter.peek() {
                // consume second '/'
                iter.next();
                // consume rest
                while iter.next().is_some() {}
                let end = line.len();
                out.push(Token { kind: TokenKind::Comment, start, end });
                break;
            } else {
                let end = iter.peek().map(|(i, _)| *i).unwrap_or(line.len());
                out.push(Token { kind: TokenKind::Punctuation, start, end });
                continue;
            }
        }

        // Strings
        if ch == '"' || ch == '\'' {
            let quote = ch;
            // consume opening
            iter.next();
            let mut last_idx = start + quote.len_utf8();
            let mut escaped = false;
            while let Some((i, c)) = iter.next() {
                last_idx = i + c.len_utf8();
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
            let end = last_idx;
            out.push(Token { kind: TokenKind::String, start, end });
            continue;
        }

        // Numbers
        if ch.is_ascii_digit() {
            iter.next();
            while let Some(&(_, c)) = iter.peek() {
                if c.is_ascii_digit() || c == '.' || c == '_' { iter.next(); } else { break }
            }
            let end = iter.peek().map(|(i, _)| *i).unwrap_or(line.len());
            out.push(Token { kind: TokenKind::Number, start, end });
            continue;
        }

        // Identifier
        if ch.is_alphabetic() || ch == '_' {
            iter.next();
            while let Some(&(_, c)) = iter.peek() {
                if c.is_alphanumeric() || c == '_' { iter.next(); } else { break }
            }
            let end = iter.peek().map(|(i, _)| *i).unwrap_or(line.len());
            out.push(Token { kind: TokenKind::Identifier, start, end });
            continue;
        }

        // Punctuation single char
        iter.next();
        let end = iter.peek().map(|(i, _)| *i).unwrap_or(line.len());
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
