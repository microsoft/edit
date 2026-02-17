// Copyright (c) iEdit contributors.
// Licensed under the MIT License.

//! INI/conf file syntax highlighting tokenizer.
//!
//! Supports INI-style config files used by AWS CLI, kubectl, git, systemd,
//! .editorconfig, .env, .properties, and similar formats.
//!
//! Recognized constructs:
//! - Sections: `[section]`, `[profile dev]`
//! - Key-value pairs: `key = value`, `key: value`, `key=value`
//! - Comments: `#` and `;`
//! - Quoted strings in values: `"..."` and `'...'`
//! - Numbers and booleans in values
//!
//! No multi-line state is needed for this format.

use super::{Token, TokenKind};

pub fn tokenize_line(line: &[u8], _state: u8, tokens: &mut Vec<Token>) -> u8 {
    if line.is_empty() {
        return 0;
    }

    let len = line.len();
    let mut i = 0;

    // Skip leading whitespace.
    while i < len && (line[i] == b' ' || line[i] == b'\t') {
        i += 1;
    }

    if i >= len {
        return 0;
    }

    // Comment line (# or ;).
    if line[i] == b'#' || line[i] == b';' {
        tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
        return 0;
    }

    // Section header: [section name]
    if line[i] == b'[' {
        let start = i;
        // Find the closing bracket.
        while i < len && line[i] != b']' {
            i += 1;
        }
        if i < len {
            i += 1; // include ']'
        }
        tokens.push(Token { offset: start, len: i - start, kind: TokenKind::Section });

        // Rest of line after section (usually a comment).
        skip_space(line, &mut i);
        if i < len && (line[i] == b'#' || line[i] == b';') {
            tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
        }
        return 0;
    }

    // Key = value or key: value
    let key_start = i;
    while i < len && line[i] != b'=' && line[i] != b':' && line[i] != b'#' && line[i] != b';' {
        i += 1;
    }

    if i < len && (line[i] == b'=' || line[i] == b':') {
        // Trim trailing whitespace from key.
        let mut key_end = i;
        while key_end > key_start && (line[key_end - 1] == b' ' || line[key_end - 1] == b'\t') {
            key_end -= 1;
        }

        if key_end > key_start {
            tokens.push(Token { offset: key_start, len: key_end - key_start, kind: TokenKind::Key });
        }

        // The delimiter (= or :).
        tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
        i += 1;
        skip_space(line, &mut i);

        // Tokenize the value.
        tokenize_value(line, i, tokens);
    } else {
        // No delimiter found - could be a bare key or directive. Treat as default.
        if i > key_start {
            tokens.push(Token { offset: key_start, len: i - key_start, kind: TokenKind::Default });
        }
        // Trailing comment.
        if i < len && (line[i] == b'#' || line[i] == b';') {
            tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
        }
    }

    0
}

fn tokenize_value(line: &[u8], mut i: usize, tokens: &mut Vec<Token>) {
    let len = line.len();

    while i < len {
        skip_space(line, &mut i);
        if i >= len {
            break;
        }

        match line[i] {
            // Inline comment.
            b'#' | b';' => {
                // In INI files, # and ; start comments only if preceded by whitespace
                // or at the start of the value portion.
                tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
                return;
            }
            // Quoted string.
            b'"' | b'\'' => {
                let start = i;
                let quote = line[i];
                i += 1;
                while i < len && line[i] != quote {
                    if line[i] == b'\\' {
                        i += 1; // skip escaped char
                    }
                    i += 1;
                }
                if i < len {
                    i += 1; // closing quote
                }
                tokens.push(Token { offset: start, len: i - start, kind: TokenKind::String });
            }
            // Unquoted value.
            _ => {
                let start = i;
                // Scan to end of value (stop at inline comment or end of line).
                while i < len {
                    if (line[i] == b'#' || line[i] == b';') && i > 0 && line[i - 1] == b' ' {
                        break;
                    }
                    i += 1;
                }
                // Trim trailing whitespace.
                let mut end = i;
                while end > start && (line[end - 1] == b' ' || line[end - 1] == b'\t') {
                    end -= 1;
                }
                if end > start {
                    let val = &line[start..end];
                    let kind = classify_value(val);
                    tokens.push(Token { offset: start, len: end - start, kind });
                }
            }
        }
    }
}

fn classify_value(val: &[u8]) -> TokenKind {
    // Booleans.
    if val.eq_ignore_ascii_case(b"true")
        || val.eq_ignore_ascii_case(b"false")
        || val.eq_ignore_ascii_case(b"yes")
        || val.eq_ignore_ascii_case(b"no")
        || val.eq_ignore_ascii_case(b"on")
        || val.eq_ignore_ascii_case(b"off")
    {
        return TokenKind::Boolean;
    }

    // Null-like.
    if val.eq_ignore_ascii_case(b"null") || val.eq_ignore_ascii_case(b"none") || val == b"~" {
        return TokenKind::Null;
    }

    // Number (integer or simple float).
    if is_number(val) {
        return TokenKind::Number;
    }

    TokenKind::String
}

fn is_number(val: &[u8]) -> bool {
    if val.is_empty() {
        return false;
    }

    let mut i = 0;
    if val[i] == b'+' || val[i] == b'-' {
        i += 1;
        if i >= val.len() {
            return false;
        }
    }

    let mut has_digit = false;
    while i < val.len() && val[i].is_ascii_digit() {
        has_digit = true;
        i += 1;
    }
    if i < val.len() && val[i] == b'.' {
        i += 1;
        while i < val.len() && val[i].is_ascii_digit() {
            has_digit = true;
            i += 1;
        }
    }

    has_digit && i == val.len()
}

fn skip_space(line: &[u8], i: &mut usize) {
    while *i < line.len() && (line[*i] == b' ' || line[*i] == b'\t') {
        *i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        tokenize_line(input.as_bytes(), 0, &mut tokens);
        tokens
    }

    #[test]
    fn test_comment_hash() {
        let tokens = tokenize("# This is a comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
    }

    #[test]
    fn test_comment_semicolon() {
        let tokens = tokenize("; Another comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
    }

    #[test]
    fn test_section() {
        let tokens = tokenize("[default]");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Section);
        assert_eq!(tokens[0].offset, 0);
        assert_eq!(tokens[0].len, 9);
    }

    #[test]
    fn test_section_with_spaces() {
        let tokens = tokenize("[profile dev]");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Section);
    }

    #[test]
    fn test_section_with_comment() {
        let tokens = tokenize("[section] # comment");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].kind, TokenKind::Section);
        assert_eq!(tokens[1].kind, TokenKind::Comment);
    }

    #[test]
    fn test_key_value_equals() {
        let tokens = tokenize("aws_access_key_id = AKIAIOSFODNN7EXAMPLE");
        assert!(tokens.len() >= 3);
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[1].kind, TokenKind::Punctuation);
        assert_eq!(tokens[2].kind, TokenKind::String);
    }

    #[test]
    fn test_key_value_colon() {
        let tokens = tokenize("server: localhost");
        assert!(tokens.len() >= 3);
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[1].kind, TokenKind::Punctuation);
        assert_eq!(tokens[2].kind, TokenKind::String);
    }

    #[test]
    fn test_key_value_no_spaces() {
        let tokens = tokenize("key=value");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[1].kind, TokenKind::Punctuation);
        assert_eq!(tokens[2].kind, TokenKind::String);
    }

    #[test]
    fn test_key_value_number() {
        let tokens = tokenize("port = 8080");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::Number);
    }

    #[test]
    fn test_key_value_boolean() {
        let tokens = tokenize("enabled = true");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::Boolean);
    }

    #[test]
    fn test_key_value_quoted() {
        let tokens = tokenize("name = \"hello world\"");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::String);
    }

    #[test]
    fn test_inline_comment() {
        let tokens = tokenize("key = value # comment");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Comment));
    }

    #[test]
    fn test_indented_key() {
        let tokens = tokenize("  key = value");
        assert_eq!(tokens[0].kind, TokenKind::Key);
    }

    #[test]
    fn test_null_value() {
        let tokens = tokenize("value = none");
        assert_eq!(tokens[2].kind, TokenKind::Null);
    }

    #[test]
    fn test_empty_line() {
        let tokens = tokenize("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_whitespace_line() {
        let tokens = tokenize("   ");
        assert!(tokens.is_empty());
    }
}
