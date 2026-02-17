// Copyright (c) iEdit contributors.
// Licensed under the MIT License.

//! YAML syntax highlighting tokenizer.
//!
//! State encoding (u8):
//!   0 = normal
//!   1 = inside block scalar (literal `|` or folded `>`)

use super::{Token, TokenKind};

/// YAML line states.
const STATE_NORMAL: u8 = 0;
const STATE_BLOCK_SCALAR: u8 = 1;

pub fn tokenize_line(line: &[u8], state: u8, tokens: &mut Vec<Token>) -> u8 {
    if line.is_empty() {
        return if state == STATE_BLOCK_SCALAR { STATE_BLOCK_SCALAR } else { STATE_NORMAL };
    }

    // If we're in a block scalar, check indentation.
    // Block scalars continue as long as lines are indented (or empty).
    if state == STATE_BLOCK_SCALAR {
        if line[0] == b' ' || line[0] == b'\t' {
            // Still inside block scalar, entire line is a string.
            tokens.push(Token { offset: 0, len: line.len(), kind: TokenKind::String });
            return STATE_BLOCK_SCALAR;
        }
        // No longer indented => exit block scalar, fall through to normal parsing.
    }

    let mut i = 0;
    let len = line.len();

    // Skip leading whitespace.
    let indent_start = i;
    while i < len && (line[i] == b' ' || line[i] == b'\t') {
        i += 1;
    }

    if i >= len {
        return STATE_NORMAL;
    }

    // Document markers: `---` and `...`
    if i == 0 && len >= 3 {
        if &line[0..3] == b"---" && (len == 3 || line[3] == b' ' || line[3] == b'\n') {
            tokens.push(Token { offset: 0, len: 3, kind: TokenKind::Punctuation });
            i = 3;
            return tokenize_value(line, i, tokens);
        }
        if &line[0..3] == b"..." && (len == 3 || line[3] == b' ' || line[3] == b'\n') {
            tokens.push(Token { offset: 0, len: 3, kind: TokenKind::Punctuation });
            return STATE_NORMAL;
        }
    }

    // Full-line comment.
    if line[i] == b'#' {
        tokens.push(Token { offset: indent_start, len: len - indent_start, kind: TokenKind::Comment });
        return STATE_NORMAL;
    }

    // Directive (e.g., %YAML, %TAG).
    if i == 0 && line[0] == b'%' {
        tokens.push(Token { offset: 0, len, kind: TokenKind::Tag });
        return STATE_NORMAL;
    }

    // List item marker `- `.
    if line[i] == b'-' && i + 1 < len && line[i + 1] == b' ' {
        tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
        i += 2;
        if i >= len {
            return STATE_NORMAL;
        }
    }

    // Try to find a key: value pair.
    // A key is everything before the first `: ` or `:\n` or `:$`.
    if let Some(new_state) = try_tokenize_key_value(line, i, tokens) {
        return new_state;
    }

    // If no key-value found, tokenize the rest as a value.
    tokenize_value(line, i, tokens)
}

/// Tries to parse `key: value`. Returns `Some(state)` if successful, `None` otherwise.
fn try_tokenize_key_value(line: &[u8], start: usize, tokens: &mut Vec<Token>) -> Option<u8> {
    let len = line.len();
    let mut i = start;

    // Quoted key.
    if i < len && (line[i] == b'"' || line[i] == b'\'') {
        let quote = line[i];
        let key_start = i;
        i += 1;
        while i < len && line[i] != quote {
            if line[i] == b'\\' && quote == b'"' {
                i += 1; // skip escaped char
            }
            i += 1;
        }
        if i < len {
            i += 1; // closing quote
        }
        // Look for colon after key.
        if i < len && line[i] == b':' && (i + 1 >= len || line[i + 1] == b' ' || line[i + 1] == b'\n')
        {
            tokens.push(Token { offset: key_start, len: i - key_start, kind: TokenKind::Key });
            tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
            i += 1;
            skip_space(line, &mut i);
            let state = tokenize_value(line, i, tokens);
            return Some(state);
        }
        // Not a key-value, backtrack.
        return None;
    }

    // Unquoted key: scan for `: ` or `:\n` or `:$`.
    while i < len {
        if line[i] == b':' && (i + 1 >= len || line[i + 1] == b' ' || line[i + 1] == b'\n') {
            let key_len = i - start;
            if key_len > 0 {
                tokens.push(Token { offset: start, len: key_len, kind: TokenKind::Key });
            }
            tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
            i += 1;
            skip_space(line, &mut i);
            let state = tokenize_value(line, i, tokens);
            return Some(state);
        }
        if line[i] == b'#' && i > 0 && line[i - 1] == b' ' {
            // Inline comment, so this can't be a key-value on this segment.
            break;
        }
        i += 1;
    }

    None
}

/// Tokenizes a YAML value starting at position `i`. Returns the new line state.
fn tokenize_value(line: &[u8], mut i: usize, tokens: &mut Vec<Token>) -> u8 {
    let len = line.len();

    while i < len {
        skip_space(line, &mut i);
        if i >= len {
            break;
        }

        match line[i] {
            // Comment.
            b'#' => {
                tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
                return STATE_NORMAL;
            }
            // Quoted string.
            b'"' | b'\'' => {
                let start = i;
                let quote = line[i];
                i += 1;
                while i < len && line[i] != quote {
                    if line[i] == b'\\' && quote == b'"' {
                        i += 1;
                    }
                    i += 1;
                }
                if i < len {
                    i += 1; // closing quote
                }
                tokens.push(Token { offset: start, len: i - start, kind: TokenKind::String });
            }
            // Anchor (&name) or alias (*name).
            b'&' | b'*' => {
                let start = i;
                i += 1;
                while i < len && is_anchor_char(line[i]) {
                    i += 1;
                }
                tokens.push(Token { offset: start, len: i - start, kind: TokenKind::Anchor });
            }
            // Tag (!tag or !!type).
            b'!' => {
                let start = i;
                i += 1;
                while i < len && line[i] != b' ' && line[i] != b'\n' {
                    i += 1;
                }
                tokens.push(Token { offset: start, len: i - start, kind: TokenKind::Tag });
            }
            // Block scalar indicators.
            b'|' | b'>' => {
                let start = i;
                i += 1;
                // Optional chomping indicator and indentation indicator.
                while i < len && (line[i] == b'+' || line[i] == b'-' || line[i].is_ascii_digit()) {
                    i += 1;
                }
                tokens.push(Token { offset: start, len: i - start, kind: TokenKind::Punctuation });
                // Rest of line after block scalar indicator (might be a comment).
                skip_space(line, &mut i);
                if i < len && line[i] == b'#' {
                    tokens.push(Token { offset: i, len: len - i, kind: TokenKind::Comment });
                }
                return STATE_BLOCK_SCALAR;
            }
            // Flow indicators.
            b'[' | b']' | b'{' | b'}' | b',' => {
                tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
                i += 1;
            }
            // List item inside value context.
            b'-' if i + 1 < len && line[i + 1] == b' ' => {
                tokens.push(Token { offset: i, len: 1, kind: TokenKind::Punctuation });
                i += 2;
            }
            // Other value: number, boolean, null, or plain scalar.
            _ => {
                let start = i;
                // Scan to end of value (stop at comment or end of line).
                while i < len {
                    if line[i] == b'#' && i > 0 && line[i - 1] == b' ' {
                        break;
                    }
                    i += 1;
                }
                // Trim trailing whitespace from the value.
                let mut end = i;
                while end > start && (line[end - 1] == b' ' || line[end - 1] == b'\t') {
                    end -= 1;
                }
                if end > start {
                    let val = &line[start..end];
                    let kind = classify_scalar(val);
                    tokens.push(Token { offset: start, len: end - start, kind });
                }
            }
        }
    }

    STATE_NORMAL
}

/// Classifies a plain (unquoted) scalar value.
fn classify_scalar(val: &[u8]) -> TokenKind {
    // null / Null / NULL / ~
    if val == b"null" || val == b"Null" || val == b"NULL" || val == b"~" {
        return TokenKind::Null;
    }

    // true / false / True / False / TRUE / FALSE / yes / no / Yes / No / YES / NO / on / off / On / Off / ON / OFF
    if val == b"true"
        || val == b"false"
        || val == b"True"
        || val == b"False"
        || val == b"TRUE"
        || val == b"FALSE"
        || val == b"yes"
        || val == b"no"
        || val == b"Yes"
        || val == b"No"
        || val == b"YES"
        || val == b"NO"
        || val == b"on"
        || val == b"off"
        || val == b"On"
        || val == b"Off"
        || val == b"ON"
        || val == b"OFF"
    {
        return TokenKind::Boolean;
    }

    // Number: integer or float (including 0x, 0o, 0b prefixed, infinity, NaN).
    if is_yaml_number(val) {
        return TokenKind::Number;
    }

    TokenKind::String
}

/// Checks if a byte sequence represents a YAML number.
fn is_yaml_number(val: &[u8]) -> bool {
    if val.is_empty() {
        return false;
    }

    let mut i = 0;

    // Optional sign.
    if val[i] == b'+' || val[i] == b'-' {
        i += 1;
        if i >= val.len() {
            return false;
        }
    }

    // Special float values.
    if &val[i..] == b".inf" || &val[i..] == b".Inf" || &val[i..] == b".INF" {
        return true;
    }
    if val == b".nan" || val == b".NaN" || val == b".NAN" {
        return true;
    }

    // Hex (0x), octal (0o), binary (0b).
    if i + 1 < val.len() && val[i] == b'0' {
        match val[i + 1] {
            b'x' | b'X' => return val[i + 2..].iter().all(|b| b.is_ascii_hexdigit()),
            b'o' | b'O' => return val[i + 2..].iter().all(|b| (b'0'..=b'7').contains(b)),
            b'b' | b'B' => return val[i + 2..].iter().all(|b| *b == b'0' || *b == b'1'),
            _ => {}
        }
    }

    // Decimal integer or float.
    let mut has_digit = false;
    while i < val.len() && val[i].is_ascii_digit() {
        has_digit = true;
        i += 1;
    }
    // Optional decimal point.
    if i < val.len() && val[i] == b'.' {
        i += 1;
        while i < val.len() && val[i].is_ascii_digit() {
            has_digit = true;
            i += 1;
        }
    }
    // Optional exponent.
    if i < val.len() && (val[i] == b'e' || val[i] == b'E') {
        i += 1;
        if i < val.len() && (val[i] == b'+' || val[i] == b'-') {
            i += 1;
        }
        while i < val.len() && val[i].is_ascii_digit() {
            i += 1;
        }
    }

    has_digit && i == val.len()
}

fn is_anchor_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'-' || b == b'.'
}

fn skip_space(line: &[u8], i: &mut usize) {
    while *i < line.len() && (line[*i] == b' ' || line[*i] == b'\t') {
        *i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> (Vec<Token>, u8) {
        let mut tokens = Vec::new();
        let state = tokenize_line(input.as_bytes(), STATE_NORMAL, &mut tokens);
        (tokens, state)
    }

    fn tokenize_with_state(input: &str, state: u8) -> (Vec<Token>, u8) {
        let mut tokens = Vec::new();
        let new_state = tokenize_line(input.as_bytes(), state, &mut tokens);
        (tokens, new_state)
    }

    #[test]
    fn test_comment() {
        let (tokens, state) = tokenize("# This is a comment");
        assert_eq!(state, STATE_NORMAL);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[0].offset, 0);
    }

    #[test]
    fn test_key_value_string() {
        let (tokens, _) = tokenize("name: hello");
        assert!(tokens.len() >= 2);
        assert_eq!(tokens[0].kind, TokenKind::Key); // "name"
        assert_eq!(tokens[1].kind, TokenKind::Punctuation); // ":"
        assert_eq!(tokens[2].kind, TokenKind::String); // "hello"
    }

    #[test]
    fn test_key_value_number() {
        let (tokens, _) = tokenize("port: 8080");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::Number);
    }

    #[test]
    fn test_key_value_boolean() {
        let (tokens, _) = tokenize("enabled: true");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::Boolean);
    }

    #[test]
    fn test_key_value_null() {
        let (tokens, _) = tokenize("value: null");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::Null);
    }

    #[test]
    fn test_quoted_string_value() {
        let (tokens, _) = tokenize("msg: \"hello world\"");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert_eq!(tokens[2].kind, TokenKind::String);
    }

    #[test]
    fn test_list_item() {
        let (tokens, _) = tokenize("  - item");
        assert_eq!(tokens[0].kind, TokenKind::Punctuation); // "-"
        assert_eq!(tokens[1].kind, TokenKind::String); // "item"
    }

    #[test]
    fn test_anchor() {
        let (tokens, _) = tokenize("key: &anchor value");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        // Should have anchor token
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Anchor));
    }

    #[test]
    fn test_tag() {
        let (tokens, _) = tokenize("key: !!str 123");
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Tag));
    }

    #[test]
    fn test_block_scalar() {
        let (tokens, state) = tokenize("description: |");
        assert_eq!(state, STATE_BLOCK_SCALAR);
        assert_eq!(tokens[0].kind, TokenKind::Key);

        // Indented continuation.
        let (tokens2, state2) = tokenize_with_state("  This is text", STATE_BLOCK_SCALAR);
        assert_eq!(state2, STATE_BLOCK_SCALAR);
        assert_eq!(tokens2[0].kind, TokenKind::String);

        // Un-indented line exits block scalar.
        let (_, state3) = tokenize_with_state("next_key: value", STATE_BLOCK_SCALAR);
        assert_eq!(state3, STATE_NORMAL);
    }

    #[test]
    fn test_document_marker() {
        let (tokens, _) = tokenize("---");
        assert_eq!(tokens[0].kind, TokenKind::Punctuation);
    }

    #[test]
    fn test_inline_comment() {
        let (tokens, _) = tokenize("key: value # comment");
        assert_eq!(tokens[0].kind, TokenKind::Key);
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Comment));
    }

    #[test]
    fn test_flow_mapping() {
        let (tokens, _) = tokenize("{a: 1, b: 2}");
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Punctuation));
    }

    #[test]
    fn test_numbers() {
        assert!(is_yaml_number(b"42"));
        assert!(is_yaml_number(b"-3.14"));
        assert!(is_yaml_number(b"1e10"));
        assert!(is_yaml_number(b"0xFF"));
        assert!(is_yaml_number(b"0o77"));
        assert!(is_yaml_number(b"0b1010"));
        assert!(is_yaml_number(b".inf"));
        assert!(is_yaml_number(b".NaN"));
        assert!(!is_yaml_number(b"abc"));
        assert!(!is_yaml_number(b""));
    }
}
