// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A simple JSONC parser with trailing comma support.
//!
//! It's designed for parsing our small settings files,
//! but its performance is rather competitive in general.

use std::fmt;
use std::hint::unreachable_unchecked;

use stdext::arena::{Arena, ArenaString};

use crate::unicode::MeasurementConfig;

/// Maximum nesting depth to prevent stack overflow.
const MAX_DEPTH: usize = 64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    /// Invalid JSON syntax
    Syntax,
    /// Maximum nesting depth exceeded
    MaxDepth,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    line: usize,
    column: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self.kind {
            ParseErrorKind::Syntax => "Invalid JSON",
            ParseErrorKind::MaxDepth => "JSON too deeply nested",
        };
        write!(f, "{}:{}: {}", self.line, self.column, message)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Array(&'a [Value<'a>]),
    Object(&'a [(&'a str, Value<'a>)]),
}

impl<'a> Value<'a> {
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&'a str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&'a [Value<'a>]> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<Object<'a>> {
        match self {
            Value::Object(entries) => Some(Object { entries }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Object<'a> {
    entries: &'a [(&'a str, Value<'a>)],
}

impl<'a> Object<'a> {
    pub fn get(&self, key: &str) -> Option<&'a Value<'a>> {
        self.entries.iter().find(|e| e.0 == key).map(|e| &e.1)
    }

    pub fn get_bool(&self, key: &str) -> Option<bool> {
        self.get(key).and_then(Value::as_bool)
    }

    pub fn get_number(&self, key: &str) -> Option<f64> {
        self.get(key).and_then(Value::as_number)
    }

    pub fn get_str(&self, key: &str) -> Option<&'a str> {
        self.get(key).and_then(Value::as_str)
    }

    pub fn get_array(&self, key: &str) -> Option<&'a [Value<'a>]> {
        self.get(key).and_then(Value::as_array)
    }

    pub fn get_object(&self, key: &str) -> Option<Object<'a>> {
        self.get(key).and_then(Value::as_object)
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a (&'a str, Value<'a>)> {
        self.entries.iter()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

pub fn parse<'a>(arena: &'a Arena, input: &str) -> Result<Value<'a>, ParseError> {
    let mut parser = Parser::new(arena, input);
    parser.skip_bom();
    let value = parser.parse_value(0)?;
    parser.skip_whitespace_and_comments()?;
    if parser.pos == parser.input.len() {
        Ok(value)
    } else {
        // Unexpected data after JSON value
        Err(parser.fail(parser.pos, ParseErrorKind::Syntax))
    }
}

struct Parser<'a, 'i> {
    arena: &'a Arena,
    input: &'i str,
    bytes: &'i [u8],
    pos: usize,
}

impl<'a, 'i> Parser<'a, 'i> {
    fn new(arena: &'a Arena, input: &'i str) -> Self {
        Self { arena, input, bytes: input.as_bytes(), pos: 0 }
    }

    fn parse_value(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        // Prevent stack overflow from deeply nested structures
        if depth >= MAX_DEPTH {
            return Err(self.fail(self.pos, ParseErrorKind::MaxDepth));
        }

        self.skip_whitespace_and_comments()?;

        let ch = match self.peek() {
            Some(ch) => ch,
            // Unexpected end of input
            None => return Err(self.fail(self.pos, ParseErrorKind::Syntax)),
        };

        match ch {
            'n' => self.parse_null(),
            't' => self.parse_true(),
            'f' => self.parse_false(),
            '-' | '0'..='9' => self.parse_number(),
            '"' => self.parse_string(),
            '[' => self.parse_array(depth),
            '{' => self.parse_object(depth),
            _ => Err(self.fail(self.pos, ParseErrorKind::Syntax)),
        }
    }

    fn parse_null(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect_str("null")?;
        Ok(Value::Null)
    }

    fn parse_true(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect_str("true")?;
        Ok(Value::Bool(true))
    }

    fn parse_false(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect_str("false")?;
        Ok(Value::Bool(false))
    }

    fn parse_number(&mut self) -> Result<Value<'a>, ParseError> {
        let start = self.pos;

        while self.pos < self.bytes.len()
            && matches!(self.bytes[self.pos], b'0'..=b'9' | b'.' | b'-' | b'+' | b'e' | b'E')
        {
            self.pos += 1;
        }

        if let Ok(num) = self.input[start..self.pos].parse::<f64>()
            && num.is_finite()
        {
            Ok(Value::Number(num))
        } else {
            Err(self.fail(self.pos, ParseErrorKind::Syntax))
        }
    }

    fn parse_string(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect(b'"')?;

        let mut result = ArenaString::new_in(self.arena);

        loop {
            if self.pos >= self.bytes.len() {
                // Unterminated string
                return Err(self.fail(self.pos, ParseErrorKind::Syntax));
            }

            let b = self.bytes[self.pos];
            self.pos += 1;

            match b {
                b'"' => break,
                b'\\' => self.parse_escape(&mut result)?,
                ..=0x1f => {
                    // Control characters must be escaped
                    return Err(self.fail(self.pos - 1, ParseErrorKind::Syntax));
                }
                _ => {
                    let beg = self.pos - 1;

                    while self.pos < self.bytes.len()
                        && !matches!(self.bytes[self.pos], b'"' | b'\\' | ..=0x1f)
                    {
                        self.pos += 1;
                    }

                    result.push_str(&self.input[beg..self.pos]);
                }
            }
        }

        Ok(Value::String(result.leak()))
    }

    #[cold]
    fn parse_escape(&mut self, result: &mut ArenaString) -> Result<(), ParseError> {
        if self.pos >= self.bytes.len() {
            // Unterminated escape sequence
            return Err(self.fail(self.pos, ParseErrorKind::Syntax));
        }

        let b = self.bytes[self.pos];
        self.pos += 1;

        let ch = match b {
            b'"' => b'"',
            b'\\' => b'\\',
            b'/' => b'/',
            b'b' => b'\x08',
            b'f' => b'\x0C',
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'u' => return self.parse_unicode_escape(result),
            _ => {
                // Invalid escape sequence
                return Err(self.fail(self.pos - 2, ParseErrorKind::Syntax));
            }
        };

        result.push(ch as char);
        Ok(())
    }

    #[cold]
    fn parse_unicode_escape(&mut self, result: &mut ArenaString) -> Result<(), ParseError> {
        let start = self.pos - 2; // parse_escape() already advanced past "\u"
        let mut code = self.parse_hex4()?;

        if (0xd800..=0xdbff).contains(&code) {
            if self.is_str("\\u")
                && let _ = self.advance(2)
                && let Ok(low) = self.parse_hex4()
                && (0xdc00..=0xdfff).contains(&low)
            {
                code = 0x10000 + ((code - 0xd800) << 10) + (low - 0xdc00);
            } else {
                code = u32::MAX;
            };
        }

        match char::from_u32(code) {
            Some(c) => {
                result.push(c);
                Ok(())
            }
            None => Err(self.fail(start, ParseErrorKind::Syntax)),
        }
    }

    fn parse_hex4(&mut self) -> Result<u32, ParseError> {
        let start = self.pos - 2; // parse_unicode_escape() already advanced past "\u"

        self.bytes
            .get(self.pos..self.pos + 4)
            .and_then(|b| {
                self.pos += 4;
                b.iter().try_fold(0u32, |acc, &b| {
                    let d = (b as char).to_digit(16)?;
                    Some((acc << 4) | d)
                })
            })
            .ok_or_else(|| self.fail(start, ParseErrorKind::Syntax))
    }

    fn parse_array(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        let mut values = Vec::new_in(self.arena);
        let mut expects_comma = false;

        self.expect(b'[')?;

        loop {
            self.skip_whitespace_and_comments()?;

            match self.peek() {
                // Unexpected end of input
                None => return Err(self.fail(self.pos, ParseErrorKind::Syntax)),
                Some(']') => break,
                Some(',') => {
                    if !expects_comma {
                        // Unexpected comma
                        return Err(self.fail(self.pos, ParseErrorKind::Syntax));
                    }

                    self.advance(1);
                    self.skip_whitespace_and_comments()?;
                    expects_comma = false;
                }
                Some(_) => {
                    if expects_comma {
                        // Missing comma
                        return Err(self.fail(self.pos, ParseErrorKind::Syntax));
                    }

                    values.push(self.parse_value(depth + 1)?);
                    expects_comma = true;
                }
            }
        }

        self.expect(b']')?;
        Ok(Value::Array(values.leak()))
    }

    fn parse_object(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        let mut entries = Vec::new_in(self.arena);
        let mut expects_comma = false;

        self.expect(b'{')?;

        loop {
            self.skip_whitespace_and_comments()?;

            match self.peek() {
                // Unexpected end of input
                None => return Err(self.fail(self.pos, ParseErrorKind::Syntax)),
                Some(',') => {
                    if !expects_comma {
                        // Unexpected comma
                        return Err(self.fail(self.pos, ParseErrorKind::Syntax));
                    }

                    self.advance(1);
                    self.skip_whitespace_and_comments()?;
                    expects_comma = false;
                }
                Some('}') => break,
                Some(_) => {
                    if expects_comma {
                        // Missing comma
                        return Err(self.fail(self.pos, ParseErrorKind::Syntax));
                    }

                    let key = match self.parse_string()? {
                        Value::String(s) => s,
                        // The entire point of parse_string is to return a string.
                        // If that fails, we all should start farming potatoes.
                        // This is essentially an unwrap_unchecked().
                        _ => unsafe { unreachable_unchecked() },
                    };
                    self.skip_whitespace_and_comments()?;
                    self.expect(b':')?;

                    let value = self.parse_value(depth + 1)?;
                    entries.push((key, value));
                    expects_comma = true;
                }
            }
        }

        self.expect(b'}')?;
        Ok(Value::Object(entries.leak()))
    }

    fn skip_bom(&mut self) {
        if self.is_str("\u{feff}") {
            self.advance(3);
        }
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), ParseError> {
        loop {
            loop {
                if self.pos >= self.bytes.len() {
                    return Ok(());
                }
                match self.bytes[self.pos] {
                    b' ' | b'\t' | b'\n' | b'\r' => self.pos += 1,
                    _ => break,
                }
            }

            if self.is_str("//") {
                self.pos += 2;
                while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else if self.is_str("/*") {
                let start = self.pos;
                self.pos += 2;
                loop {
                    while self.pos < self.bytes.len() && self.bytes[self.pos] != b'*' {
                        self.pos += 1;
                    }
                    if self.pos >= self.bytes.len() {
                        return Err(self.fail(start, ParseErrorKind::Syntax));
                    }
                    if self.is_str("*/") {
                        self.pos += 2;
                        break;
                    }
                    self.pos += 1;
                }
            } else {
                return Ok(());
            }
        }
    }

    fn expect(&mut self, expected: u8) -> Result<(), ParseError> {
        if self.bytes.get(self.pos) == Some(&expected) {
            self.pos += 1;
            Ok(())
        } else {
            Err(self.fail(self.pos, ParseErrorKind::Syntax))
        }
    }

    fn expect_str(&mut self, expected: &str) -> Result<(), ParseError> {
        if self.is_str(expected) {
            self.pos += expected.len();
            Ok(())
        } else {
            Err(self.fail(self.pos, ParseErrorKind::Syntax))
        }
    }

    fn is_str(&self, expected: &str) -> bool {
        self.bytes.get(self.pos..self.pos + expected.len()) == Some(expected.as_bytes())
    }

    fn peek(&self) -> Option<char> {
        if self.pos < self.bytes.len() { Some(self.bytes[self.pos] as char) } else { None }
    }

    fn advance(&mut self, num: usize) {
        self.pos += num;
    }

    #[cold]
    fn fail(&self, pos: usize, kind: ParseErrorKind) -> ParseError {
        let mut cfg = MeasurementConfig::new(&self.bytes);
        let pos = cfg.goto_offset(pos);
        let line = pos.logical_pos.y.max(0) as usize + 1;
        let column = pos.logical_pos.x.max(0) as usize + 1;
        ParseError { kind, line, column }
    }
}

#[allow(non_snake_case)]
#[allow(clippy::invisible_characters)]
#[cfg(test)]
mod tests {
    use stdext::arena::scratch_arena;

    use super::*;

    #[test]
    fn test_null() {
        let scratch = scratch_arena(None);
        assert!(parse(&scratch, "null").unwrap().is_null());
    }

    #[test]
    fn test_bool() {
        let scratch = scratch_arena(None);
        assert_eq!(parse(&scratch, "true").unwrap().as_bool(), Some(true));
        assert_eq!(parse(&scratch, "false").unwrap().as_bool(), Some(false));
    }

    #[test]
    fn test_number() {
        let scratch = scratch_arena(None);
        assert_eq!(parse(&scratch, "0").unwrap().as_number(), Some(0.0));
        assert_eq!(parse(&scratch, "123").unwrap().as_number(), Some(123.0));
        assert_eq!(parse(&scratch, "-456").unwrap().as_number(), Some(-456.0));
        assert_eq!(parse(&scratch, "3.15").unwrap().as_number(), Some(3.15));
        assert_eq!(parse(&scratch, "1e10").unwrap().as_number(), Some(1e10));
        assert_eq!(parse(&scratch, "1.5e-3").unwrap().as_number(), Some(0.0015));
    }

    #[test]
    fn test_string() {
        let scratch = scratch_arena(None);
        assert_eq!(parse(&scratch, r#""hello""#).unwrap().as_str(), Some("hello"));
        assert_eq!(parse(&scratch, r#""hello\nworld""#).unwrap().as_str(), Some("hello\nworld"));
        assert_eq!(parse(&scratch, r#""\u0041\u0042\u0043""#).unwrap().as_str(), Some("ABC"));
    }

    #[test]
    fn test_array() {
        let scratch = scratch_arena(None);
        let value = parse(&scratch, "[1, 2, 3]").unwrap();
        let arr = value.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0].as_number(), Some(1.0));
        assert_eq!(arr[1].as_number(), Some(2.0));
        assert_eq!(arr[2].as_number(), Some(3.0));
    }

    #[test]
    fn test_object() {
        let scratch = scratch_arena(None);
        let value = parse(&scratch, r#"{"a": 1, "b": true}"#).unwrap();
        let obj = value.as_object().unwrap();
        assert_eq!(obj.get_number("a"), Some(1.0));
        assert_eq!(obj.get_bool("b"), Some(true));
    }

    #[test]
    fn test_comments() {
        let scratch = scratch_arena(None);
        let input = r#"{
            // Line comment
            "a": 1,
            /* Block comment */
            "b": 2
        }"#;
        let value = parse(&scratch, input).unwrap();
        let obj = value.as_object().unwrap();
        assert_eq!(obj.get_number("a"), Some(1.0));
        assert_eq!(obj.get_number("b"), Some(2.0));
    }

    #[test]
    fn test_trailing_comma() {
        let scratch = scratch_arena(None);
        assert!(parse(&scratch, "[1, 2, 3,]").is_ok());
        assert!(parse(&scratch, r#"{"a": 1,}"#).is_ok());
    }

    #[test]
    fn test_nested() {
        let scratch = scratch_arena(None);
        let input = r#"{
            "nested": {
                "array": [1, 2, {"deep": true}]
            }
        }"#;
        let value = parse(&scratch, input).unwrap();
        let obj = value.as_object().unwrap();
        let nested = obj.get_object("nested").unwrap();
        let array = nested.get_array("array").unwrap();
        assert_eq!(array.len(), 3);
        let deep_obj = array[2].as_object().unwrap();
        assert_eq!(deep_obj.get_bool("deep"), Some(true));
    }

    #[test]
    fn test_max_depth() {
        let scratch = scratch_arena(None);
        let mut input = String::new();
        for _ in 0..100 {
            input.push('[');
        }
        for _ in 0..100 {
            input.push(']');
        }
        assert!(parse(&scratch, &input).is_err());
    }

    #[test]
    fn test_invalid_json() {
        let scratch = scratch_arena(None);
        assert!(parse(&scratch, "").is_err());
        assert!(parse(&scratch, "{").is_err());
        assert!(parse(&scratch, r#"{"a":}"#).is_err());
        assert!(parse(&scratch, "[1, 2,").is_err());
        assert!(parse(&scratch, r#""unterminated"#).is_err());
    }

    #[test]
    fn test_control_chars() {
        let scratch = scratch_arena(None);
        // Control characters must be escaped
        assert!(parse(&scratch, "\"\x01\"").is_err());
    }

    #[test]
    fn test_unicode() {
        let scratch = scratch_arena(None);
        // Test emoji (surrogate pair)
        assert_eq!(parse(&scratch, r#""\uD83D\uDE00""#).unwrap().as_str(), Some("😀"));
        // Test regular unicode
        assert_eq!(parse(&scratch, r#""\u2764""#).unwrap().as_str(), Some("❤"));
    }
}
