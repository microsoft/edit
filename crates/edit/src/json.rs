// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A simple JSONC parser. It's designed for parsing our small settings files.

use std::fmt;

use stdext::arena::{Arena, ArenaString};

/// Maximum nesting depth to prevent stack overflow.
const MAX_DEPTH: usize = 64;

/// A JSON value.
#[derive(Debug, Clone)]
pub enum Value<'a> {
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Array(&'a [Value<'a>]),
    Object(&'a [ObjectEntry<'a>]),
}

/// An entry in a JSON object.
#[derive(Debug, Clone)]
pub struct ObjectEntry<'a> {
    pub key: &'a str,
    pub value: Value<'a>,
}

/// A parse error.
#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    position: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JSON parse error at position {}: {}", self.position, self.message)
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    fn new(message: impl Into<String>, position: usize) -> Self {
        Self { message: message.into(), position }
    }
}

impl<'a> Value<'a> {
    /// Returns the value as a boolean, if it is one.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns the value as a number, if it is one.
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    /// Returns the value as a string, if it is one.
    pub fn as_str(&self) -> Option<&'a str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the value as an array, if it is one.
    pub fn as_array(&'a self) -> Option<&'a [Value<'a>]> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Returns the value as an object, if it is one.
    pub fn as_object(&'a self) -> Option<Object<'a>> {
        match self {
            Value::Object(entries) => Some(Object { entries }),
            _ => None,
        }
    }

    /// Returns true if the value is null.
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
}

/// A JSON object with convenient lookup methods.
#[derive(Debug, Clone, Copy)]
pub struct Object<'a> {
    entries: &'a [ObjectEntry<'a>],
}

impl<'a> Object<'a> {
    /// Gets the value associated with a key.
    pub fn get(&self, key: &str) -> Option<&'a Value<'a>> {
        self.entries.iter().find(|e| e.key == key).map(|e| &e.value)
    }

    /// Gets a boolean value.
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        self.get(key).and_then(Value::as_bool)
    }

    /// Gets a number value.
    pub fn get_number(&self, key: &str) -> Option<f64> {
        self.get(key).and_then(Value::as_number)
    }

    /// Gets a string value.
    pub fn get_str(&self, key: &str) -> Option<&'a str> {
        self.get(key).and_then(Value::as_str)
    }

    /// Gets an array value.
    pub fn get_array(&self, key: &str) -> Option<&'a [Value<'a>]> {
        self.get(key).and_then(Value::as_array)
    }

    /// Gets an object value.
    pub fn get_object(&self, key: &str) -> Option<Object<'a>> {
        self.get(key).and_then(Value::as_object)
    }

    /// Returns an iterator over the entries.
    pub fn iter(&self) -> impl Iterator<Item = &'a ObjectEntry<'a>> {
        self.entries.iter()
    }

    /// Returns the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns true if the object is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Parses a JSONC string into a Value.
pub fn parse<'a>(arena: &'a Arena, input: &'a str) -> Result<Value<'a>, ParseError> {
    let mut parser = Parser::new(arena, input);
    let value = parser.parse_value(0)?;
    parser.skip_whitespace_and_comments()?;
    if parser.pos < parser.input.len() {
        return Err(ParseError::new("Unexpected data after JSON value", parser.pos));
    }
    Ok(value)
}

struct Parser<'a> {
    arena: &'a Arena,
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(arena: &'a Arena, input: &'a str) -> Self {
        Self { arena, input, pos: 0 }
    }

    fn parse_value(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        // Prevent stack overflow from deeply nested structures
        if depth >= MAX_DEPTH {
            return Err(ParseError::new("Maximum nesting depth exceeded", self.pos));
        }

        self.skip_whitespace_and_comments()?;

        let ch = self.peek().ok_or_else(|| ParseError::new("Unexpected end of input", self.pos))?;

        match ch {
            'n' => self.parse_null(),
            't' | 'f' => self.parse_bool(),
            '"' => self.parse_string(),
            '[' => self.parse_array(depth),
            '{' => self.parse_object(depth),
            '-' | '0'..='9' => self.parse_number(),
            _ => Err(ParseError::new(format!("Unexpected character '{}'", ch), self.pos)),
        }
    }

    fn parse_null(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect_str("null")?;
        Ok(Value::Null)
    }

    fn parse_bool(&mut self) -> Result<Value<'a>, ParseError> {
        if self.peek() == Some('t') {
            self.expect_str("true")?;
            Ok(Value::Bool(true))
        } else {
            self.expect_str("false")?;
            Ok(Value::Bool(false))
        }
    }

    fn parse_number(&mut self) -> Result<Value<'a>, ParseError> {
        let start = self.pos;

        // Optional minus
        if self.peek() == Some('-') {
            self.advance();
        }

        // Integer part
        if self.peek() == Some('0') {
            self.advance();
        } else if matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        } else {
            return Err(ParseError::new("Invalid number", self.pos));
        }

        // Fractional part
        if self.peek() == Some('.') {
            self.advance();
            if !matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                return Err(ParseError::new("Invalid number: expected digit after '.'", self.pos));
            }
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        }

        // Exponent part
        if matches!(self.peek(), Some('e') | Some('E')) {
            self.advance();
            if matches!(self.peek(), Some('+') | Some('-')) {
                self.advance();
            }
            if !matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                return Err(ParseError::new(
                    "Invalid number: expected digit in exponent",
                    self.pos,
                ));
            }
            while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        }

        let num_str = &self.input[start..self.pos];
        let num = num_str.parse::<f64>().map_err(|_| ParseError::new("Invalid number", start))?;

        // Check for infinity/NaN which could be problematic
        if !num.is_finite() {
            return Err(ParseError::new("Number overflow", start));
        }

        Ok(Value::Number(num))
    }

    fn parse_string(&mut self) -> Result<Value<'a>, ParseError> {
        self.expect('"')?;

        let mut result = ArenaString::new_in(self.arena);
        let mut has_escapes = false;

        loop {
            let ch = self.next().ok_or_else(|| ParseError::new("Unterminated string", self.pos))?;

            match ch {
                '"' => break,
                '\\' => {
                    has_escapes = true;
                    let escaped = self
                        .next()
                        .ok_or_else(|| ParseError::new("Unterminated string escape", self.pos))?;
                    match escaped {
                        '"' => result.push('"'),
                        '\\' => result.push('\\'),
                        '/' => result.push('/'),
                        'b' => result.push('\x08'),
                        'f' => result.push('\x0C'),
                        'n' => result.push('\n'),
                        'r' => result.push('\r'),
                        't' => result.push('\t'),
                        'u' => {
                            let code = self.parse_unicode_escape()?;
                            result.push(code);
                        }
                        _ => {
                            return Err(ParseError::new(
                                format!("Invalid escape sequence '\\{}'", escaped),
                                self.pos - 2,
                            ));
                        }
                    }
                }
                '\x00'..='\x1F' => {
                    return Err(ParseError::new(
                        "Unescaped control character in string",
                        self.pos - 1,
                    ));
                }
                _ => {
                    if !has_escapes {
                        // Fast path: if we haven't seen any escapes yet, we might be able
                        // to just return a slice of the input
                        let start = self.pos - ch.len_utf8();
                        let end = self.find_string_end()?;
                        if end.is_some() {
                            // find_string_end leaves pos at the closing quote
                            let slice = &self.input[start..self.pos];
                            self.expect('"')?;
                            return Ok(Value::String(slice));
                        }
                        // If we hit an escape or the end, fall back to the slow path
                        result.push_str(&self.input[start..self.pos]);
                    } else {
                        result.push(ch);
                    }
                }
            }
        }

        // Copy the string data to arena to ensure proper lifetime
        Ok(Value::String(result.leak()))
    }

    // Fast path helper: scan ahead to see if the rest of the string has no escapes
    fn find_string_end(&mut self) -> Result<Option<()>, ParseError> {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            match ch {
                '"' => return Ok(Some(())),
                '\\' | '\x00'..='\x1F' => {
                    self.pos = start;
                    return Ok(None);
                }
                _ => self.advance(),
            }
        }
        Err(ParseError::new("Unterminated string", self.pos))
    }

    fn parse_unicode_escape(&mut self) -> Result<char, ParseError> {
        let mut code = 0u32;
        for _ in 0..4 {
            let ch = self
                .next()
                .ok_or_else(|| ParseError::new("Incomplete unicode escape", self.pos))?;
            let digit = ch
                .to_digit(16)
                .ok_or_else(|| ParseError::new("Invalid unicode escape", self.pos - 1))?;
            code = code * 16 + digit;
        }

        // Handle UTF-16 surrogate pairs
        if (0xD800..=0xDBFF).contains(&code) {
            // High surrogate
            if self.peek() == Some('\\') {
                let pos = self.pos;
                self.advance();
                if self.peek() == Some('u') {
                    self.advance();
                    let mut low = 0u32;
                    for _ in 0..4 {
                        let ch = self.next().ok_or_else(|| {
                            ParseError::new("Incomplete unicode escape", self.pos)
                        })?;
                        let digit = ch.to_digit(16).ok_or_else(|| {
                            ParseError::new("Invalid unicode escape", self.pos - 1)
                        })?;
                        low = low * 16 + digit;
                    }

                    if (0xDC00..=0xDFFF).contains(&low) {
                        // Valid surrogate pair
                        code = 0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00);
                    } else {
                        return Err(ParseError::new("Invalid surrogate pair", pos));
                    }
                } else {
                    self.pos = pos;
                }
            }
        } else if (0xDC00..=0xDFFF).contains(&code) {
            // Low surrogate without high surrogate
            return Err(ParseError::new("Invalid surrogate pair", self.pos - 6));
        }

        char::from_u32(code).ok_or_else(|| ParseError::new("Invalid unicode code point", self.pos))
    }

    fn parse_array(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        self.expect('[')?;
        self.skip_whitespace_and_comments()?;

        if self.peek() == Some(']') {
            self.advance();
            return Ok(Value::Array(&[]));
        }

        let mut values = Vec::new_in(self.arena);

        loop {
            let value = self.parse_value(depth + 1)?;
            values.push(value);

            self.skip_whitespace_and_comments()?;

            match self.peek() {
                Some(',') => {
                    self.advance();
                    self.skip_whitespace_and_comments()?;
                    // Allow trailing comma before ]
                    if self.peek() == Some(']') {
                        break;
                    }
                }
                Some(']') => break,
                Some(ch) => {
                    return Err(ParseError::new(
                        format!("Expected ',' or ']', found '{}'", ch),
                        self.pos,
                    ));
                }
                None => return Err(ParseError::new("Unterminated array", self.pos)),
            }
        }

        self.expect(']')?;
        Ok(Value::Array(values.leak()))
    }

    fn parse_object(&mut self, depth: usize) -> Result<Value<'a>, ParseError> {
        self.expect('{')?;
        self.skip_whitespace_and_comments()?;

        if self.peek() == Some('}') {
            self.advance();
            return Ok(Value::Object(&[]));
        }

        let mut entries = Vec::new_in(self.arena);

        loop {
            self.skip_whitespace_and_comments()?;

            // Parse key
            if self.peek() != Some('"') {
                return Err(ParseError::new("Expected string key", self.pos));
            }
            let key = match self.parse_string()? {
                Value::String(s) => s,
                _ => unreachable!(),
            };

            self.skip_whitespace_and_comments()?;
            self.expect(':')?;

            let value = self.parse_value(depth + 1)?;
            entries.push(ObjectEntry { key, value });

            self.skip_whitespace_and_comments()?;

            match self.peek() {
                Some(',') => {
                    self.advance();
                    self.skip_whitespace_and_comments()?;
                    // Allow trailing comma before }
                    if self.peek() == Some('}') {
                        break;
                    }
                }
                Some('}') => break,
                Some(ch) => {
                    return Err(ParseError::new(
                        format!("Expected ',' or '}}', found '{}'", ch),
                        self.pos,
                    ));
                }
                None => return Err(ParseError::new("Unterminated object", self.pos)),
            }
        }

        self.expect('}')?;
        Ok(Value::Object(entries.leak()))
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), ParseError> {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                    self.advance();
                }
                Some('/') => {
                    let pos = self.pos;
                    self.advance();
                    match self.peek() {
                        Some('/') => {
                            // Line comment
                            self.advance();
                            while matches!(self.peek(), Some(c) if c != '\n') {
                                self.advance();
                            }
                        }
                        Some('*') => {
                            // Block comment
                            self.advance();
                            let mut closed = false;
                            while let Some(ch) = self.next() {
                                if ch == '*' && self.peek() == Some('/') {
                                    self.advance();
                                    closed = true;
                                    break;
                                }
                            }
                            if !closed {
                                return Err(ParseError::new("Unterminated block comment", pos));
                            }
                        }
                        _ => {
                            // Not a comment, backtrack
                            self.pos = pos;
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn expect(&mut self, expected: char) -> Result<(), ParseError> {
        match self.next() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(ParseError::new(
                format!("Expected '{}', found '{}'", expected, ch),
                self.pos - 1,
            )),
            None => Err(ParseError::new(
                format!("Expected '{}', found end of input", expected),
                self.pos,
            )),
        }
    }

    fn expect_str(&mut self, expected: &str) -> Result<(), ParseError> {
        let start = self.pos;
        for expected_ch in expected.chars() {
            match self.next() {
                Some(ch) if ch == expected_ch => {}
                _ => return Err(ParseError::new(format!("Expected '{}'", expected), start)),
            }
        }
        Ok(())
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn advance(&mut self) {
        if let Some(ch) = self.peek() {
            self.pos += ch.len_utf8();
        }
    }
}

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
