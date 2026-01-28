// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Regex -> IR Compiler
//!
//! This module compiles regex patterns into IR instructions.
//! Ideally this would use a proper TNFA → TDFA(1) compiler, but that's really complex.
//! We get by with a dead simple translation, because we have:
//!
//! - No backreferences
//! - No lookahead/lookbehind (except `\>` word boundary)
//! - Greedy matching only (no lazy quantifiers)
//! - Implicit `^`
//!
//! The code generator uses continuation-passing style (CPS): each `emit()` call receives
//! two continuation nodes (`on_match` and `on_fail`) and returns the entry node for that
//! subpattern. This means we build the IR graph **backwards** - we must know where to
//! jump on success/failure before we can emit the current node.
//!
//! This reverse iteration has a side effect on capture groups: they get pushed onto the
//! captures list in reverse order. We fix this with `captures.reverse()` at the end.
//!
//! # Supported Patterns
//!
//! | Pattern      | IR Translation                                           |
//! |--------------|----------------------------------------------------------|
//! | `foo`        | `Prefix("foo")` - single prefix check                    |
//! | `\+\+\+`     | `Prefix("+++")` - escapes fused into literals            |
//! | `(?i:foo)`   | `PrefixInsensitive("foo")`                               |
//! | `[a-z]+`     | `Charset{cs, min=1, max=∞}` - greedy char class          |
//! | `[a-z]?`     | `Charset{cs, min=0, max=1}` - optional char              |
//! | `$`          | `EndOfLine` condition                                    |
//! | `.*`         | `MovImm off, MAX` - skip to end of line                  |
//! | `\>`         | `If Charset(\w) then FAIL else MATCH` - word boundary    |
//! | `(foo)`      | Wraps inner with `Mov` to save start/end positions       |
//!
//! # Gotchas
//!
//! - `\w` includes bytes 0xC2-0xF4 (UTF-8 leading bytes) so that it can consume multibyte characters.
//!   This isn't Unicode-correct but works for identifiers in most programming languages.
//! - We don't create loops to keep the IR generation and optimization simple.
//!   This means that e.g. (a|b)+ is not supported. For now that's fine.
//! - The `parse()` function wires its generated IR into the provided destination nodes.
//!   Don't pass nodes that are already part of the IR graph.

use super::*;

// 0xC2-0xF4 are UTF-8 leading bytes for multibyte sequences. Including them lets
// `\w+` consume entire multibyte characters, which is important for identifiers
// containing non-ASCII letters (e.g., `naïve`, `café`).
const ASCII_WORD_CHARSET: Charset = {
    let mut charset = Charset::no();
    charset.set_range(b'0'..=b'9', true);
    charset.set_range(b'A'..=b'Z', true);
    charset.set_range(b'_'..=b'_', true);
    charset.set_range(b'a'..=b'z', true);
    charset.set_range(0xC2..=0xF4, true);
    charset
};

// Whitespace character set for `\s`
const ASCII_WHITESPACE_CHARSET: Charset = {
    let mut charset = Charset::no();
    charset.set(b' ', true);
    charset.set(b'\t', true);
    charset.set(b'\n', true);
    charset.set(b'\r', true);
    charset.set(0x0B, true); // vertical tab
    charset.set(0x0C, true); // form feed
    charset
};

// Digit character set for `\d`
const ASCII_DIGIT_CHARSET: Charset = {
    let mut charset = Charset::no();
    charset.set_range(b'0'..=b'9', true);
    charset
};

pub type CaptureList<'a> = Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>;

#[derive(Debug, Clone)]
enum Regex {
    /// Empty input
    Empty,
    /// `foo`
    Literal(String, bool), // (string, case_insensitive)
    /// `[a-z]`
    CharClass(Charset),
    /// `[a-z][0-9]`
    Concat(Vec<Regex>),
    /// `a|b|c`
    Alt(Vec<Regex>),
    /// `?`, `+`, `*`, `{n,m}`
    Repeat {
        inner: Box<Regex>,
        min: u32,
        max: u32, // u32::MAX means unbounded
    },
    /// `(foo)`, `(?:foo)`
    Group { inner: Box<Regex>, capturing: bool },
    /// `$`
    EndOfLine,
    /// `\>`
    WordEnd,
    /// `.`
    Dot,
}

struct RegexParser<'a> {
    input: &'a str,
    pos: usize,
    case_insensitive: bool,
}

impl<'a> RegexParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0, case_insensitive: false }
    }

    fn parse(mut self) -> Result<Regex, String> {
        let result = self.parse_alternation()?;
        if self.pos < self.input.len() {
            return Err(format!(
                "unexpected character '{}' at position {}",
                self.peek().unwrap(),
                self.pos
            ));
        }
        Ok(result)
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn expect(&mut self, expected: char) -> Result<(), String> {
        match self.advance() {
            Some(c) if c == expected => Ok(()),
            Some(c) => {
                Err(format!("expected '{}', found '{}' at position {}", expected, c, self.pos))
            }
            None => Err(format!("expected '{}', found end of pattern", expected)),
        }
    }

    /// a|b|c
    fn parse_alternation(&mut self) -> Result<Regex, String> {
        let mut alts = vec![self.parse_concatenation()?];

        while self.peek() == Some('|') {
            self.advance();
            alts.push(self.parse_concatenation()?);
        }

        if alts.len() == 1 { Ok(alts.pop().unwrap()) } else { Ok(Regex::Alt(alts)) }
    }

    /// [a-b][c-d]
    fn parse_concatenation(&mut self) -> Result<Regex, String> {
        let mut parts = Vec::new();

        while let Some(c) = self.peek() {
            // Stop at alternation or group end
            if c == '|' || c == ')' {
                break;
            }

            parts.push(self.parse_quantified()?);
        }

        match parts.len() {
            0 => Ok(Regex::Empty),
            1 => Ok(parts.pop().unwrap()),
            _ => Ok(Regex::Concat(parts)),
        }
    }

    /// a?, a*, a+, a{n,m}
    fn parse_quantified(&mut self) -> Result<Regex, String> {
        let base = self.parse_atom()?;

        let (min, max) = match self.peek() {
            Some('?') => {
                self.advance();
                (0, 1)
            }
            Some('*') => {
                self.advance();
                (0, u32::MAX)
            }
            Some('+') => {
                self.advance();
                (1, u32::MAX)
            }
            Some('{') => self.parse_repetition_bounds()?,
            _ => return Ok(base),
        };

        Ok(Regex::Repeat { inner: Box::new(base), min, max })
    }

    /// {n,m}
    fn parse_repetition_bounds(&mut self) -> Result<(u32, u32), String> {
        self.expect('{')?;

        let min = self.parse_number()?;

        let max = if self.peek() == Some(',') {
            self.advance();
            if self.peek() == Some('}') { u32::MAX } else { self.parse_number()? }
        } else {
            min
        };

        self.expect('}')?;
        Ok((min, max))
    }

    fn parse_number(&mut self) -> Result<u32, String> {
        let start = self.pos;
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }
        if start == self.pos {
            return Err("expected number".to_string());
        }
        self.input[start..self.pos].parse().map_err(|e| format!("invalid number: {}", e))
    }

    /// Parse a single atom (literal, class, group, anchor)
    fn parse_atom(&mut self) -> Result<Regex, String> {
        match self.peek() {
            None => Ok(Regex::Empty),
            Some('(') => self.parse_group(),
            Some('[') => self.parse_char_class(),
            Some('.') => {
                self.advance();
                Ok(Regex::Dot)
            }
            Some('$') => {
                self.advance();
                Ok(Regex::EndOfLine)
            }
            _ => self.parse_literal(),
        }
    }

    /// Parse a literal string, including escaped metacharacters like `\+`, `\*`, etc.
    ///
    /// This function is responsible for a critical optimization: fusing consecutive escaped
    /// metacharacters into a single literal. For example, `\+\+\+` becomes `Literal("+++")`.
    fn parse_literal(&mut self) -> Result<Regex, String> {
        let mut lit = String::new();

        loop {
            match self.peek() {
                Some('\\') => {
                    let escape_char = self.input[self.pos + 1..].chars().next();
                    match escape_char {
                        // Character classes
                        Some('w' | 'W' | 'd' | 'D' | 's' | 'S') => {
                            if lit.is_empty() {
                                // Start with the class
                                self.advance(); // consume '\'
                                return self.parse_escape_as_regex();
                            } else {
                                // Return accumulated literal, leave escape for next parse
                                break;
                            }
                        }
                        // Word boundary
                        Some('>') => {
                            if lit.is_empty() {
                                self.advance(); // consume '\'
                                self.advance(); // consume '>'
                                return Ok(Regex::WordEnd);
                            } else {
                                break;
                            }
                        }
                        // Simple escape
                        Some(c) if !c.is_ascii_alphanumeric() => {
                            // Check if this char would be quantified
                            let after_escape = self.pos + 1 + c.len_utf8();
                            if after_escape < self.input.len() && !lit.is_empty() {
                                let next = self.input[after_escape..].chars().next();
                                if matches!(next, Some('?' | '*' | '+' | '{')) {
                                    break;
                                }
                            }
                            self.advance(); // consume '\'
                            self.advance(); // consume escaped char
                            lit.push(c);
                        }
                        Some(c) => {
                            return Err(format!("unknown escape sequence '\\{}'", c));
                        }
                        None => {
                            return Err("unexpected end of pattern after backslash".to_string());
                        }
                    }
                }
                Some(c) if !is_meta_char(c) => {
                    let next_pos = self.pos + c.len_utf8();
                    if next_pos < self.input.len() && !lit.is_empty() {
                        let next = self.input[next_pos..].chars().next();
                        if matches!(next, Some('?' | '*' | '+' | '{')) {
                            break;
                        }
                    }
                    lit.push(c);
                    self.advance();
                }
                _ => break,
            }
        }

        // We couldn't parse anything - must be an unexpected meta character
        if lit.is_empty() {
            return Err(format!(
                "unexpected character '{}' at position {}",
                self.peek().unwrap_or('\0'),
                self.pos
            ));
        }

        Ok(Regex::Literal(lit, self.case_insensitive))
    }

    /// \w, \d, etc.
    fn parse_escape_as_regex(&mut self) -> Result<Regex, String> {
        match self.advance() {
            Some('w') => Ok(Regex::CharClass(ASCII_WORD_CHARSET)),
            Some('W') => {
                let mut cs = ASCII_WORD_CHARSET;
                cs.invert();
                Ok(Regex::CharClass(cs))
            }
            Some('d') => Ok(Regex::CharClass(ASCII_DIGIT_CHARSET)),
            Some('D') => {
                let mut cs = ASCII_DIGIT_CHARSET;
                cs.invert();
                Ok(Regex::CharClass(cs))
            }
            Some('s') => Ok(Regex::CharClass(ASCII_WHITESPACE_CHARSET)),
            Some('S') => {
                let mut cs = ASCII_WHITESPACE_CHARSET;
                cs.invert();
                Ok(Regex::CharClass(cs))
            }
            Some(c) => Err(format!("unknown escape sequence '\\{}'", c)),
            None => Err("unexpected end of pattern after backslash".to_string()),
        }
    }

    /// (foo), (?:foo), (?i:foo)
    fn parse_group(&mut self) -> Result<Regex, String> {
        self.expect('(')?;

        // Check for special group types
        if self.peek() == Some('?') {
            self.advance();
            match self.peek() {
                Some(':') => {
                    // Non-capturing (?:...)
                    self.advance();
                    let inner = self.parse_alternation()?;
                    self.expect(')')?;
                    Ok(Regex::Group { inner: Box::new(inner), capturing: false })
                }
                Some('i') => {
                    // Case-insensitive (?i:...)
                    self.advance();
                    self.expect(':')?;
                    let old_ci = self.case_insensitive;
                    self.case_insensitive = true;
                    let inner = self.parse_alternation()?;
                    self.case_insensitive = old_ci;
                    self.expect(')')?;
                    Ok(Regex::Group { inner: Box::new(inner), capturing: false })
                }
                _ => Err("unsupported group modifier".to_string()),
            }
        } else {
            // Capturing (...)
            let inner = self.parse_alternation()?;
            self.expect(')')?;
            Ok(Regex::Group { inner: Box::new(inner), capturing: true })
        }
    }

    /// [a-z], [^a-z], etc.
    fn parse_char_class(&mut self) -> Result<Regex, String> {
        self.expect('[')?;

        let negated = if self.peek() == Some('^') {
            self.advance();
            true
        } else {
            false
        };

        let mut charset = Charset::no();

        // First char can be ] or - literally
        if self.peek() == Some(']') {
            charset.set(b']', true);
            self.advance();
        }

        while let Some(c) = self.peek() {
            if c == ']' {
                self.advance();
                break;
            }

            if c == '\\' {
                self.advance();
                let escaped = self.parse_escape_char()?;
                match escaped {
                    EscapedChar::Char(b) => charset.set(b, true),
                    EscapedChar::Class(cs) => charset.merge(&cs),
                }
            } else {
                let start = c as u8;
                self.advance();

                // Check for range
                if self.peek() == Some('-') && !self.input[self.pos + 1..].starts_with(']') {
                    self.advance(); // consume -
                    let end = match self.peek() {
                        Some('\\') => {
                            self.advance();
                            match self.parse_escape_char()? {
                                EscapedChar::Char(b) => b,
                                EscapedChar::Class(_) => {
                                    return Err("cannot use character class in range".to_string());
                                }
                            }
                        }
                        Some(c) => {
                            self.advance();
                            c as u8
                        }
                        None => {
                            return Err("unexpected end of pattern in character class".to_string());
                        }
                    };
                    charset.set_range(start..=end, true);
                } else {
                    charset.set(start, true);
                }
            }
        }

        if negated {
            charset.invert();
        }

        Ok(Regex::CharClass(charset))
    }

    fn parse_escape_char(&mut self) -> Result<EscapedChar, String> {
        match self.advance() {
            Some('w') => Ok(EscapedChar::Class(ASCII_WORD_CHARSET)),
            Some('W') => {
                let mut cs = ASCII_WORD_CHARSET;
                cs.invert();
                Ok(EscapedChar::Class(cs))
            }
            Some('d') => Ok(EscapedChar::Class(ASCII_DIGIT_CHARSET)),
            Some('D') => {
                let mut cs = ASCII_DIGIT_CHARSET;
                cs.invert();
                Ok(EscapedChar::Class(cs))
            }
            Some('s') => Ok(EscapedChar::Class(ASCII_WHITESPACE_CHARSET)),
            Some('S') => {
                let mut cs = ASCII_WHITESPACE_CHARSET;
                cs.invert();
                Ok(EscapedChar::Class(cs))
            }
            Some(c) if !c.is_ascii_alphanumeric() => Ok(EscapedChar::Char(c as u8)),
            Some(c) => Err(format!("unknown escape sequence '\\{}'", c)),
            None => Err("unexpected end of pattern after backslash".to_string()),
        }
    }
}

enum EscapedChar {
    Char(u8),
    Class(Charset),
}

fn is_meta_char(c: char) -> bool {
    matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | '|' | '?' | '*' | '+' | '.' | '^' | '$')
}

struct CodeGen<'a, 'c> {
    compiler: &'c mut Compiler<'a>,
    captures: CaptureList<'a>,
    dst_good: IRCell<'a>,
    dst_bad: IRCell<'a>,
}

impl<'a, 'c> CodeGen<'a, 'c> {
    fn new(compiler: &'c mut Compiler<'a>, dst_good: IRCell<'a>, dst_bad: IRCell<'a>) -> Self {
        let captures = CaptureList::new_in(compiler.arena);
        Self { compiler, captures, dst_good, dst_bad }
    }

    fn generate(&mut self, regex: &Regex) -> Result<IRCell<'a>, String> {
        self.emit(regex, self.dst_good, self.dst_bad)
    }

    /// Core emission function. Returns the entry node for matching `regex`.
    ///
    /// The generated IR forms a DAG where:
    /// - Matching the pattern leads to `on_match`
    /// - Failing to match leads to `on_fail`
    ///
    /// For `IRI::If` nodes: `then` = match branch, `next` = fail branch.
    fn emit(
        &mut self,
        regex: &Regex,
        on_match: IRCell<'a>,
        on_fail: IRCell<'a>,
    ) -> Result<IRCell<'a>, String> {
        match regex {
            Regex::Empty => Ok(on_match),

            Regex::Literal(s, case_insensitive) => {
                if s.is_empty() {
                    return Ok(on_match);
                }
                let s = self.compiler.intern_string(s);
                let condition = if *case_insensitive {
                    Condition::PrefixInsensitive(s)
                } else {
                    Condition::Prefix(s)
                };
                let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });
                if_node.borrow_mut().next = Some(on_fail);
                Ok(if_node)
            }

            Regex::CharClass(cs) => {
                let cs = self.compiler.intern_charset(cs);
                let condition = Condition::Charset { cs, min: 1, max: u32::MAX };
                let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });
                if_node.borrow_mut().next = Some(on_fail);
                Ok(if_node)
            }

            Regex::Dot => {
                let cs = Charset::yes();
                let cs = self.compiler.intern_charset(&cs);
                let condition = Condition::Charset { cs, min: 1, max: 1 };
                let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });
                if_node.borrow_mut().next = Some(on_fail);
                Ok(if_node)
            }

            Regex::EndOfLine => {
                let if_node = self
                    .compiler
                    .alloc_iri(IRI::If { condition: Condition::EndOfLine, then: on_match });
                if_node.borrow_mut().next = Some(on_fail);
                Ok(if_node)
            }

            Regex::WordEnd => {
                // \> is a zero-width assertion: succeeds if NOT followed by a word char.
                // We invert the logic: check for word char, swap success/failure branches.
                let cs = self.compiler.intern_charset(&ASCII_WORD_CHARSET);
                let condition = Condition::Charset { cs, min: 1, max: 1 };
                let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_fail });
                if_node.borrow_mut().next = Some(on_match);
                Ok(if_node)
            }

            Regex::Concat(parts) => {
                let mut current_target = on_match;

                // We iterate in reverse because of continuation-passing style,
                // as explained in the module doc.
                for part in parts.iter().rev() {
                    current_target = self.emit(part, current_target, on_fail)?;
                }

                Ok(current_target)
            }

            Regex::Alt(alts) => {
                let mut current_fail = on_fail;

                // We iterate in reverse because of continuation-passing style,
                // as explained in the module doc.
                for alt in alts.iter().rev() {
                    current_fail = self.emit(alt, on_match, current_fail)?;
                }

                Ok(current_fail)
            }

            Regex::Repeat { inner, min, max } => {
                self.emit_repeat(inner, *min, *max, on_match, on_fail)
            }

            Regex::Group { inner, capturing } => {
                if *capturing {
                    // Capturing group: wrap inner pattern with Mov instructions to save
                    // the start and end positions of the matched substring.
                    let start_reg = self.compiler.alloc_vreg();
                    let end_reg = self.compiler.alloc_vreg();

                    let off_reg = self.compiler.get_reg(Register::InputOffset);
                    let save_end = self.compiler.alloc_iri(IRI::Mov { dst: end_reg, src: off_reg });
                    save_end.borrow_mut().next = Some(on_match);

                    let inner_node = self.emit(inner, save_end, on_fail)?;

                    // Push *after* emit, so nested groups come first in the reversed list.
                    self.captures.push((start_reg, end_reg));

                    let save_start =
                        self.compiler.alloc_iri(IRI::Mov { dst: start_reg, src: off_reg });
                    save_start.borrow_mut().next = Some(inner_node);

                    Ok(save_start)
                } else {
                    self.emit(inner, on_match, on_fail)
                }
            }
        }
    }

    /// Emit IR for repetition quantifiers (`?`, `+`, `*`, `{n,m}`).
    ///
    /// # Why no loops?
    ///
    /// Creating IR loops (self-referential nodes) would complicate the optimizer.
    /// Since no LSH file needs unbounded repetition on complex patterns (`(foo)+`), we simply reject them.
    fn emit_repeat(
        &mut self,
        inner: &Regex,
        min: u32,
        max: u32,
        on_match: IRCell<'a>,
        on_fail: IRCell<'a>,
    ) -> Result<IRCell<'a>, String> {
        // `.*` = skip to end of line. Very common pattern, special-cased for speed.
        if min == 0 && max == u32::MAX && matches!(*inner, Regex::Dot) {
            let off_reg = self.compiler.get_reg(Register::InputOffset);
            let skip_node = self.compiler.alloc_iri(IRI::MovImm { dst: off_reg, imm: u32::MAX });
            skip_node.borrow_mut().next = Some(on_match);
            return Ok(skip_node);
        }

        // `.+` = one or more of any char.
        if min >= 1 && max == u32::MAX && matches!(*inner, Regex::Dot) {
            let cs = Charset::yes();
            return self.emit_charset(&cs, min, max, on_match, on_fail);
        }

        // CharClass: delegate to emit_charset which uses the VM's native min/max support.
        if let Regex::CharClass(ref cs) = *inner {
            return self.emit_charset(cs, min, max, on_match, on_fail);
        }

        // Single-char literal like `#+`: convert to charset for efficient handling.
        if let Regex::Literal(ref s, case_insensitive) = *inner
            && s.len() == 1
        {
            let b = s.as_bytes()[0];
            let mut cs = Charset::no();
            if case_insensitive {
                cs.set(b.to_ascii_lowercase(), true);
                cs.set(b.to_ascii_uppercase(), true);
            } else {
                cs.set(b, true);
            }
            return self.emit_charset(&cs, min, max, on_match, on_fail);
        }

        // Reject unbounded repetition on anything else - would need loops.
        if max == u32::MAX {
            return Err(
                "unbounded repetition on complex patterns are not yet supported (would require loops)"
                    .to_string(),
            );
        }

        // Bounded repetition: unroll. `x{2,4}` gets translated to `x x x? x?`.
        // We emit in reverse order (continuation-passing style),
        // so optional matches come first, then required matches.
        let mut current = on_match;
        // Optional: Both branches succeed go to `current`.
        for _ in min..max {
            current = self.emit(inner, current, current)?;
        }
        // Required: Failure goes to `on_fail`.
        for _ in 0..min {
            current = self.emit(inner, current, on_fail)?;
        }
        Ok(current)
    }

    fn emit_charset(
        &mut self,
        cs: &Charset,
        min: u32,
        max: u32,
        on_match: IRCell<'a>,
        on_fail: IRCell<'a>,
    ) -> Result<IRCell<'a>, String> {
        let cs = self.compiler.intern_charset(cs);
        let condition = Condition::Charset { cs, min, max };
        let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });

        // min=0 implies that it cannot fail. Remove `on_fail` to allow for later optimizations.
        if_node.borrow_mut().next = Some(if min == 0 { on_match } else { on_fail });

        Ok(if_node)
    }
}

/// Parse a regex pattern and generate IR that matches it.
///
/// The generated IR is wired to `dst_good` on successful match and `dst_bad` on failure.
/// The returned tuple contains the start of the IR graph and a list of capture group ranges.
pub fn parse<'a>(
    compiler: &mut Compiler<'a>,
    pattern: &str,
    dst_good: IRCell<'a>,
    dst_bad: IRCell<'a>,
) -> Result<(IRCell<'a>, CaptureList<'a>), String> {
    let parser = RegexParser::new(pattern);
    let regex = parser.parse()?;

    let mut codegen = CodeGen::new(compiler, dst_good, dst_bad);
    let entry = codegen.generate(&regex)?;

    // Reverse captures: Concat iterates in reverse, so groups are pushed in reverse order.
    codegen.captures.reverse();

    Ok((entry, codegen.captures))
}
