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

use std::slice;

use stdext::collections::BVec;

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

pub type CaptureList<'a> = BVec<'a, (IRRegCell<'a>, IRRegCell<'a>)>;

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

enum Atom {
    Empty,
    Meta(char),
    Char(char),
    WordEnd,
    Class(Charset),
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

    fn rest(&self) -> &'a str {
        &self.input[self.pos..]
    }

    fn peek(&self) -> Option<char> {
        self.rest().chars().next()
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
        let base = self.parse_primary()?;

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
    fn parse_primary(&mut self) -> Result<Regex, String> {
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
        let mut prev_atom_lit_len = 0;
        let mut prev_atom_pos = self.pos;

        loop {
            let start = self.pos;
            match self.parse_atom()? {
                Atom::Meta('?' | '*' | '+' | '{') => {
                    // Quantifiers apply to the preceding atom, so we need to pop
                    // the last atom (= char / escape char) and stop parsing.
                    if prev_atom_lit_len == 0 {
                        self.pos = start;
                    } else {
                        lit.truncate(prev_atom_lit_len);
                        self.pos = prev_atom_pos;
                    }
                    break;
                }
                Atom::Char(c) => {
                    prev_atom_lit_len = lit.len();
                    prev_atom_pos = start;
                    lit.push(c);
                }
                Atom::WordEnd if lit.is_empty() => {
                    return Ok(Regex::WordEnd);
                }
                Atom::Class(cs) if lit.is_empty() => {
                    return Ok(Regex::CharClass(cs));
                }
                _ => {
                    self.pos = start;
                    break;
                }
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
        fn unexpected_end() -> Result<Regex, String> {
            Err("unexpected end of pattern in character class".to_string())
        }
        fn unexpected_unicode(c: char) -> Result<Regex, String> {
            Err(format!("non-ASCII character '{c:?}' not supported in character class"))
        }
        fn unexpected_class() -> Result<Regex, String> {
            Err("cannot use character class in character class range".to_string())
        }
        fn invalid_range(start: u8, end: u8) -> Result<Regex, String> {
            Err(format!("invalid character range {:?}-{:?}", start as char, end as char))
        }

        self.expect('[')?;

        let negated = if self.peek() == Some('^') {
            self.advance();
            true
        } else {
            false
        };

        let mut charset = Charset::no();

        // First char can be ] or - literally
        if let Some(ch) = self.peek()
            && matches!(ch, ']' | '-')
        {
            charset.set(ch as u8, true);
            self.advance();
        }

        loop {
            match self.parse_atom()? {
                Atom::Empty => return unexpected_end(),
                Atom::Class(cs) => {
                    charset.merge(&cs);
                }
                Atom::WordEnd => {
                    charset.set(b'>', true);
                }
                Atom::Meta(']') => break,
                Atom::Meta(c) | Atom::Char(c) => {
                    if !c.is_ascii() {
                        return unexpected_unicode(c);
                    }

                    let start = c as u8;
                    let mut end = start;

                    // Check for ranges, e.g. [a-z].
                    // We exclude patterns like [a-], because this implicitly sets 'a'..='a' in this iteration,
                    // and then '-'..='-' in the next iteration, which is the exact behavior we need.
                    if let rest = self.rest()
                        && rest.starts_with("-")
                        && !rest.starts_with("-]")
                    {
                        self.advance(); // consume -

                        match self.parse_atom()? {
                            Atom::Empty => return unexpected_end(),
                            Atom::Class(_) => return unexpected_class(),
                            Atom::WordEnd => {
                                end = b'>';
                            }
                            Atom::Meta(c) | Atom::Char(c) => {
                                if !c.is_ascii() {
                                    return unexpected_unicode(c);
                                }
                                end = c as u8;
                            }
                        }
                    }

                    if start > end {
                        return invalid_range(start, end);
                    }
                    charset.set_range(start..=end, true);
                }
            }
        }

        if negated {
            charset.invert();
        }

        Ok(Regex::CharClass(charset))
    }

    fn parse_atom(&mut self) -> Result<Atom, String> {
        let Some(c) = self.advance() else {
            return Ok(Atom::Empty);
        };

        match c {
            '(' | ')' | '[' | ']' | '{' | '}' | '|' | '?' | '*' | '+' | '.' | '^' | '$' => {
                Ok(Atom::Meta(c))
            }
            '\\' => {
                let Some(ce) = self.advance() else {
                    return Err("unexpected end of pattern after backslash".to_string());
                };

                match ce {
                    '>' => Ok(Atom::WordEnd),
                    'w' => Ok(Atom::Class(ASCII_WORD_CHARSET)),
                    'W' => {
                        let mut cs = ASCII_WORD_CHARSET;
                        cs.invert();
                        Ok(Atom::Class(cs))
                    }
                    'd' => Ok(Atom::Class(ASCII_DIGIT_CHARSET)),
                    'D' => {
                        let mut cs = ASCII_DIGIT_CHARSET;
                        cs.invert();
                        Ok(Atom::Class(cs))
                    }
                    's' => Ok(Atom::Class(ASCII_WHITESPACE_CHARSET)),
                    'S' => {
                        let mut cs = ASCII_WHITESPACE_CHARSET;
                        cs.invert();
                        Ok(Atom::Class(cs))
                    }
                    't' => Ok(Atom::Char('\t')),
                    c if !c.is_ascii_alphanumeric() => Ok(Atom::Char(c)),
                    c => Err(format!("unknown escape sequence '\\{c}'")),
                }
            }
            _ => Ok(Atom::Char(c)),
        }
    }
}

struct CodeGen<'a, 'c> {
    compiler: &'c mut Compiler<'a>,
    captures: CaptureList<'a>,
    dst_good: IRCell<'a>,
    dst_bad: IRCell<'a>,
}

impl<'a, 'c> CodeGen<'a, 'c> {
    fn new(compiler: &'c mut Compiler<'a>, dst_good: IRCell<'a>, dst_bad: IRCell<'a>) -> Self {
        let captures = CaptureList::empty();
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
                self.emit_literal(s, *case_insensitive, on_match, on_fail)
            }

            Regex::CharClass(cs) => self.emit_charset(cs, 1, 1, on_match, on_fail),

            Regex::Dot => {
                let dst = self.compiler.get_reg(Register::InputOffset);
                let node = self.compiler.alloc_iri(IRI::AddImm { dst, imm: 1 });
                node.borrow_mut().next = Some(on_match);
                Ok(node)
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
                self.emit_charset(&ASCII_WORD_CHARSET, 1, 1, on_fail, on_match)
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
                    self.captures.push(self.compiler.arena, (start_reg, end_reg));

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

        // Single-char literal like `#+`
        if let Regex::Literal(ref s, case_insensitive) = *inner
            && s.len() == 1
        {
            // Optional single char: `a?`
            // It can be trivially translated to a Prefix/PrefixInsensitive check
            // where even on_fail is a success and is thus connected to on_match.
            if min == 0 && max == 1 {
                return self.emit(inner, on_match, on_match);
            }

            // Otherwise, we must translate to a Charset match.
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

    fn emit_literal(
        &mut self,
        s: &str,
        case_insensitive: bool,
        on_match: IRCell<'a>,
        on_fail: IRCell<'a>,
    ) -> Result<IRCell<'a>, String> {
        if s.is_empty() {
            return Ok(on_match);
        }
        let s = self.compiler.intern_string(s);
        let condition =
            if case_insensitive { Condition::PrefixInsensitive(s) } else { Condition::Prefix(s) };
        let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });
        if_node.borrow_mut().next = Some(on_fail);
        Ok(if_node)
    }

    fn emit_charset(
        &mut self,
        cs: &Charset,
        min: u32,
        max: u32,
        on_match: IRCell<'a>,
        on_fail: IRCell<'a>,
    ) -> Result<IRCell<'a>, String> {
        let mut next = if min == 0 { on_match } else { on_fail };

        // If the expression is of form [a], [ab], [aA], or [aAbB] it is
        // worth translating it to a Prefix/PrefixInsensitive check.
        // The [a] and [aA] cases are an obvious improvement, but even the other
        // two cases are worth it due to the shorter instruction encoding.
        if max == 1 {
            let mut cs = cs.clone();
            let mut chars = [(0u8, false); 2];
            let mut count = 0;

            for slot in &mut chars {
                let Some(mut idx) = cs.get_and_reset_lowest() else { break };
                let case_insensitive = idx.is_ascii_uppercase() && cs.get(idx.to_ascii_lowercase());
                if case_insensitive {
                    idx = idx.to_ascii_lowercase();
                    cs.set(idx, false);
                }
                *slot = (idx, case_insensitive);
                count += 1;
            }

            if count > 0 && cs.covers_none() {
                for &(ch, insensitive) in chars[..count].iter().rev() {
                    let s = unsafe { str::from_utf8_unchecked(slice::from_ref(&ch)) };
                    let node = self.emit_literal(s, insensitive, on_match, next)?;
                    next = node;
                }
                return Ok(next);
            }
        }

        let cs = self.compiler.intern_charset(cs);
        let condition = Condition::Charset { cs, min, max };
        let if_node = self.compiler.alloc_iri(IRI::If { condition, then: on_match });

        // min=0 implies that it cannot fail. Remove `on_fail` to allow for later optimizations.
        if_node.borrow_mut().next = Some(next);

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
