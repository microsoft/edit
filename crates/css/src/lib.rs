// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A minimal CSS parser for TUI styling.
//!
//! Supports a tiny subset of CSS:
//! - Selectors: `.class`, `.class.class`, `.class, .class`, `.class > .class`, `.class .class`
//! - Properties: `color`, `background-color`
//! - Values: `#RRGGBBAA`, `var(--red)`, etc.

#![feature(allocator_api, cold_path)]

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::ops::{BitOr, BitXor};
use std::vec::Vec as StdVec;

use stdext::arena::Arena;
use stdext::hash;

pub mod oklab;
pub use oklab::{Oklab, StraightRgba};

/// Index into the terminal color palette (0-15) plus Background/Foreground.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IndexedColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    Background,
    Foreground,
}

impl IndexedColor {
    fn from_var_name(name: &str) -> Option<Self> {
        match name {
            "black" => Some(Self::Black),
            "red" => Some(Self::Red),
            "green" => Some(Self::Green),
            "yellow" => Some(Self::Yellow),
            "blue" => Some(Self::Blue),
            "magenta" => Some(Self::Magenta),
            "cyan" => Some(Self::Cyan),
            "white" => Some(Self::White),
            "bright-black" => Some(Self::BrightBlack),
            "bright-red" => Some(Self::BrightRed),
            "bright-green" => Some(Self::BrightGreen),
            "bright-yellow" => Some(Self::BrightYellow),
            "bright-blue" => Some(Self::BrightBlue),
            "bright-magenta" => Some(Self::BrightMagenta),
            "bright-cyan" => Some(Self::BrightCyan),
            "bright-white" => Some(Self::BrightWhite),
            "background" => Some(Self::Background),
            "foreground" => Some(Self::Foreground),
            _ => None,
        }
    }
}

/// VT text attributes (italic, underline).
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Attributes(pub u8);

#[allow(non_upper_case_globals)]
impl Attributes {
    pub const None: Self = Self(0);
    pub const Italic: Self = Self(0b1);
    pub const Underlined: Self = Self(0b10);
    pub const All: Self = Self(0b11);

    pub const fn is(self, attr: Self) -> bool {
        (self.0 & attr.0) == attr.0
    }
}

impl BitOr for Attributes {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitXor for Attributes {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

/// A color value from CSS.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorValue {
    /// Direct RGBA color from hex literal.
    Direct(StraightRgba),
    /// Indexed color from var() reference.
    Indexed(IndexedColor),
}

/// CSS property types we support.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyKind {
    Color,
    BackgroundColor,
}

/// A single CSS declaration (property: value).
#[derive(Debug, Clone, Copy)]
pub struct Declaration {
    pub property: PropertyKind,
    pub value: ColorValue,
}

/// CSS selector types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Selector<'a> {
    /// Simple class selector: `.foo`
    Simple(u64),
    /// AND clause: `.foo.bar`
    And(&'a [u64]),
    /// OR clause: `.foo, .bar`
    Or(&'a [Selector<'a>]),
    /// Child selector: `.foo > .bar`
    Child { parent: u64, child: Box<Selector<'a>> },
    /// Descendant selector: `.foo .bar`
    Descendant { ancestor: u64, descendant: Box<Selector<'a>> },
}

/// A CSS rule with selector and declarations.
#[derive(Debug)]
pub struct Rule<'a> {
    pub selector: Selector<'a>,
    pub declarations: &'a [Declaration],
}

/// Computed style properties.
#[derive(Debug, Clone, Copy, Default)]
pub struct ComputedStyle {
    pub color: Option<ColorValue>,
    pub background_color: Option<ColorValue>,
}

/// A parsed CSS stylesheet with indexed lookup.
pub struct Stylesheet<'a> {
    /// Map from class hash to rules containing that class.
    /// This enables O(1) lookup by class name.
    rules_by_class: HashMap<u64, std::vec::Vec<&'a Rule<'a>>>,
    /// All rules for complete traversal.
    #[allow(dead_code)]
    all_rules: &'a [Rule<'a>],
}

impl<'a> Stylesheet<'a> {
    /// Build index for fast class lookup.
    fn new(rules: &'a [Rule<'a>]) -> Self {
        let mut rules_by_class: HashMap<u64, std::vec::Vec<&'a Rule<'a>>> = HashMap::new();

        for rule in rules {
            // Extract all class hashes from this selector.
            let mut classes = std::vec::Vec::new();
            Self::extract_classes(&rule.selector, &mut classes);

            // Add this rule to all class buckets.
            for &class_hash in &classes {
                rules_by_class.entry(class_hash).or_default().push(rule);
            }
        }

        Self { rules_by_class, all_rules: rules }
    }

    fn extract_classes(selector: &Selector<'a>, out: &mut std::vec::Vec<u64>) {
        match selector {
            Selector::Simple(hash) => {
                if !out.contains(hash) {
                    out.push(*hash);
                }
            }
            Selector::And(hashes) => {
                for &hash in *hashes {
                    if !out.contains(&hash) {
                        out.push(hash);
                    }
                }
            }
            Selector::Or(selectors) => {
                for sel in *selectors {
                    Self::extract_classes(sel, out);
                }
            }
            Selector::Child { parent, child } => {
                if !out.contains(parent) {
                    out.push(*parent);
                }
                Self::extract_classes(child, out);
            }
            Selector::Descendant { ancestor, descendant } => {
                if !out.contains(ancestor) {
                    out.push(*ancestor);
                }
                Self::extract_classes(descendant, out);
            }
        }
    }
}

/// Context for cascading style lookup.
#[derive(Default)]
pub struct StyleContext {
    /// Stack of ancestor class hashes (parent, grandparent, etc.).
    ancestors: std::vec::Vec<u64>,
    /// Current element's class hash.
    current: u64,
}

impl StyleContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Push a new class onto the ancestor stack (when entering a child element).
    pub fn push(&mut self, class_hash: u64) {
        if self.current != 0 {
            self.ancestors.push(self.current);
        }
        self.current = class_hash;
    }

    /// Pop the most recent class from the ancestor stack (when exiting an element).
    pub fn pop(&mut self) {
        self.current = self.ancestors.pop().unwrap_or(0);
    }

    /// Look up computed styles for the current element.
    pub fn lookup<'a>(&self, stylesheet: &'a Stylesheet<'a>) -> ComputedStyle {
        let mut style = ComputedStyle::default();

        // Get rules that match the current class.
        if let Some(rules) = stylesheet.rules_by_class.get(&self.current) {
            for rule in rules {
                if self.matches_selector(&rule.selector) {
                    // Apply declarations in source order (later ones override).
                    for decl in rule.declarations {
                        match decl.property {
                            PropertyKind::Color => style.color = Some(decl.value),
                            PropertyKind::BackgroundColor => {
                                style.background_color = Some(decl.value)
                            }
                        }
                    }
                }
            }
        }

        style
    }

    fn matches_selector(&self, selector: &Selector) -> bool {
        match selector {
            Selector::Simple(hash) => *hash == self.current,
            Selector::And(hashes) => hashes.iter().all(|h| *h == self.current),
            Selector::Or(selectors) => selectors.iter().any(|s| self.matches_selector(s)),
            Selector::Child { parent, child } => {
                self.ancestors.last() == Some(parent) && self.matches_selector(child)
            }
            Selector::Descendant { ancestor, descendant } => {
                self.ancestors.contains(ancestor) && self.matches_selector(descendant)
            }
        }
    }
}

/// CSS parse error.
#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    Syntax,
    UnexpectedEof,
    InvalidHexColor,
    InvalidProperty,
    InvalidSelector,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self.kind {
            ParseErrorKind::Syntax => "syntax error",
            ParseErrorKind::UnexpectedEof => "unexpected end of file",
            ParseErrorKind::InvalidHexColor => "invalid hex color",
            ParseErrorKind::InvalidProperty => "invalid property",
            ParseErrorKind::InvalidSelector => "invalid selector",
        };
        write!(f, "{}:{}: {}", self.line, self.column, msg)
    }
}

impl std::error::Error for ParseError {}

struct Parser<'a, 'i> {
    arena: &'a Arena,
    input: &'i str,
    bytes: &'i [u8],
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a, 'i> Parser<'a, 'i> {
    fn new(arena: &'a Arena, input: &'i str) -> Self {
        Self { arena, input, bytes: input.as_bytes(), pos: 0, line: 1, column: 1 }
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError { kind, line: self.line, column: self.column }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\r' => {
                    self.pos += 1;
                    self.column += 1;
                }
                b'\n' => {
                    self.pos += 1;
                    self.line += 1;
                    self.column = 1;
                }
                b'/' if self.pos + 1 < self.bytes.len() && self.bytes[self.pos + 1] == b'*' => {
                    // Skip /* */ comments
                    self.pos += 2;
                    while self.pos + 1 < self.bytes.len() {
                        if self.bytes[self.pos] == b'*' && self.bytes[self.pos + 1] == b'/' {
                            self.pos += 2;
                            break;
                        }
                        if self.bytes[self.pos] == b'\n' {
                            self.line += 1;
                            self.column = 1;
                        } else {
                            self.column += 1;
                        }
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.bytes.len() { Some(self.bytes[self.pos]) } else { None }
    }

    fn expect(&mut self, ch: u8) -> Result<(), ParseError> {
        self.skip_whitespace();
        if self.peek() == Some(ch) {
            self.pos += 1;
            self.column += 1;
            Ok(())
        } else {
            Err(self.error(ParseErrorKind::Syntax))
        }
    }

    fn parse_identifier(&mut self) -> Result<&'i str, ParseError> {
        self.skip_whitespace();
        let start = self.pos;

        if self.pos >= self.bytes.len() {
            return Err(self.error(ParseErrorKind::UnexpectedEof));
        }

        // Identifiers: [a-zA-Z_-][a-zA-Z0-9_-]*
        let first = self.bytes[self.pos];
        if !first.is_ascii_alphabetic() && first != b'_' && first != b'-' {
            return Err(self.error(ParseErrorKind::Syntax));
        }

        self.pos += 1;
        self.column += 1;

        while self.pos < self.bytes.len() {
            let ch = self.bytes[self.pos];
            if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'-' {
                self.pos += 1;
                self.column += 1;
            } else {
                break;
            }
        }

        Ok(&self.input[start..self.pos])
    }

    fn parse_hex_color(&mut self) -> Result<StraightRgba, ParseError> {
        self.expect(b'#')?;
        let start = self.pos;

        // Expect exactly 8 hex digits for RRGGBBAA
        for _ in 0..8 {
            if self.pos >= self.bytes.len() || !self.bytes[self.pos].is_ascii_hexdigit() {
                return Err(self.error(ParseErrorKind::InvalidHexColor));
            }
            self.pos += 1;
            self.column += 1;
        }

        let hex_str = &self.input[start..self.pos];
        let value = u32::from_str_radix(hex_str, 16)
            .map_err(|_| self.error(ParseErrorKind::InvalidHexColor))?;

        Ok(StraightRgba::from_be(value))
    }

    fn parse_var(&mut self) -> Result<IndexedColor, ParseError> {
        // var(--name)
        self.parse_identifier()?; // "var"
        self.expect(b'(')?;
        self.expect(b'-')?;
        self.expect(b'-')?;
        let name = self.parse_identifier()?;
        self.expect(b')')?;

        IndexedColor::from_var_name(name).ok_or_else(|| self.error(ParseErrorKind::Syntax))
    }

    fn parse_color_value(&mut self) -> Result<ColorValue, ParseError> {
        self.skip_whitespace();
        match self.peek() {
            Some(b'#') => Ok(ColorValue::Direct(self.parse_hex_color()?)),
            Some(b'v') => Ok(ColorValue::Indexed(self.parse_var()?)),
            _ => Err(self.error(ParseErrorKind::Syntax)),
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        let prop_name = self.parse_identifier()?;
        self.expect(b':')?;

        let property = match prop_name {
            "color" => PropertyKind::Color,
            "background-color" => PropertyKind::BackgroundColor,
            _ => return Err(self.error(ParseErrorKind::InvalidProperty)),
        };

        let value = self.parse_color_value()?;
        self.expect(b';')?;

        Ok(Declaration { property, value })
    }

    fn parse_simple_selector(&mut self) -> Result<u64, ParseError> {
        self.expect(b'.')?;
        let class_name = self.parse_identifier()?;
        Ok(hash::hash(0, class_name.as_bytes()))
    }

    fn parse_selector(&mut self) -> Result<Selector<'a>, ParseError> {
        self.skip_whitespace();

        // Parse first simple selector or AND chain
        let mut classes = StdVec::new_in(self.arena);
        classes.push(self.parse_simple_selector()?);

        // Check for adjacent classes (.foo.bar = AND)
        self.skip_whitespace();
        while self.peek() == Some(b'.') {
            classes.push(self.parse_simple_selector()?);
            self.skip_whitespace();
        }

        let mut current = if classes.len() == 1 {
            Selector::Simple(classes[0])
        } else {
            Selector::And(classes.leak())
        };

        // Check for combinators
        loop {
            self.skip_whitespace();
            match self.peek() {
                Some(b'>') => {
                    // Child combinator
                    self.pos += 1;
                    self.column += 1;
                    let parent_hash = match current {
                        Selector::Simple(h) => h,
                        _ => return Err(self.error(ParseErrorKind::InvalidSelector)),
                    };
                    current = Selector::Child {
                        parent: parent_hash,
                        child: Box::new(self.parse_selector()?),
                    };
                }
                Some(b'.') => {
                    // Descendant combinator (space before .)
                    let ancestor_hash = match current {
                        Selector::Simple(h) => h,
                        _ => return Err(self.error(ParseErrorKind::InvalidSelector)),
                    };
                    current = Selector::Descendant {
                        ancestor: ancestor_hash,
                        descendant: Box::new(self.parse_selector()?),
                    };
                }
                Some(b',') => {
                    // OR combinator
                    self.pos += 1;
                    self.column += 1;
                    let mut selectors = StdVec::new_in(self.arena);
                    selectors.push(current);
                    selectors.push(self.parse_selector()?);
                    current = Selector::Or(selectors.leak());
                }
                _ => break,
            }
        }

        Ok(current)
    }

    fn parse_rule(&mut self) -> Result<Rule<'a>, ParseError> {
        let selector = self.parse_selector()?;
        self.expect(b'{')?;

        let mut declarations = StdVec::new_in(self.arena);
        loop {
            self.skip_whitespace();
            if self.peek() == Some(b'}') {
                self.pos += 1;
                self.column += 1;
                break;
            }
            declarations.push(self.parse_declaration()?);
        }

        Ok(Rule { selector, declarations: declarations.leak() })
    }

    fn parse_stylesheet(&mut self) -> Result<&'a [Rule<'a>], ParseError> {
        let mut rules = StdVec::new_in(self.arena);

        loop {
            self.skip_whitespace();
            if self.pos >= self.bytes.len() {
                break;
            }
            rules.push(self.parse_rule()?);
        }

        Ok(rules.leak())
    }
}

/// Parse a CSS string into a stylesheet.
pub fn parse<'a>(arena: &'a Arena, input: &str) -> Result<Stylesheet<'a>, ParseError> {
    let mut parser = Parser::new(arena, input);
    let rules = parser.parse_stylesheet()?;
    Ok(Stylesheet::new(rules))
}

/// Hash a class name for use in selectors.
#[inline]
pub fn hash_class(name: &str) -> u64 {
    hash::hash(0, name.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_selector() {
        let arena = Arena::new(4096).unwrap();
        let css = ".button { color: #ff0000ff; }";
        let stylesheet = parse(&arena, css).unwrap();
        assert_eq!(stylesheet.all_rules.len(), 1);
    }

    #[test]
    fn test_multiple_rules() {
        let arena = Arena::new(4096).unwrap();
        let css = r#"
            .button { color: #ff0000ff; }
            .label { background-color: var(--blue); }
        "#;
        let stylesheet = parse(&arena, css).unwrap();
        assert_eq!(stylesheet.all_rules.len(), 2);
    }

    #[test]
    fn test_lookup() {
        let arena = Arena::new(4096).unwrap();
        let css = ".button { color: #ff0000ff; }";
        let stylesheet = parse(&arena, css).unwrap();

        let mut ctx = StyleContext::new();
        ctx.push(hash_class("button"));
        let style = ctx.lookup(&stylesheet);

        assert!(style.color.is_some());
        if let Some(ColorValue::Direct(color)) = style.color {
            assert_eq!(color, StraightRgba::from_be(0xff0000ff));
        } else {
            panic!("Expected direct color");
        }
    }

    #[test]
    fn test_child_selector() {
        let arena = Arena::new(4096).unwrap();
        let css = ".parent > .child { color: #00ff00ff; }";
        let stylesheet = parse(&arena, css).unwrap();

        let mut ctx = StyleContext::new();
        ctx.push(hash_class("parent"));
        ctx.push(hash_class("child"));
        let style = ctx.lookup(&stylesheet);

        assert!(style.color.is_some());
    }

    #[test]
    fn test_var_colors() {
        let arena = Arena::new(4096).unwrap();
        let css = ".button { color: var(--red); }";
        let stylesheet = parse(&arena, css).unwrap();

        let mut ctx = StyleContext::new();
        ctx.push(hash_class("button"));
        let style = ctx.lookup(&stylesheet);

        assert!(style.color.is_some());
        if let Some(ColorValue::Indexed(idx)) = style.color {
            assert_eq!(idx, IndexedColor::Red);
        } else {
            panic!("Expected indexed color");
        }
    }
}
