use std::fmt::Debug;
use std::ops::RangeInclusive;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Charset {
    table: [bool; 256],
}

impl Charset {
    pub fn new() -> Self {
        Charset { table: [false; 256] }
    }

    pub fn set_range(&mut self, r: RangeInclusive<u8>) {
        for b in r {
            self.table[b as usize] = true;
        }
    }

    pub fn set(&mut self, b: u8) {
        self.table[b as usize] = true;
    }

    pub fn set_class(&mut self, class: char) {
        match class {
            'd' => self.set_range(b'0'..=b'9'),
            'w' => {
                self.set_range(b'0'..=b'9');
                self.set_range(b'A'..=b'Z');
                self.set_range(b'a'..=b'z');
                self.set(b'_');
            }
            's' => {
                self.set(b' ');
                self.set(b'\t');
                self.set(b'\n');
                self.set(b'\r');
            }
            _ => {}
        }
    }
}

// Print consecutive ranges, such as "CharSet([0-9, a-z, A-Z])"
impl Debug for Charset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let show_char = |f: &mut std::fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b.is_ascii_graphic() || b == b' ' {
                let b = b as char;
                write!(f, "'{b}'")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !self.table[beg] {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && self.table[end] {
                end += 1;
            }

            if !first {
                write!(f, ", ")?;
            }
            show_char(f, beg)?;
            if end - beg > 1 {
                write!(f, "-")?;
                show_char(f, end - 1)?;
            }

            beg = end;
            first = false;
        }

        write!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleRegex {
    /// Sequence of regexes (concatenation)
    Seq(Vec<SimpleRegex>),
    /// Alternation (e.g. foo|bar)
    Alt(Vec<SimpleRegex>),
    /// Matches a fixed string (case sensitive)
    Literal(String),
    /// Matches a fixed string (case insensitive)
    LiteralInsensitive(String),
    /// Matches a charset (allocated index)
    Charset(Charset),
    /// Matches a single character (any)
    AnyChar,
    /// Quantifier: {min,max}
    Repeat(Box<SimpleRegex>, usize, Option<usize>),
}

impl SimpleRegex {
    pub fn from_str(value: &str) -> Result<Self, String> {
        RegexParser::new(value).parse_regex()
    }
}

pub struct RegexParser<'a> {
    input: &'a [u8],
    pos: usize,
    case_insensitive: bool,
}

impl<'a> RegexParser<'a> {
    fn new(s: &'a str) -> Self {
        RegexParser { input: s.as_bytes(), pos: 0, case_insensitive: false }
    }

    fn parse_regex(&mut self) -> Result<SimpleRegex, String> {
        if self.try_consume("(?i)") {
            self.case_insensitive = true;
        }
        self.parse_alt()
    }

    fn parse_alt(&mut self) -> Result<SimpleRegex, String> {
        let mut seqs = vec![self.parse_seq()?];
        while self.try_consume("|") {
            seqs.push(self.parse_seq()?);
        }
        if seqs.len() == 1 { Ok(seqs.pop().unwrap()) } else { Ok(SimpleRegex::Alt(seqs)) }
    }

    fn parse_seq(&mut self) -> Result<SimpleRegex, String> {
        let mut items = Vec::new();
        while !self.eof() && !self.peek_is('|') && !self.peek_is(')') {
            items.push(self.parse_atom()?);
        }
        if items.len() == 1 { Ok(items.pop().unwrap()) } else { Ok(SimpleRegex::Seq(items)) }
    }

    fn parse_atom(&mut self) -> Result<SimpleRegex, String> {
        let mut atom = if self.try_consume("(") {
            let inner = self.parse_alt()?;
            self.expect(")")?;
            inner
        } else if self.try_consume("[") {
            let cs = self.parse_charset()?;
            SimpleRegex::Charset(cs)
        } else if self.try_consume(".") {
            SimpleRegex::AnyChar
        } else if self.peek(0) == Some(b'\\')
            && let Some(c) = self.peek(1)
            && matches!(c, b'd' | b'w' | b's')
        {
            self.consume(2);
            let mut cs = Charset::new();
            cs.set_class(c as char);
            SimpleRegex::Charset(cs)
        } else {
            let mut lit = String::new();

            while let Some(mut c) = self.peek(0)
                && !matches!(
                    c,
                    b'(' | b')' | b'[' | b']' | b'{' | b'}' | b'|' | b'*' | b'+' | b'?' | b'.'
                )
            {
                if c == b'\\' {
                    c = match self.peek(1) {
                        Some(b'd' | b'w' | b's') => break,
                        Some(c) => c,
                        None => return Err("Unexpected end after backslash".to_string()),
                    };
                    self.consume(1);
                }

                lit.push(c as char);
                self.consume(1);
            }

            if lit.is_empty() {
                return Err("Unexpected end or metachar".to_string());
            }

            if self.case_insensitive {
                SimpleRegex::LiteralInsensitive(lit)
            } else {
                SimpleRegex::Literal(lit)
            }
        };

        // Quantifiers
        if self.try_consume("{") {
            let min = self.parse_number()?;
            let max = if self.try_consume(",") {
                if self.peek_is('}') { None } else { Some(self.parse_number()?) }
            } else {
                Some(min)
            };
            self.expect("}")?;
            atom = SimpleRegex::Repeat(Box::new(atom), min, max);
        } else if self.try_consume("*") {
            atom = SimpleRegex::Repeat(Box::new(atom), 0, None);
        } else if self.try_consume("+") {
            atom = SimpleRegex::Repeat(Box::new(atom), 1, None);
        } else if self.try_consume("?") {
            atom = SimpleRegex::Repeat(Box::new(atom), 0, Some(1));
        }

        Ok(atom)
    }

    fn parse_charset(&mut self) -> Result<Charset, String> {
        let mut cs = Charset::new();
        let negate = self.try_consume("^");
        while !self.eof() && !self.peek_is(']') {
            let c = self.consume_char()?;
            if c == b'\\' {
                let class = self.consume_char()?;
                cs.set_class(class as char);
            } else if self.peek_is('-') && self.input.get(self.pos + 1) != Some(&b']') {
                self.consume(1); // consume '-'
                let end = self.consume_char()?;
                cs.set_range(c..=end);
            } else {
                cs.set(c);
            }
        }
        self.expect("]")?;
        if negate {
            for b in 0..=255 {
                cs.table[b] = !cs.table[b];
            }
        }
        Ok(cs)
    }

    fn parse_number(&mut self) -> Result<usize, String> {
        let mut n = 0;
        let mut found = false;

        while let Some(&b) = self.input.get(self.pos) {
            if !b.is_ascii_digit() {
                break;
            }

            n = n * 10 + (b - b'0') as usize;
            self.pos += 1;
            found = true;
        }

        if found { Ok(n) } else { Err("Expected number".to_string()) }
    }

    fn try_consume(&mut self, s: &str) -> bool {
        let bytes = s.as_bytes();
        if self.input.get(self.pos..self.pos + bytes.len()) == Some(bytes) {
            self.pos += bytes.len();
            true
        } else {
            false
        }
    }

    fn consume_char(&mut self) -> Result<u8, String> {
        if let Some(&c) = self.input.get(self.pos) {
            self.pos += 1;
            Ok(c)
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    fn consume(&mut self, n: usize) {
        self.pos += n;
    }

    fn expect(&mut self, s: &str) -> Result<(), String> {
        if self.try_consume(s) { Ok(()) } else { Err(format!("Expected '{s}'")) }
    }

    fn peek_is(&self, c: char) -> bool {
        self.peek(0) == Some(c as u8)
    }

    fn peek(&self, off: usize) -> Option<u8> {
        self.input.get(self.pos + off).copied()
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number() {
        let digits = {
            let mut cs = Charset::new();
            cs.set_class('d');
            SimpleRegex::Charset(cs)
        };
        let re = SimpleRegex::from_str(r#"-?\d*(\.\d+)?([eE][+-]?\d+)?"#).unwrap();

        assert_eq!(
            re,
            SimpleRegex::Seq(vec![
                // -?
                SimpleRegex::Repeat(Box::new(SimpleRegex::Literal("-".to_string())), 0, Some(1)),
                // \d*
                SimpleRegex::Repeat(Box::new(digits.clone()), 0, None),
                // (\.\d+)?
                SimpleRegex::Repeat(
                    Box::new(SimpleRegex::Seq(vec![
                        SimpleRegex::Literal(".".to_string()),
                        SimpleRegex::Repeat(Box::new(digits.clone()), 1, None),
                    ])),
                    0,
                    Some(1)
                ),
                // ([eE][+-]?\d+)?
                SimpleRegex::Repeat(
                    Box::new(SimpleRegex::Seq(vec![
                        SimpleRegex::Charset({
                            let mut cs = Charset::new();
                            cs.set(b'e');
                            cs.set(b'E');
                            cs
                        }),
                        SimpleRegex::Repeat(
                            Box::new(SimpleRegex::Charset({
                                let mut cs = Charset::new();
                                cs.set(b'+');
                                cs.set(b'-');
                                cs
                            })),
                            0,
                            Some(1)
                        ),
                        SimpleRegex::Repeat(Box::new(digits.clone()), 1, None)
                    ])),
                    0,
                    Some(1)
                ),
            ])
        );
    }
}
