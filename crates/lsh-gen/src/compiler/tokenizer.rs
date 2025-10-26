// Example script:
//
// fn main() {
//     /#/ {
//         yield Comment;
//
//         /\tdeleted:.*/ { yield BrightRed; }
//         /\tmodified:.*/ { yield BrightBlue; }
//         /\tnew file:.*/ { yield BrightGreen; }
//         /\trenamed:.*/ { yield BrightBlue; }
//     }
//
//     /diff --git.*/ {
//         yield BrightBlue;
//         diff();
//     }
// }
//
// fn diff() {
//     /diff.*/ { yield BrightBlue; }
//     /---.*/ { yield BrightBlue; }
//     /\+\+\+.*/ { yield BrightBlue; }
//     /-.*/ { yield BrightRed; }
//     /\+.*/ { yield BrightGreen; }
// }

use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Identifier(String),
    Regex(String),

    // Keywords
    Return,
    Fn,
    Yield,
    If,
    Else,

    // Punctuation
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Semicolon,

    // End of file
    Eof,

    // Error
    Error(String),
}

pub struct Tokenizer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    current_pos: usize,
    last_pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, chars: input.chars().peekable(), current_pos: 0, last_pos: 0 }
    }

    pub fn position(&self) -> (usize, usize) {
        let line = self.input[..self.last_pos].lines().count();
        let column =
            self.input[..self.last_pos].lines().last().map_or(0, |line| line.chars().count());
        (line + 1, column + 1)
    }

    pub fn next_token(&mut self) -> Token {
        self.last_pos = self.current_pos;

        self.skip_whitespace();

        match self.advance() {
            Some(ch) => match ch {
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ';' => Token::Semicolon,
                '/' => self.read_regex(),
                c if c.is_alphabetic() || c == '_' => self.read_identifier_or_keyword(c),
                c => Token::Error(format!("Unexpected character: '{}'", c)),
            },
            None => Token::Eof,
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            self.current_pos += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.peek()
            && ch.is_whitespace()
        {
            self.advance();
        }
    }

    fn read_regex(&mut self) -> Token {
        let beg = self.current_pos;

        while let Some(ch) = self.advance()
            && ch != '/'
        {}

        let end = self.current_pos - 1;
        Token::Regex(String::from(&self.input[beg..end]))
    }

    fn read_identifier_or_keyword(&mut self, first_char: char) -> Token {
        let mut value = String::new();
        value.push(first_char);

        while let Some(&ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        match value.as_str() {
            "return" => Token::Return,
            "fn" => Token::Fn,
            "yield" => Token::Yield,
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Identifier(value),
        }
    }
}

// Iterator implementation for convenient usage
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token {
            Token::Eof => None,
            _ => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut tokenizer = Tokenizer::new("fn main() { yield; }");

        assert_eq!(tokenizer.next_token(), Token::Fn);
        assert_eq!(tokenizer.next_token(), Token::Identifier("main".to_string()));
        assert_eq!(tokenizer.next_token(), Token::LeftParen);
        assert_eq!(tokenizer.next_token(), Token::RightParen);
        assert_eq!(tokenizer.next_token(), Token::LeftBrace);
        assert_eq!(tokenizer.next_token(), Token::Yield);
        assert_eq!(tokenizer.next_token(), Token::Semicolon);
        assert_eq!(tokenizer.next_token(), Token::RightBrace);
        assert_eq!(tokenizer.next_token(), Token::Eof);
    }

    #[test]
    fn test_regex() {
        let mut tokenizer = Tokenizer::new("/\\tdeleted:.*/");

        assert_eq!(tokenizer.next_token(), Token::Regex("\\tdeleted:.*".to_string()));
        assert_eq!(tokenizer.next_token(), Token::Eof);
    }

    #[test]
    fn test_complex_example() {
        let input = r#"
            /#/ {
                yield Comment;
            }
        "#;

        let mut tokenizer = Tokenizer::new(input);

        assert_eq!(tokenizer.next_token(), Token::Regex("#".to_string()));
        assert_eq!(tokenizer.next_token(), Token::LeftBrace);
        assert_eq!(tokenizer.next_token(), Token::Yield);
        assert_eq!(tokenizer.next_token(), Token::Identifier("Comment".to_string()));
        assert_eq!(tokenizer.next_token(), Token::Semicolon);
        assert_eq!(tokenizer.next_token(), Token::RightBrace);
        assert_eq!(tokenizer.next_token(), Token::Eof);
    }
}
