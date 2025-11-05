// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    // Literals
    Identifier(&'a str),
    Regex(&'a str),

    // Keywords
    Pub,
    Fn,
    If,
    Else,
    Loop,
    Return,
    Yield,

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
    start_pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, chars: input.chars().peekable(), current_pos: 0, start_pos: 0 }
    }

    pub fn position(&self) -> (usize, usize) {
        let mut line = 1;
        let mut column = 1;

        for ch in self.input[..self.start_pos].chars() {
            column += 1;
            if ch == '\n' {
                line += 1;
                column = 1;
            }
        }

        (line, column)
    }

    pub fn next(&mut self) -> Token<'a> {
        self.skip_whitespace();

        self.start_pos = self.current_pos;

        match self.advance() {
            None => Token::Eof,
            Some(ch) => match ch {
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ';' => Token::Semicolon,
                '/' => self.read_regex(),
                c => self.read_identifier_or_keyword(c),
            },
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

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn skip_whitespace(&mut self) {
        loop {
            // Skip whitespace. advance() has been inlined with ASCII assumptions.
            while let Some(ch) = self.peek()
                && ch.is_ascii_whitespace()
            {
                self.chars.next();
                self.current_pos += 1;
            }

            // Check for "//" comments. Skip the line if found.
            if !self.input.get(self.current_pos..).is_some_and(|s| s.starts_with("//")) {
                break;
            }
            for ch in &mut self.chars {
                self.current_pos += 1;
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    fn read_regex(&mut self) -> Token<'a> {
        let beg = self.current_pos;
        let mut last_char = '\0';

        while let Some(ch) = self.advance()
            && (ch != '/' || last_char == '\\')
        {
            last_char = ch;
        }

        let end = self.current_pos - 1;
        Token::Regex(&self.input[beg..end])
    }

    fn read_identifier_or_keyword(&mut self, ch: char) -> Token<'a> {
        fn is_ident_start(ch: char) -> bool {
            ch.is_ascii_alphabetic() || ch == '_'
        }
        fn is_ident_continuation(ch: char) -> bool {
            ch.is_ascii_alphanumeric() || ch == '_' || ch == '.'
        }

        if !is_ident_start(ch) {
            return Token::Error(format!("Unexpected character: '{ch}'"));
        }

        while let Some(ch) = self.peek()
            && is_ident_continuation(ch)
        {
            self.advance();
        }

        match self.input.get(self.start_pos..self.current_pos).unwrap_or("") {
            "pub" => Token::Pub,
            "fn" => Token::Fn,
            "if" => Token::If,
            "else" => Token::Else,
            "loop" => Token::Loop,
            "return" => Token::Return,
            "yield" => Token::Yield,
            ident => Token::Identifier(ident),
        }
    }
}
