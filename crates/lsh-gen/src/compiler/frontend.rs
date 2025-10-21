// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::RefCell;

use stdext::arena::{Arena, ArenaString};

use crate::compiler::regex::*;
use crate::compiler::tokenizer::*;

/// Intermediate representation of a parsed source file.
pub enum IR<'a> {
    Program(NodeCell<'a>),
}

type NodeCell<'a> = &'a RefCell<Node<'a>>;

#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: NodeCell<'a>,
}

#[derive(Debug)]
pub enum Condition<'a> {
    JumpIfNotMatchCharset { charset: &'a Charset },
    JumpIfNotMatchPrefix { prefix: &'a str },
    JumpIfNotMatchPrefixInsensitive { prefix: &'a str },
}

#[derive(Debug)]
pub enum Node<'a> {
    Return,
    Jump { destination: NodeCell<'a> },
    If { condition: Condition<'a>, then_branch: NodeCell<'a>, else_branch: NodeCell<'a> },
    Yield { color: &'a str, next: NodeCell<'a> },
    Call { name: &'a str, next: NodeCell<'a> },
}

impl<'a> Node<'a> {
    fn set_next(&mut self, _next: NodeCell<'a>) {
        todo!()
    }
}

pub struct Frontend<'a> {
    arena: &'a Arena,
    functions: Vec<Function<'a>>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
}

impl<'a> Frontend<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            arena,
            functions: Vec::new(),
            charsets: Default::default(),
            strings: Default::default(),
        }
    }

    /// Parses the given source code into a graph-based IR.
    pub fn parse<'s>(&'s mut self, src: &str) -> ParseResult<()>
    where
        's: 'a,
    {
        let mut parser =
            Parser { frontend: self, tokenizer: Tokenizer::new(src), current_token: Token::Eof };
        parser.run()?;
        Ok(())
    }
}

type ParseResult<T> = Result<T, ParseError>;

struct ParseError {
    message: String,
}

macro_rules! raise {
    ($msg:literal) => {
        return Err(ParseError { message: $msg.to_string() })
    };
    ($($arg:tt)*) => {
        return Err(ParseError { message: format!($($arg)*) })
    };
}

struct Parser<'a, 'src> {
    frontend: &'a mut Frontend<'a>,
    tokenizer: Tokenizer<'src>,
    current_token: Token,
}

impl<'a, 'src> Parser<'a, 'src> {
    pub fn run(&mut self) -> ParseResult<()> {
        self.advance();
        while !matches!(self.current_token, Token::Eof) {
            let f = self.parse_function()?;
            self.frontend.functions.push(f);
        }
        Ok(())
    }

    fn parse_function(&mut self) -> ParseResult<Function<'a>> {
        self.expect_token(Token::Fn)?;

        let name = match &self.current_token {
            Token::Identifier(n) => arena_clone_str(self.frontend.arena, n),
            _ => raise!("Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;

        let ret = self.alloc_node(Node::Return);
        let block = self.parse_block(ret)?;
        Ok(Function { name, body: block.map_or(ret, |(first, _)| first) })
    }

    fn parse_block(
        &mut self,
        next: NodeCell<'a>,
    ) -> ParseResult<Option<(NodeCell<'a>, NodeCell<'a>)>> {
        self.expect_token(Token::LeftBrace)?;

        let mut statements: Option<(&RefCell<Node<'a>>, &RefCell<Node<'a>>)> = None;

        while !matches!(self.current_token, Token::RightBrace | Token::Eof) {
            let statement = self.parse_statement(next)?;
            if let Some((_, last)) = &mut statements {
                last.borrow_mut().set_next(statement);
                *last = statement;
            } else {
                statements = Some((statement, statement));
            }
        }

        self.expect_token(Token::RightBrace)?;

        Ok(statements)
    }

    fn parse_statement(&mut self, next: NodeCell<'a>) -> ParseResult<NodeCell<'a>> {
        match &self.current_token {
            Token::Return => {
                self.advance();
                self.expect_token(Token::Semicolon)?;
                Ok(self.alloc_node(Node::Return))
            }
            Token::If => self.parse_if_statement(next),
            Token::Yield => self.parse_yield(next),
            Token::Identifier(_) => self.parse_call(next),
            _ => raise!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_if_statement(&mut self, next: NodeCell<'a>) -> ParseResult<NodeCell<'a>> {
        loop {
            self.expect_token(Token::If)?;
            self.parse_regex()?;
            self.parse_block(next)?;

            if !matches!(self.current_token, Token::Else) {
                break;
            }

            self.advance();

            if !matches!(self.current_token, Token::If) {
                self.parse_block(next)?;
                break;
            }
        }
    }

    fn parse_regex(&mut self) -> ParseResult<NodeCell<'a>> {
        todo!()
    }

    fn parse_yield(&mut self, next: NodeCell<'a>) -> ParseResult<NodeCell<'a>> {
        self.expect_token(Token::Yield)?;

        let color = match &self.current_token {
            Token::Identifier(c) => self.frontend.strings.intern(self.frontend.arena, c),
            _ => raise!("Expected color name after yield"),
        };
        self.advance();

        self.expect_token(Token::Semicolon)?;

        Ok(self.alloc_node(Node::Yield { color, next }))
    }

    fn parse_call(&mut self, next: NodeCell<'a>) -> ParseResult<NodeCell<'a>> {
        let name = match &self.current_token {
            Token::Identifier(n) => self.frontend.strings.intern(self.frontend.arena, n),
            _ => raise!("Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Semicolon)?;

        Ok(self.alloc_node(Node::Call { name, next }))
    }

    fn expect_token(&mut self, expected: Token) -> ParseResult<()> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            raise!("Expected {:?}, found {:?}", expected, self.current_token)
        }
    }

    fn advance(&mut self) {
        self.current_token = self.tokenizer.next_token();
    }

    fn alloc_node(&self, node: Node<'a>) -> NodeCell<'a> {
        self.frontend.arena.alloc_uninit().write(RefCell::new(node))
    }
}

trait Intern<'a, T: ?Sized> {
    fn intern(&mut self, arena: &'a Arena, item: &T) -> &'a T;
}

impl<'a> Intern<'a, Charset> for Vec<&'a Charset> {
    fn intern(&mut self, arena: &'a Arena, value: &Charset) -> &'a Charset {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena.alloc_uninit().write(value.clone());
            self.push(s);
            s
        }
    }
}

impl<'a> Intern<'a, str> for Vec<&'a str> {
    fn intern(&mut self, arena: &'a Arena, value: &str) -> &'a str {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena_clone_str(arena, value);
            self.push(s);
            s
        }
    }
}

fn arena_clone_str<'a>(arena: &'a Arena, s: &str) -> &'a str {
    ArenaString::from_str(arena, s).leak()
}
