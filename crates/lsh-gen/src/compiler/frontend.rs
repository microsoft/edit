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
pub enum Node<'a> {
    /// Block containing statements
    Block { statements: Option<NodeCell<'a>>, next: Option<NodeCell<'a>> },
    /// Regex match block: pattern, body, next statement
    RegexMatch { pattern: &'a Charset, body: NodeCell<'a>, next: Option<NodeCell<'a>> },
    /// Yield statement: color name
    Yield { color: &'a str, next: Option<NodeCell<'a>> },
    /// Function call: name
    Call { name: &'a str, next: Option<NodeCell<'a>> },
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
    /// Entrypoint. Call this.
    fn run(&mut self) -> ParseResult<()> {
        self.advance();
        while !matches!(self.current_token, Token::Eof) {
            let function = self.parse_function()?;
            self.frontend.functions.push(function);
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

        let body = self.parse_block()?;

        Ok(Function { name, body })
    }

    fn parse_block(&mut self) -> ParseResult<NodeCell<'a>> {
        self.expect_token(Token::LeftBrace)?;

        let mut statements = None;
        let mut last_statement = None;

        while !matches!(self.current_token, Token::RightBrace | Token::Eof) {
            let statement = self.parse_statement()?;

            if statements.is_none() {
                statements = Some(statement);
                last_statement = Some(statement);
            } else {
                last_statement.unwrap().borrow_mut().set_next(Some(statement));
                last_statement = Some(statement);
            }
        }

        self.expect_token(Token::RightBrace)?;

        Ok(self.alloc_node(Node::Block { statements, next: None }))
    }

    fn parse_statement(&mut self) -> ParseResult<NodeCell<'a>> {
        match &self.current_token {
            Token::Regex(_) => self.parse_regex_match(),
            Token::Yield => self.parse_yield(),
            Token::Identifier(_) => self.parse_call(),
            _ => raise!("Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_regex_match(&mut self) -> ParseResult<NodeCell<'a>> {
        let pattern = match &self.current_token {
            Token::Regex(r) => {
                let charset =
                    Charset::from_regex(r).map_err(|e| format!("Invalid regex: {}", e))?;
                self.frontend.charsets.intern(self.frontend.arena, &charset)
            }
            _ => raise!("Expected regex pattern"),
        };
        self.advance();

        let body = self.parse_block()?;

        Ok(self.alloc_node(Node::RegexMatch { pattern, body, next: None }))
    }

    fn parse_yield(&mut self) -> ParseResult<NodeCell<'a>> {
        self.expect_token(Token::Yield)?;

        let color = match &self.current_token {
            Token::Identifier(c) => self.frontend.strings.intern(self.frontend.arena, c),
            _ => raise!("Expected color name after yield"),
        };
        self.advance();

        self.expect_token(Token::Semicolon)?;

        Ok(self.alloc_node(Node::Yield { color, next: None }))
    }

    fn parse_call(&mut self) -> ParseResult<NodeCell<'a>> {
        let name = match &self.current_token {
            Token::Identifier(n) => self.frontend.strings.intern(self.frontend.arena, n),
            _ => raise!("Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Semicolon)?;

        Ok(self.alloc_node(Node::Call { name, next: None }))
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

impl<'a> Node<'a> {
    /// Helper method to set the next pointer for any node type
    fn set_next(&mut self, next: Option<NodeCell<'a>>) {
        match self {
            Node::Block { next: n, .. }
            | Node::RegexMatch { next: n, .. }
            | Node::Yield { next: n, .. }
            | Node::Call { next: n, .. } => *n = next,
        }
    }

    /// Helper method to get the next pointer for any node type
    pub fn next(&self) -> Option<NodeCell<'a>> {
        match self {
            Node::Block { next, .. }
            | Node::RegexMatch { next, .. }
            | Node::Yield { next, .. }
            | Node::Call { next, .. } => *next,
        }
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
