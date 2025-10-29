// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ptr;

use crate::compiler::*;
use crate::definitions::HighlightKind;

macro_rules! raise {
    ($self:ident, $msg:literal) => {{
        let (line, column) = $self.tokenizer.position();
        return Err(CompileError { line, column, message: $msg.to_string() })
    }};
    ($self:ident, $($arg:tt)*) => {{
        let (line, column) = $self.tokenizer.position();
        return Err(CompileError { line, column, message: format!($($arg)*) })
    }};
}

struct NodeSpan<'a> {
    pub first: NodeCell<'a>,
    pub last: NodeCell<'a>,
}

impl<'a> NodeSpan<'a> {
    pub fn single(node: NodeCell<'a>) -> Self {
        Self { first: node, last: node }
    }
}

struct RegexSpan<'a> {
    pub src: NodeCell<'a>,
    pub dst_good: NodeCell<'a>,
    pub dst_bad: NodeCell<'a>,
}

pub struct Parser<'a, 'c, 'src> {
    compiler: &'c mut Compiler<'a>,
    tokenizer: Tokenizer<'src>,
    current_token: Token,
}

impl<'a, 'c, 'src> Parser<'a, 'c, 'src> {
    pub fn new(compiler: &'c mut Compiler<'a>, src: &'src str) -> Self {
        Self { compiler, tokenizer: Tokenizer::new(src), current_token: Token::Eof }
    }

    pub fn run(&mut self) -> CompileResult<()> {
        self.advance();
        while !matches!(self.current_token, Token::Eof) {
            let f = self.parse_function()?;
            self.compiler.functions.push(f);
        }
        Ok(())
    }

    fn parse_function(&mut self) -> CompileResult<Function<'a>> {
        self.expect_token(Token::Fn)?;

        let name = match &self.current_token {
            Token::Identifier(n) => arena_clone_str(self.compiler.arena, n),
            _ => raise!(self, "Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;

        let span = self.parse_block()?;
        if let mut last = span.last.borrow_mut()
            && last.next().is_none()
        {
            last.set_next(self.compiler.alloc_node(Node::Return));
        }
        Ok(Function { name, body: span.first })
    }

    fn parse_block(&mut self) -> CompileResult<NodeSpan<'a>> {
        self.expect_token(Token::LeftBrace)?;

        // TODO: a bit inoptimal to always allocate a noop node
        let mut result: Option<NodeSpan> = None;

        while !matches!(self.current_token, Token::RightBrace | Token::Eof) {
            let s = self.parse_statement()?;
            if let Some(span) = &mut result {
                span.last.borrow_mut().set_next(s.first);
                span.last = s.last;
            } else {
                result = Some(s);
            }
        }

        self.expect_token(Token::RightBrace)?;
        Ok(match result {
            Some(span) => span,
            None => NodeSpan::single(self.compiler.alloc_noop()),
        })
    }

    fn parse_statement(&mut self) -> CompileResult<NodeSpan<'a>> {
        match &self.current_token {
            Token::Loop => {
                self.advance();
                let span = self.parse_block()?;
                span.last.borrow_mut().set_next(span.first);
                Ok(span)
            }
            Token::Return => {
                self.advance();
                self.expect_token(Token::Semicolon)?;
                Ok(NodeSpan::single(self.compiler.alloc_node(Node::Return)))
            }
            Token::If => self.parse_if_statement(),
            Token::Yield => self.parse_yield(),
            Token::Identifier(_) => self.parse_call(),
            _ => raise!(self, "Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_if_statement(&mut self) -> CompileResult<NodeSpan<'a>> {
        let mut first: Option<NodeCell<'a>> = None;
        let mut else_branch: Option<NodeCell<'a>> = None;
        let last = self.compiler.alloc_noop();

        loop {
            self.expect_token(Token::If)?;
            let re = self.parse_if_regex()?;
            let bl = self.parse_block()?;

            // Keep track of the first node to later return it.
            if first.is_none() {
                first = Some(re.src);
            }

            // Connect the last else branch to form an "else if".
            if let Some(n) = else_branch {
                n.borrow_mut().set_next(re.src);
            }

            // Connect the if to the {}.
            re.dst_good.borrow_mut().set_next(bl.first);
            // Connect the end of the {} to the end of the if/else chain.
            bl.last.borrow_mut().set_next(last);

            // No else branch? Create a hidden connection to the end.
            if !matches!(self.current_token, Token::Else) {
                re.dst_bad.borrow_mut().set_next(last);
                break;
            }

            self.advance();

            // The else branch has a block? Connect it with the if.
            if !matches!(self.current_token, Token::If) {
                let bl = self.parse_block()?;
                re.dst_bad.borrow_mut().set_next(bl.first);
                // Connect the end of the {} to the end of the if/else chain.
                bl.last.borrow_mut().set_next(last);
                break;
            }

            else_branch = Some(re.dst_bad);
        }

        Ok(NodeSpan { first: first.unwrap(), last })
    }

    fn parse_if_regex(&mut self) -> CompileResult<RegexSpan<'a>> {
        let pattern = match &self.current_token {
            Token::Regex(s) => s.as_str(),
            _ => raise!(self, "Expected regex after if"),
        };
        let dst_good = self.compiler.alloc_noop();
        let dst_bad = self.compiler.alloc_noop();
        let src = match regex::parse(self.compiler, pattern, dst_good, dst_bad) {
            Ok(s) => s,
            Err(e) => raise!(self, "{}", e),
        };

        self.advance();

        Ok(RegexSpan { src, dst_good, dst_bad })
    }

    fn parse_yield(&mut self) -> CompileResult<NodeSpan<'a>> {
        self.expect_token(Token::Yield)?;

        let color = match &self.current_token {
            Token::Identifier(c) => c,
            _ => raise!(self, "Expected color name after yield"),
        };
        let color = match HighlightKind::from_str(color) {
            Some(c) => c.as_usize(),
            None => raise!(self, "Unknown highlight color: {}", color),
        };

        self.advance();
        self.expect_token(Token::Semicolon)?;

        Ok(NodeSpan::single(self.compiler.alloc_node(Node::Add {
            dst: Register::HighlightKind,
            src: Register::Zero,
            imm: color,
            next: None,
        })))
    }

    fn parse_call(&mut self) -> CompileResult<NodeSpan<'a>> {
        let name = match &self.current_token {
            Token::Identifier(n) => self.compiler.strings.intern(self.compiler.arena, n),
            _ => raise!(self, "Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Semicolon)?;

        Ok(NodeSpan::single(self.compiler.alloc_node(Node::Call { name, next: None })))
    }

    fn expect_token(&mut self, expected: Token) -> CompileResult<()> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            raise!(self, "Expected {:?}, found {:?}", expected, self.current_token)
        }
    }

    fn advance(&mut self) {
        self.current_token = self.tokenizer.next_token();
    }
}
