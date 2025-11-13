// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use super::tokenizer::*;
use super::*;

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

struct IRSpan<'a> {
    pub first: IRCell<'a>,
    pub last: IRCell<'a>,
}

impl<'a> IRSpan<'a> {
    pub fn single(node: IRCell<'a>) -> Self {
        Self { first: node, last: node }
    }
}

struct RegexSpan<'a> {
    pub src: IRCell<'a>,
    pub dst_good: IRCell<'a>,
    pub dst_bad: IRCell<'a>,
}

enum LoopFlowControlKind {
    Break,
    Continue,
}

struct LoopFlowControl<'a> {
    line: usize,
    column: usize,
    kind: LoopFlowControlKind,
    ir: IRCell<'a>,
}

struct Context<'a> {
    loop_start: Option<IRCell<'a>>,
    loop_exit: Option<IRCell<'a>>,
}

impl<'a> Context<'a> {
    fn add_break(&mut self, compiler: &mut Compiler<'a>) -> IRCell<'a> {
        let exit = self.loop_exit.get_or_insert_with(|| compiler.alloc_noop());
        let ir = compiler.alloc_noop();
        ir.borrow_mut().set_next(exit);
        ir
    }

    fn add_continue(&mut self, compiler: &mut Compiler<'a>) -> IRCell<'a> {
        let start = self.loop_start.get_or_insert_with(|| compiler.alloc_noop());
        let ir = compiler.alloc_noop();
        ir.borrow_mut().set_next(start);
        ir
    }
}

pub struct Parser<'a, 'c, 'src> {
    compiler: &'c mut Compiler<'a>,
    tokenizer: Tokenizer<'src>,
    current_token: Token<'src>,
    context: Vec<Context<'a>, &'a Arena>,
}

impl<'a, 'c, 'src> Parser<'a, 'c, 'src> {
    pub fn new(compiler: &'c mut Compiler<'a>, src: &'src str) -> Self {
        let context = Vec::new_in(compiler.arena);
        Self { compiler, tokenizer: Tokenizer::new(src), current_token: Token::Eof, context }
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
        let public = if matches!(self.current_token, Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        self.expect_token(Token::Fn)?;

        let name = match self.current_token {
            Token::Identifier(n) => arena_clone_str(self.compiler.arena, n),
            _ => raise!(self, "Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;

        let span = self.parse_block()?;

        if let mut last = span.last.borrow_mut()
            && last.wants_next()
        {
            last.set_next(self.compiler.alloc_iri(IRI::Return));
        }

        Ok(Function { name, body: span.first, public })
    }

    fn parse_block(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::LeftBrace)?;

        // TODO: a bit inoptimal to always allocate a noop node
        let mut result: Option<IRSpan> = None;

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
            None => IRSpan::single(self.compiler.alloc_noop()),
        })
    }

    fn parse_statement(&mut self) -> CompileResult<IRSpan<'a>> {
        match self.current_token {
            Token::Loop => self.parse_loop(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_statement(),
            Token::Yield => self.parse_yield(),
            Token::Identifier(_) => self.parse_call(),
            _ => raise!(self, "Unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_loop(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Loop)?;

        self.context.push(Context { loop_start: None, loop_exit: None });
        let mut span = self.parse_block()?;
        let ctx = self.context.pop().unwrap();

        // Connect the end of the block back to the beginning of the loop.
        span.last = self.compiler.chain_iri(span.last, IRI::Loop { dst: span.first });

        // Patch up break and continue statements.
        if let Some(loop_start) = ctx.loop_start {
            loop_start.borrow_mut().set_next(span.first);
            span.first = loop_start;
        }
        if let Some(loop_exit) = ctx.loop_exit {
            span.last = loop_exit;
        }

        Ok(span)
    }

    fn parse_break(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Break)?;
        self.expect_token(Token::Semicolon)?;

        let ir = match self.context.last_mut() {
            Some(ctx) => ctx.add_break(self.compiler),
            _ => raise!(self, "loop control statement outside of a loop"),
        };
        Ok(IRSpan::single(ir))
    }

    fn parse_continue(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Continue)?;
        self.expect_token(Token::Semicolon)?;

        let ir = match self.context.last_mut() {
            Some(ctx) => ctx.add_continue(self.compiler),
            _ => raise!(self, "loop control statement outside of a loop"),
        };
        Ok(IRSpan::single(ir))
    }

    fn parse_return(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Return)?;
        self.expect_token(Token::Semicolon)?;
        Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Return)))
    }

    fn parse_if_statement(&mut self) -> CompileResult<IRSpan<'a>> {
        let mut first: Option<IRCell<'a>> = None;
        let mut else_branch: Option<IRCell<'a>> = None;
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
            if let mut block_last = bl.last.borrow_mut()
                && block_last.wants_next()
            {
                block_last.set_next(last);
            }

            // No else branch? Create a hidden connection to the end.
            if !matches!(self.current_token, Token::Else) {
                re.dst_bad.borrow_mut().set_next(last);
                break;
            }

            // Gobble the "else" token.
            self.advance();

            // The else branch has a block? Connect it with the if.
            if !matches!(self.current_token, Token::If) {
                let bl = self.parse_block()?;
                re.dst_bad.borrow_mut().set_next(bl.first);
                // Connect the end of the {} to the end of the if/else chain.
                bl.last.borrow_mut().set_next(last);
                break;
            }

            // Otherwise, we expect an "if" in the next iteration to form an "else if".
            else_branch = Some(re.dst_bad);
        }

        Ok(IRSpan { first: first.unwrap(), last })
    }

    fn parse_if_regex(&mut self) -> CompileResult<RegexSpan<'a>> {
        let pattern = match self.current_token {
            Token::Regex(s) => s,
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

    fn parse_yield(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Yield)?;

        let color = match self.current_token {
            Token::Identifier(c) => c,
            _ => raise!(self, "Expected color name after yield"),
        };
        let imm = self.compiler.intern_highlight_kind(color).value;

        self.advance();
        self.expect_token(Token::Semicolon)?;

        let set = self.compiler.alloc_iri(IRI::Add {
            dst: Register::HighlightKind,
            src: Register::Zero,
            imm,
        });
        let flush = self.compiler.chain_iri(set, IRI::Flush);
        Ok(IRSpan { first: set, last: flush })
    }

    fn parse_call(&mut self) -> CompileResult<IRSpan<'a>> {
        let name = match self.current_token {
            Token::Identifier(n) => self.compiler.strings.intern(self.compiler.arena, n),
            _ => raise!(self, "Expected function name"),
        };
        self.advance();

        self.expect_token(Token::LeftParen)?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Semicolon)?;

        Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Call { name })))
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
        self.current_token = self.tokenizer.next();
    }
}
