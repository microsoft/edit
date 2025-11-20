// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::collections::HashMap;

use super::tokenizer::*;
use super::*;

macro_rules! raise {
    ($self:ident, $msg:literal) => {{
        let path = $self.path.to_string();
        let (line, column) = $self.tokenizer.position();
        return Err(CompileError { path, line, column, message: $msg.to_string() })
    }};
    ($self:ident, $($arg:tt)*) => {{
        let path = $self.path.to_string();
        let (line, column) = $self.tokenizer.position();
        return Err(CompileError { path, line, column, message: format!($($arg)*) })
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
    path: &'src str,
    tokenizer: Tokenizer<'src>,
    current_token: Token<'src>,
    context: Vec<Context<'a>, &'a Arena>,
    variables: HashMap<&'src str, VRegId>,
}

impl<'a, 'c, 'src> Parser<'a, 'c, 'src> {
    pub fn new(compiler: &'c mut Compiler<'a>, path: &'src str, src: &'src str) -> Self {
        let context = Vec::new_in(compiler.arena);
        Self {
            compiler,
            path,
            tokenizer: Tokenizer::new(src),
            current_token: Token::Eof,
            context,
            variables: Default::default(),
        }
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
        // Reset symbol table for new function
        self.variables.clear();

        let public = if matches!(self.current_token, Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        self.expect_token(Token::Fn)?;

        let name = match self.current_token {
            Token::Identifier(n) => arena_clone_str(self.compiler.arena, n),
            _ => raise!(self, "expected function name"),
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

        Ok(Function { name, body: span.first, public, used_registers: 0 })
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
            Token::Var => self.parse_var_declaration(),
            Token::Loop => self.parse_loop(),
            Token::Until => self.parse_until(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            Token::Await => self.parse_await(),
            Token::Yield => self.parse_yield(),
            Token::Identifier(_) => self.parse_identifier(),
            _ => raise!(self, "unexpected token: {:?}", self.current_token),
        }
    }

    fn parse_loop(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Loop)?;

        let loop_start = self.compiler.alloc_noop();
        let loop_exit = self.compiler.alloc_noop();
        self.parse_until_impl(loop_start, loop_start, loop_exit)
    }

    fn parse_until(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Until)?;

        let re = self.parse_if_regex()?;

        let loop_exit = self.compiler.alloc_noop();
        re.dst_good.borrow_mut().set_next(loop_exit);
        self.parse_until_impl(re.src, re.dst_bad, loop_exit)
    }

    fn parse_until_impl(
        &mut self,
        loop_start: IRCell<'a>,
        loop_good: IRCell<'a>,
        loop_exit: IRCell<'a>,
    ) -> CompileResult<IRSpan<'a>> {
        self.context.push(Context { loop_start: Some(loop_start), loop_exit: Some(loop_exit) });
        let block = self.parse_block();
        self.context.pop();

        let block = block?;
        loop_good.borrow_mut().set_next(block.first);
        block.last.borrow_mut().set_next(loop_start);
        Ok(IRSpan { first: loop_start, last: loop_exit })
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

    fn parse_if(&mut self) -> CompileResult<IRSpan<'a>> {
        let save_reg = self.compiler.alloc_vreg();

        let mut prev: Option<IRCell<'a>> = None;
        let first = self.compiler.save_position(save_reg);
        let last = self.compiler.alloc_noop();

        loop {
            self.expect_token(Token::If)?;
            let re = self.parse_if_regex()?;
            let bl = self.parse_block()?;

            // Connect the previous else branch to form an "else if".
            // If there's no previous one, we're in the first iteration,
            // and so we connect it to the instruction that saves the position.
            prev.unwrap_or(first).borrow_mut().set_next(re.src);

            // Connect the if to the {}.
            re.dst_good.borrow_mut().set_next(bl.first);
            // Connect the end of the {} to the end of the if/else chain.
            if let mut block_last = bl.last.borrow_mut()
                && block_last.wants_next()
            {
                block_last.set_next(last);
            }

            // The "else" branch of the if needs to restore the position.
            let dst_bad = self.compiler.restore_position(save_reg);
            re.dst_bad.borrow_mut().set_next(dst_bad);

            // No else branch? dst_bad (= no hit) means we're done, so make that connection.
            if !matches!(self.current_token, Token::Else) {
                dst_bad.borrow_mut().set_next(last);
                break;
            }

            // Gobble the "else" token.
            self.advance();

            // The else branch has a block? That's our dst_bad.
            if !matches!(self.current_token, Token::If) {
                let bl = self.parse_block()?;
                dst_bad.borrow_mut().set_next(bl.first);
                // Connect the end of the {} to the end of the if/else chain.
                bl.last.borrow_mut().set_next(last);
                break;
            }

            // Otherwise, we expect an "if" in the next iteration to form an "else if".
            prev = Some(dst_bad);
        }

        Ok(IRSpan { first, last })
    }

    fn parse_if_regex(&mut self) -> CompileResult<RegexSpan<'a>> {
        let pattern = match self.current_token {
            Token::Regex(s) => s,
            _ => raise!(self, "expected regex"),
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

    fn parse_await(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Await)?;

        match self.current_token {
            Token::Identifier("input") => self.advance(),
            _ => raise!(self, "expected 'input' after await"),
        }

        self.expect_token(Token::Semicolon)?;

        let ir = self.compiler.alloc_iri(IRI::AwaitInput);
        Ok(IRSpan::single(ir))
    }

    fn parse_yield(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Yield)?;

        let color = match self.current_token {
            Token::Identifier(c) => c,
            _ => raise!(self, "expected color name after yield"),
        };
        let kind = self.compiler.intern_highlight_kind(color).value;

        self.advance();
        self.expect_token(Token::Semicolon)?;

        let set = self.compiler.alloc_iri(IRI::SetHighlightKind { kind });
        let flush = self.compiler.chain_iri(set, IRI::Flush);
        Ok(IRSpan { first: set, last: flush })
    }

    fn parse_var_declaration(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Var)?;

        let name = match self.current_token {
            Token::Identifier(n) => n,
            _ => raise!(self, "expected variable name"),
        };
        self.advance();

        self.expect_token(Token::Equals)?;
        let (expr, vreg) = self.parse_expression()?;
        self.expect_token(Token::Semicolon)?;
        self.variables.insert(name, vreg);
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> CompileResult<IRSpan<'a>> {
        let name = match self.current_token {
            Token::Identifier(n) => n,
            _ => raise!(self, "expected identifier"),
        };
        self.advance();

        match self.current_token {
            // foo();
            Token::LeftParen => {
                self.advance();
                self.expect_token(Token::RightParen)?;
                self.expect_token(Token::Semicolon)?;
                let name = self.compiler.strings.intern(self.compiler.arena, name);
                Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Call { name })))
            }
            // foo = expr;
            Token::Equals => {
                self.advance();
                let (expr, vreg) = self.parse_expression()?;
                self.expect_token(Token::Semicolon)?;
                self.variables.insert(name, vreg);
                Ok(expr)
            }
            // foo += expr;
            Token::PlusEquals => {
                let lhs_vreg = self.get_variable(name)?;
                self.advance();

                let Token::Integer(val) = self.current_token else {
                    raise!(self, "expected integer literal after '+='");
                };
                self.advance();

                self.expect_token(Token::Semicolon)?;

                let result_vreg = self.compiler.alloc_vreg();
                let ir = self.compiler.alloc_iri(IRI::AddImm {
                    dst: result_vreg,
                    src: lhs_vreg,
                    imm: val,
                });
                self.variables.insert(name, result_vreg);
                Ok(IRSpan::single(ir))
            }
            _ => raise!(self, "expected '(', or '=', '+=' after identifier"),
        }
    }

    fn parse_expression(&mut self) -> CompileResult<(IRSpan<'a>, VRegId)> {
        let (lhs_span, lhs_vreg) = match self.current_token {
            Token::Integer(val) => {
                let vreg = self.compiler.alloc_vreg();
                let ir = self.compiler.alloc_iri(IRI::LoadImm { dst: vreg, value: val });
                self.advance();
                (IRSpan::single(ir), vreg)
            }
            Token::Identifier(name) => {
                let vreg = self.get_variable(name)?;
                self.advance();
                (IRSpan::single(self.compiler.alloc_noop()), vreg)
            }
            _ => raise!(self, "expected integer or identifier in expression"),
        };

        // Check for binary operator
        if matches!(self.current_token, Token::Plus) {
            self.advance();

            // Parse right-hand side - only integer literals supported
            match self.current_token {
                Token::Integer(val) => {
                    self.advance();
                    let result_vreg = self.compiler.alloc_vreg();
                    let add_ir = self.compiler.alloc_iri(IRI::AddImm {
                        dst: result_vreg,
                        src: lhs_vreg,
                        imm: val,
                    });
                    // Chain: lhs_span -> add_ir
                    lhs_span.last.borrow_mut().set_next(add_ir);
                    Ok((IRSpan { first: lhs_span.first, last: add_ir }, result_vreg))
                }
                _ => raise!(self, "expected integer literal after '+'"),
            }
        } else {
            Ok((lhs_span, lhs_vreg))
        }
    }

    fn get_variable(&self, name: &str) -> CompileResult<VRegId> {
        match self.variables.get(name) {
            Some(&vreg) => Ok(vreg),
            None => raise!(self, "undefined variable '{}'", name),
        }
    }

    fn expect_token(&mut self, expected: Token) -> CompileResult<()> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            raise!(self, "expected {:?}, found {:?}", expected, self.current_token)
        }
    }

    fn advance(&mut self) {
        self.current_token = self.tokenizer.next();
    }
}
