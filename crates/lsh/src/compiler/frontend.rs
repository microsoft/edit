// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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
    /// Bitfield tracking allocated registers. Bit 5-15 correspond to x5-x15.
    /// A set bit means the register is in use.
    allocated_registers: u16,
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
            allocated_registers: 0, // No registers allocated initially
        }
    }

    /// Allocate a temporary register from the pool (x5-x15).
    /// Returns the lowest available register number.
    fn alloc_register(&mut self) -> Register {
        let available = !self.allocated_registers & 0xFFE0; // 0xFFE0 = bits 5-15
        if available == 0 {
            panic!("Register allocation failed: all 11 registers (x5-x15) are in use");
        }
        let bit = available.trailing_zeros();
        self.allocated_registers |= 1u16 << bit;
        unsafe { std::mem::transmute(bit as u8) }
    }

    /// Free a previously allocated register.
    fn free_register(&mut self, reg: Register) {
        let bit = reg as u8;
        debug_assert!(
            reg >= Register::X5 && reg <= Register::X15,
            "Attempted to free non-allocatable register: {:?}",
            reg
        );
        debug_assert!(
            (self.allocated_registers & (1u16 << bit)) != 0,
            "Double-free of register: {:?}",
            reg
        );
        self.allocated_registers &= !(1u16 << bit);
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
            Token::Until => self.parse_until(),
            Token::Break => self.parse_break(),
            Token::Continue => self.parse_continue(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_statement(),
            Token::Await => self.parse_await(),
            Token::Yield => self.parse_yield(),
            Token::Identifier(_) => self.parse_call(),
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

    fn parse_if_statement(&mut self) -> CompileResult<IRSpan<'a>> {
        // Allocate a register to save the position for backtracking across if/else branches
        let save_reg = self.alloc_register();

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

        // Free the register after the if/else chain is complete
        self.free_register(save_reg);

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
            _ => raise!(self, "expected function name"),
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
            raise!(self, "expected {:?}, found {:?}", expected, self.current_token)
        }
    }

    fn advance(&mut self) {
        self.current_token = self.tokenizer.next();
    }
}
