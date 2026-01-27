// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Parser: transforms DSL source into IR graph.
//!
//! ## Gotchas
//!
//! - Variables are in SSA-ish form: `var x = off; x = x + 1;` creates a new vreg for the
//!   second `x`. But the variable *name* still maps to the new vreg, so later reads see it.
//! - Physical registers (off, etc.) don't follow SSA. Reading them doesn't create a vreg;
//!   the `parse_expression` function handles this by copying to a fresh vreg.
//! - The `raise!` macro captures tokenizer position at call time. Don't advance() before raising.

use std::collections::HashMap;
use std::collections::hash_map::Entry;

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

struct RegexSpan<'a> {
    pub src: IRCell<'a>,
    pub dst_good: IRCell<'a>,
    pub dst_bad: IRCell<'a>,
    pub capture_groups: Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
}

struct Context<'a> {
    loop_start: Option<IRCell<'a>>,
    loop_exit: Option<IRCell<'a>>,
    capture_groups: Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
}

pub struct Parser<'a, 'c, 'src> {
    compiler: &'c mut Compiler<'a>,
    path: &'src str,
    tokenizer: Tokenizer<'src>,
    current_token: Token<'src>,
    context: Vec<Context<'a>, &'a Arena>,
    variables: HashMap<&'src str, IRRegCell<'a>>,
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

    fn parse_attributes(&mut self) -> CompileResult<FunctionAttributes<'a>> {
        let mut attributes = FunctionAttributes::default();

        while self.current_token == Token::Hash {
            self.advance();
            self.expect_token(Token::LeftBracket)?;

            let key = match self.current_token {
                Token::Identifier(k) => k,
                _ => raise!(self, "expected attribute name"),
            };
            self.advance();

            self.expect_token(Token::Equals)?;

            let value = match self.current_token {
                Token::String(s) => arena_clone_str(self.compiler.arena, s),
                _ => raise!(self, "expected attribute value"),
            };
            self.advance();

            self.expect_token(Token::RightBracket)?;

            match key {
                "display_name" => attributes.display_name = Some(value),
                "path" => attributes.paths.push(value),
                _ => raise!(self, "unknown attribute '{}'", key),
            }
        }

        Ok(attributes)
    }

    fn parse_function(&mut self) -> CompileResult<Function<'a>> {
        // Reset symbol table for new function
        self.variables =
            HashMap::from_iter([("off", self.compiler.get_reg(Register::InputOffset))]);

        let attributes = self.parse_attributes()?;

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

        Ok(Function { name, attributes, body: span.first, public })
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
        // First, save the current input offset.
        // This is used to detect if the loop made any progress.
        let saved_offset = self.compiler.alloc_vreg();
        let first = self.compiler.alloc_iri(IRI::Mov {
            dst: saved_offset,
            src: self.compiler.get_reg(Register::InputOffset),
        });

        self.context.push(Context {
            loop_start: Some(loop_start),
            loop_exit: Some(loop_exit),
            capture_groups: Vec::new_in(self.compiler.arena),
        });
        let block = self.parse_block()?;
        self.context.pop();

        // Force advance the input offset by 1 if it got stuck in the loop iteration.
        //   if input_offset == saved_offset {
        //       input_offset += 1;
        //   }
        let advance = self.compiler.alloc_ir(IR {
            next: Some(first),
            instr: IRI::AddImm { dst: self.compiler.get_reg(Register::InputOffset), imm: 1 },
            offset: usize::MAX,
        });
        let advance_check = self.compiler.alloc_ir(IR {
            next: Some(first),
            instr: IRI::If {
                condition: Condition::Cmp {
                    lhs: self.compiler.get_reg(Register::InputOffset),
                    rhs: saved_offset,
                    op: ComparisonOp::Eq,
                },
                then: advance,
            },
            offset: usize::MAX,
        });

        // NOTE: It's crucial that we connect the block with the loop before calling collect_interesting_charset,
        // as the until statement's regex is not part of the loop but still counts as an "interesting charset",
        // for the purpose of skipping uninteresting characters.
        first.borrow_mut().set_next(loop_start);
        loop_good.borrow_mut().set_next(block.first);

        // Skip any uninteresting characters before the next loop iteration.
        //   if /.*?/ {}
        let interesting = self.compiler.collect_interesting_charset(loop_start);
        let fast_skip = if interesting.covers_none() {
            advance_check
        } else {
            let mut skip_charset = interesting.clone();
            skip_charset.invert();
            let skip_charset = self.compiler.intern_charset(&skip_charset);
            self.compiler.alloc_ir(IR {
                next: Some(advance_check),
                instr: IRI::If { condition: Condition::Charset(skip_charset), then: advance_check },
                offset: usize::MAX,
            })
        };

        if let mut block_last = block.last.borrow_mut()
            && block_last.wants_next()
        {
            block_last.set_next(fast_skip);
        }

        Ok(IRSpan { first, last: loop_exit })
    }

    fn parse_break(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Break)?;
        self.expect_token(Token::Semicolon)?;

        if let Some(exit) = self.context.last_mut().and_then(|ctx| ctx.loop_exit) {
            let ir = self.compiler.alloc_noop();
            ir.borrow_mut().set_next(exit);
            Ok(IRSpan::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_continue(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Continue)?;
        self.expect_token(Token::Semicolon)?;

        if let Some(start) = self.context.last_mut().and_then(|ctx| ctx.loop_start) {
            let ir = self.compiler.alloc_noop();
            ir.borrow_mut().set_next(start);
            Ok(IRSpan::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_return(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::Return)?;
        self.expect_token(Token::Semicolon)?;
        Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Return)))
    }

    fn parse_if(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_token(Token::If)?;

        if matches!(self.current_token, Token::Regex(_)) {
            self.parse_if_regex_chain()
        } else {
            self.parse_if_comparison()
        }
    }

    fn parse_if_regex_chain(&mut self) -> CompileResult<IRSpan<'a>> {
        let mut prev: Option<IRCell<'a>> = None;

        // First, save the current input offset.
        // This is used to restore the position on failed matches.
        let save_reg = self.compiler.alloc_vreg();
        let first = self.compiler.alloc_iri(IRI::Mov {
            dst: save_reg,
            src: self.compiler.get_reg(Register::InputOffset),
        });

        let last = self.compiler.alloc_noop();

        loop {
            let re = self.parse_if_regex()?;

            // Push context with capture groups for the block
            let (loop_start, loop_exit) = self
                .context
                .last()
                .map(|ctx| (ctx.loop_start, ctx.loop_exit))
                .unwrap_or((None, None));
            self.context.push(Context { loop_start, loop_exit, capture_groups: re.capture_groups });
            let bl = self.parse_block()?;
            self.context.pop();

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
            let dst_bad = self.compiler.alloc_iri(IRI::Mov {
                dst: self.compiler.get_reg(Register::InputOffset),
                src: save_reg,
            });
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
                if let mut bl_last = bl.last.borrow_mut()
                    && bl_last.wants_next()
                {
                    bl_last.set_next(last);
                }
                break;
            }

            // Otherwise, we expect an "if" in the next iteration to form an "else if".
            self.expect_token(Token::If)?;
            prev = Some(dst_bad);
        }

        Ok(IRSpan { first, last })
    }

    fn parse_if_comparison(&mut self) -> CompileResult<IRSpan<'a>> {
        // Parse: if var1 OP var2 { block } where OP is ==, !=, <, >, <=, >=
        let lhs_name = match self.current_token {
            Token::Identifier(n) => n,
            _ => raise!(self, "expected variable name"),
        };
        let lhs_vreg = self.get_variable(lhs_name)?;
        self.advance();

        let op = match self.current_token {
            Token::EqualsEquals => ComparisonOp::Eq,
            Token::NotEquals => ComparisonOp::Ne,
            Token::LessThan => ComparisonOp::Lt,
            Token::GreaterThan => ComparisonOp::Gt,
            Token::LessThanEquals => ComparisonOp::Le,
            Token::GreaterThanEquals => ComparisonOp::Ge,
            _ => raise!(self, "expected comparison operator (==, !=, <, >, <=, >=)"),
        };
        self.advance();

        let rhs_name = match self.current_token {
            Token::Identifier(n) => n,
            _ => raise!(self, "expected variable name"),
        };
        let rhs_vreg = self.get_variable(rhs_name)?;
        self.advance();

        let dst_good = self.compiler.alloc_noop();
        let dst_bad = self.compiler.alloc_noop();
        let cmp = self.compiler.alloc_ir(IR {
            next: Some(dst_bad),
            instr: IRI::If {
                condition: Condition::Cmp { lhs: lhs_vreg, rhs: rhs_vreg, op },
                then: dst_good,
            },
            offset: usize::MAX,
        });

        let bl = self.parse_block()?;
        dst_good.borrow_mut().set_next(bl.first);

        let end = self.compiler.alloc_noop();

        // Handle optional else branch
        if matches!(self.current_token, Token::Else) {
            self.advance();

            // Check for else if
            if matches!(self.current_token, Token::If) {
                let else_if_span = self.parse_if()?;
                dst_bad.borrow_mut().set_next(else_if_span.first);
                if let mut block_last = bl.last.borrow_mut()
                    && block_last.wants_next()
                {
                    block_last.set_next(end);
                }
                if let mut else_if_last = else_if_span.last.borrow_mut()
                    && else_if_last.wants_next()
                {
                    else_if_last.set_next(end);
                }
                Ok(IRSpan { first: cmp, last: end })
            } else {
                let else_bl = self.parse_block()?;
                dst_bad.borrow_mut().set_next(else_bl.first);
                if let mut block_last = bl.last.borrow_mut()
                    && block_last.wants_next()
                {
                    block_last.set_next(end);
                }
                if let mut else_last = else_bl.last.borrow_mut()
                    && else_last.wants_next()
                {
                    else_last.set_next(end);
                }
                Ok(IRSpan { first: cmp, last: end })
            }
        } else {
            dst_bad.borrow_mut().set_next(end);
            if let mut block_last = bl.last.borrow_mut()
                && block_last.wants_next()
            {
                block_last.set_next(end);
            }
            Ok(IRSpan { first: cmp, last: end })
        }
    }

    fn parse_if_regex(&mut self) -> CompileResult<RegexSpan<'a>> {
        let pattern = match self.current_token {
            Token::Regex(s) => s,
            _ => raise!(self, "expected regex"),
        };
        let dst_good = self.compiler.alloc_noop();
        let dst_bad = self.compiler.alloc_noop();
        let mut capture_groups = Vec::new_in(self.compiler.arena);
        let src = match regex::parse(self.compiler, pattern, dst_good, dst_bad, &mut capture_groups)
        {
            Ok(s) => s,
            Err(e) => raise!(self, "{}", e),
        };

        self.advance();

        Ok(RegexSpan { src, dst_good, dst_bad, capture_groups })
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

        // Check if this is a capture group reference: yield $n as color
        if let Token::Submatch(capture_index) = self.current_token {
            let capture_index = capture_index as usize - 1;
            self.advance();

            // Expect "as"
            match self.current_token {
                Token::Identifier("as") => self.advance(),
                _ => raise!(self, "expected 'as' after capture group reference"),
            }

            let color = match self.current_token {
                Token::Identifier(c) => c,
                _ => raise!(self, "expected color name after 'as'"),
            };
            let kind = self.compiler.intern_highlight_kind(color).value;
            self.advance();
            self.expect_token(Token::Semicolon)?;

            let (start_vreg, end_vreg) = match self.context.last() {
                Some(ctx) if capture_index < ctx.capture_groups.len() => {
                    ctx.capture_groups[capture_index]
                }
                Some(_) => raise!(
                    self,
                    "capture group ${} not found in current context",
                    capture_index + 1
                ),
                None => raise!(self, "no regex context available for capture group reference"),
            };

            let kind_vreg = self.compiler.alloc_vreg();

            let span = self
                .compiler
                .build_chain()
                // Set HighlightStart to the start of the capture group
                .append(IRI::Mov {
                    dst: self.compiler.get_reg(Register::HighlightStart),
                    src: start_vreg,
                })
                // Set HighlightKind to the color
                .append(IRI::MovKind { dst: kind_vreg, kind })
                // Set InputOffset to the end of the capture group and flush
                .append(IRI::Mov {
                    dst: self.compiler.get_reg(Register::InputOffset),
                    src: end_vreg,
                })
                .append(IRI::Flush { kind: kind_vreg })
                .build();
            Ok(span)
        } else {
            // Normal yield: yield color;
            let color = match self.current_token {
                Token::Identifier(c) => c,
                _ => raise!(self, "expected color name after yield"),
            };
            let kind = self.compiler.intern_highlight_kind(color).value;

            self.advance();
            self.expect_token(Token::Semicolon)?;

            let vreg = self.compiler.alloc_vreg();
            let span = self
                .compiler
                .build_chain()
                .append(IRI::MovKind { dst: vreg, kind })
                .append(IRI::Flush { kind: vreg })
                .build();
            Ok(span)
        }
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

        match self.variables.entry(name) {
            Entry::Vacant(e) => _ = e.insert(vreg),
            Entry::Occupied(_) => raise!(self, "variable '{}' already declared", name),
        }

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

                let ir = self.compiler.alloc_iri(IRI::AddImm { dst: lhs_vreg, imm: val });
                self.variables.insert(name, lhs_vreg);
                Ok(IRSpan::single(ir))
            }
            _ => raise!(self, "expected '(', or '=', '+=' after identifier"),
        }
    }

    fn parse_expression(&mut self) -> CompileResult<(IRSpan<'a>, IRRegCell<'a>)> {
        let (lhs_span, lhs_vreg) = match self.current_token {
            Token::Integer(val) => {
                let vreg = self.compiler.alloc_vreg();
                let ir = self.compiler.alloc_iri(IRI::MovImm { dst: vreg, imm: val });
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
                    let add_ir = self.compiler.alloc_iri(IRI::AddImm { dst: lhs_vreg, imm: val });
                    lhs_span.last.borrow_mut().set_next(add_ir);
                    Ok((IRSpan { first: lhs_span.first, last: add_ir }, lhs_vreg))
                }
                _ => raise!(self, "expected integer literal after '+'"),
            }
        } else if lhs_vreg.borrow().physical.is_some() {
            // For expressions of type `var virtual = phyiscal;`, we need to ensure
            // that we actually copy the physical register into a new virtual one.
            // The remaining code assumes single assignment form, while physical registers are permanent.
            let dst = self.compiler.alloc_vreg();
            let node = self.compiler.alloc_iri(IRI::Mov { dst, src: lhs_vreg });
            Ok((IRSpan::single(node), dst))
        } else {
            Ok((lhs_span, lhs_vreg))
        }
    }

    fn get_variable(&self, name: &str) -> CompileResult<IRRegCell<'a>> {
        match self.variables.get(name) {
            Some(&reg) => Ok(reg),
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
