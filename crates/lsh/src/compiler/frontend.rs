// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Frontend: DSL source code -> IR graph
//!
//! ## Gotchas
//!
//! - Variables use dedicated virtual registers. Expressions emit directly into a
//!   caller-specified destination register (destination-driven codegen, à la Lua 5).
//! - The `raise!` macro captures token_start position at call time. Don't advance before raising.

use std::collections::HashMap;
use std::collections::hash_map::Entry;

use stdext::collections::BVec;

use super::*;

macro_rules! raise {
    ($self:ident, $msg:literal) => {{
        let path = $self.path.to_string();
        let (line, column) = $self.position();
        return Err(CompileError { path, line, column, message: $msg.to_string() })
    }};
    ($self:ident, $($arg:tt)*) => {{
        let path = $self.path.to_string();
        let (line, column) = $self.position();
        return Err(CompileError { path, line, column, message: format!($($arg)*) })
    }};
}

/// A parsed condition (regex match or comparison) that branches to
/// `dst_good` on success and `dst_bad` on failure.  Entry is `src`.
struct CondSpan<'a> {
    pub src: IRCell<'a>,
    pub dst_good: IRCell<'a>,
    pub dst_bad: IRCell<'a>,
    pub capture_groups: BVec<'a, (IRRegCell<'a>, IRRegCell<'a>)>,
}

struct Context<'a> {
    loop_start: Option<IRCell<'a>>,
    loop_exit: Option<IRCell<'a>>,
    capture_groups: BVec<'a, (IRRegCell<'a>, IRRegCell<'a>)>,
}

pub struct Parser<'a, 'c, 'src> {
    compiler: &'c mut Compiler<'a>,
    path: &'src str,
    src: &'src str,
    pos: usize,
    token_start: usize,
    context: BVec<'a, Context<'a>>,
    variables: HashMap<&'src str, IRRegCell<'a>>,
}

impl<'a, 'c, 'src> Parser<'a, 'c, 'src> {
    pub fn new(compiler: &'c mut Compiler<'a>, path: &'src str, src: &'src str) -> Self {
        let context = BVec::empty();
        Self { compiler, path, src, pos: 0, token_start: 0, context, variables: Default::default() }
    }

    pub fn run(&mut self) -> CompileResult<()> {
        while !self.is_at_eof() {
            let f = self.parse_function()?;
            self.compiler.functions.push(f);
        }
        Ok(())
    }

    fn parse_attributes(&mut self) -> CompileResult<FunctionAttributes<'a>> {
        let mut attributes = FunctionAttributes::default();

        while self.peek() == Some('#') {
            self.pos += 1;
            self.expect('[')?;

            let key = self.read_identifier()?;
            self.expect('=')?;
            let value = arena_clone_str(self.compiler.arena, self.read_string()?);
            self.expect(']')?;

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

        let public = if self.is_keyword("pub") {
            self.pos += 3;
            true
        } else {
            false
        };

        self.expect_keyword("fn")?;

        let name = arena_clone_str(self.compiler.arena, self.read_identifier()?);

        self.expect('(')?;
        self.expect(')')?;

        let span = self.parse_block()?;

        if let mut last = span.last.borrow_mut()
            && last.wants_next()
        {
            last.set_next(self.compiler.alloc_iri(IRI::Return));
        }

        Ok(Function { name, attributes, body: span.first, public })
    }

    fn parse_block(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect('{')?;

        // TODO: a bit inoptimal to always allocate a noop node
        let mut result: Option<IRSpan> = None;

        while !matches!(self.peek(), Some('}') | None) {
            let s = self.parse_statement()?;
            if let Some(span) = &mut result {
                span.last.borrow_mut().set_next(s.first);
                span.last = s.last;
            } else {
                result = Some(s);
            }
        }

        self.expect('}')?;
        Ok(match result {
            Some(span) => span,
            None => IRSpan::single(self.compiler.alloc_noop()),
        })
    }

    fn parse_statement(&mut self) -> CompileResult<IRSpan<'a>> {
        if self.is_keyword("var") {
            self.parse_var_declaration()
        } else if self.is_keyword("loop") {
            self.parse_loop()
        } else if self.is_keyword("until") {
            self.parse_until()
        } else if self.is_keyword("break") {
            self.parse_break()
        } else if self.is_keyword("continue") {
            self.parse_continue()
        } else if self.is_keyword("return") {
            self.parse_return()
        } else if self.is_keyword("if") {
            self.parse_if()
        } else if self.is_keyword("await") {
            self.parse_await()
        } else if self.is_keyword("yield") {
            self.parse_yield()
        } else if self.peek() == Some('/') {
            self.parse_regex_stmt()
        } else if self.peek().is_some_and(Self::is_ident_start) {
            self.parse_identifier_stmt()
        } else {
            self.mark();
            raise!(self, "unexpected token")
        }
    }

    fn parse_loop(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("loop")?;

        let loop_start = self.compiler.alloc_noop();
        let loop_exit = self.compiler.alloc_noop();
        self.parse_until_impl(loop_start, loop_start, loop_exit)
    }

    fn parse_until(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("until")?;

        let re = self.parse_regex_match()?;

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

        self.context.push(
            self.compiler.arena,
            Context {
                loop_start: Some(loop_start),
                loop_exit: Some(loop_exit),
                capture_groups: BVec::empty(),
            },
        );
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
        let fast_skip = if interesting.covers_all() {
            advance_check
        } else {
            let mut skip_charset = interesting.clone();
            skip_charset.invert();
            let skip_charset = self.compiler.intern_charset(&skip_charset);

            self.compiler.alloc_ir(IR {
                next: Some(advance_check),
                instr: IRI::If {
                    condition: Condition::Charset { cs: skip_charset, min: 1, max: u32::MAX },
                    then: advance_check,
                },
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
        self.expect_keyword("break")?;
        self.expect(';')?;

        if let Some(exit) = self.context.last_mut().and_then(|ctx| ctx.loop_exit) {
            let ir = self.compiler.alloc_noop();
            ir.borrow_mut().set_next(exit);
            Ok(IRSpan::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_continue(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("continue")?;
        self.expect(';')?;

        if let Some(start) = self.context.last_mut().and_then(|ctx| ctx.loop_start) {
            let ir = self.compiler.alloc_noop();
            ir.borrow_mut().set_next(start);
            Ok(IRSpan::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_return(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("return")?;
        self.expect(';')?;
        Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Return)))
    }

    fn parse_if(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("if")?;

        let last = self.compiler.alloc_noop();
        let mut first: Option<IRCell<'a>> = None;
        let mut prev_bad: Option<IRCell<'a>> = None;

        loop {
            let cond = self.parse_condition()?;

            // Chain: previous else-branch -> this condition's entry.
            if let Some(prev) = prev_bad {
                prev.borrow_mut().set_next(cond.src);
            }
            if first.is_none() {
                first = Some(cond.src);
            }

            // Push context with capture groups for the block.
            let (loop_start, loop_exit) = self
                .context
                .last()
                .map(|ctx| (ctx.loop_start, ctx.loop_exit))
                .unwrap_or((None, None));
            self.context.push(
                self.compiler.arena,
                Context { loop_start, loop_exit, capture_groups: cond.capture_groups },
            );
            let bl = self.parse_block()?;
            self.context.pop();

            // Good path -> block -> join.
            cond.dst_good.borrow_mut().set_next(bl.first);
            if let mut bl_last = bl.last.borrow_mut()
                && bl_last.wants_next()
            {
                bl_last.set_next(last);
            }

            // No else? Bad path -> join.
            if !self.is_keyword("else") {
                cond.dst_bad.borrow_mut().set_next(last);
                break;
            }

            self.pos += 4; // eat "else"

            if !self.is_keyword("if") {
                // Plain else block.
                let else_bl = self.parse_block()?;
                cond.dst_bad.borrow_mut().set_next(else_bl.first);
                if let mut else_last = else_bl.last.borrow_mut()
                    && else_last.wants_next()
                {
                    else_last.set_next(last);
                }
                break;
            }

            // else if — loop around.
            self.expect_keyword("if")?;
            prev_bad = Some(cond.dst_bad);
        }

        Ok(IRSpan { first: first.unwrap(), last })
    }

    /// Parse a condition: either a regex `/pattern/` or a comparison `lhs OP rhs`.
    /// Returns a `CondSpan` whose `dst_good`/`dst_bad` are ready for the caller to wire up.
    fn parse_condition(&mut self) -> CompileResult<CondSpan<'a>> {
        if self.peek() == Some('/') { self.parse_regex_match() } else { self.parse_comparison() }
    }

    /// Parse a comparison condition: `ident OP ident`.
    fn parse_comparison(&mut self) -> CompileResult<CondSpan<'a>> {
        let lhs_name = self.read_identifier()?;
        let lhs_vreg = self.get_variable(lhs_name)?;

        self.mark();
        let op = if self.is_str("==") {
            self.pos += 2;
            ComparisonOp::Eq
        } else if self.is_str("!=") {
            self.pos += 2;
            ComparisonOp::Ne
        } else if self.is_str("<=") {
            self.pos += 2;
            ComparisonOp::Le
        } else if self.is_str(">=") {
            self.pos += 2;
            ComparisonOp::Ge
        } else if self.is_str("<") {
            self.pos += 1;
            ComparisonOp::Lt
        } else if self.is_str(">") {
            self.pos += 1;
            ComparisonOp::Gt
        } else {
            raise!(self, "expected comparison operator (==, !=, <, >, <=, >=)")
        };

        let rhs_name = self.read_identifier()?;
        let rhs_vreg = self.get_variable(rhs_name)?;

        let dst_good = self.compiler.alloc_noop();
        let dst_bad = self.compiler.alloc_noop();
        let src = self.compiler.alloc_ir(IR {
            next: Some(dst_bad),
            instr: IRI::If {
                condition: Condition::Cmp { lhs: lhs_vreg, rhs: rhs_vreg, op },
                then: dst_good,
            },
            offset: usize::MAX,
        });

        Ok(CondSpan { src, dst_good, dst_bad, capture_groups: BVec::empty() })
    }

    /// Parse a regex match expression `/pattern/`.
    /// Self-contained: saves offset before, restores on failure.
    /// The existing optimizer coalesces redundant save/restore chains in if/else if.
    fn parse_regex_match(&mut self) -> CompileResult<CondSpan<'a>> {
        let pattern = self.read_regex()?;
        let dst_good = self.compiler.alloc_noop();
        let nfa_bad = self.compiler.alloc_noop();
        let (nfa_src, capture_groups) =
            match regex::parse(self.compiler, pattern, dst_good, nfa_bad) {
                Ok(result) => result,
                Err(err) => raise!(self, "{}", err),
            };

        // Save offset before the NFA, restore on failure.
        let save_reg = self.compiler.alloc_vreg();
        let src = self.compiler.alloc_iri(IRI::Mov {
            dst: save_reg,
            src: self.compiler.get_reg(Register::InputOffset),
        });
        src.borrow_mut().set_next(nfa_src);

        let dst_bad = self.compiler.alloc_iri(IRI::Mov {
            dst: self.compiler.get_reg(Register::InputOffset),
            src: save_reg,
        });
        nfa_bad.borrow_mut().set_next(dst_bad);

        Ok(CondSpan { src, dst_good, dst_bad, capture_groups })
    }

    fn parse_await(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("await")?;

        let ident = self.read_identifier()?;
        if ident != "input" {
            raise!(self, "expected 'input' after await");
        }

        self.expect(';')?;

        let ir = self.compiler.alloc_iri(IRI::AwaitInput);
        Ok(IRSpan::single(ir))
    }

    fn parse_yield(&mut self) -> CompileResult<IRSpan<'a>> {
        self.expect_keyword("yield")?;

        // Check if this is a capture group reference: yield $n as color
        if self.peek() == Some('$') {
            self.pos += 1;
            let capture_index = self.read_integer()? as usize - 1;

            // Expect "as"
            let kw = self.read_identifier()?;
            if kw != "as" {
                raise!(self, "expected 'as' after capture group reference");
            }

            let color = self.read_identifier()?;
            let kind = self.compiler.intern_highlight_kind(color).value;
            self.expect(';')?;

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

            let hs_preg = self.compiler.get_reg(Register::HighlightStart);
            let off_preg = self.compiler.get_reg(Register::InputOffset);
            let off_vreg = self.compiler.alloc_vreg();
            let kind_vreg = self.compiler.alloc_vreg();

            let span = self
                .compiler
                .build_chain()
                // Save offset
                .append(IRI::Mov { dst: off_vreg, src: off_preg })
                // Set start/end temporarily
                .append(IRI::Mov { dst: hs_preg, src: start_vreg })
                .append(IRI::Mov { dst: off_preg, src: end_vreg })
                // Highlight!
                .append(IRI::MovKind { dst: kind_vreg, kind })
                .append(IRI::Flush { kind: kind_vreg })
                // Restore offset
                .append(IRI::Mov { dst: off_preg, src: off_vreg })
                .build();
            Ok(span)
        } else {
            // Normal yield: yield color;
            let color = self.read_identifier()?;
            let kind = self.compiler.intern_highlight_kind(color).value;

            self.expect(';')?;

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
        self.expect_keyword("var")?;

        let name = self.read_identifier()?;
        self.expect('=')?;
        let var_vreg = self.compiler.alloc_vreg();
        let span = self.parse_expression_into(var_vreg)?;
        self.expect(';')?;

        match self.variables.entry(name) {
            Entry::Vacant(e) => _ = e.insert(var_vreg),
            Entry::Occupied(_) => raise!(self, "variable '{}' already declared", name),
        }

        Ok(span)
    }

    fn parse_identifier_stmt(&mut self) -> CompileResult<IRSpan<'a>> {
        let name = self.read_identifier()?;

        match self.peek() {
            // foo();
            Some('(') => {
                self.pos += 1;
                self.expect(')')?;
                self.expect(';')?;
                let name = self.compiler.strings.intern(self.compiler.arena, name);
                Ok(IRSpan::single(self.compiler.alloc_iri(IRI::Call { name })))
            }
            // foo = expr;
            Some('=') if !self.is_str("==") => {
                self.pos += 1;
                let dst = self.get_variable(name)?;
                let span = self.parse_expression_into(dst)?;
                self.expect(';')?;
                Ok(span)
            }
            // foo += expr;
            Some('+') if self.is_str("+=") => {
                let lhs_vreg = self.get_variable(name)?;
                self.pos += 2;

                let val = self.read_integer()?;
                self.expect(';')?;

                let ir = self.compiler.alloc_iri(IRI::AddImm { dst: lhs_vreg, imm: val });
                self.variables.insert(name, lhs_vreg);
                Ok(IRSpan::single(ir))
            }
            _ => {
                self.mark();
                raise!(self, "expected '(', '=' or '+=' after identifier")
            }
        }
    }

    /// Standalone regex statement: `/pattern/;`
    /// Side effect: advances offset on match, does nothing on mismatch.
    fn parse_regex_stmt(&mut self) -> CompileResult<IRSpan<'a>> {
        let re = self.parse_regex_match()?;
        self.expect(';')?;

        let last = self.compiler.alloc_noop();
        re.dst_good.borrow_mut().set_next(last);
        re.dst_bad.borrow_mut().set_next(last);
        Ok(IRSpan { first: re.src, last })
    }

    /// Destination-driven expression codegen: emits code that places the
    /// expression result directly into `dst`, avoiding redundant copies.
    fn parse_expression_into(&mut self, dst: IRRegCell<'a>) -> CompileResult<IRSpan<'a>> {
        self.mark();
        let span = match self.peek() {
            Some('0'..='9') => {
                let val = self.read_integer()?;
                IRSpan::single(self.compiler.alloc_iri(IRI::MovImm { dst, imm: val }))
            }
            Some('/') => {
                // Regex expression: materializes 1 (match) or 0 (no match) into dst.
                let re = self.parse_regex_match()?;
                let last = self.compiler.alloc_noop();

                let good = self.compiler.alloc_iri(IRI::MovImm { dst, imm: 1 });
                good.borrow_mut().set_next(last);
                re.dst_good.borrow_mut().set_next(good);

                let bad = self.compiler.alloc_iri(IRI::MovImm { dst, imm: 0 });
                bad.borrow_mut().set_next(last);
                re.dst_bad.borrow_mut().set_next(bad);

                IRSpan { first: re.src, last }
            }
            Some(c) if Self::is_ident_start(c) => {
                let name = self.read_identifier()?;
                let src = self.get_variable(name)?;
                if std::ptr::eq(src, dst) {
                    IRSpan::single(self.compiler.alloc_noop())
                } else {
                    IRSpan::single(self.compiler.alloc_iri(IRI::Mov { dst, src }))
                }
            }
            _ => raise!(self, "expected integer, identifier, or regex in expression"),
        };

        // Check for binary operator
        if self.peek() == Some('+') && !self.is_str("+=") {
            self.pos += 1;
            let val = self.read_integer()?;
            let add_ir = self.compiler.alloc_iri(IRI::AddImm { dst, imm: val });
            span.last.borrow_mut().set_next(add_ir);
            Ok(IRSpan { first: span.first, last: add_ir })
        } else {
            Ok(span)
        }
    }

    fn get_variable(&self, name: &str) -> CompileResult<IRRegCell<'a>> {
        match self.variables.get(name) {
            Some(&reg) => Ok(reg),
            None => raise!(self, "undefined variable '{}'", name),
        }
    }

    //
    // vvv Tokenization helpers start here vvv
    //

    fn is_ident_start(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn is_ident_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_' || ch == '.'
    }

    fn rest(&self) -> &'src str {
        &self.src[self.pos..]
    }

    fn skip_whitespace_comments(&mut self) {
        loop {
            let rest = self.rest();
            let trimmed = rest.trim_ascii_start();
            self.pos = self.src.len() - trimmed.len();

            if let Some(after_comment) = trimmed.strip_prefix("//") {
                self.pos += 2 + after_comment.find('\n').unwrap_or(after_comment.len());
            } else {
                break;
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.skip_whitespace_comments();
        self.rest().chars().next()
    }

    fn is_str(&self, s: &str) -> bool {
        self.rest().starts_with(s)
    }

    fn is_at_eof(&mut self) -> bool {
        self.skip_whitespace_comments();
        self.pos >= self.src.len()
    }

    /// Mark current position for error reporting.
    fn mark(&mut self) {
        self.skip_whitespace_comments();
        self.token_start = self.pos;
    }

    fn position(&self) -> (usize, usize) {
        let before = &self.src[..self.token_start];
        let line = before.bytes().filter(|&b| b == b'\n').count() + 1;
        let column = before.rfind('\n').map_or(before.len(), |i| before.len() - i - 1) + 1;
        (line, column)
    }

    fn expect(&mut self, ch: char) -> CompileResult<()> {
        self.mark();
        if self.rest().starts_with(ch) {
            self.pos += ch.len_utf8();
            Ok(())
        } else {
            raise!(self, "expected '{}'", ch)
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> CompileResult<()> {
        self.mark();
        if let Some(rest) = self.rest().strip_prefix(kw)
            && !rest.chars().next().is_some_and(Self::is_ident_char)
        {
            self.pos += kw.len();
            Ok(())
        } else {
            raise!(self, "expected '{}'", kw)
        }
    }

    /// Check if next token is keyword (without consuming).
    fn is_keyword(&mut self, kw: &str) -> bool {
        self.skip_whitespace_comments();
        self.rest()
            .strip_prefix(kw)
            .is_some_and(|rest| !rest.chars().next().is_some_and(Self::is_ident_char))
    }

    fn read_identifier(&mut self) -> CompileResult<&'src str> {
        self.mark();
        let start = self.pos;

        if !self.rest().chars().next().is_some_and(Self::is_ident_start) {
            raise!(self, "expected identifier");
        }

        let rest = self.rest();
        let len = rest.find(|c| !Self::is_ident_char(c)).unwrap_or(rest.len());
        self.pos += len;
        Ok(&self.src[start..self.pos])
    }

    fn read_integer(&mut self) -> CompileResult<u32> {
        self.mark();
        let start = self.pos;

        let rest = self.rest();
        let len = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        self.pos += len;

        if start == self.pos {
            raise!(self, "expected integer");
        }

        self.src[start..self.pos].parse().map_err(|_| {
            let path = self.path.to_string();
            let (line, column) = self.position();
            CompileError { path, line, column, message: "invalid integer".to_string() }
        })
    }

    fn read_string(&mut self) -> CompileResult<&'src str> {
        self.mark();
        self.expect('"')?;
        let start = self.pos;
        let end = self.find_closing_delimiter(b'"');
        self.pos = end;
        self.expect('"')?;
        Ok(&self.src[start..end])
    }

    fn read_regex(&mut self) -> CompileResult<&'src str> {
        self.mark();
        self.expect('/')?;
        let start = self.pos;
        let end = self.find_closing_delimiter(b'/');
        self.pos = end;
        self.expect('/')?;
        Ok(&self.src[start..end])
    }

    /// Find unescaped closing delimiter (handles `\x` escapes).
    fn find_closing_delimiter(&self, delim: u8) -> usize {
        let bytes = self.rest().as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'\\' => i += 2,
                b if b == delim => return self.pos + i,
                _ => i += 1,
            }
        }
        self.pos + bytes.len()
    }
}
