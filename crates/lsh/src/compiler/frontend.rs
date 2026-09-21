// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Frontend: DSL source code -> IR graph
//!
//! ## Gotchas
//!
//! - Variables are in SSA-ish form: `var x = off; x = x + 1;` creates a new vreg for the
//!   second `x`. But the variable *name* still maps to the new vreg, so later reads see it.
//! - Physical registers (off, etc.) don't follow SSA. Reading them doesn't create a vreg;
//!   the `parse_expression` function handles this by copying to a fresh vreg.
//! - The `raise!` macro captures token_start position at call time. Don't advance before raising.

use std::collections::HashMap;
use std::collections::hash_map::Entry;

use super::ir::*;
use super::{CompileError, CompileResult, regex};
use crate::runtime::Register;

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

struct RegexSpan {
    pub src: NodeId,
    pub dst_good: NodeId,
    pub dst_bad: NodeId,
    pub capture_groups: Vec<(RegId, RegId)>,
}

struct Context {
    loop_start: Option<NodeId>,
    loop_exit: Option<NodeId>,
    capture_groups: Vec<(RegId, RegId)>,
}

pub struct Parser<'c, 'src> {
    program: &'c mut Program,
    path: &'src str,
    src: &'src str,
    pos: usize,
    token_start: usize,
    context: Vec<Context>,
    variables: HashMap<&'src str, RegId>,
}

impl<'c, 'src> Parser<'c, 'src> {
    pub fn new(program: &'c mut Program, path: &'src str, src: &'src str) -> Self {
        let context = Vec::new();
        Self { program, path, src, pos: 0, token_start: 0, context, variables: Default::default() }
    }

    pub fn run(&mut self) -> CompileResult<()> {
        while !self.is_at_eof() {
            let f = self.parse_function()?;
            self.program.functions.push(f);
        }
        Ok(())
    }

    fn parse_attributes(&mut self) -> CompileResult<FunctionAttributes> {
        let mut attributes = FunctionAttributes::default();

        while self.peek() == Some('#') {
            self.pos += 1;
            self.expect('[')?;

            let key = self.read_identifier()?;
            self.expect('=')?;
            let value = self.read_string()?.to_string();
            self.expect(']')?;

            match key {
                "display_name" => attributes.display_name = Some(value),
                "path" => attributes.paths.push(value),
                _ => raise!(self, "unknown attribute '{}'", key),
            }
        }

        Ok(attributes)
    }

    fn parse_function(&mut self) -> CompileResult<Function> {
        // Reset symbol table for new function
        self.variables = HashMap::from_iter([("off", self.program.get_reg(Register::InputOffset))]);

        let attributes = self.parse_attributes()?;

        let public = if self.is_keyword("pub") {
            self.pos += 3;
            true
        } else {
            false
        };

        self.expect_keyword("fn")?;

        let name = self.read_identifier()?.to_string();

        self.expect('(')?;
        self.expect(')')?;

        let span = self.parse_block()?;

        if self.program.graph[span.last].wants_next() {
            let end = self.program.alloc_iri(Op::Return);
            self.program.graph[span.last].set_next(end);
        }

        Ok(Function { name, attributes, body: span.first, public })
    }

    fn parse_block(&mut self) -> CompileResult<Fragment> {
        self.expect('{')?;

        // TODO: a bit inoptimal to always allocate a noop node
        let mut result: Option<Fragment> = None;

        while !matches!(self.peek(), Some('}') | None) {
            let s = self.parse_statement()?;
            if let Some(span) = &mut result {
                self.program.graph[span.last].set_next(s.first);
                span.last = s.last;
            } else {
                result = Some(s);
            }
        }

        self.expect('}')?;
        Ok(match result {
            Some(span) => span,
            None => Fragment::single(self.program.alloc_noop()),
        })
    }

    fn parse_statement(&mut self) -> CompileResult<Fragment> {
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
        } else if self.peek().is_some_and(Self::is_ident_start) {
            self.parse_identifier_stmt()
        } else {
            self.mark();
            raise!(self, "unexpected token")
        }
    }

    fn parse_loop(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("loop")?;

        let loop_start = self.program.alloc_noop();
        let loop_exit = self.program.alloc_noop();
        self.parse_until_impl(loop_start, loop_start, loop_exit)
    }

    fn parse_until(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("until")?;

        let re = self.parse_if_regex()?;

        let loop_exit = self.program.alloc_noop();
        self.program.graph[re.dst_good].set_next(loop_exit);
        self.parse_until_impl(re.src, re.dst_bad, loop_exit)
    }

    fn parse_until_impl(
        &mut self,
        loop_start: NodeId,
        loop_good: NodeId,
        loop_exit: NodeId,
    ) -> CompileResult<Fragment> {
        // First, save the current input offset.
        // This is used to detect if the loop made any progress.
        let saved_offset = self.program.alloc_vreg();
        let first = self.program.alloc_iri(Op::Mov {
            dst: saved_offset,
            src: self.program.get_reg(Register::InputOffset),
        });

        self.context.push(Context {
            loop_start: Some(loop_start),
            loop_exit: Some(loop_exit),
            capture_groups: Vec::new(),
        });
        let block = self.parse_block()?;
        self.context.pop();

        // Force advance the input offset by 1 if it got stuck in the loop iteration.
        //   if input_offset == saved_offset {
        //       input_offset += 1;
        //   }
        let advance = self.program.alloc_ir(Node {
            next: Some(first),
            instr: Op::AddImm { dst: self.program.get_reg(Register::InputOffset), imm: 1 },
        });
        let advance_check = self.program.alloc_ir(Node {
            next: Some(first),
            instr: Op::If {
                condition: Condition::Cmp {
                    lhs: self.program.get_reg(Register::InputOffset),
                    rhs: saved_offset,
                    op: ComparisonOp::Eq,
                },
                then: advance,
            },
        });

        // NOTE: It's crucial that we connect the block with the loop before calling collect_interesting_charset,
        // as the until statement's regex is not part of the loop but still counts as an "interesting charset",
        // for the purpose of skipping uninteresting characters.
        self.program.graph[first].set_next(loop_start);
        self.program.graph[loop_good].set_next(block.first);

        // Skip any uninteresting characters before the next loop iteration.
        //   if /.*?/ {}
        let interesting = self.program.collect_interesting_charset(loop_start);
        let fast_skip = if interesting.covers_all() {
            advance_check
        } else {
            let mut skip_charset = interesting.clone();
            skip_charset.invert();
            let skip_charset = self.program.intern_charset(&skip_charset);

            self.program.alloc_ir(Node {
                next: Some(advance_check),
                instr: Op::If {
                    condition: Condition::Charset { cs: skip_charset, min: 1, max: u32::MAX },
                    then: advance_check,
                },
            })
        };

        if let block_last = &mut self.program.graph[block.last]
            && block_last.wants_next()
        {
            block_last.set_next(fast_skip);
        }

        Ok(Fragment { first, last: loop_exit })
    }

    fn parse_break(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("break")?;
        self.expect(';')?;

        if let Some(exit) = self.context.last_mut().and_then(|ctx| ctx.loop_exit) {
            let ir = self.program.alloc_noop();
            self.program.graph[ir].set_next(exit);
            Ok(Fragment::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_continue(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("continue")?;
        self.expect(';')?;

        if let Some(start) = self.context.last_mut().and_then(|ctx| ctx.loop_start) {
            let ir = self.program.alloc_noop();
            self.program.graph[ir].set_next(start);
            Ok(Fragment::single(ir))
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_return(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("return")?;
        self.expect(';')?;
        Ok(Fragment::single(self.program.alloc_iri(Op::Return)))
    }

    fn parse_if(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("if")?;

        if self.peek() == Some('/') {
            self.parse_if_regex_chain()
        } else {
            self.parse_if_comparison()
        }
    }

    fn parse_if_regex_chain(&mut self) -> CompileResult<Fragment> {
        let mut prev: Option<NodeId> = None;

        // First, save the current input offset.
        // This is used to restore the position on failed matches.
        let save_reg = self.program.alloc_vreg();
        let first = self
            .program
            .alloc_iri(Op::Mov { dst: save_reg, src: self.program.get_reg(Register::InputOffset) });

        let last = self.program.alloc_noop();

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
            self.program.graph[prev.unwrap_or(first)].set_next(re.src);

            // Connect the if to the {}.
            self.program.graph[re.dst_good].set_next(bl.first);
            // Connect the end of the {} to the end of the if/else chain.
            if let block_last = &mut self.program.graph[bl.last]
                && block_last.wants_next()
            {
                block_last.set_next(last);
            }

            // The "else" branch of the if needs to restore the position.
            let dst_bad = self.program.alloc_iri(Op::Mov {
                dst: self.program.get_reg(Register::InputOffset),
                src: save_reg,
            });
            self.program.graph[re.dst_bad].set_next(dst_bad);

            // No else branch? dst_bad (= no hit) means we're done, so make that connection.
            if !self.is_keyword("else") {
                self.program.graph[dst_bad].set_next(last);
                break;
            }

            // Gobble the "else" keyword.
            self.pos += 4;

            // The else branch has a block? That's our dst_bad.
            if !self.is_keyword("if") {
                let bl = self.parse_block()?;
                self.program.graph[dst_bad].set_next(bl.first);
                // Connect the end of the {} to the end of the if/else chain.
                if let bl_last = &mut self.program.graph[bl.last]
                    && bl_last.wants_next()
                {
                    bl_last.set_next(last);
                }
                break;
            }

            // Otherwise, we expect an "if" in the next iteration to form an "else if".
            self.expect_keyword("if")?;
            prev = Some(dst_bad);
        }

        Ok(Fragment { first, last })
    }

    fn parse_if_comparison(&mut self) -> CompileResult<Fragment> {
        // Parse: if var1 OP var2 { block } where OP is ==, !=, <, >, <=, >=
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

        let dst_good = self.program.alloc_noop();
        let dst_bad = self.program.alloc_noop();
        let cmp = self.program.alloc_ir(Node {
            next: Some(dst_bad),
            instr: Op::If {
                condition: Condition::Cmp { lhs: lhs_vreg, rhs: rhs_vreg, op },
                then: dst_good,
            },
        });

        let bl = self.parse_block()?;
        self.program.graph[dst_good].set_next(bl.first);

        let end = self.program.alloc_noop();

        // Handle optional else branch
        let next = if self.is_keyword("else") {
            self.pos += 4;

            let else_bl = if self.is_keyword("if") { self.parse_if() } else { self.parse_block() };
            let else_bl = else_bl?;

            if let else_last = &mut self.program.graph[else_bl.last]
                && else_last.wants_next()
            {
                else_last.set_next(end);
            }

            else_bl.first
        } else {
            end
        };

        self.program.graph[dst_bad].set_next(next);
        if let block_last = &mut self.program.graph[bl.last]
            && block_last.wants_next()
        {
            block_last.set_next(end);
        }

        Ok(Fragment { first: cmp, last: end })
    }

    fn parse_if_regex(&mut self) -> CompileResult<RegexSpan> {
        let pattern = self.read_regex()?;
        let dst_good = self.program.alloc_noop();
        let dst_bad = self.program.alloc_noop();
        match regex::parse(self.program, pattern, dst_good, dst_bad) {
            Ok((src, capture_groups)) => Ok(RegexSpan { src, dst_good, dst_bad, capture_groups }),
            Err(err) => raise!(self, "{}", err),
        }
    }

    fn parse_await(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("await")?;

        let ident = self.read_identifier()?;
        if ident != "input" {
            raise!(self, "expected 'input' after await");
        }

        self.expect(';')?;

        let ir = self.program.alloc_iri(Op::AwaitInput);
        Ok(Fragment::single(ir))
    }

    fn parse_yield(&mut self) -> CompileResult<Fragment> {
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
            let kind = self.program.intern_highlight_kind(color).value;
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

            let hs_preg = self.program.get_reg(Register::HighlightStart);
            let off_preg = self.program.get_reg(Register::InputOffset);
            let off_vreg = self.program.alloc_vreg();
            let kind_vreg = self.program.alloc_vreg();

            let span = self
                .program
                .build_chain()
                // Save offset
                .append(Op::Mov { dst: off_vreg, src: off_preg })
                // Set start/end temporarily
                .append(Op::Mov { dst: hs_preg, src: start_vreg })
                .append(Op::Mov { dst: off_preg, src: end_vreg })
                // Highlight!
                .append(Op::MovKind { dst: kind_vreg, kind })
                .append(Op::Flush { kind: kind_vreg })
                // Restore offset
                .append(Op::Mov { dst: off_preg, src: off_vreg })
                .build();
            Ok(span)
        } else {
            // Normal yield: yield color;
            let color = self.read_identifier()?;
            let kind = self.program.intern_highlight_kind(color).value;

            self.expect(';')?;

            let vreg = self.program.alloc_vreg();
            let span = self
                .program
                .build_chain()
                .append(Op::MovKind { dst: vreg, kind })
                .append(Op::Flush { kind: vreg })
                .build();
            Ok(span)
        }
    }

    fn parse_var_declaration(&mut self) -> CompileResult<Fragment> {
        self.expect_keyword("var")?;

        let name = self.read_identifier()?;
        self.expect('=')?;
        let (expr, vreg) = self.parse_expression()?;
        self.expect(';')?;

        match self.variables.entry(name) {
            Entry::Vacant(e) => _ = e.insert(vreg),
            Entry::Occupied(_) => raise!(self, "variable '{}' already declared", name),
        }

        Ok(expr)
    }

    fn parse_identifier_stmt(&mut self) -> CompileResult<Fragment> {
        let name = self.read_identifier()?;

        match self.peek() {
            // foo();
            Some('(') => {
                self.pos += 1;
                self.expect(')')?;
                self.expect(';')?;
                let name = self.program.intern_string(name);
                Ok(Fragment::single(self.program.alloc_iri(Op::Call { name })))
            }
            // foo = expr;
            Some('=') if !self.is_str("==") => {
                self.pos += 1;
                let (expr, vreg) = self.parse_expression()?;
                self.expect(';')?;
                self.variables.insert(name, vreg);
                Ok(expr)
            }
            // foo += expr;
            Some('+') if self.is_str("+=") => {
                let lhs_vreg = self.get_variable(name)?;
                self.pos += 2;

                let val = self.read_integer()?;
                self.expect(';')?;

                let ir = self.program.alloc_iri(Op::AddImm { dst: lhs_vreg, imm: val });
                self.variables.insert(name, lhs_vreg);
                Ok(Fragment::single(ir))
            }
            _ => {
                self.mark();
                raise!(self, "expected '(', '=' or '+=' after identifier")
            }
        }
    }

    fn parse_expression(&mut self) -> CompileResult<(Fragment, RegId)> {
        self.mark();
        let (lhs_span, lhs_vreg) = match self.peek() {
            Some('0'..='9') => {
                let val = self.read_integer()?;
                let vreg = self.program.alloc_vreg();
                let ir = self.program.alloc_iri(Op::MovImm { dst: vreg, imm: val });
                (Fragment::single(ir), vreg)
            }
            Some(c) if Self::is_ident_start(c) => {
                let name = self.read_identifier()?;
                let vreg = self.get_variable(name)?;
                (Fragment::single(self.program.alloc_noop()), vreg)
            }
            _ => raise!(self, "expected integer or identifier in expression"),
        };

        // Check for binary operator
        if self.peek() == Some('+') && !self.is_str("+=") {
            self.pos += 1;

            // Parse right-hand side - only integer literals supported
            let val = self.read_integer()?;
            let add_ir = self.program.alloc_iri(Op::AddImm { dst: lhs_vreg, imm: val });
            self.program.graph[lhs_span.last].set_next(add_ir);
            Ok((Fragment { first: lhs_span.first, last: add_ir }, lhs_vreg))
        } else if lhs_vreg.is_physical() {
            // For expressions of type `var virtual = physical;`, we need to ensure
            // that we actually copy the physical register into a new virtual one.
            // The remaining code assumes single assignment form, while physical registers are permanent.
            let dst = self.program.alloc_vreg();
            let node = self.program.alloc_iri(Op::Mov { dst, src: lhs_vreg });
            Ok((Fragment::single(node), dst))
        } else {
            Ok((lhs_span, lhs_vreg))
        }
    }

    fn get_variable(&self, name: &str) -> CompileResult<RegId> {
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
