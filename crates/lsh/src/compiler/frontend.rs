// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Frontend2: DSL source code -> basic-block SSA IR
//!
//! This is a rewrite of `frontend.rs` that emits into `ir::FuncBody` using
//! `ssa::SSABuilder` instead of the old IR graph.

use std::collections::HashMap;

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

struct Context2 {
    loop_start: Option<ir::BlockId>,
    loop_exit: Option<ir::BlockId>,
    capture_groups: Vec<(ir::Value, ir::Value)>,
}

pub struct Parser2<'a, 'c, 'src> {
    compiler: &'c mut Compiler<'a>,
    func: ir::FuncBody<'a>,
    ssa: ssa::SSABuilder,
    current_block: ir::BlockId,
    path: &'src str,
    src: &'src str,
    pos: usize,
    token_start: usize,
    context: Vec<Context2>,
    variables: HashMap<&'src str, ir::Variable>,
    next_var: u32,

    /// Completed functions: (name, body, attributes, is_public).
    pub functions: Vec<(&'a str, ir::FuncBody<'a>, FunctionAttributes<'a>, bool)>,
}

impl<'a, 'c, 'src> Parser2<'a, 'c, 'src> {
    pub fn new(compiler: &'c mut Compiler<'a>, path: &'src str, src: &'src str) -> Self {
        let func = ir::FuncBody::new();
        let ssa = ssa::SSABuilder::new();
        let current_block = ir::BlockId(0); // placeholder, overwritten per function
        Self {
            compiler,
            func,
            ssa,
            current_block,
            path,
            src,
            pos: 0,
            token_start: 0,
            context: Vec::new(),
            variables: HashMap::new(),
            next_var: 1, // 0 = OFF
            functions: Vec::new(),
        }
    }

    pub fn run(&mut self) -> CompileResult<()> {
        while !self.is_at_eof() {
            self.parse_function()?;
        }
        Ok(())
    }

    fn alloc_variable(&mut self) -> ir::Variable {
        let v = ir::Variable(self.next_var);
        self.next_var += 1;
        v
    }

    /// Create a new block, declare it in the SSA builder, and return its ID.
    fn create_block(&mut self) -> ir::BlockId {
        let block = self.func.create_block();
        self.ssa.declare_block(block);
        block
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

    fn parse_function(&mut self) -> CompileResult<()> {
        // Reset per-function state.
        self.func = ir::FuncBody::new();
        self.ssa = ssa::SSABuilder::new();
        self.variables = HashMap::new();
        self.next_var = 0;
        self.context.clear();

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

        // Create the entry block.
        let entry = self.create_block();
        self.ssa.seal_block(&mut self.func, entry); // entry has no predecessors
        self.current_block = entry;

        // The physical `off` register is already 0 at function entry.
        // No SSA tracking needed — `off` is managed explicitly via ReadOff/WriteOff.

        self.parse_block_body()?;

        // If the current block is unterminated, emit a return.
        if self.func.block(self.current_block).term.is_none() {
            self.func.set_term(self.current_block, ir::Term::Return);
        }

        let body = std::mem::take(&mut self.func);
        self.functions.push((name, body, attributes, public));
        Ok(())
    }

    fn parse_block_body(&mut self) -> CompileResult<()> {
        self.expect('{')?;

        while !matches!(self.peek(), Some('}') | None) {
            self.parse_statement()?;
        }

        self.expect('}')?;
        Ok(())
    }

    fn parse_statement(&mut self) -> CompileResult<()> {
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

    fn parse_loop(&mut self) -> CompileResult<()> {
        self.expect_keyword("loop")?;

        let loop_header = self.create_block();
        let loop_exit = self.create_block();

        self.func
            .set_term(self.current_block, ir::Term::Jump { target: loop_header, args: vec![] });

        self.current_block = loop_header;
        self.parse_until_impl(loop_header, loop_exit)
    }

    fn parse_until(&mut self) -> CompileResult<()> {
        self.expect_keyword("until")?;

        let loop_header = self.create_block();
        let loop_exit = self.create_block();

        self.func
            .set_term(self.current_block, ir::Term::Jump { target: loop_header, args: vec![] });

        self.current_block = loop_header;

        // Parse the `until` regex condition. The NFA reads physical `off` directly.
        let then_block = self.create_block();
        let else_block = self.create_block();
        self.emit_regex_condition(then_block, else_block)?;

        self.ssa.seal_block(&mut self.func, then_block);
        self.current_block = then_block;
        self.func.set_term(self.current_block, ir::Term::Jump { target: loop_exit, args: vec![] });

        self.ssa.seal_block(&mut self.func, else_block);
        self.current_block = else_block;

        self.parse_until_impl(loop_header, loop_exit)
    }

    fn parse_until_impl(
        &mut self,
        loop_header: ir::BlockId,
        loop_exit: ir::BlockId,
    ) -> CompileResult<()> {
        // Save the physical offset at the start of the loop body.
        // This is used to detect if the loop made any progress.
        let saved_off = self.func.push_inst(self.current_block, ir::InstKind::ReadOff);

        self.context.push(Context2 {
            loop_start: Some(loop_header),
            loop_exit: Some(loop_exit),
            capture_groups: Vec::new(),
        });
        self.parse_block_body()?;
        self.context.pop();

        // If block was terminated (e.g. by break/return), skip the advance check.
        if self.func.block(self.current_block).term.is_some() {
            self.ssa.seal_block(&mut self.func, loop_header);
            self.ssa.seal_block(&mut self.func, loop_exit);
            self.current_block = loop_exit;
            return Ok(());
        }

        // Fast-skip: skip uninteresting characters.
        // The charset skip operates on the physical `off` register directly.
        let interesting = collect_interesting_charset(&self.func, loop_header);
        if !interesting.covers_all() {
            let mut skip_charset = interesting.clone();
            skip_charset.invert();
            let skip_charset = self.compiler.intern_charset(&skip_charset);

            let skip_cont = self.create_block();
            self.func.set_term(
                self.current_block,
                ir::Term::CondBranch {
                    kind: ir::CondKind::Charset { cs: skip_charset, min: 1, max: u32::MAX },
                    then_block: skip_cont,
                    then_args: vec![],
                    else_block: skip_cont,
                    else_args: vec![],
                },
            );
            self.ssa.seal_block(&mut self.func, skip_cont);
            self.current_block = skip_cont;
        }

        // Read the (possibly advanced) physical offset for the advance check.
        let current_off = self.func.push_inst(self.current_block, ir::InstKind::ReadOff);

        // Advance check: if offset didn't change, advance by 1.
        let advance_block = self.create_block();
        let backedge_block = self.create_block();

        self.func.set_term(
            self.current_block,
            ir::Term::CondBranch {
                kind: ir::CondKind::Cmp { lhs: current_off, rhs: saved_off, op: ComparisonOp::Eq },
                then_block: advance_block,
                then_args: vec![],
                else_block: backedge_block,
                else_args: vec![],
            },
        );

        // advance_block: off += 1, then backedge
        self.ssa.seal_block(&mut self.func, advance_block);
        self.current_block = advance_block;
        self.func.push_inst(self.current_block, ir::InstKind::AdvanceOff(1));
        self.func
            .set_term(self.current_block, ir::Term::Jump { target: backedge_block, args: vec![] });

        // backedge_block: jump back to loop header
        self.ssa.seal_block(&mut self.func, backedge_block);
        self.current_block = backedge_block;
        self.func
            .set_term(self.current_block, ir::Term::Jump { target: loop_header, args: vec![] });

        self.ssa.seal_block(&mut self.func, loop_header);
        self.ssa.seal_block(&mut self.func, loop_exit);
        self.current_block = loop_exit;
        Ok(())
    }

    fn parse_break(&mut self) -> CompileResult<()> {
        self.expect_keyword("break")?;
        self.expect(';')?;

        if let Some(exit) = self.context.last().and_then(|ctx| ctx.loop_exit) {
            self.func.set_term(self.current_block, ir::Term::Jump { target: exit, args: vec![] });
            // Create a dead block for any code after break.
            let dead = self.create_block();
            self.ssa.seal_block(&mut self.func, dead);
            self.current_block = dead;
            Ok(())
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_continue(&mut self) -> CompileResult<()> {
        self.expect_keyword("continue")?;
        self.expect(';')?;

        if let Some(start) = self.context.last().and_then(|ctx| ctx.loop_start) {
            self.func.set_term(self.current_block, ir::Term::Jump { target: start, args: vec![] });
            // Create a dead block for any code after continue.
            let dead = self.create_block();
            self.ssa.seal_block(&mut self.func, dead);
            self.current_block = dead;
            Ok(())
        } else {
            raise!(self, "loop control statement outside of a loop")
        }
    }

    fn parse_return(&mut self) -> CompileResult<()> {
        self.expect_keyword("return")?;
        self.expect(';')?;
        self.func.set_term(self.current_block, ir::Term::Return);
        // Create a dead block for any code after return.
        let dead = self.create_block();
        self.ssa.seal_block(&mut self.func, dead);
        self.current_block = dead;
        Ok(())
    }

    fn parse_if(&mut self) -> CompileResult<()> {
        self.expect_keyword("if")?;

        let join_block = self.create_block();

        loop {
            if self.peek() == Some('/') {
                // Regex condition.
                let then_block = self.create_block();
                let else_block = self.create_block();

                let captures = self.emit_regex_condition(then_block, else_block)?;

                // Parse then body.
                self.ssa.seal_block(&mut self.func, then_block);
                self.current_block = then_block;

                // Push context with capture groups.
                let (loop_start, loop_exit) = self
                    .context
                    .last()
                    .map(|ctx| (ctx.loop_start, ctx.loop_exit))
                    .unwrap_or((None, None));
                self.context.push(Context2 { loop_start, loop_exit, capture_groups: captures });
                self.parse_block_body()?;
                self.context.pop();

                if self.func.block(self.current_block).term.is_none() {
                    self.func.set_term(
                        self.current_block,
                        ir::Term::Jump { target: join_block, args: vec![] },
                    );
                }

                // Handle else.
                self.ssa.seal_block(&mut self.func, else_block);
                self.current_block = else_block;
            } else {
                // Comparison condition.
                let then_block = self.create_block();
                let else_block = self.create_block();

                self.emit_comparison_condition(then_block, else_block)?;

                // Parse then body.
                self.ssa.seal_block(&mut self.func, then_block);
                self.current_block = then_block;

                let (loop_start, loop_exit) = self
                    .context
                    .last()
                    .map(|ctx| (ctx.loop_start, ctx.loop_exit))
                    .unwrap_or((None, None));
                self.context.push(Context2 { loop_start, loop_exit, capture_groups: Vec::new() });
                self.parse_block_body()?;
                self.context.pop();

                if self.func.block(self.current_block).term.is_none() {
                    self.func.set_term(
                        self.current_block,
                        ir::Term::Jump { target: join_block, args: vec![] },
                    );
                }

                // Handle else.
                self.ssa.seal_block(&mut self.func, else_block);
                self.current_block = else_block;
            }

            if !self.is_keyword("else") {
                // No else — fall through to join.
                if self.func.block(self.current_block).term.is_none() {
                    self.func.set_term(
                        self.current_block,
                        ir::Term::Jump { target: join_block, args: vec![] },
                    );
                }
                break;
            }

            self.pos += 4; // eat "else"

            if self.is_keyword("if") {
                // else if — continue loop.
                self.expect_keyword("if")?;
                continue;
            }

            // Plain else block.
            self.parse_block_body()?;
            if self.func.block(self.current_block).term.is_none() {
                self.func.set_term(
                    self.current_block,
                    ir::Term::Jump { target: join_block, args: vec![] },
                );
            }
            break;
        }

        self.ssa.seal_block(&mut self.func, join_block);
        self.current_block = join_block;
        Ok(())
    }

    /// Emit a regex condition that branches to `then_block` on match and `else_block` on fail.
    /// Returns capture groups from the regex.
    ///
    /// The physical `off` register is assumed to already be in sync with the SSA value
    /// (via WriteOff at the loop header or function entry). The NFA reads/writes `off`
    /// directly. On match, we ReadOff to capture the new position. On fail, `off` is
    /// unchanged and we re-define OFF to the pre-NFA SSA value.
    fn emit_regex_condition(
        &mut self,
        then_block: ir::BlockId,
        else_block: ir::BlockId,
    ) -> CompileResult<Vec<(ir::Value, ir::Value)>> {
        let pattern = self.read_regex()?;

        // Create sink blocks for the NFA.
        let match_sink = self.create_block();
        let fail_sink = self.create_block();

        let (nfa_entry, captures, needs_save_restore) = match regex::compile_regex(
            &mut self.func,
            &mut self.ssa,
            self.compiler,
            pattern,
            match_sink,
            fail_sink,
        ) {
            Ok(result) => result,
            Err(err) => raise!(self, "{}", err),
        };

        // Only save/restore `off` if the NFA can corrupt it on partial match failure.
        let off_save = if needs_save_restore {
            let val = self.func.push_inst(self.current_block, ir::InstKind::ReadOff);
            Some(val)
        } else {
            None
        };

        // Jump to NFA entry.
        self.func.set_term(self.current_block, ir::Term::Jump { target: nfa_entry, args: vec![] });

        // match_sink: NFA matched, physical `off` is advanced.
        self.ssa.seal_block(&mut self.func, match_sink);
        self.func.set_term(match_sink, ir::Term::Jump { target: then_block, args: vec![] });

        // fail_sink: restore `off` if the NFA could have corrupted it.
        self.ssa.seal_block(&mut self.func, fail_sink);
        if let Some(saved) = off_save {
            self.func.push_inst(fail_sink, ir::InstKind::WriteOff(saved));
        }
        self.func.set_term(fail_sink, ir::Term::Jump { target: else_block, args: vec![] });

        Ok(captures)
    }

    /// Emit a comparison condition: `ident OP ident`.
    fn emit_comparison_condition(
        &mut self,
        then_block: ir::BlockId,
        else_block: ir::BlockId,
    ) -> CompileResult<()> {
        let lhs_name = self.read_identifier()?;
        let lhs = if lhs_name == "off" {
            self.func.push_inst(self.current_block, ir::InstKind::ReadOff)
        } else {
            let lhs_var = self.get_variable(lhs_name)?;
            self.ssa.use_var(&mut self.func, lhs_var, self.current_block)
        };

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
        let rhs = if rhs_name == "off" {
            self.func.push_inst(self.current_block, ir::InstKind::ReadOff)
        } else {
            let rhs_var = self.get_variable(rhs_name)?;
            self.ssa.use_var(&mut self.func, rhs_var, self.current_block)
        };

        self.func.set_term(
            self.current_block,
            ir::Term::CondBranch {
                kind: ir::CondKind::Cmp { lhs, rhs, op },
                then_block,
                then_args: vec![],
                else_block,
                else_args: vec![],
            },
        );

        Ok(())
    }

    fn parse_await(&mut self) -> CompileResult<()> {
        self.expect_keyword("await")?;

        let ident = self.read_identifier()?;
        if ident != "input" {
            raise!(self, "expected 'input' after await");
        }

        self.expect(';')?;

        self.func.push_inst(self.current_block, ir::InstKind::AwaitInput);
        // After await, the runtime resets off=0 for the new line.
        // No SSA tracking needed — the physical register is authoritative.
        Ok(())
    }

    fn parse_yield(&mut self) -> CompileResult<()> {
        self.expect_keyword("yield")?;

        if self.peek() == Some('$') {
            // Capture group yield: yield $n as color
            self.pos += 1;
            let capture_index = self.read_integer()? as usize - 1;

            let kw = self.read_identifier()?;
            if kw != "as" {
                raise!(self, "expected 'as' after capture group reference");
            }

            let color = self.read_identifier()?;
            let kind = self.compiler.intern_highlight_kind(color).value;
            self.expect(';')?;

            let (start_val, end_val) = match self.context.last() {
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

            let block = self.current_block;

            // Save current offset
            let saved_off = self.func.push_inst(block, ir::InstKind::ReadOff);
            // Set highlight start to capture start
            self.func.push_inst(block, ir::InstKind::WriteHs(start_val));
            // Set offset to capture end
            self.func.push_inst(block, ir::InstKind::WriteOff(end_val));
            // Flush with the highlight kind
            let kind_val = self.func.push_inst(block, ir::InstKind::HlKind(kind));
            self.func.push_inst(block, ir::InstKind::Flush(kind_val));
            // Restore offset
            self.func.push_inst(block, ir::InstKind::WriteOff(saved_off));
        } else {
            // Normal yield: yield color;
            let color = self.read_identifier()?;
            let kind = self.compiler.intern_highlight_kind(color).value;
            self.expect(';')?;

            let block = self.current_block;
            let kind_val = self.func.push_inst(block, ir::InstKind::HlKind(kind));
            self.func.push_inst(block, ir::InstKind::Flush(kind_val));
        }

        Ok(())
    }

    fn parse_var_declaration(&mut self) -> CompileResult<()> {
        self.expect_keyword("var")?;

        let name = self.read_identifier()?;
        self.expect('=')?;
        let val = self.parse_expression()?;
        self.expect(';')?;

        if self.variables.contains_key(name) {
            raise!(self, "variable '{}' already declared", name);
        }
        let var = self.alloc_variable();
        self.variables.insert(name, var);
        self.ssa.def_var(var, val, self.current_block);

        Ok(())
    }

    fn parse_identifier_stmt(&mut self) -> CompileResult<()> {
        let name = self.read_identifier()?;

        match self.peek() {
            // foo();
            Some('(') => {
                self.pos += 1;
                self.expect(')')?;
                self.expect(';')?;
                let name = self.compiler.strings.intern(self.compiler.arena, name);
                self.func.push_inst(self.current_block, ir::InstKind::Call(name));
                Ok(())
            }
            // foo = expr;
            Some('=') if !self.is_str("==") => {
                self.pos += 1;
                let val = self.parse_expression()?;
                self.expect(';')?;
                if name == "off" {
                    self.func.push_inst(self.current_block, ir::InstKind::WriteOff(val));
                } else {
                    let var = self.get_variable(name)?;
                    self.ssa.def_var(var, val, self.current_block);
                }
                Ok(())
            }
            // foo += expr;
            Some('+') if self.is_str("+=") => {
                self.pos += 2;
                let imm = self.read_integer()?;
                self.expect(';')?;
                if name == "off" {
                    self.func.push_inst(self.current_block, ir::InstKind::AdvanceOff(imm));
                } else {
                    let var = self.get_variable(name)?;
                    let current_val = self.ssa.use_var(&mut self.func, var, self.current_block);
                    let new_val = self
                        .func
                        .push_inst(self.current_block, ir::InstKind::AddImm(current_val, imm));
                    self.ssa.def_var(var, new_val, self.current_block);
                }
                Ok(())
            }
            _ => {
                self.mark();
                raise!(self, "expected '(', '=' or '+=' after identifier")
            }
        }
    }

    /// Standalone regex statement: `/pattern/;`
    /// Advances offset on match, does nothing on mismatch.
    fn parse_regex_stmt(&mut self) -> CompileResult<()> {
        let join_block = self.create_block();
        let match_block = self.create_block();
        let fail_block = self.create_block();

        let _captures = self.emit_regex_condition(match_block, fail_block)?;

        // match: fall through to join
        self.ssa.seal_block(&mut self.func, match_block);
        self.current_block = match_block;
        // (offset already updated in emit_regex_condition's match_sink)

        self.expect(';')?;

        self.func.set_term(self.current_block, ir::Term::Jump { target: join_block, args: vec![] });

        // fail: fall through to join
        self.ssa.seal_block(&mut self.func, fail_block);
        self.func.set_term(fail_block, ir::Term::Jump { target: join_block, args: vec![] });

        self.ssa.seal_block(&mut self.func, join_block);
        self.current_block = join_block;
        Ok(())
    }

    /// Parse an expression, returning the SSA value it produces.
    fn parse_expression(&mut self) -> CompileResult<ir::Value> {
        self.mark();
        let val = match self.peek() {
            Some('0'..='9') => {
                let imm = self.read_integer()?;
                Ok(self.func.push_inst(self.current_block, ir::InstKind::Imm(imm)))
            }
            Some('/') => {
                // Regex as expression: materializes 1 (match) or 0 (no match).
                let match_block = self.create_block();
                let fail_block = self.create_block();
                let join_block = self.create_block();

                let _captures = self.emit_regex_condition(match_block, fail_block)?;

                // match -> imm 1 -> join
                self.ssa.seal_block(&mut self.func, match_block);
                let one = self.func.push_inst(match_block, ir::InstKind::Imm(1));
                self.func
                    .set_term(match_block, ir::Term::Jump { target: join_block, args: vec![] });

                // fail -> imm 0 -> join
                self.ssa.seal_block(&mut self.func, fail_block);
                let zero = self.func.push_inst(fail_block, ir::InstKind::Imm(0));
                self.func.set_term(fail_block, ir::Term::Jump { target: join_block, args: vec![] });

                // join: use a variable to merge the two values via SSA phi
                self.ssa.seal_block(&mut self.func, join_block);
                let result_var = self.alloc_variable();
                self.ssa.def_var(result_var, one, match_block);
                self.ssa.def_var(result_var, zero, fail_block);
                self.current_block = join_block;
                let result = self.ssa.use_var(&mut self.func, result_var, join_block);
                Ok(result)
            }
            Some(c) if Self::is_ident_start(c) => {
                let name = self.read_identifier()?;
                if name == "off" {
                    Ok(self.func.push_inst(self.current_block, ir::InstKind::ReadOff))
                } else {
                    let var = self.get_variable(name)?;
                    Ok(self.ssa.use_var(&mut self.func, var, self.current_block))
                }
            }
            _ => raise!(self, "expected integer, identifier, or regex in expression"),
        }?;

        // Check for binary operator: + imm
        if self.peek() == Some('+') && !self.is_str("+=") {
            self.pos += 1;
            let imm = self.read_integer()?;
            Ok(self.func.push_inst(self.current_block, ir::InstKind::AddImm(val, imm)))
        } else {
            Ok(val)
        }
    }

    fn get_variable(&self, name: &str) -> CompileResult<ir::Variable> {
        match self.variables.get(name) {
            Some(&var) => Ok(var),
            None => raise!(self, "undefined variable '{}'", name),
        }
    }

    //
    // vvv Tokenization helpers (copied verbatim from frontend.rs) vvv
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

/// Collect all "interesting" characters from conditions reachable from `start_block`.
///
/// Returns a charset where `true` = a character that some regex condition in the loop
/// body might match. The fast-skip skips over the inverted charset (chars no condition
/// cares about), preventing the loop from spinning on unmatched input.
fn collect_interesting_charset(func: &ir::FuncBody<'_>, start_block: ir::BlockId) -> Charset {
    use std::collections::{HashSet, VecDeque};

    let mut charset = Charset::no();
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(start_block);

    while let Some(block_id) = queue.pop_front() {
        if !visited.insert(block_id) {
            continue;
        }

        let block = func.block(block_id);
        match &block.term {
            Some(ir::Term::CondBranch { kind, then_block, else_block, .. }) => {
                match kind {
                    ir::CondKind::Charset { cs, .. } => charset.merge(cs),
                    ir::CondKind::Prefix(s) | ir::CondKind::PrefixInsensitive(s) => {
                        if let Some(&b) = s.as_bytes().first() {
                            charset.set(b, true);
                            if matches!(kind, ir::CondKind::PrefixInsensitive(_)) {
                                charset.set(b.to_ascii_uppercase(), true);
                                charset.set(b.to_ascii_lowercase(), true);
                            }
                        }
                    }
                    ir::CondKind::Cmp { .. } | ir::CondKind::EndOfLine => {}
                }

                // Follow the else (fail) path — that continues the condition chain.
                // Skip the then (match) path — that enters the if-body which is irrelevant
                // for the fast-skip charset.
                // Exception: if then == else (e.g. optional charset `a?`), follow both.
                if then_block == else_block {
                    queue.push_back(*then_block);
                } else {
                    queue.push_back(*else_block);
                }
            }
            Some(ir::Term::Jump { target, .. }) => {
                queue.push_back(*target);
            }
            _ => {}
        }
    }

    charset
}
