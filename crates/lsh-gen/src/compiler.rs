// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use super::*;

pub struct Compiler<'a> {
    pub arena: &'a Arena,
    pub functions: Vec<Function<'a>>,
    pub charsets: Vec<&'a Charset>,
    pub strings: Vec<&'a str>,
}

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            arena,
            functions: Vec::new(),
            charsets: Default::default(),
            strings: Default::default(),
        }
    }

    pub fn parse(&mut self, src: &str) -> CompileResult<()> {
        let mut parser = Parser::new(self, src);
        parser.run()?;
        Ok(())
    }

    pub fn optimize(&mut self) {
        optimizer::optimize(self);
    }

    pub fn assemble(&mut self) -> CompileResult<Assembly<'a>> {
        backend::Backend::new().compile(self)
    }

    pub fn alloc_ir(&self, ir: IR<'a>) -> IRCell<'a> {
        self.arena.alloc_uninit().write(RefCell::new(ir))
    }

    pub fn alloc_iri(&self, instr: IRI<'a>) -> IRCell<'a> {
        self.arena.alloc_uninit().write(RefCell::new(IR { next: None, instr, offset: usize::MAX }))
    }

    pub fn alloc_noop(&self) -> IRCell<'a> {
        self.alloc_iri(IRI::Add { dst: Register::Zero, src: Register::Zero, imm: 0 })
    }

    pub fn chain_iri(&self, prev: IRCell<'a>, instr: IRI<'a>) -> IRCell<'a> {
        let ir = self.arena.alloc_uninit().write(RefCell::new(IR {
            next: None,
            instr,
            offset: usize::MAX,
        }));
        prev.borrow_mut().set_next(ir);
        ir
    }

    pub fn intern_charset(&mut self, charset: &Charset) -> &'a Charset {
        self.charsets.intern(self.arena, charset)
    }

    pub fn intern_string(&mut self, s: &str) -> &'a str {
        self.strings.intern(self.arena, s)
    }

    pub fn visit_nodes_from(&self, root: IRCell<'a>) -> TreeVisitor<'a> {
        TreeVisitor { stack: vec![root], visited: HashSet::new() }
    }

    pub fn as_mermaid(&self) -> String {
        let mut output = String::new();

        // ---
        // config:
        // layout: elk
        // elk:
        //   considerModelOrder: NONE
        // ---
        output.push_str("flowchart TB\n");

        for func in &self.functions {
            _ = writeln!(output, "  subgraph {}", func.name);
            _ = writeln!(output, "    direction TB");
            _ = writeln!(output, "    {}_start@{{shape: start}} --> N{:p}", func.name, func.body);

            let mut visited = HashSet::new();
            let mut to_visit = vec![func.body];

            while let Some(node_cell) = to_visit.pop() {
                if !visited.insert(node_cell.as_ptr()) {
                    continue;
                }

                let node = node_cell.borrow();
                _ = write!(output, "    N{node_cell:p}");

                match node.instr {
                    IRI::Add { dst: Register::Zero, src: Register::Zero, imm: 0 } => {
                        output.push_str("[noop]");
                    }
                    IRI::Add { dst: Register::HighlightKind, src: Register::Zero, imm } => {
                        _ = write!(output, "[\"hk = {:?}\"]", unsafe {
                            HighlightKind::from_usize(imm)
                        });
                    }
                    IRI::Add { dst, src, imm } => {
                        _ = write!(output, "[\"{} = ", dst.mnemonic());
                        match (src, imm) {
                            (Register::Zero, 0) => {
                                _ = write!(output, "0");
                            }
                            (Register::Zero, usize::MAX) => {
                                _ = write!(output, "max");
                            }
                            (Register::Zero, _) => {
                                _ = write!(output, "{imm}");
                            }
                            (_, 0) => {
                                _ = write!(output, "{}", src.mnemonic());
                            }
                            _ => {
                                _ = write!(output, "{} + {}", src.mnemonic(), imm);
                            }
                        }
                        output.push_str("\"]");
                    }
                    IRI::If { condition, then } => {
                        match condition {
                            Condition::Charset(cs) => {
                                _ = writeln!(output, "{{\"charset: {cs:?}\"}}");
                            }
                            Condition::Prefix(s) => {
                                _ = writeln!(output, "{{\"match: {s}\"}}");
                            }
                            Condition::PrefixInsensitive(s) => {
                                _ = writeln!(output, "{{\"imatch: {s}\"}}");
                            }
                        }
                        _ = writeln!(output, "    N{node_cell:p} -->|yes| N{then:p}");
                        to_visit.push(then);
                    }
                    IRI::Call { name } => {
                        _ = write!(output, "[\"Call {name}\"]");
                    }
                    IRI::Return => {
                        output.push_str("[return]");
                    }
                    IRI::Flush => {
                        output.push_str("[flush]");
                    }
                    IRI::Loop { dst } => {
                        _ = write!(output, "[loop] --> N{dst:p}");
                    }
                }

                match node.instr {
                    IRI::If { .. } => {
                        if let Some(next) = node.next {
                            _ = writeln!(output, "    N{node_cell:p} -->|no| N{next:p}");
                        }
                    }
                    _ => {
                        if let Some(next) = node.next {
                            _ = writeln!(output, " --> N{next:p}");
                        } else {
                            _ = writeln!(output);
                        }
                    }
                }

                if let Some(next) = node.next {
                    to_visit.push(next);
                }
            }

            _ = writeln!(output, "  end");
        }

        output
    }
}

pub struct TreeVisitor<'a> {
    stack: Vec<IRCell<'a>>,
    visited: HashSet<*const RefCell<IR<'a>>>,
}

impl<'a> Iterator for TreeVisitor<'a> {
    type Item = IRCell<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(cell) = self.stack.pop() {
            if self.visited.insert(cell) {
                let node = cell.borrow_mut();
                if let IRI::If { then, .. } = node.instr {
                    self.stack.push(then);
                }
                if let Some(next) = node.next {
                    self.stack.push(next);
                }
                return Some(cell);
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub name: &'a str,
    pub body: IRCell<'a>,
    pub public: bool,
}

// To be honest, I don't think this qualifies as an "intermediate representation",
// if we compare this to popular compilers. It's still in a tree representation after all.
// But whatever. It's still intermediate to us.
#[derive(Debug)]
pub struct IR<'a> {
    pub next: Option<IRCell<'a>>,
    pub instr: IRI<'a>,
    pub offset: usize,
}

pub type IRCell<'a> = &'a RefCell<IR<'a>>;

// IRI = Immediate Representation Instruction
#[derive(Debug)]
pub enum IRI<'a> {
    Add { dst: Register, src: Register, imm: usize },
    If { condition: Condition<'a>, then: IRCell<'a> },
    Call { name: &'a str },
    Return,
    Flush,
    Loop { dst: IRCell<'a> },
}

#[derive(Debug, Clone, Copy)]
pub enum Condition<'a> {
    Charset(&'a Charset),
    Prefix(&'a str),
    PrefixInsensitive(&'a str),
}

impl<'a> IR<'a> {
    pub fn wants_next(&self) -> bool {
        self.next.is_none() && !matches!(self.instr, IRI::Return | IRI::Loop { .. })
    }

    pub fn set_next(&mut self, n: IRCell<'a>) {
        debug_assert!(self.next.is_none());
        self.next = Some(n);
    }
}
