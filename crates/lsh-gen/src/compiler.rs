// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use super::*;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub struct CompileError {
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error at {}:{}: {}", self.line, self.column, self.message)
    }
}

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
                    IRI::Add { dst, src, imm } => {
                        if dst == Register::Zero && src == Register::Zero && imm == 0 {
                            _ = write!(output, "[noop]");
                        } else if dst == Register::HighlightKind && src == Register::Zero {
                            _ = write!(output, "[hk = {:?}]", unsafe {
                                HighlightKind::from_usize(imm)
                            });
                        } else {
                            _ = write!(output, "[{} = ", dst.mnemonic());
                            if src != Register::Zero {
                                _ = write!(output, "{} + ", src.mnemonic());
                            }
                            if imm == usize::MAX {
                                _ = write!(output, "max]");
                            } else {
                                _ = write!(output, "{imm}]");
                            }
                        }
                    }
                    IRI::If { condition, then } => {
                        match condition {
                            Condition::Charset(cs) => {
                                _ = writeln!(output, "{{charset: {cs:?}}}");
                            }
                            Condition::Prefix(s) => {
                                _ = writeln!(output, "{{match: {s}}}");
                            }
                            Condition::PrefixInsensitive(s) => {
                                _ = writeln!(output, "{{imatch: {s}}}");
                            }
                        }
                        _ = writeln!(output, "    N{node_cell:p} -->|yes| N{then:p}");
                        to_visit.push(then);
                    }
                    IRI::Call { name } => {
                        _ = write!(output, "[Call {name}]");
                    }
                    IRI::Return => {
                        _ = write!(output, "[return]");
                    }
                    IRI::Flush => {
                        _ = write!(output, "[flush]");
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    Zero,
    ProgramCounter,
    ProcedureStart,
    InputOffset,
    HighlightStart,
    HighlightKind,
}

impl Register {
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Register::Zero => "zero",
            Register::ProgramCounter => "pc",
            Register::ProcedureStart => "ps",
            Register::InputOffset => "off",
            Register::HighlightStart => "hs",
            Register::HighlightKind => "hk",
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct Registers {
    pub zero: u32, // Zero
    pub pc: u32,   // ProgramCounter
    pub ps: u32,   // ProcedureStart
    pub off: u32,  // InputOffset
    pub hs: u32,   // HighlightStart
    pub hk: u32,   // HighlightKind
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // Encoding:
    //   imm[31:12] | src[11:8] | dst[7:4] | 0000
    //
    // NOTE: This allows for jumps by manipulating Register::ProgramCounter.
    Add { dst: Register, src: Register, imm: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0001
    //
    // NOTE: The VM takes care of saving the return address.
    Call { dst: usize },

    // Encoding:
    //                                       0010
    Return,

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0011
    //
    // Jumps to `dst` if the test succeeds.
    // `idx` specifies the charset/string to use.
    JumpIfMatchCharset { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0100
    JumpIfMatchPrefix { idx: usize, dst: usize },

    // Encoding:
    //   dst[31:12] |      idx[11:4]       | 0101
    JumpIfMatchPrefixInsensitive { idx: usize, dst: usize },

    // Encoding:
    //                                       0110
    //
    // Flushes the current HighlightKind to the output.
    FlushHighlight,

    // Encoding:
    //   dst[31:12] |                      | 0111
    //
    // Checks if we're at the end and exit if so.
    // Otherwise, jumps to `dst`.
    Loop { dst: usize },
}

impl Instruction {
    const IMM_MAX: usize = (1 << 20) - 1;

    #[allow(clippy::identity_op)]
    pub fn encode(&self) -> u32 {
        match *self {
            Instruction::Add { dst, src, imm } => {
                Self::cast_imm(imm) | (src as u32) << 8 | (dst as u32) << 4 | 0b0000
            }
            Instruction::Call { dst } => Self::cast_imm(dst) | 0b0001,
            Instruction::Return => 0b0010,
            Instruction::JumpIfMatchCharset { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0011
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0100
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
                Self::cast_imm(dst) | (idx as u32) << 4 | 0b0101
            }
            Instruction::FlushHighlight => 0b0110,
            Instruction::Loop { dst } => Self::cast_imm(dst) | 0b0111,
        }
    }

    pub fn mnemonic(&self) -> String {
        match *self {
            Instruction::Add { dst, src, imm } => {
                let mut str = String::with_capacity(48);
                _ = write!(str, "add   {}, {}, ", dst.mnemonic(), src.mnemonic());
                if imm > 1024 * 1024 {
                    _ = write!(str, "{:#x}", imm & Self::IMM_MAX);
                } else {
                    _ = write!(str, "{imm}");
                }
                str
            }
            Instruction::Call { dst } => format!("call  {dst}"),
            Instruction::Return => "ret".to_string(),
            Instruction::JumpIfMatchCharset { idx, dst } => {
                format!("jc    {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefix { idx, dst } => {
                format!("jp    {idx:?}, {dst}")
            }
            Instruction::JumpIfMatchPrefixInsensitive { idx, dst } => {
                format!("jpi   {idx:?}, {dst}")
            }
            Instruction::FlushHighlight => "flush".to_string(),
            Instruction::Loop { dst } => {
                format!("loop  {dst}")
            }
        }
    }

    fn cast_imm(imm: usize) -> u32 {
        if imm == usize::MAX {
            (Self::IMM_MAX << 12) as u32
        } else {
            assert!(imm <= Self::IMM_MAX);
            (imm << 12) as u32
        }
    }
}
