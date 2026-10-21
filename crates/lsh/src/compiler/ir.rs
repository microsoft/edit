//! Shared program representation and graph construction, independent of compilation phases.

use std::collections::{HashSet, VecDeque};
use std::fmt::{self, Write as _};

use super::charset::Charset;
use super::index_vec::{IndexType, IndexVec, index_type};
use crate::runtime::Register;

index_type!(pub struct CharsetId(u32));
index_type!(pub struct StringId(u32));

index_type!(pub struct RegId(u32));

impl RegId {
    pub fn is_physical(self) -> bool {
        self.to_usize() < Register::COUNT
    }
}

pub struct Program {
    pub graph: Graph,
    pub register_count: usize,
    pub functions: Vec<Function>,
    pub charsets: IndexVec<CharsetId, Charset>,
    pub strings: IndexVec<StringId, String>,
    pub highlight_kinds: Vec<HighlightKind>,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            register_count: Register::COUNT,
            functions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            highlight_kinds: vec![HighlightKind { identifier: "other".to_string(), value: 0 }],
        }
    }

    pub fn alloc_ir(&mut self, ir: Node) -> NodeId {
        self.graph.nodes.push(ir)
    }

    pub fn alloc_iri(&mut self, instr: Op) -> NodeId {
        self.alloc_ir(Node { next: None, instr })
    }

    pub fn alloc_noop(&mut self) -> NodeId {
        self.alloc_iri(Op::Noop)
    }

    pub fn build_chain<'s>(&'s mut self) -> ChainBuilder<'s> {
        ChainBuilder { program: self, span: None }
    }

    pub fn get_reg(&self, reg: Register) -> RegId {
        RegId::from_usize(reg as usize)
    }

    pub fn alloc_vreg(&mut self) -> RegId {
        let reg = RegId::from_usize(self.register_count);
        self.register_count = self.register_count.checked_add(1).expect("too many registers");
        reg
    }

    pub fn intern_charset(&mut self, charset: &Charset) -> CharsetId {
        let existing = self
            .charsets
            .iter_enumerated()
            .find_map(|(id, value)| (value == charset).then_some(id));
        existing.unwrap_or_else(|| self.charsets.push(charset.clone()))
    }

    pub fn intern_string(&mut self, value: &str) -> StringId {
        let existing = self
            .strings
            .iter_enumerated()
            .find_map(|(id, existing)| (existing == value).then_some(id));
        existing.unwrap_or_else(|| self.strings.push(value.to_string()))
    }

    pub fn intern_highlight_kind(&mut self, identifier: &str) -> &HighlightKind {
        let idx = match self
            .highlight_kinds
            .binary_search_by(|hk| hk.identifier.as_str().cmp(identifier))
        {
            Ok(idx) => idx,
            Err(idx) => {
                let identifier = identifier.to_string();
                let value = self.highlight_kinds.len() as u32;
                self.highlight_kinds.insert(idx, HighlightKind { identifier, value });
                idx
            }
        };
        &self.highlight_kinds[idx]
    }

    pub fn visit_nodes_from(&self, root: NodeId) -> GraphWalker {
        let mut stack = VecDeque::new();
        stack.push_back(root);
        GraphWalker { current: None, stack, visited: Default::default(), follow_then: true }
    }

    /// Same as [`Self::visit_nodes_from`], but doesn't traverse `then` edges.
    /// A `then` target is still visited if some node's `next` edge points at it too.
    fn visit_fallthrough_nodes_from(&self, root: NodeId) -> GraphWalker {
        let mut visitor = self.visit_nodes_from(root);
        visitor.follow_then = false;
        visitor
    }

    /// Collect all "interesting" characters from conditions in a loop body.
    /// Returns a charset where true = interesting character that should be checked.
    pub fn collect_interesting_charset(&self, loop_body: NodeId) -> Charset {
        let mut charset = Charset::no();

        // Only conditions reachable without consuming input matter here,
        // so we don't descend into if bodies. Let's assume:
        //   loop { if /a/ { loop { if /b/ {} } } }
        // The inner loop's inverted charset includes "a", and merging it into the
        // outer loop would cover all characters and defeat fast-skips entirely.
        //
        // Conditions that a preceding optional/alternation can fall through to, as in
        //   if /[ab]?c/ {}
        //   --> if "a" .then -> if "c"
        //              .else -> if "b" .then -> if "c"
        //                              .else -> if "c"
        // are still reached, because `if "c"` sits on the `next` chain.
        let mut visitor = self.visit_fallthrough_nodes_from(loop_body);
        while let Some(node) = visitor.next(&self.graph) {
            let node = &self.graph[node];
            if let Op::If { condition, .. } = node.instr {
                match condition {
                    Condition::Cmp { .. } => {}
                    Condition::EndOfLine => {}
                    Condition::Charset { cs, .. } => {
                        // Merge this charset
                        charset.merge(&self.charsets[cs]);
                    }
                    Condition::Prefix(s) | Condition::PrefixInsensitive(s) => {
                        // First character of the prefix is interesting
                        if let Some(&b) = self.strings[s].as_bytes().first() {
                            charset.set(b, true);
                            if matches!(condition, Condition::PrefixInsensitive(_)) {
                                charset.set(b.to_ascii_uppercase(), true);
                                charset.set(b.to_ascii_lowercase(), true);
                            }
                        }
                    }
                }
            }
        }

        charset
    }
}

index_type!(pub struct NodeId(u32));

pub struct Graph {
    nodes: IndexVec<NodeId, Node>,
}

impl Graph {
    pub fn new() -> Self {
        Self { nodes: IndexVec::new() }
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

impl std::ops::Index<NodeId> for Graph {
    type Output = Node;

    fn index(&self, id: NodeId) -> &Self::Output {
        &self.nodes[id]
    }
}

impl std::ops::IndexMut<NodeId> for Graph {
    fn index_mut(&mut self, id: NodeId) -> &mut Self::Output {
        &mut self.nodes[id]
    }
}

pub struct GraphWalker {
    current: Option<NodeId>,
    stack: VecDeque<NodeId>,
    visited: HashSet<NodeId>,
    follow_then: bool,
}

impl GraphWalker {
    pub fn next(&mut self, graph: &Graph) -> Option<NodeId> {
        if let Some(cell) = self.current.take() {
            {
                let ir = &graph[cell];
                if let Op::If { then, .. } = ir.instr
                    && self.follow_then
                {
                    self.stack.push_back(then);
                }
                if let Some(next) = ir.next {
                    self.stack.push_back(next);
                }
            }
        }

        while let Some(cell) = self.stack.pop_front() {
            if self.visited.insert(cell) {
                self.current = Some(cell);
                return self.current;
            }
        }

        None
    }
}

#[derive(Debug, Clone, Default)]
pub struct FunctionAttributes {
    pub display_name: Option<String>,
    pub paths: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub attributes: FunctionAttributes,
    pub body: NodeId,
    pub public: bool,
}

#[derive(Debug)]
pub struct Node {
    pub next: Option<NodeId>,
    pub instr: Op,
}

impl Node {
    pub fn wants_next(&self) -> bool {
        if self.next.is_some() {
            return false;
        }

        match self.instr {
            Op::Mov { dst, .. } | Op::MovImm { dst, .. }
                if dst.to_usize() == Register::ProgramCounter as usize =>
            {
                false
            }
            Op::Return => false,
            _ => true,
        }
    }

    pub fn set_next(&mut self, n: NodeId) {
        debug_assert!(self.wants_next());
        self.next = Some(n);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Noop,
    Mov { dst: RegId, src: RegId },
    MovImm { dst: RegId, imm: u32 },
    MovKind { dst: RegId, kind: u32 },
    AddImm { dst: RegId, imm: u32 },
    If { condition: Condition, then: NodeId },
    Call { name: StringId },
    Return,
    Flush { kind: RegId },
    AwaitInput,
}

#[derive(Clone, Copy)]
pub struct Fragment {
    pub first: NodeId,
    pub last: NodeId,
}

impl Fragment {
    pub fn single(node: NodeId) -> Self {
        Self { first: node, last: node }
    }
}

pub struct ChainBuilder<'s> {
    program: &'s mut Program,
    span: Option<Fragment>,
}

impl<'s> ChainBuilder<'s> {
    pub fn append(&mut self, instr: Op) -> &mut Self {
        let node = self.program.alloc_iri(instr);
        if let Some(span) = &mut self.span {
            self.program.graph[span.last].set_next(node);
            span.last = node;
        } else {
            self.span = Some(Fragment::single(node));
        }
        self
    }

    pub fn build(&mut self) -> Fragment {
        self.span.unwrap_or_else(|| Fragment::single(self.program.alloc_noop()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Cmp { lhs: RegId, rhs: RegId, op: ComparisonOp },
    EndOfLine,
    Charset { cs: CharsetId, min: u32, max: u32 },
    Prefix(StringId),
    PrefixInsensitive(StringId),
}

#[derive(Clone)]
pub struct HighlightKind {
    pub identifier: String,
    pub value: u32,
}

impl HighlightKind {
    pub fn fmt_camelcase(&self) -> HighlightKindCamelcaseFormatter<'_> {
        HighlightKindCamelcaseFormatter { identifier: &self.identifier }
    }
}

pub struct HighlightKindCamelcaseFormatter<'a> {
    identifier: &'a str,
}

impl<'a> fmt::Display for HighlightKindCamelcaseFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut capitalize_next = true;
        for c in self.identifier.chars() {
            if c == '.' {
                capitalize_next = true;
            } else if capitalize_next {
                capitalize_next = false;
                f.write_char(c.to_ascii_uppercase())?;
            } else {
                f.write_char(c)?;
            }
        }
        Ok(())
    }
}
