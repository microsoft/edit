// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{self};
use std::ops::{Index, IndexMut};
use std::ptr;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};
use stdext::arena::{Arena, ArenaString};

use crate::definitions::*;

trait Intern<'a, T: ?Sized> {
    fn intern(&mut self, arena: &'a Arena, item: &T) -> &'a T;
}

impl<'a> Intern<'a, Charset> for Vec<&'a Charset> {
    fn intern(&mut self, arena: &'a Arena, value: &Charset) -> &'a Charset {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = arena.alloc_uninit().write(value.clone());
            self.push(s);
            s
        }
    }
}

impl<'a> Intern<'a, str> for Vec<&'a str> {
    fn intern(&mut self, arena: &'a Arena, value: &str) -> &'a str {
        if let Some(&s) = self.iter().find(|&&v| v == value) {
            s
        } else {
            let s = ArenaString::from_str(arena, value).leak();
            self.push(s);
            s
        }
    }
}

pub struct Frontend<'a> {
    arena: &'a Arena,
    root: Option<&'a NodeCell<'a>>,
    functions: Vec<(&'a str, &'a NodeCell<'a>)>,
    charsets: Vec<&'a Charset>,
    strings: Vec<&'a str>,
    origin: i32,
}

impl<'a> Frontend<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Frontend {
            arena,
            root: None,
            functions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            origin: -1,
        }
    }

    fn alloc_node(&self, node: Node<'a>) -> &'a NodeCell<'a> {
        self.arena.alloc_uninit().write(NodeCell::new(node))
    }

    fn alloc_intra_node(&self) -> &'a NodeCell<'a> {
        self.alloc_node(Node::Intra(IntraNode {
            edges: Vec::new_in(self.arena),
            coverage: Charset::default(),
            has_root_loop: false,
        }))
    }

    fn alloc_leaf_node(&self, leaf: LeafNode<'a>) -> &'a NodeCell<'a> {
        self.alloc_node(Node::Leaf(leaf))
    }

    pub fn declare_root(&mut self, name: &'static str) {
        let name = ArenaString::from_str(self.arena, name).leak();
        let node = self.alloc_intra_node();
        self.functions.push((name, node));
        if self.root.is_none() {
            self.root = Some(node);
        }
    }

    fn find_root_by_name(&self, name: &str) -> &'a NodeCell<'a> {
        self.functions
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, n)| *n)
            .unwrap_or_else(|| panic!("No such root state: {name}"))
    }

    pub fn parse(&mut self, root: &'static str, rule: &Rule) {
        self.origin += 1;

        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .dot_matches_new_line(true)
            .build()
            .parse(rule.pattern)
            .unwrap();

        let src = self.find_root_by_name(root);
        let dst = self.alloc_leaf_node(match rule.action {
            ActionDefinition::Continue => LeafNode::Pop(0),
            ActionDefinition::Jump(name) => LeafNode::Jump(self.find_root_by_name(name)),
            ActionDefinition::Push(name) => LeafNode::Push(self.find_root_by_name(name)),
            ActionDefinition::Pop => LeafNode::Pop(1),
        });

        // By creating a deferred target node like this:
        //   [node] --- Chars(0) --> Pop
        // we allow later regex to add more transitions to [node].
        //
        // During `simplify_indirections()` we'll then clean those up again, by connection
        // nodes together directly if they're joined by just a `Chars(0)` transition.
        let deferred_dst = self.alloc_intra_node();

        // Here's where the magic happens.
        let deferred_dst = self.transform(HighlightKindOp::None, src, deferred_dst, &hir);

        // Now we connect the [node] --- Chars(0) --> Pop
        self.add_transition(rule.kind, deferred_dst, dst, GraphTest::Chars(0));
    }

    fn transform(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        hir: &Hir,
    ) -> &'a NodeCell<'a> {
        fn is_any_class(class: &ClassBytes) -> bool {
            class.ranges() == [ClassBytesRange::new(0, 255)]
        }

        match hir.kind() {
            HirKind::Empty => self.transform_option(kind, src, dst),
            HirKind::Literal(lit) => self.transform_literal(kind, src, dst, &lit.0),
            HirKind::Class(Class::Bytes(class)) if is_any_class(class) => {
                self.transform_any(kind, src, dst)
            }
            HirKind::Class(Class::Bytes(class)) => self.transform_class(kind, src, dst, class),
            HirKind::Class(Class::Unicode(class)) => {
                self.transform_class(kind, src, dst, &class.to_byte_class().unwrap())
            }
            HirKind::Look(Look::WordEndHalfAscii) => {
                self.transform_class_plus(
                    kind,
                    src,
                    self.alloc_leaf_node(LeafNode::Fallback),
                    &ClassBytes::new(vec![
                        ClassBytesRange::new(b'0', b'9'),
                        ClassBytesRange::new(b'A', b'Z'),
                        ClassBytesRange::new(b'_', b'_'),
                        ClassBytesRange::new(b'a', b'z'),
                    ]),
                );
                self.transform_option(kind, src, dst)
            }
            HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
                (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                    self.transform_any_star(kind, src, dst)
                }
                (0, None, HirKind::Class(Class::Bytes(class))) => {
                    let dst = self.transform_class_plus(kind, src, dst, class);
                    self.transform_option(kind, src, dst);
                    dst
                }
                (0, Some(1), _) => {
                    let dst = self.transform(kind, src, dst, &rep.sub);
                    self.transform_option(kind, src, dst);
                    dst
                }
                (1, None, HirKind::Class(Class::Bytes(class))) => {
                    self.transform_class_plus(kind, src, dst, class)
                }
                _ => panic!("Unsupported HIR: {hir:?}"),
            },
            HirKind::Concat(hirs) if hirs.len() >= 2 => self.transform_concat(kind, src, dst, hirs),
            HirKind::Alternation(hirs) if hirs.len() >= 2 => {
                self.transform_alt(kind, src, dst, hirs)
            }
            _ => panic!("Unsupported HIR: {hir:?}"),
        }
    }

    // string
    fn transform_literal(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        lit: &[u8],
    ) -> &'a NodeCell<'a> {
        let prefix = ArenaString::from_utf8(self.arena, lit).unwrap();
        let prefix = self.strings.intern(self.arena, &prefix);
        self.add_transition(kind, src, dst, GraphTest::Prefix(prefix))
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        class: &ClassBytes,
    ) -> &'a NodeCell<'a> {
        let c = self.class_to_charset(class);
        let c = self.charsets.intern(self.arena, &c);
        self.add_transition(kind, src, dst, GraphTest::Charset(c))
    }

    // [eE]
    fn transform_class(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        class: &ClassBytes,
    ) -> &'a NodeCell<'a> {
        let mut charset = self.class_to_charset(class);
        let mut actual_dst = None;

        for i in 0..256 {
            if !charset[i] {
                continue;
            }

            if i >= 128 {
                panic!("Invalid non-ASCII class character {i}");
            }

            let ch = i as u8;
            let mut str = ArenaString::new_in(self.arena);
            str.push(ch as char);

            // NOTE: Uppercase chars have a lower numeric value than lowercase chars.
            // As such, we need to test for `is_ascii_uppercase`.
            let test = if ch.is_ascii_uppercase()
                && let upper = ch.to_ascii_lowercase() as usize
                && charset[upper]
            {
                charset[upper] = false;
                str.make_ascii_lowercase();
                GraphTest::PrefixInsensitive(self.strings.intern(self.arena, &str))
            } else {
                GraphTest::Prefix(self.strings.intern(self.arena, &str))
            };

            let d = self.add_transition(kind, src, dst, test);
            if !ptr::eq(d, *actual_dst.get_or_insert(d)) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
    ) -> &'a NodeCell<'a> {
        self.add_transition(kind, src, dst, GraphTest::Chars(0))
    }

    // .*
    fn transform_any_star(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
    ) -> &'a NodeCell<'a> {
        self.add_transition(kind, src, dst, GraphTest::Chars(usize::MAX))
    }

    // .
    fn transform_any(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
    ) -> &'a NodeCell<'a> {
        self.add_transition(kind, src, dst, GraphTest::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        hirs: &[Hir],
    ) -> &'a NodeCell<'a> {
        fn check_lowercase_literal(hir: &Hir) -> Option<u8> {
            if let HirKind::Class(Class::Bytes(class)) = hir.kind()
                && let ranges = class.ranges()
                && ranges.len() == 2
                && ranges[0].len() == 1
                && ranges[1].len() == 1
                && let lower_a = ranges[0].start().to_ascii_lowercase()
                && let lower_b = ranges[1].start().to_ascii_lowercase()
                && lower_a == lower_b
            {
                Some(lower_a)
            } else {
                None
            }
        }

        let mut it = hirs.iter().peekable();
        let mut src = src;

        while let Some(hir) = it.next() {
            // Transform [aA][bB][cC] into PrefixInsensitive("abc").
            let prefix_insensitive = check_lowercase_literal(hir).map(|ch| {
                let mut str = ArenaString::new_in(self.arena);
                str.push(ch as char);

                while let Some(next_hir) = it.peek() {
                    if let Some(next_ch) = check_lowercase_literal(next_hir) {
                        str.push(next_ch as char);
                        it.next();
                    } else {
                        break;
                    }
                }

                str.make_ascii_lowercase();
                str
            });

            let more = it.peek().is_some();
            let kind = if more { HighlightKindOp::None } else { kind };
            let dst = if more { self.alloc_intra_node() } else { dst };

            if let Some(str) = prefix_insensitive {
                let str = self.strings.intern(self.arena, &str);
                src = self.add_transition(kind, src, dst, GraphTest::PrefixInsensitive(str));
            } else {
                src = self.transform(kind, src, dst, hir);
            }
        }

        src
    }

    // (a|b)
    fn transform_alt(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        hirs: &[Hir],
    ) -> &'a NodeCell<'a> {
        let mut actual_dst = None;

        for hir in hirs {
            let d = self.transform(kind, src, dst, hir);
            if !ptr::eq(d, *actual_dst.get_or_insert(d)) {
                panic!("Diverging destinations for alternation transformer: {hirs:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // [a-z] -> 256-ary LUT
    fn class_to_charset(&mut self, class: &ClassBytes) -> Charset {
        let mut charset = Charset::default();

        for r in class.iter() {
            charset[r.start() as usize..=r.end() as usize].fill(true);
        }

        // If the class includes \w, we also set any non-ASCII UTF8 starters.
        // That's not how Unicode works, but it simplifies the implementation.
        if [(b'0', b'9'), (b'A', b'Z'), (b'_', b'_'), (b'a', b'z')]
            .iter()
            .all(|&(beg, end)| charset[beg as usize..=end as usize].iter().all(|&b| b))
        {
            charset[0xC2..=0xF4].fill(true);
        }

        charset
    }

    fn add_transition(
        &mut self,
        kind: HighlightKindOp,
        src: &'a NodeCell<'a>,
        dst: &'a NodeCell<'a>,
        test: GraphTest<'a>,
    ) -> &'a NodeCell<'a> {
        let mut src = src.borrow_mut();
        let src = match &mut *src {
            Node::Intra(n) => n,
            Node::Leaf(_) => panic!("Cannot add transition from leaf node"),
        };

        // Check if the edge already exists.
        for t in &src.edges {
            if t.test != test {
                continue;
            }

            if ptr::eq(t.dst, dst) {
                return t.dst;
            }

            panic!("Diverging actions for the same test: {:?} -> {:?} vs {:?}", t.test, t.dst, dst);
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in &src.edges {
            use GraphTest::*;

            if match (t.test, test) {
                (Chars(_), _) => true,
                (Charset(p), Charset(n)) => n.is_superset(p),
                (Charset(p), Prefix(n)) => p.covers_char(n.as_bytes()[0]),
                (Charset(p), PrefixInsensitive(n)) => p.covers_char_insensitive(n.as_bytes()[0]),
                (Prefix(p), Prefix(s)) => s.starts_with(p),
                (PrefixInsensitive(p), Prefix(s) | PrefixInsensitive(s)) => {
                    let s = s.as_bytes();
                    let p = p.as_bytes();
                    p.len() <= s.len() && s[..p.len()].eq_ignore_ascii_case(p)
                }
                _ => false,
            } {
                panic!(
                    "Attempted to add unreachable test: {:?} is encompassed by {:?}",
                    test, t.test
                );
            }
        }

        src.edges.push(Edge { origin: self.origin, test, kind, dst });
        dst
    }

    pub fn finalize(&mut self) {
        self.finalize_compute_charset_coverage();
        self.finalize_add_root_loops();
        self.finalize_add_fallbacks();
        self.finalize_add_rescue_fallbacks();
        self.finalize_simplify_indirections();
    }

    pub fn visit_graph<F: FnMut(&'a NodeCell<'a>)>(&self, mut callback: F) {
        for &(_, root) in &self.functions {
            let mut stack = Vec::new();
            let mut visited = HashSet::new();

            stack.push(root);

            while let Some(node) = stack.pop() {
                if !visited.insert(node as *const _) {
                    continue;
                }

                callback(node);

                if let Node::Intra(n) = &*node.borrow() {
                    stack.extend(n.edges.iter().rev().map(|e| e.dst));
                }
            }
        }
    }

    /// Compute existing charset coverage.
    /// Technically we don't need to do that for the root states.
    fn finalize_compute_charset_coverage(&mut self) {
        self.visit_graph(|n| {
            if let Node::Intra(n) = &mut *n.borrow_mut() {
                for t in &n.edges {
                    match t.test {
                        GraphTest::Chars(_) => n.coverage.fill(true),
                        GraphTest::Charset(c) => n.coverage.merge(c),
                        _ => {}
                    }
                }
            }
        });
    }

    // Compute fast skips for root states & fallback to 1 byte skip.
    // This is different from coverage computation above, because here
    // we want to "skip" any character that can never be matched.
    fn finalize_add_root_loops(&mut self) {
        for &(_, src) in &self.functions {
            let mut src = src.borrow_mut();
            let Node::Intra(src) = &mut *src else {
                unreachable!();
            };

            let mut cs = Charset::no();
            for t in &src.edges {
                match t.test {
                    GraphTest::Chars(_) => {
                        cs.fill(true);
                        break;
                    }
                    GraphTest::Charset(c) => {
                        cs.merge(c);
                    }
                    GraphTest::Prefix(s) => {
                        let ch = s.as_bytes()[0];
                        cs.set(ch, true);
                    }
                    GraphTest::PrefixInsensitive(s) => {
                        let ch = s.as_bytes()[0];
                        cs.set(ch.to_ascii_uppercase(), true);
                        cs.set(ch.to_ascii_lowercase(), true);
                    }
                }
            }

            if !cs.covers_all() {
                cs.invert();

                let cs = self.charsets.intern(self.arena, &cs);
                let dst = self.alloc_leaf_node(LeafNode::Loop);
                src.edges.push(Edge {
                    origin: -1,
                    test: GraphTest::Charset(cs),
                    kind: HighlightKindOp::None,
                    dst,
                });
                src.edges.push(Edge {
                    origin: -1,
                    test: GraphTest::Chars(1),
                    kind: HighlightKindOp::None,
                    dst,
                });
            }
        }
    }

    /// Add fallbacks from earlier regexes to later regexes that they cover.
    fn finalize_add_fallbacks(&mut self) {
        /*const CS_YES: Charset = Charset::yes();

        for &(_, root) in &self.functions {
            Self::visit_graph(root, |n| {
                let Node::Intra(n) = n else {
                    return;
                };

                for t in &n.edges {
                    if t.origin >= 0
                        && matches!(t.test, GraphTest::Chars(_) | GraphTest::Charset(_))
                    {
                        let fallback_cs = match t.test {
                            GraphTest::Chars(_) => &CS_YES,
                            GraphTest::Charset(c) => c,
                            _ => unreachable!(),
                        };

                        Self::visit_graph(t.dst, |n| {})
                    }
                }
            });

            let mut visit;
            visit = |node: &NodeCell<'a>| {
                let mut node = node.borrow_mut();
                let Node::Intra(node) = &mut *node else {
                    return;
                };

                for t in &node.edges {
                    if t.origin >= fallback.origin {
                        continue;
                    }

                    let dst = match &*t.dst.borrow() {
                        Node::Leaf(LeafNode::Fallback) => {
                            t.test = fallback.test;
                            t.dst = fallback.dst;
                             continue;
                        }
                        Node::Intra(dst)
                            // Check if the fallback is a superset of this transition.
                            // This applies recursively, which means we assert that the fallback covers
                            // the entire "path" from the original `fallback.src` down to this state.
                            if match t.test {
                                GraphTest::Chars(0) => true,
                                GraphTest::Chars(_) => fallback_cs.covers_all(),
                                GraphTest::Charset(c) => fallback_cs.is_superset(c),
                                GraphTest::Prefix(s) => fallback_cs.covers_str(&s),
                                GraphTest::PrefixInsensitive(s) => {
                                    fallback_cs.covers_str_insensitive(&s)
                                }
                            } => t.dst,
                        _ => continue,
                    };

                    visit(dst);
                }
            };
            visit(root);
        }

        // We iterate in reverse order, so that "deeper" transitions are processed first.
        for t in self.transitions.indices().rev() {
            let fallback = {
                let t = &self.transitions[t];

                if t.origin < 0 || !matches!(t.test, GraphTest::Chars(_) | GraphTest::Charset(_)) {
                    continue;
                }

                t.clone()
            };

            let fallback_cs = match fallback.test {
                GraphTest::Chars(_) => &CS_YES,
                GraphTest::Charset(c) => c,
                _ => unreachable!(),
            };

            stack.clear();
            stack.push(fallback.src);
            visited.fill(false);

            // Find any states that can be reached from `fallback.src`
            // and lack coverage that this `fallback` can provide.
            while let Some(src) = stack.pop() {
                {
                    let s = &mut self.states[src];

                    // In the first iteration, `src` will be `fallback.src`, but since it
                    // won't be a `is_strict_superset` of itself we won't add it (= good).
                    if fallback_cs.is_strict_superset(&s.coverage) {
                        s.coverage.merge(fallback_cs);
                        new_transitions.push({
                            let mut f = fallback.clone();
                            f.src = src;
                            f
                        });
                    }
                }

                for t in self.transitions.indices() {
                    let t = &mut self.transitions[t];

                    if t.src != src || t.origin >= fallback.origin {
                        continue;
                    }

                    match t.dst {
                        GraphAction::Fallback => {
                            t.test = fallback.test;
                            t.dst = fallback.dst;
                        }
                        GraphAction::Jump(dst) if !visited[dst] => {
                            visited[dst] = true;

                            // Check if the fallback is a superset of this transition.
                            // This applies recursively, which means we assert that the fallback covers
                            // the entire "path" from the original `fallback.src` down to this state.
                            if match t.test {
                                GraphTest::Chars(0) => true,
                                GraphTest::Chars(_) => fallback_cs.covers_all(),
                                GraphTest::Charset(c) => fallback_cs.is_superset(c),
                                GraphTest::Prefix(s) => fallback_cs.covers_str(&s),
                                GraphTest::PrefixInsensitive(s) => {
                                    fallback_cs.covers_str_insensitive(&s)
                                }
                            } {
                                stack.push(dst);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }*/
    }

    /// Add always-matching fallbacks to all remaining ones,
    /// that drop down to a None highlight kind.
    fn finalize_add_rescue_fallbacks(&mut self) {
        self.visit_graph(|n| {
            let Node::Intra(n) = &mut *n.borrow_mut() else {
                return;
            };

            if !n.coverage.covers_all() {
                n.edges.push(Edge {
                    origin: -1,
                    test: GraphTest::Chars(0),
                    kind: HighlightKindOp::None,
                    dst: self.alloc_leaf_node(LeafNode::Pop(0)),
                });
            }

            for t in &mut n.edges {
                let mut dst = t.dst.borrow_mut();
                if matches!(&*dst, Node::Leaf(LeafNode::Fallback)) {
                    *dst = Node::Leaf(LeafNode::Pop(0));
                    t.kind = HighlightKindOp::None;
                }
            }
        });
    }

    // Remove pointless indirections from `add_indirect_target()` like this:
    //   [node] --- Chars(0) --> Pop
    fn finalize_simplify_indirections(&mut self) {
        self.visit_graph(|n| {
            let Node::Intra(n) = &mut *n.borrow_mut() else {
                return;
            };

            for t in &mut n.edges {
                while let Node::Intra(dst) = &*t.dst.borrow()
                    && dst.edges.len() == 1
                    && let dst_t = &dst.edges[0]
                    && matches!(dst_t.test, GraphTest::Chars(0))
                {
                    if dst_t.kind != HighlightKindOp::None {
                        if t.kind != HighlightKindOp::None && t.kind != dst_t.kind {
                            panic!(
                                "Inconsistent highlight kinds for indirection: {:?} vs {:?}",
                                t.kind, dst_t.kind
                            );
                        }
                        t.kind = dst_t.kind;
                    }

                    t.dst = dst_t.dst;
                }
            }
        });
    }
}

pub type NodeCell<'a> = RefCell<Node<'a>>;

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Intra(IntraNode<'a>),
    Leaf(LeafNode<'a>),
}

#[derive(Debug, Clone)]
pub struct IntraNode<'a> {
    pub edges: Vec<Edge<'a>, &'a Arena>,
    /// The sum of chars that transition going out of this state cover.
    coverage: Charset,
    has_root_loop: bool,
}

#[derive(Debug, Clone)]
pub enum LeafNode<'a> {
    Jump(&'a NodeCell<'a>), // change to
    Push(&'a NodeCell<'a>), // push to
    Pop(usize),             // pop this many
    Loop,                   // loop to the start of the state & do an exit check
    Fallback,               // replace with a fallback transition (for look-aheads like \b)
}

#[derive(Debug, Clone)]
pub struct Edge<'a> {
    pub origin: i32,
    pub test: GraphTest<'a>,
    pub kind: HighlightKindOp,
    pub dst: &'a NodeCell<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum GraphTest<'a> {
    Chars(usize),
    Charset(&'a Charset),
    Prefix(&'a str),
    PrefixInsensitive(&'a str),
}

impl PartialEq for GraphTest<'_> {
    fn eq(&self, other: &Self) -> bool {
        use GraphTest::*;

        match (*self, *other) {
            (Chars(a), Chars(b)) => a == b,
            (Charset(a), Charset(b)) => ptr::eq(a, b),
            (Prefix(a), Prefix(b)) => ptr::eq(a, b),
            (PrefixInsensitive(a), PrefixInsensitive(b)) => ptr::eq(a, b),
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Hash)]
pub struct Charset {
    bits: [bool; 256],
}

impl Charset {
    pub const fn no() -> Self {
        Charset { bits: [false; 256] }
    }

    pub const fn yes() -> Self {
        Charset { bits: [true; 256] }
    }

    pub fn fill(&mut self, value: bool) {
        self.bits.fill(value);
    }

    pub fn invert(&mut self) {
        for b in &mut self.bits {
            *b = !*b;
        }
    }

    pub fn set(&mut self, index: u8, value: bool) {
        self.bits[index as usize] = value;
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    pub fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b)
    }

    pub fn covers_char(&self, b: u8) -> bool {
        self.bits[b as usize]
    }

    pub fn covers_char_insensitive(&self, b: u8) -> bool {
        self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
    }

    pub fn covers_str(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| self.bits[b as usize])
    }

    pub fn covers_str_insensitive(&self, s: &str) -> bool {
        s.as_bytes().iter().all(|&b| {
            self.bits[b.to_ascii_uppercase() as usize] && self.bits[b.to_ascii_lowercase() as usize]
        })
    }

    pub fn is_superset(&self, other: &Charset) -> bool {
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            if s && !o {
                return false;
            }
        }
        true
    }

    pub fn is_strict_superset(&self, other: &Charset) -> bool {
        let mut has_extra = false;
        for (&s, &o) in self.bits.iter().zip(other.bits.iter()) {
            if !s && o {
                return false;
            }
            has_extra |= s && !o;
        }
        has_extra
    }
}

impl Default for Charset {
    fn default() -> Self {
        Self::no()
    }
}

impl fmt::Debug for Charset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let show_char = |f: &mut fmt::Formatter<'_>, b: usize| {
            let b = b as u8;
            if b.is_ascii_graphic() {
                let b = b as char;
                write!(f, "{b}")
            } else {
                write!(f, "0x{b:02X}")
            }
        };

        let mut beg = 0;
        let mut first = true;

        write!(f, "[")?;

        while beg < 256 {
            while beg < 256 && !self.bits[beg] {
                beg += 1;
            }
            if beg >= 256 {
                break;
            }

            let mut end = beg;
            while end < 256 && self.bits[end] {
                end += 1;
            }

            if !first {
                write!(f, ", ")?;
            }
            show_char(f, beg)?;
            if end - beg > 1 {
                write!(f, "-")?;
                show_char(f, end - 1)?;
            }

            beg = end;
            first = false;
        }

        write!(f, "]")
    }
}

impl<I> Index<I> for Charset
where
    [bool]: Index<I>,
{
    type Output = <[bool] as Index<I>>::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.bits.index(index)
    }
}

impl<I> IndexMut<I> for Charset
where
    [bool]: IndexMut<I>,
{
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.bits.index_mut(index)
    }
}
