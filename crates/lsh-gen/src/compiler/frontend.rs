// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::fmt::{self};
use std::ops::{Index, IndexMut};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};

use crate::definitions::*;
use crate::graph::{DiGraph, EdgeId, NodeId};
use crate::handles::{HandleVec, declare_handle};

declare_handle!(pub CharsetHandle(usize));
declare_handle!(pub StringHandle(usize));
pub struct Frontend {
    graph: DiGraph<GraphState, GraphTransition>,
    root_count: usize,
    charsets: HandleVec<CharsetHandle, Charset>,
    strings: HandleVec<StringHandle, String>,
    origin: i32,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend {
            graph: Default::default(),
            root_count: 0,
            charsets: Default::default(),
            strings: Default::default(),
            origin: -1,
        }
    }

    pub fn declare_root(&mut self, name: &'static str) {
        self.graph.add_node(GraphState { name: Some(name), ..Default::default() });
        self.root_count += 1;
    }

    fn find_root_by_name(&self, name: &str) -> NodeId {
        self.graph
            .iter_nodes()
            .take(self.root_count)
            .find(|n| n.value.name == Some(name))
            .map(|n| n.id)
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
        let action = match rule.action {
            ActionDefinition::Continue => GraphAction::Pop(0),
            ActionDefinition::Jump(name) => GraphAction::Jump(self.find_root_by_name(name)),
            ActionDefinition::Push(name) => GraphAction::Push(self.find_root_by_name(name)),
            ActionDefinition::Pop => GraphAction::Pop(1),
        };

        // By creating a deferred target node like this:
        //   [node] --- Chars(0) --> Pop
        // we allow later regex to add more transitions to [node].
        //
        // During `simplify_indirections()` we'll then clean those up again, by connection
        // nodes together directly if they're joined by just a `Chars(0)` transition.
        let deferred_dst = self.graph.add_node(GraphState::default());

        // Here's where the magic happens.
        let deferred_dst = self.transform(HighlightKindOp::None, src, deferred_dst, &hir);

        // Now we connect the [node] --- Chars(0) --> Pop
        let dst = self.graph.add_node(GraphState { action: Some(action), ..Default::default() });
        self.add_transition(rule.kind, deferred_dst, dst, GraphTest::Chars(0));
    }

    fn transform(&mut self, kind: HighlightKindOp, src: NodeId, dst: NodeId, hir: &Hir) -> NodeId {
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
                let fallback = self.graph.add_node(GraphState {
                    action: Some(GraphAction::Fallback),
                    ..Default::default()
                });
                self.transform_class_plus(
                    kind,
                    src,
                    fallback,
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
        src: NodeId,
        dst: NodeId,
        lit: &[u8],
    ) -> NodeId {
        let prefix = String::from_utf8(lit.to_vec()).unwrap();
        let prefix = self.intern_string(prefix);
        self.add_transition(kind, src, dst, GraphTest::Prefix(prefix))
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        kind: HighlightKindOp,
        src: NodeId,
        dst: NodeId,
        class: &ClassBytes,
    ) -> NodeId {
        let c = self.class_to_charset(class);
        let c = self.intern_charset(&c);
        self.add_transition(kind, src, dst, GraphTest::Charset(c))
    }

    // [eE]
    fn transform_class(
        &mut self,
        kind: HighlightKindOp,
        src: NodeId,
        dst: NodeId,
        class: &ClassBytes,
    ) -> NodeId {
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
            let mut str = String::new();
            str.push(ch as char);

            // NOTE: Uppercase chars have a lower numeric value than lowercase chars.
            // As such, we need to test for `is_ascii_uppercase`.
            let test = if ch.is_ascii_uppercase()
                && let upper = ch.to_ascii_lowercase() as usize
                && charset[upper]
            {
                charset[upper] = false;
                str.make_ascii_lowercase();
                GraphTest::PrefixInsensitive(self.intern_string(str))
            } else {
                GraphTest::Prefix(self.intern_string(str))
            };

            let d = self.add_transition(kind, src, dst, test);
            if d != *actual_dst.get_or_insert(d) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(&mut self, kind: HighlightKindOp, src: NodeId, dst: NodeId) -> NodeId {
        self.add_transition(kind, src, dst, GraphTest::Chars(0))
    }

    // .*
    fn transform_any_star(&mut self, kind: HighlightKindOp, src: NodeId, dst: NodeId) -> NodeId {
        self.add_transition(kind, src, dst, GraphTest::Chars(usize::MAX))
    }

    // .
    fn transform_any(&mut self, kind: HighlightKindOp, src: NodeId, dst: NodeId) -> NodeId {
        self.add_transition(kind, src, dst, GraphTest::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
        kind: HighlightKindOp,
        src: NodeId,
        dst: NodeId,
        hirs: &[Hir],
    ) -> NodeId {
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
                let mut str = String::new();
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
            let dst = if more { self.graph.add_node(GraphState::default()) } else { dst };

            if let Some(str) = prefix_insensitive {
                let str = self.intern_string(str);
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
        src: NodeId,
        dst: NodeId,
        hirs: &[Hir],
    ) -> NodeId {
        let mut actual_dst = None;

        for hir in hirs {
            let d = self.transform(kind, src, dst, hir);
            if d != *actual_dst.get_or_insert(d) {
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
        src: NodeId,
        dst: NodeId,
        test: GraphTest,
    ) -> NodeId {
        // Check if the edge already exists.
        for t in self.graph.get_edges_from(src) {
            if t.test != test {
                continue;
            }

            if t.dst == dst {
                return t.dst;
            }

            panic!("Diverging actions for the same test: {:?} -> {:?} vs {:?}", t.test, t.dst, dst);
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in self.graph.get_edges_from(src) {
            use GraphTest::*;

            if match (t.test, test) {
                (Chars(_), _) => true,
                (Charset(p), Charset(n)) => self.charsets[n].is_superset(&self.charsets[p]),
                (Charset(p), Prefix(n)) => {
                    self.charsets[p].covers_char(self.strings[n].as_bytes()[0])
                }
                (Charset(p), PrefixInsensitive(n)) => {
                    self.charsets[p].covers_char_insensitive(self.strings[n].as_bytes()[0])
                }
                (Prefix(p), Prefix(s)) => self.strings[s].starts_with(self.strings[p].as_str()),
                (PrefixInsensitive(p), Prefix(s) | PrefixInsensitive(s)) => {
                    let s = self.strings[s].as_bytes();
                    let p = self.strings[p].as_bytes();
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

        self.graph.add_edge(
            src,
            dst,
            GraphTransition { origin: self.origin, test, kind, ..Default::default() },
        );

        dst
    }

    fn intern_charset(&mut self, cs: &Charset) -> CharsetHandle {
        if let Some((idx, _)) = self.charsets.enumerate().find(|&(_, v)| v == cs) {
            idx
        } else {
            self.charsets.push(cs.clone())
        }
    }

    fn intern_string(&mut self, string: String) -> StringHandle {
        if let Some((idx, _)) = self.strings.enumerate().find(|&(_, v)| *v == string) {
            idx
        } else {
            self.strings.push(string)
        }
    }

    pub fn finalize(&mut self) {
        self.finalize_compute_charset_coverage();
        self.finalize_add_root_loops();
        self.finalize_add_fallbacks();
        self.finalize_add_rescue_fallbacks();
        self.finalize_simplify_indirections();
        self.finalize_strings();
        self.finalize_charsets();
    }

    /// Compute existing charset coverage.
    /// Technically we don't need to do that for the root states.
    fn finalize_compute_charset_coverage(&mut self) {
        for (t, src) in self.graph.iter_edges_with_src_mut() {
            match t.test {
                GraphTest::Chars(_) => src.coverage.fill(true),
                GraphTest::Charset(c) => src.coverage.merge(&self.charsets[c]),
                _ => {}
            }
        }
    }

    // Compute fast skips for root states & fallback to 1 byte skip.
    // This is different from coverage computation above, because here
    // we want to "skip" any character that can never be matched.
    fn finalize_add_root_loops(&mut self) {
        let mut coverages = vec![Charset::no(); self.root_count];

        for t in self.graph.iter_edges() {
            let Some(cs) = coverages.get_mut(t.src.0) else {
                continue;
            };

            match t.test {
                GraphTest::Chars(_) => {
                    cs.fill(true);
                    break;
                }
                GraphTest::Charset(c) => {
                    cs.merge(&self.charsets[c]);
                }
                GraphTest::Prefix(s) => {
                    let ch = self.strings[s].as_bytes()[0];
                    cs.set(ch, true);
                }
                GraphTest::PrefixInsensitive(s) => {
                    let ch = self.strings[s].as_bytes()[0];
                    cs.set(ch.to_ascii_uppercase(), true);
                    cs.set(ch.to_ascii_lowercase(), true);
                }
            }
        }

        for (i, mut cs) in coverages.into_iter().enumerate() {
            if !cs.covers_all() {
                cs.invert();

                let cs = self.intern_charset(&cs);
                let src = NodeId(i);
                let dst = self
                    .graph
                    .add_node(GraphState { action: Some(GraphAction::Loop), ..Default::default() });
                self.graph.add_edge(
                    src,
                    dst,
                    GraphTransition {
                        origin: -1,
                        test: GraphTest::Charset(cs),
                        kind: HighlightKindOp::None,
                    },
                );
                self.graph.add_edge(
                    src,
                    dst,
                    GraphTransition {
                        origin: -1,
                        test: GraphTest::Chars(1),
                        kind: HighlightKindOp::None,
                    },
                );
            }
        }
    }

    /// Add fallbacks from earlier regexes to later regexes that they cover.
    fn finalize_add_fallbacks(&mut self) {
        const CS_YES: Charset = Charset::yes();
        let mut stack = Vec::new();
        let mut visited = vec![false; self.states.len()];
        let mut new_transitions = Vec::new();

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
                GraphTest::Charset(c) => &self.charsets[c],
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
                        GraphAction::Jump(dst) if !visited[dst.0] => {
                            visited[dst.0] = true;

                            // Check if the fallback is a superset of this transition.
                            // This applies recursively, which means we assert that the fallback covers
                            // the entire "path" from the original `fallback.src` down to this state.
                            if match t.test {
                                GraphTest::Chars(0) => true,
                                GraphTest::Chars(_) => fallback_cs.covers_all(),
                                GraphTest::Charset(c) => fallback_cs.is_superset(&self.charsets[c]),
                                GraphTest::Prefix(s) => fallback_cs.covers_str(&self.strings[s]),
                                GraphTest::PrefixInsensitive(s) => {
                                    fallback_cs.covers_str_insensitive(&self.strings[s])
                                }
                            } {
                                stack.push(dst);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        self.transitions.extend(new_transitions);
    }

    /// Add always-matching fallbacks to all remaining ones,
    /// that drop down to a None highlight kind.
    fn finalize_add_rescue_fallbacks(&mut self) {
        for (src, s) in self.states.enumerate() {
            if !s.coverage.covers_all() {
                self.graph.add_edge(GraphTransition {
                    origin: -1,
                    src,
                    test: GraphTest::Chars(0),
                    kind: HighlightKindOp::None,
                    dst: NodeId::Pop(0),
                });
            }
        }

        for t in &mut self.transitions {
            if t.dst == GraphAction::Fallback {
                t.dst = GraphAction::Pop(0);
                t.kind = HighlightKindOp::None;
            }
        }
    }

    // Remove pointless indirections from `add_indirect_target()` like this:
    //   [node] --- Chars(0) --> Pop
    fn finalize_simplify_indirections(&mut self) {
        let mut src_seen = vec![false; self.states.len()];

        loop {
            let mut any_fixed = false;

            src_seen.fill(false);

            for i in 0..self.transitions.len() {
                let (src, dst, kind) = {
                    let t = &self.transitions[EdgeId(i)];
                    if src_seen[t.src.0] {
                        continue;
                    }

                    src_seen[t.src.0] = true;

                    if t.test != GraphTest::Chars(0) {
                        continue;
                    }

                    (t.src, t.dst, t.kind)
                };

                for t in &mut self.transitions {
                    if t.dst == GraphAction::Jump(src) {
                        if kind != HighlightKindOp::None {
                            if t.kind != HighlightKindOp::None && t.kind != kind {
                                panic!(
                                    "Inconsistent highlight kinds for indirection: {:?} vs {:?}",
                                    t.kind, kind
                                );
                            }
                            t.kind = kind;
                        }

                        t.dst = dst;
                        any_fixed = true;
                    }
                }
            }

            if !any_fixed {
                break;
            }
        }
    }

    fn finalize_strings(&mut self) {
        let mut used = vec![false; self.strings.len()];
        let mut new_indices = vec![StringHandle::MAX; self.strings.len()];

        for t in &self.transitions {
            if let GraphTest::Prefix(s) | GraphTest::PrefixInsensitive(s) = t.test {
                used[s.0] = true;
            }
        }

        let mut old_idx = 0;
        let mut new_idx = 0;
        self.strings.retain(|_| {
            let v = used[old_idx];

            if v {
                new_indices[old_idx] = StringHandle(new_idx);
                new_idx += 1;
            }

            old_idx += 1;
            v
        });

        for t in &mut self.transitions {
            if let GraphTest::Prefix(h) | GraphTest::PrefixInsensitive(h) = &mut t.test {
                *h = new_indices[h.0];
            }
        }
    }

    fn finalize_charsets(&mut self) {
        let mut used = vec![false; self.charsets.len()];
        let mut new_indices = vec![CharsetHandle::MAX; self.charsets.len()];

        for t in &self.transitions {
            if let GraphTest::Charset(c) = t.test {
                used[c.0] = true;
            }
        }

        let mut old_idx = 0;
        let mut new_idx = 0;
        self.charsets.retain(|_| {
            let v = used[old_idx];

            if v {
                new_indices[old_idx] = CharsetHandle(new_idx);
                new_idx += 1;
            }

            old_idx += 1;
            v
        });

        for t in &mut self.transitions {
            if let GraphTest::Charset(h) = &mut t.test {
                *h = new_indices[h.0];
            }
        }
    }

    pub fn charsets(&self) -> &HandleVec<CharsetHandle, Charset> {
        &self.charsets
    }

    pub fn strings(&self) -> &HandleVec<StringHandle, String> {
        &self.strings
    }
}

#[derive(Debug, Clone)]
pub struct GraphState {
    /// Root states will have a name from the definitions file.
    name: Option<&'static str>,
    action: Option<GraphAction>,
    /// The sum of chars that transition going out of this state cover.
    coverage: Charset,
    has_root_loop: bool,
}

impl Default for GraphState {
    fn default() -> Self {
        Self { name: None, action: None, coverage: Charset::default(), has_root_loop: false }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GraphAction {
    Jump(NodeId), // change to
    Push(NodeId), // push to
    Pop(usize),   // pop this many
    Loop,         // loop to the start of the state & do an exit check
    Fallback,     // replace with a fallback transition (for look-aheads like \b)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphTest {
    Chars(usize),
    Charset(CharsetHandle),
    Prefix(StringHandle),
    PrefixInsensitive(StringHandle),
}

#[derive(Debug, Clone)]
pub struct GraphTransition {
    origin: i32,
    pub test: GraphTest,
    pub kind: HighlightKindOp,
}

impl Default for GraphTransition {
    fn default() -> Self {
        Self { origin: -1, test: GraphTest::Chars(0), kind: HighlightKindOp::None }
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
