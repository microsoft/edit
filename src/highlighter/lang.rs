//! Work in Progress!
//!
//! This file takes a [`LanguageDefinition`] which describes syntax highlighting rules
//! for a language via a list of regular expressions that result in
//! * a highlight kind (comment, string, number, etc.)
//! * a push/pop action of another state (allows for nesting languages, such as in Markdown)
//!
//! It then transforms the definition into a list of [`WipState`], which are directions
//! to our custom DFA engine. The engine is very simple to reduce binary size.
//! Each defined state represents a root. Each additional state represents one step in
//! the regular expression. The difference between the two is that the root states will
//! seek to the next possible occurrence of any of the defined regular expressions,
//! whereas the additional states will try to match the next character without seeking.
//! If it doesn't match, it will fall back to the next possible defined regular expression.

#![allow(dead_code, unused)]

use core::panic;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, LinkedList, VecDeque};
use std::fmt::{Debug, Write};
use std::mem::{ManuallyDrop, forget};
use std::ops::{Index, IndexMut};
use std::{mem, ptr, slice};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Dot, Hir, HirKind, Look};

use super::{Action, Consume, HighlightKind};
use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::cell::SemiRefCell;
use crate::helpers::AsciiStringHelpers;
use crate::highlighter::{CharsetFormatter, State, Transition};

pub struct LanguageDefinition {
    #[allow(dead_code)]
    name: &'static str,
    extensions: &'static [&'static str],
    states: &'static [StateDefinition],
}

pub struct StateDefinition {
    name: &'static str,
    rules: &'static [(&'static str, HighlightKind, ActionDefinition)],
}

#[derive(Debug, Clone, Copy)]
enum ActionDefinition {
    Push(&'static str), // state name to push
    Pop,
}

pub const JSON: LanguageDefinition = {
    use ActionDefinition::*;
    use HighlightKind::*;

    LanguageDefinition {
        name: "JSON",
        extensions: &["json", "jsonc"],
        states: &[
            StateDefinition {
                name: "ground",
                rules: &[
                    (r#"//.*"#, Comment, Pop),
                    (r#"/\*"#, Comment, Push("comment")),
                    (r#"""#, String, Push("string")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop),
                    (r#"(?:true|false|null)"#, Keyword, Pop),
                ],
            },
            StateDefinition { name: "comment", rules: &[(r#"\*/"#, Comment, Pop)] },
            StateDefinition {
                name: "string",
                rules: &[(r#"\\"#, String, Push("string_escape")), (r#"""#, String, Pop)],
            },
            StateDefinition { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
        ],
    }
};

#[derive(Clone, PartialEq, Eq)]
struct Charset([bool; 256]);

impl Charset {
    pub fn fill(&mut self, value: bool) {
        self.0.fill(value);
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.0.iter_mut().zip(other.0.iter()) {
            *a |= *b;
        }
    }

    pub fn merge_str(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.0[*b as usize] = true;
        }
    }

    pub fn merge_str_insensitive(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.0[b.to_ascii_uppercase() as usize] = true;
            self.0[b.to_ascii_lowercase() as usize] = true;
        }
    }

    pub fn is_any(&self) -> bool {
        self.0.iter().all(|&b| b)
    }

    pub fn is_superset(&self, other: &Charset) -> bool {
        for (a, b) in self.0.iter().zip(other.0.iter()) {
            if *b && !*a {
                return false;
            }
        }
        true
    }

    pub fn iter(&self) -> impl Iterator<Item = bool> + '_ {
        self.0.iter().copied()
    }
}

impl Default for Charset {
    fn default() -> Self {
        Charset([false; 256])
    }
}

impl Debug for Charset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", CharsetFormatter(&self.0))
    }
}

impl<I> Index<I> for Charset
where
    [bool]: Index<I>,
{
    type Output = <[bool] as Index<I>>::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.0.index(index)
    }
}

impl<I> IndexMut<I> for Charset
where
    [bool]: IndexMut<I>,
{
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.0.index_mut(index)
    }
}

#[derive(Debug)]
struct WipState<'a> {
    name: Option<&'static str>,

    /// The transitions leading away from this state.
    transitions: LinkedList<WipTransition<'a>, &'a Arena>,

    // All of these members are used to find fallback states.
    // The idea is that we find the shallowest (closest to the root) state
    // that has a superset of the characters that led to this state.
    /// Depth of this node.
    depth: usize,
    shallowest_parent: Option<&'a WipStateCell<'a>>,
    /// A fallback state must be a superset of this charset.
    required_charset: Charset,
    fallback_done: bool,
}

type WipStateCell<'a> = SemiRefCell<WipState<'a>>;

#[derive(Debug, Clone, Copy)]
enum WipAction<'a> {
    // Same as super::Action
    Change(&'a WipStateCell<'a>),
    Push(&'a WipStateCell<'a>),
    Pop,
}

impl PartialEq for WipAction<'_> {
    fn eq(&self, other: &Self) -> bool {
        use WipAction::*;
        match (self, other) {
            (Change(a), Change(b)) => ptr::eq(a.as_ptr(), b.as_ptr()),
            (Push(a), Push(b)) => ptr::eq(a.as_ptr(), b.as_ptr()),
            (Pop, Pop) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum WipConsume<'a> {
    // Same as super::Consume
    Chars(usize),
    Prefix(ArenaString<'a>),
    PrefixInsensitive(ArenaString<'a>),
    Charset(&'a Charset),
}

#[derive(Debug, Clone)]
struct WipTransition<'a> {
    test: WipConsume<'a>,
    kind: HighlightKind,
    action: WipAction<'a>,
}

pub fn parse_language_definition(def: &LanguageDefinition) {
    let scratch = scratch_arena(None);
    let mut ctx = WipContext::new(&scratch);

    for s in def.states {
        ctx.add_root(s.name);
    }

    for (ground_idx, state) in def.states.iter().enumerate() {
        for (pattern, kind, action) in state.rules {
            let src = ctx.get_root(ground_idx);
            let dst = match action {
                ActionDefinition::Push(name) => WipAction::Push(ctx.get_root_by_name(name)),
                ActionDefinition::Pop => WipAction::Pop,
            };
            let hir = regex_syntax::ParserBuilder::new()
                .utf8(false)
                .unicode(false)
                .dot_matches_new_line(true)
                .build()
                .parse(pattern)
                .unwrap();
            ctx.transform(src, dst, &hir);
        }
    }

    ctx.compute_required_charsets();
    ctx.connect_fallbacks();
    print!("{}", ctx.format_as_mermaid(&scratch));
}

struct WipContext<'a> {
    arena: &'a Arena,
    roots: Vec<&'a WipStateCell<'a>, &'a Arena>,
    kind: HighlightKind,
}

impl<'a> WipContext<'a> {
    fn new(arena: &'a Arena) -> Self {
        WipContext { arena, roots: Vec::with_capacity_in(16, arena), kind: HighlightKind::Other }
    }

    fn add_root(&mut self, name: &'static str) {
        let s = self.add_state(0);
        {
            let mut s = s.borrow_mut();
            s.name = Some(name);
            s.fallback_done = true;
        }
        self.roots.push(s);
    }

    fn get_root(&self, idx: usize) -> &'a WipStateCell<'a> {
        self.roots[idx]
    }

    fn get_root_by_name(&self, name: &str) -> &'a WipStateCell<'a> {
        for s in &self.roots {
            if s.borrow().name == Some(name) {
                return s;
            }
        }
        panic!("Unknown state name: {name}")
    }

    fn transform(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        hir: &Hir,
    ) -> WipAction<'a> {
        fn is_any_class(class: &ClassBytes) -> bool {
            class.ranges() == [ClassBytesRange::new(0, 255)]
        }

        match hir.kind() {
            HirKind::Literal(lit) => self.transform_literal(src, dst, &lit.0),
            HirKind::Class(Class::Bytes(class)) if is_any_class(class) => {
                self.transform_any(src, dst)
            }
            HirKind::Class(Class::Bytes(class)) => self.transform_class(src, dst, class),
            HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
                (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                    self.transform_any_star(src, dst)
                }
                (0, None, HirKind::Class(Class::Bytes(class))) => {
                    let dst = self.transform_class_plus(src, dst, class);
                    self.transform_option(src, dst);
                    dst
                }
                (0, Some(1), _) => {
                    let dst = self.transform(src, dst, &rep.sub);
                    self.transform_option(src, dst);
                    dst
                }
                (1, None, HirKind::Class(Class::Bytes(class))) => {
                    self.transform_class_plus(src, dst, class)
                }
                _ => panic!("Unsupported HIR: {hir:?}"),
            },
            HirKind::Concat(hirs) if hirs.len() >= 2 => self.transform_concat(src, dst, hirs),
            HirKind::Alternation(hirs) if hirs.len() >= 2 => self.transform_alt(src, dst, hirs),
            _ => panic!("Unsupported HIR: {hir:?}"),
        }
    }

    // string
    fn transform_literal(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        lit: &[u8],
    ) -> WipAction<'a> {
        self.add_transition(
            src,
            dst,
            WipConsume::Prefix(ArenaString::from_utf8_lossy_owned({
                let mut v = Vec::new_in(self.arena);
                v.extend_from_slice(lit);
                v
            })),
        )
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        class: &ClassBytes,
    ) -> WipAction<'a> {
        let c = self.arena.alloc_uninit();
        let c = c.write(self.class_to_charset(class));
        self.add_transition(src, dst, WipConsume::Charset(c))
    }

    // [eE]
    fn transform_class(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        class: &ClassBytes,
    ) -> WipAction<'a> {
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
                WipConsume::PrefixInsensitive(str)
            } else {
                WipConsume::Prefix(str)
            };

            let d = self.add_transition(src, dst, test);
            if d != *actual_dst.get_or_insert(d) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(&mut self, src: &'a WipStateCell<'a>, dst: WipAction<'a>) -> WipAction<'a> {
        self.add_transition(src, dst, WipConsume::Chars(0))
    }

    // .*
    fn transform_any_star(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
    ) -> WipAction<'a> {
        self.add_transition(src, dst, WipConsume::Chars(usize::MAX))
    }

    // .
    fn transform_any(&mut self, src: &'a WipStateCell<'a>, dst: WipAction<'a>) -> WipAction<'a> {
        self.add_transition(src, dst, WipConsume::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        hirs: &[Hir],
    ) -> WipAction<'a> {
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

        let depth = src.borrow().depth + 1;
        let mut it = hirs.iter().peekable();
        let mut src = WipAction::Change(src);

        while let Some(mut hir) = it.next() {
            let src_idx = match src {
                WipAction::Change(idx) => idx,
                _ => panic!("Unexpected action in transform_concat"),
            };

            if let Some(ch) = check_lowercase_literal(hir) {
                // Transform [aA][bB][cC] into PrefixInsensitive("abc").
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

                let next = if it.peek().is_some() {
                    WipAction::Change(self.add_state(depth))
                } else {
                    dst
                };
                src = self.add_transition(src_idx, next, WipConsume::PrefixInsensitive(str));
            } else {
                // Any other sequence is simply concatenated.
                let next = if it.peek().is_some() {
                    WipAction::Change(self.add_state(depth))
                } else {
                    dst
                };
                src = self.transform(src_idx, next, hir);
            }
        }

        src
    }

    // (a|b)
    fn transform_alt(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        hirs: &[Hir],
    ) -> WipAction<'a> {
        let mut actual_dst = None;

        for hir in hirs {
            let d = self.transform(src, dst, hir);
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

        // If the class includes \w, we also set any non-ASCII characters.
        // That's not how Unicode works, but it simplifies the implementation.
        if [(b'0', b'9'), (b'A', b'Z'), (b'_', b'_'), (b'a', b'z')]
            .iter()
            .all(|&(beg, end)| charset[beg as usize..=end as usize].iter().all(|&b| b))
        {
            charset[0x80..=0xFF].fill(true);
        }

        charset
    }

    fn add_state(&mut self, depth: usize) -> &'a WipStateCell<'a> {
        self.arena.alloc_uninit().write(WipStateCell::new(WipState {
            name: None,

            transitions: LinkedList::new_in(self.arena),

            depth,
            shallowest_parent: None,
            required_charset: Charset::default(),
            fallback_done: false,
        }))
    }

    fn add_transition(
        &mut self,
        src: &'a WipStateCell<'a>,
        dst: WipAction<'a>,
        test: WipConsume<'a>,
    ) -> WipAction<'a> {
        let mut s = src.borrow_mut();

        // Check if the edge already exists.
        for t in s.transitions.iter() {
            if t.test == test {
                if mem::discriminant(&t.action) != mem::discriminant(&dst) {
                    panic!(
                        "Diverging actions for the same test: {:?} -> {:?} vs {:?}",
                        test, t.action, dst
                    );
                }
                return t.action;
            }
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in s.transitions.iter() {
            use WipConsume::*;

            if match (&t.test, &test) {
                (Chars(_), _) => true,
                (Prefix(p), Prefix(s)) => s.starts_with(p.as_str()),
                (PrefixInsensitive(p), Prefix(s) | PrefixInsensitive(s)) => {
                    s.starts_with_ignore_ascii_case(p)
                }
                (Charset(p), Charset(n)) => {
                    // If all the bits in `n` are also true in `p`
                    n.iter().zip(p.iter()).all(|(n, p)| !n || p)
                }
                _ => false,
            } {
                panic!(
                    "Attempted to add unreachable test: {:?} is encompassed by {:?}",
                    test, t.test
                );
            }
        }

        // Remember the shallowest parent for the state.
        if let WipAction::Change(dst) = dst {
            let mut dst = dst.borrow_mut();
            if dst
                .shallowest_parent
                .is_none_or(|p| !ptr::eq(p.as_ptr(), &*s) && p.borrow().depth > s.depth)
            {
                dst.shallowest_parent = Some(src);
            }
        }

        s.transitions.push_back(WipTransition { test, kind: self.kind, action: dst });
        dst
    }

    fn compute_required_charsets(&self) {
        fn run(state: &WipStateCell) {
            let src = state.borrow();

            let scratch = scratch_arena(None);
            let mut dsts = Vec::with_capacity_in(src.transitions.len(), &*scratch);
            dsts.extend(src.transitions.iter().filter_map(|t| match t.action {
                WipAction::Change(s) => Some((&t.test, s)),
                _ => None,
            }));

            for &(test, dst) in &dsts {
                let mut cs = dst.borrow_mut();
                let cs = &mut cs.required_charset;

                match test {
                    WipConsume::Chars(_) => cs.fill(true),
                    WipConsume::Prefix(s) => cs.merge_str(s),
                    WipConsume::PrefixInsensitive(s) => cs.merge_str_insensitive(s),
                    WipConsume::Charset(c) => cs.merge(c),
                }

                cs.merge(&src.required_charset);
            }

            for (test, dst) in &dsts {
                run(dst);
            }
        }

        for root in &self.roots {
            run(root);
        }
    }

    fn connect_fallbacks(&self) {
        fn run(state: &WipStateCell) {
            {
                let src = state.borrow();

                for t in &src.transitions {
                    if let WipAction::Change(dst) = t.action
                        && !dst.borrow().fallback_done
                    {
                        run(dst);
                    }
                }

                if src.fallback_done {
                    return;
                }
            }

            let mut src = state.borrow_mut();
            let required_charset = &src.required_charset;
            if required_charset.is_any() {
                return;
            }

            let mut parent = src.shallowest_parent;
            let mut found = None;
            while let Some(p) = parent {
                let p_borrow = p.borrow();
                if p_borrow.required_charset.is_superset(required_charset) {
                    found = Some(p);
                    break;
                }
                parent = p_borrow.shallowest_parent;
            }

            src.transitions.push_back(WipTransition {
                test: WipConsume::Chars(0),
                kind: HighlightKind::Other,
                action: if let Some(parent) = found {
                    WipAction::Change(parent)
                } else {
                    WipAction::Pop
                },
            });
            src.fallback_done = true;
        }

        for root in &self.roots {
            run(root);
        }
    }

    fn format_as_mermaid<'o>(&self, arena: &'o Arena) -> ArenaString<'o> {
        struct Visitor<'a, 'o> {
            node_ids: HashMap<*const WipState<'a>, (usize, bool)>,
            output: ArenaString<'o>,
        }

        fn create_node_id<'v>(
            visitor: &'v mut Visitor,
            node: &WipStateCell,
        ) -> &'v mut (usize, bool) {
            let ptr = node.as_ptr() as *const _;
            let id = visitor.node_ids.len();

            match visitor.node_ids.entry(ptr) {
                Entry::Vacant(e) => e.insert((id, false)),
                Entry::Occupied(mut e) => e.into_mut(),
            }
        }

        fn visit(visitor: &mut Visitor, src: &WipStateCell) -> Option<usize> {
            let (src_id, visited) = create_node_id(visitor, src);
            if *visited {
                None
            } else {
                *visited = true;
                Some(*src_id)
            }
        }

        fn print_transition(
            visitor: &mut Visitor,
            src_id: usize,
            src: &WipStateCell,
            t: &WipTransition,
        ) {
            let dst = match t.action {
                WipAction::Change(dst) => {
                    format!("{}", create_node_id(visitor, dst).0)
                }
                WipAction::Push(dst) => {
                    let s = dst.borrow();
                    let dst_id = create_node_id(visitor, dst).0;
                    format!("push{}[/\"Push({})\"/]", src_id << 16 | dst_id, s.name.unwrap())
                }
                WipAction::Pop => {
                    format!("pop{}[/\"Pop\"/]", src_id << 16)
                }
            };
            let label = match &t.test {
                WipConsume::Chars(usize::MAX) => "Chars(Line)".to_string(),
                WipConsume::Chars(n) => format!("Chars({n})"),
                WipConsume::Prefix(s) => format!("Prefix({s})"),
                WipConsume::PrefixInsensitive(s) => format!("PrefixInsensitive({s})"),
                WipConsume::Charset(c) => format!("Charset({c:?})"),
            };
            let label = label.replace('"', "&quot;");
            let label = label.replace('\\', r#"\\"#);
            _ = writeln!(&mut visitor.output, "    {src_id} -->|\"{label}\"| {dst}");
        }

        fn print_state_bfs(visitor: &mut Visitor, src: &WipStateCell) {
            let Some(src_id) = visit(visitor, src) else {
                return;
            };
            for t in &src.borrow().transitions {
                print_transition(visitor, src_id, src, t);
            }
            for t in &src.borrow().transitions {
                if let WipAction::Change(idx) = t.action {
                    print_state_bfs(visitor, idx);
                }
            }
        }

        fn print_state_dfs(visitor: &mut Visitor, src: &WipStateCell) {
            let Some(src_id) = visit(visitor, src) else {
                return;
            };
            for t in &src.borrow().transitions {
                print_transition(visitor, src_id, src, t);
                if let WipAction::Change(idx) = t.action {
                    print_state_bfs(visitor, idx);
                }
            }
        }

        let mut visitor = Visitor { node_ids: HashMap::new(), output: ArenaString::new_in(arena) };

        _ = write!(&mut visitor.output, "---\nconfig:\n  layout: elk\n---\nflowchart TD\n");
        for s in &self.roots {
            _ = writeln!(
                &mut visitor.output,
                "    {}[\"{}\"]",
                visitor.node_ids.len(),
                s.borrow().name.unwrap()
            );
            create_node_id(&mut visitor, s);
        }
        for s in &self.roots {
            print_state_dfs(&mut visitor, s);
        }

        visitor.output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_language_definition() {
        parse_language_definition(&JSON);
    }
}
