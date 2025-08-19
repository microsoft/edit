use std::fmt::{self, Write as _};
use std::mem;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};

use crate::definitions::*;
use crate::handles::{HandleVec, declare_handle};
use crate::interner::Interner;

declare_handle!(pub StateHandle(usize));
declare_handle!(pub TransitionHandle(usize));

pub struct GraphBuilder {
    roots: RootList,
    states: HandleVec<StateHandle, GraphState>,
    transitions: HandleVec<TransitionHandle, GraphTransition>,
    charsets: Interner<Charset>,
    strings: Interner<String>,
    origin: i32,
}

impl GraphBuilder {
    pub fn new() -> Self {
        GraphBuilder {
            roots: Default::default(),
            states: Default::default(),
            transitions: Default::default(),
            charsets: Default::default(),
            strings: Default::default(),
            origin: -1,
        }
    }

    pub fn declare_root(&mut self, name: &'static str) {
        self.roots.declare(name)
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

        let src = self.roots.try_find_actual_by_name(root).unwrap_or_else(|| {
            let src = self.add_state();
            self.states[src].name = Some(root);
            self.roots.define(root, src);
            src
        });

        // By creating a deferred target node like this:
        //   [node] --- Chars(0) --> Pop
        // we allow later regex to add more transitions to [node].
        //
        // During `simplify_indirections()` we'll then clean those up again, by connection
        // nodes together directly if they're joined by just a `Chars(0)` transition.
        let dst = self.add_state();
        let dst = self.transform(None, src, GraphAction::Change(dst), &hir);
        let dst = match dst {
            GraphAction::Change(dst) => dst,
            _ => panic!("Unexpected transform result: {dst:?}"),
        };
        let actual_dst = match rule.action {
            ActionDefinition::Done => GraphAction::Pop(0),
            ActionDefinition::Change(name) => {
                GraphAction::Change(self.roots.find_alias_by_name(name))
            }
            ActionDefinition::Push(name) => GraphAction::Push(self.roots.find_alias_by_name(name)),
            ActionDefinition::Pop => GraphAction::Pop(1),
        };
        self.add_transition(rule.kind, dst, actual_dst, GraphTest::Chars(0));
    }

    fn transform(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        hir: &Hir,
    ) -> GraphAction {
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
                    GraphAction::Fallback,
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
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        lit: &[u8],
    ) -> GraphAction {
        let prefix = String::from_utf8(lit.to_vec()).unwrap();
        let prefix = self.strings.intern(prefix);
        self.add_transition(kind, src, dst, GraphTest::Prefix(prefix))
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        class: &ClassBytes,
    ) -> GraphAction {
        let c = self.class_to_charset(class);
        let c = self.charsets.intern(c);
        self.add_transition(kind, src, dst, GraphTest::Charset(c))
    }

    // [eE]
    fn transform_class(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        class: &ClassBytes,
    ) -> GraphAction {
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
                GraphTest::PrefixInsensitive(self.strings.intern(str))
            } else {
                GraphTest::Prefix(self.strings.intern(str))
            };

            let d = self.add_transition(kind, src, dst, test);
            if d != *actual_dst.get_or_insert(d) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(0))
    }

    // .*
    fn transform_any_star(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(usize::MAX))
    }

    // .
    fn transform_any(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        hirs: &[Hir],
    ) -> GraphAction {
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
        let mut src = GraphAction::Change(src);

        while let Some(hir) = it.next() {
            let src_idx = match src {
                GraphAction::Change(idx) => idx,
                _ => panic!("Unexpected action in transform_concat"),
            };

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
            let kind = if more { None } else { kind };
            let dst = if more {
                GraphAction::Change(self.add_state())
            } else {
                dst
            };

            if let Some(str) = prefix_insensitive {
                let str = self.strings.intern(str);
                src = self.add_transition(kind, src_idx, dst, GraphTest::PrefixInsensitive(str));
            } else {
                src = self.transform(kind, src_idx, dst, hir);
            }
        }

        src
    }

    // (a|b)
    fn transform_alt(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        hirs: &[Hir],
    ) -> GraphAction {
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

    fn add_state(&mut self) -> StateHandle {
        self.states.push(GraphState {
            name: None,
            coverage: Charset::default(),
        })
    }

    fn add_transition(
        &mut self,
        kind: Option<HighlightKind>,
        src: StateHandle,
        dst: GraphAction,
        test: GraphTest,
    ) -> GraphAction {
        // Check if the edge already exists.
        for t in &self.transitions {
            if t.src != src || t.test != test {
                continue;
            }

            if mem::discriminant(&t.dst) == mem::discriminant(&dst) {
                return t.dst;
            }

            panic!(
                "Diverging actions for the same test: {:?} -> {:?} vs {:?}",
                t.test, t.dst, dst
            );
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in self.transitions_from_state(src) {
            use GraphTest::*;

            if match (&t.test, &test) {
                (Chars(_), _) => true,
                (Charset(p), Charset(n)) => n.is_superset(p),
                (Charset(p), Prefix(n)) => p.covers_char(n.as_bytes()[0]),
                (Charset(p), PrefixInsensitive(n)) => p.covers_char_insensitive(n.as_bytes()[0]),
                (Prefix(p), Prefix(s)) => s.starts_with(p.as_str()),
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

        self.transitions.push(GraphTransition {
            origin: self.origin,
            src,
            test,
            kind,
            dst,
        });
        dst
    }

    pub fn finalize(&mut self) {
        self.finalize_resolve_root_aliases();
        self.finalize_compute_charset_coverage();
        self.finalize_add_root_loops();
        self.finalize_add_fallbacks();
        self.finalize_add_rescue_fallbacks();
        self.finalize_simplify_indirections();
        self.finalize_remove_unreachable();
        self.finalize_sort();
    }

    fn finalize_resolve_root_aliases(&mut self) {
        for t in &mut self.transitions {
            match &mut t.dst {
                GraphAction::Change(dst) | GraphAction::Push(dst) => {
                    if let Some(actual) = self.roots.try_find_actual_by_alias(*dst) {
                        *dst = actual;
                    }
                }
                GraphAction::Pop(_) | GraphAction::Fallback => {}
            }
        }
    }

    /// Compute existing charset coverage.
    /// Technically we don't need to do that for the root states.
    fn finalize_compute_charset_coverage(&mut self) {
        for t in &self.transitions {
            match &t.test {
                GraphTest::Chars(_) => self.states[t.src].coverage.fill(true),
                GraphTest::Charset(c) => self.states[t.src].coverage.merge(c),
                _ => {}
            }
        }
    }

    // Compute fast skips for root states & fallback to 1 byte skip.
    // This is different from coverage computation above, because here
    // we want to "skip" any character that can never be matched.
    fn finalize_add_root_loops(&mut self) {
        for src in self.states.indices() {
            {
                let s = &mut self.states[src];

                if s.name.is_none() {
                    continue;
                }

                self.states[src].coverage.fill(true);
            }

            let mut cs = Charset::no();

            for t in self.transitions_from_state(src) {
                match &t.test {
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

                self.transitions.push(GraphTransition {
                    origin: -1,
                    src,
                    test: GraphTest::Charset(self.charsets.intern(cs)),
                    kind: None,
                    dst: GraphAction::Pop(0),
                });
                self.transitions.push(GraphTransition {
                    origin: -1,
                    src,
                    test: GraphTest::Chars(1),
                    kind: None,
                    dst: GraphAction::Pop(0),
                });
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

            let fallback_cs = match &fallback.test {
                GraphTest::Chars(_) => &CS_YES,
                GraphTest::Charset(c) => &**c,
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
                            t.test = fallback.test.clone();
                            t.dst = fallback.dst;
                        }
                        GraphAction::Change(dst) if !visited[dst.0] => {
                            visited[dst.0] = true;

                            // Check if the fallback is a superset of this transition.
                            // This applies recursively, which means we assert that the fallback covers
                            // the entire "path" from the original `fallback.src` down to this state.
                            if match &t.test {
                                GraphTest::Chars(0) => true,
                                GraphTest::Chars(_) => fallback_cs.covers_all(),
                                GraphTest::Charset(c) => fallback_cs.is_superset(c),
                                GraphTest::Prefix(s) => fallback_cs.covers_str(s),
                                GraphTest::PrefixInsensitive(s) => {
                                    fallback_cs.covers_str_insensitive(s)
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
                self.transitions.push(GraphTransition {
                    origin: -1,
                    src,
                    test: GraphTest::Chars(0),
                    kind: None,
                    dst: GraphAction::Pop(0),
                });
            }
        }

        for t in &mut self.transitions {
            if t.dst == GraphAction::Fallback {
                t.dst = GraphAction::Pop(0);
                t.kind = None;
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
                    let t = &self.transitions[TransitionHandle(i)];
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
                    if t.dst == GraphAction::Change(src) {
                        if kind.is_some() {
                            if t.kind.is_some() && t.kind != kind {
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

    fn finalize_remove_unreachable(&mut self) {
        let mut visited = vec![false; self.states.len()];
        let mut stack = Vec::new();

        stack.push(StateHandle(0));
        visited[0] = true;

        while let Some(src) = stack.pop() {
            for t in self.transitions_from_state(src) {
                let dst = match t.dst {
                    GraphAction::Change(d) | GraphAction::Push(d) => d,
                    GraphAction::Pop(_) | GraphAction::Fallback => continue,
                };
                if !visited[dst.0] {
                    visited[dst.0] = true;
                    stack.push(dst);
                }
            }
        }

        let mut new_indices = vec![StateHandle::MAX; self.states.len()];

        // Remove unreachable states and transitions
        let mut old_idx = 0;
        let mut new_idx = 0;
        self.states.retain_mut(|_| {
            let v = visited[old_idx];

            if v {
                new_indices[old_idx] = StateHandle(new_idx);
                new_idx += 1;
            }

            old_idx += 1;
            v
        });

        // Remove unreachable transitions, Remap reachable ones
        self.transitions.retain_mut(|t| {
            t.src = match new_indices[t.src.0] {
                StateHandle::MAX => return false,
                new => new,
            };

            match &mut t.dst {
                GraphAction::Change(d) | GraphAction::Push(d) => {
                    *d = match new_indices[d.0] {
                        StateHandle::MAX => return false,
                        new => new,
                    }
                }
                GraphAction::Pop(_) | GraphAction::Fallback => {}
            }

            true
        });
    }

    fn finalize_sort(&mut self) {
        self.transitions.sort_by_key(|t| t.src);
    }

    pub fn format_as_mermaid(&self) -> String {
        let mut output = String::new();
        let mut visited = vec![false; self.states.len()];

        _ = writeln!(&mut output, "flowchart TD");

        let mut iter = self.transitions.iter().peekable();
        while let Some(t) = iter.next() {
            if !visited[t.src.0] {
                visited[t.src.0] = true;
                let s = &self.states[t.src];
                if let Some(name) = s.name {
                    _ = writeln!(&mut output, "    {0}[\"{0} ({1})\"]", t.src.0, name);
                }
            }

            let label = match &t.test {
                GraphTest::Chars(usize::MAX) => "Chars(Line)".to_string(),
                GraphTest::Chars(n) => format!("Chars({n})"),
                GraphTest::Charset(c) => format!("Charset({c:?})"),
                GraphTest::Prefix(s) => {
                    let mut label = String::new();
                    _ = write!(label, "Prefix({s}");

                    loop {
                        let Some(next) = iter.peek() else {
                            break;
                        };
                        let GraphTest::Prefix(next_s) = &next.test else {
                            break;
                        };
                        if next.dst != t.dst {
                            break;
                        }

                        _ = write!(label, ", {}", next_s);
                        iter.next();
                    }

                    label.push(')');
                    label
                }
                GraphTest::PrefixInsensitive(s) => {
                    let mut label = String::new();
                    _ = write!(label, "PrefixInsensitive({s}");

                    loop {
                        let Some(next) = iter.peek() else {
                            break;
                        };
                        let GraphTest::PrefixInsensitive(next_s) = &next.test else {
                            break;
                        };
                        if next.dst != t.dst {
                            break;
                        }

                        _ = write!(label, ", {next_s}");
                        iter.next();
                    }

                    label.push(')');
                    label
                }
            };

            let dst = match &t.dst {
                GraphAction::Change(dst) => {
                    format!("{}", dst.0)
                }
                GraphAction::Push(dst) => {
                    format!(
                        "push{}[/\"{}\"/]",
                        t.src.0 << 16 | dst.0,
                        self.states[*dst].name.unwrap()
                    )
                }
                GraphAction::Pop(_) => {
                    format!("pop{}@{{ shape: stop }}", t.src.0 << 16)
                }
                GraphAction::Fallback => unreachable!(),
            };

            let label = {
                let mut res = String::with_capacity(label.len());
                for c in label.chars() {
                    match c {
                        '\t' => res.push_str(r#"\\t"#),
                        '"' => res.push_str("&quot;"),
                        '\\' => res.push_str(r#"\\"#),
                        _ => res.push(c),
                    }
                }
                res
            };
            _ = writeln!(
                &mut output,
                "    {src} -->|\"{label}<br/>{kind:?}\"| {dst}",
                src = t.src.0,
                kind = t.kind,
            );
        }

        output.pop(); // Remove the last newline character.
        output
    }

    pub fn charsets(&self) -> Vec<Rc<Charset>> {
        self.charsets.extract()
    }

    pub fn strings(&self) -> Vec<Rc<String>> {
        self.strings.extract()
    }

    pub fn states(&self) -> impl Iterator<Item = StateHandle> {
        (0..self.states.len()).map(StateHandle)
    }

    pub fn transitions_from_state(
        &self,
        src: StateHandle,
    ) -> impl Iterator<Item = &GraphTransition> {
        self.transitions.iter().filter(move |&t| t.src == src)
    }
}

#[derive(Default)]
struct RootList {
    list: Vec<(&'static str, StateHandle, StateHandle)>,
}

impl RootList {
    const ROOT_ALIAS_OFFSET: usize = StateHandle::MAX.0 / 2;

    pub fn declare(&mut self, name: &'static str) {
        let alias = StateHandle(Self::ROOT_ALIAS_OFFSET + self.list.len());
        self.list.push((name, alias, StateHandle::MAX));
    }

    fn define(&mut self, name: &str, actual: StateHandle) {
        for (n, _, a) in &mut self.list {
            if *n == name {
                *a = actual;
                return;
            }
        }

        panic!("No root state found with name: {name}");
    }

    fn find_alias_by_name(&self, name: &str) -> StateHandle {
        for (n, alias, _) in &self.list {
            if *n == name {
                return *alias;
            }
        }

        panic!("No root state found with name: {name}");
    }

    fn try_find_actual_by_name(&self, name: &str) -> Option<StateHandle> {
        self.list
            .iter()
            .find(|(n, _, actual)| *n == name && *actual != StateHandle::MAX)
            .map(|(_, _, actual)| *actual)
    }

    fn try_find_actual_by_alias(&self, alias: StateHandle) -> Option<StateHandle> {
        alias
            .0
            .checked_sub(Self::ROOT_ALIAS_OFFSET)
            .and_then(|idx| self.list.get(idx))
            .map(|(_, _, actual)| *actual)
    }
}

#[derive(Debug, Default, Clone)]
pub struct GraphState {
    /// Root states will have a name from the definitions file.
    name: Option<&'static str>,
    /// The sum of chars that transition going out of this state cover.
    coverage: Charset,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GraphAction {
    Change(StateHandle),
    Push(StateHandle),
    Pop(usize),
    Fallback,
}

#[derive(Debug, Clone)]
pub enum GraphTest {
    Chars(usize),
    Charset(Rc<Charset>),
    Prefix(Rc<String>),
    PrefixInsensitive(Rc<String>),
}

impl PartialEq for GraphTest {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GraphTest::Chars(a), GraphTest::Chars(b)) => a == b,
            (GraphTest::Charset(a), GraphTest::Charset(b)) => Rc::ptr_eq(a, b),
            (GraphTest::Prefix(a), GraphTest::Prefix(b)) => Rc::ptr_eq(a, b),
            (GraphTest::PrefixInsensitive(a), GraphTest::PrefixInsensitive(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for GraphTest {}

#[derive(Debug, Clone)]
pub struct GraphTransition {
    origin: i32,
    pub src: StateHandle,
    pub test: GraphTest,
    pub kind: Option<HighlightKind>,
    pub dst: GraphAction,
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
            if b.is_ascii_graphic() || b == b' ' {
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
