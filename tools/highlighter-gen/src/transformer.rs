use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Write as _};
use std::mem::{self, MaybeUninit};
use std::ops::{Index, IndexMut};
use std::rc::{Rc, Weak};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind};

use crate::definitions::*;

pub struct GraphBuilder {
    roots: Vec<Rc<WipStateCell>>,
    states: Vec<Weak<WipStateCell>>,
    charsets: CharsetInterner,

    origin: i32,
}

impl GraphBuilder {
    pub fn new() -> Self {
        GraphBuilder {
            roots: Vec::with_capacity(16),
            states: Vec::with_capacity(16),
            charsets: CharsetInterner { charsets: Vec::with_capacity(16) },

            origin: -1,
        }
    }

    pub fn add_root(&mut self, name: &'static str) {
        let s = self.add_state();
        {
            let mut s = s.borrow_mut();
            s.name = Some(name);
        }
        self.roots.push(s);
    }

    fn get_root_by_name(&self, name: &str) -> Rc<WipStateCell> {
        self.roots
            .iter()
            .find(|s| s.borrow().name == Some(name))
            .cloned()
            .expect("Unknown state name")
    }

    pub fn parse(&mut self, root: usize, rule: &Rule) {
        self.origin += 1;
        let dst = self.add_indirect_target(rule.2);
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .dot_matches_new_line(true)
            .build()
            .parse(rule.0)
            .unwrap();
        self.transform(rule.1, self.roots[root].clone(), dst, &hir);
    }

    fn transform(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
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
            HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
                (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                    self.transform_any_star(kind, src, dst)
                }
                (0, None, HirKind::Class(Class::Bytes(class))) => {
                    let dst = self.transform_class_plus(kind, src.clone(), dst.clone(), class);
                    self.transform_option(kind, src, dst.clone());
                    dst
                }
                (0, Some(1), _) => {
                    let dst = self.transform(kind, src.clone(), dst.clone(), &rep.sub);
                    self.transform_option(kind, src, dst.clone());
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
        src: Rc<WipStateCell>,
        dst: GraphAction,
        lit: &[u8],
    ) -> GraphAction {
        self.add_transition(
            kind,
            src,
            dst,
            GraphTest::Prefix(String::from_utf8(lit.to_vec()).unwrap()),
        )
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
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
        src: Rc<WipStateCell>,
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
                GraphTest::PrefixInsensitive(str)
            } else {
                GraphTest::Prefix(str)
            };

            let d = self.add_transition(kind, src.clone(), dst.clone(), test);
            if d != *actual_dst.get_or_insert(d.clone()) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(0))
    }

    // .*
    fn transform_any_star(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(usize::MAX))
    }

    // .
    fn transform_any(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
        dst: GraphAction,
    ) -> GraphAction {
        self.add_transition(kind, src, dst, GraphTest::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
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

                str
            });

            let more = it.peek().is_some();
            let kind = if more { None } else { kind };
            let dst = if more { GraphAction::Change(self.add_state()) } else { dst.clone() };

            if let Some(str) = prefix_insensitive {
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
        src: Rc<WipStateCell>,
        dst: GraphAction,
        hirs: &[Hir],
    ) -> GraphAction {
        let mut actual_dst = None;

        for hir in hirs {
            let d = self.transform(kind, src.clone(), dst.clone(), hir);
            if d != *actual_dst.get_or_insert(d.clone()) {
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

    fn add_state(&mut self) -> Rc<WipStateCell> {
        let s = Rc::new(WipStateCell::new(GraphState {
            id: 0,
            transitions: Vec::new(),

            name: None,
            coverage: Charset::default(),
        }));
        self.states.push(Rc::downgrade(&s));
        s
    }

    fn add_transition(
        &mut self,
        kind: Option<HighlightKind>,
        src: Rc<WipStateCell>,
        dst: GraphAction,
        test: GraphTest,
    ) -> GraphAction {
        let mut s = src.borrow_mut();

        // Check if the edge already exists.
        for t in &s.transitions {
            if t.test != test {
                continue;
            }

            if mem::discriminant(&t.action) == mem::discriminant(&dst) {
                return t.action.clone();
            }

            panic!(
                "Diverging actions for the same test: {:?} -> {:?} vs {:?}",
                t.test, t.action, dst
            );
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in &s.transitions {
            use GraphTest::*;

            if match (&t.test, &test) {
                (Chars(_), _) => true,
                (Charset(p), Charset(n)) => n.is_superset(p),
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

        s.transitions.push(GraphTransition {
            origin: self.origin,
            test,
            kind,
            action: dst.clone(),
        });
        dst
    }

    // By creating a deferred target node like this:
    //   [node] --- Chars(0) --> Pop
    // we allow later regex to add more transitions to [node].
    //
    // During `simplify_indirections()` we'll then clean those up again, by connection
    // nodes together directly if they're joined by just a `Chars(0)` transition.
    fn add_indirect_target(&mut self, action: ActionDefinition) -> GraphAction {
        let dst = match action {
            ActionDefinition::Change(name) => GraphAction::Change(self.get_root_by_name(name)),
            ActionDefinition::Push(name) => GraphAction::Push(self.get_root_by_name(name)),
            ActionDefinition::Pop => GraphAction::Pop,
        };
        let d = self.add_state();
        self.add_transition(None, d.clone(), dst, GraphTest::Chars(0));
        GraphAction::Change(d)
    }

    pub fn finalize(&mut self) {
        self.finalize_simplify_indirections();
        self.finalize_compute_charset_coverage();
        self.finalize_add_fallbacks();
        self.finalize_add_always_matching_fallbacks();
        self.finalize_compute_fast_skips();
        self.finalize_assign_state_ids();
    }

    // Remove pointless indirections from `add_indirect_target()` like this:
    //   [node] --- Chars(0) --> Pop
    fn finalize_simplify_indirections(&mut self) {
        // Returns a transition which consists of just a single Chars(0) test.
        let find = || {
            for state in &self.states {
                if let Some(state) = state.upgrade()
                    && let mut s = state.borrow_mut()
                    && s.transitions.len() == 1
                    && let t = &s.transitions[0]
                    && t.test == GraphTest::Chars(0)
                {
                    let action = t.action.clone();
                    s.transitions.clear();
                    drop(s);
                    return Some((state, action));
                }
            }
            None
        };

        // Progressively:
        // * Find a pointless indirection to remove
        // * Find all states that point to it
        // * Connect their transitions directly to the indirect target
        while let Some((dst, action)) = find() {
            for state in &self.states {
                let Some(state) = state.upgrade() else {
                    continue;
                };

                let mut s = state.borrow_mut();
                for t in &mut s.transitions {
                    if let GraphAction::Change(d) = &t.action
                        && Rc::ptr_eq(d, &dst)
                    {
                        t.action = action.clone();
                    }
                }
            }
        }

        // We now got a bunch of indirections with no transitions. Remove them.
        self.states.retain(|s| s.upgrade().is_some_and(|s| !s.borrow().transitions.is_empty()));
    }

    /// Compute existing charset coverage.
    /// Technically we don't need to do that for the root states.
    fn finalize_compute_charset_coverage(&mut self) {
        for s in &self.states {
            let Some(s) = s.upgrade() else {
                continue;
            };

            let mut s = s.borrow_mut();
            let s = &mut *s;

            for t in &s.transitions {
                match &t.test {
                    GraphTest::Chars(_) => {
                        s.coverage.fill(true);
                        break;
                    }
                    GraphTest::Charset(c) => {
                        s.coverage.merge(c);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Add fallbacks from earlier regexes to later regexes that cover them.
    fn finalize_add_fallbacks(&mut self) {
        fn fallback_find(src: Rc<WipStateCell>) {
            let s = src.borrow();
            let mut has_children = false;

            for t in &s.transitions {
                if let GraphAction::Change(dst) = &t.action {
                    fallback_find(dst.clone());
                    has_children = true;
                }
            }

            if !has_children {
                return;
            }

            for t in &s.transitions {
                let mut cs_buf = MaybeUninit::uninit();
                let cs = match &t.test {
                    GraphTest::Chars(_) => cs_buf.write(Charset::yes()),
                    GraphTest::Charset(c) => &**c,
                    _ => continue,
                };

                for origin in 0..t.origin {
                    fallback_add(origin, t, t, cs);
                }
            }
        }

        fn fallback_add(
            filter_origin: i32,
            t: &GraphTransition,
            fallback: &GraphTransition,
            fallback_cs: &Charset,
        ) {
            if t.origin != filter_origin {
                return;
            }

            if !match &t.test {
                GraphTest::StopIfDone => false,
                GraphTest::Chars(_) => true,
                GraphTest::Charset(c) => fallback_cs.is_superset(c),
                GraphTest::Prefix(s) => fallback_cs.covers_str(s),
                GraphTest::PrefixInsensitive(s) => fallback_cs.covers_str_insensitive(s),
            } {
                return;
            }

            let GraphAction::Change(dst) = &t.action else {
                return;
            };

            let mut d = dst.borrow_mut();

            for t in &d.transitions {
                fallback_add(filter_origin, t, fallback, fallback_cs);
            }

            if fallback_cs.is_strict_superset(&d.coverage) {
                let mut f = fallback.clone();
                f.origin = -1;
                d.transitions.push(f);
                d.coverage.merge(fallback_cs);
            }
        }

        for root in &self.roots {
            fallback_find(root.clone());
        }
    }

    /// Add always-matching fallbacks to all remaining ones.
    fn finalize_add_always_matching_fallbacks(&mut self) {
        for s in &self.states[self.roots.len()..] {
            let Some(s) = s.upgrade() else {
                continue;
            };

            let mut s = s.borrow_mut();
            if !s.coverage.covers_all() {
                s.transitions.push(GraphTransition {
                    origin: -1,
                    test: GraphTest::Chars(0),
                    kind: None,
                    action: GraphAction::Pop,
                });
            }
        }
    }

    // Compute fast skips for root states & fallback to 1 byte skip.
    // This is different from coverage computation above, because here
    // we want to "skip" any character that can never be matched.
    fn finalize_compute_fast_skips(&mut self) {
        for (idx, root) in self.roots.iter().enumerate() {
            let mut s = root.borrow_mut();
            let mut cs = Charset::no();

            for t in &s.transitions {
                match &t.test {
                    GraphTest::StopIfDone => {}
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

            let action = if idx == 0 {
                // Resets the start of the highlight span.
                GraphAction::Pop
            } else {
                GraphAction::Change(root.clone())
            };

            if !cs.covers_all() {
                cs.invert();
                let cs = self.charsets.intern(cs);

                s.transitions.push(GraphTransition {
                    origin: -1,
                    test: GraphTest::StopIfDone,
                    kind: None,
                    action: GraphAction::Pop,
                });
                s.transitions.push(GraphTransition {
                    origin: -1,
                    test: GraphTest::Charset(cs),
                    kind: None,
                    action: action.clone(),
                });
                s.transitions.push(GraphTransition {
                    origin: -1,
                    test: GraphTest::Chars(1),
                    kind: None,
                    action,
                });
            }
        }
    }

    // Assign IDs to states.
    fn finalize_assign_state_ids(&mut self) {
        let mut id = 0;
        for s in &self.states {
            let Some(s) = s.upgrade() else {
                continue;
            };

            let mut s = s.borrow_mut();
            s.id = id;
            id += 1;
        }
    }

    pub fn format_as_mermaid(&self) -> String {
        struct Visitor {
            visited: HashSet<*const WipStateCell>,
            output: String,
        }

        fn escape(s: &str) -> String {
            let mut res = String::with_capacity(s.len());
            for c in s.chars() {
                match c {
                    '\t' => res.push_str(r#"\\t"#),
                    '"' => res.push_str("&quot;"),
                    '\\' => res.push_str(r#"\\"#),
                    _ => res.push(c),
                }
            }
            res
        }

        fn print_state(visitor: &mut Visitor, src: Rc<WipStateCell>) {
            if !visitor.visited.insert(&*src) {
                return;
            }

            let transitions = &src.borrow().transitions;
            let mut iter = transitions.iter().peekable();

            while let Some(t) = iter.next() {
                let label = match &t.test {
                    GraphTest::StopIfDone => "StopIfDone".to_string(),
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
                            if next.action != t.action {
                                break;
                            }

                            _ = write!(label, ", {next_s}");
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
                            if next.action != t.action {
                                break;
                            }

                            _ = write!(label, ", {next_s}");
                            iter.next();
                        }

                        label.push(')');
                        label
                    }
                };

                let dst = match &t.action {
                    GraphAction::Change(dst) => {
                        format!("{}", dst.borrow().id)
                    }
                    GraphAction::Push(dst) => {
                        format!(
                            "push{}[/\"{}\"/]",
                            src.borrow().id << 16 | dst.borrow().id,
                            dst.borrow().name.unwrap()
                        )
                    }
                    GraphAction::Pop => {
                        format!("pop{}@{{ shape: stop }}", src.borrow().id << 16)
                    }
                };

                let label = escape(&label);
                _ = writeln!(
                    &mut visitor.output,
                    "    {src} -->|\"{label}<br/>{kind:?}\"| {dst}",
                    src = src.borrow().id,
                    kind = t.kind,
                );

                if let GraphAction::Change(dst) = &t.action {
                    print_state(visitor, dst.clone());
                }
            }
        }

        let mut visitor = Visitor { visited: HashSet::new(), output: String::new() };

        _ = writeln!(&mut visitor.output, "flowchart TD");
        for src in &self.roots {
            _ = writeln!(
                &mut visitor.output,
                "    {}[\"{}\"]",
                src.borrow().id,
                src.borrow().name.unwrap()
            );
        }
        for src in &self.roots {
            print_state(&mut visitor, src.clone());
        }

        visitor.output.pop(); // Remove the last newline character.
        visitor.output
    }

    pub fn charsets(&self) -> Vec<Rc<Charset>> {
        self.charsets.extract()
    }

    pub fn states(&self) -> Vec<Rc<WipStateCell>> {
        self.states.iter().filter_map(Weak::upgrade).collect()
    }
}

struct CharsetInterner {
    charsets: Vec<Weak<Charset>>,
}

impl CharsetInterner {
    pub fn intern(&mut self, mut charset: Charset) -> Rc<Charset> {
        if let Some(rc) = self.charsets.iter().filter_map(|w| w.upgrade()).find(|c| **c == charset)
        {
            return rc;
        }

        charset.id = self.charsets.len();

        let rc = Rc::new(charset);
        self.charsets.push(Rc::downgrade(&rc));
        rc
    }

    pub fn extract(&self) -> Vec<Rc<Charset>> {
        self.charsets.iter().filter_map(Weak::upgrade).collect()
    }
}

#[derive(Debug, Default)]
pub struct GraphState {
    pub id: usize,
    pub transitions: Vec<GraphTransition>,

    name: Option<&'static str>,
    coverage: Charset,
}

pub type WipStateCell = RefCell<GraphState>;

#[derive(Clone)]
pub enum GraphAction {
    // Same as super::Action
    Change(Rc<WipStateCell>),
    Push(Rc<WipStateCell>),
    Pop,
}

impl Debug for GraphAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GraphAction::Change(s) => write!(f, "Change({:p})", Rc::as_ptr(s)),
            GraphAction::Push(s) => write!(f, "Push({:p})", Rc::as_ptr(s)),
            GraphAction::Pop => write!(f, "Pop"),
        }
    }
}

impl PartialEq for GraphAction {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GraphAction::Change(a), GraphAction::Change(b)) => Rc::ptr_eq(a, b),
            (GraphAction::Push(a), GraphAction::Push(b)) => Rc::ptr_eq(a, b),
            (GraphAction::Pop, GraphAction::Pop) => true,
            _ => false,
        }
    }
}

impl Eq for GraphAction {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraphTest {
    StopIfDone,
    Chars(usize),
    Charset(Rc<Charset>),
    Prefix(String),
    PrefixInsensitive(String),
}

#[derive(Debug, Clone)]
pub struct GraphTransition {
    origin: i32,
    pub test: GraphTest,
    pub kind: Option<HighlightKind>,
    pub action: GraphAction,
}

#[derive(Clone)]
pub struct Charset {
    id: usize,
    bits: [bool; 256],
}

impl Charset {
    pub fn no() -> Self {
        Charset { id: 0, bits: [false; 256] }
    }

    pub fn yes() -> Self {
        Charset { id: 0, bits: [true; 256] }
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn bits(&self) -> &[bool; 256] {
        &self.bits
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

impl Debug for Charset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let show_char = |f: &mut std::fmt::Formatter<'_>, b: usize| {
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

impl PartialEq for Charset {
    fn eq(&self, other: &Self) -> bool {
        self.bits == other.bits
    }
}

impl Eq for Charset {}

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
