use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Write as _};
use std::mem;
use std::ops::{Index, IndexMut};
use std::rc::{Rc, Weak};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind};

use crate::definitions::*;

pub struct GraphBuilder {
    states: Vec<WipStateCell>,
    charsets: Vec<Weak<Charset>>,
    root_count: usize,
    kind: HighlightKind,
}

impl GraphBuilder {
    pub fn new() -> Self {
        GraphBuilder {
            states: Vec::with_capacity(16),
            charsets: Vec::with_capacity(16),
            root_count: 0,
            kind: HighlightKind::Other,
        }
    }

    pub fn add_root(&mut self, name: &'static str) {
        let idx = self.add_state(0);
        {
            let mut s = self.states[idx].borrow_mut();
            s.name = Some(name);
            s.fallback_done = true;
        }
        self.root_count += 1;
    }

    fn get_root_by_name(&self, name: &str) -> usize {
        for (idx, s) in self.states.iter().enumerate().take(self.root_count) {
            if s.borrow().name == Some(name) {
                return idx;
            }
        }
        panic!("Unknown state name: {name}")
    }

    pub fn parse(&mut self, root: usize, rule: &Rule) {
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .dot_matches_new_line(true)
            .build()
            .parse(rule.0)
            .unwrap();
        self.kind = rule.1;
        let dst = match rule.2 {
            ActionDefinition::Push(name) => GraphAction::Push(self.get_root_by_name(name)),
            ActionDefinition::Pop => GraphAction::Pop,
        };
        self.transform(root, dst, &hir);
    }

    fn transform(&mut self, src: usize, dst: GraphAction, hir: &Hir) -> GraphAction {
        fn is_any_class(class: &ClassBytes) -> bool {
            class.ranges() == [ClassBytesRange::new(0, 255)]
        }

        match hir.kind() {
            HirKind::Empty => self.transform_option(src, dst),
            HirKind::Literal(lit) => self.transform_literal(src, dst, &lit.0),
            HirKind::Class(Class::Bytes(class)) if is_any_class(class) => {
                self.transform_any(src, dst)
            }
            HirKind::Class(Class::Bytes(class)) => self.transform_class(src, dst, class),
            HirKind::Class(Class::Unicode(class)) => {
                self.transform_class(src, dst, &class.to_byte_class().unwrap())
            }
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
    fn transform_literal(&mut self, src: usize, dst: GraphAction, lit: &[u8]) -> GraphAction {
        self.add_transition(
            src,
            dst,
            GraphConsume::Prefix(String::from_utf8(lit.to_vec()).unwrap()),
        )
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        src: usize,
        dst: GraphAction,
        class: &ClassBytes,
    ) -> GraphAction {
        let c = self.class_to_charset(class);
        let c = self.intern_charset(c);
        self.add_transition(src, dst, GraphConsume::Charset(c))
    }

    // [eE]
    fn transform_class(&mut self, src: usize, dst: GraphAction, class: &ClassBytes) -> GraphAction {
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
                GraphConsume::PrefixInsensitive(str)
            } else {
                GraphConsume::Prefix(str)
            };

            let d = self.add_transition(src, dst, test);
            if d != *actual_dst.get_or_insert(d) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(&mut self, src: usize, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(0))
    }

    // .*
    fn transform_any_star(&mut self, src: usize, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(usize::MAX))
    }

    // .
    fn transform_any(&mut self, src: usize, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(1))
    }

    // (a)(b)
    fn transform_concat(&mut self, src: usize, dst: GraphAction, hirs: &[Hir]) -> GraphAction {
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

        let depth = self.states[src].borrow().depth + 1;
        let mut it = hirs.iter().peekable();
        let mut src = GraphAction::Change(src);

        while let Some(hir) = it.next() {
            let src_idx = match src {
                GraphAction::Change(idx) => idx,
                _ => panic!("Unexpected action in transform_concat"),
            };

            if let Some(ch) = check_lowercase_literal(hir) {
                // Transform [aA][bB][cC] into PrefixInsensitive("abc").
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

                let next = if it.peek().is_some() {
                    GraphAction::Change(self.add_state(depth))
                } else {
                    dst
                };
                src = self.add_transition(src_idx, next, GraphConsume::PrefixInsensitive(str));
            } else {
                // Any other sequence is simply concatenated.
                let next = if it.peek().is_some() {
                    GraphAction::Change(self.add_state(depth))
                } else {
                    dst
                };
                src = self.transform(src_idx, next, hir);
            }
        }

        src
    }

    // (a|b)
    fn transform_alt(&mut self, src: usize, dst: GraphAction, hirs: &[Hir]) -> GraphAction {
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

    fn intern_charset(&mut self, mut charset: Charset) -> Rc<Charset> {
        if let Some(rc) = self.charsets.iter().filter_map(|w| w.upgrade()).find(|c| **c == charset)
        {
            return rc;
        }

        charset.id = self.charsets.len();

        let rc = Rc::new(charset);
        self.charsets.push(Rc::downgrade(&rc));
        rc
    }

    fn add_state(&mut self, depth: usize) -> usize {
        let idx = self.states.len();
        self.states.push(WipStateCell::new(WipState {
            name: None,

            transitions: Vec::new(),

            depth,
            shallowest_parent: None,
            required_charset: Charset::default(),
            fallback_done: false,
        }));
        idx
    }

    fn add_transition(&mut self, src: usize, dst: GraphAction, test: GraphConsume) -> GraphAction {
        let mut s = self.states[src].borrow_mut();

        // Check if the edge already exists.
        for t in &s.transitions {
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
        for t in &s.transitions {
            use GraphConsume::*;

            if match (&t.test, &test) {
                (Chars(_), _) => true,
                (Prefix(p), Prefix(s)) => s.starts_with(p.as_str()),
                (PrefixInsensitive(p), Prefix(s) | PrefixInsensitive(s)) => {
                    let s = s.as_bytes();
                    let p = p.as_bytes();
                    p.len() <= s.len() && s[..p.len()].eq_ignore_ascii_case(p)
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
        if let GraphAction::Change(dst) = dst {
            let mut dst = self.states[dst].borrow_mut();
            if dst
                .shallowest_parent
                .is_none_or(|p| p != src && self.states[p].borrow().depth > s.depth)
            {
                dst.shallowest_parent = Some(src);
            }
        }

        s.transitions.push(GraphTransition { test, kind: self.kind, action: dst });
        dst
    }

    pub fn finalize(&mut self) {
        if self.states.last().is_some_and(|s| s.borrow().transitions.is_empty()) {
            self.states.pop();
        }
        for src in 0..self.root_count {
            self.compute_required_charsets_impl(src);
        }
        for src in 0..self.root_count {
            self.connect_fallbacks_impl(src);
        }
    }

    fn compute_required_charsets_impl(&self, src: usize) {
        let src = self.states[src].borrow();

        for t in &src.transitions {
            let GraphAction::Change(dst) = t.action else {
                continue;
            };

            let mut dst = self.states[dst].borrow_mut();
            let cs = &mut dst.required_charset;

            match &t.test {
                GraphConsume::Chars(_) => cs.fill(true),
                GraphConsume::Prefix(s) => cs.merge_str(s),
                GraphConsume::PrefixInsensitive(s) => cs.merge_str_insensitive(s),
                GraphConsume::Charset(c) => cs.merge(c),
            }

            cs.merge(&src.required_charset);
        }

        for t in &src.transitions {
            let GraphAction::Change(dst) = t.action else {
                continue;
            };
            self.compute_required_charsets_impl(dst);
        }
    }

    fn connect_fallbacks_impl(&self, src: usize) {
        {
            let src = self.states[src].borrow();

            for t in &src.transitions {
                if let GraphAction::Change(dst) = t.action
                    && !self.states[dst].borrow().fallback_done
                {
                    self.connect_fallbacks_impl(dst);
                }
            }

            if src.fallback_done {
                return;
            }
        }

        let mut src = self.states[src].borrow_mut();

        if !src.transitions.iter().any(|t| matches!(t.test, GraphConsume::Chars(_))) {
            let action = 'res: {
                if !src.required_charset.covers_all() {
                    let mut parent = src.shallowest_parent;
                    while let Some(p) = parent {
                        let pb = self.states[p].borrow();
                        if pb.required_charset.is_superset(&src.required_charset) {
                            break 'res GraphAction::Change(p);
                        }
                        parent = pb.shallowest_parent;
                    }
                }
                GraphAction::Pop
            };

            src.transitions.push(GraphTransition {
                test: GraphConsume::Chars(0),
                kind: HighlightKind::Other,
                action,
            });
        }

        src.fallback_done = true;
    }

    pub fn format_as_mermaid(&self) -> String {
        struct Visitor<'a> {
            states: &'a [WipStateCell],
            visited: HashSet<usize>,
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

        fn print_state(visitor: &mut Visitor, src: usize) {
            if !visitor.visited.insert(src) {
                return;
            }

            let transitions = &visitor.states[src].borrow().transitions;
            let mut iter = transitions.iter().peekable();

            while let Some(t) = iter.next() {
                let label = match &t.test {
                    GraphConsume::Chars(usize::MAX) => "Chars(Line)".to_string(),
                    GraphConsume::Chars(n) => format!("Chars({n})"),
                    GraphConsume::Prefix(s) => {
                        let mut label = String::new();
                        _ = write!(label, "Prefix({s}");

                        loop {
                            let Some(next) = iter.peek() else {
                                break;
                            };
                            let GraphConsume::Prefix(next_s) = &next.test else {
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
                    GraphConsume::PrefixInsensitive(s) => {
                        let mut label = String::new();
                        _ = write!(label, "PrefixInsensitive({s}");

                        loop {
                            let Some(next) = iter.peek() else {
                                break;
                            };
                            let GraphConsume::PrefixInsensitive(next_s) = &next.test else {
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
                    GraphConsume::Charset(c) => format!("Charset({c:?})"),
                };

                let dst = match &t.action {
                    GraphAction::Change(dst) => {
                        format!("{dst}")
                    }
                    GraphAction::Push(dst) => {
                        format!(
                            "push{}[/\"{}\"/]",
                            src << 16 | dst,
                            visitor.states[*dst].borrow().name.unwrap()
                        )
                    }
                    GraphAction::Pop => {
                        format!("pop{}@{{ shape: stop }}", src << 16)
                    }
                };

                let label = escape(&label);
                _ = writeln!(
                    &mut visitor.output,
                    "    {src} -->|\"{label}<br/>{kind}\"| {dst}",
                    kind = t.kind.as_str()
                );

                if let GraphAction::Change(idx) = &t.action {
                    print_state(visitor, *idx);
                }
            }
        }

        let mut visitor =
            Visitor { states: &self.states, visited: HashSet::new(), output: String::new() };

        _ = write!(&mut visitor.output, "---\nconfig:\n  layout: elk\n---\nflowchart TD\n");
        for src in 0..self.root_count {
            _ = writeln!(
                &mut visitor.output,
                "    {}[\"{}\"]",
                src,
                self.states[src].borrow().name.unwrap()
            );
        }
        for src in 0..self.root_count {
            print_state(&mut visitor, src);
        }

        visitor.output
    }

    pub fn charsets(&self) -> Vec<Rc<Charset>> {
        self.charsets.iter().filter_map(Weak::upgrade).collect()
    }

    pub fn states(&self) -> &[WipStateCell] {
        &self.states
    }
}

#[derive(Default)]
pub struct WipState {
    name: Option<&'static str>,

    pub transitions: Vec<GraphTransition>,

    depth: usize,
    shallowest_parent: Option<usize>,
    required_charset: Charset,
    fallback_done: bool,
}

pub type WipStateCell = RefCell<WipState>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphAction {
    // Same as super::Action
    Change(usize),
    Push(usize),
    Pop,
}

#[derive(Debug, PartialEq, Eq)]
pub enum GraphConsume {
    // Same as super::Consume
    Chars(usize),
    Prefix(String),
    PrefixInsensitive(String),
    Charset(Rc<Charset>),
}

pub struct GraphTransition {
    pub test: GraphConsume,
    pub kind: HighlightKind,
    pub action: GraphAction,
}

#[derive(Clone)]
pub struct Charset {
    id: usize,
    bits: [bool; 256],
}

impl Charset {
    pub fn id(&self) -> usize {
        self.id
    }

    pub fn bits(&self) -> &[bool; 256] {
        &self.bits
    }

    pub fn fill(&mut self, value: bool) {
        self.bits.fill(value);
    }

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    pub fn merge_str(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.bits[*b as usize] = true;
        }
    }

    pub fn merge_str_insensitive(&mut self, s: &str) {
        for b in s.as_bytes() {
            self.bits[b.to_ascii_uppercase() as usize] = true;
            self.bits[b.to_ascii_lowercase() as usize] = true;
        }
    }

    pub fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b)
    }

    pub fn is_superset(&self, other: &Charset) -> bool {
        for (a, b) in self.bits.iter().zip(other.bits.iter()) {
            if *b && !*a {
                return false;
            }
        }
        true
    }

    pub fn iter(&self) -> impl Iterator<Item = bool> + '_ {
        self.bits.iter().copied()
    }
}

impl Default for Charset {
    fn default() -> Self {
        Charset { id: 0, bits: [false; 256] }
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
