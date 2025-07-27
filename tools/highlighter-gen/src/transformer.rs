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
    charsets: Vec<Weak<Charset>>,

    origin: i32,
    kind: HighlightKind,
}

impl GraphBuilder {
    pub fn new() -> Self {
        GraphBuilder {
            roots: Vec::with_capacity(16),
            states: Vec::with_capacity(16),
            charsets: Vec::with_capacity(16),

            origin: -1,
            kind: HighlightKind::Other,
        }
    }

    pub fn add_root(&mut self, name: &'static str) {
        let s = self.add_state(0);
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
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .dot_matches_new_line(true)
            .build()
            .parse(rule.0)
            .unwrap();
        let dst = match rule.2 {
            ActionDefinition::Push(name) => GraphAction::Push(self.get_root_by_name(name)),
            ActionDefinition::Pop => GraphAction::Pop,
        };
        self.origin += 1;
        self.kind = rule.1;
        self.transform(self.roots[root].clone(), dst, &hir);
    }

    fn transform(&mut self, src: Rc<WipStateCell>, dst: GraphAction, hir: &Hir) -> GraphAction {
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
                    let dst = self.transform_class_plus(src.clone(), dst.clone(), class);
                    self.transform_option(src, dst.clone());
                    dst
                }
                (0, Some(1), _) => {
                    let dst = self.transform(src.clone(), dst.clone(), &rep.sub);
                    self.transform_option(src, dst.clone());
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
        src: Rc<WipStateCell>,
        dst: GraphAction,
        lit: &[u8],
    ) -> GraphAction {
        self.add_transition(
            src,
            dst,
            GraphConsume::Prefix(String::from_utf8(lit.to_vec()).unwrap()),
        )
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        src: Rc<WipStateCell>,
        dst: GraphAction,
        class: &ClassBytes,
    ) -> GraphAction {
        let c = self.class_to_charset(class);
        let c = self.intern_charset(c);
        self.add_transition(src, dst, GraphConsume::Charset(c))
    }

    // [eE]
    fn transform_class(
        &mut self,
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
                GraphConsume::PrefixInsensitive(str)
            } else {
                GraphConsume::Prefix(str)
            };

            let d = self.add_transition(src.clone(), dst.clone(), test);
            if d != *actual_dst.get_or_insert(d.clone()) {
                panic!("Diverging destinations for class transformer: {class:?}");
            }
        }

        actual_dst.unwrap_or(dst)
    }

    // .?
    fn transform_option(&mut self, src: Rc<WipStateCell>, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(0))
    }

    // .*
    fn transform_any_star(&mut self, src: Rc<WipStateCell>, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(usize::MAX))
    }

    // .
    fn transform_any(&mut self, src: Rc<WipStateCell>, dst: GraphAction) -> GraphAction {
        self.add_transition(src, dst, GraphConsume::Chars(1))
    }

    // (a)(b)
    fn transform_concat(
        &mut self,
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

        let depth = src.borrow().depth + 1;
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
                    dst.clone()
                };
                src = self.add_transition(src_idx, next, GraphConsume::PrefixInsensitive(str));
            } else {
                // Any other sequence is simply concatenated.
                let next = if it.peek().is_some() {
                    GraphAction::Change(self.add_state(depth))
                } else {
                    dst.clone()
                };
                src = self.transform(src_idx, next, hir);
            }
        }

        src
    }

    // (a|b)
    fn transform_alt(
        &mut self,
        src: Rc<WipStateCell>,
        dst: GraphAction,
        hirs: &[Hir],
    ) -> GraphAction {
        let mut actual_dst = None;

        for hir in hirs {
            let d = self.transform(src.clone(), dst.clone(), hir);
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

    fn add_state(&mut self, depth: usize) -> Rc<WipStateCell> {
        let s = Rc::new(WipStateCell::new(GraphState {
            id: 0,
            transitions: Vec::new(),

            name: None,
            depth,
            coverage: Charset::default(),
        }));
        self.states.push(Rc::downgrade(&s));
        s
    }

    fn add_transition(
        &mut self,
        src: Rc<WipStateCell>,
        dst: GraphAction,
        test: GraphConsume,
    ) -> GraphAction {
        let mut s = src.borrow_mut();

        // Check if the edge already exists.
        for t in &s.transitions {
            if t.test == test {
                if mem::discriminant(&t.action) != mem::discriminant(&dst) {
                    panic!(
                        "Diverging actions for the same test: {:?} -> {:?} vs {:?}",
                        test, t.action, dst
                    );
                }
                return t.action.clone();
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

        s.transitions.push(GraphTransition {
            origin: self.origin,
            test,
            kind: self.kind,
            action: dst.clone(),
        });
        dst
    }

    pub fn finalize(&mut self) {
        for s in &self.roots {
            let mut s = s.borrow_mut();
            s.coverage.fill(true);
        }

        for s in &self.states[self.roots.len()..] {
            let Some(s) = s.upgrade() else {
                continue;
            };

            let mut s = s.borrow_mut();
            let s = &mut *s;

            for t in &s.transitions {
                match &t.test {
                    GraphConsume::Chars(_) => {
                        s.coverage.fill(true);
                        break;
                    }
                    GraphConsume::Charset(c) => {
                        s.coverage.merge(c);
                    }
                    _ => {}
                }
            }
        }

        for root in &self.roots {
            Self::fallback_find(root.clone());
        }

        for s in &self.states[self.roots.len()..] {
            let Some(s) = s.upgrade() else {
                continue;
            };

            let mut s = s.borrow_mut();
            if !s.coverage.covers_all() {
                s.transitions.push(GraphTransition {
                    origin: -1,
                    test: GraphConsume::Chars(0),
                    kind: HighlightKind::Other,
                    action: GraphAction::Pop,
                });
            }
        }

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

    fn fallback_find(src: Rc<WipStateCell>) {
        let s = src.borrow();
        let mut has_children = false;

        for t in &s.transitions {
            if let GraphAction::Change(dst) = &t.action {
                Self::fallback_find(dst.clone());
                has_children = true;
            }
        }

        if !has_children {
            return;
        }

        for t in &s.transitions {
            if !matches!(t.test, GraphConsume::Chars(_) | GraphConsume::Charset(_)) {
                continue;
            }

            for t_dst in &s.transitions {
                if t_dst.origin >= t.origin {
                    break;
                }
                if let GraphAction::Change(dst) = &t_dst.action {
                    Self::fallback_add(dst.clone(), t_dst.origin, t);
                }
            }
        }
    }

    fn fallback_add(src: Rc<WipStateCell>, filter_origin: i32, fallback: &GraphTransition) {
        let Ok(mut s) = src.try_borrow_mut() else {
            return; // TODO
        };

        for t in &s.transitions {
            if let GraphAction::Change(dst) = &t.action
                && t.origin == filter_origin
            {
                Self::fallback_add(dst.clone(), filter_origin, fallback);
            }
        }

        let mut cs_buf = MaybeUninit::uninit();
        let cs = match &fallback.test {
            GraphConsume::Chars(_) => cs_buf.write(Charset::yes()),
            GraphConsume::Charset(c) => &**c,
            _ => unreachable!(),
        };

        if !cs.is_strict_superset(&s.coverage) {
            return;
        }

        let mut f = fallback.clone();
        f.origin = -1;
        s.transitions.push(f);
        s.coverage.merge(cs);
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
                    "    {src} -->|\"{label}<br/>{kind}\"| {dst}",
                    src = src.borrow().id,
                    kind = t.kind.as_str()
                );

                if let GraphAction::Change(dst) = &t.action {
                    print_state(visitor, dst.clone());
                }
            }
        }

        let mut visitor = Visitor { visited: HashSet::new(), output: String::new() };

        _ = write!(&mut visitor.output, "---\nconfig:\n  layout: elk\n---\nflowchart TD\n");
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

        visitor.output
    }

    pub fn charsets(&self) -> Vec<Rc<Charset>> {
        self.charsets.iter().filter_map(Weak::upgrade).collect()
    }

    pub fn states(&self) -> Vec<Rc<WipStateCell>> {
        self.states.iter().filter_map(Weak::upgrade).collect()
    }
}

#[derive(Debug, Default)]
pub struct GraphState {
    pub id: usize,
    pub transitions: Vec<GraphTransition>,

    name: Option<&'static str>,
    depth: usize,
    coverage: Charset,
}

pub type WipStateCell = RefCell<GraphState>;

#[derive(Debug, Clone)]
pub enum GraphAction {
    // Same as super::Action
    Change(Rc<WipStateCell>),
    Push(Rc<WipStateCell>),
    Pop,
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
pub enum GraphConsume {
    // Same as super::Consume
    Chars(usize),
    Prefix(String),
    PrefixInsensitive(String),
    Charset(Rc<Charset>),
}

#[derive(Debug, Clone)]
pub struct GraphTransition {
    origin: i32,
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

    pub fn merge(&mut self, other: &Charset) {
        for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
            *a |= *b;
        }
    }

    pub fn covers_all(&self) -> bool {
        self.bits.iter().all(|&b| b)
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

    pub fn iter(&self) -> impl Iterator<Item = bool> + '_ {
        self.bits.iter().copied()
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
