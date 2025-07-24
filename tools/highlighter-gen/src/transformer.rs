use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Write as _;
use std::mem;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind};

use crate::types::*;

pub fn parse_language_definition(def: &LanguageDefinition) {
    let mut ctx = WipContext::new();

    for s in def.states {
        ctx.add_root(s.name);
    }

    for (root, state) in def.states.iter().enumerate() {
        for rule in state.rules {
            ctx.parse(root, rule);
        }
    }

    ctx.compute_required_charsets();
    ctx.connect_fallbacks();
    print!("{}", ctx.format_as_mermaid());
}

#[derive(Default)]
struct WipState {
    name: Option<&'static str>,
    transitions: Vec<WipTransition>,
    depth: usize,
    shallowest_parent: Option<usize>,
    required_charset: Charset,
    fallback_done: bool,
}

type WipStateCell = RefCell<WipState>;

struct WipContext {
    states: Vec<WipStateCell>,
    root_count: usize,
    kind: HighlightKind,
}

impl WipContext {
    fn new() -> Self {
        WipContext { states: Vec::with_capacity(16), root_count: 0, kind: HighlightKind::Other }
    }

    fn add_root(&mut self, name: &'static str) {
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

    fn parse(&mut self, root: usize, rule: &Rule) {
        let hir = regex_syntax::ParserBuilder::new()
            .utf8(false)
            .unicode(false)
            .dot_matches_new_line(true)
            .build()
            .parse(rule.0)
            .unwrap();
        self.kind = rule.1;
        let dst = match rule.2 {
            ActionDefinition::Push(name) => WipAction::Push(self.get_root_by_name(name)),
            ActionDefinition::Pop => WipAction::Pop,
        };
        self.transform(root, dst, &hir);
    }

    fn transform(&mut self, src: usize, dst: WipAction, hir: &Hir) -> WipAction {
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
    fn transform_literal(&mut self, src: usize, dst: WipAction, lit: &[u8]) -> WipAction {
        self.add_transition(src, dst, WipConsume::Prefix(String::from_utf8(lit.to_vec()).unwrap()))
    }

    // [a-z]+
    fn transform_class_plus(
        &mut self,
        src: usize,
        dst: WipAction,
        class: &ClassBytes,
    ) -> WipAction {
        let c = Box::new(self.class_to_charset(class));
        self.add_transition(src, dst, WipConsume::Charset(c))
    }

    // [eE]
    fn transform_class(&mut self, src: usize, dst: WipAction, class: &ClassBytes) -> WipAction {
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
    fn transform_option(&mut self, src: usize, dst: WipAction) -> WipAction {
        self.add_transition(src, dst, WipConsume::Chars(0))
    }

    // .*
    fn transform_any_star(&mut self, src: usize, dst: WipAction) -> WipAction {
        self.add_transition(src, dst, WipConsume::Chars(usize::MAX))
    }

    // .
    fn transform_any(&mut self, src: usize, dst: WipAction) -> WipAction {
        self.add_transition(src, dst, WipConsume::Chars(1))
    }

    // (a)(b)
    fn transform_concat(&mut self, src: usize, dst: WipAction, hirs: &[Hir]) -> WipAction {
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
        let mut src = WipAction::Change(src);

        while let Some(hir) = it.next() {
            let src_idx = match src {
                WipAction::Change(idx) => idx,
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
    fn transform_alt(&mut self, src: usize, dst: WipAction, hirs: &[Hir]) -> WipAction {
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

    fn add_transition(&mut self, src: usize, dst: WipAction, test: WipConsume) -> WipAction {
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
            use WipConsume::*;

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
        if let WipAction::Change(dst) = dst {
            let mut dst = self.states[dst].borrow_mut();
            if dst
                .shallowest_parent
                .is_none_or(|p| p != src && self.states[p].borrow().depth > s.depth)
            {
                dst.shallowest_parent = Some(src);
            }
        }

        s.transitions.push(WipTransition { test, kind: self.kind, action: dst });
        dst
    }

    fn compute_required_charsets(&self) {
        for src in 0..self.root_count {
            self.compute_required_charsets_impl(src);
        }
    }

    fn compute_required_charsets_impl(&self, src: usize) {
        let src = self.states[src].borrow();

        for t in &src.transitions {
            let WipAction::Change(dst) = t.action else {
                continue;
            };

            let mut dst = self.states[dst].borrow_mut();
            let cs = &mut dst.required_charset;

            match &t.test {
                WipConsume::Chars(_) => cs.fill(true),
                WipConsume::Prefix(s) => cs.merge_str(s),
                WipConsume::PrefixInsensitive(s) => cs.merge_str_insensitive(s),
                WipConsume::Charset(c) => cs.merge(c),
            }

            cs.merge(&src.required_charset);
        }

        for t in &src.transitions {
            let WipAction::Change(dst) = t.action else {
                continue;
            };
            self.compute_required_charsets_impl(dst);
        }
    }

    fn connect_fallbacks(&self) {
        for src in 0..self.root_count {
            self.connect_fallbacks_impl(src);
        }
    }

    fn connect_fallbacks_impl(&self, src: usize) {
        {
            let src = self.states[src].borrow();

            for t in &src.transitions {
                if let WipAction::Change(dst) = t.action
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
        let required_charset = &src.required_charset;
        if required_charset.is_any() {
            return;
        }

        let mut parent = src.shallowest_parent;
        let mut found = None;
        while let Some(p) = parent {
            let pb = self.states[p].borrow();
            if pb.required_charset.is_superset(required_charset) {
                found = Some(p);
                break;
            }
            parent = pb.shallowest_parent;
        }

        src.transitions.push(WipTransition {
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

    fn format_as_mermaid(&self) -> String {
        struct Visitor<'a> {
            states: &'a [WipStateCell],
            visited: HashSet<usize>,
            output: String,
        }

        fn print_transition(visitor: &mut Visitor, src_id: usize, t: &WipTransition) {
            let src = &visitor.states[src_id].borrow();
            let dst = match &t.action {
                WipAction::Change(dst) => {
                    format!("{dst}")
                }
                WipAction::Push(dst) => {
                    format!("push{}[/\"Push({})\"/]", src_id << 16 | dst, src.name.unwrap())
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

        fn print_state_bfs(visitor: &mut Visitor, src: usize) {
            if !visitor.visited.insert(src) {
                return;
            }
            for t in &visitor.states[src].borrow().transitions {
                print_transition(visitor, src, t);
            }
            for t in &visitor.states[src].borrow().transitions {
                if let WipAction::Change(idx) = &t.action {
                    print_state_bfs(visitor, *idx);
                }
            }
        }

        fn print_state_dfs(visitor: &mut Visitor, src: usize) {
            if !visitor.visited.insert(src) {
                return;
            }
            for t in &visitor.states[src].borrow().transitions {
                print_transition(visitor, src, t);
                if let WipAction::Change(idx) = &t.action {
                    print_state_bfs(visitor, *idx);
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
            print_state_dfs(&mut visitor, src);
        }

        visitor.output
    }
}
