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
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::{ManuallyDrop, forget};
use std::{mem, ptr, slice};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Dot, Hir, HirKind, Look};

use super::{Action, Consume, HighlightKind};
use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::helpers::AsciiStringHelpers;
use crate::highlighter::{CharsetFormatter, Transition};

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
    Pop(usize),         // count
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
                    (r#"//.*"#, Comment, Pop(1)),
                    (r#"/\*"#, Comment, Push("comment")),
                    (r#"""#, String, Push("string")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?\b"#, Number, Pop(1)),
                    (r#"(?:true|false|null)\b"#, Keyword, Pop(1)),
                ],
            },
            StateDefinition { name: "comment", rules: &[(r#"\*/"#, Comment, Pop(1))] },
            StateDefinition {
                name: "string",
                rules: &[(r#"\\"#, String, Push("string_escape")), (r#"""#, String, Pop(1))],
            },
            StateDefinition { name: "string_escape", rules: &[(r#"."#, String, Pop(1))] },
        ],
    }
};

struct WipState {
    transitions: Vec<WipTransition>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum WipAction {
    // Same as super::Action
    Change(usize),
    Push(usize),
    Pop(usize),

    /// An indicator that this transition between states is not yet final
    /// and will be later resolved to fall back to another state.
    ///
    /// Example:
    /// * `[eE]\d+` -> [`HighlightKind::Number`] (here: just the exponent suffix)
    /// * `\w+`     -> [`HighlightKind::Keyword`]
    ///
    /// If after the `[eE]` we fail to find a digit, we should "fall back"
    /// to the next best rule that encompasses the `[eE]` so far: `\w+`.
    ResolveFallback,
}

#[derive(Debug, PartialEq, Eq)]
enum WipConsume {
    // Same as super::Consume
    Chars(usize),
    Prefix(String),
    PrefixInsensitive(String),
    Charset(Box<[bool; 256]>),
    Line,

    /// A special case for transitions that are not yet finalized.
    Never,
}

struct WipTransition {
    test: WipConsume,
    kind: HighlightKind,
    action: WipAction,
}

struct WipContext<'a> {
    states: &'a mut Vec<WipState>,
    kind: HighlightKind,
}

impl WipContext<'_> {
    fn add_state(&mut self) -> usize {
        self.states.push(WipState { transitions: Vec::new() });
        self.states.len() - 1
    }

    fn add_transition(&mut self, src: usize, dst: WipAction, test: WipConsume) -> WipAction {
        let transitions = &mut self.states[src].transitions;

        // Check if the edge already exists.
        for t in transitions.iter() {
            if t.test == test {
                match t.action {
                    WipAction::Change(_) => return t.action,
                    _ => panic!("Existing edge with non-change action"),
                }
            }
        }

        // Check for plausibility: if any prior test encompasses the new test, panic.
        for t in transitions.iter() {
            use WipConsume::*;

            if match (&t.test, &test) {
                (Prefix(p), Prefix(s)) => s.starts_with(p),
                (PrefixInsensitive(p), Prefix(s) | PrefixInsensitive(s)) => {
                    s.starts_with_ignore_ascii_case(p)
                }
                (Charset(p), Charset(n)) => {
                    // If all the bits in `n` are also true in `p`
                    n.iter().zip(p.iter()).all(|(&n, &p)| !n || p)
                }
                (Chars(_), _) => true,
                (Line, _) => true,
                _ => false,
            } {
                panic!(
                    "Attempted to add unreachable test: {:?} is encompassed by {:?}",
                    test, t.test
                );
            }
        }

        transitions.push(WipTransition { test, kind: self.kind, action: dst });
        dst
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
            HirKind::Look(Look::WordAscii) => {
                self.add_transition(src, WipAction::ResolveFallback, WipConsume::Never);
                self.transform_option(src, dst);
                dst
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
        let charset = self.class_to_charset(class);
        self.add_transition(src, dst, WipConsume::Charset(charset))
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
            let str = String::from_utf8(slice::from_ref(&ch).to_vec()).unwrap();

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
        self.add_transition(src, dst, WipConsume::Line)
    }

    // .
    fn transform_any(&mut self, src: usize, dst: WipAction) -> WipAction {
        self.add_transition(src, dst, WipConsume::Chars(1))
    }

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

        let mut it = hirs.iter().peekable();
        let mut src = WipAction::Change(src);

        while let Some(mut hir) = it.next() {
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

                let next =
                    if it.peek().is_some() { WipAction::Change(self.add_state()) } else { dst };
                src = self.add_transition(src_idx, next, WipConsume::PrefixInsensitive(str));
            } else {
                // Any other sequence is simply concatenated.
                let next =
                    if it.peek().is_some() { WipAction::Change(self.add_state()) } else { dst };
                src = self.transform(src_idx, next, hir);
            }
        }

        src
    }

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

    fn class_to_charset(&mut self, class: &ClassBytes) -> Box<[bool; 256]> {
        let mut charset = Box::new([false; 256]);

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
}

fn print_mermaid(def_states: &[StateDefinition], states: &[WipState]) {
    // Print header for Mermaid graph
    println!("---");
    println!("config:");
    println!("  layout: elk");
    println!("  elk:");
    println!("    nodePlacementStrategy: NETWORK_SIMPLEX");
    println!("---");
    println!("flowchart TD");

    fn print_transition(def_states: &[StateDefinition], src_idx: usize, t: &WipTransition) {
        let dst = match t.action {
            WipAction::Change(idx) => format!("{idx}"),
            WipAction::Push(idx) => {
                format!("push{}[/\"Push({})\"/]", src_idx << 16 | idx, def_states[idx].name)
            }
            WipAction::Pop(count) => {
                format!("pop{}[/\"Pop({count})\"/]", src_idx << 16 | count)
            }
            WipAction::ResolveFallback => {
                format!("pending{}[/Pending/]", src_idx << 16)
            }
        };
        let label = match &t.test {
            WipConsume::Prefix(s) => format!("Prefix({s})"),
            WipConsume::PrefixInsensitive(s) => format!("PrefixInsensitive({s})"),
            WipConsume::Charset(c) => format!("Charset({:?})", CharsetFormatter(c)),
            WipConsume::Chars(n) => format!("Chars({n})"),
            WipConsume::Line => "Line".to_string(),
            WipConsume::Never => "Never".to_string(),
        };
        let label = label.replace('"', "&quot;");
        let label = label.replace('\\', r#"\\"#);
        println!("    {src_idx} -->|\"{label}\"| {dst}");
    }

    fn print_state_bfs(
        visited: &mut HashSet<usize>,
        def_states: &[StateDefinition],
        states: &[WipState],
        src_idx: usize,
    ) {
        if !visited.insert(src_idx) {
            return;
        }
        for t in &states[src_idx].transitions {
            print_transition(def_states, src_idx, t);
        }
        for t in &states[src_idx].transitions {
            if let WipAction::Change(idx) = t.action {
                print_state_bfs(visited, def_states, states, idx);
            }
        }
    }

    fn print_state_dfs(
        visited: &mut HashSet<usize>,
        def_states: &[StateDefinition],
        states: &[WipState],
        src_idx: usize,
    ) {
        if !visited.insert(src_idx) {
            return;
        }
        for t in &states[src_idx].transitions {
            print_transition(def_states, src_idx, t);
            if let WipAction::Change(idx) = t.action {
                print_state_bfs(visited, def_states, states, idx);
            }
        }
    }

    let mut visited = HashSet::with_capacity(states.len());

    for (idx, s) in def_states.iter().enumerate() {
        println!("    {idx}[\"{}\"]", s.name);
    }
    for src_idx in 0..def_states.len() {
        print_state_dfs(&mut visited, def_states, states, src_idx);
    }

    assert_eq!(visited.len(), states.len(), "Not all states were visited");
}

#[allow(dead_code)]
pub fn parse_language_definition(def: &LanguageDefinition) {
    let mut state_names = HashMap::new();
    let mut states = Vec::new();

    for state in def.states {
        state_names.insert(state.name, states.len());
        states.push(WipState { transitions: Vec::new() });
    }

    for (ground_idx, state) in def.states.iter().enumerate() {
        for (pattern, kind, action) in state.rules {
            let mut ctx = WipContext { states: &mut states, kind: *kind };
            let dst = match action {
                ActionDefinition::Push(name) => match state_names.get(name) {
                    Some(&idx) => WipAction::Push(idx),
                    None => panic!("Unknown state name: {name}"),
                },
                ActionDefinition::Pop(count) => WipAction::Pop(*count),
            };
            let hir = regex_syntax::ParserBuilder::new()
                .utf8(false)
                .unicode(false)
                .dot_matches_new_line(true)
                .build()
                .parse(pattern)
                .unwrap();
            ctx.transform(ground_idx, dst, &hir);
        }
    }

    print_mermaid(def.states, &states);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_language_definition() {
        parse_language_definition(&JSON);
    }
}
