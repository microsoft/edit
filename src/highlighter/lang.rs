#![allow(dead_code, unused)]

use core::panic;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::{ManuallyDrop, forget};
use std::{mem, ptr, slice};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};

use super::{Action, Consume, HighlightKind};
use crate::arena::{Arena, ArenaString, scratch_arena};
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
                    // Comments (jsonc)
                    (r#"//.*"#, Comment, Pop(1)),
                    (r#"/\*"#, Comment, Push("comment")),
                    // Strings
                    (r#"""#, String, Push("string")),
                    // Numbers (start: minus or digit)
                    (r#"-\d*(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop(1)),
                    (r#"\d*(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop(1)),
                    // Booleans/null
                    (r#"true\b"#, Keyword, Pop(1)),
                    (r#"false\b"#, Keyword, Pop(1)),
                    (r#"null\b"#, Keyword, Pop(1)),
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
    Change(usize),
    Push(usize),
    Pop(usize),
}

#[derive(PartialEq, Eq)]
enum WipConsume {
    Chars(usize),
    Prefix(String),
    PrefixInsensitive(String),
    Charset(Box<[bool; 256]>),
    Line,
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
        let src = &mut self.states[src].transitions;

        // Check if the edge already exists.
        for t in src.iter() {
            if t.test == test {
                match t.action {
                    WipAction::Change(_) => return t.action,
                    _ => panic!("Existing edge with non-change action"),
                }
            }
        }

        src.push(WipTransition { test, kind: self.kind, action: dst });
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
            HirKind::Look(Look::WordAscii) => dst,
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
    println!("%%{{init:{{'fontFamily':'monospace','flowchart':{{'defaultRenderer':'elk'}}}}}}%%");
    println!("graph TD");

    // Print nodes (states)
    for (idx, _state) in states.iter().enumerate() {
        println!(
            "    {idx}[\"{}\"]",
            match def_states.get(idx) {
                Some(state) => state.name,
                None => &format!("{idx}"),
            }
        );
    }

    // Print edges (transitions)
    for (src_idx, state) in states.iter().enumerate() {
        for t in &state.transitions {
            let dst = match t.action {
                WipAction::Change(idx) => format!("{idx}"),
                WipAction::Push(idx) => {
                    format!("push{}[/\"Push({})\"/]", src_idx << 16 | idx, def_states[idx].name)
                }
                WipAction::Pop(count) => {
                    format!("pop{}[/\"Pop({count})\"/]", src_idx << 16 | count)
                }
            };
            let label = match &t.test {
                WipConsume::Prefix(s) => format!("Prefix({s})"),
                WipConsume::PrefixInsensitive(s) => format!("PrefixInsensitive({s})"),
                WipConsume::Charset(c) => format!("Charset({:?})", CharsetFormatter(c)),
                WipConsume::Chars(n) => format!("Chars({n})"),
                WipConsume::Line => "Line".to_string(),
            };
            let label = label.replace('"', "&quot;");
            let label = label.replace('\\', r#"\\"#);
            println!("    {src_idx} -->|\"{label}\"| {dst}");
        }
    }
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
