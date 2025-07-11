#![allow(dead_code, unused)]

use core::panic;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::{ManuallyDrop, forget};
use std::{mem, ptr, slice};

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};

use super::{Action, Consume, HighlightKind};
use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::cell::SemiRefCell;
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
                    (r#"-?\d*(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop(1)),
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

type NodeCell<'a> = SemiRefCell<Node<'a>>;

// Nodes form a DFA graph which is mostly shaped like a tree.
// Each group of sibling nodes represent the edges coming out of a DFA state.
struct Node<'a> {
    edge_first: Option<&'a EdgeCell<'a>>,
    edge_last: Option<&'a EdgeCell<'a>>,
}

impl<'a> Node<'a> {
    fn new_in(arena: &'a Arena) -> &'a mut NodeCell<'a> {
        arena.alloc_uninit().write(NodeCell::new(Node { edge_first: None, edge_last: None }))
    }
}

type EdgeCell<'a> = SemiRefCell<Edge<'a>>;

struct Edge<'a> {
    edge_next: Option<&'a EdgeCell<'a>>,
    dst: &'a NodeCell<'a>,
    test: Consume<'a>,
}

fn add_edge<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    test: Consume<'a>,
) -> &'a NodeCell<'a> {
    let mut src = src.borrow_mut();

    // Check if the edge already exists.
    {
        let mut edge = src.edge_first;
        while let Some(e) = edge {
            let e = e.borrow();
            if e.test == test {
                return e.dst;
            }
            edge = e.edge_next;
        }
    }

    let edge = arena.alloc_uninit().write(EdgeCell::new(Edge { edge_next: None, dst, test }));

    if let Some(last) = src.edge_last {
        last.borrow_mut().edge_next = Some(edge);
    } else {
        src.edge_first = Some(edge);
    }

    src.edge_last = Some(edge);
    dst
}

fn transform<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    hir: &Hir,
) -> &'a NodeCell<'a> {
    fn is_any_class(class: &ClassBytes) -> bool {
        class.ranges() == [ClassBytesRange::new(0, 255)]
    }

    match hir.kind() {
        HirKind::Literal(lit) => transform_literal(arena, src, dst, &lit.0),
        HirKind::Class(Class::Bytes(class)) if is_any_class(class) => {
            transform_any(arena, src, dst)
        }
        HirKind::Class(Class::Bytes(class)) => transform_class(arena, src, dst, class),
        HirKind::Look(Look::WordAscii) => dst,
        HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
            (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                transform_any_star(arena, src, dst)
            }
            (0, None, HirKind::Class(Class::Bytes(class))) => {
                let dst = transform_class_plus(arena, src, dst, class);
                transform_option(arena, src, dst);
                dst
            }
            (0, Some(1), _) => {
                let dst = transform(arena, src, dst, &rep.sub);
                transform_option(arena, src, dst);
                dst
            }
            (1, None, HirKind::Class(Class::Bytes(class))) => {
                transform_class_plus(arena, src, dst, class)
            }
            _ => panic!("Unsupported HIR: {hir:?}"),
        },
        HirKind::Concat(hirs) if hirs.len() >= 2 => transform_concat(arena, src, dst, hirs),
        HirKind::Alternation(hirs) if hirs.len() >= 2 => transform_alt(arena, src, dst, hirs),
        _ => panic!("Unsupported HIR: {hir:?}"),
    }
}

// string
fn transform_literal<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    lit: &[u8],
) -> &'a NodeCell<'a> {
    let copy = arena.alloc_uninit_slice(lit.len()).write_clone_of_slice(lit);
    let copy = str::from_utf8(copy).unwrap();
    add_edge(arena, src, dst, Consume::Prefix(copy))
}

// [a-z]+
fn transform_class_plus<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    class: &ClassBytes,
) -> &'a NodeCell<'a> {
    let charset = class_to_charset(arena, class);
    add_edge(arena, src, dst, Consume::Charset(charset))
}

// [eE]
fn transform_class<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    class: &ClassBytes,
) -> &'a NodeCell<'a> {
    let charset = class_to_charset(arena, class);
    let mut actual_dst = None;

    for i in 0..256 {
        if !charset[i] {
            continue;
        }

        if i >= 128 {
            panic!("Invalid non-ASCII class character {i}");
        }

        let ch = i as u8;
        let copy = arena.alloc_uninit().write(ch.to_ascii_lowercase());
        let copy = str::from_utf8(slice::from_ref(copy)).unwrap();

        // NOTE: Uppercase chars have a lower numeric value than lowercase chars.
        // As such, we need to test for `is_ascii_uppercase`.
        let test = if ch.is_ascii_uppercase()
            && let upper = ch.to_ascii_lowercase() as usize
            && charset[upper]
        {
            charset[upper] = false;
            Consume::PrefixInsensitive(copy)
        } else {
            Consume::Prefix(copy)
        };

        let node = add_edge(arena, src, dst, test);
        if !ptr::eq(node, *actual_dst.get_or_insert(node)) {
            panic!("Diverging destinations for class transformer: {class:?}");
        }
    }

    actual_dst.unwrap_or(dst)
}

// .?
fn transform_option<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
) -> &'a NodeCell<'a> {
    add_edge(arena, src, dst, Consume::Chars(0))
}

// .*
fn transform_any_star<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
) -> &'a NodeCell<'a> {
    add_edge(arena, src, dst, Consume::Line)
}

// .
fn transform_any<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
) -> &'a NodeCell<'a> {
    add_edge(arena, src, dst, Consume::Chars(1))
}

fn transform_concat<'a>(
    arena: &'a Arena,
    mut src: &'a NodeCell<'a>,
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

    while let Some(mut hir) = it.next() {
        if let Some(ch) = check_lowercase_literal(hir) {
            // Transform [aA][bB][cC] into PrefixInsensitive("abc")
            let mut str = ManuallyDrop::new(ArenaString::new_in(arena));
            str.push(ch as char);

            while let Some(next_hir) = it.peek() {
                if let Some(next_ch) = check_lowercase_literal(next_hir) {
                    str.push(next_ch as char);
                    it.next();
                } else {
                    break;
                }
            }

            let next = if it.peek().is_some() { Node::new_in(arena) } else { dst };
            let str: &'a str = unsafe { mem::transmute(str.as_str()) };
            src = add_edge(arena, src, next, Consume::PrefixInsensitive(str));
        } else {
            let next = if it.peek().is_some() { Node::new_in(arena) } else { dst };
            src = transform(arena, src, next, hir);
        }
    }

    src
}

fn transform_alt<'a>(
    arena: &'a Arena,
    src: &'a NodeCell<'a>,
    dst: &'a NodeCell<'a>,
    hirs: &[Hir],
) -> &'a NodeCell<'a> {
    let mut actual_dst = None;

    for hir in hirs {
        let node = transform(arena, src, dst, hir);
        if !ptr::eq(node, *actual_dst.get_or_insert(node)) {
            panic!("Diverging destinations for alternation transformer: {hirs:?}");
        }
    }

    actual_dst.unwrap_or(dst)
}

fn class_to_charset<'a>(arena: &'a Arena, class: &ClassBytes) -> &'a mut [bool; 256] {
    let mut charset = arena.alloc_uninit().write([false; 256]);

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

fn print_mermaid<'a>(root: &'a NodeCell<'a>) {
    fn node_id<'a, 'v>(
        visited: &'v mut HashMap<*const NodeCell<'a>, (usize, bool)>,
        ptr: &'a NodeCell<'a>,
    ) -> &'v mut (usize, bool) {
        let num = visited.len();
        match visited.entry(ptr as *const _) {
            Entry::Occupied(mut e) => e.into_mut(),
            Entry::Vacant(mut e) => e.insert((num, false)),
        }
    }

    fn walk<'a>(
        node: &'a NodeCell<'a>,
        visited: &mut HashMap<*const NodeCell<'a>, (usize, bool)>,
        out: &mut String,
    ) {
        let node_ptr = node as *const _;
        let src_id = match node_id(visited, node) {
            (num, visited) if !*visited => {
                *visited = true;
                *num
            }
            _ => return, // Already visited
        };

        let node_ref = node.borrow();
        let mut edge = node_ref.edge_first;

        while let Some(edge_cell) = edge {
            let edge_ref = edge_cell.borrow();
            let &mut (dst_id, _) = node_id(visited, edge_ref.dst);
            let label = match &edge_ref.test {
                Consume::Prefix(s) => format!("Prefix({s})"),
                Consume::PrefixInsensitive(s) => format!("PrefixInsensitive({s})"),
                Consume::Charset(c) => format!("Charset({:?})", CharsetFormatter(c)),
                Consume::Chars(n) => format!("Chars({n})"),
                Consume::Line => "Line".to_string(),
            };
            let label = label.replace('"', "&quot;");
            out.push_str(&format!("    {src_id} -->|\"{label}\"| {dst_id}\n"));

            walk(edge_ref.dst, visited, out);

            edge = edge_ref.edge_next;
        }
    }

    let mut out = String::from(
        "%%{init:{'fontFamily':'monospace','flowchart':{'defaultRenderer':'elk'}}}%%\ngraph TD\n",
    );
    let mut visited = HashMap::new();
    walk(root, &mut visited, &mut out);
    println!("{out}");
}

#[allow(dead_code)]
pub fn parse_language_definition(def: &LanguageDefinition) {
    let scratch = scratch_arena(None);
    let root = Node::new_in(&scratch);

    for state in def.states {
        for (pattern, kind, action) in state.rules {
            let hir = regex_syntax::ParserBuilder::new()
                .utf8(false)
                .unicode(false)
                .dot_matches_new_line(true)
                .build()
                .parse(pattern)
                .unwrap();
            transform(&scratch, root, root, &hir);
        }
    }

    print_mermaid(root);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_language_definition() {
        parse_language_definition(&JSON);
    }
}
