#![allow(dead_code, unused)]

use std::collections::VecDeque;
use std::{mem, slice};

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
                    //(r#"//.*"#, Comment, Pop(1)),
                    //(r#"/\*.*?\*/"#, Comment, Pop(1)),
                    // Strings
                    //(r#"""#, String, Push("string")),
                    // Numbers (start: minus or digit)
                    (r#"-?\d*(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop(1)),
                    // Booleans/null
                    //(r#"true\b"#, Keyword, Pop(1)),
                    //(r#"false\b"#, Keyword, Pop(1)),
                    //(r#"null\b"#, Keyword, Pop(1)),
                ],
            },
            /*StateDefinition {
                name: "string",
                rules: &[(r#"\\"#, String, Push("string_escape")), (r#"""#, String, Pop(1))],
            },
            StateDefinition { name: "string_escape", rules: &[(r#"[\x00-\xff]"#, String, Pop(1))] },*/
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

impl Default for Node<'_> {
    fn default() -> Self {
        Self { edge_first: None, edge_last: None }
    }
}

impl<'a> Node<'a> {
    fn copy_in(arena: &'a Arena, val: Node<'a>) -> &'a NodeCell<'a> {
        arena.alloc_uninit().write(NodeCell::new(val))
    }

    fn default_in(arena: &'a Arena) -> &'a NodeCell<'a> {
        Self::copy_in(arena, Default::default())
    }
}

type EdgeCell<'a> = SemiRefCell<Edge<'a>>;

struct Edge<'a> {
    edge_next: Option<&'a EdgeCell<'a>>,

    destination: &'a NodeCell<'a>,
    // The condition for leaving the current node along this edge.
    test: Consume<'a>,
    // Some scratch space for building up strings for `test`.
    string: Option<ArenaString<'a>>,
}

impl<'a> Edge<'a> {
    fn copy_in(arena: &'a Arena, val: Edge<'a>) -> &'a EdgeCell<'a> {
        arena.alloc_uninit().write(EdgeCell::new(val))
    }
}

struct GraphBuilder<'a> {
    arena: &'a Arena,
    root: &'a NodeCell<'a>,
    current: Vec<NodeCell<'a>, &'a Arena>,

    repeating_class: bool,
}

impl<'a> GraphBuilder<'a> {
    fn new(arena: &'a Arena, root: &'a NodeCell<'a>) -> Self {
        Self { arena, root, current: root, repeating_class: false }
    }

    fn node_add_child(&mut self, parent: &'a NodeCell<'a>, child: &'a NodeCell<'a>) {
        {
            let mut parent = parent.borrow_mut();
            if let Some(last) = parent.child_last {
                last.borrow_mut().sibling_next = Some(child);
            } else {
                parent.child_first = Some(child);
            }
            parent.child_last = Some(child);
        }

        {
            //let mut child = child.borrow_mut();
            //child.parent = Some(parent);
        }

        self.current = child;
    }

    fn class_to_charset(&self, class: &regex_syntax::hir::ClassBytes) -> &'a mut [bool; 256] {
        use regex_syntax::hir::{ClassBytes, ClassBytesRange};

        let mut charset = self.arena.alloc_uninit().write([false; 256]);

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

    fn node_append_prefix_insensitive(&mut self, ch: u8) {
        let mut current = self.current.borrow_mut();
        let mut string = current.string.get_or_insert_with(|| ArenaString::new_in(self.arena));
        string.push(ch as char);
        current.test = Consume::PrefixInsensitive(unsafe { mem::transmute(string.as_str()) });
    }
}

impl regex_syntax::hir::Visitor for GraphBuilder<'_> {
    type Output = ();
    type Err = ();

    fn finish(self) -> Result<Self::Output, Self::Err> {
        Ok(())
    }

    fn visit_pre(&mut self, hir: &regex_syntax::hir::Hir) -> Result<(), Self::Err> {
        use regex_syntax::hir::{Class, Dot, Hir, HirKind, Literal, Look};

        let scratch = scratch_arena(Some(self.arena));
        let tests = Vec::new_in(&*scratch);

        match hir.kind() {
            HirKind::Literal(Literal(lit)) => {
                tests.push(Consume::Prefix(
                    str::from_utf8(
                        self.arena.alloc_uninit_slice(lit.len()).write_clone_of_slice(lit),
                    )
                    .unwrap(),
                ));
            }
            HirKind::Class(Class::Bytes(class)) => {
                // For cases such as [aA], we can use a prefix insensitive match.
                // And for [aA][bB][cC], we want to optimize this to PrefixInsensitive("abc").
                /*let ranges = class.ranges();
                if ranges.len() == 2
                    && ranges[0].len() == 1
                    && ranges[1].len() == 1
                    && let lower_a = ranges[0].start().to_ascii_lowercase()
                    && let lower_b = ranges[1].start().to_ascii_lowercase()
                    && lower_a == lower_b*/
                let charset = self.class_to_charset(class);

                if self.repeating_class {
                    tests.push(Consume::Charset(charset));
                } else {
                    let parent = self.current;

                    for i in 0..256 {
                        if !charset[i] {
                            continue;
                        }

                        let ch = i as u8;
                        let copy = str::from_utf8(
                            self.arena
                                .alloc_uninit_slice(1)
                                .write_clone_of_slice(slice::from_ref(&ch)),
                        )
                        .unwrap();

                        // NOTE: Uppercase chars have a lower numeric value than lowercase chars.
                        // As such, we need to test for `is_ascii_uppercase`.
                        tests.push(
                            if ch.is_ascii_uppercase()
                                && let upper = ch.to_ascii_lowercase() as usize
                                && charset[upper]
                            {
                                charset[upper] = false;
                                Consume::PrefixInsensitive(copy)
                            } else {
                                Consume::Prefix(copy)
                            },
                        );
                    }
                }
            }
            HirKind::Look(Look::WordAscii) => {}
            HirKind::Repetition(r) => match (r.min, r.max, r.sub.kind()) {
                // .*
                (0, None, _) if *r.sub == Hir::dot(Dot::AnyByte) => {
                    tests.push(Consume::Line);
                }
                // [a-z]* | [a-z]+
                (0 | 1, None, HirKind::Class(Class::Bytes(_))) => self.repeating_class = true,
                // .?
                (0, Some(1), sub) => {}
                _ => panic!("Unsupported HIR: {hir:?}"),
            },
            HirKind::Concat(_) => {} // The visitor will descend into the children.
            _ => panic!("Unsupported HIR: {hir:?}"),
        }

        for test in tests {
            let mut it = self.current.borrow().edge_first;
            while let Some(e) = it {
                let e = e.borrow();
                if e.test == edge.test {
                    self.current = e.destination;
                    return Ok(());
                }
                it = e.edge_next;
            }
        }

        Ok(())
    }

    fn visit_post(&mut self, hir: &regex_syntax::hir::Hir) -> Result<(), Self::Err> {
        use regex_syntax::hir::{Class, Dot, Hir, HirKind, Literal, Look};

        #[allow(clippy::single_match)]
        match hir.kind() {
            HirKind::Repetition(r) => match (r.min, r.max, r.sub.kind()) {
                // .*
                (0, None, _) if *r.sub == Hir::dot(Dot::AnyByte) => {} // Handled above.
                // [a-z]* | [a-z]+
                (0 | 1, None, HirKind::Class(Class::Bytes(class))) => {} // Handled above.
                // .?
                (0, Some(1), sub) => {}
                _ => {} // Handled above.
            },
            _ => {}
        }

        self.repeating_class = false;
        Ok(())
    }

    fn visit_alternation_in(&mut self) -> Result<(), Self::Err> {
        Ok(())
    }

    fn visit_concat_in(&mut self) -> Result<(), Self::Err> {
        Ok(())
    }
}

#[allow(dead_code)]
pub fn parse_language_definition(def: &LanguageDefinition) {
    let scratch = scratch_arena(None);

    for state in def.states {
        let root = Node::default_in(&scratch);

        for (pattern, kind, action) in state.rules {
            let hir = regex_syntax::ParserBuilder::new()
                .utf8(false)
                .unicode(false)
                .dot_matches_new_line(true)
                .build()
                .parse(pattern)
                .unwrap();

            regex_syntax::hir::visit(&hir, GraphBuilder::new(&scratch, root)).unwrap();
        }

        // Print the tree starting at `root`.
        let mut stack = Vec::new();
        {
            let mut edge = root.borrow().edge_first;
            while let Some(e) = edge {
                stack.push((e, 0));
                edge = e.borrow().edge_next;
            }
        }

        while let Some((edge, depth)) = stack.pop() {
            let edge = edge.borrow();
            let node = edge.destination.borrow();
            println!("{}Node: {:?}", "  ".repeat(depth), edge.test);

            // Push children onto the stack in reverse order to maintain order when popping
            let mut children = Vec::new();
            let mut child = node.edge_first;
            while let Some(c) = child {
                children.push(c);
                child = c.borrow().edge_next;
            }
            for c in children.into_iter().rev() {
                stack.push((c, depth + 1));
            }
        }
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
