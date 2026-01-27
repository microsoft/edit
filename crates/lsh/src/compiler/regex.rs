// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Regex-to-IR compiler using `regex_syntax` as the parser.
//!
//! ## Supported patterns
//!
//! - Literals: `foo`, `bar`
//! - Character classes: `[a-z]`, `[^0-9]`, `.` (any byte), `\w`, `\d`, etc.
//! - Quantifiers: `?`, `+`, `*` (greedy only)
//! - Alternation: `a|b|c`
//! - Concatenation: `abc`
//! - Capture groups: `(...)` (for `yield $1 as color`)
//! - Word boundary: `\b` (ASCII only, via `Look::WordEndHalfAscii`)
//! - End of line: `$`
//!
//! ## Gotchas
//!
//! - IMPORTANT (also the most important TODO):
//!   Alternatives are not properly implemented, because backtracking doesn't exist.
//!   E.g. matching `(a|ab)[0-9]+` will NOT match `ab123` because after matching `a` from the
//!   first alternative, it immediately tries to match `[0-9]+` and fails.
//!
//! - Single-character classes like `[eE]` get expanded into a chain of `Prefix` conditions
//!   rather than using `Charset`. This is because prefix matching advances `off` by exactly
//!   the matched length, while charset matching is greedy (matches as many as possible).
//! - For patterns like `[a-z]+|\w+`, the compiler tries to chain alternatives so that if
//!   the more specific pattern fails partway through, it falls back to the more general one.
//!   This is the `try_append_as_fallback` logic - it's complex and may have edge cases.
//! - If a charset includes `\w` (word characters), the compiler also sets bits for UTF-8
//!   continuation byte starters (0xC2-0xF4). This lets `\w+` consume multibyte characters
//!   even though we operate on bytes. It's not Unicode-correct but works for identifiers.
//! - The `parse()` function modifies `dst_good`/`dst_bad` nodes' `.next` pointers.
//!   Don't pass nodes that are already wired into the IR graph.
//!
//! ## TODO
//!
//! - Delete the entire file.
//! - Rewrite it based on a proper TDFA architecture.

use std::ptr;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind, Look};

use super::*;

const ASCII_WORD_CHARSET: Charset = {
    let mut charset = Charset::no();
    charset.set_range(b'0'..=b'9', true);
    charset.set_range(b'A'..=b'Z', true);
    charset.set_range(b'_'..=b'_', true);
    charset.set_range(b'a'..=b'z', true);
    charset.set_range(0xC2..=0xF4, true);
    charset
};

pub fn parse<'a>(
    compiler: &mut Compiler<'a>,
    pattern: &str,
    dst_good: IRCell<'a>,
    dst_bad: IRCell<'a>,
    capture_groups: &mut Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
) -> Result<IRCell<'a>, String> {
    let hir = match regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .dot_matches_new_line(true)
        .build()
        .parse(pattern)
    {
        Ok(hir) => hir,
        Err(e) => return Err(format!("{e}")),
    };

    let src = transform(compiler, dst_good, &hir, capture_groups)?;

    // Connect all unset .next pointers to dst_bad.
    for node in compiler.visit_nodes_from(src) {
        if !ptr::eq(node, dst_good)
            && !ptr::eq(node, dst_bad)
            && let mut node = node.borrow_mut()
            && node.wants_next()
        {
            node.set_next(dst_bad);
        }
    }

    Ok(src)
}

fn transform<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    hir: &Hir,
    capture_groups: &mut Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
) -> Result<IRCell<'a>, String> {
    fn is_any_class(class: &ClassBytes) -> bool {
        class.ranges() == [ClassBytesRange::new(0, 255)]
    }

    match hir.kind() {
        HirKind::Empty => Ok(dst),
        HirKind::Literal(lit) => transform_literal(compiler, dst, &lit.0),
        HirKind::Class(Class::Bytes(class)) if is_any_class(class) => transform_any(compiler, dst),
        HirKind::Class(Class::Bytes(class)) => transform_class(compiler, dst, class),
        HirKind::Class(Class::Unicode(class)) => {
            transform_class(compiler, dst, &class.to_byte_class().unwrap())
        }
        HirKind::Look(Look::End) => {
            Ok(compiler.alloc_iri(IRI::If { condition: Condition::EndOfLine, then: dst }))
        }
        HirKind::Look(Look::WordEndHalfAscii) => {
            // If the test hits, then we failed the word boundary, so go to dst_bad.
            // We don't have dst_bad here, so allocate a noop node, which later gets
            // picked up with its unset .next pointer, and set to dst_bad automatically.
            let bad = compiler.alloc_noop();
            let c = compiler.intern_charset(&ASCII_WORD_CHARSET);
            Ok(compiler.alloc_ir(IR {
                next: Some(dst),
                instr: IRI::If { condition: Condition::Charset(c), then: bad },
                offset: usize::MAX,
            }))
        }
        HirKind::Repetition(rep) => match (rep.min, rep.max, rep.greedy, rep.sub.kind()) {
            (0, None, true, HirKind::Class(Class::Bytes(class))) => {
                if is_any_class(class) {
                    transform_any_star(compiler, dst)
                } else {
                    let src = transform_class_plus(compiler, dst, class)?;
                    transform_option(src, dst)
                }
            }
            (0, Some(1), true, _) => {
                let src = transform(compiler, dst, &rep.sub, capture_groups)?;
                transform_option(src, dst)
            }
            (1, None, true, HirKind::Literal(lit)) => transform_literal_plus(compiler, dst, lit),
            (1, None, true, HirKind::Class(Class::Bytes(class))) => {
                transform_class_plus(compiler, dst, class)
            }
            _ => panic!("Unsupported HIR: {hir:?}"),
        },
        HirKind::Concat(hirs) => transform_concat(compiler, dst, hirs, capture_groups),
        HirKind::Alternation(hirs) => transform_alt(compiler, dst, hirs, capture_groups),
        HirKind::Capture(capture) => {
            // Save the current input offset before matching the capture group
            let start_vreg = compiler.alloc_vreg();
            let save_start = compiler.alloc_iri(IRI::Mov {
                dst: start_vreg,
                src: compiler.get_reg(Register::InputOffset),
            });

            // Transform the sub-expression
            let end_marker = compiler.alloc_noop();
            save_start.borrow_mut().set_next(transform(
                compiler,
                end_marker,
                &capture.sub,
                capture_groups,
            )?);

            // Save the end offset after matching
            let end_vreg = compiler.alloc_vreg();
            let save_end = compiler.alloc_iri(IRI::Mov {
                dst: end_vreg,
                src: compiler.get_reg(Register::InputOffset),
            });
            end_marker.borrow_mut().set_next(save_end);
            save_end.borrow_mut().set_next(dst);

            // Store the capture group (index is 1-based in regex, 0-based in our vec)
            let vec_index = (capture.index as usize).saturating_sub(1);
            // Ensure the vector is large enough
            while capture_groups.len() <= vec_index {
                let dummy_start = compiler.alloc_vreg();
                let dummy_end = compiler.alloc_vreg();
                capture_groups.push((dummy_start, dummy_end));
            }
            capture_groups[vec_index] = (start_vreg, end_vreg);

            Ok(save_start)
        }
        _ => panic!("Unsupported HIR: {hir:?}"),
    }
}

// string
fn transform_literal<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    lit: &[u8],
) -> Result<IRCell<'a>, String> {
    let s = String::from_utf8(lit.to_vec()).unwrap();
    let s = compiler.intern_string(&s);
    Ok(compiler.alloc_iri(IRI::If { condition: Condition::Prefix(s), then: dst }))
}

// a+
fn transform_literal_plus<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    lit: &regex_syntax::hir::Literal,
) -> Result<IRCell<'a>, String> {
    assert!(lit.0.len() == 1);
    let mut c = Charset::default();
    c.set(lit.0[0], true);
    let c = compiler.intern_charset(&c);
    Ok(compiler.alloc_iri(IRI::If { condition: Condition::Charset(c), then: dst }))
}

// [a-z]+
fn transform_class_plus<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    class: &ClassBytes,
) -> Result<IRCell<'a>, String> {
    let c = class_to_charset(class);
    let c = compiler.intern_charset(&c);
    Ok(compiler.alloc_iri(IRI::If { condition: Condition::Charset(c), then: dst }))
}

// [eE]
fn transform_class<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    class: &ClassBytes,
) -> Result<IRCell<'a>, String> {
    let mut charset = class_to_charset(class);
    let mut first: Option<IRCell<'a>> = None;
    let mut last: Option<IRCell<'a>> = None;

    for ch in 0..=255 {
        if !charset.get(ch) {
            continue;
        }

        if ch >= 128 {
            panic!("Invalid non-ASCII class character {ch}");
        }

        let mut str = String::new();
        str.push(ch as char);

        // Check if our given `class` covers lower- and uppercase of an ASCII letter,
        // and if so, we have to turn that into a `PrefixInsensitive` condition.
        //
        // NOTE: Uppercase letters have a lower numerical value than lowercase letters.
        // As such, we check for is_ascii_uppercase(), turn that into `insensitive == true`,
        // and clear out the (numerically higher) lowercase bit from `class`, so we ignore it.
        // We already handled it with our `PrefixInsensitive` condition after all.
        let lower = ch.to_ascii_lowercase();
        let insensitive = ch.is_ascii_uppercase() && charset.get(lower);

        if insensitive {
            charset.set(lower, false);
            str.make_ascii_lowercase();
        }

        let str = compiler.intern_string(&str);
        let condition =
            if insensitive { Condition::PrefixInsensitive(str) } else { Condition::Prefix(str) };

        let node = compiler.alloc_iri(IRI::If { condition, then: dst });
        if first.is_none() {
            first = Some(node);
        }
        if let Some(last) = &last {
            last.borrow_mut().set_next(node);
        }
        last = Some(node);
    }

    Ok(first.unwrap())
}

// .?
fn transform_option<'a>(src: IRCell<'a>, dst: IRCell<'a>) -> Result<IRCell<'a>, String> {
    let mut n = src.borrow_mut();

    while let Some(next) = n.next {
        n = next.borrow_mut();
    }

    n.set_next(dst);
    Ok(src)
}

// .*
fn transform_any_star<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
) -> Result<IRCell<'a>, String> {
    Ok(compiler.alloc_ir(IR {
        next: Some(dst),
        instr: IRI::MovImm { dst: compiler.get_reg(Register::InputOffset), imm: u32::MAX },
        offset: usize::MAX,
    }))
}

// .
fn transform_any<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>) -> Result<IRCell<'a>, String> {
    Ok(compiler.alloc_ir(IR {
        next: Some(dst),
        instr: IRI::AddImm { dst: compiler.get_reg(Register::InputOffset), imm: 1 },
        offset: usize::MAX,
    }))
}

// (a)(b)
fn transform_concat<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    hirs: &[Hir],
    capture_groups: &mut Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
) -> Result<IRCell<'a>, String> {
    fn is_lowercase_literal(hir: &Hir) -> Option<u8> {
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
    let mut first: Option<IRCell<'a>> = None;
    let mut last: Option<IRCell<'a>> = None;

    while let Some(hir) = it.next() {
        // Transform [aA][bB][cC] into PrefixInsensitive("abc").
        let prefix_insensitive = is_lowercase_literal(hir).map(|ch| {
            let mut str = String::new();
            str.push(ch as char);

            while let Some(next_hir) = it.peek() {
                if let Some(next_ch) = is_lowercase_literal(next_hir) {
                    str.push(next_ch as char);
                    it.next();
                } else {
                    break;
                }
            }

            str.make_ascii_lowercase();
            str
        });

        let dst = compiler.alloc_noop();
        let src = if let Some(str) = prefix_insensitive {
            let str = compiler.intern_string(&str);
            compiler.alloc_iri(IRI::If { condition: Condition::PrefixInsensitive(str), then: dst })
        } else {
            transform(compiler, dst, hir, capture_groups)?
        };
        if first.is_none() {
            first = Some(src);
        }
        if let Some(last) = &last {
            last.borrow_mut().set_next(src);
        }
        last = Some(dst);
    }

    if let Some(last) = &last {
        last.borrow_mut().set_next(dst);
    }

    Ok(first.unwrap())
}

// (a|b)
fn transform_alt<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    hirs: &[Hir],
    capture_groups: &mut Vec<(IRRegCell<'a>, IRRegCell<'a>), &'a Arena>,
) -> Result<IRCell<'a>, String> {
    let mut first: Option<IRCell<'a>> = None;
    let mut last: Option<IRCell<'a>> = None;

    for hir in hirs {
        let node = transform(compiler, dst, hir, capture_groups)?;
        if first.is_none() {
            first = Some(node);
        }
        if let Some(last) = &last {
            // Try to merge this alternative as a fallback of previous alternatives
            // if their charsets are compatible (superset relationship)
            if !try_append_as_fallback(last, node) {
                last.borrow_mut().set_next(node);
            }
        }
        last = Some(node);
    }

    Ok(first.unwrap())
}

// Try to append `fallback` as a fallback to nodes in `tree` where the charset
// of `fallback` is a superset of the charset in `tree`. This allows patterns like
// `[a-z]+foo|\w+bar` to match `foobar` by trying the more specific pattern first,
// and falling back to the more general pattern on failure.
fn try_append_as_fallback<'a>(tree: &IRCell<'a>, fallback: IRCell<'a>) -> bool {
    let fallback_charset = get_initial_charset(fallback);
    if fallback_charset.is_none() {
        return false;
    }

    append_fallback_recursive(tree, fallback, fallback_charset.unwrap())
}

fn append_fallback_recursive<'a>(
    node: &IRCell<'a>,
    fallback: IRCell<'a>,
    fallback_charset: &Charset,
) -> bool {
    let mut node_ref = node.borrow_mut();

    if let IRI::If { condition: Condition::Charset(charset), then } = &node_ref.instr {
        // If the fallback charset is a superset of this node's charset,
        // we can append the fallback to this node's failure path
        if fallback_charset.is_superset_of(charset) {
            // Recursively try to append to the success path first
            let appended_to_then = append_fallback_recursive(then, fallback, fallback_charset);

            // If not appended to success path, append to failure path (next)
            if !appended_to_then {
                if let Some(next) = node_ref.next {
                    // Try to append recursively to the next node
                    if !append_fallback_recursive(&next, fallback, fallback_charset) {
                        // If we couldn't append recursively, this might be the right place
                        // But we already have a next pointer, so we need to be careful
                        // For now, don't override existing next pointers in this branch
                    }
                } else {
                    // This node has no next pointer yet, so we can set it to the fallback
                    node_ref.next = Some(fallback);
                    return true;
                }
            }
            return appended_to_then;
        }
    }

    // Try recursively on the next node
    if let Some(next) = node_ref.next {
        drop(node_ref); // Release the borrow before recursing
        return append_fallback_recursive(&next, fallback, fallback_charset);
    }

    false
}

// Get the initial charset that a regex tree matches
fn get_initial_charset<'a>(node: IRCell<'a>) -> Option<&'a Charset> {
    let node_ref = node.borrow();
    match &node_ref.instr {
        IRI::If { condition: Condition::Charset(charset), .. } => Some(*charset),
        _ => None,
    }
}

// [a-z] -> 256-ary LUT
fn class_to_charset(class: &ClassBytes) -> Charset {
    let mut charset = Charset::default();

    for r in class.iter() {
        charset.set_range(r.start()..=r.end(), true);
    }

    // If the class includes \w, we also set any non-ASCII UTF8 starters.
    // That's not how Unicode works, but it simplifies the implementation.
    if [b'0'..=b'9', b'A'..=b'Z', b'_'..=b'_', b'a'..=b'z']
        .iter()
        .all(|r| charset.covers_range(r.clone()))
    {
        charset.set_range(0xC2..=0xF4, true);
    }

    charset
}
