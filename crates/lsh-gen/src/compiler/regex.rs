use std::ptr;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind};

use crate::compiler::*;

pub fn parse<'a>(
    compiler: &mut Compiler<'a>,
    pattern: &str,
    dst_good: NodeCell<'a>,
    dst_bad: NodeCell<'a>,
) -> Result<NodeCell<'a>, String> {
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

    let src = compiler.alloc_noop();
    transform(compiler, src, dst_good, &hir);

    // Connect all unset .next pointers to dst_bad.
    for node in compiler.visit_nodes_from(src) {
        node.borrow_mut().set_next(dst_bad);
    }

    Ok(src)
}

fn transform<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
    hir: &Hir,
) -> NodeCell<'a> {
    fn is_any_class(class: &ClassBytes) -> bool {
        class.ranges() == [ClassBytesRange::new(0, 255)]
    }

    match hir.kind() {
        HirKind::Empty => transform_option(compiler, src, dst),
        HirKind::Literal(lit) => transform_literal(compiler, src, dst, &lit.0),
        HirKind::Class(Class::Bytes(class)) if is_any_class(class) => {
            transform_any(compiler, src, dst)
        }
        HirKind::Class(Class::Bytes(class)) => transform_class(compiler, src, dst, class),
        HirKind::Class(Class::Unicode(class)) => {
            transform_class(compiler, src, dst, &class.to_byte_class().unwrap())
        }
        HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
            (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                transform_any_star(compiler, src, dst)
            }
            (0, None, HirKind::Class(Class::Bytes(class))) => {
                let dst = transform_class_plus(compiler, src, dst, class);
                transform_option(compiler, src, dst);
                dst
            }
            (0, Some(1), _) => {
                let dst = transform(compiler, src, dst, &rep.sub);
                transform_option(compiler, src, dst);
                dst
            }
            (1, None, HirKind::Class(Class::Bytes(class))) => {
                transform_class_plus(compiler, src, dst, class)
            }
            _ => panic!("Unsupported HIR: {hir:?}"),
        },
        HirKind::Concat(hirs) if hirs.len() >= 2 => transform_concat(compiler, src, dst, hirs),
        HirKind::Alternation(hirs) if hirs.len() >= 2 => transform_alt(compiler, src, dst, hirs),
        _ => panic!("Unsupported HIR: {hir:?}"),
    }
}

// string
fn transform_literal<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
    lit: &[u8],
) -> NodeCell<'a> {
    let s = String::from_utf8(lit.to_vec()).unwrap();
    let s = compiler.intern_string(&s);
    add_transition(
        compiler,
        src,
        Node::If { condition: Condition::Prefix(s), then: dst, next: None },
    )
}

// [a-z]+
fn transform_class_plus<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
    class: &ClassBytes,
) -> NodeCell<'a> {
    let c = class_to_charset(class);
    let c = compiler.intern_charset(&c);
    add_transition(
        compiler,
        src,
        Node::If { condition: Condition::Charset(c), then: dst, next: None },
    )
}

// [eE]
fn transform_class<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
    class: &ClassBytes,
) -> NodeCell<'a> {
    let mut charset = class_to_charset(class);
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

        let upper = ch.to_ascii_lowercase() as usize;
        // NOTE: Uppercase chars have a lower numeric value than lowercase chars.
        // As such, we need to test for `is_ascii_uppercase`.
        let insensitive = ch.is_ascii_uppercase() && charset[upper];

        if insensitive {
            charset[upper] = false;
            str.make_ascii_lowercase();
        }

        let str = compiler.intern_string(&str);
        let test =
            if insensitive { Condition::PrefixInsensitive(str) } else { Condition::Prefix(str) };

        let d = add_transition(compiler, src, Node::If { condition: test, then: dst, next: None });
        if !ptr::eq(d, *actual_dst.get_or_insert(d)) {
            // TODO: broken
            panic!("Diverging destinations for class transformer: {class:?}");
        }
    }

    actual_dst.unwrap_or(dst)
}

// .?
fn transform_option<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
) -> NodeCell<'a> {
    match &mut *src.borrow_mut() {
        Node::If { next, .. } => {
            assert!(next.is_none());
            *next = Some(dst);
            dst
        }
        _ => unreachable!(),
    }
}

// .*
fn transform_any_star<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
) -> NodeCell<'a> {
    add_transition(
        compiler,
        src,
        Node::Add {
            dst: Register::InputOffset,
            src: Register::Zero,
            imm: usize::MAX,
            next: Some(dst),
        },
    )
}

// .
fn transform_any<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
) -> NodeCell<'a> {
    add_transition(
        compiler,
        src,
        Node::Add {
            dst: Register::InputOffset,
            src: Register::InputOffset,
            imm: 1,
            next: Some(dst),
        },
    )
}

// (a)(b)
fn transform_concat<'a>(
    compiler: &mut Compiler<'a>,
    mut src: NodeCell<'a>,
    dst: NodeCell<'a>,
    hirs: &[Hir],
) -> NodeCell<'a> {
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

        let more = it.peek().is_some();
        let dst = if more { compiler.alloc_noop() } else { dst };

        if let Some(str) = prefix_insensitive {
            let str = compiler.intern_string(&str);
            src = add_transition(
                compiler,
                src,
                Node::If { condition: Condition::PrefixInsensitive(str), then: dst, next: None },
            );
        } else {
            src = transform(compiler, src, dst, hir);
        }
    }

    src
}

// (a|b)
fn transform_alt<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    dst: NodeCell<'a>,
    hirs: &[Hir],
) -> NodeCell<'a> {
    let mut actual_dst = None;

    for hir in hirs {
        // TODO: needs to write into the else branch
        let d = transform(compiler, src, dst, hir);
        if !ptr::eq(d, *actual_dst.get_or_insert(d)) {
            // TODO: broken
            panic!("Diverging destinations for alternation transformer: {hirs:?}");
        }
    }

    actual_dst.unwrap_or(dst)
}

// [a-z] -> 256-ary LUT
fn class_to_charset(class: &ClassBytes) -> Charset {
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

fn add_transition<'a>(
    compiler: &mut Compiler<'a>,
    src: NodeCell<'a>,
    node: Node<'a>,
) -> NodeCell<'a> {
    let n = compiler.alloc_node(node);
    src.borrow_mut().set_next(n);
    n
}
