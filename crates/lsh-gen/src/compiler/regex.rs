use std::ptr;

use regex_syntax::hir::{Class, ClassBytes, ClassBytesRange, Hir, HirKind};

use super::*;

pub fn parse<'a>(
    compiler: &mut Compiler<'a>,
    pattern: &str,
    dst_good: IRCell<'a>,
    dst_bad: IRCell<'a>,
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

    let src = transform(compiler, dst_good, &hir);

    // Connect all unset .next pointers to dst_bad.
    for node in compiler.visit_nodes_from(src) {
        if !ptr::eq(node, dst_good) && !ptr::eq(node, dst_bad) {
            node.borrow_mut().set_next_if_none(dst_bad);
        }
    }

    Ok(src)
}

fn transform<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>, hir: &Hir) -> IRCell<'a> {
    fn is_any_class(class: &ClassBytes) -> bool {
        class.ranges() == [ClassBytesRange::new(0, 255)]
    }

    match hir.kind() {
        HirKind::Empty => dst,
        HirKind::Literal(lit) => transform_literal(compiler, dst, &lit.0),
        HirKind::Class(Class::Bytes(class)) if is_any_class(class) => transform_any(compiler, dst),
        HirKind::Class(Class::Bytes(class)) => transform_class(compiler, dst, class),
        HirKind::Class(Class::Unicode(class)) => {
            transform_class(compiler, dst, &class.to_byte_class().unwrap())
        }
        HirKind::Repetition(rep) => match (rep.min, rep.max, rep.sub.kind()) {
            (0, None, HirKind::Class(Class::Bytes(class))) if is_any_class(class) => {
                transform_any_star(compiler, dst)
            }
            (0, None, HirKind::Class(Class::Bytes(class))) => {
                let src = transform_class_plus(compiler, dst, class);
                transform_option(src, dst);
                src
            }
            (0, Some(1), _) => {
                let src = transform(compiler, dst, &rep.sub);
                transform_option(src, dst);
                src
            }
            (1, None, HirKind::Class(Class::Bytes(class))) => {
                transform_class_plus(compiler, dst, class)
            }
            _ => panic!("Unsupported HIR: {hir:?}"),
        },
        HirKind::Concat(hirs) if hirs.len() >= 2 => transform_concat(compiler, dst, hirs),
        HirKind::Alternation(hirs) if hirs.len() >= 2 => transform_alt(compiler, dst, hirs),
        _ => panic!("Unsupported HIR: {hir:?}"),
    }
}

// string
fn transform_literal<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>, lit: &[u8]) -> IRCell<'a> {
    let s = String::from_utf8(lit.to_vec()).unwrap();
    let s = compiler.intern_string(&s);
    compiler.alloc_iri(IRI::If { condition: Condition::Prefix(s), then: dst })
}

// [a-z]+
fn transform_class_plus<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    class: &ClassBytes,
) -> IRCell<'a> {
    let c = class_to_charset(class);
    let c = compiler.intern_charset(&c);
    compiler.alloc_iri(IRI::If { condition: Condition::Charset(c), then: dst })
}

// [eE]
fn transform_class<'a>(
    compiler: &mut Compiler<'a>,
    dst: IRCell<'a>,
    class: &ClassBytes,
) -> IRCell<'a> {
    let mut charset = class_to_charset(class);
    let mut first: Option<IRCell<'a>> = None;
    let mut last: Option<IRCell<'a>> = None;

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

    first.unwrap()
}

// .?
fn transform_option<'a>(src: IRCell<'a>, dst: IRCell<'a>) -> IRCell<'a> {
    src.borrow_mut().set_next(dst);
    src
}

// .*
fn transform_any_star<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>) -> IRCell<'a> {
    compiler.alloc_ir(IR {
        next: Some(dst),
        instr: IRI::Add { dst: Register::InputOffset, src: Register::Zero, imm: usize::MAX },
        offset: 0,
    })
}

// .
fn transform_any<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>) -> IRCell<'a> {
    compiler.alloc_ir(IR {
        next: Some(dst),
        instr: IRI::Add { dst: Register::InputOffset, src: Register::InputOffset, imm: 1 },
        offset: 0,
    })
}

// (a)(b)
fn transform_concat<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>, hirs: &[Hir]) -> IRCell<'a> {
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

        let node = if let Some(str) = prefix_insensitive {
            let str = compiler.intern_string(&str);
            compiler.alloc_iri(IRI::If { condition: Condition::PrefixInsensitive(str), then: dst })
        } else {
            transform(compiler, dst, hir)
        };
        if first.is_none() {
            first = Some(node);
        }
        if let Some(last) = &last {
            let mut last = last.borrow_mut();
            match last.instr {
                IRI::Add { .. } => {
                    last.next = Some(node);
                }
                IRI::If { ref mut then, .. } => {
                    *then = node;
                }
                _ => unreachable!(),
            }
        }
        last = Some(node);
    }

    first.unwrap()
}

// (a|b)
fn transform_alt<'a>(compiler: &mut Compiler<'a>, dst: IRCell<'a>, hirs: &[Hir]) -> IRCell<'a> {
    let mut actual_dst = None;

    for hir in hirs {
        // TODO: needs to write into the else branch
        let d = transform(compiler, dst, hir);
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
