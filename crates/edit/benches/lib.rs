// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::hint::black_box;
use std::io::Cursor;
use std::path::Path;
use std::{mem, vec};

use ::lsh::glob;
use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use edit::arena::{self, scratch_arena};
use edit::collections::BVec;
use edit::float::parse_f64_approx;
use edit::helpers::*;
use edit::unicode::Utf8Chars;
use edit::{buffer, hash, json, lsh, oklab, simd, unicode};

struct EditingTracePatch<'a>(usize, usize, &'a str);

struct EditingTraceTransaction<'a> {
    patches: BVec<'a, EditingTracePatch<'a>>,
}

struct EditingTraceData<'a> {
    start_content: &'a str,
    end_content: &'a str,
    txns: BVec<'a, EditingTraceTransaction<'a>>,
}

fn bench_buffer(c: &mut Criterion) {
    let scratch = scratch_arena(None);
    let data = {
        let data = include_bytes!("../../../assets/editing-traces/rustcode.json.zst");
        let data = zstd::decode_all(Cursor::new(data)).unwrap();
        let data = str::from_utf8(&data).unwrap();

        let data = json::parse(&scratch, data).unwrap();
        let root = data.as_object().unwrap();
        let txns = root.get_array("txns").unwrap();

        let mut res = EditingTraceData {
            start_content: root.get_str("startContent").unwrap(),
            end_content: root.get_str("endContent").unwrap(),
            txns: BVec::empty(),
        };
        res.txns.reserve(&*scratch, txns.len());

        for txn in txns {
            let txn = txn.as_object().unwrap();
            let patches = txn.get_array("patches").unwrap();
            let mut txn = EditingTraceTransaction { patches: BVec::empty() };
            txn.patches.reserve(&*scratch, patches.len());

            for patch in patches {
                let patch = patch.as_array().unwrap();
                let offset = patch[0].as_number().unwrap() as usize;
                let del_len = patch[1].as_number().unwrap() as usize;
                let ins_str = patch[2].as_str().unwrap();
                txn.patches.push(&*scratch, EditingTracePatch(offset, del_len, ins_str));
            }

            res.txns.push(&*scratch, txn);
        }

        res
    };

    enum Op<'a> {
        Move { pos: Point },
        Replace { len: usize, text: &'a str },
        Insert { text: &'a str },
        Delete { len: usize },
    }

    let mut ops = Vec::new();
    {
        let mut tb = buffer::TextBuffer::new(false).unwrap();
        tb.set_crlf(false);
        tb.write_raw(data.start_content.as_bytes());

        for t in &data.txns {
            for p in &t.patches {
                if p.0 != tb.cursor_offset() {
                    tb.cursor_move_to_offset(p.0);
                    ops.push(Op::Move { pos: tb.cursor_logical_pos() });
                }

                if p.1 > 0 && !p.2.is_empty() {
                    tb.selection_update_delta(buffer::CursorMovement::Grapheme, p.1 as CoordType);
                    tb.write_raw(p.2.as_bytes());
                    ops.push(Op::Replace { len: p.1, text: p.2 });
                } else if p.1 > 0 {
                    tb.delete(buffer::CursorMovement::Grapheme, p.1 as CoordType);
                    ops.push(Op::Delete { len: p.1 });
                } else {
                    assert!(!p.2.is_empty());
                    tb.write_raw(p.2.as_bytes());
                    ops.push(Op::Insert { text: p.2 });
                }
            }
        }
    }

    fn bench<T: buffer::TextBufferStorage>(
        start_content: &[u8],
        ops: &[Op],
    ) -> buffer::TextBuffer<T> {
        let mut tb = buffer::TextBuffer::<T>::new_with_storage(false).unwrap();
        tb.set_crlf(false);
        tb.write_raw(start_content);

        for op in ops {
            match op {
                Op::Move { pos } => {
                    tb.cursor_move_to_logical(*pos);
                }
                Op::Replace { len, text } => {
                    tb.selection_update_delta(buffer::CursorMovement::Grapheme, *len as CoordType);
                    tb.write_raw(text.as_bytes());
                }
                Op::Insert { text } => {
                    tb.write_raw(text.as_bytes());
                }
                Op::Delete { len } => {
                    tb.delete(buffer::CursorMovement::Grapheme, *len as CoordType);
                }
            }
        }

        tb
    }

    let bench_zipper = || bench::<buffer::ZipperStorage>(data.start_content.as_bytes(), &ops);
    let bench_piece_list =
        || bench::<buffer::PieceListStorage>(data.start_content.as_bytes(), &ops);
    let bench_piece_tree =
        || bench::<buffer::PieceTreeStorage>(data.start_content.as_bytes(), &ops);
    let bench_piece_tree_avl =
        || bench::<buffer::PieceTreeAvlStorage>(data.start_content.as_bytes(), &ops);

    // Sanity check: If this fails, the implementation is incorrect.
    {
        let mut buf = bench_piece_list();
        let mut actual = String::new();
        buf.save_as_string(&mut actual);
        println!("PieceList: {}", buf.storage().committed());
        assert_eq!(actual, data.end_content);
    }
    {
        let mut buf = bench_zipper();
        let mut actual = String::new();
        buf.save_as_string(&mut actual);
        println!("Zipper: {}", buf.storage().committed());
        assert_eq!(actual, data.end_content);
    }
    {
        let mut buf = bench_piece_tree();
        let mut actual = String::new();
        buf.save_as_string(&mut actual);
        println!("PieceTree: {}", buf.storage().committed());
        assert_eq!(actual, data.end_content);
    }
    {
        let mut buf = bench_piece_tree_avl();
        let mut actual = String::new();
        buf.save_as_string(&mut actual);
        println!("PieceTreeAvl: {}", buf.storage().committed());
        assert_eq!(actual, data.end_content);
    }

    c.benchmark_group("buffer")
        .bench_function(BenchmarkId::new("PieceList", "rustcode"), |b| {
            b.iter(bench_piece_list);
        })
        .bench_function(BenchmarkId::new("Zipper", "rustcode"), |b| {
            b.iter(bench_zipper);
        })
        .bench_function(BenchmarkId::new("PieceTree", "rustcode"), |b| {
            b.iter(bench_piece_tree);
        })
        .bench_function(BenchmarkId::new("PieceTreeAvl", "rustcode"), |b| {
            b.iter(bench_piece_tree_avl);
        });
}

fn bench_float(c: &mut Criterion) {
    c.benchmark_group("float::parse_f64_approx")
        .bench_function("123", |b| b.iter(|| parse_f64_approx(black_box(b"123"))))
        .bench_function("123.456", |b| b.iter(|| parse_f64_approx(black_box(b"123.456"))))
        .bench_function("123.456e3", |b| b.iter(|| parse_f64_approx(black_box(b"123.456e3"))));
}

fn bench_glob(c: &mut Criterion) {
    // Same benchmark as in glob-match
    const PATH: &str = "foo/bar/foo/bar/foo/bar/foo/bar/foo/bar.txt";
    const GLOB: &str = "foo/**/bar.txt";

    c.benchmark_group("glob")
        .bench_function("glob_match", |b| b.iter(|| assert!(glob::glob_match(GLOB, PATH))));
}

fn bench_hash(c: &mut Criterion) {
    c.benchmark_group("hash")
        .throughput(Throughput::Bytes(8))
        .bench_function(BenchmarkId::new("hash", 8), |b| {
            let data = [0u8; 8];
            b.iter(|| hash::hash(0, black_box(&data)))
        })
        .throughput(Throughput::Bytes(16))
        .bench_function(BenchmarkId::new("hash", 16), |b| {
            let data = [0u8; 16];
            b.iter(|| hash::hash(0, black_box(&data)))
        })
        .throughput(Throughput::Bytes(1024))
        .bench_function(BenchmarkId::new("hash", 1024), |b| {
            let data = [0u8; 1024];
            b.iter(|| hash::hash(0, black_box(&data)))
        });
}

fn bench_json(c: &mut Criterion) {
    let str = include_str!("../../../assets/highlighting-tests/json.json");

    c.benchmark_group("json").throughput(Throughput::Bytes(str.len() as u64)).bench_function(
        "parse",
        |b| {
            b.iter(|| {
                let scratch = scratch_arena(None);
                let obj = json::parse(&scratch, black_box(str)).unwrap();
                black_box(obj);
            })
        },
    );
}

fn bench_lsh(c: &mut Criterion) {
    let bytes = include_bytes!("../../../assets/highlighting-tests/markdown.md");
    let bytes = &bytes[..];
    let lang = lsh::LANGUAGES.iter().find(|lang| lang.id == "markdown").unwrap();
    let highlighter = lsh::Highlighter::new(black_box(&bytes), lang);

    c.benchmark_group("lsh").throughput(Throughput::Bytes(bytes.len() as u64)).bench_function(
        "markdown",
        |b| {
            b.iter(|| {
                let mut h = highlighter.clone();
                loop {
                    let scratch = scratch_arena(None);
                    let res = h.parse_next_line(&scratch);
                    if res.is_empty() {
                        break;
                    }
                }
            })
        },
    );

    c.benchmark_group("lsh").bench_function("process_file_associations", |b| {
        let path = Path::new("/some/long/path/to/file/foo.bar.foo.bar.foo.bar");
        b.iter(|| lsh::process_file_associations(lsh::FILE_ASSOCIATIONS, black_box(path)))
    });
}

fn bench_oklab(c: &mut Criterion) {
    c.benchmark_group("oklab")
        .bench_function("StraightRgba::as_oklab", |b| {
            b.iter(|| black_box(oklab::StraightRgba::from_rgba(0xbe2c21ff)).as_oklab())
        })
        .bench_function("StraightRgba::oklab_blend", |b| {
            b.iter(|| {
                black_box(oklab::StraightRgba::from_rgba(0xbe2c217f))
                    .oklab_blend(black_box(oklab::StraightRgba::from_rgba(0x3fae3a7f)))
            })
        });
}

fn bench_simd_lines_fwd(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd");
    let buf = vec![b'\n'; 128 * MEBI];

    for &lines in &[1, 8, 128, KIBI, 128 * KIBI, 128 * MEBI] {
        group.throughput(Throughput::Bytes(lines as u64)).bench_with_input(
            BenchmarkId::new("lines_fwd", lines),
            &lines,
            |b, &lines| {
                b.iter(|| simd::lines_fwd(black_box(&buf), 0, 0, lines as CoordType));
            },
        );
    }
}

fn bench_simd_memchr2(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd");
    let mut buf = vec![0u8; 128 * MEBI + KIBI];

    // For small sizes we add a small offset of +8,
    // to ensure we also benchmark the non-SIMD tail handling.
    // For large sizes, its relative impact is negligible.
    for &bytes in &[8usize, 128 + 8, KIBI, 128 * KIBI, 128 * MEBI] {
        group.throughput(Throughput::Bytes(bytes as u64 + 1)).bench_with_input(
            BenchmarkId::new("memchr2", bytes),
            &bytes,
            |b, &size| {
                buf.fill(b'a');
                buf[size] = b'\n';
                b.iter(|| simd::memchr2(b'\n', b'\r', black_box(&buf), 0));
            },
        );
    }
}

fn bench_simd_memset<T: Copy + Default>(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd");
    let name = format!("memset<{}>", std::any::type_name::<T>());
    let size = mem::size_of::<T>();
    let mut buf: Vec<T> = vec![Default::default(); 128 * MEBI / size];

    // For small sizes we add a small offset of +8,
    // to ensure we also benchmark the non-SIMD tail handling.
    // For large sizes, its relative impact is negligible.
    for &bytes in &[8usize, 128 + 8, KIBI, 128 * KIBI, 128 * MEBI] {
        group.throughput(Throughput::Bytes(bytes as u64)).bench_with_input(
            BenchmarkId::new(&name, bytes),
            &bytes,
            |b, &bytes| {
                let slice = unsafe { buf.get_unchecked_mut(..bytes / size) };
                b.iter(|| edit::simd::memset(black_box(slice), Default::default()));
            },
        );
    }
}

fn bench_unicode(c: &mut Criterion) {
    let reference = concat!(
        "In the quiet twilight, dreams unfold, soft whispers of a story untold.\n",
        "月明かりが静かに照らし出し、夢を見る心の奥で詩が静かに囁かれる\n",
        "Stars collide in the early light of hope, echoing the silent call of the night.\n",
        "夜の静寂、希望と孤独が混ざり合うその中で詩が永遠に続く\n",
    );
    let buffer = reference.repeat(10);
    let bytes = buffer.as_bytes();

    c.benchmark_group("unicode::MeasurementConfig::goto_logical")
        .throughput(Throughput::Bytes(bytes.len() as u64))
        .bench_function("basic", |b| {
            b.iter(|| unicode::MeasurementConfig::new(&bytes).goto_logical(Point::MAX))
        })
        .bench_function("word_wrap", |b| {
            b.iter(|| {
                unicode::MeasurementConfig::new(black_box(&bytes))
                    .with_word_wrap_column(50)
                    .goto_logical(Point::MAX)
            })
        });

    c.benchmark_group("unicode::Utf8Chars")
        .throughput(Throughput::Bytes(bytes.len() as u64))
        .bench_function("next", |b| {
            b.iter(|| Utf8Chars::new(bytes, 0).fold(0u32, |acc, ch| acc.wrapping_add(ch as u32)))
        });
}

fn bench(c: &mut Criterion) {
    arena::init(128 * MEBI).unwrap();

    bench_buffer(c);
    bench_float(c);
    bench_glob(c);
    bench_hash(c);
    bench_json(c);
    bench_lsh(c);
    bench_oklab(c);
    bench_simd_lines_fwd(c);
    bench_simd_memchr2(c);
    bench_simd_memset::<u32>(c);
    bench_simd_memset::<u8>(c);
    bench_unicode(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
