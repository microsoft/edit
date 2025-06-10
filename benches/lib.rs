// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::hint::black_box;
use std::io::Cursor;
use std::{mem, vec};

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use edit::helpers::*;
use edit::simd::MemsetSafe;
use edit::{arena, buffer, hash, oklab, simd, unicode};
use serde::Deserialize;

#[derive(Deserialize)]
pub struct EditingTracePatch(pub usize, pub usize, pub String);

#[derive(Deserialize)]
pub struct EditingTraceTransaction {
    pub patches: Vec<EditingTracePatch>,
}

#[derive(Deserialize)]
pub struct EditingTraceData {
    #[serde(rename = "startContent")]
    pub start_content: String,
    #[serde(rename = "endContent")]
    pub end_content: String,
    pub txns: Vec<EditingTraceTransaction>,
}

fn bench_buffer(c: &mut Criterion) {
    let data = include_bytes!("../assets/editing-traces/rustcode.json.zst");
    let data = zstd::decode_all(Cursor::new(data)).unwrap();
    let data: EditingTraceData = serde_json::from_slice(&data).unwrap();
    let mut patches_with_coords = Vec::new();

    {
        let mut tb = buffer::TextBuffer::new(false).unwrap();
        tb.set_crlf(false);
        tb.write_raw(data.start_content.as_bytes());

        for t in &data.txns {
            for p in &t.patches {
                tb.cursor_move_to_offset(p.0);
                let beg = tb.cursor_logical_pos();

                tb.delete(buffer::CursorMovement::Grapheme, p.1 as CoordType);

                tb.write_raw(p.2.as_bytes());
                patches_with_coords.push((beg, p.1 as CoordType, p.2.clone()));
            }
        }

        let mut actual = String::new();
        tb.save_as_string(&mut actual);
        assert_eq!(actual, data.end_content);
    }

    let bench_gap_buffer = || {
        let mut buf = buffer::GapBuffer::new(false).unwrap();
        buf.replace(0..usize::MAX, data.start_content.as_bytes());

        for t in &data.txns {
            for p in &t.patches {
                buf.replace(p.0..p.0 + p.1, p.2.as_bytes());
            }
        }

        buf
    };

    let bench_text_buffer = || {
        let mut tb = buffer::TextBuffer::new(false).unwrap();
        tb.set_crlf(false);
        tb.write_raw(data.start_content.as_bytes());

        for p in &patches_with_coords {
            tb.cursor_move_to_logical(p.0);
            tb.delete(buffer::CursorMovement::Grapheme, p.1);
            tb.write_raw(p.2.as_bytes());
        }

        tb
    };

    // Sanity check: If this fails, the implementation is incorrect.
    {
        let buf = bench_gap_buffer();
        let mut actual = Vec::new();
        buf.extract_raw(0..usize::MAX, &mut actual, 0);
        assert_eq!(actual, data.end_content.as_bytes());
    }
    {
        let mut tb = bench_text_buffer();
        let mut actual = String::new();
        tb.save_as_string(&mut actual);
        assert_eq!(actual, data.end_content);
    }

    c.benchmark_group("buffer")
        .bench_function(BenchmarkId::new("GapBuffer", "rustcode"), |b| {
            b.iter(bench_gap_buffer);
        })
        .bench_function(BenchmarkId::new("TextBuffer", "rustcode"), |b| {
            b.iter(bench_text_buffer);
        });
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

fn bench_oklab(c: &mut Criterion) {
    c.benchmark_group("oklab")
        .bench_function("srgb_to_oklab", |b| b.iter(|| oklab::srgb_to_oklab(black_box(0xff212cbe))))
        .bench_function("oklab_blend", |b| {
            b.iter(|| oklab::oklab_blend(black_box(0x7f212cbe), black_box(0x7f3aae3f)))
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

fn bench_simd_memset<T: MemsetSafe + Copy + Default>(c: &mut Criterion) {
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
                b.iter(|| simd::memset(black_box(slice), Default::default()));
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
            b.iter(|| {
                unicode::Utf8Chars::new(bytes, 0).fold(0u32, |acc, ch| acc.wrapping_add(ch as u32))
            })
        });
}

fn bench(c: &mut Criterion) {
    arena::init(128 * MEBI).unwrap();

    bench_buffer(c);
    bench_hash(c);
    bench_oklab(c);
    bench_simd_lines_fwd(c);
    bench_simd_memchr2(c);
    bench_simd_memset::<u32>(c);
    bench_simd_memset::<u8>(c);
    bench_unicode(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
