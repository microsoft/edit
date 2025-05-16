use std::hint::black_box;
use std::mem;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use edit::helpers::*;
use edit::simd::MemsetSafe;
use edit::{hash, oklab, simd, unicode};

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

fn bench_simd_memchr2(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd");
    let mut buffer_u8 = [0u8; 2048];

    for &bytes in &[8usize, 32 + 8, 64 + 8, KIBI + 8] {
        group.throughput(Throughput::Bytes(bytes as u64 + 1)).bench_with_input(
            BenchmarkId::new("memchr2", bytes),
            &bytes,
            |b, &size| {
                buffer_u8.fill(b'a');
                buffer_u8[size] = b'\n';
                b.iter(|| simd::memchr2(b'\n', b'\r', black_box(&buffer_u8), 0));
            },
        );
    }
}

fn bench_simd_memset<T: MemsetSafe + Copy + Default>(c: &mut Criterion) {
    let mut group = c.benchmark_group("simd");
    let name = format!("memset<{}>", std::any::type_name::<T>());
    let size = mem::size_of::<T>();
    let mut buf: Vec<T> = vec![Default::default(); 2048 / size];

    for &bytes in &[8usize, 32 + 8, 64 + 8, KIBI + 8] {
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
    bench_hash(c);
    bench_oklab(c);
    bench_simd_memchr2(c);
    bench_simd_memset::<u32>(c);
    bench_simd_memset::<u8>(c);
    bench_unicode(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
