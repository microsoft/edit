use std::mem;

use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use edit::helpers::*;
use edit::simd::MemsetSafe;
use edit::{simd, ucd};

fn bench_ucd(c: &mut Criterion) {
    let reference = concat!(
        "In the quiet twilight, dreams unfold, soft whispers of a story untold.\n",
        "月明かりが静かに照らし出し、夢を見る心の奥で詩が静かに囁かれる\n",
        "Stars collide in the early light of hope, echoing the silent call of the night.\n",
        "夜の静寂、希望と孤独が混ざり合うその中で詩が永遠に続く\n",
    );
    let buffer = reference.repeat(10);
    let bytes = buffer.as_bytes();

    c.benchmark_group("ucd::MeasurementConfig::goto_logical")
        .throughput(Throughput::Bytes(bytes.len() as u64))
        .bench_function("basic", |b| {
            b.iter(|| ucd::MeasurementConfig::new(&bytes).goto_logical(Point::MAX))
        })
        .bench_function("word_wrap", |b| {
            b.iter(|| {
                ucd::MeasurementConfig::new(black_box(&bytes))
                    .with_word_wrap_column(50)
                    .goto_logical(Point::MAX)
            })
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

fn bench(c: &mut Criterion) {
    bench_ucd(c);
    bench_simd_memchr2(c);
    bench_simd_memset::<u8>(c);
    bench_simd_memset::<u32>(c);
}

criterion_group!(benches, bench);
criterion_main!(benches);
