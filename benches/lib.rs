use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use edit::helpers::*;
use edit::{simd, ucd};

fn bench(c: &mut Criterion) {
    let mut buffer1 = [0u8; 2048];
    let mut buffer4 = [0u32; 2048];

    let reference = concat!(
        "In the quiet twilight, dreams unfold, soft whispers of a story untold.\n",
        "月明かりが静かに照らし出し、夢を見る心の奥で詩が静かに囁かれる\n",
        "Stars collide in the early light of hope, echoing the silent call of the night.\n",
        "夜の静寂、希望と孤独が混ざり合うその中で詩が永遠に続く\n",
    );
    let buffer = reference.repeat(10);
    let bytes = buffer.as_bytes();

    let mut group = c.benchmark_group("ucd::MeasurementConfig::goto_logical");
    group.throughput(Throughput::Bytes(bytes.len() as u64));
    group.bench_function("basic", |b| {
        b.iter(|| ucd::MeasurementConfig::new(&bytes).goto_logical(Point::MAX))
    });
    group.bench_function("word_wrap", |b| {
        b.iter(|| {
            ucd::MeasurementConfig::new(&bytes).with_word_wrap_column(50).goto_logical(Point::MAX)
        })
    });
    group.finish();

    let mut group = c.benchmark_group("simd::memchr2");
    for &size in &[8usize, 40, 72, 1032] {
        group.throughput(Throughput::Bytes(size as u64 + 1));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            buffer1.fill(b'a');
            buffer1[size] = b'\n';
            b.iter(|| simd::memchr2(b'\n', b'\r', &buffer1, 0));
        });
    }
    group.finish();

    let mut group = c.benchmark_group("simd::memset::<u8>");
    for &size in &[8usize, 40, 72, 1032] {
        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| simd::memset(&mut buffer1[..size], 0));
        });
    }
    group.finish();

    let mut group = c.benchmark_group("simd::memset::<u32>");
    for &size in &[8usize, 40, 72, 1032] {
        group.throughput(Throughput::Bytes(size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| simd::memset(&mut buffer4[..size / 4], 0));
        });
    }
    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
