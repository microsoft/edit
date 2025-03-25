use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use edit::helpers::*;
use edit::ucd::MeasurementConfig;
use std::hint::black_box;

fn bench(c: &mut Criterion) {
    let reference = "The quick brown fox jumps over the lazy dog\u{1F469}\u{1F3FB}\u{200D}\u{2764}\u{FE0F}\u{200D}\u{1F48B}\u{200D}\u{1F468}\u{1F3FB}\n";
    let buffer = reference.repeat(10);
    let bytes = buffer.as_bytes();

    let mut group = c.benchmark_group("ucd");
    group.throughput(Throughput::Bytes(bytes.len() as u64));
    group.bench_function("MeasurementConfig::goto_logical", |b| {
        b.iter(|| black_box(MeasurementConfig::new(&bytes).goto_logical(Point::MAX)))
    });
    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
