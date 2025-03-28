use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use edit::helpers::*;
use edit::ucd::MeasurementConfig;
use std::hint::black_box;

fn bench(c: &mut Criterion) {
    let reference = concat!(
        "In the quiet twilight, dreams unfold, soft whispers of a story untold.\n",
        "月明かりが静かに照らし出し、夢を見る心の奥で詩が静かに囁かれる\n",
        "Stars collide in the early light of hope, echoing the silent call of the night.\n",
        "夜の静寂、希望と孤独が混ざり合うその中で詩が永遠に続く\n",
    );
    let buffer = reference.repeat(10);
    let bytes = buffer.as_bytes();

    let mut group = c.benchmark_group("ucd");
    group.throughput(Throughput::Bytes(bytes.len() as u64));
    group.bench_function("MeasurementConfig::goto_logical", |b| {
        b.iter(|| black_box(MeasurementConfig::new(&bytes).goto_logical(Point::MAX)))
    });
    group.bench_function("MeasurementConfig::goto_logical with word wrap", |b| {
        b.iter(|| {
            black_box(
                MeasurementConfig::new(&bytes)
                    .with_word_wrap_column(50)
                    .goto_logical(Point::MAX),
            )
        })
    });
    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
