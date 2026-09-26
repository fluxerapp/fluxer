// SPDX-License-Identifier: AGPL-3.0-or-later

use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use fluxer_screen_frame_bus::{CpuStagingBackend, StagingSurfacePair};

const BENCH_WIDTH: usize = 1920;
const BENCH_HEIGHT: usize = 1080;
const BENCH_BYTES_PER_PIXEL: usize = 4;
const BENCH_FRAME_LEN: usize = BENCH_WIDTH * BENCH_HEIGHT * BENCH_BYTES_PER_PIXEL;

fn build_pair() -> StagingSurfacePair<CpuStagingBackend> {
    let backends = [
        CpuStagingBackend::new(BENCH_FRAME_LEN),
        CpuStagingBackend::new(BENCH_FRAME_LEN),
    ];
    assert_eq!(backends[0].len(), BENCH_FRAME_LEN);
    assert_eq!(backends[1].len(), BENCH_FRAME_LEN);
    StagingSurfacePair::new(backends)
}

fn submit_map_cycle(c: &mut Criterion) {
    let mut pair = build_pair();
    pair.submit(0, |buf| fill_pattern(buf, 0))
        .expect("warm submit zero");

    let mut sequence: u64 = 1;
    c.bench_function("staging_pair_submit_map_cycle_1080p_rgba", |b| {
        b.iter(|| {
            pair.submit(sequence, |buf| fill_pattern(buf, sequence as u8))
                .expect("submit ok in steady state");
            let mapped_sequence = sequence - 1;
            let first_byte = pair
                .try_map(mapped_sequence, |buf| buf[0])
                .expect("cpu backend is always ready");
            black_box(first_byte);
            sequence = sequence.wrapping_add(1);
        });
    });
}

fn fill_pattern(buf: &mut [u8], seed: u8) {
    assert_eq!(buf.len(), BENCH_FRAME_LEN, "bench backend has fixed size");
    let chunk = seed.wrapping_add(1);
    for byte in buf.iter_mut() {
        *byte = chunk;
    }
}

criterion_group!(benches, submit_map_cycle);
criterion_main!(benches);
