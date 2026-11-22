// SPDX-License-Identifier: AGPL-3.0-or-later

use std::hint::black_box;
use std::time::Instant;

use criterion::Criterion;
use push_core::job::body;

const USERS: u64 = 10_000;

const TARGET_NS: f64 = 250_000.0;

fn median_ns(runs: usize, mut f: impl FnMut()) -> f64 {
    let mut samples: Vec<f64> = (0..runs)
        .map(|_| {
            let start = Instant::now();
            f();
            start.elapsed().as_nanos() as f64
        })
        .collect();
    samples.sort_by(f64::total_cmp);
    samples.get(samples.len() / 2).copied().unwrap_or(f64::NAN)
}

fn main() {
    let head = br#"{"channel_id":"1400000000000000100","notification":{"title":"x"},"user_ids":"#;
    let tail = br#","v":1}"#;
    let ids: Vec<u64> = (0..USERS).map(|n| 1_400_000_000_000_010_000 + n).collect();
    let mut c = Criterion::default().configure_from_args();
    c.bench_function("push_job_write_10k", |b| {
        b.iter(|| body(black_box(head), black_box(&ids), black_box(tail)))
    });
    c.final_summary();
    let median = median_ns(201, || {
        black_box(body(black_box(head), black_box(&ids), black_box(tail)));
    });
    let ok = median <= TARGET_NS;
    println!("\ntarget check (median):");
    println!(
        "  {} push_job_write_10k: {:.1} us, target {:.0} us",
        if ok { "met   " } else { "MISSED" },
        median / 1e3,
        TARGET_NS / 1e3,
    );
    if !ok {
        std::process::exit(1);
    }
}
