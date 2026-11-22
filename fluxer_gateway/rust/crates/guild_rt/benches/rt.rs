// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::hint::black_box;
use std::time::{Duration, Instant};

use criterion::{BatchSize, Criterion};
use guild_rt::{ResourceKind, Sink, drop_later, metrics, start};

const MEMBERS: u64 = 250_000;
const DROP_MAX_NS: f64 = 50_000.0;
const METRICS_MAX_NS: f64 = 1_000.0;

struct Member {
    nick: Option<String>,
    roles: Vec<u64>,
    fragment: Vec<u8>,
}

impl Member {
    fn weight(&self) -> usize {
        self.nick.as_ref().map_or(0, String::len) + self.roles.len() * 8 + self.fragment.len()
    }
}

struct FakeCore {
    by_user: HashMap<u64, Member>,
    order: Vec<u64>,
}

fn core(members: u64) -> FakeCore {
    let mut by_user = HashMap::with_capacity(members as usize);
    let mut order = Vec::with_capacity(members as usize);
    for n in 0..members {
        let user = 1_400_000_000_000_000_000 + n * 7_919;
        by_user.insert(
            user,
            Member {
                nick: (n % 3 == 0).then(|| format!("member {n}")),
                roles: (0..(n % 4))
                    .map(|r| 1_410_000_000_000_000_000 + r)
                    .collect(),
                fragment: vec![b'x'; 48 + (n % 64) as usize],
            },
        );
        order.push(user);
    }
    FakeCore { by_user, order }
}

fn median(mut samples: Vec<f64>) -> f64 {
    samples.sort_by(f64::total_cmp);
    samples.get(samples.len() / 2).copied().unwrap_or(f64::NAN)
}

fn wait_for_empty_queue() {
    let deadline = Instant::now() + Duration::from_secs(30);
    while Instant::now() < deadline {
        let queued = metrics()
            .into_iter()
            .find(|(name, _)| *name == "dropper_queue")
            .map_or(0, |(_, v)| v);
        if queued == 0 {
            return;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

fn report(failed: &mut u32, ok: bool, line: String) {
    if !ok {
        *failed += 1;
    }
    println!("  {} {line}", if ok { "met   " } else { "MISSED" });
}

fn main() {
    assert!(start(), "dropper thread");
    let sink = Sink(ResourceKind::MemberListCore);
    let mut c = Criterion::default().configure_from_args();
    {
        let mut group = c.benchmark_group("rt_drop_250k");
        group.sample_size(10);
        group.bench_function("drop_later", |b| {
            b.iter_batched(
                || core(MEMBERS),
                |core| drop_later(core, ResourceKind::MemberListCore),
                BatchSize::PerIteration,
            )
        });
        group.finish();
    }
    wait_for_empty_queue();
    sink.opened();
    sink.bytes_moved(0, 1 << 20);
    c.bench_function("rt_metrics_read", |b| b.iter(|| black_box(metrics())));
    c.final_summary();

    let mut failed = 0;
    println!("\ntarget check (median against target):");
    let drops = (0..21)
        .map(|_| {
            let value = core(MEMBERS);
            black_box(
                value.order.len() + value.by_user.values().map(Member::weight).sum::<usize>(),
            );
            let start = Instant::now();
            drop_later(value, ResourceKind::MemberListCore);
            let ns = start.elapsed().as_nanos() as f64;
            wait_for_empty_queue();
            ns
        })
        .collect();
    let drop_ns = median(drops);
    report(
        &mut failed,
        drop_ns <= DROP_MAX_NS,
        format!(
            "rt_drop_250k: {:.3} us on the caller, target {:.3} us",
            drop_ns / 1e3,
            DROP_MAX_NS / 1e3
        ),
    );
    let reads = (0..10_001)
        .map(|_| {
            let start = Instant::now();
            black_box(metrics());
            start.elapsed().as_nanos() as f64
        })
        .collect();
    let read_ns = median(reads);
    report(
        &mut failed,
        read_ns <= METRICS_MAX_NS,
        format!(
            "rt_metrics_read: {:.3} us, target {:.3} us",
            read_ns / 1e3,
            METRICS_MAX_NS / 1e3
        ),
    );
    sink.bytes_moved(1 << 20, 0);
    sink.closed();
    println!("\ncriterion targets: {failed} missed");
    if failed > 0 {
        std::process::exit(1);
    }
}
