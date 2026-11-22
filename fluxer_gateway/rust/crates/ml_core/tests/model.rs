// SPDX-License-Identifier: AGPL-3.0-or-later

mod strategy;
mod support;

use std::sync::atomic::{AtomicU64, Ordering};

use proptest::test_runner::{Config as PtConfig, RngSeed, TestCaseError, TestRunner};
use strategy::{op, setup};
use support::{Harness, Op, Setup};

static WIDE_SYNCS: AtomicU64 = AtomicU64::new(0);
static QUARANTINES: AtomicU64 = AtomicU64::new(0);
static FILLS: AtomicU64 = AtomicU64::new(0);
static ENCODED: AtomicU64 = AtomicU64::new(0);
static PARTIALS: AtomicU64 = AtomicU64::new(0);

fn run(setup: Setup, ops: Vec<Op>) -> Result<(), TestCaseError> {
    let mut harness = Harness::new(&setup).map_err(TestCaseError::fail)?;
    harness.compare("load").map_err(TestCaseError::fail)?;
    for (i, op) in ops.iter().enumerate() {
        harness
            .step(op)
            .and_then(|_| harness.compare(&format!("op {i} {op:?}")))
            .map_err(TestCaseError::fail)?;
    }
    if harness.widest_sync > ml_core::ranges::MAX_RANGES {
        WIDE_SYNCS.fetch_add(1, Ordering::Relaxed);
    }
    if harness.most_quarantined > 0 {
        QUARANTINES.fetch_add(1, Ordering::Relaxed);
    }
    FILLS.fetch_add(harness.fills, Ordering::Relaxed);
    ENCODED.fetch_add(harness.encoded_fills, Ordering::Relaxed);
    PARTIALS.fetch_add(harness.partials, Ordering::Relaxed);
    Ok(())
}

#[test]
fn core_matches_reference_model() {
    let cases: u32 = std::env::var("PROPTEST_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(2_000);
    let seed: u64 = std::env::var("PROPTEST_RNG_SEED")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1);
    let config = PtConfig {
        cases,
        failure_persistence: None,
        rng_seed: RngSeed::Fixed(seed),
        max_shrink_iters: 20_000,
        ..PtConfig::default()
    };
    let mut runner = TestRunner::new(config);
    let strategy = (setup(), proptest::collection::vec(op(), 1..300));
    let result = runner.run(&strategy, |(setup, ops)| run(setup, ops));
    if let Err(e) = result {
        panic!("{e}");
    }
    let wide = WIDE_SYNCS.load(Ordering::Relaxed);
    let quarantines = QUARANTINES.load(Ordering::Relaxed);
    let fills = FILLS.load(Ordering::Relaxed);
    let encoded = ENCODED.load(Ordering::Relaxed);
    let partials = PARTIALS.load(Ordering::Relaxed);
    println!(
        "model cases passed: {cases} (seed {seed}), {wide} with a sync above {} ranges, {quarantines} with a quarantined member, {fills} item fills, {encoded} encoded fills, {partials} partial renders",
        ml_core::ranges::MAX_RANGES
    );
    if cases >= 32 {
        assert!(
            wide > 0,
            "no case synced more than {} ranges",
            ml_core::ranges::MAX_RANGES
        );
        assert!(quarantines > 0, "no case quarantined a member");
        assert!(
            fills > 0 && encoded > 0,
            "no render asked for items or encoded items"
        );
        assert!(partials > 0, "no render stopped at the budget");
    }
}
