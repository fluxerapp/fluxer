// SPDX-License-Identifier: AGPL-3.0-or-later

use perm_core::{Permissions, Stage};
use proptest::prelude::*;
use proptest::test_runner::{Config, RngSeed, TestRunner};

fn stage() -> impl Strategy<Value = Stage> {
    (any::<u64>(), any::<u64>()).prop_map(|(a, d)| {
        Stage::new(
            Permissions::from_bits_retain(a),
            Permissions::from_bits_retain(d),
        )
    })
}

#[test]
fn folding_equals_sequential_application() {
    let cases = std::env::var("PERM_STAGE_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1_000_000);
    let mut config = Config::with_cases(cases);
    config.rng_seed = RngSeed::Fixed(0x5743_4147_4521);
    let mut runner = TestRunner::new(config);
    let strategy = (
        proptest::collection::vec(stage(), 0..12),
        any::<u64>(),
        0usize..12,
    );
    let result = runner.run(&strategy, |(stages, x, split)| {
        let x = Permissions::from_bits_retain(x);
        let sequential = stages.iter().fold(x, |acc, s| s.apply(acc));
        let folded = Stage::fold(stages.iter().copied());
        prop_assert_eq!(folded.apply(x), sequential);
        let cut = split.min(stages.len());
        let (head, tail) = stages.split_at(cut);
        let joined = Stage::fold(head.iter().copied()).then(Stage::fold(tail.iter().copied()));
        prop_assert_eq!(joined, folded);
        Ok(())
    });
    println!("stage algebra: {cases} stage lists checked");
    if let Err(e) = result {
        panic!("{e}");
    }
}
