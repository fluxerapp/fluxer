// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::atomic::{AtomicU64, Ordering};

use proptest::test_runner::TestRunner;
use support::{Driver, cases, params, rng_seed, runner_config, steps, trace};

#[test]
fn invariants_hold_after_every_apply() {
    let clean = AtomicU64::new(0);
    let mut runner = TestRunner::new(runner_config(cases(256)));
    let result = runner.run(&(params(), steps()), |(params, steps)| {
        trace(Driver::new(&params), &steps);
        clean.fetch_add(1, Ordering::Relaxed);
        Ok(())
    });
    println!(
        "invariants: {} cases clean (seed {})",
        clean.load(Ordering::Relaxed),
        rng_seed()
    );
    if let Err(e) = result {
        panic!("{e}");
    }
}

#[test]
fn equal_streams_give_equal_class_ids_and_deltas() {
    let mut runner = TestRunner::new(runner_config((cases(256) / 4).max(64)));
    let result = runner.run(&(params(), steps()), |(params, steps)| {
        assert_eq!(
            trace(Driver::new(&params), &steps),
            trace(Driver::new(&params), &steps)
        );
        Ok(())
    });
    if let Err(e) = result {
        panic!("{e}");
    }
}
