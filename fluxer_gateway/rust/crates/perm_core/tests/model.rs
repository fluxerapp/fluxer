// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::atomic::{AtomicU64, Ordering};

use proptest::test_runner::TestRunner;
use support::{
    Driver, apply_step, cases, check_effects, check_view, check_viewsets, maybe_compact, params,
    rng_seed, runner_config, steps,
};

#[test]
fn model_matches_reference() {
    let clean = AtomicU64::new(0);
    let events = AtomicU64::new(0);
    let compactions = AtomicU64::new(0);
    let slot_compactions = AtomicU64::new(0);
    let dirty = AtomicU64::new(0);
    let mut runner = TestRunner::new(runner_config(cases(256)));
    let result = runner.run(&(params(), steps()), |(params, steps)| {
        let mut driver = Driver::new(&params);
        check_view(&driver);
        check_viewsets(&driver);
        for ops in &steps {
            let (before, effects) = apply_step(&mut driver, ops);
            check_effects(&driver, &before, &effects);
            check_view(&driver);
            check_viewsets(&driver);
            if maybe_compact(&mut driver, &effects).is_some() {
                check_view(&driver);
                check_viewsets(&driver);
            }
            events.fetch_add(ops.len() as u64, Ordering::Relaxed);
        }
        clean.fetch_add(1, Ordering::Relaxed);
        compactions.fetch_add(driver.compactions, Ordering::Relaxed);
        slot_compactions.fetch_add(driver.slot_compactions, Ordering::Relaxed);
        dirty.fetch_add(driver.dirty, Ordering::Relaxed);
        Ok(())
    });
    println!(
        "model: {} cases clean (seed {}), {} ops, {} compactions ({} of channel slots), {} dirty retries",
        clean.load(Ordering::Relaxed),
        rng_seed(),
        events.load(Ordering::Relaxed),
        compactions.load(Ordering::Relaxed),
        slot_compactions.load(Ordering::Relaxed),
        dirty.load(Ordering::Relaxed)
    );
    if let Err(e) = result {
        panic!("{e}");
    }
}
