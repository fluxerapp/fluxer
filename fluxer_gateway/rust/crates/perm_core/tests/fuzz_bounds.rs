// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::collections::BTreeSet;
use std::path::Path;

use support::{
    apply_step, check_effects, check_view_over, check_viewsets_audit, check_viewsets_over,
    maybe_compact, sweep,
};

const WORK: u64 = 40_000;
const SWEEP: usize = 256;

fn seed(target: &str, name: &str) -> Vec<u8> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("seeds")
        .join(target)
        .join(name);
    std::fs::read(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

#[test]
fn an_unbounded_sweep_keeps_every_index() {
    let keys: Vec<u32> = (0..5000).map(|i| i % 7).collect();
    for salt in [0, 1, 99] {
        assert_eq!(
            sweep(&keys, usize::MAX, salt),
            (0..keys.len()).collect::<Vec<_>>()
        );
    }
}

#[test]
fn a_bounded_sweep_keeps_the_first_index_of_every_key() {
    let keys: Vec<u32> = (0..5000).map(|i| (i * 31) % 97).collect();
    for salt in [0, 1, 7, 1000] {
        let picked = sweep(&keys, SWEEP, salt);
        let firsts: BTreeSet<usize> = (0..97u32)
            .filter_map(|k| keys.iter().position(|x| *x == k))
            .collect();
        assert!(firsts.iter().all(|i| picked.contains(i)));
        assert!(picked.len() <= firsts.len() + SWEEP + 1);
        assert!(picked.windows(2).all(|w| w[0] < w[1]));
    }
}

#[test]
fn bounded_sweeps_rotate_with_the_salt() {
    let keys = vec![(); 4096];
    let a: BTreeSet<usize> = sweep(&keys, SWEEP, 0).into_iter().collect();
    let b: BTreeSet<usize> = sweep(&keys, SWEEP, 1).into_iter().collect();
    assert_ne!(a, b);
    assert!(a.contains(&0) && b.contains(&0));
}

#[test]
fn viewsets_regression_seeds_pass_the_bounded_checks() {
    for name in [
        "regression-oom-38137521d4d3",
        "regression-slow-unit-c070c4199c1d",
    ] {
        let case = support::bytes::decode(&seed("viewsets", name)).bounded(WORK);
        let mut driver = case.driver();
        check_viewsets_over(&driver, SWEEP);
        check_viewsets_audit(&driver);
        for ops in &case.steps {
            let (_, effects) = apply_step(&mut driver, ops);
            check_viewsets_over(&driver, SWEEP);
            check_viewsets_audit(&driver);
            if maybe_compact(&mut driver, &effects).is_some() {
                check_viewsets_over(&driver, SWEEP);
            }
        }
    }
}

#[test]
fn events_regression_seeds_pass_the_bounded_checks() {
    for name in [
        "regression-slow-unit-71430ba60e31",
        "regression-slow-unit-da47ebf9484c",
    ] {
        let case = support::bytes::decode(&seed("events", name)).bounded(WORK);
        let mut driver = case.driver();
        check_view_over(&driver, WORK, SWEEP);
        for ops in &case.steps {
            let (before, effects) = apply_step(&mut driver, ops);
            check_effects(&driver, &before, &effects);
            check_view_over(&driver, WORK, SWEEP);
            if maybe_compact(&mut driver, &effects).is_some() {
                check_view_over(&driver, WORK, SWEEP);
            }
        }
    }
}
