// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

pub trait MonotonicClock: Send + Sync {
    fn now_ns(&self) -> u64;
}

pub struct SystemMonotonicClock {
    origin: Instant,
    last_observed: AtomicU64,
}

impl SystemMonotonicClock {
    pub fn new() -> Self {
        let me = Self {
            origin: Instant::now(),
            last_observed: AtomicU64::new(0),
        };
        assert_eq!(me.last_observed.load(Ordering::Acquire), 0);
        assert!(me.origin.elapsed().as_nanos() < u64::MAX as u128);
        me
    }
}

impl Default for SystemMonotonicClock {
    fn default() -> Self {
        Self::new()
    }
}

impl MonotonicClock for SystemMonotonicClock {
    fn now_ns(&self) -> u64 {
        let elapsed = self.origin.elapsed();
        let ns = elapsed.as_nanos();
        assert!(ns <= u64::MAX as u128);
        let ns_u64 = ns as u64;
        let prior = self.last_observed.fetch_max(ns_u64, Ordering::AcqRel);
        let observed = ns_u64.max(prior);
        assert!(observed >= prior);
        observed
    }
}
