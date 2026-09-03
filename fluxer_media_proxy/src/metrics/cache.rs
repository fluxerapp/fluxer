// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct CoalescerMetrics {
    pub(super) coalescer_leader: AtomicU64,
    pub(super) coalescer_waiter: AtomicU64,
    pub(super) coalescer_waiter_rejected: AtomicU64,
}

impl CoalescerMetrics {
    pub(crate) fn new() -> Self {
        Self {
            coalescer_leader: AtomicU64::new(0),
            coalescer_waiter: AtomicU64::new(0),
            coalescer_waiter_rejected: AtomicU64::new(0),
        }
    }

    pub fn record_leader(&self) {
        self.coalescer_leader.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_waiter(&self) {
        self.coalescer_waiter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_waiter_rejected(&self) {
        self.coalescer_waiter_rejected
            .fetch_add(1, Ordering::Relaxed);
    }
}

#[derive(Debug)]
pub struct TransformCacheMetrics {
    pub(super) transform_cache_hits: AtomicU64,
    pub(super) transform_cache_misses: AtomicU64,
    pub(super) transform_cache_insert_rejected: AtomicU64,
    pub(super) transform_cache_evictions: AtomicU64,
}

impl TransformCacheMetrics {
    pub(crate) fn new() -> Self {
        Self {
            transform_cache_hits: AtomicU64::new(0),
            transform_cache_misses: AtomicU64::new(0),
            transform_cache_insert_rejected: AtomicU64::new(0),
            transform_cache_evictions: AtomicU64::new(0),
        }
    }

    pub fn record_hit(&self) {
        self.transform_cache_hits.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_miss(&self) {
        self.transform_cache_misses.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_insert_rejected(&self) {
        self.transform_cache_insert_rejected
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_eviction(&self) {
        self.transform_cache_evictions
            .fetch_add(1, Ordering::Relaxed);
    }
}
