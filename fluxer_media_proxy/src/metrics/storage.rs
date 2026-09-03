// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct StorageMetrics {
    pub(super) storage_hits: AtomicU64,
    pub(super) storage_misses: AtomicU64,
    pub(super) storage_errors: AtomicU64,
}

impl StorageMetrics {
    pub(crate) fn new() -> Self {
        Self {
            storage_hits: AtomicU64::new(0),
            storage_misses: AtomicU64::new(0),
            storage_errors: AtomicU64::new(0),
        }
    }

    pub fn record_hit(&self) {
        self.storage_hits.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_miss(&self) {
        self.storage_misses.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_error(&self) {
        self.storage_errors.fetch_add(1, Ordering::Relaxed);
    }
}
