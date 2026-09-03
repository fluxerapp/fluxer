// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct RelayMetrics {
    pub(super) relay_upstream_success: AtomicU64,
    pub(super) relay_upstream_retries: AtomicU64,
    pub(super) relay_upstream_failures_retryable: AtomicU64,
    pub(super) relay_upstream_failures_hard: AtomicU64,
}

impl RelayMetrics {
    pub(crate) fn new() -> Self {
        Self {
            relay_upstream_success: AtomicU64::new(0),
            relay_upstream_retries: AtomicU64::new(0),
            relay_upstream_failures_retryable: AtomicU64::new(0),
            relay_upstream_failures_hard: AtomicU64::new(0),
        }
    }

    pub fn record_success(&self) {
        self.relay_upstream_success.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_retry(&self) {
        self.relay_upstream_retries.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_retryable_failure(&self) {
        self.relay_upstream_failures_retryable
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_hard_failure(&self) {
        self.relay_upstream_failures_hard
            .fetch_add(1, Ordering::Relaxed);
    }
}
