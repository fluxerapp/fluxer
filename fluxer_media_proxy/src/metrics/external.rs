// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct ExternalMetrics {
    pub(super) fetch_failures: AtomicU64,
    pub(super) blocked_url_attempts: AtomicU64,
    pub(super) external_buffer_rejected: AtomicU64,
    pub(super) external_stream_overruns: AtomicU64,
}

impl ExternalMetrics {
    pub(crate) fn new() -> Self {
        Self {
            fetch_failures: AtomicU64::new(0),
            blocked_url_attempts: AtomicU64::new(0),
            external_buffer_rejected: AtomicU64::new(0),
            external_stream_overruns: AtomicU64::new(0),
        }
    }

    pub fn record_fetch_failure(&self) {
        self.fetch_failures.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_blocked_url(&self) {
        self.blocked_url_attempts.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_buffer_rejected(&self) {
        self.external_buffer_rejected
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_stream_overrun(&self) {
        self.external_stream_overruns
            .fetch_add(1, Ordering::Relaxed);
    }
}
