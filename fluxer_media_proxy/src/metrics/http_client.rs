// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct HTTPClientMetrics {
    pub(super) http_retries: AtomicU64,
    pub(super) http_retries_exhausted: AtomicU64,
    pub(super) http_retryable_status: AtomicU64,
    pub(super) http_retryable_error: AtomicU64,
}

impl HTTPClientMetrics {
    pub(crate) fn new() -> Self {
        Self {
            http_retries: AtomicU64::new(0),
            http_retries_exhausted: AtomicU64::new(0),
            http_retryable_status: AtomicU64::new(0),
            http_retryable_error: AtomicU64::new(0),
        }
    }

    pub fn record_retry(&self) {
        self.http_retries.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_retries_exhausted(&self) {
        self.http_retries_exhausted.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_retryable_status(&self) {
        self.http_retryable_status.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_retryable_error(&self) {
        self.http_retryable_error.fetch_add(1, Ordering::Relaxed);
    }
}
