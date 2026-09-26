// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct NSFWMetrics {
    pub(super) nsfw_calls_ok: AtomicU64,
    pub(super) nsfw_calls_failed: AtomicU64,
    pub(super) nsfw_calls_disabled: AtomicU64,
}

impl NSFWMetrics {
    pub(crate) fn new() -> Self {
        Self {
            nsfw_calls_ok: AtomicU64::new(0),
            nsfw_calls_failed: AtomicU64::new(0),
            nsfw_calls_disabled: AtomicU64::new(0),
        }
    }

    pub fn record_success(&self) {
        self.nsfw_calls_ok.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_failure(&self) {
        self.nsfw_calls_failed.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_disabled(&self) {
        self.nsfw_calls_disabled.fetch_add(1, Ordering::Relaxed);
    }
}
