// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt::{self, Write as _};
use std::sync::atomic::{AtomicU64, Ordering};

const HISTOGRAM_BUCKETS_MS: &[u64] = &[
    1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000,
];

pub struct Histogram {
    pub(super) buckets: [AtomicU64; 13],
    pub(super) inf: AtomicU64,
    pub(super) sum_ms: AtomicU64,
    pub(super) count: AtomicU64,
}

impl Histogram {
    pub const fn new() -> Self {
        Self {
            buckets: [const { AtomicU64::new(0) }; 13],
            inf: AtomicU64::new(0),
            sum_ms: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }

    pub fn observe(&self, ms: u64) {
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            if ms <= upper {
                self.buckets[index].fetch_add(1, Ordering::Relaxed);
                break;
            }
        }
        self.inf.fetch_add(1, Ordering::Relaxed);
        self.sum_ms.fetch_add(ms, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);
    }

    pub(super) fn render_series(
        &self,
        out: &mut String,
        name: &str,
        labels: Option<&str>,
    ) -> fmt::Result {
        let prefix = labels
            .map(|labels| format!("{labels},"))
            .unwrap_or_default();
        let suffix = labels
            .map(|labels| format!("{{{labels}}}"))
            .unwrap_or_default();
        let mut cumulative = 0;
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            cumulative += self.buckets[index].load(Ordering::Relaxed);
            writeln!(out, "{name}_bucket{{{prefix}le=\"{upper}\"}} {cumulative}")?;
        }
        writeln!(
            out,
            "{name}_bucket{{{prefix}le=\"+Inf\"}} {}",
            self.inf.load(Ordering::Relaxed)
        )?;
        writeln!(
            out,
            "{name}_sum{suffix} {}",
            self.sum_ms.load(Ordering::Relaxed)
        )?;
        writeln!(
            out,
            "{name}_count{suffix} {}",
            self.count.load(Ordering::Relaxed)
        )
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Self::new()
    }
}
