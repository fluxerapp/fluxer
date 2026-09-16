// SPDX-License-Identifier: AGPL-3.0-or-later

use libc::{CLOCK_MONOTONIC, clock_gettime, timespec};
use std::fmt::Write;
use std::sync::Arc;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};

const ORDERING: Ordering = Ordering::Relaxed;
pub(crate) type AdditionalMetricsRenderer = Arc<dyn Fn(&mut String) + Send + Sync>;

const HISTOGRAM_BUCKETS_MS: &[u64] = &[
    1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000,
];

pub fn now_ms() -> i64 {
    let mut ts = timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    let rc = unsafe { clock_gettime(CLOCK_MONOTONIC, &mut ts) };
    if rc != 0 {
        return 0;
    }
    ts.tv_sec
        .saturating_mul(1_000)
        .saturating_add(ts.tv_nsec / 1_000_000)
}

pub struct Histogram {
    buckets: [AtomicU64; 13],
    inf: AtomicU64,
    sum_ms: AtomicU64,
    count: AtomicU64,
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
        for (i, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            if ms <= upper {
                self.buckets[i].fetch_add(1, ORDERING);
                self.inf.fetch_add(1, ORDERING);
                self.sum_ms.fetch_add(ms, ORDERING);
                self.count.fetch_add(1, ORDERING);
                return;
            }
        }
        self.inf.fetch_add(1, ORDERING);
        self.sum_ms.fetch_add(ms, ORDERING);
        self.count.fetch_add(1, ORDERING);
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ServiceMetrics {
    pub requests_total: AtomicU64,
    pub request_errors_total: AtomicU64,
    pub cache_hits_total: AtomicU64,
    pub cache_misses_total: AtomicU64,
    pub shard_forwards_total: AtomicU64,
    pub request_duration: Histogram,
    start_ms: AtomicI64,
    additional_renderer: Option<AdditionalMetricsRenderer>,
}

impl Default for ServiceMetrics {
    fn default() -> Self {
        Self {
            requests_total: AtomicU64::new(0),
            request_errors_total: AtomicU64::new(0),
            cache_hits_total: AtomicU64::new(0),
            cache_misses_total: AtomicU64::new(0),
            shard_forwards_total: AtomicU64::new(0),
            request_duration: Histogram::new(),
            start_ms: AtomicI64::new(0),
            additional_renderer: None,
        }
    }
}

impl ServiceMetrics {
    pub(crate) fn with_additional_renderer(renderer: AdditionalMetricsRenderer) -> Self {
        Self {
            additional_renderer: Some(renderer),
            ..Self::default()
        }
    }

    pub fn init(&self) {
        self.start_ms.store(now_ms(), ORDERING);
    }

    pub fn record_request(&self) {
        self.requests_total.fetch_add(1, ORDERING);
    }

    pub fn record_request_error(&self) {
        self.request_errors_total.fetch_add(1, ORDERING);
    }

    pub fn record_cache_hit(&self) {
        self.cache_hits_total.fetch_add(1, ORDERING);
    }

    pub fn record_cache_miss(&self) {
        self.cache_misses_total.fetch_add(1, ORDERING);
    }

    pub fn record_shard_forward(&self) {
        self.shard_forwards_total.fetch_add(1, ORDERING);
    }

    pub fn record_request_duration(&self, ms: u64) {
        self.request_duration.observe(ms);
    }

    pub fn snapshot(&self) -> MetricsSnapshot {
        MetricsSnapshot {
            requests_total: self.requests_total.load(ORDERING),
            request_errors_total: self.request_errors_total.load(ORDERING),
            cache_hits_total: self.cache_hits_total.load(ORDERING),
            cache_misses_total: self.cache_misses_total.load(ORDERING),
            shard_forwards_total: self.shard_forwards_total.load(ORDERING),
        }
    }

    pub fn render_prometheus(&self, service_name: &str) -> String {
        let mut out = String::new();

        render_counter(
            &mut out,
            &format!("fluxer_{service_name}_requests_total"),
            &self.requests_total,
        );
        render_counter(
            &mut out,
            &format!("fluxer_{service_name}_request_errors_total"),
            &self.request_errors_total,
        );
        render_counter(
            &mut out,
            &format!("fluxer_{service_name}_cache_hits_total"),
            &self.cache_hits_total,
        );
        render_counter(
            &mut out,
            &format!("fluxer_{service_name}_cache_misses_total"),
            &self.cache_misses_total,
        );
        render_counter(
            &mut out,
            &format!("fluxer_{service_name}_shard_forwards_total"),
            &self.shard_forwards_total,
        );
        render_histogram(
            &mut out,
            &format!("fluxer_{service_name}_request_duration_ms"),
            &self.request_duration,
        );

        let uptime_ms = now_ms() - self.start_ms.load(ORDERING);
        render_gauge(
            &mut out,
            &format!("fluxer_{service_name}_uptime_seconds"),
            uptime_ms as f64 / 1000.0,
        );

        if let Some(renderer) = self.additional_renderer.as_ref() {
            renderer(&mut out);
        }

        out
    }
}

#[derive(Clone, Debug, Default)]
pub struct MetricsSnapshot {
    pub requests_total: u64,
    pub request_errors_total: u64,
    pub cache_hits_total: u64,
    pub cache_misses_total: u64,
    pub shard_forwards_total: u64,
}

fn render_counter(out: &mut String, name: &str, counter: &AtomicU64) {
    let _ = writeln!(out, "# TYPE {name} counter");
    let _ = writeln!(out, "{name} {}", counter.load(ORDERING));
}

fn render_gauge(out: &mut String, name: &str, value: f64) {
    let _ = writeln!(out, "# TYPE {name} gauge");
    let _ = writeln!(out, "{name} {value:.3}");
}

fn render_histogram(out: &mut String, name: &str, hist: &Histogram) {
    let _ = writeln!(out, "# TYPE {name} histogram");
    let mut cumulative = 0;
    for (i, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
        cumulative += hist.buckets[i].load(ORDERING);
        let _ = writeln!(out, "{name}_bucket{{le=\"{upper}\"}} {cumulative}");
    }
    let _ = writeln!(
        out,
        "{name}_bucket{{le=\"+Inf\"}} {}",
        hist.inf.load(ORDERING)
    );
    let _ = writeln!(out, "{name}_sum {}", hist.sum_ms.load(ORDERING));
    let _ = writeln!(out, "{name}_count {}", hist.count.load(ORDERING));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn histogram_observes_into_correct_bucket() {
        let h = Histogram::new();
        h.observe(0);
        h.observe(3);
        h.observe(50);
        h.observe(100_000);
        assert_eq!(1, h.buckets[0].load(ORDERING));
        assert_eq!(1, h.buckets[1].load(ORDERING));
        assert_eq!(1, h.buckets[4].load(ORDERING));
        assert_eq!(4, h.inf.load(ORDERING));
        assert_eq!(4, h.count.load(ORDERING));
    }

    #[test]
    fn render_prometheus_produces_valid_output() {
        let m = ServiceMetrics::default();
        m.init();
        m.record_request();
        m.record_request();
        m.record_request_error();
        m.record_cache_hit();
        m.record_request_duration(42);

        let text = m.render_prometheus("user_svc");
        assert!(text.contains("# TYPE fluxer_user_svc_requests_total counter"));
        assert!(text.contains("fluxer_user_svc_requests_total 2"));
        assert!(text.contains("# TYPE fluxer_user_svc_request_errors_total counter"));
        assert!(text.contains("fluxer_user_svc_request_errors_total 1"));
        assert!(text.contains("# TYPE fluxer_user_svc_cache_hits_total counter"));
        assert!(text.contains("fluxer_user_svc_cache_hits_total 1"));
        assert!(text.contains("# TYPE fluxer_user_svc_request_duration_ms histogram"));
        assert!(text.contains("fluxer_user_svc_request_duration_ms_bucket{le=\"50\"} 1"));
        assert!(text.contains("# TYPE fluxer_user_svc_uptime_seconds gauge"));
    }

    #[test]
    fn snapshot_reflects_recorded_counters() {
        let m = ServiceMetrics::default();
        m.record_request();
        m.record_shard_forward();
        let snap = m.snapshot();
        assert_eq!(snap.requests_total, 1);
        assert_eq!(snap.shard_forwards_total, 1);
    }
}
