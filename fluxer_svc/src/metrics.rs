// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::metrics::render_counter;
pub use fluxer_common::metrics::{Histogram, now_ms};
use std::fmt::Write;
use std::sync::Arc;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};

const ORDERING: Ordering = Ordering::Relaxed;
pub(crate) type AdditionalMetricsRenderer = Arc<dyn Fn(&mut String) + Send + Sync>;

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

    pub fn render_prometheus(&self, service_name: &str) -> String {
        let mut out = String::new();

        let _ = render_counter(
            &mut out,
            &format!("fluxer_{service_name}_requests_total"),
            &self.requests_total,
        );
        let _ = render_counter(
            &mut out,
            &format!("fluxer_{service_name}_request_errors_total"),
            &self.request_errors_total,
        );
        let _ = render_counter(
            &mut out,
            &format!("fluxer_{service_name}_cache_hits_total"),
            &self.cache_hits_total,
        );
        let _ = render_counter(
            &mut out,
            &format!("fluxer_{service_name}_cache_misses_total"),
            &self.cache_misses_total,
        );
        let _ = render_counter(
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

fn render_gauge(out: &mut String, name: &str, value: f64) {
    let _ = writeln!(out, "# TYPE {name} gauge");
    let _ = writeln!(out, "{name} {value:.3}");
}

fn render_histogram(out: &mut String, name: &str, hist: &Histogram) {
    let _ = writeln!(out, "# TYPE {name} histogram");
    let _ = hist.render_series(out, name, None);
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
