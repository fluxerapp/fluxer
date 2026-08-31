// SPDX-License-Identifier: AGPL-3.0-or-later

use libc::{CLOCK_MONOTONIC, clock_gettime, timespec};
use std::fmt::Write;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};

pub fn init_global() {
    GLOBAL.start_ms.store(now_ms(), Ordering::Relaxed);
}

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RequestKind {
    Health,
    Metadata,
    Thumbnail,
    Frames,
    AssetImage,
    GuildMemberImage,
    Attachment,
    External,
    Static,
    Themes,
    Upload,
    Other,
}

impl RequestKind {
    pub const ALL: [RequestKind; 12] = [
        Self::Health,
        Self::Metadata,
        Self::Thumbnail,
        Self::Frames,
        Self::AssetImage,
        Self::GuildMemberImage,
        Self::Attachment,
        Self::External,
        Self::Static,
        Self::Themes,
        Self::Upload,
        Self::Other,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::Health => "health",
            Self::Metadata => "metadata",
            Self::Thumbnail => "thumbnail",
            Self::Frames => "frames",
            Self::AssetImage => "asset_image",
            Self::GuildMemberImage => "guild_member_image",
            Self::Attachment => "attachment",
            Self::External => "external",
            Self::Static => "static",
            Self::Themes => "themes",
            Self::Upload => "upload",
            Self::Other => "other",
        }
    }
}

const HISTOGRAM_BUCKETS_MS: &[u64] = &[
    1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000,
];

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
                self.buckets[i].fetch_add(1, Ordering::Relaxed);
                self.inf.fetch_add(1, Ordering::Relaxed);
                self.sum_ms.fetch_add(ms, Ordering::Relaxed);
                self.count.fetch_add(1, Ordering::Relaxed);
                return;
            }
        }
        self.inf.fetch_add(1, Ordering::Relaxed);
        self.sum_ms.fetch_add(ms, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Metrics {
    requests_2xx: [AtomicU64; 12],
    requests_3xx: [AtomicU64; 12],
    requests_4xx: [AtomicU64; 12],
    requests_5xx: [AtomicU64; 12],
    pub transform_image_duration: Histogram,
    pub transform_video_duration: Histogram,
    pub native_transform_wait: Histogram,
    pub request_duration: Histogram,
    request_duration_per_kind: [Histogram; 12],
    pub coalescer_leader: AtomicU64,
    pub coalescer_waiter: AtomicU64,
    pub transform_cache_hits: AtomicU64,
    pub transform_cache_misses: AtomicU64,
    pub storage_hits: AtomicU64,
    pub storage_misses: AtomicU64,
    pub storage_errors: AtomicU64,
    pub nsfw_calls_ok: AtomicU64,
    pub nsfw_calls_failed: AtomicU64,
    pub nsfw_calls_disabled: AtomicU64,
    pub transform_failures: AtomicU64,
    pub decode_failures: AtomicU64,
    pub fetch_failures: AtomicU64,
    pub external_stream_overruns: AtomicU64,
    pub blocked_url_attempts: AtomicU64,
    pub framebuffer_pool_borrows: AtomicU64,
    pub framebuffer_pool_grow_events: AtomicU64,
    pub relay_upstream_success: AtomicU64,
    pub relay_upstream_failures_retryable: AtomicU64,
    pub relay_upstream_failures_hard: AtomicU64,
    pub relay_upstream_retries: AtomicU64,
    pub http_retryable_status: AtomicU64,
    pub http_retryable_error: AtomicU64,
    pub http_retries: AtomicU64,
    pub http_retries_exhausted: AtomicU64,
    pub hdr_tone_map_count: AtomicU64,
    pub heif_hdr_gain_map_count: AtomicU64,
    pub avif_libheif_decode_count: AtomicU64,
    pub avif_libheif_decode_failures: AtomicU64,
    start_ms: AtomicI64,
}

impl Metrics {
    pub const fn new() -> Self {
        Self {
            requests_2xx: [const { AtomicU64::new(0) }; 12],
            requests_3xx: [const { AtomicU64::new(0) }; 12],
            requests_4xx: [const { AtomicU64::new(0) }; 12],
            requests_5xx: [const { AtomicU64::new(0) }; 12],
            transform_image_duration: Histogram::new(),
            transform_video_duration: Histogram::new(),
            native_transform_wait: Histogram::new(),
            request_duration: Histogram::new(),
            request_duration_per_kind: [const { Histogram::new() }; 12],
            coalescer_leader: AtomicU64::new(0),
            coalescer_waiter: AtomicU64::new(0),
            transform_cache_hits: AtomicU64::new(0),
            transform_cache_misses: AtomicU64::new(0),
            storage_hits: AtomicU64::new(0),
            storage_misses: AtomicU64::new(0),
            storage_errors: AtomicU64::new(0),
            nsfw_calls_ok: AtomicU64::new(0),
            nsfw_calls_failed: AtomicU64::new(0),
            nsfw_calls_disabled: AtomicU64::new(0),
            transform_failures: AtomicU64::new(0),
            decode_failures: AtomicU64::new(0),
            fetch_failures: AtomicU64::new(0),
            external_stream_overruns: AtomicU64::new(0),
            blocked_url_attempts: AtomicU64::new(0),
            framebuffer_pool_borrows: AtomicU64::new(0),
            framebuffer_pool_grow_events: AtomicU64::new(0),
            relay_upstream_success: AtomicU64::new(0),
            relay_upstream_failures_retryable: AtomicU64::new(0),
            relay_upstream_failures_hard: AtomicU64::new(0),
            relay_upstream_retries: AtomicU64::new(0),
            http_retryable_status: AtomicU64::new(0),
            http_retryable_error: AtomicU64::new(0),
            http_retries: AtomicU64::new(0),
            http_retries_exhausted: AtomicU64::new(0),
            hdr_tone_map_count: AtomicU64::new(0),
            heif_hdr_gain_map_count: AtomicU64::new(0),
            avif_libheif_decode_count: AtomicU64::new(0),
            avif_libheif_decode_failures: AtomicU64::new(0),
            start_ms: AtomicI64::new(0),
        }
    }

    pub fn record_request(&self, kind: RequestKind, status: u16) {
        let bucket = match status / 100 {
            2 => &self.requests_2xx,
            3 => &self.requests_3xx,
            5 => &self.requests_5xx,
            _ => &self.requests_4xx,
        };
        bucket[kind as usize].fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_request_with_duration(&self, kind: RequestKind, status: u16, ms: u64) {
        self.record_request(kind, status);
        self.request_duration.observe(ms);
        self.request_duration_per_kind[kind as usize].observe(ms);
    }
}

impl Default for Metrics {
    fn default() -> Self {
        Self::new()
    }
}

pub static GLOBAL: Metrics = Metrics::new();

pub fn render() -> String {
    let mut out = String::new();
    render_request_series(
        &mut out,
        "fluxer_media_proxy_requests_2xx_total",
        &GLOBAL.requests_2xx,
    );
    render_request_series(
        &mut out,
        "fluxer_media_proxy_requests_3xx_total",
        &GLOBAL.requests_3xx,
    );
    render_request_series(
        &mut out,
        "fluxer_media_proxy_requests_4xx_total",
        &GLOBAL.requests_4xx,
    );
    render_request_series(
        &mut out,
        "fluxer_media_proxy_requests_5xx_total",
        &GLOBAL.requests_5xx,
    );
    render_histogram(
        &mut out,
        "fluxer_media_proxy_transform_image_duration_ms",
        &GLOBAL.transform_image_duration,
    );
    render_histogram(
        &mut out,
        "fluxer_media_proxy_transform_video_duration_ms",
        &GLOBAL.transform_video_duration,
    );
    render_histogram(
        &mut out,
        "fluxer_media_proxy_native_transform_wait_ms",
        &GLOBAL.native_transform_wait,
    );
    render_histogram(
        &mut out,
        "fluxer_media_proxy_request_duration_ms",
        &GLOBAL.request_duration,
    );
    render_per_kind_histogram(
        &mut out,
        "fluxer_media_proxy_request_duration_by_route_ms",
        &GLOBAL.request_duration_per_kind,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_coalescer_leader_total",
        &GLOBAL.coalescer_leader,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_coalescer_waiter_total",
        &GLOBAL.coalescer_waiter,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_transform_cache_hits_total",
        &GLOBAL.transform_cache_hits,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_transform_cache_misses_total",
        &GLOBAL.transform_cache_misses,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_storage_hits_total",
        &GLOBAL.storage_hits,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_storage_misses_total",
        &GLOBAL.storage_misses,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_storage_errors_total",
        &GLOBAL.storage_errors,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_nsfw_calls_ok_total",
        &GLOBAL.nsfw_calls_ok,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_nsfw_calls_failed_total",
        &GLOBAL.nsfw_calls_failed,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_nsfw_calls_disabled_total",
        &GLOBAL.nsfw_calls_disabled,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_transform_failures_total",
        &GLOBAL.transform_failures,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_decode_failures_total",
        &GLOBAL.decode_failures,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_fetch_failures_total",
        &GLOBAL.fetch_failures,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_external_stream_overruns_total",
        &GLOBAL.external_stream_overruns,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_blocked_url_attempts_total",
        &GLOBAL.blocked_url_attempts,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_framebuffer_pool_borrows_total",
        &GLOBAL.framebuffer_pool_borrows,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_framebuffer_pool_grow_events_total",
        &GLOBAL.framebuffer_pool_grow_events,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_relay_upstream_success_total",
        &GLOBAL.relay_upstream_success,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_relay_upstream_retries_total",
        &GLOBAL.relay_upstream_retries,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_http_retries_total",
        &GLOBAL.http_retries,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_http_retries_exhausted_total",
        &GLOBAL.http_retries_exhausted,
    );
    let _ = writeln!(
        out,
        "# TYPE fluxer_media_proxy_http_retryable_classifications_total counter"
    );
    let _ = writeln!(
        out,
        "fluxer_media_proxy_http_retryable_classifications_total{{reason=\"status\"}} {}",
        GLOBAL.http_retryable_status.load(Ordering::Relaxed)
    );
    let _ = writeln!(
        out,
        "fluxer_media_proxy_http_retryable_classifications_total{{reason=\"error\"}} {}",
        GLOBAL.http_retryable_error.load(Ordering::Relaxed)
    );
    let _ = writeln!(
        out,
        "# TYPE fluxer_media_proxy_relay_upstream_failures_total counter"
    );
    let _ = writeln!(
        out,
        "fluxer_media_proxy_relay_upstream_failures_total{{status=\"503\",retryable=\"true\"}} {}",
        GLOBAL
            .relay_upstream_failures_retryable
            .load(Ordering::Relaxed)
    );
    let _ = writeln!(
        out,
        "fluxer_media_proxy_relay_upstream_failures_total{{status=\"502\",retryable=\"false\"}} {}",
        GLOBAL.relay_upstream_failures_hard.load(Ordering::Relaxed)
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_hdr_tone_map_count_total",
        &GLOBAL.hdr_tone_map_count,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_heif_hdr_gain_map_count_total",
        &GLOBAL.heif_hdr_gain_map_count,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_avif_libheif_decode_count_total",
        &GLOBAL.avif_libheif_decode_count,
    );
    render_counter(
        &mut out,
        "fluxer_media_proxy_avif_libheif_decode_failures_total",
        &GLOBAL.avif_libheif_decode_failures,
    );
    let uptime_ms = now_ms() - GLOBAL.start_ms.load(Ordering::Relaxed);
    let _ = writeln!(
        out,
        "# HELP fluxer_media_proxy_process_uptime_seconds Seconds since process start"
    );
    let _ = writeln!(
        out,
        "# TYPE fluxer_media_proxy_process_uptime_seconds counter"
    );
    let _ = writeln!(
        out,
        "fluxer_media_proxy_process_uptime_seconds {:.3}",
        uptime_ms as f64 / 1000.0
    );
    out
}

fn render_counter(out: &mut String, name: &str, counter: &AtomicU64) {
    let _ = writeln!(out, "# TYPE {name} counter");
    let _ = writeln!(out, "{name} {}", counter.load(Ordering::Relaxed));
}

fn render_request_series(out: &mut String, name: &str, series: &[AtomicU64; 12]) {
    let _ = writeln!(out, "# TYPE {name} counter");
    for kind in RequestKind::ALL {
        let _ = writeln!(
            out,
            "{name}{{kind=\"{}\"}} {}",
            kind.label(),
            series[kind as usize].load(Ordering::Relaxed)
        );
    }
}

fn render_histogram(out: &mut String, name: &str, hist: &Histogram) {
    let _ = writeln!(out, "# TYPE {name} histogram");
    let mut cumulative = 0;
    for (i, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
        cumulative += hist.buckets[i].load(Ordering::Relaxed);
        let _ = writeln!(out, "{name}_bucket{{le=\"{upper}\"}} {cumulative}");
    }
    let _ = writeln!(
        out,
        "{name}_bucket{{le=\"+Inf\"}} {}",
        hist.inf.load(Ordering::Relaxed)
    );
    let _ = writeln!(out, "{name}_sum {}", hist.sum_ms.load(Ordering::Relaxed));
    let _ = writeln!(out, "{name}_count {}", hist.count.load(Ordering::Relaxed));
}

fn render_per_kind_histogram(out: &mut String, name: &str, hists: &[Histogram; 12]) {
    let _ = writeln!(out, "# TYPE {name} histogram");
    for kind in RequestKind::ALL {
        let hist = &hists[kind as usize];
        let label = kind.label();
        let mut cumulative = 0;
        for (i, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            cumulative += hist.buckets[i].load(Ordering::Relaxed);
            let _ = writeln!(
                out,
                "{name}_bucket{{kind=\"{label}\",le=\"{upper}\"}} {cumulative}"
            );
        }
        let _ = writeln!(
            out,
            "{name}_bucket{{kind=\"{label}\",le=\"+Inf\"}} {}",
            hist.inf.load(Ordering::Relaxed)
        );
        let _ = writeln!(
            out,
            "{name}_sum{{kind=\"{label}\"}} {}",
            hist.sum_ms.load(Ordering::Relaxed)
        );
        let _ = writeln!(
            out,
            "{name}_count{{kind=\"{label}\"}} {}",
            hist.count.load(Ordering::Relaxed)
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn histogram_observes_into_correct_bucket() {
        let h = Histogram::new();
        h.observe(0);
        h.observe(3);
        h.observe(50);
        h.observe(100_000);
        assert_eq!(1, h.buckets[0].load(Ordering::Relaxed));
        assert_eq!(1, h.buckets[1].load(Ordering::Relaxed));
        assert_eq!(1, h.buckets[4].load(Ordering::Relaxed));
        assert_eq!(4, h.inf.load(Ordering::Relaxed));
        assert_eq!(4, h.count.load(Ordering::Relaxed));
    }

    #[test]
    fn render_produces_parseable_prometheus_text() {
        init_global();
        GLOBAL.record_request(RequestKind::AssetImage, 200);
        GLOBAL.transform_image_duration.observe(42);
        let text = render();
        assert!(text.contains("# TYPE fluxer_media_proxy_requests_2xx_total counter\n"));
        assert!(text.contains("fluxer_media_proxy_requests_2xx_total{kind=\"asset_image\"}"));
        assert!(text.contains("fluxer_media_proxy_transform_image_duration_ms_bucket"));
        assert!(text.contains("fluxer_media_proxy_heif_hdr_gain_map_count_total "));
        assert!(text.contains("fluxer_media_proxy_http_retries_total "));
        assert!(text.contains(
            "fluxer_media_proxy_http_retryable_classifications_total{reason=\"status\"}"
        ));
    }

    #[test]
    fn request_kind_labels_are_unique() {
        let mut seen = HashSet::new();
        for kind in RequestKind::ALL {
            assert!(seen.insert(kind.label()));
        }
    }
}
