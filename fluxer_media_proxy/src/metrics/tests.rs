// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{Metrics, duration_millis, histogram::Histogram, request::RequestKind};
use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::time::Duration;

const FROZEN_SERIES: &[(&str, &str)] = &[
    ("fluxer_media_proxy_requests_2xx_total", "counter"),
    ("fluxer_media_proxy_requests_3xx_total", "counter"),
    ("fluxer_media_proxy_requests_4xx_total", "counter"),
    ("fluxer_media_proxy_requests_5xx_total", "counter"),
    (
        "fluxer_media_proxy_transform_image_duration_ms",
        "histogram",
    ),
    (
        "fluxer_media_proxy_transform_video_duration_ms",
        "histogram",
    ),
    ("fluxer_media_proxy_native_transform_wait_ms", "histogram"),
    ("fluxer_media_proxy_request_duration_ms", "histogram"),
    (
        "fluxer_media_proxy_request_duration_by_route_ms",
        "histogram",
    ),
    ("fluxer_media_proxy_coalescer_leader_total", "counter"),
    ("fluxer_media_proxy_coalescer_waiter_total", "counter"),
    ("fluxer_media_proxy_transform_cache_hits_total", "counter"),
    ("fluxer_media_proxy_transform_cache_misses_total", "counter"),
    ("fluxer_media_proxy_storage_hits_total", "counter"),
    ("fluxer_media_proxy_storage_misses_total", "counter"),
    ("fluxer_media_proxy_storage_errors_total", "counter"),
    ("fluxer_media_proxy_nsfw_calls_ok_total", "counter"),
    ("fluxer_media_proxy_nsfw_calls_failed_total", "counter"),
    ("fluxer_media_proxy_nsfw_calls_disabled_total", "counter"),
    ("fluxer_media_proxy_transform_failures_total", "counter"),
    ("fluxer_media_proxy_decode_failures_total", "counter"),
    ("fluxer_media_proxy_fetch_failures_total", "counter"),
    ("fluxer_media_proxy_blocked_url_attempts_total", "counter"),
    (
        "fluxer_media_proxy_framebuffer_pool_borrows_total",
        "counter",
    ),
    (
        "fluxer_media_proxy_framebuffer_pool_grow_events_total",
        "counter",
    ),
    ("fluxer_media_proxy_relay_upstream_success_total", "counter"),
    ("fluxer_media_proxy_relay_upstream_retries_total", "counter"),
    ("fluxer_media_proxy_http_retries_total", "counter"),
    ("fluxer_media_proxy_http_retries_exhausted_total", "counter"),
    (
        "fluxer_media_proxy_http_retryable_classifications_total",
        "counter",
    ),
    (
        "fluxer_media_proxy_relay_upstream_failures_total",
        "counter",
    ),
    ("fluxer_media_proxy_hdr_tone_map_count_total", "counter"),
    (
        "fluxer_media_proxy_heif_hdr_gain_map_count_total",
        "counter",
    ),
    (
        "fluxer_media_proxy_avif_libheif_decode_count_total",
        "counter",
    ),
    (
        "fluxer_media_proxy_avif_libheif_decode_failures_total",
        "counter",
    ),
    ("fluxer_media_proxy_process_uptime_seconds", "counter"),
];

const GOLDEN_IMAGE_DURATION_SERIES: &str = r#"# TYPE fluxer_media_proxy_transform_image_duration_ms histogram
fluxer_media_proxy_transform_image_duration_ms_bucket{le="1"} 0
fluxer_media_proxy_transform_image_duration_ms_bucket{le="5"} 0
fluxer_media_proxy_transform_image_duration_ms_bucket{le="10"} 0
fluxer_media_proxy_transform_image_duration_ms_bucket{le="25"} 0
fluxer_media_proxy_transform_image_duration_ms_bucket{le="50"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="100"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="250"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="500"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="1000"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="2500"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="5000"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="10000"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="30000"} 1
fluxer_media_proxy_transform_image_duration_ms_bucket{le="+Inf"} 1
fluxer_media_proxy_transform_image_duration_ms_sum 42
fluxer_media_proxy_transform_image_duration_ms_count 1
"#;

const GOLDEN_ROUTE_DURATION_SERIES: &str = r#"fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="1"} 0
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="5"} 0
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="10"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="25"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="50"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="100"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="250"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="500"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="1000"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="2500"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="5000"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="10000"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="30000"} 1
fluxer_media_proxy_request_duration_by_route_ms_bucket{kind="asset_image",le="+Inf"} 1
fluxer_media_proxy_request_duration_by_route_ms_sum{kind="asset_image"} 7
fluxer_media_proxy_request_duration_by_route_ms_count{kind="asset_image"} 1
"#;

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
    let metrics = Metrics::new();
    metrics
        .request()
        .record_request(RequestKind::AssetImage, 200);
    metrics.transform().observe_image_duration(42);
    let text = metrics.render();
    assert!(text.contains("# TYPE fluxer_media_proxy_requests_2xx_total counter\n"));
    assert!(text.contains("fluxer_media_proxy_requests_2xx_total{kind=\"asset_image\"}"));
    assert!(text.contains("fluxer_media_proxy_transform_image_duration_ms_bucket"));
    assert!(text.contains("fluxer_media_proxy_heif_hdr_gain_map_count_total "));
    assert!(text.contains("fluxer_media_proxy_http_retries_total "));
    assert!(
        text.contains("fluxer_media_proxy_http_retryable_classifications_total{reason=\"status\"}")
    );
}

#[test]
fn request_kind_labels_are_unique() {
    let mut seen = HashSet::new();
    for kind in RequestKind::ALL {
        assert!(seen.insert(kind.label()));
    }
}

#[test]
fn render_keeps_every_frozen_series_name_and_shape() {
    let metrics = Metrics::new();
    metrics.transform().observe_image_duration(42);
    metrics
        .request()
        .record_request_with_duration(RequestKind::AssetImage, 200, 7);
    let text = metrics.render();

    for (name, kind) in FROZEN_SERIES {
        assert!(
            text.contains(&format!("# TYPE {name} {kind}\n")),
            "missing frozen series {name}"
        );
    }
    for kind in RequestKind::ALL {
        assert!(text.contains(&format!(
            "fluxer_media_proxy_requests_4xx_total{{kind=\"{}\"}} 0\n",
            kind.label()
        )));
    }
    assert!(text.contains(GOLDEN_IMAGE_DURATION_SERIES));
    assert!(text.contains(GOLDEN_ROUTE_DURATION_SERIES));
    assert!(text.contains(
        "fluxer_media_proxy_http_retryable_classifications_total{reason=\"status\"} 0\nfluxer_media_proxy_http_retryable_classifications_total{reason=\"error\"} 0\n"
    ));
    assert!(text.contains(
        "fluxer_media_proxy_relay_upstream_failures_total{status=\"503\",retryable=\"true\"} 0\nfluxer_media_proxy_relay_upstream_failures_total{status=\"502\",retryable=\"false\"} 0\n"
    ));
    assert!(text.contains(
        "# HELP fluxer_media_proxy_process_uptime_seconds Seconds since process start\n# TYPE fluxer_media_proxy_process_uptime_seconds counter\nfluxer_media_proxy_process_uptime_seconds 0."
    ));
}

#[test]
fn informational_and_unknown_statuses_stay_out_of_the_client_error_series() {
    let metrics = Metrics::new();
    let request = metrics.request();
    request.record_request(RequestKind::External, 100);
    request.record_request(RequestKind::External, 404);
    request.record_request(RequestKind::External, 700);
    let text = metrics.render();
    assert!(text.contains("fluxer_media_proxy_requests_1xx_total{kind=\"external\"} 1\n"));
    assert!(text.contains("fluxer_media_proxy_requests_4xx_total{kind=\"external\"} 1\n"));
    assert!(text.contains("fluxer_media_proxy_requests_other_total{kind=\"external\"} 1\n"));
}

#[test]
fn duration_millis_truncates_to_whole_milliseconds() {
    assert_eq!(0, duration_millis(Duration::from_micros(999)));
    assert_eq!(1_500, duration_millis(Duration::from_millis(1_500)));
    assert_eq!(u64::MAX, duration_millis(Duration::MAX));
}
