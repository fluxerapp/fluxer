// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    Metrics,
    histogram::Histogram,
    now_ms,
    request::{REQUEST_KIND_COUNT, RequestKind},
};
use std::{
    fmt::{self, Write as _},
    sync::atomic::{AtomicU64, Ordering},
};

pub(super) fn render_metrics(out: &mut String, metrics: &Metrics) -> fmt::Result {
    render_request_series(
        out,
        "fluxer_media_proxy_requests_1xx_total",
        &metrics.request.requests_1xx,
    )?;
    render_request_series(
        out,
        "fluxer_media_proxy_requests_2xx_total",
        &metrics.request.requests_2xx,
    )?;
    render_request_series(
        out,
        "fluxer_media_proxy_requests_3xx_total",
        &metrics.request.requests_3xx,
    )?;
    render_request_series(
        out,
        "fluxer_media_proxy_requests_4xx_total",
        &metrics.request.requests_4xx,
    )?;
    render_request_series(
        out,
        "fluxer_media_proxy_requests_5xx_total",
        &metrics.request.requests_5xx,
    )?;
    render_request_series(
        out,
        "fluxer_media_proxy_requests_other_total",
        &metrics.request.requests_other,
    )?;
    render_histogram(
        out,
        "fluxer_media_proxy_transform_image_duration_ms",
        &metrics.transform.transform_image_duration,
    )?;
    render_histogram(
        out,
        "fluxer_media_proxy_transform_video_duration_ms",
        &metrics.transform.transform_video_duration,
    )?;
    render_histogram(
        out,
        "fluxer_media_proxy_native_transform_wait_ms",
        &metrics.native_transform.native_transform_wait,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_native_transform_rejected_total",
        &metrics.native_transform.native_transform_rejected,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_native_tasks_detached_total",
        &metrics.native_transform.native_tasks_detached,
    )?;
    render_gauge(
        out,
        "fluxer_media_proxy_native_tasks_detached_active",
        &metrics.native_transform.native_tasks_detached_active,
    )?;
    render_histogram(
        out,
        "fluxer_media_proxy_native_task_detached_duration_ms",
        &metrics.native_transform.native_task_detached_duration,
    )?;
    render_histogram(
        out,
        "fluxer_media_proxy_request_duration_ms",
        &metrics.request.request_duration,
    )?;
    render_per_kind_histogram(
        out,
        "fluxer_media_proxy_request_duration_by_route_ms",
        &metrics.request.request_duration_per_kind,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_coalescer_leader_total",
        &metrics.coalescer.coalescer_leader,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_coalescer_waiter_total",
        &metrics.coalescer.coalescer_waiter,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_coalescer_waiter_rejected_total",
        &metrics.coalescer.coalescer_waiter_rejected,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_transform_cache_hits_total",
        &metrics.transform_cache.transform_cache_hits,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_transform_cache_misses_total",
        &metrics.transform_cache.transform_cache_misses,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_transform_cache_insert_rejected_total",
        &metrics.transform_cache.transform_cache_insert_rejected,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_transform_cache_evictions_total",
        &metrics.transform_cache.transform_cache_evictions,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_storage_hits_total",
        &metrics.storage.storage_hits,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_storage_misses_total",
        &metrics.storage.storage_misses,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_storage_errors_total",
        &metrics.storage.storage_errors,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_nsfw_calls_ok_total",
        &metrics.nsfw.nsfw_calls_ok,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_nsfw_calls_failed_total",
        &metrics.nsfw.nsfw_calls_failed,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_nsfw_calls_disabled_total",
        &metrics.nsfw.nsfw_calls_disabled,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_transform_failures_total",
        &metrics.transform.transform_failures,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_decode_failures_total",
        &metrics.transform.decode_failures,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_fetch_failures_total",
        &metrics.external.fetch_failures,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_blocked_url_attempts_total",
        &metrics.external.blocked_url_attempts,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_external_buffer_rejected_total",
        &metrics.external.external_buffer_rejected,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_external_stream_overruns_total",
        &metrics.external.external_stream_overruns,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_framebuffer_pool_borrows_total",
        &metrics.native_transform.framebuffer_pool_borrows,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_framebuffer_pool_grow_events_total",
        &metrics.native_transform.framebuffer_pool_grow_events,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_relay_upstream_success_total",
        &metrics.relay.relay_upstream_success,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_relay_upstream_retries_total",
        &metrics.relay.relay_upstream_retries,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_http_retries_total",
        &metrics.http_client.http_retries,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_http_retries_exhausted_total",
        &metrics.http_client.http_retries_exhausted,
    )?;
    writeln!(
        out,
        "# TYPE fluxer_media_proxy_http_retryable_classifications_total counter"
    )?;
    writeln!(
        out,
        "fluxer_media_proxy_http_retryable_classifications_total{{reason=\"status\"}} {}",
        metrics
            .http_client
            .http_retryable_status
            .load(Ordering::Relaxed)
    )?;
    writeln!(
        out,
        "fluxer_media_proxy_http_retryable_classifications_total{{reason=\"error\"}} {}",
        metrics
            .http_client
            .http_retryable_error
            .load(Ordering::Relaxed)
    )?;
    writeln!(
        out,
        "# TYPE fluxer_media_proxy_relay_upstream_failures_total counter"
    )?;
    writeln!(
        out,
        "fluxer_media_proxy_relay_upstream_failures_total{{status=\"503\",retryable=\"true\"}} {}",
        metrics
            .relay
            .relay_upstream_failures_retryable
            .load(Ordering::Relaxed)
    )?;
    writeln!(
        out,
        "fluxer_media_proxy_relay_upstream_failures_total{{status=\"502\",retryable=\"false\"}} {}",
        metrics
            .relay
            .relay_upstream_failures_hard
            .load(Ordering::Relaxed)
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_hdr_tone_map_count_total",
        &metrics.transform.hdr_tone_map_count,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_heif_hdr_gain_map_count_total",
        &metrics.transform.heif_hdr_gain_map_count,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_avif_libheif_decode_count_total",
        &metrics.transform.avif_libheif_decode_count,
    )?;
    render_counter(
        out,
        "fluxer_media_proxy_avif_libheif_decode_failures_total",
        &metrics.transform.avif_libheif_decode_failures,
    )?;
    let uptime_ms = now_ms() - metrics.start_ms;
    writeln!(
        out,
        "# HELP fluxer_media_proxy_process_uptime_seconds Seconds since process start"
    )?;
    writeln!(
        out,
        "# TYPE fluxer_media_proxy_process_uptime_seconds counter"
    )?;
    writeln!(
        out,
        "fluxer_media_proxy_process_uptime_seconds {:.3}",
        uptime_ms as f64 / 1000.0
    )
}

fn render_counter(out: &mut String, name: &str, counter: &AtomicU64) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    writeln!(out, "{name} {}", counter.load(Ordering::Relaxed))
}

fn render_gauge(out: &mut String, name: &str, gauge: &AtomicU64) -> fmt::Result {
    writeln!(out, "# TYPE {name} gauge")?;
    writeln!(out, "{name} {}", gauge.load(Ordering::Relaxed))
}

fn render_request_series(
    out: &mut String,
    name: &str,
    series: &[AtomicU64; REQUEST_KIND_COUNT],
) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    for kind in RequestKind::ALL {
        writeln!(
            out,
            "{name}{{kind=\"{}\"}} {}",
            kind.label(),
            series[kind as usize].load(Ordering::Relaxed)
        )?;
    }
    Ok(())
}

fn render_histogram(out: &mut String, name: &str, histogram: &Histogram) -> fmt::Result {
    writeln!(out, "# TYPE {name} histogram")?;
    histogram.render_series(out, name, None)
}

fn render_per_kind_histogram(
    out: &mut String,
    name: &str,
    histograms: &[Histogram; REQUEST_KIND_COUNT],
) -> fmt::Result {
    writeln!(out, "# TYPE {name} histogram")?;
    for kind in RequestKind::ALL {
        let label = format!("kind=\"{}\"", kind.label());
        histograms[kind as usize].render_series(out, name, Some(&label))?;
    }
    Ok(())
}
