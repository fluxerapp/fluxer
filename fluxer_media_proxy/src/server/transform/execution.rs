// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    coalescer::CoalescerError,
    image_quality::ImageQuality,
    image_transform::{AnimationMode, ImageOptions},
    media_process::{self, MediaError, ProcessedMedia},
    metrics,
    output_format::OutputFormat,
    request_log::{self, Stage},
    server::transform::TransformRuntime,
    timed_semaphore::TimedSemaphoreError,
};
use bytes::Bytes;
use std::time::{Duration, Instant};

struct TimedMedia {
    media: ProcessedMedia,
    elapsed_ms: u64,
}

pub(in crate::server) struct VideoTransformOptions {
    pub(in crate::server) format: OutputFormat,
    pub(in crate::server) width: Option<u32>,
    pub(in crate::server) height: Option<u32>,
    pub(in crate::server) quality: ImageQuality,
    pub(in crate::server) deadline_ms: Option<i64>,
}

pub(in crate::server) async fn run_transform(
    runtime: &TransformRuntime,
    data: Bytes,
    options: ImageOptions,
) -> anyhow::Result<ProcessedMedia> {
    let deadline = deadline_instant(options.deadline_ms);
    let media_limits = runtime.limits();
    let transform_metrics = runtime.metrics();
    let cost_data = data.clone();
    let timed = runtime
        .tasks()
        .run_native(
            deadline,
            move || {
                Ok(media_process::image_transform_cost(
                    &cost_data,
                    &options,
                    &media_limits,
                )?)
            },
            move || {
                let started = Instant::now();
                let media = media_process::transform_image(
                    &data,
                    &options,
                    &media_limits,
                    &transform_metrics,
                )?;
                Ok(TimedMedia {
                    media,
                    elapsed_ms: metrics::duration_millis(started.elapsed()),
                })
            },
        )
        .await?;
    runtime.metrics().observe_image_duration(timed.elapsed_ms);
    request_log::record_stage(Stage::Transform, timed.elapsed_ms);
    Ok(timed.media)
}

pub(in crate::server) async fn run_video_transform(
    runtime: &TransformRuntime,
    data: Bytes,
    options: VideoTransformOptions,
) -> anyhow::Result<ProcessedMedia> {
    let VideoTransformOptions {
        format,
        width,
        height,
        quality,
        deadline_ms,
    } = options;
    let deadline = deadline_instant(deadline_ms);
    let media_limits = runtime.limits();
    let transform_metrics = runtime.metrics();
    let cost_bytes = media_process::av_native_cost(data.len());
    let timed = runtime
        .tasks()
        .run_native(
            deadline,
            move || Ok(cost_bytes),
            move || {
                let started = Instant::now();
                let thumbnail =
                    media_process::extract_video_thumbnail(&data, format, &media_limits)?;
                let media = if width.is_none() && height.is_none() {
                    thumbnail
                } else {
                    media_process::transform_image(
                        &thumbnail.bytes,
                        &ImageOptions {
                            width,
                            height,
                            format,
                            quality,
                            animation: AnimationMode::Static,
                            deadline_ms,
                            ..Default::default()
                        },
                        &media_limits,
                        &transform_metrics,
                    )?
                };
                Ok(TimedMedia {
                    media,
                    elapsed_ms: metrics::duration_millis(started.elapsed()),
                })
            },
        )
        .await?;
    runtime.metrics().observe_video_duration(timed.elapsed_ms);
    request_log::record_stage(Stage::Transform, timed.elapsed_ms);
    Ok(timed.media)
}

pub(in crate::server) fn coalesced_work_result<T>(result: anyhow::Result<T>) -> anyhow::Result<T> {
    let Err(error) = result else {
        return result;
    };
    let retyped = match error.downcast_ref::<TimedSemaphoreError>() {
        Some(TimedSemaphoreError::QueueFull | TimedSemaphoreError::RequestTimeout) => {
            CoalescerError::RequestTimeout
        }
        Some(TimedSemaphoreError::Closed) => CoalescerError::Unavailable,
        None => return Err(error),
    };
    Err(anyhow::Error::new(retyped))
}

pub(in crate::server) fn transform_error_is_timeout(error: &anyhow::Error) -> bool {
    // A shed admission is reported as a timeout: the old era had one admission semaphore whose
    // exhaustion surfaced as TimedSemaphoreError::RequestTimeout, so every caller answered 504
    // with its own timeout reason code. Giving a full queue its own class moves the transform
    // routes to 503 and metadata SVG rasterize to 400.
    let admission = error.downcast_ref::<TimedSemaphoreError>();
    error.downcast_ref::<MediaError>() == Some(&MediaError::RequestTimeout)
        || admission == Some(&TimedSemaphoreError::RequestTimeout)
        || admission == Some(&TimedSemaphoreError::QueueFull)
}

pub(in crate::server) fn deadline_instant(deadline_ms: Option<i64>) -> Option<Instant> {
    let deadline_ms = deadline_ms?;
    let remaining_ms = deadline_ms.saturating_sub(metrics::now_ms()).max(0) as u64;
    Some(Instant::now() + Duration::from_millis(remaining_ms))
}
