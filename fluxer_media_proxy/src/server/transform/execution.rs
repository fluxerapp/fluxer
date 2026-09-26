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
    let timed = runtime
        .tasks()
        .run_native(deadline, move || {
            let started = Instant::now();
            let media =
                media_process::transform_image(&data, &options, &media_limits, &transform_metrics)?;
            Ok(TimedMedia {
                media,
                elapsed_ms: metrics::duration_millis(started.elapsed()),
            })
        })
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
    let timed = runtime
        .tasks()
        .run_native(deadline, move || {
            let started = Instant::now();
            let thumbnail = media_process::extract_video_thumbnail(&data, format, &media_limits)?;
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
        })
        .await?;
    runtime.metrics().observe_video_duration(timed.elapsed_ms);
    request_log::record_stage(Stage::Transform, timed.elapsed_ms);
    Ok(timed.media)
}

pub(in crate::server) fn coalesced_work_result<T>(result: anyhow::Result<T>) -> anyhow::Result<T> {
    match result {
        Ok(value) => Ok(value),
        Err(error) if transform_error_is_timeout(&error) => {
            Err(anyhow::Error::new(CoalescerError::RequestTimeout))
        }
        Err(error)
            if error.downcast_ref::<TimedSemaphoreError>()
                == Some(&TimedSemaphoreError::Closed) =>
        {
            Err(anyhow::Error::new(CoalescerError::Unavailable))
        }
        Err(error) => Err(error),
    }
}

pub(in crate::server) fn transform_error_is_timeout(error: &anyhow::Error) -> bool {
    // A shed admission is reported as a timeout: the old era had one admission semaphore whose
    // exhaustion surfaced as TimedSemaphoreError::RequestTimeout, so every caller answered 504
    // with its own timeout reason code. Giving a full queue its own class moves the transform
    // routes to 503, metadata SVG rasterize to 400, and on a stored asset it stops counting as a
    // timeout at all, which degrades the reply to the untransformed original.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_admission_failures_are_retyped_for_the_coalescer() {
        let shed = coalesced_work_result::<()>(Err(TimedSemaphoreError::QueueFull.into()))
            .expect_err("a full queue sheds the request");
        assert_eq!(
            Some(&CoalescerError::RequestTimeout),
            shed.downcast_ref::<CoalescerError>()
        );
        let unavailable = coalesced_work_result::<()>(Err(TimedSemaphoreError::Closed.into()))
            .expect_err("a closed pool is unavailable");
        assert_eq!(
            Some(&CoalescerError::Unavailable),
            unavailable.downcast_ref::<CoalescerError>()
        );
        let timed_out =
            coalesced_work_result::<()>(Err(TimedSemaphoreError::RequestTimeout.into()))
                .expect_err("a deadline is a timeout");
        assert_eq!(
            Some(&CoalescerError::RequestTimeout),
            timed_out.downcast_ref::<CoalescerError>()
        );
        let failed = coalesced_work_result::<()>(Err(MediaError::MediaDecodeFailed.into()))
            .expect_err("a decode failure passes through");
        assert_eq!(
            Some(&MediaError::MediaDecodeFailed),
            failed.downcast_ref::<MediaError>()
        );
    }

    #[test]
    fn a_full_native_queue_answers_the_gateway_timeout_the_old_era_answered() {
        let queue_full: anyhow::Error = TimedSemaphoreError::QueueFull.into();
        assert!(
            transform_error_is_timeout(&queue_full),
            "a shed admission is a timeout for every caller, not just the coalescer"
        );
        let shed = coalesced_work_result::<()>(Err(TimedSemaphoreError::QueueFull.into()))
            .expect_err("a full queue sheds the request");
        let error = shed
            .downcast_ref::<CoalescerError>()
            .copied()
            .expect("the coalescer sees a typed failure");
        let response = crate::server::transform::cache::coalescer_failure_response(
            error,
            "coalescer_timeout_image",
        )
        .expect("a shed transform has a response");
        assert_eq!(
            axum::http::StatusCode::GATEWAY_TIMEOUT,
            response.status(),
            "the old era answered 504 when the admission queue was full"
        );
        assert_eq!(
            Some("coalescer_timeout_image"),
            response
                .extensions()
                .get::<request_log::ErrorReason>()
                .map(|reason| reason.code)
        );
    }
}
