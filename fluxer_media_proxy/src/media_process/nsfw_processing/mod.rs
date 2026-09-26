// SPDX-License-Identifier: AGPL-3.0-or-later

use super::encoding::{VipsEncodeRequest, anim_limits_from_options, encode_vips_image};
use super::image_probe::load_image;
use super::loaded_image::{page_height, resize_loaded_image, validate_vips_image};
use super::native_runtime::{clear_vips_error, ensure_vips_init, vips_runtime};
use super::transform::transform_image;
use super::{MediaError, native_status_error};
use crate::{
    image_quality::{ImageQuality, ResolvedImageQuality},
    image_transform::ImageOptions,
    media_limits::MediaLimits,
    media_type::MediaType,
    metrics::transform::TransformMetrics,
    mime,
    native::{
        self, NativeStatus,
        nsfw_frame_output::{NSFWFrameCopyError, NSFWFrameOutput},
    },
    nsfw::{NSFW_MAX_FRAME_BYTES, NSFWClassification, NSFWClient, NSFWScanRequest, NSFWThreshold},
    output_format::OutputFormat,
};
use libc::c_int;
use std::ptr;

const MAX_NSFW_SAMPLE_FRAMES: usize = 3;
const NSFW_FRAME_SEED_BYTES: usize = 4 * 1024;
const NSFW_FRAME_MAX_DIMENSION: u32 = 512;

pub(super) struct NSFWScanSource<'a> {
    pub(super) media_limits: &'a MediaLimits,
    pub(super) metrics: &'a TransformMetrics,
    pub(super) threshold: NSFWThreshold,
    pub(super) content_type: &'a str,
    pub(super) animated: bool,
    pub(super) frame_count: u32,
    pub(super) input: &'a [u8],
    pub(super) duration_seconds: Option<f64>,
    pub(super) deadline_ms: Option<i64>,
}

pub(super) struct NSFWScanPreparation {
    request: NSFWScanRequest,
    stage: &'static str,
}

fn prepared_scan(
    threshold: NSFWThreshold,
    frames: Vec<Vec<u8>>,
    stage: &'static str,
) -> Result<Option<NSFWScanPreparation>, MediaError> {
    let request = NSFWScanRequest::new(threshold, frames).map_err(|err| {
        tracing::warn!("nsfw {stage} scan request rejected: {err}");
        MediaError::NsfwScanUnavailable
    })?;
    Ok(Some(NSFWScanPreparation { request, stage }))
}

pub(super) fn nsfw_scan_buffers(
    source: NSFWScanSource<'_>,
) -> Result<Option<NSFWScanPreparation>, MediaError> {
    let category = mime::category(source.content_type);
    if category == Some(mime::Category::Video) {
        let frames = nsfw_video_scan_buffers(&source).map_err(|err| {
            tracing::warn!("nsfw video frame extract failed: {err:?}");
            MediaError::NsfwScanUnavailable
        })?;
        return prepared_scan(source.threshold, frames, "video");
    }
    if category != Some(mime::Category::Image) {
        return Ok(None);
    }
    if source.animated {
        match extract_animated_image_frames_for_nsfw(&source) {
            Ok(frames) => return prepared_scan(source.threshold, frames, "animated"),
            Err(err) => tracing::warn!(
                "nsfw animated frame extract failed: {err:?} - falling back to static scan"
            ),
        }
    }
    let jpeg = encode_static_image_for_nsfw(
        source.input,
        source.media_limits,
        source.metrics,
        source.deadline_ms,
    )
    .map_err(|err| {
        tracing::warn!("nsfw static JPEG encode failed: {err:?}");
        MediaError::NsfwScanUnavailable
    })?;
    let mut frames = Vec::new();
    frames
        .try_reserve_exact(1)
        .map_err(|_| MediaError::AllocationFailed)?;
    frames.push(jpeg);
    prepared_scan(source.threshold, frames, "static")
}

pub(super) async fn classify_nsfw_buffers(
    client: &NSFWClient,
    prepared: Option<NSFWScanPreparation>,
) -> Result<NSFWClassification, MediaError> {
    let Some(NSFWScanPreparation { request, stage }) = prepared else {
        return Ok(NSFWClassification::not_scanned());
    };
    client.check_buffers(request).await.map_err(|err| {
        tracing::warn!("nsfw {stage} classify failed: {err}");
        MediaError::NsfwScanUnavailable
    })
}

fn nsfw_video_scan_buffers(source: &NSFWScanSource<'_>) -> Result<Vec<Vec<u8>>, MediaError> {
    extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        media_limits: source.media_limits,
        input: source.input,
        duration_seconds: source.duration_seconds,
        deadline_ms: source.deadline_ms,
    })
}

fn extract_animated_image_frames_for_nsfw(
    source: &NSFWScanSource<'_>,
) -> Result<Vec<Vec<u8>>, MediaError> {
    let media_type = MediaType::from_mime(source.content_type);
    if let Some(media_type @ (MediaType::APNG | MediaType::GIF | MediaType::WebP)) = media_type {
        return extract_native_animated_frames_for_nsfw(
            source.media_limits,
            source.input,
            source.frame_count,
            media_type,
            source.deadline_ms,
        );
    }
    extract_vips_animation_frames_for_nsfw(source.media_limits, source.input, source.deadline_ms)
}

fn animated_nsfw_frame_indices(frame_count: u32) -> Result<Vec<u32>, MediaError> {
    if frame_count == 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let candidates = if frame_count == 1 {
        [0, 0, 0]
    } else if frame_count == 2 {
        [0, 1, 1]
    } else {
        [0, frame_count / 2, frame_count - 1]
    };
    let mut indices = Vec::new();
    indices
        .try_reserve_exact(MAX_NSFW_SAMPLE_FRAMES)
        .map_err(|_| MediaError::AllocationFailed)?;
    for index in candidates {
        if !indices.contains(&index) {
            indices.push(index);
        }
    }
    Ok(indices)
}

fn native_source_frame_limit(media_limits: &MediaLimits) -> c_int {
    media_limits.animated_frames().min(c_int::MAX as u32) as c_int
}

fn extract_native_animated_frames_for_nsfw(
    media_limits: &MediaLimits,
    input: &[u8],
    frame_count: u32,
    media_type: MediaType,
    deadline_ms: Option<i64>,
) -> Result<Vec<Vec<u8>>, MediaError> {
    if input.is_empty() || input.len() > media_limits.max_media_proxy_bytes() {
        return Err(MediaError::StreamTooLong);
    }
    let runtime = vips_runtime()?;
    let source_indices = animated_nsfw_frame_indices(frame_count)?;
    let mut indices: Vec<c_int> = Vec::new();
    indices
        .try_reserve_exact(source_indices.len())
        .map_err(|_| MediaError::AllocationFailed)?;
    for index in source_indices {
        indices.push(c_int::try_from(index).map_err(|_| MediaError::InvalidImageDimensions)?);
    }
    let mut output = NSFWFrameOutput::new(indices.len());
    let max_frames = native_source_frame_limit(media_limits);
    let max_total_pixels = media_limits.animated_total_pixels();
    let deadline = deadline_ms.unwrap_or(0);
    let status_code = unsafe {
        match media_type {
            MediaType::APNG => native::fluxer_ffmpeg_extract_apng_frames_for_nsfw(
                input.as_ptr().cast(),
                input.len(),
                runtime.config().ffmpeg_decoder_threads(),
                deadline,
                indices.as_ptr(),
                indices.len(),
                max_frames,
                max_total_pixels,
                NSFW_MAX_FRAME_BYTES,
                output.as_mut_ptr(),
            ),
            MediaType::GIF => native::fluxer_ffmpeg_extract_gif_frames_for_nsfw(
                input.as_ptr().cast(),
                input.len(),
                runtime.config().ffmpeg_decoder_threads(),
                deadline,
                indices.as_ptr(),
                indices.len(),
                max_frames,
                max_total_pixels,
                NSFW_MAX_FRAME_BYTES,
                output.as_mut_ptr(),
            ),
            MediaType::WebP => native::fluxer_webp_extract_frames_for_nsfw(
                input.as_ptr().cast(),
                input.len(),
                runtime.config().webp_thread_level(),
                deadline,
                indices.as_ptr(),
                indices.len(),
                max_frames,
                max_total_pixels,
                NSFW_MAX_FRAME_BYTES,
                output.as_mut_ptr(),
            ),
            _ => return Err(MediaError::MediaDecodeFailed),
        }
    };
    copy_native_nsfw_frames(&output, status_code)
}

fn copy_native_nsfw_frames(
    output: &NSFWFrameOutput,
    status_code: c_int,
) -> Result<Vec<Vec<u8>>, MediaError> {
    if let Some(error) = native_status_error(
        NativeStatus::from_code(status_code),
        MediaError::MediaDecodeFailed,
    ) {
        return Err(error);
    }
    output.copy_frames().map_err(|err| match err {
        NSFWFrameCopyError::AllocationFailed => MediaError::AllocationFailed,
        NSFWFrameCopyError::InvalidOutput => MediaError::MediaDecodeFailed,
    })
}

fn extract_vips_animation_frames_for_nsfw(
    media_limits: &MediaLimits,
    input: &[u8],
    deadline_ms: Option<i64>,
) -> Result<Vec<Vec<u8>>, MediaError> {
    ensure_vips_init()?;
    let loaded = load_image(input, "n=-1,access=sequential,fail=true")?;
    validate_vips_image(media_limits, &loaded)?;
    let total_height = unsafe { native::fluxer_vips_image_get_height(loaded.as_ptr()) };
    let width = unsafe { native::fluxer_vips_image_get_width(loaded.as_ptr()) };
    if total_height <= 0 || width <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let page_h = page_height(&loaded)?.ok_or(MediaError::InvalidImageDimensions)?;
    let n_pages =
        u32::try_from(total_height / page_h).map_err(|_| MediaError::InvalidImageDimensions)?;
    let indices = animated_nsfw_frame_indices(n_pages)?;
    let mut out: Vec<Vec<u8>> = Vec::new();
    out.try_reserve_exact(indices.len())
        .map_err(|_| MediaError::AllocationFailed)?;
    for idx in indices {
        let index = c_int::try_from(idx).map_err(|_| MediaError::InvalidImageDimensions)?;
        let top = index
            .checked_mul(page_h)
            .ok_or(MediaError::InvalidImageDimensions)?;
        let mut sub_raw = ptr::null_mut();
        let rc = unsafe {
            native::fluxer_vips_extract_area(loaded.as_ptr(), &mut sub_raw, 0, top, width, page_h)
        };
        let sub = unsafe { loaded.adopt_derived_raw(sub_raw) };
        if let Some(error) = native_status_error(
            NativeStatus::from_code(rc),
            MediaError::MediaTransformFailed,
        ) {
            clear_vips_error();
            return Err(error);
        }
        let sub = sub.ok_or_else(|| {
            clear_vips_error();
            MediaError::MediaTransformFailed
        })?;
        let resized = resize_loaded_image(
            media_limits,
            sub,
            &ImageOptions {
                width: Some(NSFW_FRAME_MAX_DIMENSION),
                height: Some(NSFW_FRAME_MAX_DIMENSION),
                deadline_ms,
                ..Default::default()
            },
        )?;
        out.push(encode_vips_image(
            &resized,
            VipsEncodeRequest {
                format: OutputFormat::JPEG,
                quality: ResolvedImageQuality::Low,
                page_height: None,
                effort_override: None,
                animation: anim_limits_from_options(&ImageOptions {
                    deadline_ms,
                    ..Default::default()
                }),
                animation_loop_count: None,
                media_limits,
                full_canvas_animation: false,
            },
        )?);
    }
    if out.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(out)
}

pub(super) fn compute_frame_sample_timestamps(
    duration_seconds: Option<f64>,
    prng: &mut rand_chacha::ChaCha8Rng,
) -> [f64; MAX_NSFW_SAMPLE_FRAMES] {
    const DEFAULT_SAMPLING_WINDOW_SECONDS: f64 = 1.0;

    use rand::RngExt;
    let valid: Option<f64> = duration_seconds.filter(|d| d.is_finite() && *d > 0.0);
    let sampling_window_seconds = valid.unwrap_or(DEFAULT_SAMPLING_WINDOW_SECONDS);

    let clamp = |v: f64| -> f64 {
        if !v.is_finite() {
            return 0.0;
        }
        match valid {
            Some(max_v) => v.clamp(0.0, max_v),
            None => v.max(0.0),
        }
    };

    let start_base = clamp((sampling_window_seconds * 0.1 + 0.5).clamp(1.0, 2.0));
    let middle_base = clamp(sampling_window_seconds / 2.0);
    let end_candidate = if sampling_window_seconds > 2.0 {
        sampling_window_seconds - 1.0
    } else {
        sampling_window_seconds * 0.95
    };
    let min_end = start_base + 0.5;
    let end_base = clamp(end_candidate.max(min_end));

    let mut jitter = |v: f64| -> f64 {
        let radius = (v.abs() * 0.1).max(0.05);
        let r: f64 = prng.random();
        clamp(v + (r * 2.0 - 1.0) * radius)
    };

    [jitter(start_base), jitter(middle_base), jitter(end_base)]
}

pub(super) fn nsfw_frame_seed(input: &[u8]) -> u64 {
    let take = input.len().min(NSFW_FRAME_SEED_BYTES);
    wyhash::wyhash(&input[..take], 0)
}

pub(super) struct VideoNSFWFramesRequest<'a> {
    pub(super) media_limits: &'a MediaLimits,
    pub(super) input: &'a [u8],
    pub(super) duration_seconds: Option<f64>,
    pub(super) deadline_ms: Option<i64>,
}

pub(super) fn extract_video_frames_for_nsfw(
    request: VideoNSFWFramesRequest<'_>,
) -> Result<Vec<Vec<u8>>, MediaError> {
    use rand::SeedableRng as _;
    let VideoNSFWFramesRequest {
        media_limits,
        input,
        duration_seconds,
        deadline_ms,
    } = request;
    if input.is_empty() || input.len() > media_limits.max_media_proxy_bytes() {
        return Err(MediaError::StreamTooLong);
    }
    let runtime = vips_runtime()?;
    let seed = nsfw_frame_seed(input);
    let mut prng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
    let timestamps = compute_frame_sample_timestamps(duration_seconds, &mut prng);

    let mut output = NSFWFrameOutput::new(timestamps.len());
    let status_code = unsafe {
        native::fluxer_av_extract_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            deadline_ms.unwrap_or(0),
            timestamps.as_ptr(),
            timestamps.len(),
            NSFW_MAX_FRAME_BYTES,
            output.as_mut_ptr(),
        )
    };
    copy_native_nsfw_frames(&output, status_code)
}

pub fn encode_static_image_for_nsfw(
    input: &[u8],
    media_limits: &MediaLimits,
    metrics: &TransformMetrics,
    deadline_ms: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    let options = ImageOptions {
        width: Some(NSFW_FRAME_MAX_DIMENSION),
        height: Some(NSFW_FRAME_MAX_DIMENSION),
        format: OutputFormat::JPEG,
        quality: ImageQuality::Low,
        deadline_ms,
        ..Default::default()
    };
    transform_image(input, &options, media_limits, metrics).map(|media| media.bytes)
}

#[cfg(test)]
mod tests;
