// SPDX-License-Identifier: AGPL-3.0-or-later

use super::apng::encode_animated_apng;
use super::native_runtime::{clear_vips_error, vips_runtime};
use super::streaming_encoder::write_vips_image_to_vec;
use super::transform_plan::output_suffix;
use super::{
    MediaError, native_animated_frame_limit, native_buffer_to_media_bytes, native_status_error,
};
use crate::{
    constants,
    image_quality::ResolvedImageQuality,
    image_transform::{EncodeEffort, ImageOptions},
    media_limits::MediaLimits,
    native::{
        self, NativeStatus, VipsImageHandle, buffer::NativeBuffer, delay_array::VipsDelayArray,
    },
    output_format::OutputFormat,
};
use libc::{c_int, c_void, size_t};
use std::ptr;

const ANIMATED_ENCODE_FLUSH_HEADROOM_MS: i64 = 3_000;
const GIF_PLAY_COUNT_MAX: u32 = u16::MAX as u32 + 1;
const WEBP_PLAY_COUNT_MAX: u32 = u16::MAX as u32;

#[derive(Clone, Copy, Debug)]
pub(super) struct AnimLimits {
    pub(super) deadline_unix_ms: Option<i64>,
    pub(super) flush_deadline_unix_ms: Option<i64>,
    pub(super) max_frames: u32,
    pub(super) max_duration_ms: u32,
}

pub(super) fn anim_limits_from_options(options: &ImageOptions) -> AnimLimits {
    let encode_limits = options.animation.encode_limits();
    AnimLimits {
        deadline_unix_ms: options.deadline_ms.map(|deadline| {
            if deadline > ANIMATED_ENCODE_FLUSH_HEADROOM_MS {
                deadline - ANIMATED_ENCODE_FLUSH_HEADROOM_MS
            } else {
                deadline
            }
        }),
        flush_deadline_unix_ms: options.deadline_ms,
        max_frames: encode_limits.max_frames().get(),
        max_duration_ms: encode_limits.max_duration_ms().get(),
    }
}

fn native_optional_dimension(dimension: Option<u32>) -> Result<c_int, MediaError> {
    match dimension {
        None => Ok(0),
        Some(0) => Err(MediaError::InvalidImageDimensions),
        Some(value) => c_int::try_from(value).map_err(|_| MediaError::InvalidImageDimensions),
    }
}

pub(super) fn frame_delays_ms(
    image: &VipsImageHandle<'_>,
    page_count: c_int,
) -> Result<Vec<u32>, MediaError> {
    let delays = VipsDelayArray::read(image, page_count).map_err(|status| {
        if status == NativeStatus::AllocationFailed {
            return MediaError::AllocationFailed;
        }
        MediaError::MediaEncodeFailed
    })?;
    delays
        .as_slice()
        .iter()
        .copied()
        .map(|delay| {
            u32::try_from(delay)
                .ok()
                .filter(|delay| *delay > 0)
                .ok_or(MediaError::MediaEncodeFailed)
        })
        .collect()
}

pub(super) fn truncated_frame_count(
    delays: &[u32],
    max_frames: u32,
    max_duration_ms: u32,
) -> usize {
    let mut timestamp_ms = 0u32;
    let mut frames = 0usize;
    for delay_ms in delays.iter().copied() {
        if max_frames > 0 && frames as u64 >= max_frames as u64 {
            break;
        }
        if max_duration_ms > 0 && timestamp_ms >= max_duration_ms {
            break;
        }
        frames += 1;
        timestamp_ms = timestamp_ms.saturating_add(delay_ms);
    }
    frames
}

fn apply_gif_animation_loop_count(
    image: &VipsImageHandle<'_>,
    loop_count: u32,
) -> Result<(), MediaError> {
    let loop_count = loop_count.min(GIF_PLAY_COUNT_MAX) as c_int;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_set_animation_loop_count(image.as_ptr(), loop_count)
    });
    if let Some(error) = native_status_error(status, MediaError::MediaEncodeFailed) {
        clear_vips_error();
        return Err(error);
    }
    Ok(())
}

pub(super) struct VipsEncodeRequest<'limits> {
    pub(super) format: OutputFormat,
    pub(super) quality: ResolvedImageQuality,
    pub(super) page_height: Option<c_int>,
    pub(super) effort_override: Option<EncodeEffort>,
    pub(super) animation: AnimLimits,
    pub(super) animation_loop_count: Option<u32>,
    pub(super) media_limits: &'limits MediaLimits,
    pub(super) full_canvas_animation: bool,
}

pub(super) fn encode_vips_image(
    image: &VipsImageHandle<'_>,
    request: VipsEncodeRequest<'_>,
) -> Result<Vec<u8>, MediaError> {
    let VipsEncodeRequest {
        format,
        quality,
        page_height,
        effort_override,
        animation,
        animation_loop_count,
        media_limits,
        full_canvas_animation,
    } = request;
    if format == OutputFormat::WebP && page_height.is_some_and(|ph| ph > 0) {
        return encode_animated_webp(AnimatedWebPEncodeRequest {
            media_limits,
            image,
            quality,
            effort_override,
            loop_count: resolve_animation_loop_count(image, animation_loop_count),
            limits: animation,
            full_canvas_frames: full_canvas_animation,
        });
    }
    if format == OutputFormat::APNG
        && let Some(ph) = page_height
        && ph > 0
    {
        return encode_animated_apng(image, ph, animation, media_limits, animation_loop_count);
    }
    if format == OutputFormat::GIF
        && let Some(frame_height) = page_height
        && frame_height > 0
    {
        apply_gif_animation_loop_count(
            image,
            resolve_animation_loop_count(image, animation_loop_count),
        )?;
        let suffix = output_suffix(format, quality, Some(frame_height), effort_override)?;
        return write_vips_image_to_vec(
            image,
            &suffix,
            constants::MAX_MEDIA_PROXY_BYTES,
            animation.deadline_unix_ms,
        );
    }
    let suffix = output_suffix(format, quality, page_height, effort_override)?;
    write_vips_image_to_vec(
        image,
        &suffix,
        constants::MAX_MEDIA_PROXY_BYTES,
        animation.deadline_unix_ms,
    )
}

pub(super) fn resolve_animation_loop_count(
    image: &VipsImageHandle<'_>,
    carried: Option<u32>,
) -> u32 {
    if let Some(loop_count) = carried {
        return loop_count;
    }
    let field = c"loop";
    let has_field =
        unsafe { native::fluxer_vips_image_has_field(image.as_ptr(), field.as_ptr()) } != 0;
    if !has_field {
        return 0;
    }
    let mut loop_count: c_int = 0;
    let rc = unsafe {
        native::fluxer_vips_image_get_int(image.as_ptr(), field.as_ptr(), &mut loop_count)
    };
    if rc != 0 {
        clear_vips_error();
        return 0;
    }
    u32::try_from(loop_count).unwrap_or(0)
}

struct AnimatedWebPEncodeRequest<'request, 'source> {
    media_limits: &'request MediaLimits,
    image: &'request VipsImageHandle<'source>,
    quality: ResolvedImageQuality,
    effort_override: Option<EncodeEffort>,
    loop_count: u32,
    limits: AnimLimits,
    full_canvas_frames: bool,
}

fn webp_animation_limits(limits: AnimLimits) -> native::WebpAnimLimits {
    native::WebpAnimLimits {
        max_frames: limits.max_frames.min(c_int::MAX as u32) as c_int,
        max_duration_ms: limits.max_duration_ms.min(c_int::MAX as u32) as c_int,
        deadline_monotonic_ms: limits.deadline_unix_ms.unwrap_or(0),
    }
}

fn encode_animated_webp(request: AnimatedWebPEncodeRequest<'_, '_>) -> Result<Vec<u8>, MediaError> {
    let AnimatedWebPEncodeRequest {
        media_limits,
        image,
        quality,
        effort_override,
        loop_count,
        limits,
        full_canvas_frames,
    } = request;
    let q = quality.encoder_quality();
    let effort = effort_override
        .map(EncodeEffort::get)
        .unwrap_or_else(|| quality.default_effort(true));
    let c_limits = webp_animation_limits(limits);
    let loop_count = loop_count.min(WEBP_PLAY_COUNT_MAX) as u16;
    let runtime = vips_runtime()?;
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_webp_encode_animated(
            image.as_ptr(),
            q as c_int,
            if quality.is_lossless() { 1 } else { 0 },
            effort as c_int,
            90,
            1,
            runtime.config().webp_thread_level(),
            c_int::from(loop_count),
            if full_canvas_frames { 1 } else { 0 },
            &c_limits,
            media_limits.max_media_proxy_bytes(),
            &mut out_ptr,
            &mut out_size,
        )
    };
    let output = unsafe { NativeBuffer::from_webp_owned(out_ptr, out_size) };
    match NativeStatus::from_code(rc) {
        NativeStatus::Ok => {}
        NativeStatus::DeadlineExceeded => {
            clear_vips_error();
            return Err(MediaError::RequestTimeout);
        }
        NativeStatus::WorkLimitExceeded | NativeStatus::OutputLimitExceeded => {
            clear_vips_error();
            return Err(MediaError::StreamTooLong);
        }
        NativeStatus::AllocationFailed => {
            clear_vips_error();
            return Err(MediaError::AllocationFailed);
        }
        NativeStatus::Unsupported
        | NativeStatus::InvalidDimensions
        | NativeStatus::CodecFailure => {
            clear_vips_error();
            return Err(MediaError::MediaEncodeFailed);
        }
    }
    let out = match output {
        Some(out) => out,
        None => {
            clear_vips_error();
            return Err(MediaError::MediaEncodeFailed);
        }
    };
    native_buffer_to_media_bytes(out)?.try_into_vec()
}

pub(super) fn try_transform_animated_webp_direct(
    input: &[u8],
    options: &ImageOptions,
    quality: ResolvedImageQuality,
    effort_override: Option<EncodeEffort>,
    media_limits: &MediaLimits,
) -> Result<Option<Vec<u8>>, MediaError> {
    let effort = effort_override
        .map(EncodeEffort::get)
        .unwrap_or_else(|| quality.default_effort(true));
    let c_limits = webp_animation_limits(anim_limits_from_options(options));
    let runtime = vips_runtime()?;
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_webp_transform_animated(
            input.as_ptr().cast(),
            input.len(),
            native_optional_dimension(options.width)?,
            native_optional_dimension(options.height)?,
            quality.encoder_quality() as c_int,
            if quality.is_lossless() { 1 } else { 0 },
            effort as c_int,
            90,
            1,
            runtime.config().webp_thread_level(),
            native_animated_frame_limit(media_limits),
            media_limits.animated_total_pixels(),
            &c_limits,
            media_limits.max_media_proxy_bytes(),
            &mut out_ptr,
            &mut out_size,
        )
    };
    let output = unsafe { NativeBuffer::from_webp_owned(out_ptr, out_size) };
    match NativeStatus::from_code(rc) {
        NativeStatus::Ok => {}
        NativeStatus::Unsupported => {
            clear_vips_error();
            return Ok(None);
        }
        NativeStatus::DeadlineExceeded => {
            clear_vips_error();
            return Err(MediaError::RequestTimeout);
        }
        NativeStatus::WorkLimitExceeded | NativeStatus::OutputLimitExceeded => {
            clear_vips_error();
            return Err(MediaError::StreamTooLong);
        }
        NativeStatus::InvalidDimensions => {
            clear_vips_error();
            return Err(MediaError::InvalidImageDimensions);
        }
        NativeStatus::AllocationFailed => {
            clear_vips_error();
            return Err(MediaError::AllocationFailed);
        }
        NativeStatus::CodecFailure => {
            clear_vips_error();
            return Err(MediaError::MediaTransformFailed);
        }
    }
    let out = match output {
        Some(out) => out,
        None => {
            clear_vips_error();
            return Err(MediaError::MediaTransformFailed);
        }
    };
    native_buffer_to_media_bytes(out)?.try_into_vec().map(Some)
}
