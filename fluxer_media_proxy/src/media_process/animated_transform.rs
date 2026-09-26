// SPDX-License-Identifier: AGPL-3.0-or-later

use super::image_probe::animated_probe_from_image;
use super::loaded_image::validate_dimensions;
use super::native_runtime::{clear_vips_error, vips_runtime};
use super::{
    MediaError, native_animated_frame_limit, native_buffer_to_media_bytes, native_status_error,
};
use crate::{
    image_transform::ImageOptions,
    media_limits::MediaLimits,
    metrics::transform::TransformMetrics,
    mime,
    native::{self, NativeStatus, VipsImageHandle, buffer::NativeBuffer},
};
use libc::{c_int, c_void, size_t};
use std::ptr;

pub(super) struct DecodedHEIFPrimaryStill {
    pub(super) image: VipsImageHandle<'static>,
    pub(super) hdr_tone_mapped: bool,
}

pub(super) struct DecodedAnimation {
    pub(super) image: VipsImageHandle<'static>,
    pub(super) loop_count: Option<u32>,
}

pub(super) fn try_decode_heif_primary_still(
    input: &[u8],
    media_limits: &MediaLimits,
    metrics: &TransformMetrics,
) -> Result<Option<DecodedHEIFPrimaryStill>, MediaError> {
    let mut raw = ptr::null_mut();
    let mut facts = native::FluxerHEIFPrimaryStillDecodeFacts::empty();
    let rc = unsafe {
        native::fluxer_heif_decode_primary_still(
            input.as_ptr().cast(),
            input.len(),
            0,
            &mut raw,
            media_limits.image_pixels(),
            c_int::try_from(media_limits.image_dimension())
                .map_err(|_| MediaError::InvalidImageDimensions)?,
            &mut facts,
        )
    };
    let image = unsafe { VipsImageHandle::from_raw_owned(raw) };
    match NativeStatus::from_code(rc) {
        NativeStatus::Ok if image.is_some() => {}
        NativeStatus::Ok | NativeStatus::Unsupported | NativeStatus::CodecFailure => {
            clear_vips_error();
            metrics.record_heif_primary_still_decode_failure();
            return Ok(None);
        }
        status => {
            clear_vips_error();
            metrics.record_heif_primary_still_decode_failure();
            return Err(native_status_error(status, MediaError::MediaDecodeFailed)
                .expect("non-success native status must map to an error"));
        }
    }
    metrics.record_heif_primary_still_decode();
    if facts.hdr_gain_map_detected != 0 {
        metrics.record_heif_hdr_gain_map_detected();
    }
    let image = image.ok_or(MediaError::MediaDecodeFailed)?;
    Ok(Some(DecodedHEIFPrimaryStill {
        image,
        hdr_tone_mapped: facts.hdr_tone_mapped != 0,
    }))
}

pub(super) fn try_decode_apng(
    input: &[u8],
    animated: bool,
    media_limits: &MediaLimits,
) -> Result<Option<DecodedAnimation>, MediaError> {
    let runtime = vips_runtime()?;
    let mut raw = ptr::null_mut();
    let mut num_plays = 0u32;
    let rc = unsafe {
        native::fluxer_ffmpeg_decode_apng(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            0,
            &mut raw,
            if animated {
                native_animated_frame_limit(media_limits)
            } else {
                1
            },
            media_limits.animated_total_pixels(),
            if animated { 1 } else { 0 },
            &mut num_plays,
        )
    };
    let image = unsafe { VipsImageHandle::from_raw_owned(raw) };
    let status = NativeStatus::from_code(rc);
    match status {
        NativeStatus::Ok if image.is_some() => {
            let image = image.ok_or(MediaError::MediaDecodeFailed)?;
            Ok(Some(DecodedAnimation {
                image,
                loop_count: Some(num_plays),
            }))
        }
        NativeStatus::Ok | NativeStatus::Unsupported | NativeStatus::CodecFailure => {
            clear_vips_error();
            if animated {
                return Err(MediaError::MediaDecodeFailed);
            }
            Ok(None)
        }
        status => {
            clear_vips_error();
            Err(native_status_error(status, MediaError::MediaDecodeFailed)
                .expect("non-success native status must map to an error"))
        }
    }
}

pub(super) fn decode_heif_sequence(
    input: &[u8],
    media_limits: &MediaLimits,
) -> Result<Option<DecodedAnimation>, MediaError> {
    let runtime = vips_runtime()?;
    let mut raw = ptr::null_mut();
    let mut frame_count = 0;
    let rc = unsafe {
        native::fluxer_ffmpeg_decode_heif_sequence(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            0,
            &mut raw,
            native_animated_frame_limit(media_limits),
            media_limits.animated_total_pixels(),
            &mut frame_count,
        )
    };
    let image = unsafe { VipsImageHandle::from_raw_owned(raw) };
    match NativeStatus::from_code(rc) {
        NativeStatus::Ok if image.is_some() => {}
        NativeStatus::Ok | NativeStatus::Unsupported | NativeStatus::CodecFailure => {
            clear_vips_error();
            return Ok(None);
        }
        status => {
            clear_vips_error();
            return Err(native_status_error(status, MediaError::MediaDecodeFailed)
                .expect("non-success native status must map to an error"));
        }
    }
    let image = image.ok_or(MediaError::MediaDecodeFailed)?;
    let frame_count = u32::try_from(frame_count).map_err(|_| MediaError::MediaDecodeFailed)?;
    if frame_count < 2 {
        return Ok(None);
    }
    let probe = animated_probe_from_image(&image)?.ok_or(MediaError::MediaDecodeFailed)?;
    let pages = u32::try_from(probe.pages).map_err(|_| MediaError::MediaDecodeFailed)?;
    if pages != frame_count {
        return Err(MediaError::MediaDecodeFailed);
    }
    validate_dimensions(media_limits, probe.width, probe.height)?;
    Ok(Some(DecodedAnimation {
        image,
        loop_count: None,
    }))
}

#[derive(Clone, Copy, Debug)]
pub(super) struct GifResizeDims {
    pub(super) width: c_int,
    pub(super) height: c_int,
}

pub(super) fn gif_resize_dims(
    sniffed: mime::SniffInfo,
    options: &ImageOptions,
) -> Option<GifResizeDims> {
    if sniffed.width == 0 || sniffed.height == 0 {
        return None;
    }
    let src_w = sniffed.width;
    let src_h = sniffed.height;
    let scale = match (options.width, options.height) {
        (Some(0), _) | (_, Some(0)) => return None,
        (Some(w), Some(h)) => (w as f64 / src_w as f64).min(h as f64 / src_h as f64),
        (Some(w), None) => w as f64 / src_w as f64,
        (None, Some(h)) => h as f64 / src_h as f64,
        (None, None) => return None,
    }
    .min(1.0);
    let target_w = ((src_w as f64) * scale).round().max(1.0) as u32;
    let target_h = ((src_h as f64) * scale).round().max(1.0) as u32;
    if target_w == src_w && target_h == src_h {
        return None;
    }
    Some(GifResizeDims {
        width: target_w as c_int,
        height: target_h as c_int,
    })
}

pub(super) fn resize_animated_gif_with_ffmpeg(
    input: &[u8],
    dims: GifResizeDims,
    options: &ImageOptions,
    media_limits: &MediaLimits,
) -> Result<Vec<u8>, MediaError> {
    let runtime = vips_runtime()?;
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let mut out_capacity: size_t = 0;
    let rc = unsafe {
        native::fluxer_ffmpeg_resize_gif(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            dims.width,
            dims.height,
            options.deadline_ms.unwrap_or(0),
            native_animated_frame_limit(media_limits),
            // The GIF fast path resizes without re-timing, so the encode budget must never bind
            // tighter than the decode budget. Capping it here silently dropped the tail of any
            // animation longer than FLUXER_MEDIA_PROXY_MAX_ENCODE_DURATION_MS.
            native_animated_frame_limit(media_limits),
            c_int::MAX,
            media_limits.animated_total_pixels(),
            media_limits.max_media_proxy_bytes(),
            &mut out_ptr,
            &mut out_size,
            &mut out_capacity,
        )
    };
    let output =
        unsafe { NativeBuffer::from_av_owned_with_resident_bytes(out_ptr, out_size, out_capacity) };
    match NativeStatus::from_code(rc) {
        NativeStatus::Ok => {}
        NativeStatus::DeadlineExceeded => return Err(MediaError::RequestTimeout),
        NativeStatus::WorkLimitExceeded | NativeStatus::OutputLimitExceeded => {
            return Err(MediaError::StreamTooLong);
        }
        NativeStatus::AllocationFailed => return Err(MediaError::AllocationFailed),
        NativeStatus::Unsupported
        | NativeStatus::InvalidDimensions
        | NativeStatus::CodecFailure => {
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
    native_buffer_to_media_bytes(out)?.try_into_vec()
}
