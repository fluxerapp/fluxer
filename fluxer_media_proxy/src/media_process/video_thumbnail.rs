// SPDX-License-Identifier: AGPL-3.0-or-later

use super::native_runtime::{clear_vips_error, vips_runtime};
use super::transform_plan::output_suffix;
use super::{MediaError, ProcessedMedia, native_buffer_to_media_bytes, native_status_error};
use crate::{
    constants,
    image_quality::ResolvedImageQuality,
    media_limits::MediaLimits,
    native::{self, NativeStatus, buffer::NativeBuffer},
    output_format::OutputFormat,
};
use libc::{c_int, c_void, size_t};
use std::ptr;

#[derive(Clone, Copy, Debug)]
pub struct VideoThumbnailOptions {
    pub format: OutputFormat,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub quality: ResolvedImageQuality,
    pub deadline_ms: Option<i64>,
}

pub fn extract_video_thumbnail(
    input: &[u8],
    format: OutputFormat,
    media_limits: &MediaLimits,
) -> Result<ProcessedMedia, MediaError> {
    extract_video_thumbnail_with_options(
        input,
        VideoThumbnailOptions {
            format,
            width: None,
            height: None,
            quality: ResolvedImageQuality::High,
            deadline_ms: None,
        },
        media_limits,
    )
}

pub fn extract_video_thumbnail_with_options(
    input: &[u8],
    options: VideoThumbnailOptions,
    media_limits: &MediaLimits,
) -> Result<ProcessedMedia, MediaError> {
    if input.len() > media_limits.max_media_proxy_bytes() {
        return Err(MediaError::StreamTooLong);
    }
    let VideoThumbnailOptions {
        format,
        width,
        height,
        quality,
        deadline_ms,
    } = options;
    let runtime = vips_runtime()?;
    let (max_width, max_height) = native_thumbnail_bounds(width, height, media_limits)?;
    let suffix = output_suffix(format, quality, None, None)?;
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let mut out_capacity: size_t = 0;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_video_thumbnail_ex(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            deadline_ms.unwrap_or(0),
            suffix.as_ptr(),
            constants::MAX_VIDEO_PACKETS_FOR_THUMBNAIL as c_int,
            max_width,
            max_height,
            media_limits.max_media_proxy_bytes(),
            ptr::null_mut(),
            ptr::null_mut(),
            &mut out_ptr,
            &mut out_size,
            &mut out_capacity,
        )
    });
    let output = unsafe {
        NativeBuffer::from_vips_owned_with_resident_bytes(out_ptr, out_size, out_capacity)
    };
    if let Some(error) = native_status_error(status, MediaError::MediaDecodeFailed) {
        clear_vips_error();
        return Err(error);
    }
    let output = match output {
        Some(output) => output,
        None => {
            clear_vips_error();
            return Err(MediaError::MediaDecodeFailed);
        }
    };
    let bytes = native_buffer_to_media_bytes(output)?.try_into_vec()?;
    Ok(ProcessedMedia {
        bytes,
        content_type: format.mime(),
    })
}

fn native_thumbnail_bounds(
    width: Option<u32>,
    height: Option<u32>,
    media_limits: &MediaLimits,
) -> Result<(c_int, c_int), MediaError> {
    if width.is_none() && height.is_none() {
        return Ok((0, 0));
    }
    let dimension_limit = media_limits.image_dimension();
    let max_width = width.unwrap_or(dimension_limit);
    let max_height = height.unwrap_or(dimension_limit);
    if max_width == 0
        || max_height == 0
        || max_width > dimension_limit
        || max_height > dimension_limit
    {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok((
        c_int::try_from(max_width).map_err(|_| MediaError::InvalidImageDimensions)?,
        c_int::try_from(max_height).map_err(|_| MediaError::InvalidImageDimensions)?,
    ))
}
