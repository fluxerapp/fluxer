// SPDX-License-Identifier: AGPL-3.0-or-later

mod av;
mod gif;
mod heif;
mod jpeg;
mod png;
mod webp;

pub use av::{AvContainer, audio_duration_seconds, av_container, strip_av_metadata};

use super::image_probe::{load_image, probe_image_dims};
use super::loaded_image::validate_vips_image;
use super::native_runtime::{clear_vips_error, ensure_vips_init};
use super::{MediaError, ensure_deadline_pending};
use crate::{
    constants,
    media_limits::MediaLimits,
    mime,
    native::{self, buffer::NativeBuffer},
};
use std::ptr;

const TIFF_SUFFIX: &std::ffi::CStr = c".tif[compression=jpeg,Q=80,predictor=horizontal,strip]";

pub struct StrippedImage {
    pub bytes: Vec<u8>,
    pub content_type: &'static str,
    pub width: u32,
    pub height: u32,
}

fn reencode_tiff(
    input: &[u8],
    media_limits: &MediaLimits,
    deadline_ms: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    let image = load_image(input, "n=-1")?;
    validate_vips_image(media_limits, &image)?;
    let image = jpeg::srgb_without_alpha(image)?;
    ensure_deadline_pending(deadline_ms)?;
    let mut buffer = ptr::null_mut();
    let mut size = 0;
    let rc = unsafe {
        native::fluxer_vips_image_write_to_buffer(
            image.as_ptr(),
            TIFF_SUFFIX.as_ptr(),
            &mut buffer,
            &mut size,
        )
    };
    let encoded = unsafe { NativeBuffer::from_vips_owned(buffer, size) };
    if rc != 0 {
        clear_vips_error();
        return Err(MediaError::MediaEncodeFailed);
    }
    let encoded = encoded.ok_or(MediaError::MediaEncodeFailed)?;
    if encoded.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    encoded
        .try_to_vec()
        .map_err(|_| MediaError::AllocationFailed)
}

pub fn strips_image(sniffed_mime: &str) -> bool {
    matches!(
        sniffed_mime,
        "image/jpeg"
            | "image/png"
            | "image/apng"
            | "image/webp"
            | "image/gif"
            | "image/avif"
            | "image/heic"
            | "image/heif"
            | "image/tiff"
    )
}

pub fn strip_image_metadata(
    input: &[u8],
    media_limits: &MediaLimits,
    deadline_ms: Option<i64>,
) -> Result<StrippedImage, MediaError> {
    ensure_vips_init()?;
    let sniffed = mime::sniff(input).mime;
    if !strips_image(sniffed) {
        return Err(MediaError::UnsupportedMediaType);
    }
    let dims = probe_image_dims(media_limits, input)?;
    let bytes = match sniffed {
        "image/jpeg" => {
            let upright = jpeg::orientation(&load_image(input, "")?) <= 1;
            if upright {
                jpeg::strip_segments(input)?
            } else {
                jpeg::reencode_upright(input, media_limits, deadline_ms)?
            }
        }
        "image/png" | "image/apng" => png::strip_chunks(input)?,
        "image/webp" => webp::strip_chunks(input)?,
        "image/gif" => gif::strip_blocks(input)?,
        "image/avif" | "image/heic" | "image/heif" => heif::strip_items(input)?,
        _ => reencode_tiff(input, media_limits, deadline_ms)?,
    };
    Ok(StrippedImage {
        bytes,
        content_type: sniffed,
        width: dims.width,
        height: dims.height,
    })
}
