// SPDX-License-Identifier: AGPL-3.0-or-later

use super::native_runtime::clear_vips_error;
use super::transform::{StaticThumbnailRequest, load_static_thumbnail};
use super::{MediaError, ensure_deadline_pending, native_optional_deadline, native_status_error};
use crate::{
    media_limits::MediaLimits,
    native::{self, NativeStatus, VipsImageHandle, buffer::NativeBuffer},
    thumbhash,
};
use base64::{Engine as _, engine::general_purpose};
use libc::{c_void, size_t};
use std::ptr;

pub(super) fn encode_thumbhash(
    media_limits: &MediaLimits,
    input: &[u8],
    deadline_ms: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    let image = load_static_thumbnail(StaticThumbnailRequest {
        media_limits,
        input,
        width: thumbhash::MAX_DIM,
        height: thumbhash::MAX_DIM,
        deadline_ms,
    })?;
    encode_thumbhash_image(&image, deadline_ms)
}

pub(super) fn encode_thumbhash_image(
    image: &VipsImageHandle<'_>,
    deadline_ms: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    ensure_deadline_pending(deadline_ms)?;
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) as u32 };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) as u32 };
    if width == 0 || height == 0 || width > thumbhash::MAX_DIM || height > thumbhash::MAX_DIM {
        return Err(MediaError::InvalidImageDimensions);
    }
    let mut rgba_ptr: *mut c_void = ptr::null_mut();
    let mut rgba_size: size_t = 0;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_extract_rgba(
            image.as_ptr(),
            native_optional_deadline(deadline_ms),
            &mut rgba_ptr,
            &mut rgba_size,
        )
    });
    let rgba = unsafe { NativeBuffer::from_vips_owned(rgba_ptr, rgba_size) };
    if let Some(error) = native_status_error(status, MediaError::MediaTransformFailed) {
        clear_vips_error();
        return Err(error);
    }
    let rgba = rgba.ok_or(MediaError::MediaTransformFailed)?;
    thumbhash::encode_rgba(rgba.as_slice(), width, height)
        .map_err(|_| MediaError::InvalidImageDimensions)
}

pub(super) fn optional_thumbhash(
    result: Result<Vec<u8>, MediaError>,
    source: &'static str,
) -> Option<Vec<u8>> {
    match result {
        Ok(hash) => Some(hash),
        Err(error) => {
            tracing::warn!("optional {source} placeholder generation failed: {error:?}");
            None
        }
    }
}

pub(super) fn encoded_placeholder(hash: Option<Vec<u8>>) -> Option<String> {
    hash.map(|hash| general_purpose::STANDARD.encode(hash))
}
