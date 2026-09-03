// SPDX-License-Identifier: AGPL-3.0-or-later

use super::loaded_image::{page_height, validate_animation_geometry, validate_vips_image};
use super::native_runtime::{clear_vips_error, ensure_vips_init, vips_runtime};
use super::{AnimatedProbe, ImageDimensions, MediaError, native_status_error};
use crate::{
    media_limits::MediaLimits,
    mime,
    native::{self, NativeStatus, VipsImageHandle},
};
use libc::c_int;
use std::{ffi::CString, ptr};

pub(crate) fn probe_animated(
    media_limits: &MediaLimits,
    input: &[u8],
) -> Result<Option<AnimatedProbe>, MediaError> {
    let mut width = 0;
    let mut height = 0;
    let mut pages = 0;
    let rc = unsafe {
        native::fluxer_vips_probe_animated(
            input.as_ptr().cast(),
            input.len(),
            &mut width,
            &mut height,
            &mut pages,
        )
    };
    match native::NativeStatus::from_code(rc) {
        native::NativeStatus::Ok => {}
        native::NativeStatus::Unsupported | native::NativeStatus::CodecFailure => {
            clear_vips_error();
            return Ok(None);
        }
        status => {
            clear_vips_error();
            return Err(native_status_error(status, MediaError::MediaDecodeFailed)
                .expect("non-success native status must map to an error"));
        }
    }
    if pages <= 0 || width <= 0 || height <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    validate_animation_geometry(media_limits, width as u32, height as u32, pages as u32)?;
    Ok(Some(AnimatedProbe {
        width,
        height,
        pages,
    }))
}

pub(crate) fn probe_image_dims(
    media_limits: &MediaLimits,
    input: &[u8],
) -> Result<ImageDimensions, MediaError> {
    ensure_vips_init()?;
    if let Some(probe) = probe_animated(media_limits, input)? {
        return Ok(ImageDimensions {
            width: probe.width as u32,
            height: probe.height as u32,
            pages: probe.pages as u32,
        });
    }
    if mime::sniff(input).mime == "image/bmp"
        && let Some(image) = try_decode_bmp(input, media_limits)?
    {
        return probe_loaded_image_dims(media_limits, &image);
    }
    let image = load_image(input, "access=sequential")?;
    probe_loaded_image_dims(media_limits, &image)
}

fn probe_loaded_image_dims(
    media_limits: &MediaLimits,
    image: &native::VipsImageHandle<'_>,
) -> Result<ImageDimensions, MediaError> {
    validate_vips_image(media_limits, image)?;
    let mut width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) as u32 };
    let mut height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) as u32 };
    if unsafe { native::fluxer_vips_image_get_orientation_swap(image.as_ptr()) } != 0 {
        std::mem::swap(&mut width, &mut height);
    }
    let pages = match page_count(image)? {
        Some(pages) => u32::try_from(pages).map_err(|_| MediaError::InvalidImageDimensions)?,
        None => 1,
    };
    Ok(ImageDimensions {
        width,
        height,
        pages,
    })
}

pub(crate) fn load_image<'source>(
    input: &'source [u8],
    options: &str,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    let options = CString::new(options).map_err(|_| MediaError::MediaDecodeFailed)?;
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(
            input.as_ptr().cast(),
            input.len(),
            options.as_ptr(),
        )
    };
    unsafe { native::VipsImageHandle::from_raw_borrowing(raw, input) }.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaDecodeFailed
    })
}

fn page_count(image: &native::VipsImageHandle<'_>) -> Result<Option<c_int>, MediaError> {
    let Some(page_height) = page_height(image)? else {
        return Ok(None);
    };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    Ok(Some(height / page_height))
}

pub(crate) fn animated_probe_from_image(
    image: &native::VipsImageHandle<'_>,
) -> Result<Option<AnimatedProbe>, MediaError> {
    let Some(page_height) = page_height(image)? else {
        return Ok(None);
    };
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let pages = page_count(image)?.ok_or(MediaError::InvalidImageDimensions)?;
    if width <= 0 || pages <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(Some(AnimatedProbe {
        width,
        height: page_height,
        pages,
    }))
}

pub(super) fn try_decode_bmp(
    input: &[u8],
    media_limits: &MediaLimits,
) -> Result<Option<VipsImageHandle<'static>>, MediaError> {
    let runtime = vips_runtime()?;
    let mut raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_ffmpeg_decode_bmp(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            0,
            &mut raw,
            media_limits.image_pixels(),
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
    Ok(Some(image.ok_or(MediaError::MediaDecodeFailed)?))
}
