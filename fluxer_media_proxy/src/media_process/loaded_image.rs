// SPDX-License-Identifier: AGPL-3.0-or-later

use super::native_runtime::clear_vips_error;
use super::{MediaError, native_status_error};
use crate::{
    image_transform::{ImageOptions, ResizeMode},
    media_limits::MediaLimits,
    native,
};
use libc::c_int;
use std::ptr;

const VIPS_IMAGE_BANDS_MAX: c_int = 4;
const RESIZE_SCALE_IDENTITY_THRESHOLD: f64 = 0.999;

pub(crate) fn page_height(
    image: &native::VipsImageHandle<'_>,
) -> Result<Option<c_int>, MediaError> {
    let field = c"page-height";
    let has_page_height =
        unsafe { native::fluxer_vips_image_has_field(image.as_ptr(), field.as_ptr()) } != 0;
    if !has_page_height {
        return Ok(None);
    }
    let mut page_height = 0;
    let rc = unsafe {
        native::fluxer_vips_image_get_int(image.as_ptr(), field.as_ptr(), &mut page_height)
    };
    if rc != 0 {
        clear_vips_error();
        return Err(MediaError::InvalidImageDimensions);
    }
    if page_height <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    if total_height <= 0 || total_height % page_height != 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(Some(page_height))
}

pub(crate) fn validate_dimensions_u32(
    media_limits: &MediaLimits,
    width: u32,
    height: u32,
) -> Result<(), MediaError> {
    let max_dimension = media_limits.image_dimension();
    if width == 0 || height == 0 || width > max_dimension || height > max_dimension {
        return Err(MediaError::InvalidImageDimensions);
    }
    if width as usize * height as usize > media_limits.image_pixels() {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(())
}

pub(crate) fn validate_dimensions(
    media_limits: &MediaLimits,
    width: c_int,
    height: c_int,
) -> Result<(), MediaError> {
    if width <= 0 || height <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    validate_dimensions_u32(media_limits, width as u32, height as u32)
}

pub(super) fn validate_animation_geometry(
    media_limits: &MediaLimits,
    width: u32,
    height: u32,
    frames: u32,
) -> Result<(), MediaError> {
    validate_dimensions_u32(media_limits, width, height)?;
    if frames == 0 || frames > media_limits.animated_frames() {
        return Err(MediaError::InvalidImageDimensions);
    }
    let frame_pixels = width as usize * height as usize;
    if frames > 1 && frame_pixels > media_limits.animated_total_pixels() / frames as usize {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(())
}

pub(crate) fn validate_vips_image(
    media_limits: &MediaLimits,
    image: &native::VipsImageHandle<'_>,
) -> Result<(), MediaError> {
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    match page_height(image)? {
        Some(page_height) => {
            validate_dimensions(media_limits, width, page_height)?;
            let frames = height / page_height;
            let width = usize::try_from(width).map_err(|_| MediaError::InvalidImageDimensions)?;
            let page_height =
                usize::try_from(page_height).map_err(|_| MediaError::InvalidImageDimensions)?;
            let frames = u32::try_from(frames).map_err(|_| MediaError::InvalidImageDimensions)?;
            if frames > media_limits.animated_frames() {
                return Err(MediaError::InvalidImageDimensions);
            }
            let total_pixels = width
                .checked_mul(page_height)
                .and_then(|pixels| pixels.checked_mul(frames as usize))
                .ok_or(MediaError::InvalidImageDimensions)?;
            if total_pixels > media_limits.animated_total_pixels() {
                return Err(MediaError::InvalidImageDimensions);
            }
        }
        None => validate_dimensions(media_limits, width, height)?,
    }
    let bands = unsafe { native::fluxer_vips_image_get_bands(image.as_ptr()) };
    if bands <= 0 || bands > VIPS_IMAGE_BANDS_MAX {
        return Err(MediaError::InvalidImageDimensions);
    }
    let format = unsafe { native::fluxer_vips_image_get_format(image.as_ptr()) };
    let supported_format = unsafe {
        format == native::fluxer_vips_format_uchar
            || format == native::fluxer_vips_format_ushort
            || format == native::fluxer_vips_format_float
    };
    if !supported_format {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(())
}

pub(crate) fn normalize_vips_image_to_uchar<'source>(
    media_limits: &MediaLimits,
    image: native::VipsImageHandle<'source>,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    validate_vips_image(media_limits, &image)?;
    let format = unsafe { native::fluxer_vips_image_get_format(image.as_ptr()) };
    if format == unsafe { native::fluxer_vips_format_uchar } {
        return Ok(image);
    }
    let mut normalized_raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_image_to_rgba(image.as_ptr(), &mut normalized_raw) };
    let normalized = unsafe { image.adopt_derived_raw(normalized_raw) };
    if let Some(error) = native_status_error(
        native::NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    drop(image);
    let normalized = normalized.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })?;
    validate_vips_image(media_limits, &normalized)?;
    let normalized_format = unsafe { native::fluxer_vips_image_get_format(normalized.as_ptr()) };
    if normalized_format != unsafe { native::fluxer_vips_format_uchar } {
        return Err(MediaError::MediaTransformFailed);
    }
    Ok(normalized)
}

pub(crate) fn resize_loaded_image<'source>(
    media_limits: &MediaLimits,
    image: native::VipsImageHandle<'source>,
    options: &ImageOptions,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    let (Some(target_width), Some(target_height)) = (options.width, options.height) else {
        return resize_loaded_image_fit_inside(media_limits, image, options);
    };
    if options.animation.is_animated() || options.resize_mode == ResizeMode::Fit {
        return resize_loaded_image_fit_inside(media_limits, image, options);
    }

    let source_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let source_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    validate_dimensions(media_limits, source_width, source_height)?;

    let scale_width = target_width as f64 / source_width as f64;
    let scale_height = target_height as f64 / source_height as f64;
    let scale = scale_width.max(scale_height).min(1.0);
    let current = resize_loaded_image_by_scale(media_limits, image, scale)?;
    let scaled_width = unsafe { native::fluxer_vips_image_get_width(current.as_ptr()) };
    let scaled_height = unsafe { native::fluxer_vips_image_get_height(current.as_ptr()) };
    validate_dimensions(media_limits, scaled_width, scaled_height)?;

    let final_width = scaled_width.min(target_width as c_int);
    let final_height = scaled_height.min(target_height as c_int);
    if final_width == scaled_width && final_height == scaled_height {
        return Ok(current);
    }

    let had_page_height = page_height(&current)?.is_some();
    let left = (scaled_width - final_width) / 2;
    let top = (scaled_height - final_height) / 2;
    let cropped = extract_vips_image_area(&current, left, top, final_width, final_height)?;
    drop(current);
    if had_page_height {
        unsafe { native::fluxer_vips_set_page_height(cropped.as_ptr(), final_height) };
    }
    validate_vips_image(media_limits, &cropped)?;
    Ok(cropped)
}

fn resize_loaded_image_fit_inside<'source>(
    media_limits: &MediaLimits,
    image: native::VipsImageHandle<'source>,
    options: &ImageOptions,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    if options.width.is_none() && options.height.is_none() {
        return Ok(image);
    }
    let source_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    let source_height = if options.animation.is_animated() {
        page_height(&image)?.unwrap_or(total_height)
    } else {
        total_height
    };
    validate_dimensions(media_limits, source_width, source_height)?;

    let scale = match (options.width, options.height) {
        (Some(width), Some(height)) => {
            (width as f64 / source_width as f64).min(height as f64 / source_height as f64)
        }
        (Some(width), None) => width as f64 / source_width as f64,
        (None, Some(height)) => height as f64 / source_height as f64,
        (None, None) => unreachable!("dimension presence is checked before the scale calculation"),
    }
    .min(1.0);
    resize_loaded_image_by_scale(media_limits, image, scale)
}

pub(crate) fn resize_loaded_image_by_scale<'source>(
    media_limits: &MediaLimits,
    image: native::VipsImageHandle<'source>,
    scale: f64,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    assert!(scale.is_finite());
    assert!(scale > 0.0);
    assert!(scale <= 1.0);
    if scale >= RESIZE_SCALE_IDENTITY_THRESHOLD {
        return Ok(image);
    }
    let old_page_height = page_height(&image)?;
    if let Some(old_page_height) = old_page_height {
        let old_total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
        let page_count = old_total_height / old_page_height;
        if page_count > 1 {
            validate_vips_image(media_limits, &image)?;
            return resize_loaded_animation_pages_by_scale(
                media_limits,
                image,
                scale,
                old_page_height,
                page_count,
            );
        }
    }
    let resized = resize_vips_image(&image, scale)?;
    drop(image);
    if old_page_height.is_some() {
        let new_total_height = unsafe { native::fluxer_vips_image_get_height(resized.as_ptr()) };
        if new_total_height <= 0 {
            return Err(MediaError::InvalidImageDimensions);
        }
        unsafe { native::fluxer_vips_set_page_height(resized.as_ptr(), new_total_height) };
    }
    validate_vips_image(media_limits, &resized)?;
    Ok(resized)
}

fn resize_loaded_animation_pages_by_scale<'source>(
    media_limits: &MediaLimits,
    image: native::VipsImageHandle<'source>,
    scale: f64,
    old_page_height: c_int,
    page_count: c_int,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    assert!(old_page_height > 0);
    assert!(page_count > 1);
    let source_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    validate_dimensions(media_limits, source_width, old_page_height)?;
    let page_count_usize =
        usize::try_from(page_count).map_err(|_| MediaError::InvalidImageDimensions)?;
    let mut resized_pages = Vec::new();
    resized_pages
        .try_reserve_exact(page_count_usize)
        .map_err(|_| MediaError::AllocationFailed)?;
    let mut resized_page_width = 0;
    let mut resized_page_height = 0;
    for page_index in 0..page_count {
        let top = page_index
            .checked_mul(old_page_height)
            .ok_or(MediaError::InvalidImageDimensions)?;
        let page = extract_vips_image_area(&image, 0, top, source_width, old_page_height)?;
        let resized_page = resize_vips_image(&page, scale)?;
        drop(page);
        let page_width = unsafe { native::fluxer_vips_image_get_width(resized_page.as_ptr()) };
        let page_height = unsafe { native::fluxer_vips_image_get_height(resized_page.as_ptr()) };
        validate_dimensions(media_limits, page_width, page_height)?;
        if page_index == 0 {
            resized_page_width = page_width;
            resized_page_height = page_height;
        } else if page_width != resized_page_width || page_height != resized_page_height {
            return Err(MediaError::InvalidImageDimensions);
        }
        resized_pages.push(resized_page);
    }
    assert_eq!(resized_pages.len(), page_count_usize);
    let output_width =
        u32::try_from(resized_page_width).map_err(|_| MediaError::InvalidImageDimensions)?;
    let output_page_height =
        u32::try_from(resized_page_height).map_err(|_| MediaError::InvalidImageDimensions)?;
    let output_page_count =
        u32::try_from(page_count).map_err(|_| MediaError::InvalidImageDimensions)?;
    validate_animation_geometry(
        media_limits,
        output_width,
        output_page_height,
        output_page_count,
    )?;
    let expected_total_height = resized_page_height
        .checked_mul(page_count)
        .ok_or(MediaError::InvalidImageDimensions)?;
    let mut page_pointers = Vec::new();
    page_pointers
        .try_reserve_exact(page_count_usize)
        .map_err(|_| MediaError::AllocationFailed)?;
    page_pointers.extend(resized_pages.iter().map(native::VipsImageHandle::as_ptr));
    assert_eq!(page_pointers.len(), page_count_usize);
    let max_pages = c_int::try_from(media_limits.animated_frames())
        .expect("validated animated frame limit must fit c_int");
    let mut joined_raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_join_animation_pages(
            image.as_ptr(),
            page_pointers.as_mut_ptr(),
            page_count,
            max_pages,
            media_limits.animated_total_pixels(),
            &mut joined_raw,
        )
    };
    let joined = unsafe { image.adopt_derived_raw(joined_raw) };
    if let Some(error) = native_status_error(
        native::NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    drop(resized_pages);
    drop(image);
    let joined = joined.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })?;
    let joined_width = unsafe { native::fluxer_vips_image_get_width(joined.as_ptr()) };
    let joined_total_height = unsafe { native::fluxer_vips_image_get_height(joined.as_ptr()) };
    if joined_width != resized_page_width || joined_total_height != expected_total_height {
        return Err(MediaError::InvalidImageDimensions);
    }
    if page_height(&joined)? != Some(resized_page_height) {
        return Err(MediaError::InvalidImageDimensions);
    }
    validate_vips_image(media_limits, &joined)?;
    Ok(joined)
}

fn extract_vips_image_area<'source>(
    image: &native::VipsImageHandle<'source>,
    left: c_int,
    top: c_int,
    width: c_int,
    height: c_int,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    let mut extracted_raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_extract_area(
            image.as_ptr(),
            &mut extracted_raw,
            left,
            top,
            width,
            height,
        )
    };
    let extracted = unsafe { image.adopt_derived_raw(extracted_raw) };
    if let Some(error) = native_status_error(
        native::NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    extracted.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })
}

fn resize_vips_image<'source>(
    image: &native::VipsImageHandle<'source>,
    scale: f64,
) -> Result<native::VipsImageHandle<'source>, MediaError> {
    assert!(scale.is_finite());
    assert!(scale > 0.0);
    assert!(scale < RESIZE_SCALE_IDENTITY_THRESHOLD);
    let mut resized_raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_resize(image.as_ptr(), &mut resized_raw, scale) };
    let resized = unsafe { image.adopt_derived_raw(resized_raw) };
    if let Some(error) = native_status_error(
        native::NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    resized.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })
}
