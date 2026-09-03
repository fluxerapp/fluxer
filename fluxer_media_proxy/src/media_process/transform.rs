// SPDX-License-Identifier: AGPL-3.0-or-later

use super::animated_transform::{
    DecodedAnimation, decode_heif_sequence, gif_resize_dims, resize_animated_gif_with_ffmpeg,
    try_decode_apng, try_decode_heif_primary_still,
};
use super::encoding::{
    VipsEncodeRequest, anim_limits_from_options, encode_vips_image,
    try_transform_animated_webp_direct,
};
use super::image_probe::{animated_probe_from_image, load_image, probe_animated, try_decode_bmp};
use super::loaded_image::{
    normalize_vips_image_to_uchar, page_height, resize_loaded_image, resize_loaded_image_by_scale,
    validate_dimensions_u32, validate_vips_image,
};
use super::native_runtime::{clear_vips_error, ensure_vips_init, last_vips_error};
use super::transform_plan::{
    AutoQualitySource, output_is_sdr, resolve_auto_quality, should_transform_animated_webp_direct,
};
use super::{
    AnimatedProbe, MediaError, ProcessedMedia, ensure_deadline_pending,
    native_animated_frame_limit, native_optional_deadline, native_status_error,
};
use crate::{
    constants,
    image_transform::ImageOptions,
    media_limits::MediaLimits,
    metrics::transform::TransformMetrics,
    mime,
    native::{self, NativeStatus, VipsImageHandle},
    output_format::OutputFormat,
};
use libc::c_int;
use std::ptr;

pub(super) fn source_supports_pages(mime: &str) -> bool {
    matches!(
        mime,
        "image/webp" | "image/gif" | "image/apng" | "image/heic" | "image/heif" | "image/avif"
    )
}

fn should_use_resize_path(options: &ImageOptions, probe: Option<AnimatedProbe>) -> bool {
    if options.width.is_none() && options.height.is_none() {
        return false;
    }
    if options.is_animated() {
        return true;
    }
    match probe {
        Some(p) => p.pages <= 1,
        None => true,
    }
}

fn effective_transform_format(
    sniffed_mime: &str,
    requested: OutputFormat,
    animated: bool,
) -> OutputFormat {
    if animated && sniffed_mime == "image/apng" && requested == OutputFormat::PNG {
        OutputFormat::APNG
    } else {
        requested
    }
}

fn is_heif_source(mime: &str) -> bool {
    matches!(mime, "image/avif" | "image/heic" | "image/heif")
}

fn heif_source_may_be_a_sequence(sniffed: mime::SniffInfo) -> bool {
    match sniffed.mime {
        "image/avif" => sniffed.animated,
        "image/heic" | "image/heif" => true,
        _ => false,
    }
}

pub fn transform_image(
    input: &[u8],
    options: &ImageOptions,
    media_limits: &MediaLimits,
    metrics: &TransformMetrics,
) -> Result<ProcessedMedia, MediaError> {
    if input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    if let Some(width) = options.width
        && (width == 0 || width > media_limits.image_dimension())
    {
        return Err(MediaError::InvalidImageDimensions);
    }
    if let Some(height) = options.height
        && (height == 0 || height > media_limits.image_dimension())
    {
        return Err(MediaError::InvalidImageDimensions);
    }
    ensure_vips_init()?;
    let sniffed = mime::sniff(input);
    let animated = options.is_animated();
    let format = effective_transform_format(sniffed.mime, options.format, animated);
    let animated_avif = sniffed.mime == "image/avif" && sniffed.animated;
    let full_canvas_animation = animated
        && format == OutputFormat::WebP
        && (matches!(sniffed.mime, "image/gif" | "image/apng") || animated_avif);
    if animated
        && format == OutputFormat::GIF
        && !options.wants_cover_crop()
        && sniffed.mime == "image/gif"
    {
        let bytes = if let Some(dims) = gif_resize_dims(sniffed, options) {
            resize_animated_gif_with_ffmpeg(input, dims, options, media_limits)?
        } else {
            input.to_vec()
        };
        return Ok(ProcessedMedia {
            bytes,
            content_type: "image/gif",
        });
    }
    if should_transform_animated_webp_direct(sniffed, options, format) {
        validate_dimensions_u32(media_limits, sniffed.width, sniffed.height)?;
        let resolved = resolve_auto_quality(
            AutoQualitySource {
                format,
                animated,
                sniffed_mime: sniffed.mime,
                input,
                quality: options.quality,
                probe: None,
            },
            options.effort_override,
        );
        let direct = try_transform_animated_webp_direct(
            input,
            options,
            resolved.quality,
            resolved.effort_override,
            media_limits,
        )?;
        if let Some(bytes) = direct {
            return Ok(ProcessedMedia {
                bytes,
                content_type: format.mime(),
            });
        }
    }
    let heif_source = is_heif_source(sniffed.mime);
    let decoded_animation = if sniffed.mime == "image/apng" {
        try_decode_apng(input, animated, media_limits)?
    } else if animated && heif_source_may_be_a_sequence(sniffed) {
        decode_heif_sequence(input, media_limits)?
    } else {
        None
    };
    let animated_probe = if let Some(decoded) = decoded_animation.as_ref() {
        animated_probe_from_image(&decoded.image)?
    } else if animated && !heif_source {
        probe_animated(media_limits, input)?
    } else {
        None
    };
    let resolved_quality = resolve_auto_quality(
        AutoQualitySource {
            format,
            animated,
            sniffed_mime: sniffed.mime,
            input,
            quality: options.quality,
            probe: animated_probe,
        },
        options.effort_override,
    );
    let requires_heif_sdr_decode = heif_source && output_is_sdr(format);
    let use_heif_primary_still_path = heif_source
        && !animated_avif
        && decoded_animation.is_none()
        && (animated || requires_heif_sdr_decode);
    if use_heif_primary_still_path
        && let Some(decoded) = try_decode_heif_primary_still(input, media_limits, metrics)?
    {
        let mut image = decoded.image;
        validate_vips_image(media_limits, &image)?;
        if decoded.hdr_tone_mapped {
            metrics.record_hdr_tone_map();
        }
        image = resize_loaded_image(media_limits, image, options)?;
        let page_height = if animated { page_height(&image)? } else { None };
        let bytes = encode_vips_image(
            &image,
            VipsEncodeRequest {
                format,
                quality: resolved_quality.quality,
                page_height,
                effort_override: resolved_quality.effort_override,
                animation: anim_limits_from_options(options),
                animation_loop_count: None,
                media_limits,
                full_canvas_animation,
            },
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    if let Some(decoded) = decoded_animation {
        let DecodedAnimation { image, loop_count } = decoded;
        validate_vips_image(media_limits, &image)?;
        let image = resize_loaded_image(media_limits, image, options)?;
        let page_height = if animated { page_height(&image)? } else { None };
        let bytes = encode_vips_image(
            &image,
            VipsEncodeRequest {
                format,
                quality: resolved_quality.quality,
                page_height,
                effort_override: resolved_quality.effort_override,
                animation: anim_limits_from_options(options),
                animation_loop_count: loop_count,
                media_limits,
                full_canvas_animation,
            },
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    if sniffed.mime == "image/bmp"
        && let Some(image) = try_decode_bmp(input, media_limits)?
    {
        validate_vips_image(media_limits, &image)?;
        let image = resize_loaded_image(media_limits, image, options)?;
        let page_height = if animated { page_height(&image)? } else { None };
        let bytes = encode_vips_image(
            &image,
            VipsEncodeRequest {
                format,
                quality: resolved_quality.quality,
                page_height,
                effort_override: resolved_quality.effort_override,
                animation: anim_limits_from_options(options),
                animation_loop_count: None,
                media_limits,
                full_canvas_animation,
            },
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    if should_use_resize_path(options, animated_probe) {
        let mut raw = ptr::null_mut();
        let crop =
            if options.wants_cover_crop() && options.width.is_some() && options.height.is_some() {
                native::THUMB_CROP_CENTRE
            } else {
                native::THUMB_CROP_NONE
            };
        let n_pages: c_int = if animated && source_supports_pages(sniffed.mime) {
            -1
        } else {
            1
        };
        let (max_pages, max_total_pixels) = if n_pages == -1 {
            (
                native_animated_frame_limit(media_limits),
                media_limits.animated_total_pixels(),
            )
        } else {
            (1, media_limits.image_pixels())
        };
        let rc = unsafe {
            native::fluxer_vips_thumbnail_buffer_ex(
                input.as_ptr().cast(),
                input.len(),
                0,
                &mut raw,
                options.width.unwrap_or(0) as c_int,
                options.height.unwrap_or(0) as c_int,
                n_pages,
                crop,
                max_pages,
                max_total_pixels,
            )
        };
        if rc != 0 || raw.is_null() {
            let err = last_vips_error();
            clear_vips_error();
            tracing::error!(
                target: "fluxer_media_proxy::transform_debug",
                stage = "thumbnail_buffer_ex",
                sniffed_mime = %sniffed.mime,
                animated = animated,
                w = options.width.unwrap_or(0),
                h = options.height.unwrap_or(0),
                pages = animated_probe.map(|p| p.pages).unwrap_or(0),
                vips_err = %err,
                "transform failed"
            );
            return Err(MediaError::MediaTransformFailed);
        }
        let image = unsafe { VipsImageHandle::from_raw_borrowing(raw, input) }
            .ok_or(MediaError::MediaTransformFailed)?;
        let image = normalize_vips_image_to_uchar(media_limits, image)?;
        let page_height = if animated { page_height(&image)? } else { None };
        let bytes = encode_vips_image(
            &image,
            VipsEncodeRequest {
                format,
                quality: resolved_quality.quality,
                page_height,
                effort_override: resolved_quality.effort_override,
                animation: anim_limits_from_options(options),
                animation_loop_count: None,
                media_limits,
                full_canvas_animation,
            },
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    let loader_options = if animated && source_supports_pages(sniffed.mime) {
        if sniffed.mime == "image/jpeg" {
            "n=-1,access=sequential"
        } else {
            "n=-1,access=sequential,fail=true"
        }
    } else if sniffed.mime == "image/jpeg" {
        "access=sequential"
    } else {
        "access=sequential,fail=true"
    };
    let loaded = load_image(input, loader_options)?;
    let loaded = normalize_vips_image_to_uchar(media_limits, loaded)?;
    let mut oriented_raw = ptr::null_mut();
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_autorot(loaded.as_ptr(), 0, &mut oriented_raw)
    });
    if let Some(error) = native_status_error(status, MediaError::MediaTransformFailed) {
        clear_vips_error();
        return Err(error);
    }
    let base = unsafe { loaded.adopt_derived_raw(oriented_raw) }
        .ok_or(MediaError::MediaTransformFailed)?;
    validate_vips_image(media_limits, &base)?;
    let page_height = if animated { page_height(&base)? } else { None };
    let bytes = encode_vips_image(
        &base,
        VipsEncodeRequest {
            format,
            quality: resolved_quality.quality,
            page_height,
            effort_override: resolved_quality.effort_override,
            animation: anim_limits_from_options(options),
            animation_loop_count: None,
            media_limits,
            full_canvas_animation,
        },
    )?;
    Ok(ProcessedMedia {
        bytes,
        content_type: format.mime(),
    })
}

pub(super) struct StaticThumbnailRequest<'a> {
    pub(super) media_limits: &'a MediaLimits,
    pub(super) input: &'a [u8],
    pub(super) width: u32,
    pub(super) height: u32,
    pub(super) deadline_ms: Option<i64>,
}

pub(super) fn load_static_thumbnail<'source>(
    request: StaticThumbnailRequest<'source>,
) -> Result<VipsImageHandle<'source>, MediaError> {
    let StaticThumbnailRequest {
        media_limits,
        input,
        width,
        height,
        deadline_ms,
    } = request;
    ensure_vips_init()?;
    ensure_deadline_pending(deadline_ms)?;
    if mime::sniff(input).mime == "image/bmp"
        && let Some(image) = try_decode_bmp(input, media_limits)?
    {
        let image = normalize_vips_image_to_uchar(media_limits, image)?;
        let scale = fit_inside_scale(&image, width, height);
        return resize_loaded_image_by_scale(media_limits, image, scale);
    }
    let mut raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_thumbnail_buffer_ex(
            input.as_ptr().cast(),
            input.len(),
            native_optional_deadline(deadline_ms),
            &mut raw,
            c_int::try_from(width).map_err(|_| MediaError::InvalidImageDimensions)?,
            c_int::try_from(height).map_err(|_| MediaError::InvalidImageDimensions)?,
            1,
            native::THUMB_CROP_NONE,
            1,
            media_limits.image_pixels(),
        )
    };
    let image = unsafe { VipsImageHandle::from_raw_borrowing(raw, input) };
    ensure_deadline_pending(deadline_ms)?;
    if let Some(error) = native_status_error(
        NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    let image = image.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })?;
    normalize_vips_image_to_uchar(media_limits, image)
}

fn fit_inside_scale(image: &VipsImageHandle<'_>, width: u32, height: u32) -> f64 {
    let image_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let image_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    if image_width <= 0 || image_height <= 0 {
        return 1.0;
    }
    (f64::from(width) / f64::from(image_width))
        .min(f64::from(height) / f64::from(image_height))
        .min(1.0)
}
