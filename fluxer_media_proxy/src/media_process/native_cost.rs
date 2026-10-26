// SPDX-License-Identifier: AGPL-3.0-or-later

use super::image_probe::probe_image_dims;
use super::loaded_image::static_output_pixels;
use super::native_runtime::ensure_vips_init;
use super::{ImageDimensions, MediaError};
use crate::{
    constants, image_quality::ImageQuality, image_transform::ImageOptions,
    media_limits::MediaLimits, mime, output_format::OutputFormat,
};

pub const AV_NATIVE_COST_BYTES: usize = 256 << 20;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeCost {
    Bytes(usize),
    UpToBudget(usize),
}
const ANIMATED_CANVAS_BUFFERS: usize = 8;
const RGBA_BYTES_PER_PIXEL: usize = 4;

fn decode_bytes_per_pixel(sniffed_mime: &str) -> usize {
    match sniffed_mime {
        "image/jxl" => 40,
        "image/webp" => 20,
        "image/avif" | "image/heic" | "image/heif" => 12,
        "image/jpeg" => 2,
        "image/svg+xml" => 0,
        _ => 8,
    }
}

fn encode_bytes_per_pixel(format: OutputFormat, quality: ImageQuality) -> usize {
    match (format, quality) {
        (OutputFormat::WebP, ImageQuality::Lossless | ImageQuality::Auto) => 48,
        (OutputFormat::WebP, _) => 28,
        (OutputFormat::JPEG, _) => 4,
        _ => 8,
    }
}

pub fn image_transform_cost(
    input: &[u8],
    options: &ImageOptions,
    media_limits: &MediaLimits,
) -> Result<NativeCost, MediaError> {
    ensure_vips_init()?;
    let sniffed_mime = mime::sniff(input).mime;
    let dims = probe_image_dims(media_limits, input)?;
    let source_pixels = dims.width as usize * dims.height as usize;
    let options =
        fit_static_output_to_budget(input.len(), sniffed_mime, options, dims, media_limits);
    let output_pixels = static_output_pixels(&options, dims);
    let encode = encode_bytes_per_pixel(options.format, options.quality);
    let pixel_cost = if options.is_animated() && dims.pages > 1 {
        source_pixels.max(output_pixels) * RGBA_BYTES_PER_PIXEL * ANIMATED_CANVAS_BUFFERS
            + output_pixels * encode
    } else {
        source_pixels * decode_bytes_per_pixel(sniffed_mime) + output_pixels * encode
    };
    Ok(NativeCost::Bytes(input.len().saturating_add(pixel_cost)))
}

pub(super) fn fit_static_output_to_budget(
    input_len: usize,
    sniffed_mime: &str,
    options: &ImageOptions,
    dims: ImageDimensions,
    media_limits: &MediaLimits,
) -> ImageOptions {
    let mut fitted = *options;
    if options.is_animated() {
        return fitted;
    }
    let source_pixels = dims.width as usize * dims.height as usize;
    let affordable_pixels = media_limits
        .native_memory_budget_bytes()
        .saturating_sub(input_len)
        .saturating_sub(source_pixels * decode_bytes_per_pixel(sniffed_mime))
        / encode_bytes_per_pixel(options.format, options.quality);
    let max_pixels = affordable_pixels.min(constants::MAX_STATIC_OUTPUT_PIXELS);
    if max_pixels == 0 || static_output_pixels(options, dims) <= max_pixels {
        return fitted;
    }
    let (width, height) = match (options.wants_cover_crop(), options.width, options.height) {
        (true, Some(width), Some(height)) => (width, height),
        _ => (dims.width, dims.height),
    };
    let (width, height) = (f64::from(width), f64::from(height));
    let shrink = (max_pixels as f64 / (width * height)).sqrt();
    fitted.width = Some(((width * shrink).floor() as u32).max(1));
    fitted.height = Some(((height * shrink).floor() as u32).max(1));
    fitted
}

pub fn av_native_cost(input_len: usize) -> NativeCost {
    NativeCost::UpToBudget(
        input_len
            .min(constants::MAX_MEDIA_PROXY_BYTES)
            .saturating_add(AV_NATIVE_COST_BYTES),
    )
}

pub fn metadata_cost(input: &[u8], media_limits: &MediaLimits) -> Result<NativeCost, MediaError> {
    let sniffed_mime = mime::sniff(input).mime;
    match mime::category(sniffed_mime) {
        Some(mime::Category::Image) => {
            ensure_vips_init()?;
            let dims = probe_image_dims(media_limits, input)?;
            let source_pixels = dims.width as usize * dims.height as usize;
            Ok(NativeCost::Bytes(input.len().saturating_add(
                source_pixels * decode_bytes_per_pixel(sniffed_mime),
            )))
        }
        Some(mime::Category::Video | mime::Category::Audio) => Ok(av_native_cost(input.len())),
        _ => Ok(NativeCost::Bytes(input.len())),
    }
}
