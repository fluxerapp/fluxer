// SPDX-License-Identifier: AGPL-3.0-or-later

use super::MediaError;
use super::image_probe::probe_image_dims;
use super::loaded_image::static_output_pixels;
use super::native_runtime::ensure_vips_init;
use crate::{
    image_quality::ImageQuality, image_transform::ImageOptions, media_limits::MediaLimits, mime,
    output_format::OutputFormat,
};

pub const AV_NATIVE_COST_BYTES: usize = 256 << 20;
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
) -> Result<usize, MediaError> {
    ensure_vips_init()?;
    let sniffed_mime = mime::sniff(input).mime;
    let dims = probe_image_dims(media_limits, input)?;
    let source_pixels = dims.width as usize * dims.height as usize;
    let output_pixels = static_output_pixels(options, dims);
    let encode = encode_bytes_per_pixel(options.format, options.quality);
    let pixel_cost = if options.is_animated() && dims.pages > 1 {
        source_pixels.max(output_pixels) * RGBA_BYTES_PER_PIXEL * ANIMATED_CANVAS_BUFFERS
            + output_pixels * encode
    } else {
        source_pixels * decode_bytes_per_pixel(sniffed_mime) + output_pixels * encode
    };
    Ok(input.len().saturating_add(pixel_cost))
}

pub fn metadata_cost(input: &[u8], media_limits: &MediaLimits) -> Result<usize, MediaError> {
    let sniffed_mime = mime::sniff(input).mime;
    match mime::category(sniffed_mime) {
        Some(mime::Category::Image) => {
            ensure_vips_init()?;
            let dims = probe_image_dims(media_limits, input)?;
            let source_pixels = dims.width as usize * dims.height as usize;
            Ok(input
                .len()
                .saturating_add(source_pixels * decode_bytes_per_pixel(sniffed_mime)))
        }
        Some(mime::Category::Video | mime::Category::Audio) => {
            Ok(input.len().saturating_add(AV_NATIVE_COST_BYTES))
        }
        _ => Ok(input.len()),
    }
}
