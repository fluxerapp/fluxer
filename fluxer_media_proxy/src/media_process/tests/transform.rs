// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::image_probe::load_image;
use super::super::native_runtime::ensure_vips_init;
use super::super::transform::source_supports_pages;
use super::super::{ImageOptions, MediaError, image_transform_cost};
use super::fixtures::{animated_mode, metadata_value, transform_image};
use crate::{
    mime, native,
    output_format::OutputFormat,
    test_fixtures::{
        JXL_64_LOSSY_ALPHA, JXL_4100_LOSSY_ALPHA, png_dimensions, synthetic_bmp, synthetic_png,
    },
};

#[test]
fn transforms_png_to_webp() {
    let png = synthetic_png(32, 24);
    let out = transform_image(
        &png,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", out.content_type);
    assert!(out.bytes.starts_with(b"RIFF"));
}

#[test]
fn transforms_static_png_with_animated_flag_does_not_pass_n_to_pngload() {
    let png = synthetic_png(48, 48);
    let out = transform_image(
        &png,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("static-png + animated=true must transform without erroring");
    assert_eq!("image/webp", out.content_type);
    assert!(out.bytes.starts_with(b"RIFF"));
}

#[test]
fn transforms_a_24_bit_bmp_to_webp_without_unblocking_imagemagick() {
    ensure_vips_init().expect("vips must initialise");
    let bmp = synthetic_bmp(4, 4);
    assert_eq!("image/bmp", mime::sniff(&bmp).mime);
    assert!(mime::is_supported_media_mime("image/bmp"));
    assert!(
        load_image(&bmp, "access=sequential,fail=true").is_err(),
        "the libvips loader allowlist must keep ImageMagick blocked for bmp"
    );
    let out = transform_image(
        &bmp,
        &ImageOptions {
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .expect("a 24-bit bmp must transform end to end");
    assert_eq!("image/webp", out.content_type);
    assert!(out.bytes.starts_with(b"RIFF"));
    assert_eq!(
        Some(b"WEBP"),
        out.bytes.get(8..12).map(|tag| tag.try_into().unwrap())
    );
    let decoded =
        load_image(&out.bytes, "access=sequential,fail=true").expect("output must be webp");
    assert_eq!(4, unsafe {
        native::fluxer_vips_image_get_width(decoded.as_ptr())
    });
    assert_eq!(4, unsafe {
        native::fluxer_vips_image_get_height(decoded.as_ptr())
    });
}

#[test]
fn reports_bmp_metadata_dimensions_and_placeholder() {
    let bmp = synthetic_bmp(64, 48);
    let meta = metadata_value(&bmp, "photo.bmp");
    assert_eq!("image/bmp", meta["content_type"]);
    assert_eq!("bmp", meta["format"]);
    assert_eq!(64, meta["width"]);
    assert_eq!(48, meta["height"]);
    assert_eq!(false, meta["animated"]);
    assert!(meta["placeholder"].as_str().is_some_and(|p| !p.is_empty()));
}

#[test]
fn source_supports_pages_matches_libvips_loader_list() {
    assert!(source_supports_pages("image/webp"));
    assert!(source_supports_pages("image/gif"));
    assert!(source_supports_pages("image/apng"));
    assert!(source_supports_pages("image/heif"));
    assert!(source_supports_pages("image/avif"));
    assert!(!source_supports_pages("image/png"));
    assert!(!source_supports_pages("image/jpeg"));
    assert!(!source_supports_pages("image/bmp"));
    assert!(!source_supports_pages("application/octet-stream"));
}

#[test]
fn refuses_jxl_above_the_decode_pixel_cap_before_decoding() {
    for width in [None, Some(128)] {
        let err = transform_image(
            JXL_4100_LOSSY_ALPHA,
            &ImageOptions {
                width,
                format: OutputFormat::WebP,
                ..Default::default()
            },
        )
        .unwrap_err();
        assert_eq!(MediaError::InvalidImageDimensions, err);
    }
}

#[test]
fn transforms_jxl_within_the_decode_pixel_cap() {
    let out = transform_image(
        JXL_64_LOSSY_ALPHA,
        &ImageOptions {
            width: Some(32),
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", out.content_type);
}

#[test]
fn refuses_full_size_svg_raster_above_the_static_output_cap() {
    let svg = br#"<svg xmlns="http://www.w3.org/2000/svg" width="16383" height="16383"></svg>"#;
    let err = transform_image(
        svg,
        &ImageOptions {
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap_err();
    assert_eq!(MediaError::InvalidImageDimensions, err);
    let err = transform_image(
        svg,
        &ImageOptions {
            width: Some(16384),
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap_err();
    assert_eq!(MediaError::InvalidImageDimensions, err);
}

#[test]
fn rasterizes_large_svg_into_a_bounded_box() {
    let svg = br#"<svg xmlns="http://www.w3.org/2000/svg" width="16383" height="16383"></svg>"#;
    let out = transform_image(
        svg,
        &ImageOptions {
            width: Some(64),
            height: Some(64),
            format: OutputFormat::PNG,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(Some((64, 64)), png_dimensions(&out.bytes));
}

#[test]
fn transform_cost_charges_the_full_source_decode_for_jxl() {
    let cost = image_transform_cost(
        JXL_4100_LOSSY_ALPHA,
        &ImageOptions {
            width: Some(128),
            format: OutputFormat::WebP,
            ..Default::default()
        },
        &super::fixtures::test_media_limits(),
    )
    .unwrap();
    assert!(cost >= 4100 * 4100 * 40, "cost {cost}");
}

#[test]
fn transform_cost_charges_svg_at_its_output_size() {
    let svg = br#"<svg xmlns="http://www.w3.org/2000/svg" width="16383" height="16383"></svg>"#;
    let cost = image_transform_cost(
        svg,
        &ImageOptions {
            width: Some(64),
            height: Some(64),
            format: OutputFormat::WebP,
            ..Default::default()
        },
        &super::fixtures::test_media_limits(),
    )
    .unwrap();
    assert!(cost < 1 << 20, "cost {cost}");
}
