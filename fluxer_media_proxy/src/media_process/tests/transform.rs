// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::ImageOptions;
use super::super::image_probe::load_image;
use super::super::native_runtime::ensure_vips_init;
use super::super::transform::source_supports_pages;
use super::fixtures::{animated_mode, metadata_value, transform_image};
use crate::{
    mime, native,
    output_format::OutputFormat,
    test_fixtures::{synthetic_bmp, synthetic_png},
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
