// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::image_probe::{load_image, probe_image_dims};
use super::super::loaded_image::validate_jxl_decode_pixels;
use super::super::native_runtime::ensure_vips_init;
use super::super::{ImageDimensions, ImageOptions, MediaError, NativeCost, image_transform_cost};
use super::fixtures::{animated_mode, metadata_value, test_media_limits, transform_image};
use crate::{
    constants,
    metrics::transform::TransformMetrics,
    mime, native,
    output_format::OutputFormat,
    test_fixtures::{
        JPEG_ROTATED_MISMATCHED_SCAN, JXL_64_LOSSY_ALPHA, JXL_4100_LOSSLESS_RGB16,
        JXL_4100_LOSSY_ALPHA, JXL_4100_LOSSY_RGB_DEPTH, JXL_7000X4000_LOSSY_RGB, png_dimensions,
        synthetic_bmp, synthetic_png,
    },
};

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
fn refuses_jxl_above_the_decode_pixel_cap_before_decoding() {
    for jxl in [
        JXL_4100_LOSSY_ALPHA,
        JXL_4100_LOSSY_RGB_DEPTH,
        JXL_4100_LOSSLESS_RGB16,
    ] {
        for width in [None, Some(128)] {
            let err = transform_image(
                jxl,
                &ImageOptions {
                    width,
                    format: OutputFormat::WebP,
                    ..Default::default()
                },
            )
            .unwrap_err();
            assert_eq!(MediaError::TooLargeToProcess, err);
        }
    }
}

#[test]
fn jxl_decode_cap_depends_on_the_header_layout() {
    let dims = |width, height, pages, bands, eight_bit| ImageDimensions {
        width,
        height,
        pages,
        bands,
        eight_bit,
    };
    let simple_cap = (8192, 6144);
    let complex_cap = (4096, 4096);
    for bands in [1, 3] {
        let (width, height) = simple_cap;
        assert_eq!(
            Ok(()),
            validate_jxl_decode_pixels("image/jxl", dims(width, height, 1, bands, true))
        );
        assert_eq!(
            Err(MediaError::TooLargeToProcess),
            validate_jxl_decode_pixels("image/jxl", dims(width, height + 1, 1, bands, true))
        );
    }
    for (pages, bands, eight_bit) in [(1, 2, true), (1, 4, true), (1, 3, false), (2, 3, true)] {
        let (width, height) = complex_cap;
        assert_eq!(
            Ok(()),
            validate_jxl_decode_pixels(
                "image/jxl",
                dims(width, height / pages, pages, bands, eight_bit)
            )
        );
        assert_eq!(
            Err(MediaError::TooLargeToProcess),
            validate_jxl_decode_pixels(
                "image/jxl",
                dims(width, height / pages + 1, pages, bands, eight_bit)
            )
        );
    }
    assert_eq!(
        Ok(()),
        validate_jxl_decode_pixels("image/png", dims(16384, 16384, 1, 4, false))
    );
}

#[test]
fn thumbnails_8_bit_rgb_jxl_above_the_decode_pixel_cap() {
    let out = transform_image(
        JXL_7000X4000_LOSSY_RGB,
        &ImageOptions {
            width: Some(550),
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", out.content_type);
    let dims = probe_image_dims(&test_media_limits(), &out.bytes).unwrap();
    assert_eq!((550, 314), (dims.width, dims.height));
}

#[test]
fn full_size_jxl_above_the_static_output_cap_is_downscaled_to_fit() {
    let options = ImageOptions {
        format: OutputFormat::WebP,
        ..Default::default()
    };
    let out = transform_image(JXL_7000X4000_LOSSY_RGB, &options).unwrap();
    assert_eq!("image/webp", out.content_type);
    let dims = probe_image_dims(&test_media_limits(), &out.bytes).unwrap();
    assert_eq!((6636, 3792), (dims.width, dims.height));
    assert!(dims.width as usize * dims.height as usize <= constants::MAX_STATIC_OUTPUT_PIXELS);

    let cost =
        image_transform_cost(JXL_7000X4000_LOSSY_RGB, &options, &test_media_limits()).unwrap();
    assert_eq!(
        NativeCost::Bytes(JXL_7000X4000_LOSSY_RGB.len() + 7000 * 4000 * 40 + 6636 * 3792 * 28),
        cost
    );
}

#[test]
fn full_size_jxl_is_downscaled_to_fit_the_native_memory_budget() {
    let options = ImageOptions {
        format: OutputFormat::WebP,
        ..Default::default()
    };
    let decode = JXL_7000X4000_LOSSY_RGB.len() + 7000 * 4000 * 40;
    let budget = decode + 10_000_000 * 28;
    let media_limits = test_media_limits().with_native_memory_budget(budget);
    let out = super::super::transform_image(
        JXL_7000X4000_LOSSY_RGB,
        &options,
        &media_limits,
        &TransformMetrics::default(),
    )
    .unwrap();
    let dims = probe_image_dims(&test_media_limits(), &out.bytes).unwrap();
    assert_eq!((4183, 2390), (dims.width, dims.height));
    assert_eq!(
        NativeCost::Bytes(decode + 4183 * 2390 * 28),
        image_transform_cost(JXL_7000X4000_LOSSY_RGB, &options, &media_limits).unwrap()
    );
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
    assert_eq!(MediaError::TooLargeToProcess, err);
    let err = transform_image(
        svg,
        &ImageOptions {
            width: Some(16384),
            format: OutputFormat::WebP,
            ..Default::default()
        },
    )
    .unwrap_err();
    assert_eq!(MediaError::TooLargeToProcess, err);
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
    let NativeCost::Bytes(cost) = cost else {
        panic!("an image is charged exact bytes")
    };
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
    let NativeCost::Bytes(cost) = cost else {
        panic!("an image is charged exact bytes")
    };
    assert!(cost < 1 << 20, "cost {cost}");
}

#[test]
fn transforms_a_rotated_jpeg_with_mismatched_scan_headers_at_full_size() {
    let out = transform_image(
        JPEG_ROTATED_MISMATCHED_SCAN,
        &ImageOptions {
            format: OutputFormat::PNG,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(Some((768, 1024)), png_dimensions(&out.bytes));
}
