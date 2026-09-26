// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::apng::{png_crc32, rewrite_actl_frame_count};
use super::super::image_probe::probe_image_dims;
use super::super::{
    AnimationLimits, AnimationMode, ImageDimensions, ImageOptions, ImageQuality, MediaError,
};
use super::fixtures::{
    animated_mode, parse_png_chunks, png_ihdr_dimensions, test_media_limits, transform_image,
};
use crate::{
    metrics::now_ms,
    output_format::OutputFormat,
    test_fixtures::{first_webp_anim_frame_size, gif_loop_count, webp_animation_loop_count},
};
use base64::Engine as _;

#[test]
fn animated_apng_input_transforms_through_ffmpeg_decode_path() {
    let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
    let apng = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();

    let animated_webp = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", animated_webp.content_type);
    assert!(animated_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
    assert_eq!(
        Some((8, 8)),
        first_webp_anim_frame_size(&animated_webp.bytes)
    );

    let static_webp = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: AnimationMode::Static,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", static_webp.content_type);
    assert!(!static_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
    assert_eq!(
        ImageDimensions {
            width: 8,
            height: 8,
            pages: 1
        },
        probe_image_dims(&test_media_limits(), &static_webp.bytes).unwrap()
    );

    let animated_gif = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/gif", animated_gif.content_type);
    assert!(animated_gif.bytes.starts_with(b"GIF89a") || animated_gif.bytes.starts_with(b"GIF87a"));

    let animated_png = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::APNG,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", animated_png.content_type);
    assert!(animated_png.bytes.starts_with(b"\x89PNG\r\n\x1a\n"));
    let apng_chunks = parse_png_chunks(&animated_png.bytes).unwrap();
    assert_eq!(Some((8, 8)), png_ihdr_dimensions(apng_chunks[0].data));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fcTL"));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fdAT"));
    assert_eq!(
        Some(2),
        apng_chunks
            .iter()
            .find(|chunk| chunk.kind == *b"acTL")
            .and_then(|chunk| chunk.data.get(..4))
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
    );

    let animated_png_alias = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::PNG,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", animated_png_alias.content_type);
    let alias_chunks = parse_png_chunks(&animated_png_alias.bytes).unwrap();
    assert_eq!(Some((8, 8)), png_ihdr_dimensions(alias_chunks[0].data));
    assert!(alias_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
}

fn apng_num_plays(bytes: &[u8]) -> Option<u32> {
    parse_png_chunks(bytes)
        .ok()?
        .iter()
        .find(|chunk| chunk.kind == *b"acTL")
        .and_then(|chunk| chunk.data.get(4..8))
        .and_then(|plays| plays.try_into().ok())
        .map(u32::from_be_bytes)
}

fn patch_apng_num_plays(bytes: &[u8], num_plays: u32) -> Vec<u8> {
    let mut out = bytes.to_vec();
    let position = out
        .windows(4)
        .position(|window| window == b"acTL")
        .expect("fixture carries an acTL chunk");
    let payload_start = position + 4;
    out[payload_start + 4..payload_start + 8].copy_from_slice(&num_plays.to_be_bytes());
    let payload: [u8; 8] = out[payload_start..payload_start + 8]
        .try_into()
        .expect("acTL payload is eight bytes");
    let crc = png_crc32(b"acTL", &payload, None).expect("crc without a deadline");
    out[payload_start + 8..payload_start + 12].copy_from_slice(&crc.to_be_bytes());
    out
}

#[test]
fn animated_apng_transform_carries_the_source_num_plays() {
    let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
    let apng = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();
    assert_eq!(Some(0), apng_num_plays(&apng));

    let looped = patch_apng_num_plays(&apng, 7);
    assert_eq!(Some(7), apng_num_plays(&looped));

    let animated_png = transform_image(
        &looped,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::APNG,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", animated_png.content_type);
    assert_eq!(Some(7), apng_num_plays(&animated_png.bytes));
}

#[test]
fn animated_apng_encode_truncates_at_the_frame_cap_without_erroring() {
    let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
    let apng = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();

    let capped = transform_image(
        &apng,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::APNG,
            quality: ImageQuality::Lossless,
            animation: AnimationMode::Animated(
                AnimationLimits::new(1, 30_000).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", capped.content_type);
    let chunks = parse_png_chunks(&capped.bytes).unwrap();
    assert_eq!(
        Some(1),
        chunks
            .iter()
            .find(|chunk| chunk.kind == *b"acTL")
            .and_then(|chunk| chunk.data.get(..4))
            .and_then(|frames| frames.try_into().ok())
            .map(u32::from_be_bytes)
    );
    assert!(!chunks.iter().any(|chunk| chunk.kind == *b"fdAT"));
}

#[test]
fn apng_with_saturated_num_plays_clamps_per_format_instead_of_failing() {
    let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
    let apng = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();
    let saturated = patch_apng_num_plays(&apng, u32::MAX);
    assert_eq!(Some(u32::MAX), apng_num_plays(&saturated));

    let animated_webp = transform_image(
        &saturated,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", animated_webp.content_type);
    assert_eq!(
        Some(u16::MAX),
        webp_animation_loop_count(&animated_webp.bytes)
    );

    let animated_gif = transform_image(
        &saturated,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/gif", animated_gif.content_type);
    assert_eq!(Some(u16::MAX), gif_loop_count(&animated_gif.bytes));

    let animated_png = transform_image(
        &saturated,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::APNG,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", animated_png.content_type);
    assert_eq!(Some(u32::MAX), apng_num_plays(&animated_png.bytes));
}

#[test]
fn animated_apng_encode_stops_at_the_deadline_instead_of_timing_out() {
    let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
    let apng = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();
    let options = |deadline_ms: Option<i64>| ImageOptions {
        width: Some(8),
        format: OutputFormat::APNG,
        quality: ImageQuality::Lossless,
        animation: animated_mode(),
        deadline_ms,
        ..Default::default()
    };
    assert_eq!(
        "image/apng",
        transform_image(&apng, &options(None)).unwrap().content_type
    );

    let expired_encode_deadline = now_ms() + 2_000;
    assert!(matches!(
        transform_image(&apng, &options(Some(expired_encode_deadline))),
        Err(MediaError::MediaEncodeFailed)
    ));
}

#[test]
fn rewriting_the_actl_frame_count_keeps_the_chunk_crc_valid() {
    let mut encoded = vec![0u8; 33];
    let actl_offset = encoded.len();
    let mut payload = [0u8; 8];
    payload[..4].copy_from_slice(&7u32.to_be_bytes());
    payload[4..].copy_from_slice(&3u32.to_be_bytes());
    encoded.extend_from_slice(&8u32.to_be_bytes());
    encoded.extend_from_slice(b"acTL");
    encoded.extend_from_slice(&payload);
    encoded.extend_from_slice(
        &png_crc32(b"acTL", &payload, None)
            .expect("crc without a deadline")
            .to_be_bytes(),
    );

    rewrite_actl_frame_count(&mut encoded, actl_offset, 2, None)
        .expect("acTL frame count is rewritten");

    let payload_start = actl_offset + 8;
    let payload_end = payload_start + 8;
    assert_eq!(
        2,
        u32::from_be_bytes(
            encoded[payload_start..payload_start + 4]
                .try_into()
                .unwrap()
        )
    );
    assert_eq!(
        3,
        u32::from_be_bytes(encoded[payload_start + 4..payload_end].try_into().unwrap())
    );
    assert_eq!(
        png_crc32(b"acTL", &encoded[payload_start..payload_end], None).unwrap(),
        u32::from_be_bytes(encoded[payload_end..payload_end + 4].try_into().unwrap())
    );
}
