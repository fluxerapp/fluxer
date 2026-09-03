// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::image_probe::probe_image_dims;
use super::super::{ImageDimensions, ImageOptions, ImageQuality, ResizeMode};
use super::fixtures::{
    animated_mode, assert_animated_webp, decode_rgba, parse_png_chunks, test_media_limits,
    transform_image,
};
use crate::{
    metrics::transform::TransformMetrics, mime, output_format::OutputFormat,
    test_fixtures::webp_chunk_payloads,
};
use base64::Engine as _;

#[test]
fn animated_heic_sequence_decodes_every_frame_and_encodes_an_animation() {
    let sequence_b64 = "AAAAKGZ0eXBoZXZjAAAAAG1pZjFoZWljbWlhZm1zZjFpc29taGV2YwAAAtdtb292AAAAbG12aGQAAAAAAAAAAAAAAAAAAAAKAAAABAABAAABAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAACY3RyYWsAAABcdGtoZAAAAAcAAAAAAAAAAAAAAAEAAAAAAAAABAAAAAAAAAAAAAAAAAEAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAQAAAAEAAAAAAAf9tZGlhAAAAIG1kaGQAAAAAAAAAAAAAAAAAAAAKAAAABFXLAAAAAAAhaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAAAAAAG2bWluZgAAACRkaW5mAAAAHGRyZWYAAAAAAAAAAQAAAAx1cmwgAAAAAQAAAXZzdGJsAAAA7nN0c2QAAAAAAAAAAQAAAN5odmMxAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAEAAQABIAAAASAAAAAAAAAABBEhFVkMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGP//AAAAeGh2Y0MBAWAAAAAAAAAAAAAe8AD8/fj4AAAPA2AAAQAYQAEMAf//AWAAAAMAkAAAAwAAAwAeloCQYQABACtCAQEBYAAAAwCQAAADAAADAB6gIIEFllqSSmubgIaDAgAAAwAUAAADAAIQYgABAAdEAcFytCJAAAAAEGNjc3QAAAAAwAAAAAAAABhzdHRzAAAAAAAAAAEAAAAEAAAAAQAAABxzdHNjAAAAAAAAAAEAAAABAAAABAAAAAEAAAAkc3RzegAAAAAAAAAAAAAABAAAAB4AAAAUAAAAFgAAABcAAAAUc3RjbwAAAAAAAAABAAAEgwAAABRzdHNzAAAAAAAAAAEAAAABAAAAFHZtaGQAAAABAAAAAAAAAAAAAAFWbWV0YQAAAAAAAAAhaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAAAAAAAiaWxvYwAAAABEQAABAAEAAAAABF0AAQAAAAAAAAAeAAAAI2lpbmYAAAAAAAEAAAAVaW5mZQIAAAAAAQAAaHZjMQAAAAAOcGl0bQAAAAAAAQAAANZpcHJwAAAAt2lwY28AAAB4aHZjQwEDcAAAAAAAAAAAAB7wAPz9+PgAAA8DYAABABhAAQwB//8DcAAAAwCQAAADAAADAB66AkBhAAEAK0IBAQNwAAADAJAAAAMAAAMAHqAggQWW6kkprm4CGgwIAAADAMgAAAMACEBiAAEAB0QBwXKwIkAAAAATY29scm5jbHgAAQANAAaAAAAAFGlzcGUAAAAAAAAAQAAAAEAAAAAQcGl4aQAAAAADCAgIAAAAF2lwbWEAAAAAAAAAAQABBIECAwQAAAAmbWRhdAAAABooAa8J4CE5dV///sFn+9+qu//6bdHQcw9fgAAAAGdtZGF0AAAAGigBrwzgKGIep//+cn78j8uGP/9E0AcnMQj4AAAAEAIB0Al+KTYKhIVYOmGcqmIAAAASAgHQEf/UpKYKhITEg+YZQKpiAAAAEwIB0Bif/cpJGCwSFRIERhlAqmI=";
    let heic = base64::engine::general_purpose::STANDARD
        .decode(sequence_b64)
        .expect("fixture decodes");
    let sniffed = mime::sniff(&heic);
    assert_eq!("image/heic", sniffed.mime);
    assert!(!sniffed.animated);

    let animated = transform_image(
        &heic,
        &ImageOptions {
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("animated heic transforms");
    assert_eq!("image/webp", animated.content_type);
    assert_animated_webp(&animated.bytes, 4);
    assert_eq!(
        ImageDimensions {
            width: 64,
            height: 64,
            pages: 4
        },
        probe_image_dims(&test_media_limits(), &animated.bytes).expect("probes webp")
    );

    let mut heif = heic.clone();
    let ftyp_len = u32::from_be_bytes(heif[0..4].try_into().expect("ftyp length")) as usize;
    for brand in heif[8..ftyp_len].chunks_exact_mut(4) {
        if brand == b"heic" || brand == b"hevc" {
            brand.copy_from_slice(b"msf1");
        }
    }
    assert_eq!("image/heif", mime::sniff(&heif).mime);
    let animated_heif = transform_image(
        &heif,
        &ImageOptions {
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("animated heif transforms");
    assert_animated_webp(&animated_heif.bytes, 4);
}

#[test]
fn still_heic_decodes_as_a_single_frame_when_animation_is_requested() {
    let still_b64 = "AAAAHGZ0eXBoZWljAAAAAG1pZjFoZWljbWlhZgAAAVZtZXRhAAAAAAAAACFoZGxyAAAAAAAAAABwaWN0AAAAAAAAAAAAAAAAAAAAACJpbG9jAAAAAERAAAEAAQAAAAABegABAAAAAAAAAB4AAAAjaWluZgAAAAAAAQAAABVpbmZlAgAAAAABAABodmMxAAAAAA5waXRtAAAAAAABAAAA1mlwcnAAAAC3aXBjbwAAAHhodmNDAQNwAAAAAAAAAAAAHvAA/P34+AAADwNgAAEAGEABDAH//wNwAAADAJAAAAMAAAMAHroCQGEAAQArQgEBA3AAAAMAkAAAAwAAAwAeoCCBBZbqSSmubgIaDAgAAAMAyAAAAwAIQGIAAQAHRAHBcrAiQAAAABNjb2xybmNseAABAA0ABoAAAAAUaXNwZQAAAAAAAABAAAAAQAAAABBwaXhpAAAAAAMICAgAAAAXaXBtYQAAAAAAAAABAAEEgQIDBAAAACZtZGF0AAAAGigBrwngITl1X//+wWf736q7//pt0dBzD1+A";
    let heic = base64::engine::general_purpose::STANDARD
        .decode(still_b64)
        .expect("fixture decodes");
    assert_eq!("image/heic", mime::sniff(&heic).mime);

    let still = transform_image(
        &heic,
        &ImageOptions {
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("still heic transforms");
    assert_eq!("image/webp", still.content_type);
    assert!(webp_chunk_payloads(&still.bytes, b"ANIM").is_empty());
    assert_eq!(
        ImageDimensions {
            width: 64,
            height: 64,
            pages: 1
        },
        probe_image_dims(&test_media_limits(), &still.bytes).expect("probes webp")
    );
}

#[test]
fn avif_direct_decode_applies_resize_and_crop() {
    let fixture_b64 = "AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAAD5bWV0YQAAAAAAAAAvaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAFBpY3R1cmVIYW5kbGVyAAAAAA5waXRtAAAAAAABAAAAHmlsb2MAAAAARAAAAQABAAAAAQAAASEAAAFZAAAAKGlpbmYAAAAAAAEAAAAaaW5mZQIAAAAAAQAAYXYwMUNvbG9yAAAAAGppcHJwAAAAS2lwY28AAAAUaXNwZQAAAAAAAABAAAAAMAAAABBwaXhpAAAAAAMICAgAAAAMYXYxQ4EADAAAAAATY29scm5jbHgAAgACAAIAAAAAF2lwbWEAAAAAAAAAAQABBAECgwQAAAFhbWRhdAoGGBV/vbAIMs4CRgAABBBBQEqBANtxpEnkS8i7Ewu1Oa+E52+0gHxmN6DekBiIYovbIpo+I+L2MbaIuGgpmhiq3wmhtHx3Lyb9HWhe08jL3lTmL0L92z3pFGZiyNiXjoWSnt6Vs2YF9Ogt2S1YudcnVbcGESJSHNs+6UmubDO+hIB+aL08iAZr/qkVPsTgHY5xL3y7b0B4W8BuTdfXeVy/nJ8V2xmFc1fc4DXzEalW69hTvoJEKuitiwnHu32Gr1Qbjk88s36/tv1BQ2bbYX/QIFDJwLoME7YrHOzOB0zEmhjjdKZkNDwlG0u7YsB5EvaXAnkkgF6l5yaKb8tv2ZBYJO+kDNE7uK8kt5dEIlsrravn8byytjhCTzx5rRLwkj6obavPpIgh/z/z9mG1oxZ2zWugKXunGbw64JUJ+fUiTa2frsG0dGb02dKJ4rPXq9ZQY/B4G3nuZg==";
    let avif = base64::engine::general_purpose::STANDARD
        .decode(fixture_b64)
        .unwrap();

    let resized = transform_image(
        &avif,
        &ImageOptions {
            width: Some(32),
            format: OutputFormat::WebP,
            quality: ImageQuality::High,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", resized.content_type);
    assert_eq!(
        ImageDimensions {
            width: 32,
            height: 24,
            pages: 1
        },
        probe_image_dims(&test_media_limits(), &resized.bytes).unwrap()
    );

    let cropped = transform_image(
        &avif,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::PNG,
            quality: ImageQuality::High,
            resize_mode: ResizeMode::Cover,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/png", cropped.content_type);
    assert_eq!(
        ImageDimensions {
            width: 32,
            height: 32,
            pages: 1
        },
        probe_image_dims(&test_media_limits(), &cropped.bytes).unwrap()
    );
}

#[test]
fn hdr_pq_avif_tone_maps_to_sdr_pixels() {
    let hdr_b64 = "AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAAD5bWV0YQAAAAAAAAAvaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAFBpY3R1cmVIYW5kbGVyAAAAAA5waXRtAAAAAAABAAAAHmlsb2MAAAAARAAAAQABAAAAAQAAASEAAAAkAAAAKGlpbmYAAAAAAAEAAAAaaW5mZQIAAAAAAQAAYXYwMUNvbG9yAAAAAGppcHJwAAAAS2lwY28AAAAUaXNwZQAAAAAAAABAAAAAMAAAABBwaXhpAAAAAAMKCgoAAAAMYXYxQ4EATAAAAAATY29scm5jbHgACQAQAAkAAAAAF2lwbWEAAAAAAAAAAQABBAECgwQAAAAsbWRhdAoOAgAABVV/vGr5UJEAkEAyEhAAhgAIIAAAABAABWqmwW/2MA==";
    let avif = base64::engine::general_purpose::STANDARD
        .decode(hdr_b64)
        .expect("fixture decodes");
    assert_eq!("image/avif", mime::sniff(&avif).mime);
    let metrics = TransformMetrics::new();
    let png = super::super::transform_image(
        &avif,
        &ImageOptions {
            format: OutputFormat::PNG,
            quality: ImageQuality::Lossless,
            ..Default::default()
        },
        &test_media_limits(),
        &metrics,
    )
    .expect("hdr avif transforms");
    assert_eq!("image/png", png.content_type);
    assert_eq!(1, metrics.hdr_tone_map_count());
    let chunks = parse_png_chunks(&png.bytes).expect("valid png");
    let ihdr = chunks.first().expect("png has chunks");
    assert_eq!(*b"IHDR", ihdr.kind);
    assert_eq!(
        8, ihdr.data[8],
        "tone mapped output must be 8 bit per channel"
    );
    let (width, height, rgba) = decode_rgba(&png.bytes);
    assert_eq!((64, 48), (width, height));
    let centre = ((height as usize / 2) * width as usize + width as usize / 2) * 4;
    let expected = [166u8, 160, 165, 255];
    for (channel, value) in expected.iter().enumerate() {
        assert!(
            rgba[centre + channel].abs_diff(*value) <= 3,
            "channel {channel} tone mapped to {} instead of {value}",
            rgba[centre + channel]
        );
    }
    assert!(
        rgba[centre] >= 150,
        "a PQ mid grey read as plain sRGB would land near 128, got {}",
        rgba[centre]
    );
    for pixel in rgba.chunks_exact(4) {
        assert_eq!(255, pixel[3], "tone mapped output must stay opaque");
        for channel in 0..3 {
            assert!(
                pixel[channel].abs_diff(expected[channel]) <= 8,
                "flat source produced {pixel:?}"
            );
        }
    }
}

#[test]
fn hdr_pq_avif_tone_maps_even_when_the_colour_signal_is_one_libheif_cannot_model() {
    let hdr_icc_b64 = "AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAAGObWV0YQAAAAAAAAAvaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAFBpY3R1cmVIYW5kbGVyAAAAAA5waXRtAAAAAAABAAAAHmlsb2MAAAAARAAAAQABAAAAAQAAAbYAAAAkAAAAKGlpbmYAAAAAAAEAAAAaaW5mZQIAAAAAAQAAYXYwMUNvbG9yAAAAAP9pcHJwAAAA32lwY28AAAAUaXNwZQAAAAAAAABAAAAAMAAAABBwaXhpAAAAAAMKCgoAAAAMYXYxQ4EATAAAAAATY29scm5jbHgACQAQAAkAAAAAlGNvbHJwcm9mAAAAiG5vbmUEMAAAbW50clJHQiBYWVogAAAAAAAAAAAAAAAAYWNzcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABhpcG1hAAAAAAAAAAEAAQUBAoMEBQAAACxtZGF0Cg4CAAAFVX+8avlQkQCQQDISEACGAAggAAAAEAAFaqbBb/Yw";
    let avif = base64::engine::general_purpose::STANDARD
        .decode(hdr_icc_b64)
        .expect("fixture decodes");
    assert_eq!("image/avif", mime::sniff(&avif).mime);
    let nclx = avif
        .windows(8)
        .position(|window| window == b"colrnclx")
        .expect("fixture carries an nclx colour box");
    assert!(
        avif.windows(8).any(|window| window == b"colrprof"),
        "fixture carries an icc colour box"
    );
    let mut cases = vec![("an icc profile", avif.clone())];
    for (label, offset) in [
        ("unspecified primaries", nclx + 8),
        ("unspecified matrix coefficients", nclx + 12),
    ] {
        let mut mutated = avif.clone();
        mutated[offset..offset + 2].copy_from_slice(&2u16.to_be_bytes());
        cases.push((label, mutated));
    }
    for (label, bytes) in cases {
        let metrics = TransformMetrics::new();
        let png = super::super::transform_image(
            &bytes,
            &ImageOptions {
                format: OutputFormat::PNG,
                quality: ImageQuality::Lossless,
                ..Default::default()
            },
            &test_media_limits(),
            &metrics,
        )
        .unwrap_or_else(|error| panic!("hdr avif with {label} transforms: {error:?}"));
        assert_eq!(1, metrics.hdr_tone_map_count(), "hdr avif with {label}");
        let (width, height, rgba) = decode_rgba(&png.bytes);
        assert_eq!((64, 48), (width, height), "hdr avif with {label}");
        let centre = ((height as usize / 2) * width as usize + width as usize / 2) * 4;
        assert!(
            rgba[centre] >= 150,
            "hdr avif with {label} read its PQ samples as plain sRGB and landed at {}",
            rgba[centre]
        );
    }
}
