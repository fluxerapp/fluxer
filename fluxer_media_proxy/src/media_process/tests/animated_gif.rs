// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::animated_transform::gif_resize_dims;
use super::super::{
    AnimationLimits, AnimationMode, ImageOptions, ImageQuality, MediaError, ResizeMode,
};
use super::fixtures::{animated_mode, parse_png_chunks, png_ihdr_dimensions, transform_image};
use crate::{
    mime,
    output_format::OutputFormat,
    test_fixtures::{
        animated_gif_fixture, animated_gif_frames, first_webp_anim_frame_size, gif_frame_delays_cs,
        gif_loop_count, webp_canvas_size,
    },
};

#[test]
fn animated_gif_encodes_to_animated_webp_with_alpha() {
    let gif = animated_gif_fixture();

    let animated_webp = transform_image(
        &gif,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", animated_webp.content_type);
    assert!(animated_webp.bytes.starts_with(b"RIFF"));
    assert_eq!(&animated_webp.bytes[8..12], b"WEBP");
    assert!(animated_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
    assert!(
        animated_webp.bytes.windows(4).any(|chunk| chunk == b"ALPH")
            || animated_webp.bytes.windows(4).any(|chunk| chunk == b"VP8L")
    );
    let (canvas_width, canvas_height, feature_flags) =
        webp_canvas_size(&animated_webp.bytes).unwrap();
    assert_eq!((32, 32), (canvas_width, canvas_height));
    assert_ne!(0, feature_flags & 0x02);
    assert_ne!(0, feature_flags & 0x10);
    assert_eq!(
        Some((32, 32)),
        first_webp_anim_frame_size(&animated_webp.bytes)
    );

    let static_webp = transform_image(
        &gif,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: AnimationMode::Static,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/webp", static_webp.content_type);
    assert!(static_webp.bytes.starts_with(b"RIFF"));
    assert_eq!(&static_webp.bytes[8..12], b"WEBP");

    let animated_gif = transform_image(
        &gif,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
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
        &gif,
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::APNG,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!("image/apng", animated_png.content_type);
    let apng_chunks = parse_png_chunks(&animated_png.bytes).unwrap();
    assert_eq!(Some((32, 32)), png_ihdr_dimensions(apng_chunks[0].data));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fcTL"));
    assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fdAT"));
}

#[test]
fn animated_gif_resize_preserves_last_frame_delay() {
    let gif = animated_gif_fixture();
    assert_eq!(vec![20, 20], gif_frame_delays_cs(&gif));

    let resized = transform_image(
        &gif,
        &ImageOptions {
            width: Some(16),
            height: Some(16),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();

    assert_eq!("image/gif", resized.content_type);
    assert_eq!(vec![20, 20], gif_frame_delays_cs(&resized.bytes));
}

#[test]
fn animated_gif_without_a_resize_passes_through_byte_identically() {
    let gif = animated_gif_fixture();
    let passed_through = transform_image(
        &gif,
        &ImageOptions {
            format: OutputFormat::GIF,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();

    assert_eq!("image/gif", passed_through.content_type);
    assert_eq!(gif, passed_through.bytes);
}

#[test]
fn animated_gif_with_trailing_bytes_after_the_trailer_passes_through_byte_identically() {
    let mut gif = animated_gif_frames(3, 20);
    gif.extend_from_slice(b"trailing garbage");
    let passed_through = transform_image(
        &gif,
        &ImageOptions {
            format: OutputFormat::GIF,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("a gif with trailing bytes after the trailer still passes through");

    assert_eq!("image/gif", passed_through.content_type);
    assert_eq!(gif, passed_through.bytes);
}

#[test]
fn animated_gif_over_the_frame_cap_passes_through_byte_identically() {
    let gif = animated_gif_frames(5, 20);
    let passed_through = transform_image(
        &gif,
        &ImageOptions {
            format: OutputFormat::GIF,
            animation: AnimationMode::Animated(
                AnimationLimits::new(3, 30_000).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .expect("a gif over the frame cap still passes through");

    assert_eq!("image/gif", passed_through.content_type);
    assert_eq!(gif, passed_through.bytes);
}

#[test]
fn animated_gif_over_the_duration_cap_passes_through_byte_identically() {
    let gif = animated_gif_frames(5, 20);
    let passed_through = transform_image(
        &gif,
        &ImageOptions {
            format: OutputFormat::GIF,
            animation: AnimationMode::Animated(
                AnimationLimits::new(4_096, 400).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .expect("a gif over the duration cap still passes through");

    assert_eq!("image/gif", passed_through.content_type);
    assert_eq!(gif, passed_through.bytes);
}

fn resized_animated_gif_with_encode_limits(
    input: &[u8],
    max_frames: u32,
    max_duration_ms: u32,
) -> Vec<u8> {
    transform_image(
        input,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: AnimationMode::Animated(
                AnimationLimits::new(max_frames, max_duration_ms).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .expect("animated gif resize ignores the encode caps")
    .bytes
}

fn assert_animated_gif(bytes: &[u8], frames: usize) {
    assert_eq!(b"GIF89a", &bytes[..6]);
    assert_eq!(Some(&0x3b), bytes.last());
    assert_eq!(frames, gif_frame_delays_cs(bytes).len());
}

#[test]
fn animated_gif_resize_keeps_every_frame_past_the_encode_caps() {
    // A resize is not a re-time. The old proxy handed the ffmpeg resizer only the decode caps, so
    // a long GIF came back whole; binding the encode budget here cut the tail off anything past
    // FLUXER_MEDIA_PROXY_MAX_ENCODE_DURATION_MS while the same GIF passed through untouched when
    // no resize was asked for.
    let gif = animated_gif_frames(5, 20);
    assert_eq!(vec![20u16; 5], gif_frame_delays_cs(&gif));

    let uncapped = resized_animated_gif_with_encode_limits(&gif, 4_096, 30_000);
    assert_animated_gif(&uncapped, 5);
    assert_eq!(vec![20u16; 5], gif_frame_delays_cs(&uncapped));

    for (max_frames, max_duration_ms) in [(3, 30_000), (1, 30_000), (4_096, 400), (1, 1)] {
        let capped = resized_animated_gif_with_encode_limits(&gif, max_frames, max_duration_ms);
        assert_animated_gif(&capped, 5);
        assert_eq!(uncapped, capped);
    }
}

fn cover_cropped_animated_gif_with_encode_limits(
    input: &[u8],
    max_frames: u32,
    max_duration_ms: u32,
) -> Vec<u8> {
    transform_image(
        input,
        &ImageOptions {
            width: Some(8),
            height: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            resize_mode: ResizeMode::Cover,
            animation: AnimationMode::Animated(
                AnimationLimits::new(max_frames, max_duration_ms).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .expect("the generic animated encode writes every frame without failing")
    .bytes
}

#[test]
fn generic_animated_gif_encode_output_is_unchanged_by_the_encode_caps() {
    let gif = animated_gif_frames(5, 20);
    assert_eq!(vec![20u16; 5], gif_frame_delays_cs(&gif));

    let uncapped = cover_cropped_animated_gif_with_encode_limits(&gif, 4_096, 30_000);
    assert_animated_gif(&uncapped, 5);
    assert_eq!(
        uncapped,
        cover_cropped_animated_gif_with_encode_limits(&gif, 2, 30_000)
    );
    assert_eq!(
        uncapped,
        cover_cropped_animated_gif_with_encode_limits(&gif, 4_096, 400)
    );
}

#[test]
fn vips_gif_encode_writes_every_source_frame_past_the_encode_caps() {
    let gif = animated_gif_frames(5, 20);
    assert_eq!(vec![20u16; 5], gif_frame_delays_cs(&gif));

    for (max_frames, max_duration_ms) in [(4_096, 30_000), (2, 30_000), (4_096, 400), (1, 1)] {
        assert_animated_gif(
            &cover_cropped_animated_gif_with_encode_limits(&gif, max_frames, max_duration_ms),
            5,
        );
    }
}

#[test]
fn a_zero_delay_source_resizes_to_the_hundred_millisecond_browser_default() {
    // A GIF that declares a 0 cs delay is not a 20 ms animation: authoring tools emit 0 and
    // every renderer shows it at 100 ms. Clamping it to the fast-frame minimum instead played
    // the resized animation five times faster than the original.
    let gif = animated_gif_frames(5, 0);
    assert_eq!(vec![0u16; 5], gif_frame_delays_cs(&gif));

    let resized = transform_image(
        &gif,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(vec![10u16; 5], gif_frame_delays_cs(&resized.bytes));
}

#[test]
fn a_sub_minimum_nonzero_delay_still_clamps_to_the_fast_frame_minimum() {
    // A declared-but-tiny delay keeps the 20 ms floor; only "no delay at all" means 100 ms.
    let gif = animated_gif_frames(5, 1);
    let resized = transform_image(
        &gif,
        &ImageOptions {
            width: Some(8),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(vec![2u16; 5], gif_frame_delays_cs(&resized.bytes));
}

#[test]
fn animated_gif_resize_still_errors_on_an_expired_deadline() {
    let gif = animated_gif_frames(5, 20);
    assert!(matches!(
        transform_image(
            &gif,
            &ImageOptions {
                width: Some(8),
                format: OutputFormat::GIF,
                quality: ImageQuality::Lossless,
                animation: animated_mode(),
                deadline_ms: Some(1),
                ..Default::default()
            },
        ),
        Err(MediaError::RequestTimeout)
    ));
}

#[test]
fn animated_gif_resize_preserves_the_source_loop_count() {
    let gif = animated_gif_fixture();
    assert_eq!(Some(0), gif_loop_count(&gif));

    let resized = transform_image(
        &gif,
        &ImageOptions {
            width: Some(16),
            height: Some(16),
            format: OutputFormat::GIF,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();

    assert_eq!("image/gif", resized.content_type);
    assert_eq!(gif_loop_count(&gif), gif_loop_count(&resized.bytes));
}

#[test]
fn animated_gif_two_bounds_fit_inside_without_distortion() {
    let dims = gif_resize_dims(
        mime::SniffInfo {
            mime: "image/gif",
            animated: true,
            width: 320,
            height: 240,
            ..Default::default()
        },
        &ImageOptions {
            width: Some(240),
            height: Some(240),
            format: OutputFormat::GIF,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("4:3 GIF should be reduced to fit in a 240px square");

    assert_eq!(240, dims.width);
    assert_eq!(180, dims.height);
}
