// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::encoding::try_transform_animated_webp_direct;
use super::super::image_probe::probe_image_dims;
use super::super::native_runtime::ensure_vips_init;
use super::super::{
    AnimationLimits, AnimationMode, ImageOptions, ImageQuality, MediaError, ResolvedImageQuality,
};
use super::fixtures::{animated_mode, assert_animated_webp, test_media_limits, transform_image};
use crate::{
    mime,
    native::{self, NativeStatus},
    output_format::OutputFormat,
    test_fixtures::{
        animated_gif_fixture, animated_gif_frames, gif_frame_delays_cs, gif_loop_count,
        webp_animation_loop_count, webp_canvas_size, webp_chunk_payloads, webp_with_metadata_chunk,
    },
};
use libc::c_int;

fn webp_direct_transform(input: &[u8]) -> Option<Vec<u8>> {
    ensure_vips_init().expect("libvips initialises");
    try_transform_animated_webp_direct(
        input,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
        ResolvedImageQuality::High,
        None,
        &test_media_limits(),
    )
    .expect("direct animated webp transform succeeds or falls through")
}

fn animated_webp_source() -> Vec<u8> {
    transform_image(
        &animated_gif_fixture(),
        &ImageOptions {
            width: Some(32),
            height: Some(32),
            format: OutputFormat::WebP,
            quality: ImageQuality::Lossless,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("animated gif encodes to animated webp")
    .bytes
}

#[test]
fn animated_webp_transforms_directly_and_falls_through_on_embedded_metadata() {
    let source = animated_webp_source();
    assert!(mime::sniff(&source).animated);

    let direct = transform_image(
        &source,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .expect("animated webp transforms through the direct path");
    assert_eq!("image/webp", direct.content_type);
    assert!(direct.bytes.starts_with(b"RIFF"));
    assert_eq!(&direct.bytes[8..12], b"WEBP");
    assert!(direct.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
    let (canvas_width, canvas_height, feature_flags) =
        webp_canvas_size(&direct.bytes).expect("direct output carries a VP8X canvas");
    assert_eq!((16, 16), (canvas_width, canvas_height));
    assert_ne!(0, feature_flags & 0x02);
    let source_frames = webp_chunk_payloads(&source, b"ANMF").len();
    assert!(source_frames > 1);
    assert_eq!(
        source_frames,
        webp_chunk_payloads(&direct.bytes, b"ANMF").len()
    );

    for (fourcc, flag) in [(b"ICCP", 0x20u8), (b"EXIF", 0x08), (b"XMP ", 0x04)] {
        let tagged = webp_with_metadata_chunk(&source, fourcc, flag)
            .expect("animated webp accepts an embedded metadata chunk");
        assert_eq!(
            None,
            webp_direct_transform(&tagged),
            "chunk {}",
            String::from_utf8_lossy(fourcc)
        );
        let fallback = transform_image(
            &tagged,
            &ImageOptions {
                width: Some(16),
                format: OutputFormat::WebP,
                animation: animated_mode(),
                ..Default::default()
            },
        )
        .expect("tagged animated webp falls through to the generic path");
        assert_eq!("image/webp", fallback.content_type);
        assert!(fallback.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
    }
    assert!(
        webp_direct_transform(&source).is_some(),
        "untagged source stays on the direct path"
    );
}

#[test]
fn animated_webp_encode_carries_the_source_loop_count() {
    let mut gif = animated_gif_fixture();
    let netscape = gif
        .windows(11)
        .position(|window| window == b"NETSCAPE2.0")
        .expect("fixture carries a NETSCAPE application extension");
    gif[netscape + 13] = 3;
    gif[netscape + 14] = 0;
    assert_eq!(Some(3), gif_loop_count(&gif));

    let out = transform_image(
        &gif,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            animation: animated_mode(),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(Some(4), webp_animation_loop_count(&out.bytes));
}

#[test]
fn animated_webp_nsfw_frames_extract_as_bounded_jpegs() {
    let source = animated_webp_source();
    let indices: [c_int; 2] = [0, 1];
    let mut frames = native::nsfw_frame_output::NSFWFrameOutput::new(indices.len());
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_webp_extract_frames_for_nsfw(
            source.as_ptr().cast(),
            source.len(),
            0,
            0,
            indices.as_ptr(),
            indices.len(),
            16,
            test_media_limits().animated_total_pixels(),
            1_048_576,
            frames.as_mut_ptr(),
        )
    });
    assert_eq!(NativeStatus::Ok, status);
    let extracted = frames.copy_frames().expect("frames copy out");
    assert_eq!(2, extracted.len());
    for frame in &extracted {
        assert_eq!(&[0xFF, 0xD8], &frame[..2]);
        let dims = probe_image_dims(&test_media_limits(), frame).unwrap();
        assert!(dims.width <= 512 && dims.height <= 512);
    }
}

#[test]
fn zero_delay_animation_frames_are_clamped_instead_of_rejected() {
    let mut gif = animated_gif_fixture();
    let mut offset = 0usize;
    let mut patched = 0usize;
    while offset + 8 <= gif.len() {
        if gif[offset] == 0x21 && gif[offset + 1] == 0xF9 && gif[offset + 2] == 0x04 {
            gif[offset + 4] = 0;
            gif[offset + 5] = 0;
            patched += 1;
        }
        offset += 1;
    }
    assert_eq!(2, patched);
    assert!(gif_frame_delays_cs(&gif).iter().all(|delay| *delay == 0));

    for format in [OutputFormat::WebP, OutputFormat::APNG] {
        let out = transform_image(
            &gif,
            &ImageOptions {
                width: Some(16),
                format,
                animation: animated_mode(),
                ..Default::default()
            },
        )
        .unwrap_or_else(|err| panic!("zero-delay {format:?} encode failed: {err:?}"));
        assert_eq!(format.mime(), out.content_type);
    }
}

fn animated_webp_with_encode_limits(
    input: &[u8],
    max_frames: u32,
    max_duration_ms: u32,
) -> Vec<u8> {
    transform_image(
        input,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            animation: AnimationMode::Animated(
                AnimationLimits::new(max_frames, max_duration_ms).expect("valid animation limits"),
            ),
            ..Default::default()
        },
    )
    .expect("animated webp encode truncates instead of failing")
    .bytes
}

fn direct_animated_webp_with_encode_limits(
    input: &[u8],
    max_frames: u32,
    max_duration_ms: u32,
) -> Vec<u8> {
    ensure_vips_init().expect("libvips initialises");
    try_transform_animated_webp_direct(
        input,
        &ImageOptions {
            width: Some(16),
            format: OutputFormat::WebP,
            animation: AnimationMode::Animated(
                AnimationLimits::new(max_frames, max_duration_ms).expect("valid animation limits"),
            ),
            ..Default::default()
        },
        ResolvedImageQuality::High,
        None,
        &test_media_limits(),
    )
    .expect("direct animated webp transform truncates instead of failing")
    .expect("direct animated webp transform stays on the direct path")
}

#[test]
fn animated_webp_encode_truncates_at_the_frame_cap_without_erroring() {
    let gif = animated_gif_frames(4, 20);
    assert_eq!(vec![20u16; 4], gif_frame_delays_cs(&gif));

    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 4_096, 30_000), 4);
    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 3, 30_000), 3);
    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 2, 30_000), 2);
}

#[test]
fn animated_webp_encode_truncates_at_the_duration_cap_without_erroring() {
    let gif = animated_gif_frames(4, 20);

    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 4_096, 800), 4);
    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 4_096, 600), 3);
    assert_animated_webp(&animated_webp_with_encode_limits(&gif, 4_096, 400), 2);
}

#[test]
fn animated_webp_direct_transform_truncates_at_the_encode_caps() {
    let source = animated_webp_with_encode_limits(&animated_gif_frames(4, 20), 4_096, 30_000);
    assert_animated_webp(&source, 4);

    assert_animated_webp(
        &direct_animated_webp_with_encode_limits(&source, 4_096, 30_000),
        4,
    );
    assert_animated_webp(
        &direct_animated_webp_with_encode_limits(&source, 2, 30_000),
        2,
    );
    assert_animated_webp(
        &direct_animated_webp_with_encode_limits(&source, 4_096, 400),
        2,
    );
}

#[test]
fn animated_webp_encode_still_errors_on_an_expired_deadline() {
    let expired = ImageOptions {
        width: Some(16),
        format: OutputFormat::WebP,
        animation: animated_mode(),
        deadline_ms: Some(1),
        ..Default::default()
    };
    let gif = animated_gif_frames(4, 20);
    assert!(matches!(
        transform_image(&gif, &expired),
        Err(MediaError::RequestTimeout)
    ));

    let source = animated_webp_with_encode_limits(&gif, 4_096, 30_000);
    ensure_vips_init().expect("libvips initialises");
    assert!(matches!(
        try_transform_animated_webp_direct(
            &source,
            &expired,
            ResolvedImageQuality::High,
            None,
            &test_media_limits(),
        ),
        Err(MediaError::RequestTimeout)
    ));
}
