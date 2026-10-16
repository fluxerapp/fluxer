// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::nsfw_processing::{VideoNSFWFramesRequest, extract_video_frames_for_nsfw};
use super::super::placeholder::optional_thumbhash;
use super::super::{
    AV_NATIVE_COST_BYTES, MediaError, MetadataOptions, NSFW_PREVIEW_MAX_DIMENSION, NativeCost,
    extract_video_thumbnail, finish_metadata, metadata_cost, prepare_metadata, probe_av_metadata,
};
use super::fixtures::{metadata_value, test_media_limits};
use crate::{
    constants,
    metrics::transform::TransformMetrics,
    mime,
    nsfw::{NSFWClient, NSFWPolicy},
    output_format::OutputFormat,
    test_fixtures::{
        JXL_4100_LOSSLESS_RGB16, JXL_4100_LOSSY_ALPHA, JXL_4100_LOSSY_RGB_DEPTH,
        JXL_7000X4000_LOSSY_RGB, fixture_audio_mp3_with_png_cover_art,
        fixture_audio_mp4_with_attached_picture, fixture_audio_only_mp4, fixture_h264_mp4,
        fixture_mkv_with_png_video_stream, fixture_mp4_with_undecodable_video, synthetic_png,
        synthetic_wav,
    },
};
use std::{io::Write as _, sync::Arc};

fn mapped_past_the_buffer_bound(head: &[u8]) -> memmap2::Mmap {
    let mut file = tempfile::tempfile().expect("temp file");
    file.write_all(head).expect("written head");
    file.set_len(constants::MAX_MEDIA_PROXY_BYTES as u64 + 1)
        .expect("sparse tail");
    unsafe { memmap2::Mmap::map(&file).expect("mapped file") }
}

#[test]
fn metadata_json_includes_dimensions_and_placeholder() {
    let png = synthetic_png(16, 16);
    let meta = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(async {
            finish_metadata(
                prepare_metadata(
                    &png,
                    &MetadataOptions::default(),
                    &test_media_limits(),
                    &TransformMetrics::default(),
                )
                .expect("metadata prepares"),
                &NSFWClient::disabled(),
            )
            .await
            .unwrap()
        });
    assert!(meta.contains("\"format\":\"png\""));
    assert!(meta.contains("\"width\":16"));
    assert!(meta.contains("\"height\":16"));
    assert!(meta.contains("\"placeholder\":\""));
    assert!(meta.contains("\"nsfw\":false"));
    assert!(meta.contains("\"nsfw_probability\":0"));
}

#[test]
fn metadata_json_returns_unavailable_when_nsfw_service_fails() {
    let png = synthetic_png(16, 16);
    let err = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            let client = NSFWClient::new(
                "http://127.0.0.1:9",
                0.85,
                Arc::new(crate::metrics::nsfw::NSFWMetrics::default()),
            )
            .expect("nsfw client builds");
            finish_metadata(
                prepare_metadata(
                    &png,
                    &MetadataOptions {
                        placeholder: false,
                        nsfw: NSFWPolicy::enabled(0.85).expect("valid threshold"),
                        deadline_ms: None,
                    },
                    &test_media_limits(),
                    &TransformMetrics::default(),
                )
                .expect("metadata prepares"),
                &client,
            )
            .await
            .unwrap_err()
        });
    assert_eq!(err, MediaError::NsfwScanUnavailable);
}

#[test]
fn metadata_json_uses_null_dimensions_for_audio() {
    let wav = synthetic_wav();
    let value = metadata_value(&wav, "test.wav");
    assert_eq!(value["content_type"], "audio/wav");
    assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
    assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
    assert_eq!(value["duration"], 1);
}

#[test]
fn metadata_json_classifies_audio_only_mp4_as_audio() {
    let mp4 = fixture_audio_only_mp4();
    assert_eq!("video/mp4", mime::sniff(&mp4).mime);
    let value = metadata_value(&mp4, "renamed.mp4");
    assert_eq!(value["content_type"], "audio/mp4");
    assert_eq!(value["format"], "m4a");
    assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
    assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
    assert_eq!(value["duration"], 1);
}

#[test]
fn metadata_json_treats_mp4_attached_picture_as_audio_cover_art() {
    let mp4 = fixture_audio_mp4_with_attached_picture();
    let probe = probe_av_metadata(&mp4, NSFW_PREVIEW_MAX_DIMENSION, &test_media_limits(), None)
        .expect("audio mp4 probes")
        .probe;
    assert!(probe.has_audio);
    assert!(!probe.has_video);
    let value = metadata_value(&mp4, "renamed.mp4");
    assert_eq!(value["content_type"], "audio/mp4");
    assert_eq!(value["format"], "m4a");
    assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
    assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
}

#[test]
fn metadata_json_accepts_audio_with_png_cover_art() {
    let mp3 = fixture_audio_mp3_with_png_cover_art();
    assert_eq!("audio/mpeg", mime::sniff(&mp3).mime);
    let probe = probe_av_metadata(&mp3, NSFW_PREVIEW_MAX_DIMENSION, &test_media_limits(), None)
        .expect("audio with png cover art probes")
        .probe;
    assert!(probe.has_audio);
    assert!(!probe.has_video);
    let value = metadata_value(&mp3, "cover.mp3");
    assert_eq!(value["content_type"], "audio/mpeg");
    assert_eq!(value["format"], "mp3");
    assert_eq!(value["size"], mp3.len());
    assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
    assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
    assert_eq!(value["animated"], false);
    assert_eq!(value["duration"], 1);
    assert_eq!(None, value.get("placeholder"));
}

#[test]
fn metadata_json_degrades_when_the_video_frame_fails_to_decode() {
    let intact = fixture_h264_mp4();
    let intact_value = metadata_value(&intact, "intact.mp4");
    assert_eq!(intact_value["width"], 16);
    assert_eq!(intact_value["height"], 16);

    let broken = fixture_mp4_with_undecodable_video();
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        probe_av_metadata(
            &broken,
            NSFW_PREVIEW_MAX_DIMENSION,
            &test_media_limits(),
            None
        )
        .err()
    );
    let value = metadata_value(&broken, "broken.mp4");
    assert_eq!(value["content_type"], "video/mp4");
    assert_eq!(value["format"], "mp4");
    assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
    assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
    assert_eq!(None, value.get("placeholder"));
}

#[test]
fn metadata_json_still_rejects_a_playable_video_stream_outside_the_codec_allowlist() {
    let mkv = fixture_mkv_with_png_video_stream();
    assert_eq!("video/x-matroska", mime::sniff(&mkv).mime);
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        probe_av_metadata(&mkv, NSFW_PREVIEW_MAX_DIMENSION, &test_media_limits(), None).err()
    );
    let err = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(async {
            prepare_metadata(
                &mkv,
                &MetadataOptions::default(),
                &test_media_limits(),
                &TransformMetrics::default(),
            )
            .err()
            .expect("metadata preparation fails")
        });
    assert_eq!(MediaError::MediaDecodeFailed, err);
}

#[test]
fn metadata_keeps_the_placeholder_for_every_aspect_ratio() {
    for (width, height, filename) in [
        (64, 64, "square.png"),
        (800, 50, "wide.png"),
        (1000, 100, "banner.png"),
        (100, 1000, "tall.png"),
    ] {
        let value = metadata_value(&synthetic_png(width, height), filename);
        assert_eq!(width, value["width"]);
        assert_eq!(height, value["height"]);
        assert!(
            value["placeholder"]
                .as_str()
                .is_some_and(|placeholder| !placeholder.is_empty()),
            "{filename} should keep its placeholder"
        );
    }
}

#[test]
fn metadata_omits_the_placeholder_when_thumbhash_allocation_fails() {
    assert_eq!(
        None,
        optional_thumbhash(Err(MediaError::AllocationFailed), "image_metadata")
    );
    assert_eq!(
        None,
        optional_thumbhash(Err(MediaError::MediaTransformFailed), "video_metadata")
    );
}

#[test]
fn metadata_succeeds_without_a_placeholder_when_thumbhash_generation_fails() {
    let mut png = synthetic_png(64, 64);
    let idat = png
        .windows(4)
        .position(|window| window == b"IDAT")
        .expect("synthetic png has an IDAT chunk");
    png.truncate(idat + 8);
    let value = metadata_value(&png, "truncated.png");
    assert_eq!("image/png", value["content_type"]);
    assert_eq!(64, value["width"]);
    assert_eq!(64, value["height"]);
    assert_eq!(None, value.get("placeholder"));
}

#[test]
fn metadata_refuses_jxl_above_the_decode_pixel_cap() {
    for jxl in [
        JXL_4100_LOSSY_ALPHA,
        JXL_4100_LOSSY_RGB_DEPTH,
        JXL_4100_LOSSLESS_RGB16,
    ] {
        let err = prepare_metadata(
            jxl,
            &MetadataOptions::default(),
            &test_media_limits(),
            &TransformMetrics::default(),
        )
        .err()
        .expect("metadata preparation fails");
        assert_eq!(MediaError::TooLargeToProcess, err);
    }
}

#[test]
fn metadata_admits_8_bit_rgb_jxl_above_the_decode_pixel_cap() {
    let value = metadata_value(JXL_7000X4000_LOSSY_RGB, "big.jxl");
    assert_eq!("image/jxl", value["content_type"]);
    assert_eq!(7000, value["width"]);
    assert_eq!(4000, value["height"]);
    assert!(value["placeholder"].as_str().is_some_and(|p| !p.is_empty()));
}

#[test]
fn metadata_refuses_an_image_past_the_buffer_bound() {
    let png = mapped_past_the_buffer_bound(&synthetic_png(16, 16));
    assert_eq!("image/png", mime::sniff(&png).mime);
    let err = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(async {
            prepare_metadata(
                &png,
                &MetadataOptions::default(),
                &test_media_limits(),
                &TransformMetrics::default(),
            )
            .err()
            .expect("metadata preparation fails")
        });
    assert_eq!(MediaError::StreamTooLong, err);
}

#[test]
fn native_cost_of_a_video_past_the_buffer_bound_leaves_out_the_mapped_tail() {
    let mp4 = mapped_past_the_buffer_bound(&fixture_h264_mp4());
    assert_eq!(
        NativeCost::UpToBudget(constants::MAX_MEDIA_PROXY_BYTES + AV_NATIVE_COST_BYTES),
        metadata_cost(&mp4, &test_media_limits()).expect("video cost")
    );
}

#[test]
fn video_reads_accept_an_input_past_the_buffer_bound() {
    let mp4 = mapped_past_the_buffer_bound(&fixture_h264_mp4());
    let limits = test_media_limits();

    let probe = probe_av_metadata(&mp4, NSFW_PREVIEW_MAX_DIMENSION, &limits, None)
        .expect("probed video")
        .probe;
    assert!(probe.has_video);
    let poster = extract_video_thumbnail(&mp4, OutputFormat::WebP, &limits).expect("poster frame");
    assert_eq!("image/webp", mime::sniff(&poster.bytes).mime);
    let frames = extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        input: &mp4,
        duration_seconds: probe.duration_seconds,
        deadline_ms: None,
    })
    .expect("sampled frames");
    assert!(!frames.is_empty());
}
