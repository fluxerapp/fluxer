// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::placeholder::{encode_thumbhash, optional_thumbhash};
use super::super::{
    MediaError, MetadataOptions, NSFW_PREVIEW_MAX_DIMENSION, metadata_json_with_options,
    probe_av_metadata,
};
use super::fixtures::{metadata_value, test_media_limits};
use crate::{
    metrics::transform::TransformMetrics,
    mime,
    nsfw::{NSFWClient, NSFWPolicy},
    test_fixtures::{
        fixture_audio_mp3_with_png_cover_art, fixture_audio_mp4_with_attached_picture,
        fixture_audio_only_mp4, fixture_h264_mp4, fixture_mkv_with_png_video_stream,
        fixture_mp4_with_undecodable_video, synthetic_png, synthetic_wav,
    },
};
use std::sync::Arc;

#[test]
fn metadata_json_includes_dimensions_and_placeholder() {
    let png = synthetic_png(16, 16);
    let meta = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(async {
            metadata_json_with_options(
                &png,
                "test.png",
                MetadataOptions::default(),
                &test_media_limits(),
                &NSFWClient::disabled(),
                &TransformMetrics::new(),
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
                Arc::new(crate::metrics::nsfw::NSFWMetrics::new()),
            )
            .expect("nsfw client builds");
            metadata_json_with_options(
                &png,
                "test.png",
                MetadataOptions {
                    placeholder: false,
                    nsfw: NSFWPolicy::enabled(0.85).expect("valid threshold"),
                },
                &test_media_limits(),
                &client,
                &TransformMetrics::new(),
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
fn metadata_json_accepts_audio_carrying_png_cover_art() {
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
            metadata_json_with_options(
                &mkv,
                "png.mkv",
                MetadataOptions::default(),
                &test_media_limits(),
                &NSFWClient::disabled(),
                &TransformMetrics::new(),
            )
            .await
            .unwrap_err()
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
fn thumbhash_for_valid_image_is_non_empty() {
    let png = synthetic_png(16, 16);
    let hash = encode_thumbhash(&test_media_limits(), &png, None).unwrap();
    assert!(!hash.is_empty());
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
