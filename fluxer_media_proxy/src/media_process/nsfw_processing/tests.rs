// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::test_fixtures::{animated_gif_fixture, ffmpeg_gen_mp4};
use base64::{Engine as _, engine::general_purpose};

const APNG_FIXTURE_BASE64: &str = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";

fn limits() -> MediaLimits {
    MediaLimits::default_from_config()
}

fn reconciled_frame_count(input: &[u8], media_limits: &MediaLimits) -> u32 {
    super::super::image_probe::probe_image_dims(media_limits, input)
        .expect("image dimensions")
        .pages
        .max(mime::sniff(input).frames)
}

fn assert_bounded_jpeg(frame: &[u8], media_limits: &MediaLimits) -> (u32, u32) {
    assert_eq!(&[0xFF, 0xD8], &frame[..2], "extracted frame is not a JPEG");
    assert!(
        frame.len() <= NSFW_MAX_FRAME_BYTES,
        "frame of {} bytes exceeds the classifier boundary",
        frame.len()
    );
    let dims =
        super::super::image_probe::probe_image_dims(media_limits, frame).expect("frame dimensions");
    assert!(
        dims.width.max(dims.height) <= NSFW_FRAME_MAX_DIMENSION,
        "frame {}x{} exceeds the {NSFW_FRAME_MAX_DIMENSION} px cap",
        dims.width,
        dims.height
    );
    (dims.width, dims.height)
}

#[test]
fn video_nsfw_frames_are_downscaled_to_the_frame_cap() {
    let Some(mp4) = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=1280x720:rate=10:duration=4",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ]) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let media_limits = limits();
    let duration = super::super::probe_av_metadata(&mp4, 0, &media_limits, None)
        .expect("probe generated mp4")
        .probe
        .duration_seconds;
    let frames = extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        media_limits: &media_limits,
        input: &mp4,
        duration_seconds: duration,
        deadline_ms: None,
    })
    .expect("extract nsfw frames");
    assert_eq!(MAX_NSFW_SAMPLE_FRAMES, frames.len());
    for frame in &frames {
        let (width, height) = assert_bounded_jpeg(frame, &media_limits);
        assert_eq!(
            NSFW_FRAME_MAX_DIMENSION,
            width.max(height),
            "a 1280x720 source must be fitted to the cap, got {width}x{height}"
        );
    }
}

#[test]
fn the_video_scan_never_spends_a_slot_on_the_container_first_frame() {
    let Some(mp4) = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=320x240:rate=10:duration=4",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ]) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let media_limits = limits();
    let metrics = TransformMetrics::new();
    let metadata = super::super::probe_av_metadata(
        &mp4,
        super::super::NSFW_PREVIEW_MAX_DIMENSION,
        &media_limits,
        None,
    )
    .expect("probe generated mp4");
    let preview = metadata
        .frame
        .as_ref()
        .expect("the generated mp4 decodes a preview frame")
        .encode_nsfw_jpeg(&media_limits, None)
        .expect("encode the preview frame");
    let frames = nsfw_video_scan_buffers(&NSFWScanSource {
        media_limits: &media_limits,
        metrics: &metrics,
        threshold: NSFWThreshold::new(0.85).expect("0.85 is a valid threshold"),
        content_type: "video/mp4",
        animated: false,
        frame_count: 0,
        input: &mp4,
        duration_seconds: metadata.probe.duration_seconds,
        deadline_ms: None,
    })
    .expect("video nsfw frames");
    assert_eq!(
        MAX_NSFW_SAMPLE_FRAMES,
        frames.len(),
        "every corroborating slot must hold a sampled frame"
    );
    assert!(
        !frames.contains(&preview),
        "the t=0 preview frame must not take one of the sampled slots"
    );
    for frame in &frames {
        assert_bounded_jpeg(frame, &media_limits);
    }
}

#[test]
fn animated_gif_and_apng_frames_extract_through_the_native_decoders() {
    let media_limits = limits();
    let gif = animated_gif_fixture();
    let gif_pages = reconciled_frame_count(&gif, &media_limits);
    assert_eq!(2, gif_pages);
    let gif_frames = extract_native_animated_frames_for_nsfw(
        &media_limits,
        &gif,
        gif_pages,
        MediaType::GIF,
        None,
    )
    .expect("native gif nsfw frames");
    assert_eq!(2, gif_frames.len());
    for frame in &gif_frames {
        assert_bounded_jpeg(frame, &media_limits);
    }

    let apng = general_purpose::STANDARD
        .decode(APNG_FIXTURE_BASE64)
        .expect("apng fixture decodes");
    assert_eq!(
        1,
        super::super::image_probe::probe_image_dims(&media_limits, &apng)
            .expect("apng dimensions")
            .pages,
        "this libvips build reports a single page for an APNG"
    );
    let apng_pages = reconciled_frame_count(&apng, &media_limits);
    assert_eq!(2, apng_pages);
    let apng_frames = extract_native_animated_frames_for_nsfw(
        &media_limits,
        &apng,
        apng_pages,
        MediaType::APNG,
        None,
    )
    .expect("native apng nsfw frames");
    assert_eq!(2, apng_frames.len());
    for frame in &apng_frames {
        assert_bounded_jpeg(frame, &media_limits);
    }
}

#[test]
fn a_frame_count_that_disagrees_with_the_container_is_rejected() {
    let media_limits = limits();
    let gif = animated_gif_fixture();
    assert_eq!(
        Err(MediaError::MediaDecodeFailed),
        extract_native_animated_frames_for_nsfw(&media_limits, &gif, 3, MediaType::GIF, None)
    );
}

#[test]
fn frame_indices_collapse_to_start_middle_and_end() {
    assert_eq!(
        Err(MediaError::InvalidImageDimensions),
        animated_nsfw_frame_indices(0)
    );
    assert_eq!(vec![0], animated_nsfw_frame_indices(1).unwrap());
    assert_eq!(vec![0, 1], animated_nsfw_frame_indices(2).unwrap());
    assert_eq!(vec![0, 1, 2], animated_nsfw_frame_indices(3).unwrap());
    assert_eq!(vec![0, 4, 8], animated_nsfw_frame_indices(9).unwrap());
}

#[test]
fn a_sample_beyond_the_packet_budget_keeps_the_frames_that_decoded() {
    let Some(mp4) = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=64x64:rate=60:duration=12",
        "-pix_fmt",
        "yuv420p",
        "-g",
        "10000",
        "-f",
        "mp4",
    ]) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let media_limits = limits();
    let frames = extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        media_limits: &media_limits,
        input: &mp4,
        duration_seconds: Some(12.0),
        deadline_ms: None,
    })
    .expect("a single keyframe video still scans on the samples that decoded");
    assert_eq!(MAX_NSFW_SAMPLE_FRAMES, frames.len());
    for frame in &frames {
        assert_bounded_jpeg(frame, &media_limits);
    }
}
