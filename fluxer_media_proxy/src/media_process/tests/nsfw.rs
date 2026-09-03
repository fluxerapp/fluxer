// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::nsfw_processing::{
    VideoNSFWFramesRequest, compute_frame_sample_timestamps, extract_video_frames_for_nsfw,
    nsfw_frame_seed,
};
use super::super::probe_av_metadata;
use super::fixtures::test_media_limits;

#[test]
fn compute_frame_sample_timestamps_distributes_start_middle_end() {
    use rand::SeedableRng as _;
    let seed = [7u8; 32];
    let mut prng = rand_chacha::ChaCha8Rng::from_seed(seed);
    let ts = compute_frame_sample_timestamps(Some(10.0), &mut prng);
    for t in ts {
        assert!((0.0..10.0).contains(&t), "ts {t} out of range");
    }
    assert!(
        ts[0] < ts[1],
        "start {} should precede middle {}",
        ts[0],
        ts[1]
    );
    assert!(
        ts[1] < ts[2],
        "middle {} should precede end {}",
        ts[1],
        ts[2]
    );
}

#[test]
fn nsfw_frame_seed_is_deterministic_per_input() {
    let a = b"hello world this is a video header blob".to_vec();
    let b = b"hello world this is a video header blob".to_vec();
    let c = b"hello world this is a different blob xx".to_vec();
    assert_eq!(nsfw_frame_seed(&a), nsfw_frame_seed(&b));
    assert_ne!(nsfw_frame_seed(&a), nsfw_frame_seed(&c));
}

#[test]
fn extract_video_frames_for_nsfw_returns_multiple_frames() {
    let fixture = std::path::Path::new("tests/fixtures/big-buck-bunny-720p-10s.mp4");
    let alt = std::path::Path::new(".benchmark-cache/media/big-buck-bunny-720p-10s.mp4");
    let path = if fixture.exists() {
        fixture
    } else if alt.exists() {
        alt
    } else {
        eprintln!("skipping: no video fixture available");
        return;
    };
    let bytes = std::fs::read(path).expect("read fixture");
    let limits = test_media_limits();
    let duration = probe_av_metadata(&bytes, 0, &limits, None)
        .expect("probe fixture")
        .probe
        .duration_seconds;
    let frames = extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        media_limits: &limits,
        input: &bytes,
        duration_seconds: duration,
        deadline_ms: None,
    })
    .expect("extract frames");
    assert!(
        !frames.is_empty() && frames.len() <= 3,
        "expected 1-3 frames, got {}",
        frames.len()
    );
    for f in &frames {
        assert!(f.len() > 100, "JPEG frame too small ({} bytes)", f.len());
        assert_eq!(&f[..2], &[0xFF, 0xD8], "not a JPEG");
    }
}
