// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::nsfw_processing::{
    VideoNSFWFramesRequest, compute_frame_sample_timestamps, extract_video_frames_for_nsfw,
};
use super::super::{MediaError, probe_av_metadata};
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
fn an_empty_input_fails_to_decode_for_the_probe_and_the_frame_sampler() {
    let frames = extract_video_frames_for_nsfw(VideoNSFWFramesRequest {
        input: &[],
        duration_seconds: None,
        deadline_ms: None,
    });
    assert_eq!(Some(MediaError::MediaDecodeFailed), frames.err());
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        probe_av_metadata(&[], 0, &test_media_limits(), None).err()
    );
}
