// SPDX-License-Identifier: AGPL-3.0-or-later

mod adversarial;
mod ffmpeg_cli;
mod images;
mod media;

pub use adversarial::{
    ADVERSARIAL_RANGE_HEADERS, ADVERSARIAL_TEXT_INPUTS, adversarial_media_bytes,
};
pub use ffmpeg_cli::{ffmpeg_gen_media, ffmpeg_gen_mp4, ffmpeg_gen_rotated_mp4, ffmpeg_mirror_mp4};
pub use images::{
    animated_gif_fixture, animated_gif_frames, apng_header, first_webp_anim_frame_size,
    gif_frame_delays_cs, gif_loop_count, minimal_gif, png_dimensions, synthetic_bmp, synthetic_png,
    webp_animation_loop_count, webp_canvas_size, webp_chunk_payloads, webp_with_metadata_chunk,
};
pub use media::{
    fixture_audio_mp3_with_png_cover_art, fixture_audio_mp4_with_attached_picture,
    fixture_audio_only_mp4, fixture_h264_mp4, fixture_jpeg, fixture_mkv_with_png_video_stream,
    fixture_mp4_with_undecodable_video, synthetic_wav,
};
