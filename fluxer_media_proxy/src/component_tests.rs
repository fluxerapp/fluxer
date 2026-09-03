// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    media_process::native_runtime::ensure_vips_init,
    native::{
        self, NativeStatus,
        nsfw_frame_output::{NSFWFrameCopyError, NSFWFrameOutput},
    },
    test_fixtures::{adversarial_media_bytes, ffmpeg_gen_media, minimal_gif},
};
use libc::{c_double, c_int, c_void, size_t};
use std::{ffi::CString, ptr};

fn thumbnail_status(
    input: &[u8],
    deadline_monotonic_ms: i64,
) -> (NativeStatus, *mut native::VipsImage) {
    ensure_vips_init().expect("libvips initialises");
    let mut out = ptr::null_mut();
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_thumbnail_buffer_ex(
            input.as_ptr().cast(),
            input.len(),
            deadline_monotonic_ms,
            &mut out,
            16,
            16,
            1,
            native::THUMB_CROP_NONE,
            1,
            1_048_576,
        )
    });
    (status, out)
}

#[test]
fn negative_thumbnail_deadline_is_rejected_at_the_native_boundary() {
    let (status, out) = thumbnail_status(&minimal_gif(), -1);
    assert_eq!(status, NativeStatus::CodecFailure);
    assert!(out.is_null());
}

#[test]
fn expired_thumbnail_deadline_is_reported_as_deadline_exceeded() {
    let (status, out) = thumbnail_status(&minimal_gif(), 1);
    assert_eq!(status, NativeStatus::DeadlineExceeded);
    assert!(out.is_null());
}

#[test]
fn empty_thumbnail_input_is_rejected_before_libvips_runs() {
    let (status, out) = thumbnail_status(&[], 0);
    assert_eq!(status, NativeStatus::CodecFailure);
    assert!(out.is_null());
}

#[test]
fn blocked_libvips_loaders_cannot_decode_untrusted_input() {
    ensure_vips_init().expect("libvips initialises");
    let ppm = b"P6\n2 2\n255\n\x00\x00\x00\xff\xff\xff\x00\x00\x00\xff\xff\xff".to_vec();
    let options = CString::new("access=sequential").expect("static string has no NUL");
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(ppm.as_ptr().cast(), ppm.len(), options.as_ptr())
    };
    assert!(raw.is_null());
    unsafe { native::fluxer_vips_error_clear() };
}

fn gif_resize_status(
    input: &[u8],
    deadline_monotonic_ms: i64,
) -> (NativeStatus, *mut c_void, size_t, size_t) {
    let mut output = ptr::null_mut::<c_void>();
    let mut output_size = usize::MAX;
    let mut output_capacity = usize::MAX;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_resize_gif(
            input.as_ptr().cast(),
            input.len(),
            1,
            1,
            1,
            deadline_monotonic_ms,
            16,
            16,
            1_000,
            1_048_576,
            1_048_576,
            &mut output,
            &mut output_size,
            &mut output_capacity,
        )
    });
    (status, output, output_size, output_capacity)
}

#[test]
fn negative_gif_deadline_is_rejected_at_the_native_boundary() {
    let (status, output, output_size, output_capacity) = gif_resize_status(&minimal_gif(), -1);
    assert_eq!(NativeStatus::CodecFailure, status);
    assert!(output.is_null());
    assert_eq!(0, output_size);
    assert_eq!(0, output_capacity);
}

fn webp_direct_transform_status(input: &[u8]) -> (NativeStatus, *mut c_void, size_t) {
    let limits = native::WebpAnimLimits {
        max_frames: 16,
        max_duration_ms: 1_000,
        deadline_monotonic_ms: 0,
    };
    let mut output = ptr::null_mut::<c_void>();
    let mut output_size = usize::MAX;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_webp_transform_animated(
            input.as_ptr().cast(),
            input.len(),
            16,
            16,
            85,
            0,
            4,
            90,
            1,
            0,
            16,
            1_048_576,
            &limits,
            1_048_576,
            &mut output,
            &mut output_size,
        )
    });
    (status, output, output_size)
}

fn apng_probe_status(input: &[u8]) -> (NativeStatus, c_int, c_int, c_int) {
    let mut width = c_int::MAX;
    let mut height = c_int::MAX;
    let mut frames = c_int::MAX;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_apng_probe(
            input.as_ptr().cast(),
            input.len(),
            16,
            1_048_576,
            &mut width,
            &mut height,
            &mut frames,
        )
    });
    (status, width, height, frames)
}

fn heif_validate_status(input: &[u8]) -> NativeStatus {
    NativeStatus::from_code(unsafe {
        native::fluxer_heif_validate(input.as_ptr().cast(), input.len(), 0)
    })
}

fn gif_animation_status(input: &[u8]) -> NativeStatus {
    NativeStatus::from_code(unsafe {
        native::fluxer_validate_gif_animation(
            input.as_ptr().cast(),
            input.len(),
            16,
            1_000,
            1_048_576,
        )
    })
}

fn probe_animated_status(input: &[u8]) -> (NativeStatus, c_int, c_int, c_int) {
    ensure_vips_init().expect("libvips initialises");
    let mut width = c_int::MAX;
    let mut height = c_int::MAX;
    let mut pages = c_int::MAX;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_probe_animated(
            input.as_ptr().cast(),
            input.len(),
            &mut width,
            &mut height,
            &mut pages,
        )
    });
    unsafe { native::fluxer_vips_error_clear() };
    (status, width, height, pages)
}

fn apng_decode_status(input: &[u8]) -> (NativeStatus, *mut native::VipsImage, u32) {
    let mut out = ptr::null_mut();
    let mut num_plays = u32::MAX;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_decode_apng(
            input.as_ptr().cast(),
            input.len(),
            1,
            0,
            &mut out,
            16,
            1_048_576,
            1,
            &mut num_plays,
        )
    });
    (status, out, num_plays)
}

fn bmp_decode_status(input: &[u8]) -> (NativeStatus, *mut native::VipsImage) {
    let mut out = ptr::null_mut();
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_decode_bmp(
            input.as_ptr().cast(),
            input.len(),
            1,
            0,
            &mut out,
            1_048_576,
        )
    });
    (status, out)
}

fn heif_still_decode_status(
    input: &[u8],
) -> (
    NativeStatus,
    *mut native::VipsImage,
    native::FluxerHEIFPrimaryStillDecodeFacts,
) {
    let mut out = ptr::null_mut();
    let mut facts = native::FluxerHEIFPrimaryStillDecodeFacts {
        hdr_tone_mapped: c_int::MAX,
        hdr_gain_map_detected: c_int::MAX,
    };
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_heif_decode_primary_still(
            input.as_ptr().cast(),
            input.len(),
            0,
            &mut out,
            1_048_576,
            4_096,
            &mut facts,
        )
    });
    (status, out, facts)
}

fn webp_nsfw_extract_status(input: &[u8]) -> NativeStatus {
    let indices: [c_int; 1] = [0];
    let mut frames = NSFWFrameOutput::new(1);
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_webp_extract_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            1,
            0,
            indices.as_ptr(),
            indices.len(),
            16,
            1_048_576,
            1_048_576,
            frames.as_mut_ptr(),
        )
    });
    assert_eq!(
        Err(NSFWFrameCopyError::InvalidOutput),
        frames.copy_frames(),
        "input {input:?}"
    );
    status
}

#[test]
fn every_remaining_native_validator_zeroes_its_out_params_on_failure() {
    let valid_gif = minimal_gif();
    for bytes in adversarial_media_bytes() {
        let decodable = bytes == valid_gif;

        assert_eq!(
            decodable,
            gif_animation_status(&bytes) == NativeStatus::Ok,
            "input {bytes:?}"
        );

        let (status, width, height, pages) = probe_animated_status(&bytes);
        assert_eq!(decodable, status == NativeStatus::Ok, "input {bytes:?}");
        if decodable {
            assert_eq!(1, width, "input {bytes:?}");
            assert_eq!(1, height, "input {bytes:?}");
            assert_eq!(1, pages, "input {bytes:?}");
        } else {
            assert_eq!(0, width, "input {bytes:?}");
            assert_eq!(0, height, "input {bytes:?}");
            assert_eq!(0, pages, "input {bytes:?}");
        }

        let (status, out, num_plays) = apng_decode_status(&bytes);
        assert_ne!(NativeStatus::Ok, status, "input {bytes:?}");
        assert!(out.is_null(), "input {bytes:?}");
        assert_eq!(0, num_plays, "input {bytes:?}");

        let (status, out) = bmp_decode_status(&bytes);
        assert_ne!(NativeStatus::Ok, status, "input {bytes:?}");
        assert!(out.is_null(), "input {bytes:?}");

        let (status, out, facts) = heif_still_decode_status(&bytes);
        assert_ne!(NativeStatus::Ok, status, "input {bytes:?}");
        assert!(out.is_null(), "input {bytes:?}");
        assert_eq!(0, facts.hdr_tone_mapped, "input {bytes:?}");
        assert_eq!(0, facts.hdr_gain_map_detected, "input {bytes:?}");

        assert_ne!(
            NativeStatus::Ok,
            webp_nsfw_extract_status(&bytes),
            "input {bytes:?}"
        );
    }
}

fn nsfw_extract_statuses(input: &[u8], deadline_monotonic_ms: i64) -> [NativeStatus; 3] {
    let indices: [c_int; 1] = [0];
    let timestamps: [c_double; 1] = [0.0];
    let mut apng_frames = NSFWFrameOutput::new(1);
    let apng = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_extract_apng_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            1,
            deadline_monotonic_ms,
            indices.as_ptr(),
            indices.len(),
            16,
            1_048_576,
            1_048_576,
            apng_frames.as_mut_ptr(),
        )
    });
    assert_eq!(
        Err(NSFWFrameCopyError::InvalidOutput),
        apng_frames.copy_frames()
    );
    let mut gif_frames = NSFWFrameOutput::new(1);
    let gif = NativeStatus::from_code(unsafe {
        native::fluxer_ffmpeg_extract_gif_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            1,
            deadline_monotonic_ms,
            indices.as_ptr(),
            indices.len(),
            16,
            1_048_576,
            1_048_576,
            gif_frames.as_mut_ptr(),
        )
    });
    assert_eq!(
        Err(NSFWFrameCopyError::InvalidOutput),
        gif_frames.copy_frames()
    );
    let mut av_frames = NSFWFrameOutput::new(1);
    let av = NativeStatus::from_code(unsafe {
        native::fluxer_av_extract_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            1,
            deadline_monotonic_ms,
            timestamps.as_ptr(),
            timestamps.len(),
            1_048_576,
            av_frames.as_mut_ptr(),
        )
    });
    assert_eq!(
        Err(NSFWFrameCopyError::InvalidOutput),
        av_frames.copy_frames()
    );
    [apng, gif, av]
}

#[test]
fn native_validation_rejects_adversarial_containers_without_allocating() {
    for bytes in adversarial_media_bytes() {
        let (status, output, output_size, output_capacity) = gif_resize_status(&bytes, -1);
        assert_eq!(NativeStatus::CodecFailure, status, "input {bytes:?}");
        assert!(output.is_null(), "input {bytes:?}");
        assert_eq!(0, output_size, "input {bytes:?}");
        assert_eq!(0, output_capacity, "input {bytes:?}");

        let (status, width, height, frames) = apng_probe_status(&bytes);
        assert_ne!(NativeStatus::Ok, status, "input {bytes:?}");
        assert_eq!(0, width, "input {bytes:?}");
        assert_eq!(0, height, "input {bytes:?}");
        assert_eq!(0, frames, "input {bytes:?}");

        let (status, output, output_size) = webp_direct_transform_status(&bytes);
        assert_ne!(NativeStatus::Ok, status, "input {bytes:?}");
        assert!(output.is_null(), "input {bytes:?}");
        assert_eq!(0, output_size, "input {bytes:?}");

        assert_eq!(
            NativeStatus::CodecFailure,
            heif_validate_status(&bytes),
            "input {bytes:?}"
        );

        for status in nsfw_extract_statuses(&bytes, -1) {
            assert_eq!(NativeStatus::CodecFailure, status, "input {bytes:?}");
        }
    }
}

fn matroska_video(color_args: &[&str]) -> Option<Vec<u8>> {
    let mut args = vec![
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=64x64:rate=10:duration=1",
        "-pix_fmt",
        "yuv420p",
        "-c:v",
        "mpeg4",
    ];
    args.extend_from_slice(color_args);
    args.extend_from_slice(&["-f", "matroska"]);
    ffmpeg_gen_media("fixture.mkv", &args)
}

fn av_nsfw_extract(
    input: &[u8],
    timestamps: &[c_double],
) -> (NativeStatus, Result<Vec<Vec<u8>>, NSFWFrameCopyError>) {
    ensure_vips_init().expect("libvips initialises");
    let mut frames = NSFWFrameOutput::new(timestamps.len());
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_av_extract_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            1,
            0,
            timestamps.as_ptr(),
            timestamps.len(),
            1_048_576,
            frames.as_mut_ptr(),
        )
    });
    let copied = frames.copy_frames();
    (status, copied)
}

#[test]
fn nsfw_frame_emit_failure_is_reported_instead_of_decoder_eof() {
    let Some(sdr) = matroska_video(&[]) else {
        eprintln!("skipping: ffmpeg is unavailable");
        return;
    };
    let timestamps: [c_double; 3] = [0.0, 0.4, 0.8];
    let (sdr_status, sdr_frames) = av_nsfw_extract(&sdr, &timestamps);
    assert_eq!(NativeStatus::Ok, sdr_status);
    assert_eq!(3, sdr_frames.expect("sdr frames").len());

    let rejected = matroska_video(&["-colorspace", "bt2020nc"]).expect("ffmpeg tags the matrix");
    let (status, frames) = av_nsfw_extract(&rejected, &timestamps);
    assert_eq!(NativeStatus::Unsupported, status);
    assert_eq!(Err(NSFWFrameCopyError::InvalidOutput), frames);
}
