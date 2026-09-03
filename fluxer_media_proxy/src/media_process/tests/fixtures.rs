// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::native_runtime::ensure_vips_init;
use super::super::{
    AnimationLimits, AnimationMode, EncodeEffort, ImageOptions, MediaError, MediaLimits,
    MetadataOptions, PNG_SIGNATURE, ProcessedMedia, metadata_json_with_options,
};
use crate::{
    constants,
    metrics::transform::TransformMetrics,
    native::{self, NativeStatus, VipsImageHandle, buffer::NativeBuffer},
    nsfw::NSFWClient,
    test_fixtures::webp_chunk_payloads,
};
use libc::{c_void, size_t};
use std::{ffi::CString, ptr};

#[derive(Clone, Copy, Debug)]
pub(super) struct PngChunk<'a> {
    pub(super) kind: [u8; 4],
    pub(super) data: &'a [u8],
}

pub(super) fn parse_png_chunks(bytes: &[u8]) -> Result<Vec<PngChunk<'_>>, MediaError> {
    if bytes.len() < PNG_SIGNATURE.len() || &bytes[..PNG_SIGNATURE.len()] != PNG_SIGNATURE {
        return Err(MediaError::MediaEncodeFailed);
    }
    let mut chunks = Vec::new();
    let mut offset = PNG_SIGNATURE.len();
    while offset + 12 <= bytes.len() {
        let len = u32::from_be_bytes([
            bytes[offset],
            bytes[offset + 1],
            bytes[offset + 2],
            bytes[offset + 3],
        ]) as usize;
        let kind: [u8; 4] = bytes[offset + 4..offset + 8]
            .try_into()
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let data_start = offset + 8;
        let data_end = data_start
            .checked_add(len)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let crc_end = data_end
            .checked_add(4)
            .ok_or(MediaError::MediaEncodeFailed)?;
        if crc_end > bytes.len() {
            return Err(MediaError::MediaEncodeFailed);
        }
        chunks.push(PngChunk {
            kind,
            data: &bytes[data_start..data_end],
        });
        offset = crc_end;
        if kind == *b"IEND" {
            return Ok(chunks);
        }
    }
    Err(MediaError::MediaEncodeFailed)
}

pub(super) fn png_ihdr_dimensions(ihdr: &[u8]) -> Option<(u32, u32)> {
    if ihdr.len() != 13 {
        return None;
    }
    Some((
        u32::from_be_bytes(ihdr[0..4].try_into().ok()?),
        u32::from_be_bytes(ihdr[4..8].try_into().ok()?),
    ))
}

pub(super) fn test_media_limits() -> MediaLimits {
    MediaLimits::default_from_config()
}

pub(super) fn test_animation_limits() -> AnimationLimits {
    AnimationLimits::new(constants::MAX_ANIMATED_FRAMES_DEFAULT, 30_000)
        .expect("valid animation limits")
}

pub(super) fn animated_mode() -> AnimationMode {
    AnimationMode::Animated(test_animation_limits())
}

pub(super) fn effort(value: u8) -> EncodeEffort {
    EncodeEffort::parse_lenient(&value.to_string()).expect("valid encode effort")
}

pub(super) fn transform_image(
    input: &[u8],
    options: &ImageOptions,
) -> Result<ProcessedMedia, MediaError> {
    super::super::transform_image(
        input,
        options,
        &test_media_limits(),
        &TransformMetrics::new(),
    )
}

pub(super) fn metadata_value(input: &[u8], filename: &str) -> serde_json::Value {
    let meta = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(async {
            metadata_json_with_options(
                input,
                filename,
                MetadataOptions::default(),
                &test_media_limits(),
                &NSFWClient::disabled(),
                &TransformMetrics::new(),
            )
            .await
            .unwrap()
        });
    serde_json::from_str(&meta).unwrap()
}

pub(super) fn assert_animated_webp(bytes: &[u8], frames: usize) {
    assert!(bytes.starts_with(b"RIFF"));
    assert_eq!(b"WEBP", &bytes[8..12]);
    assert_eq!(1, webp_chunk_payloads(bytes, b"ANIM").len());
    assert_eq!(frames, webp_chunk_payloads(bytes, b"ANMF").len());
}

pub(super) fn decode_rgba(bytes: &[u8]) -> (u32, u32, Vec<u8>) {
    ensure_vips_init().expect("libvips initialises");
    let options = CString::new("access=sequential").expect("static string has no NUL");
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(
            bytes.as_ptr().cast(),
            bytes.len(),
            options.as_ptr(),
        )
    };
    let image = unsafe { VipsImageHandle::from_raw_borrowing(raw, bytes) }.expect("decoded image");
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) as u32 };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) as u32 };
    let mut rgba_ptr: *mut c_void = ptr::null_mut();
    let mut rgba_size: size_t = 0;
    let status = NativeStatus::from_code(unsafe {
        native::fluxer_vips_extract_rgba(image.as_ptr(), 0, &mut rgba_ptr, &mut rgba_size)
    });
    assert_eq!(NativeStatus::Ok, status);
    let rgba = unsafe { NativeBuffer::from_vips_owned(rgba_ptr, rgba_size) }.expect("rgba pixels");
    (width, height, rgba.as_slice().to_vec())
}
