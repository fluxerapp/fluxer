// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{media_process, mime, native};
use libc::{c_char, c_int, c_uchar};
use std::{ffi::CString, ptr};
use thiserror::Error;

const AVPROBE_PADDING_SIZE: usize = 32;
const AVPROBE_SCORE_RETRY: c_int = 25;

#[repr(C)]
struct AVProbeData {
    filename: *const c_char,
    buf: *mut c_uchar,
    buf_size: c_int,
    mime_type: *const c_char,
}

#[repr(C)]
struct AVInputFormat {
    _private: [u8; 0],
}

unsafe extern "C" {
    fn av_probe_input_format3(
        pd: *const AVProbeData,
        is_opened: c_int,
        score_ret: *mut c_int,
    ) -> *const AVInputFormat;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Result {
    pub detected_mime: String,
    pub probe_score: i32,
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum ValidateError {
    #[error("unsupported codec")]
    UnsupportedCodec,
    #[error("empty buffer")]
    EmptyBuffer,
}

pub fn validate(buffer: &[u8], sniffed_mime: &str) -> std::result::Result<Result, ValidateError> {
    if buffer.is_empty() {
        return Err(ValidateError::EmptyBuffer);
    }
    match mime::category(sniffed_mime) {
        Some(mime::Category::Image) => validate_image(buffer, sniffed_mime),
        Some(mime::Category::Video | mime::Category::Audio) => {
            validate_av_probe(buffer, sniffed_mime)
        }
        None => validate_av_probe(buffer, "application/octet-stream"),
    }
}

fn validate_image(buffer: &[u8], sniffed_mime: &str) -> std::result::Result<Result, ValidateError> {
    media_process::native_runtime::ensure_vips_init()
        .map_err(|_| ValidateError::UnsupportedCodec)?;
    let opts = CString::new("").expect("static string has no NUL");
    let image = unsafe {
        native::fluxer_vips_image_new_from_buffer(
            buffer.as_ptr().cast(),
            buffer.len(),
            opts.as_ptr(),
        )
    };
    let Some(image) = (unsafe { native::VipsImageHandle::from_raw_borrowing(image, buffer) })
    else {
        unsafe { native::fluxer_vips_error_clear() };
        return Err(ValidateError::UnsupportedCodec);
    };
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    if width <= 0 || height <= 0 {
        return Err(ValidateError::UnsupportedCodec);
    }
    Ok(Result {
        detected_mime: sniffed_mime.to_owned(),
        probe_score: 0,
    })
}

fn validate_av_probe(
    buffer: &[u8],
    sniffed_mime: &str,
) -> std::result::Result<Result, ValidateError> {
    let probe_size = buffer.len().min(64 * 1024);
    let mut padded = vec![0u8; probe_size + AVPROBE_PADDING_SIZE];
    padded[..probe_size].copy_from_slice(&buffer[..probe_size]);
    let filename = CString::new("").expect("static string has no NUL");
    let mut score = 0;
    let pd = AVProbeData {
        filename: filename.as_ptr(),
        buf: padded.as_mut_ptr(),
        buf_size: probe_size as c_int,
        mime_type: ptr::null(),
    };
    let fmt = unsafe { av_probe_input_format3(&pd, 1, &mut score) };
    if fmt.is_null() || score < AVPROBE_SCORE_RETRY {
        return Err(ValidateError::UnsupportedCodec);
    }
    Ok(Result {
        detected_mime: sniffed_mime.to_owned(),
        probe_score: score,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_empty_buffer() {
        assert_eq!(Err(ValidateError::EmptyBuffer), validate(&[], "image/png"));
    }

    #[test]
    fn rejects_garbage_as_image() {
        assert_eq!(
            Err(ValidateError::UnsupportedCodec),
            validate(b"this is definitely not an image", "image/png")
        );
    }

    #[test]
    fn rejects_garbage_as_video() {
        let mut garbage = b"<not really mp4>".to_vec();
        garbage.extend([0u8; 64]);
        assert_eq!(
            Err(ValidateError::UnsupportedCodec),
            validate(&garbage, "video/mp4")
        );
    }
}
