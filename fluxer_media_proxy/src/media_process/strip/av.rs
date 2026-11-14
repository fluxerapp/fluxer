// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::native_runtime::ensure_vips_init;
use super::super::{MediaError, native_optional_deadline, native_status_error};
use super::strip_image_metadata;
use crate::{
    media_limits::MediaLimits,
    native::{self, FluxerAVAudioDurationOut, NativeStatus},
};
use libc::{c_int, c_void, size_t};
use std::{
    ffi::{CStr, CString},
    os::unix::ffi::OsStrExt,
    path::Path,
    slice,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AvContainer {
    demuxer: &'static CStr,
    muxer: &'static CStr,
    faststart: bool,
}

pub fn av_container(content_type: &str) -> Option<AvContainer> {
    let muxer: &'static CStr = if content_type.contains("3gpp") {
        c"3gp"
    } else if content_type.contains("mp4") {
        c"mp4"
    } else if content_type.contains("webm") {
        c"webm"
    } else if content_type.contains("quicktime") {
        c"mov"
    } else if content_type.contains("x-matroska") {
        c"matroska"
    } else if content_type.contains("x-msvideo") {
        c"avi"
    } else if content_type.contains("x-flv") {
        c"flv"
    } else if content_type.contains("mp2t") {
        c"mpegts"
    } else if content_type.contains("mpeg") {
        if content_type.starts_with("audio/") {
            c"mp3"
        } else {
            c"mpeg"
        }
    } else if content_type.contains("x-ms-wmv") {
        c"asf"
    } else if content_type.contains("wav") {
        c"wav"
    } else if content_type.contains("flac") {
        c"flac"
    } else if content_type.contains("aac") {
        c"adts"
    } else if content_type.contains("aiff") {
        c"aiff"
    } else if content_type.contains("ogg") {
        c"ogg"
    } else {
        return None;
    };
    let demuxer = match muxer.to_bytes() {
        b"3gp" | b"mov" => c"mp4",
        b"webm" => c"matroska",
        b"adts" => c"aac",
        _ => muxer,
    };
    Some(AvContainer {
        demuxer,
        muxer,
        faststart: matches!(muxer.to_bytes(), b"mp4" | b"mov" | b"3gp"),
    })
}

struct PictureStrip<'a> {
    media_limits: &'a MediaLimits,
    deadline_ms: Option<i64>,
    stripped: Vec<u8>,
}

unsafe extern "C" fn strip_attached_picture(
    user_data: *mut c_void,
    data: *const c_void,
    len: size_t,
    out_data: *mut *const c_void,
    out_len: *mut size_t,
    out_width: *mut c_int,
    out_height: *mut c_int,
) -> c_int {
    if user_data.is_null()
        || data.is_null()
        || out_data.is_null()
        || out_len.is_null()
        || out_width.is_null()
        || out_height.is_null()
    {
        return -1;
    }
    let context = unsafe { &mut *user_data.cast::<PictureStrip<'_>>() };
    let input = unsafe { slice::from_raw_parts(data.cast::<u8>(), len) };
    let Ok(stripped) = strip_image_metadata(input, context.media_limits, context.deadline_ms)
    else {
        return -1;
    };
    let (Ok(width), Ok(height)) = (
        c_int::try_from(stripped.width),
        c_int::try_from(stripped.height),
    ) else {
        return -1;
    };
    context.stripped = stripped.bytes;
    unsafe {
        *out_data = context.stripped.as_ptr().cast();
        *out_len = context.stripped.len();
        *out_width = width;
        *out_height = height;
    }
    0
}

pub fn strip_av_metadata(
    input: &[u8],
    container: AvContainer,
    output: &Path,
    media_limits: &MediaLimits,
    deadline_ms: Option<i64>,
) -> Result<(), MediaError> {
    ensure_vips_init()?;
    if input.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    let output =
        CString::new(output.as_os_str().as_bytes()).map_err(|_| MediaError::MediaEncodeFailed)?;
    let mut pictures = PictureStrip {
        media_limits,
        deadline_ms,
        stripped: Vec::new(),
    };
    let rc = unsafe {
        native::fluxer_av_strip_metadata(
            input.as_ptr().cast(),
            input.len(),
            container.demuxer.as_ptr(),
            container.muxer.as_ptr(),
            i32::from(container.faststart),
            output.as_ptr(),
            native_optional_deadline(deadline_ms),
            strip_attached_picture,
            (&raw mut pictures).cast(),
        )
    };
    match native_status_error(NativeStatus::from_code(rc), MediaError::MediaDecodeFailed) {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn positive(seconds: f64) -> Option<f64> {
    (seconds.is_finite() && seconds > 0.0).then_some(seconds)
}

pub fn audio_duration_seconds(
    input: &[u8],
    deadline_ms: Option<i64>,
) -> Result<Option<f64>, MediaError> {
    ensure_vips_init()?;
    if input.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    let mut out = FluxerAVAudioDurationOut::default();
    let rc = unsafe {
        native::fluxer_av_audio_duration(
            input.as_ptr().cast(),
            input.len(),
            native_optional_deadline(deadline_ms),
            &mut out,
        )
    };
    if let Some(error) =
        native_status_error(NativeStatus::from_code(rc), MediaError::MediaDecodeFailed)
    {
        return Err(error);
    }
    let frames = (out.frame_count > 0).then_some(out.frames_seconds);
    Ok(frames
        .or_else(|| positive(out.stream_seconds))
        .or_else(|| positive(out.format_seconds)))
}
