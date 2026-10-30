// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::native_runtime::ensure_vips_init;
use super::super::{MediaError, native_optional_deadline, native_status_error};
use crate::native::{self, FluxerAVAudioDurationOut, NativeStatus};
use std::{
    ffi::{CStr, CString},
    os::unix::ffi::OsStrExt,
    path::Path,
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

pub fn strip_av_metadata(
    input: &[u8],
    container: AvContainer,
    output: &Path,
    deadline_ms: Option<i64>,
) -> Result<(), MediaError> {
    ensure_vips_init()?;
    if input.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    let output =
        CString::new(output.as_os_str().as_bytes()).map_err(|_| MediaError::MediaEncodeFailed)?;
    let rc = unsafe {
        native::fluxer_av_strip_metadata(
            input.as_ptr().cast(),
            input.len(),
            container.demuxer.as_ptr(),
            container.muxer.as_ptr(),
            i32::from(container.faststart),
            output.as_ptr(),
            native_optional_deadline(deadline_ms),
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
