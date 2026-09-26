// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{MediaError, native_status_error};
use crate::native;
use libc::c_int;
use std::{
    ffi::{CStr, CString},
    sync::OnceLock,
};

const VIPS_ERROR_MESSAGE_MAX_CHARS: usize = 512;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeRuntimeConfig {
    vips_threads_per_pipeline: c_int,
    ffmpeg_decoder_threads: c_int,
    webp_thread_level: c_int,
}

impl NativeRuntimeConfig {
    pub const fn single_pipeline() -> Self {
        Self {
            vips_threads_per_pipeline: 1,
            ffmpeg_decoder_threads: 1,
            webp_thread_level: 0,
        }
    }

    pub(crate) const fn ffmpeg_decoder_threads(self) -> c_int {
        self.ffmpeg_decoder_threads
    }

    pub(crate) const fn webp_thread_level(self) -> c_int {
        self.webp_thread_level
    }
}

pub struct VipsRuntime {
    config: NativeRuntimeConfig,
}

impl VipsRuntime {
    fn initialize(config: NativeRuntimeConfig) -> Result<Self, MediaError> {
        let argv0 = CString::new("fluxer-media-proxy").expect("static string has no NUL");
        let rc = unsafe { native::fluxer_vips_init(argv0.as_ptr()) };
        if let Some(error) = native_status_error(
            native::NativeStatus::from_code(rc),
            MediaError::VipsInitFailed,
        ) {
            clear_vips_error();
            return Err(error);
        }
        unsafe { native::fluxer_vips_tune_for_server(config.vips_threads_per_pipeline) };
        Ok(Self { config })
    }

    pub(crate) const fn config(&self) -> NativeRuntimeConfig {
        self.config
    }
}

static VIPS_RUNTIME: OnceLock<Result<VipsRuntime, MediaError>> = OnceLock::new();

pub(crate) fn vips_runtime() -> Result<&'static VipsRuntime, MediaError> {
    VIPS_RUNTIME
        .get_or_init(|| VipsRuntime::initialize(NativeRuntimeConfig::single_pipeline()))
        .as_ref()
        .map_err(|error| *error)
}

pub fn ensure_vips_init() -> Result<(), MediaError> {
    vips_runtime().map(|_| ())
}

pub(crate) fn clear_vips_error() {
    unsafe { native::fluxer_vips_error_clear() };
}

pub(crate) fn last_vips_error() -> String {
    let buffer = unsafe { native::fluxer_vips_error_buffer() };
    if buffer.is_null() {
        return String::new();
    }
    unsafe { CStr::from_ptr(buffer) }
        .to_string_lossy()
        .trim()
        .chars()
        .take(VIPS_ERROR_MESSAGE_MAX_CHARS)
        .collect()
}
