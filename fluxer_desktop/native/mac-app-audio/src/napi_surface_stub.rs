// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::bindgen_prelude::{Error, Result, Status};
use napi_derive::napi;

fn unsupported() -> Error {
    Error::new(
        Status::GenericFailure,
        "@fluxer/mac-app-audio is only supported on macOS",
    )
}

#[napi(js_name = "pidFromWindowId")]
pub fn pid_from_window_id(_window_id: i64) -> i32 {
    0
}

#[napi(js_name = "listAudibleApplications")]
pub fn list_audible_applications() -> Result<Vec<String>> {
    Ok(Vec::new())
}

#[napi(js_name = "getBackendAvailability")]
pub fn get_backend_availability() -> Result<()> {
    Err(unsupported())
}

#[napi(object, js_name = "MacAppAudioBackendInfo")]
pub struct MacAppAudioBackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    #[napi(js_name = "minMacosVersion")]
    pub min_macos_version: String,
    #[napi(js_name = "detectedMacosVersion")]
    pub detected_macos_version: Option<String>,
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> MacAppAudioBackendInfo {
    use crate::os_version::{SCK_AUDIO_MIN_MACOS, format_version};
    MacAppAudioBackendInfo {
        backend: "mac-app-audio".to_owned(),
        supported: false,
        reason: "@fluxer/mac-app-audio is only supported on macOS".to_owned(),
        min_macos_version: format_version(SCK_AUDIO_MIN_MACOS),
        detected_macos_version: None,
    }
}

#[napi]
pub struct ProcessLoopback;

#[napi]
impl ProcessLoopback {
    #[napi(constructor)]
    pub fn new() -> Result<Self> {
        Err(unsupported())
    }
}
