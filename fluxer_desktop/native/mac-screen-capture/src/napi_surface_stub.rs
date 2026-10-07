// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::bindgen_prelude::{Error, Result, Status};
use napi_derive::napi;

fn unsupported() -> Error {
    Error::new(
        Status::GenericFailure,
        "@fluxer/mac-screen-capture is only supported on macOS",
    )
}

#[napi(object, js_name = "MacScreenCaptureSource")]
pub struct MacScreenCaptureSource {
    pub kind: String,
    pub id: String,
    pub name: String,
    pub width: u32,
    pub height: u32,
    pub app_name: Option<String>,
    pub bundle_id: Option<String>,
    pub target_pid: Option<i32>,
}

#[napi(js_name = "listSources")]
pub fn list_sources() -> Result<Vec<MacScreenCaptureSource>> {
    Ok(Vec::new())
}

#[napi(object, js_name = "MacScreenCaptureBackendSckAvailability")]
pub struct SckAvailability {
    pub supported: bool,
    pub macos_version: Option<String>,
}

#[napi(object, js_name = "MacScreenCaptureBackendAvailability")]
pub struct BackendAvailability {
    pub sck: SckAvailability,
    pub screen_permission: String,
}

#[napi(js_name = "getBackendAvailability")]
pub fn get_backend_availability() -> Result<BackendAvailability> {
    Err(unsupported())
}

#[napi(js_name = "probeScreenRecordingAccess")]
pub fn probe_screen_recording_access() -> String {
    crate::permission_probe::PROBE_UNSUPPORTED.to_owned()
}

#[napi(object, js_name = "MacScreenCaptureBackendInfo")]
pub struct MacScreenCaptureBackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    #[napi(js_name = "minMacosVersion")]
    pub min_macos_version: String,
    #[napi(js_name = "detectedMacosVersion")]
    pub detected_macos_version: Option<String>,
    #[napi(js_name = "sckAvailable")]
    pub sck_available: bool,
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> MacScreenCaptureBackendInfo {
    use crate::os_version::{SCK_MIN_MACOS, format_version};
    MacScreenCaptureBackendInfo {
        backend: "mac-screen-capture".to_owned(),
        supported: false,
        reason: "@fluxer/mac-screen-capture is only supported on macOS".to_owned(),
        min_macos_version: format_version(SCK_MIN_MACOS),
        detected_macos_version: None,
        sck_available: false,
    }
}

#[napi]
pub struct ScreenCapture;

#[napi]
impl ScreenCapture {
    #[napi(constructor)]
    pub fn new() -> Result<Self> {
        Err(unsupported())
    }
}
