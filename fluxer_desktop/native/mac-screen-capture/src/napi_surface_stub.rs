// SPDX-License-Identifier: AGPL-3.0-or-later

use napi_derive::napi;

#[napi(js_name = "probeScreenRecordingAccess")]
pub fn probe_screen_recording_access() -> String {
    crate::permission_probe::PROBE_UNSUPPORTED.to_owned()
}
