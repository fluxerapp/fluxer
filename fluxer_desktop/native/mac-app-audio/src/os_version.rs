// SPDX-License-Identifier: AGPL-3.0-or-later

pub const SCK_MIN_MACOS: (i64, i64, i64) = (12, 3, 0);

pub const COREAUDIO_TAP_MIN_MACOS: (i64, i64, i64) = (14, 2, 0);

pub fn meets_floor(version: (i64, i64, i64), floor: (i64, i64, i64)) -> bool {
    if version.0 != floor.0 {
        return version.0 > floor.0;
    }
    if version.1 != floor.1 {
        return version.1 > floor.1;
    }
    version.2 >= floor.2
}

pub fn format_version(version: (i64, i64, i64)) -> String {
    if version.2 == 0 {
        format!("{}.{}", version.0, version.1)
    } else {
        format!("{}.{}.{}", version.0, version.1, version.2)
    }
}

#[cfg(target_os = "macos")]
pub fn current_macos_version() -> Option<(i64, i64, i64)> {
    use objc2_foundation::NSProcessInfo;
    let info = NSProcessInfo::processInfo();
    let v = info.operatingSystemVersion();
    Some((
        v.majorVersion as i64,
        v.minorVersion as i64,
        v.patchVersion as i64,
    ))
}

#[cfg(not(target_os = "macos"))]
pub fn current_macos_version() -> Option<(i64, i64, i64)> {
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SupportClassification {
    pub supported: bool,
    pub sck_available: bool,
    pub coreaudio_available: bool,
    pub reason: String,
}

pub fn classify_support(detected: Option<(i64, i64, i64)>) -> SupportClassification {
    let min_sck = format_version(SCK_MIN_MACOS);
    let min_coreaudio = format_version(COREAUDIO_TAP_MIN_MACOS);
    match detected {
        None => SupportClassification {
            supported: false,
            sck_available: false,
            coreaudio_available: false,
            reason: "mac-app-audio could not detect the running macOS version. \
                 Per-app and self-excluding desktop audio capture unavailable."
                .to_owned(),
        },
        Some(v) => {
            let detected_str = format_version(v);
            let sck_ok = meets_floor(v, SCK_MIN_MACOS);
            let coreaudio_ok = meets_floor(v, COREAUDIO_TAP_MIN_MACOS);
            let supported = sck_ok || coreaudio_ok;
            let reason = if supported {
                if coreaudio_ok {
                    format!(
                        "mac-app-audio supported on macOS {detected_str} \
                         (CoreAudio process tap, requires macOS {min_coreaudio}+; \
                         ScreenCaptureKit fallback requires macOS {min_sck}+)."
                    )
                } else {
                    format!(
                        "mac-app-audio supported on macOS {detected_str} \
                         (ScreenCaptureKit per-app capture, requires macOS {min_sck}+). \
                         CoreAudio process tap requires macOS {min_coreaudio}+ \
                         and is unavailable here."
                    )
                }
            } else {
                format!(
                    "mac-app-audio requires macOS {min_sck}+ (ScreenCaptureKit). \
                     This Mac is running macOS {detected_str}. Per-app audio capture \
                     unavailable; Fluxer must not use a broader audio route that could \
                     include unrelated apps or call audio."
                )
            };
            SupportClassification {
                supported,
                sck_available: sck_ok,
                coreaudio_available: coreaudio_ok,
                reason,
            }
        }
    }
}
