// SPDX-License-Identifier: AGPL-3.0-or-later

pub const SCK_AUDIO_MIN_MACOS: (i64, i64, i64) = (13, 0, 0);

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
    pub reason: String,
}

pub fn classify_support(detected: Option<(i64, i64, i64)>) -> SupportClassification {
    let min_sck = format_version(SCK_AUDIO_MIN_MACOS);
    let Some(version) = detected else {
        return SupportClassification {
            supported: false,
            reason: "mac-app-audio could not detect the running macOS version. \
                     Per-app and self-excluding desktop audio capture unavailable."
                .to_owned(),
        };
    };
    let detected_str = format_version(version);
    if meets_floor(version, SCK_AUDIO_MIN_MACOS) {
        return SupportClassification {
            supported: true,
            reason: format!(
                "mac-app-audio supported on macOS {detected_str} \
                 (ScreenCaptureKit audio capture, requires macOS {min_sck}+)."
            ),
        };
    }
    SupportClassification {
        supported: false,
        reason: format!(
            "mac-app-audio requires macOS {min_sck}+ (ScreenCaptureKit audio capture). \
             This Mac is running macOS {detected_str}. Per-app audio capture \
             unavailable; Fluxer must not use a broader audio route that could \
             include unrelated apps or call audio."
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn audio_capture_needs_macos_13() {
        assert!(!classify_support(Some((12, 7, 6))).supported);
        assert!(!classify_support(None).supported);
        assert!(classify_support(Some((13, 0, 0))).supported);
        assert!(classify_support(Some((27, 2, 0))).supported);
    }
}
