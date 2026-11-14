// SPDX-License-Identifier: AGPL-3.0-or-later

use objc2_foundation::{NSError, NSProcessInfo};

pub fn operating_system_version_string() -> String {
    let info = NSProcessInfo::processInfo();
    info.operatingSystemVersionString().to_string()
}

pub fn ns_error_localized_description(err: &NSError) -> String {
    err.localizedDescription().to_string()
}
