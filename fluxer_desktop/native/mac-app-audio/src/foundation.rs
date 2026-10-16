// SPDX-License-Identifier: AGPL-3.0-or-later

use core::ffi::CStr;

use objc2::runtime::ProtocolObject;
use objc2_foundation::{NSError, NSMutableDictionary, NSObject, NSProcessInfo, NSString};

pub fn dict_set_str_key(
    dict: &NSMutableDictionary<NSString, NSObject>,
    key: &CStr,
    value: &NSObject,
) {
    let key_ns = NSString::from_str(key.to_str().unwrap_or(""));
    let key_proto = ProtocolObject::from_ref(&*key_ns);
    unsafe { dict.setObject_forKey(value, key_proto) };
}

pub fn operating_system_version_string() -> String {
    let info = NSProcessInfo::processInfo();
    info.operatingSystemVersionString().to_string()
}

pub fn ns_error_localized_description(err: &NSError) -> String {
    err.localizedDescription().to_string()
}
