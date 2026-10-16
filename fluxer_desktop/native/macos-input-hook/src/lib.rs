#![cfg_attr(not(target_os = "macos"), allow(dead_code))]

// SPDX-License-Identifier: AGPL-3.0-or-later

mod keymap;
mod modifiers;
mod mouse;

use napi_derive::napi;

#[cfg(target_os = "macos")]
mod caps_lock_hid;

#[cfg(target_os = "macos")]
mod platform;

#[cfg(target_os = "macos")]
pub use platform::InputHook;

#[napi(js_name = "isAvailable")]
pub fn is_available() -> bool {
    cfg!(target_os = "macos")
}

#[napi(js_name = "hasAccessibilityPermission")]
pub fn has_accessibility_permission() -> bool {
    #[cfg(target_os = "macos")]
    {
        platform::has_accessibility_permission()
    }
    #[cfg(not(target_os = "macos"))]
    {
        false
    }
}
