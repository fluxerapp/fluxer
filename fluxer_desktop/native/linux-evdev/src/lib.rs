#![cfg_attr(not(target_os = "linux"), allow(dead_code))]

// SPDX-License-Identifier: AGPL-3.0-or-later

mod keymap;

#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "linux")]
mod logind;

#[cfg(target_os = "linux")]
pub use linux::{EvdevHook, is_keyboard_readable};

#[cfg(not(target_os = "linux"))]
#[napi_derive::napi(js_name = "isKeyboardReadable")]
pub fn is_keyboard_readable() -> bool {
    false
}
