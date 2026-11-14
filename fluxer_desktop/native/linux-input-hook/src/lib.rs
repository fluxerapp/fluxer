#![deny(clippy::all)]

// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod env;
pub mod keymap;
pub mod modifiers;
pub mod mouse;

#[cfg(target_os = "linux")]
mod hook;

#[cfg(target_os = "linux")]
pub use hook::{InputHook, is_available};

#[cfg(not(target_os = "linux"))]
mod stub {
    use napi_derive::napi;

    #[napi]
    pub fn is_available() -> bool {
        false
    }
}

#[cfg(not(target_os = "linux"))]
pub use stub::is_available;
