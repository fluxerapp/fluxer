#![deny(clippy::all)]
// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod permission_probe;

#[cfg(all(target_os = "macos", not(test)))]
mod napi_surface_macos;

#[cfg(all(not(target_os = "macos"), not(test)))]
mod napi_surface_stub;
