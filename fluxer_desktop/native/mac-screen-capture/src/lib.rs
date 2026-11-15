#![deny(clippy::all)]
// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod permission_probe;

#[cfg(target_os = "macos")]
#[cfg_attr(test, allow(dead_code))]
mod napi_surface_macos;

#[cfg(not(target_os = "macos"))]
#[cfg_attr(test, allow(dead_code))]
mod napi_surface_stub;
