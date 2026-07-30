#![deny(clippy::all)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::missing_const_for_thread_local)]
#![allow(clippy::manual_is_multiple_of)]
#![allow(clippy::manual_saturating_arithmetic)]

pub mod capture_state;
pub mod frame_buffer_pool;
pub mod gpu_loss;
pub mod nv12_packing;

#[cfg(target_os = "linux")]
pub mod game_capture;
#[cfg(target_os = "linux")]
pub mod pipewire_stream;
#[cfg(target_os = "linux")]
pub mod portal;
#[cfg(target_os = "linux")]
pub mod x11_capture;

#[cfg(target_os = "linux")]
mod napi_surface_linux;

#[cfg(not(target_os = "linux"))]
mod napi_surface_stub;
