#![deny(clippy::all)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::manual_is_multiple_of)]
#![allow(clippy::manual_slice_size_calculation)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(clippy::missing_transmute_annotations)]
#![allow(clippy::missing_const_for_thread_local)]
#![allow(clippy::too_many_arguments)]

pub mod audio_converter;
pub mod os_version;
pub mod pcm_pool;
pub mod process_tree;
pub mod related_app;
pub mod source_state;

#[cfg(target_os = "macos")]
pub mod audio_source;
#[cfg(target_os = "macos")]
pub mod coreaudio_tap;
#[cfg(target_os = "macos")]
pub mod foundation;
#[cfg(target_os = "macos")]
pub mod sck;
#[cfg(target_os = "macos")]
pub mod sck_async;

#[cfg(target_os = "macos")]
#[cfg_attr(test, allow(dead_code))]
mod napi_surface_macos;

#[cfg(not(target_os = "macos"))]
#[cfg_attr(test, allow(dead_code))]
mod napi_surface_stub;
