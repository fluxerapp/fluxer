// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(clippy::all)]

mod gpu_priority;

use napi::bindgen_prelude::*;
use napi_derive::napi;

#[napi(js_name = "elevateGpuSchedulingPriority")]
pub fn elevate_gpu_scheduling_priority(
    process_id: Option<u32>,
    priority_class: Option<String>,
) -> Result<()> {
    gpu_priority::elevate(process_id, priority_class).map_err(napi::Error::from_reason)
}

#[napi(js_name = "restoreGpuSchedulingPriority")]
pub fn restore_gpu_scheduling_priority(process_id: Option<u32>) -> Result<()> {
    gpu_priority::restore(process_id).map_err(napi::Error::from_reason)
}
