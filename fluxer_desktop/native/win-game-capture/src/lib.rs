// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(clippy::all)]

#[cfg(target_os = "windows")]
mod capture_item_reuse;
mod gpu_priority;
#[cfg(target_os = "windows")]
mod live_items;

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

#[napi(object)]
pub struct WindowCaptureItemCalls {
    pub forwarded: i64,
    pub reused: i64,
}

#[napi(js_name = "reuseWindowCaptureItems")]
pub fn reuse_window_capture_items() -> Result<()> {
    #[cfg(target_os = "windows")]
    capture_item_reuse::install().map_err(napi::Error::from_reason)?;
    Ok(())
}

#[napi(js_name = "windowCaptureItemCalls")]
pub fn window_capture_item_calls() -> WindowCaptureItemCalls {
    #[cfg(target_os = "windows")]
    {
        let calls = capture_item_reuse::create_for_window_calls();
        WindowCaptureItemCalls {
            forwarded: i64::try_from(calls.forwarded).unwrap_or(i64::MAX),
            reused: i64::try_from(calls.reused).unwrap_or(i64::MAX),
        }
    }
    #[cfg(not(target_os = "windows"))]
    WindowCaptureItemCalls {
        forwarded: 0,
        reused: 0,
    }
}
