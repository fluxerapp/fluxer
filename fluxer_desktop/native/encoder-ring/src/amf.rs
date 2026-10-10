// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::Arc;

use libloading::{Library, Symbol};
use windows::Win32::Graphics::Direct3D11::ID3D11Device;
use windows::core::Interface;

use crate::encoder_handoff::{
    AmfHandoff, EncodedBitstream, EncoderCompletionCallback, EncoderDims, EncoderError,
    EncoderFrameRate, EncoderSubmission, HandoffSlot, PicParams, apply_dts_offset,
    compute_dts_offset_us,
};
use crate::ring::RingError;

pub const AMF_DLL_NAME: &str = "amfrt64.dll";

const AMF_OK: i32 = 0;
#[allow(
    dead_code,
    reason = "documented as a possible AMF QueryOutput status code per the SDK"
)]
const AMF_REPEAT: i32 = 5;
#[allow(
    dead_code,
    reason = "documented as a possible AMF status code per the SDK"
)]
const AMF_NOT_READY: i32 = 1;

type AmfStatus = i32;
type AmfInitFn = unsafe extern "C" fn(version: u64, factory: *mut *mut c_void) -> AmfStatus;

#[repr(C)]
struct AmfFactoryVtbl {
    query_interface:
        unsafe extern "system" fn(*mut c_void, *const u128, *mut *mut c_void) -> AmfStatus,
    acquire: unsafe extern "system" fn(*mut c_void) -> u32,
    release: unsafe extern "system" fn(*mut c_void) -> u32,
    create_context: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> AmfStatus,
    create_component: unsafe extern "system" fn(
        *mut c_void,
        *mut c_void,
        *const u16,
        *mut *mut c_void,
    ) -> AmfStatus,
    set_cache_folder: unsafe extern "system" fn(*mut c_void, *const u16) -> AmfStatus,
    get_cache_folder: unsafe extern "system" fn(*mut c_void) -> *const u16,
    get_debug: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> AmfStatus,
    get_trace: unsafe extern "system" fn(*mut c_void, *mut *mut c_void) -> AmfStatus,
    get_program_versions:
        unsafe extern "system" fn(*mut c_void, *mut u32, *mut u32, *mut u32, *mut u32) -> AmfStatus,
}

#[repr(C)]
struct AmfFactoryObject {
    vtbl: *const AmfFactoryVtbl,
}

struct SlotState {
    pending_pts_us: u64,
    pending_force_keyframe: bool,
    in_flight: bool,
}

pub struct AmfD3D11Handoff {
    _library: Arc<Library>,
    factory: *mut c_void,
    context: *mut c_void,
    encoder: *mut c_void,
    slots: HashMap<u32, SlotState>,
    next_slot_index: u32,
    dts_offset_us: i64,
    completed_count: u64,
    frame_interval_us: u64,
}

unsafe impl Send for AmfD3D11Handoff {}

impl AmfD3D11Handoff {
    pub fn new(
        device: ID3D11Device,
        dims: EncoderDims,
        bitrate_bps: u32,
    ) -> Result<Self, EncoderError> {
        Self::new_with_frame_rate(device, dims, bitrate_bps, EncoderFrameRate::default())
    }

    pub fn new_with_frame_rate(
        device: ID3D11Device,
        dims: EncoderDims,
        bitrate_bps: u32,
        frame_rate: EncoderFrameRate,
    ) -> Result<Self, EncoderError> {
        assert!(dims.width > 0, "width positive");
        assert!(dims.height > 0, "height positive");
        assert!(frame_rate.numerator > 0, "frame rate numerator positive");
        assert!(
            frame_rate.denominator > 0,
            "frame rate denominator positive"
        );
        if dims.width > 7680 || dims.height > 4320 {
            return Err(EncoderError::DimensionsOutOfRange {
                width: dims.width,
                height: dims.height,
            });
        }
        let library = load_runtime()?;
        let factory = init_factory(&library)?;
        let context = create_context(factory)?;
        init_dx11_context(context, &device)?;
        let encoder = create_video_encoder(factory, context, dims, bitrate_bps)?;
        let frame_interval_us = frame_rate.frame_interval_us();
        let dts_offset_us = compute_dts_offset_us(0, 0, frame_interval_us);
        let handoff = Self {
            _library: Arc::new(library),
            factory,
            context,
            encoder,
            slots: HashMap::new(),
            next_slot_index: 0,
            dts_offset_us,
            completed_count: 0,
            frame_interval_us,
        };
        assert!(!handoff.factory.is_null(), "factory non-null");
        assert!(handoff.completed_count == 0, "fresh state");
        Ok(handoff)
    }
}

fn load_runtime() -> Result<Library, EncoderError> {
    let library = unsafe { Library::new(AMF_DLL_NAME) }.map_err(|_| EncoderError::SdkNotFound {
        vendor: "amf",
        dll: AMF_DLL_NAME,
    })?;
    Ok(library)
}

fn init_factory(library: &Library) -> Result<*mut c_void, EncoderError> {
    let init: Symbol<'_, AmfInitFn> =
        unsafe { library.get(b"AMFInit\0") }.map_err(|_| EncoderError::SymbolMissing {
            vendor: "amf",
            symbol: "AMFInit",
        })?;
    let mut factory: *mut c_void = ptr::null_mut();
    const AMF_FULL_VERSION: u64 = (1_u64 << 48) | (4_u64 << 32) | (30_u64 << 16);
    let status = unsafe { init(AMF_FULL_VERSION, &mut factory) };
    if status != AMF_OK {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: status as i64,
        });
    }
    if factory.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: -1,
        });
    }
    Ok(factory)
}

fn create_context(factory: *mut c_void) -> Result<*mut c_void, EncoderError> {
    assert!(!factory.is_null(), "factory ptr non-null");
    let object = factory as *mut AmfFactoryObject;
    let vtbl = unsafe { (*object).vtbl };
    let mut context: *mut c_void = ptr::null_mut();
    let status = unsafe { ((*vtbl).create_context)(factory, &mut context) };
    if status != AMF_OK {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: status as i64,
        });
    }
    if context.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: -2,
        });
    }
    Ok(context)
}

fn init_dx11_context(context: *mut c_void, device: &ID3D11Device) -> Result<(), EncoderError> {
    assert!(!context.is_null(), "context non-null");
    let _ = device.as_raw();
    Ok(())
}

fn create_video_encoder(
    factory: *mut c_void,
    context: *mut c_void,
    dims: EncoderDims,
    bitrate_bps: u32,
) -> Result<*mut c_void, EncoderError> {
    assert!(!factory.is_null(), "factory non-null");
    assert!(!context.is_null(), "context non-null");
    assert!(dims.width > 0, "width positive");
    let _ = bitrate_bps;
    let component_id: Vec<u16> = "AMFVideoEncoderVCE_AVC\0".encode_utf16().collect();
    let object = factory as *mut AmfFactoryObject;
    let vtbl = unsafe { (*object).vtbl };
    let mut encoder: *mut c_void = ptr::null_mut();
    let status = unsafe {
        ((*vtbl).create_component)(factory, context, component_id.as_ptr(), &mut encoder)
    };
    if status != AMF_OK {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: status as i64,
        });
    }
    if encoder.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "amf",
            status: -3,
        });
    }
    Ok(encoder)
}

impl Drop for AmfD3D11Handoff {
    fn drop(&mut self) {
        self.slots.clear();
        if !self.factory.is_null() {
            let object = self.factory as *mut AmfFactoryObject;
            unsafe {
                let vtbl = (*object).vtbl;
                if !self.encoder.is_null() {
                    let _ = ((*vtbl).release)(self.encoder);
                    self.encoder = ptr::null_mut();
                }
                if !self.context.is_null() {
                    let _ = ((*vtbl).release)(self.context);
                    self.context = ptr::null_mut();
                }
                let _ = ((*vtbl).release)(self.factory);
            }
            self.factory = ptr::null_mut();
        }
    }
}

impl AmfHandoff for AmfD3D11Handoff {
    fn register_slot(
        &mut self,
        shared_handle: u64,
        _key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError> {
        assert!(shared_handle != 0, "shared_handle non-zero");
        assert!(dims.width > 0, "width positive");
        let slot_index = self.next_slot_index;
        self.next_slot_index = self.next_slot_index.saturating_add(1);
        let slot = HandoffSlot::new(slot_index, shared_handle);
        self.slots.insert(
            slot_index,
            SlotState {
                pending_pts_us: 0,
                pending_force_keyframe: false,
                in_flight: false,
            },
        );
        assert!(self.slots.contains_key(&slot_index), "slot stored");
        Ok(slot)
    }

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        _key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError> {
        assert!(slot.shared_handle != 0, "slot handle non-zero");
        assert!(dims.width > 0, "width positive");
        let state = self
            .slots
            .get_mut(&slot.slot_index)
            .ok_or(EncoderError::SlotUnknown {
                slot_index: slot.slot_index,
            })?;
        state.pending_pts_us = pic_params.pts_us;
        state.pending_force_keyframe = pic_params.force_keyframe;
        state.in_flight = true;
        Ok(())
    }

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream> {
        let state = self.slots.get_mut(&slot.slot_index)?;
        if !state.in_flight {
            return None;
        }
        state.in_flight = false;
        let pts = state.pending_pts_us;
        let dts = apply_dts_offset(pts, self.dts_offset_us);
        let _ = state.pending_force_keyframe || self.completed_count == 0;
        self.completed_count = self.completed_count.saturating_add(1);
        let _ = (pts, dts);
        None
    }

    fn unregister_slot(&mut self, slot: HandoffSlot) {
        self.slots.remove(&slot.slot_index);
    }

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError> {
        assert!(submission.shared_handle != 0, "submission handle non-zero");
        assert!(submission.dims.width > 0, "submission width positive");
        let slot = self
            .register_slot(
                submission.shared_handle,
                submission.keyed_mutex_key,
                submission.dims,
            )
            .map_err(|_| RingError::NotImplemented {
                what: "amf::register_slot in encode_shared",
            })?;
        let pts_us = submission
            .capture_pts_us
            .unwrap_or_else(|| submission.sequence.saturating_mul(self.frame_interval_us));
        let pic = PicParams::new(pts_us, false);
        AmfHandoff::encode_shared_async(
            self,
            slot,
            submission.keyed_mutex_key,
            submission.dims,
            pic,
        )
        .map_err(|_| RingError::NotImplemented {
            what: "amf::encode_shared_async",
        })?;
        if let Some(bs) = AmfHandoff::poll_completed(self, slot) {
            callback.on_complete(submission.sequence, bs.data.len() as u32);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn amf_status_constants_match_spec() {
        assert_eq!(AMF_OK, 0);
        assert_eq!(AMF_REPEAT, 5);
        assert_eq!(AMF_NOT_READY, 1);
    }
}
