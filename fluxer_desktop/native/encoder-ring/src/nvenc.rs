// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::sync::Arc;

use libloading::{Library, Symbol};
use windows::Win32::Foundation::{CloseHandle, HANDLE, WAIT_OBJECT_0};
use windows::Win32::Graphics::Direct3D11::{ID3D11Device, ID3D11Multithread};
use windows::Win32::System::Threading::{CreateEventW, WaitForSingleObject};
use windows::core::Interface;

use crate::encoder_handoff::{
    EncodedBitstream, EncoderCompletionCallback, EncoderDims, EncoderError, EncoderFrameRate,
    EncoderSubmission, HandoffSlot, MAX_BITSTREAM_BYTES, NvencHandoff, PicParams, apply_dts_offset,
    compute_dts_offset_us,
};
use crate::ring::RingError;

pub const COMPLETION_RING_CAPACITY: usize = 16;
pub const NVENC_SLOT_ID_PROBES: usize = 1024;
pub const NVENC_IN_FLIGHT_MAX: usize = 8;
const NVENC_PIPELINE_DEPTH: usize = 2;

pub const NVENC_DLL_NAME: &str = "nvEncodeAPI64.dll";

const NVENCAPI_MAJOR_VERSION: u32 = 13;
const NVENCAPI_MINOR_VERSION: u32 = 0;
const NVENCAPI_VERSION: u32 = NVENCAPI_MAJOR_VERSION | (NVENCAPI_MINOR_VERSION << 24);

const fn struct_version(ver: u32) -> u32 {
    NVENCAPI_VERSION | (ver << 16) | (0x7 << 28)
}

const NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER: u32 = struct_version(1);
const NV_ENCODE_API_FUNCTION_LIST_VER: u32 = struct_version(2);
const NV_ENC_INITIALIZE_PARAMS_VER: u32 = struct_version(7) | (1 << 31);
const NV_ENC_CONFIG_VER: u32 = struct_version(9) | (1 << 31);
const NV_ENC_PRESET_CONFIG_VER: u32 = struct_version(5) | (1 << 31);
const NV_ENC_PIC_PARAMS_VER: u32 = struct_version(7) | (1 << 31);
const NV_ENC_LOCK_BITSTREAM_VER: u32 = struct_version(2) | (1 << 31);
const NV_ENC_MAP_INPUT_RESOURCE_VER: u32 = struct_version(4);
const NV_ENC_REGISTER_RESOURCE_VER: u32 = struct_version(5);
const NV_ENC_EVENT_PARAMS_VER: u32 = struct_version(2);
const NV_ENC_CREATE_BITSTREAM_BUFFER_VER: u32 = struct_version(1);

const NV_ENC_DEVICE_TYPE_DIRECTX: u32 = 0;
const NV_ENC_INPUT_RESOURCE_TYPE_DIRECTX: u32 = 0;
const NV_ENC_BUFFER_FORMAT_NV12: u32 = 0x00000001;
const NV_ENC_PIC_STRUCT_FRAME: u32 = 0x01;
const NV_ENC_PIC_TYPE_IDR: u32 = 0x05;
const NV_ENC_PIC_FLAG_FORCEIDR: u32 = 0x2;
const NV_ENC_PIC_FLAG_OUTPUT_SPSPPS: u32 = 0x4;
#[allow(
    dead_code,
    reason = "documented rate-control mode constant per NVENC SDK reference"
)]
const NV_ENC_PARAMS_RC_CBR: u32 = 0x2;
const NV_ENC_TUNING_INFO_LOW_LATENCY: u32 = 2;
const NV_ENC_SUCCESS: i32 = 0;

const NV_ENC_CODEC_H264_GUID: Guid = Guid {
    data1: 0x6bc82762,
    data2: 0x4e63,
    data3: 0x4ca4,
    data4: [0xaa, 0x85, 0x1e, 0x50, 0xf3, 0x21, 0xf6, 0xbf],
};
const NV_ENC_PRESET_P3_GUID: Guid = Guid {
    data1: 0x47bcf4d8,
    data2: 0xb1e6,
    data3: 0x47ca,
    data4: [0x9d, 0x9c, 0xdf, 0x9f, 0xb6, 0xfa, 0x29, 0xed],
};

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Guid {
    data1: u32,
    data2: u16,
    data3: u16,
    data4: [u8; 8],
}

#[repr(C)]
struct OpenEncodeSessionExParams {
    version: u32,
    device_type: u32,
    device: *mut c_void,
    reserved: *mut c_void,
    api_version: u32,
    reserved1: [u32; 253],
    reserved2: [*mut c_void; 64],
}

#[repr(C)]
struct EventParams {
    version: u32,
    reserved: u32,
    completion_event: *mut c_void,
    reserved1: [u32; 254],
    reserved2: [*mut c_void; 64],
}

#[repr(C)]
struct RegisterResource {
    version: u32,
    resource_type: u32,
    width: u32,
    height: u32,
    pitch: u32,
    subresource_index: u32,
    resource_to_register: *mut c_void,
    registered_resource: *mut c_void,
    buffer_format: u32,
    buffer_usage: u32,
    pinput_fence_point: *mut c_void,
    chroma_offset: [u32; 2],
    reserved1: [u32; 247],
    reserved2: [*mut c_void; 28],
}

#[repr(C)]
struct MapInputResource {
    version: u32,
    subresource_index: u32,
    input_resource: *mut c_void,
    registered_resource: *mut c_void,
    mapped_resource: *mut c_void,
    mapped_buffer_fmt: u32,
    reserved1: [u32; 251],
    reserved2: [*mut c_void; 63],
}

#[repr(C)]
struct CreateBitstreamBuffer {
    version: u32,
    size: u32,
    memory_heap: u32,
    reserved: u32,
    bitstream_buffer: *mut c_void,
    bitstream_buffer_ptr: *mut c_void,
    reserved1: [u32; 58],
    reserved2: [*mut c_void; 64],
}

#[repr(C)]
struct LockBitstream {
    version: u32,
    flags: u32,
    output_bitstream: *mut c_void,
    slice_offsets: *mut u32,
    frame_idx: u32,
    h_w_encode_status: u32,
    num_slices: u32,
    bitstream_size_in_bytes: u32,
    output_time_stamp: u64,
    output_duration: u64,
    bitstream_buffer_ptr: *mut c_void,
    picture_type: u32,
    picture_struct: u32,
    frame_avg_qp: u32,
    frame_satd: u32,
    ltr_frame_idx: u32,
    ltr_frame_bitmap: u32,
    temporal_id: u32,
    reserved: [u32; 12],
    intra_mb_count: u32,
    inter_mb_count: u32,
    average_mvx: i32,
    average_mvy: i32,
    alpha_layer_size_in_bytes: u32,
    reserved1: [u32; 218],
    reserved2: [*mut c_void; 64],
}

#[repr(C)]
struct PicParamsRaw {
    version: u32,
    input_width: u32,
    input_height: u32,
    input_pitch: u32,
    encode_pic_flags: u32,
    frame_idx: u32,
    input_timestamp: u64,
    input_duration: u64,
    input_buffer: *mut c_void,
    output_bitstream: *mut c_void,
    completion_event: *mut c_void,
    buffer_fmt: u32,
    picture_struct: u32,
    picture_type: u32,
    codec_pic_params: [u8; 256],
    rc_params: [u8; 1024],
    qp_delta_map: *mut i8,
    qp_delta_map_size: u32,
    reserved3: u32,
    me_hint_counts_per_block: [u32; 2],
    me_external_hints: *mut c_void,
    reserved1: [u32; 6],
    reserved2: [*mut c_void; 2],
    me_hint_ref_pic_dpb_idx: [i8; 2],
    state_buffer_idx: u8,
    reserved4: [u8; 5],
    new_sps_pps_buffer: *mut c_void,
    reserved5: [u32; 246],
    reserved6: [*mut c_void; 60],
}

const CONFIG_BYTES: usize = 1024;
const PRESET_CONFIG_BYTES: usize = 8 + CONFIG_BYTES;

#[repr(C)]
struct InitializeParams {
    version: u32,
    encode_guid: Guid,
    preset_guid: Guid,
    encode_width: u32,
    encode_height: u32,
    dar_width: u32,
    dar_height: u32,
    frame_rate_num: u32,
    frame_rate_den: u32,
    enable_encode_async: u32,
    enable_ptd: u32,
    bitfields: u32,
    priv_data_size: u32,
    reserved: u32,
    priv_data: *mut c_void,
    encode_config: *mut c_void,
    max_encode_width: u32,
    max_encode_height: u32,
    max_me_hint_counts_per_block: [u8; 32],
    tuning_info: u32,
    buffer_format: u32,
    num_state_buffers: u32,
    output_stats_level: u32,
    reserved1: [u32; 284],
    reserved2: [*mut c_void; 64],
}

type StatusFn0 = unsafe extern "C" fn(*mut OpenEncodeSessionExParams, *mut *mut c_void) -> i32;
type DestroyEncoderFn = unsafe extern "C" fn(*mut c_void) -> i32;
type GetPresetConfigFn = unsafe extern "C" fn(*mut c_void, Guid, Guid, u32, *mut u8) -> i32;
type InitFn = unsafe extern "C" fn(*mut c_void, *mut u8) -> i32;
type RegisterFn = unsafe extern "C" fn(*mut c_void, *mut RegisterResource) -> i32;
type UnregisterFn = unsafe extern "C" fn(*mut c_void, *mut c_void) -> i32;
type MapFn = unsafe extern "C" fn(*mut c_void, *mut MapInputResource) -> i32;
type UnmapFn = unsafe extern "C" fn(*mut c_void, *mut c_void) -> i32;
type CreateBufFn = unsafe extern "C" fn(*mut c_void, *mut CreateBitstreamBuffer) -> i32;
type DestroyBufFn = unsafe extern "C" fn(*mut c_void, *mut c_void) -> i32;
type EncodePicFn = unsafe extern "C" fn(*mut c_void, *mut PicParamsRaw) -> i32;
type LockBitstreamFn = unsafe extern "C" fn(*mut c_void, *mut LockBitstream) -> i32;
type UnlockBitstreamFn = unsafe extern "C" fn(*mut c_void, *mut c_void) -> i32;
type RegisterAsyncEventFn = unsafe extern "C" fn(*mut c_void, *mut EventParams) -> i32;
type UnregisterAsyncEventFn = unsafe extern "C" fn(*mut c_void, *mut EventParams) -> i32;

#[repr(C)]
struct ApiFunctionList {
    version: u32,
    reserved: u32,
    fns: [*mut c_void; 64],
    reserved2: [*mut c_void; 275],
}

const FN_IDX_INITIALIZE_ENCODER: usize = 10;
const FN_IDX_CREATE_BITSTREAM_BUFFER: usize = 13;
const FN_IDX_DESTROY_BITSTREAM_BUFFER: usize = 14;
const FN_IDX_ENCODE_PICTURE: usize = 15;
const FN_IDX_LOCK_BITSTREAM: usize = 16;
const FN_IDX_UNLOCK_BITSTREAM: usize = 17;
const FN_IDX_REGISTER_ASYNC_EVENT: usize = 22;
const FN_IDX_UNREGISTER_ASYNC_EVENT: usize = 23;
const FN_IDX_MAP_INPUT_RESOURCE: usize = 24;
const FN_IDX_UNMAP_INPUT_RESOURCE: usize = 25;
const FN_IDX_DESTROY_ENCODER: usize = 26;
const FN_IDX_OPEN_ENCODE_SESSION_EX: usize = 28;
const FN_IDX_REGISTER_RESOURCE: usize = 29;
const FN_IDX_UNREGISTER_RESOURCE: usize = 30;
const FN_IDX_GET_PRESET_CONFIG_EX: usize = 38;

struct ApiTable {
    initialize: InitFn,
    create_buf: CreateBufFn,
    destroy_buf: DestroyBufFn,
    encode_pic: EncodePicFn,
    lock_bs: LockBitstreamFn,
    unlock_bs: UnlockBitstreamFn,
    register_event: RegisterAsyncEventFn,
    unregister_event: UnregisterAsyncEventFn,
    map_input: MapFn,
    unmap_input: UnmapFn,
    destroy_encoder: DestroyEncoderFn,
    open_session_ex: StatusFn0,
    register_resource: RegisterFn,
    unregister_resource: UnregisterFn,
    get_preset_config: GetPresetConfigFn,
}

struct SlotState {
    registered_resource: *mut c_void,
    completion_event: HANDLE,
    bitstream_buffer: *mut c_void,
    pending_pts_us: u64,
    pending_force_keyframe: bool,
    in_flight: bool,
}

pub struct NvencD3D11Handoff {
    _library: Arc<Library>,
    api: ApiTable,
    encoder: *mut c_void,
    slots: HashMap<u32, SlotState>,
    handle_to_slot: HashMap<u64, u32>,
    in_flight_order: VecDeque<u32>,
    next_slot_index: u32,
    dts_offset_us: i64,
    first_frame_pts_us: Option<u64>,
    completed_count: u64,
    completed_ring: VecDeque<EncodedBitstream>,
    dropped_count: u64,
    frame_interval_us: u64,
}

unsafe impl Send for NvencD3D11Handoff {}

impl NvencD3D11Handoff {
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
        assert!(dims.width > 0, "dims width positive");
        assert!(dims.height > 0, "dims height positive");
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
        enable_multithread_protected(&device)?;
        let library = load_runtime()?;
        let api = load_function_list(&library)?;
        let raw_device_ptr = device.as_raw();
        let encoder = open_session(&api, raw_device_ptr)?;
        let frame_interval_us = frame_rate.frame_interval_us();
        let mut handoff = Self {
            _library: Arc::new(library),
            api,
            encoder,
            slots: HashMap::new(),
            handle_to_slot: HashMap::new(),
            in_flight_order: VecDeque::with_capacity(NVENC_IN_FLIGHT_MAX),
            next_slot_index: 0,
            dts_offset_us: compute_dts_offset_us(0, 0, frame_interval_us),
            first_frame_pts_us: None,
            completed_count: 0,
            completed_ring: VecDeque::with_capacity(COMPLETION_RING_CAPACITY),
            dropped_count: 0,
            frame_interval_us,
        };
        initialize_encoder(
            &handoff.api,
            handoff.encoder,
            dims,
            bitrate_bps,
            frame_rate.numerator,
            frame_rate.denominator,
        )?;
        let _ = &mut handoff;
        assert!(!handoff.encoder.is_null(), "encoder ptr non-null");
        assert!(
            handoff.completed_ring.capacity() >= COMPLETION_RING_CAPACITY,
            "ring pre-allocated"
        );
        Ok(handoff)
    }

    pub fn pending_completion(&self) -> usize {
        let n = self.completed_ring.len();
        assert!(n <= COMPLETION_RING_CAPACITY, "completion ring bounded");
        assert!(
            self.completed_count <= u64::MAX / 2,
            "completed count plausible"
        );
        n
    }

    fn allocate_slot_index(&mut self) -> Option<u32> {
        assert!(self.slots.len() < usize::MAX, "slot map count plausible");
        for _ in 0..NVENC_SLOT_ID_PROBES {
            let candidate = self.next_slot_index;
            self.next_slot_index = candidate.wrapping_add(1);
            if !self.slots.contains_key(&candidate) {
                return Some(candidate);
            }
        }
        None
    }

    pub fn completed_count(&self) -> u64 {
        let n = self.completed_count;
        assert!(
            self.completed_ring.len() <= COMPLETION_RING_CAPACITY,
            "ring bounded"
        );
        n
    }

    fn register_slot_impl(
        &mut self,
        shared_handle: u64,
        _key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError> {
        assert!(shared_handle != 0, "shared_handle non-zero");
        assert!(dims.width > 0, "dims width positive");
        let texture_ptr = shared_handle as *mut c_void;
        let mut reg = RegisterResource {
            version: NV_ENC_REGISTER_RESOURCE_VER,
            resource_type: NV_ENC_INPUT_RESOURCE_TYPE_DIRECTX,
            width: dims.width,
            height: dims.height,
            pitch: 0,
            subresource_index: 0,
            resource_to_register: texture_ptr,
            registered_resource: ptr::null_mut(),
            buffer_format: NV_ENC_BUFFER_FORMAT_NV12,
            buffer_usage: 0,
            pinput_fence_point: ptr::null_mut(),
            chroma_offset: [0; 2],
            reserved1: [0; 247],
            reserved2: [ptr::null_mut(); 28],
        };
        let status = unsafe { (self.api.register_resource)(self.encoder, &mut reg) };
        if status != NV_ENC_SUCCESS {
            return Err(EncoderError::RegisterFailed {
                vendor: "nvenc",
                status: status as i64,
            });
        }
        let event = create_completion_event()?;
        register_async_event(&self.api, self.encoder, event)?;
        let buffer = create_bitstream_buffer(&self.api, self.encoder)?;
        let slot_index = self
            .allocate_slot_index()
            .ok_or(EncoderError::RegisterFailed {
                vendor: "nvenc-slot-exhausted",
                status: 0,
            })?;
        let slot = HandoffSlot::new(slot_index, shared_handle);
        self.slots.insert(
            slot_index,
            SlotState {
                registered_resource: reg.registered_resource,
                completion_event: event,
                bitstream_buffer: buffer,
                pending_pts_us: 0,
                pending_force_keyframe: false,
                in_flight: false,
            },
        );
        self.handle_to_slot.insert(shared_handle, slot_index);
        let _ = slot;
        assert!(self.slots.contains_key(&slot_index), "slot stored");
        assert!(
            self.handle_to_slot.contains_key(&shared_handle),
            "handle index stored"
        );
        Ok(slot)
    }

    fn drain_one_completed(&mut self, timeout_ms: u32) -> Option<EncodedBitstream> {
        assert!(
            self.in_flight_order.len() <= self.slots.len(),
            "in-flight bounded by registered slots"
        );
        let oldest = *self.in_flight_order.front()?;
        let state = self.slots.get_mut(&oldest)?;
        assert!(state.in_flight, "front of in-flight order is in flight");
        let wait = unsafe { WaitForSingleObject(state.completion_event, timeout_ms) };
        if wait != WAIT_OBJECT_0 {
            return None;
        }
        let lock_result = lock_bitstream(&self.api, self.encoder, state.bitstream_buffer);
        state.in_flight = false;
        let popped = self.in_flight_order.pop_front();
        assert_eq!(popped, Some(oldest), "popped oldest in-flight slot");
        let (data, pts, is_keyframe) = match lock_result {
            Ok(t) => t,
            Err(_) => return None,
        };
        if data.is_empty() {
            return None;
        }
        let dts = apply_dts_offset(pts, self.dts_offset_us);
        self.completed_count = self.completed_count.saturating_add(1);
        let bs =
            match std::panic::catch_unwind(|| EncodedBitstream::new(data, pts, dts, is_keyframe)) {
                Ok(b) => b,
                Err(_) => return None,
            };
        Some(bs)
    }

    fn drain_completions_into_ring(&mut self) {
        while self.completed_ring.len() < COMPLETION_RING_CAPACITY {
            let Some(bs) = self.drain_one_completed(0) else {
                break;
            };
            self.completed_ring.push_back(bs);
        }
        assert!(
            self.completed_ring.len() <= COMPLETION_RING_CAPACITY,
            "completion ring bounded"
        );
        assert!(
            self.in_flight_order.len() <= self.slots.len(),
            "in-flight bounded by registered slots"
        );
    }

    pub fn pre_register_slots(
        &mut self,
        handles: &[crate::d3d11::D3D11SharedHandle],
        dims: EncoderDims,
    ) -> Result<(), EncoderError> {
        assert!(
            !handles.is_empty(),
            "pre-register needs at least one handle"
        );
        assert!(
            handles.len() <= crate::ring::RING_SIZE_MAX,
            "pre-register bounded by ring max"
        );
        for handle in handles {
            if self.handle_to_slot.contains_key(&handle.raw_handle) {
                continue;
            }
            let _ = self.register_slot_impl(handle.raw_handle, 0, dims)?;
        }
        assert!(
            handles
                .iter()
                .all(|h| self.handle_to_slot.contains_key(&h.raw_handle)),
            "all ring handles registered"
        );
        Ok(())
    }
}

fn enable_multithread_protected(device: &ID3D11Device) -> Result<(), EncoderError> {
    let mt: ID3D11Multithread = device.cast().map_err(|_| EncoderError::SessionInitFailed {
        vendor: "nvenc-mt",
        status: -3,
    })?;
    let _ = unsafe { mt.SetMultithreadProtected(true) };
    Ok(())
}

fn load_runtime() -> Result<Library, EncoderError> {
    let library =
        unsafe { Library::new(NVENC_DLL_NAME) }.map_err(|_| EncoderError::SdkNotFound {
            vendor: "nvenc",
            dll: NVENC_DLL_NAME,
        })?;
    Ok(library)
}

fn load_function_list(library: &Library) -> Result<ApiTable, EncoderError> {
    type CreateInstanceFn = unsafe extern "C" fn(*mut ApiFunctionList) -> i32;
    let create_instance: Symbol<'_, CreateInstanceFn> = unsafe {
        library.get(b"NvEncodeAPICreateInstance\0")
    }
    .map_err(|_| EncoderError::SymbolMissing {
        vendor: "nvenc",
        symbol: "NvEncodeAPICreateInstance",
    })?;
    let mut list = ApiFunctionList {
        version: NV_ENCODE_API_FUNCTION_LIST_VER,
        reserved: 0,
        fns: [ptr::null_mut(); 64],
        reserved2: [ptr::null_mut(); 275],
    };
    let status = unsafe { create_instance(&mut list) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    let api = build_api_table(&list)?;
    Ok(api)
}

fn build_api_table(list: &ApiFunctionList) -> Result<ApiTable, EncoderError> {
    let required_indices = [
        FN_IDX_INITIALIZE_ENCODER,
        FN_IDX_CREATE_BITSTREAM_BUFFER,
        FN_IDX_DESTROY_BITSTREAM_BUFFER,
        FN_IDX_ENCODE_PICTURE,
        FN_IDX_LOCK_BITSTREAM,
        FN_IDX_UNLOCK_BITSTREAM,
        FN_IDX_REGISTER_ASYNC_EVENT,
        FN_IDX_UNREGISTER_ASYNC_EVENT,
        FN_IDX_MAP_INPUT_RESOURCE,
        FN_IDX_UNMAP_INPUT_RESOURCE,
        FN_IDX_DESTROY_ENCODER,
        FN_IDX_OPEN_ENCODE_SESSION_EX,
        FN_IDX_REGISTER_RESOURCE,
        FN_IDX_UNREGISTER_RESOURCE,
        FN_IDX_GET_PRESET_CONFIG_EX,
    ];
    for idx in required_indices.iter() {
        if list.fns[*idx].is_null() {
            return Err(EncoderError::SymbolMissing {
                vendor: "nvenc",
                symbol: "function_list entry",
            });
        }
    }
    let api = unsafe {
        ApiTable {
            initialize: std::mem::transmute::<*mut c_void, InitFn>(
                list.fns[FN_IDX_INITIALIZE_ENCODER],
            ),
            create_buf: std::mem::transmute::<*mut c_void, CreateBufFn>(
                list.fns[FN_IDX_CREATE_BITSTREAM_BUFFER],
            ),
            destroy_buf: std::mem::transmute::<*mut c_void, DestroyBufFn>(
                list.fns[FN_IDX_DESTROY_BITSTREAM_BUFFER],
            ),
            encode_pic: std::mem::transmute::<*mut c_void, EncodePicFn>(
                list.fns[FN_IDX_ENCODE_PICTURE],
            ),
            lock_bs: std::mem::transmute::<*mut c_void, LockBitstreamFn>(
                list.fns[FN_IDX_LOCK_BITSTREAM],
            ),
            unlock_bs: std::mem::transmute::<*mut c_void, UnlockBitstreamFn>(
                list.fns[FN_IDX_UNLOCK_BITSTREAM],
            ),
            register_event: std::mem::transmute::<*mut c_void, RegisterAsyncEventFn>(
                list.fns[FN_IDX_REGISTER_ASYNC_EVENT],
            ),
            unregister_event: std::mem::transmute::<*mut c_void, UnregisterAsyncEventFn>(
                list.fns[FN_IDX_UNREGISTER_ASYNC_EVENT],
            ),
            map_input: std::mem::transmute::<*mut c_void, MapFn>(
                list.fns[FN_IDX_MAP_INPUT_RESOURCE],
            ),
            unmap_input: std::mem::transmute::<*mut c_void, UnmapFn>(
                list.fns[FN_IDX_UNMAP_INPUT_RESOURCE],
            ),
            destroy_encoder: std::mem::transmute::<*mut c_void, DestroyEncoderFn>(
                list.fns[FN_IDX_DESTROY_ENCODER],
            ),
            open_session_ex: std::mem::transmute::<*mut c_void, StatusFn0>(
                list.fns[FN_IDX_OPEN_ENCODE_SESSION_EX],
            ),
            register_resource: std::mem::transmute::<*mut c_void, RegisterFn>(
                list.fns[FN_IDX_REGISTER_RESOURCE],
            ),
            unregister_resource: std::mem::transmute::<*mut c_void, UnregisterFn>(
                list.fns[FN_IDX_UNREGISTER_RESOURCE],
            ),
            get_preset_config: std::mem::transmute::<*mut c_void, GetPresetConfigFn>(
                list.fns[FN_IDX_GET_PRESET_CONFIG_EX],
            ),
        }
    };
    Ok(api)
}

fn open_session(api: &ApiTable, device: *mut c_void) -> Result<*mut c_void, EncoderError> {
    let mut params = OpenEncodeSessionExParams {
        version: NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER,
        device_type: NV_ENC_DEVICE_TYPE_DIRECTX,
        device,
        reserved: ptr::null_mut(),
        api_version: NVENCAPI_VERSION,
        reserved1: [0; 253],
        reserved2: [ptr::null_mut(); 64],
    };
    let mut encoder: *mut c_void = ptr::null_mut();
    let status = unsafe { (api.open_session_ex)(&mut params, &mut encoder) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc-open-session",
            status: status as i64,
        });
    }
    if encoder.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc-open-session",
            status: -1,
        });
    }
    Ok(encoder)
}

fn initialize_encoder(
    api: &ApiTable,
    encoder: *mut c_void,
    dims: EncoderDims,
    _bitrate_bps: u32,
    fps_num: u32,
    fps_den: u32,
) -> Result<(), EncoderError> {
    assert!(!encoder.is_null(), "encoder ptr non-null");
    assert!(dims.width > 0, "width positive");
    let mut preset = vec![0u8; PRESET_CONFIG_BYTES];
    write_u32(&mut preset, 0, NV_ENC_PRESET_CONFIG_VER);
    write_u32(&mut preset, 8, NV_ENC_CONFIG_VER);
    let status = unsafe {
        (api.get_preset_config)(
            encoder,
            NV_ENC_CODEC_H264_GUID,
            NV_ENC_PRESET_P3_GUID,
            NV_ENC_TUNING_INFO_LOW_LATENCY,
            preset.as_mut_ptr(),
        )
    };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc-preset",
            status: status as i64,
        });
    }
    let mut init_params = InitializeParams {
        version: NV_ENC_INITIALIZE_PARAMS_VER,
        encode_guid: NV_ENC_CODEC_H264_GUID,
        preset_guid: NV_ENC_PRESET_P3_GUID,
        encode_width: dims.width,
        encode_height: dims.height,
        dar_width: dims.width,
        dar_height: dims.height,
        frame_rate_num: fps_num,
        frame_rate_den: fps_den,
        enable_encode_async: 1,
        enable_ptd: 1,
        bitfields: 0,
        priv_data_size: 0,
        reserved: 0,
        priv_data: ptr::null_mut(),
        encode_config: ptr::null_mut(),
        max_encode_width: dims.width,
        max_encode_height: dims.height,
        max_me_hint_counts_per_block: [0; 32],
        tuning_info: NV_ENC_TUNING_INFO_LOW_LATENCY,
        buffer_format: NV_ENC_BUFFER_FORMAT_NV12,
        num_state_buffers: 0,
        output_stats_level: 0,
        reserved1: [0; 284],
        reserved2: [ptr::null_mut(); 64],
    };
    let status = unsafe { (api.initialize)(encoder, &mut init_params as *mut _ as *mut u8) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc-init",
            status: status as i64,
        });
    }
    Ok(())
}

fn write_u32(buf: &mut [u8], offset: usize, value: u32) {
    assert!(offset + 4 <= buf.len(), "u32 write in bounds");
    buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn create_completion_event() -> Result<HANDLE, EncoderError> {
    let event = unsafe { CreateEventW(None, false, false, windows::core::PCWSTR::null()) }
        .map_err(|_| EncoderError::SessionInitFailed {
            vendor: "nvenc",
            status: -2,
        })?;
    assert!(!event.is_invalid(), "event handle valid");
    Ok(event)
}

fn register_async_event(
    api: &ApiTable,
    encoder: *mut c_void,
    event: HANDLE,
) -> Result<(), EncoderError> {
    let mut params = EventParams {
        version: NV_ENC_EVENT_PARAMS_VER,
        reserved: 0,
        completion_event: event.0,
        reserved1: [0; 254],
        reserved2: [ptr::null_mut(); 64],
    };
    let status = unsafe { (api.register_event)(encoder, &mut params) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    Ok(())
}

fn create_bitstream_buffer(
    api: &ApiTable,
    encoder: *mut c_void,
) -> Result<*mut c_void, EncoderError> {
    let mut params = CreateBitstreamBuffer {
        version: NV_ENC_CREATE_BITSTREAM_BUFFER_VER,
        size: 0,
        memory_heap: 0,
        reserved: 0,
        bitstream_buffer: ptr::null_mut(),
        bitstream_buffer_ptr: ptr::null_mut(),
        reserved1: [0; 58],
        reserved2: [ptr::null_mut(); 64],
    };
    let status = unsafe { (api.create_buf)(encoder, &mut params) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::SessionInitFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    Ok(params.bitstream_buffer)
}

fn map_input(
    api: &ApiTable,
    encoder: *mut c_void,
    registered: *mut c_void,
) -> Result<*mut c_void, EncoderError> {
    let mut params = MapInputResource {
        version: NV_ENC_MAP_INPUT_RESOURCE_VER,
        subresource_index: 0,
        input_resource: ptr::null_mut(),
        registered_resource: registered,
        mapped_resource: ptr::null_mut(),
        mapped_buffer_fmt: 0,
        reserved1: [0; 251],
        reserved2: [ptr::null_mut(); 63],
    };
    let status = unsafe { (api.map_input)(encoder, &mut params) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::EncodeFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    Ok(params.mapped_resource)
}

struct EncodePictureArgs {
    input: *mut c_void,
    output: *mut c_void,
    event: HANDLE,
    dims: EncoderDims,
    pts_us: u64,
    force_keyframe: bool,
}

fn encode_picture(
    api: &ApiTable,
    encoder: *mut c_void,
    args: EncodePictureArgs,
) -> Result<(), EncoderError> {
    assert!(args.dims.width > 0, "dims width positive");
    assert!(!args.input.is_null(), "input ptr non-null");
    let mut flags = 0_u32;
    if args.force_keyframe {
        flags |= NV_ENC_PIC_FLAG_FORCEIDR | NV_ENC_PIC_FLAG_OUTPUT_SPSPPS;
    }
    let mut params = PicParamsRaw {
        version: NV_ENC_PIC_PARAMS_VER,
        input_width: args.dims.width,
        input_height: args.dims.height,
        input_pitch: args.dims.width,
        encode_pic_flags: flags,
        frame_idx: 0,
        input_timestamp: args.pts_us,
        input_duration: 0,
        input_buffer: args.input,
        output_bitstream: args.output,
        completion_event: args.event.0,
        buffer_fmt: NV_ENC_BUFFER_FORMAT_NV12,
        picture_struct: NV_ENC_PIC_STRUCT_FRAME,
        picture_type: NV_ENC_PIC_TYPE_IDR,
        codec_pic_params: [0; 256],
        rc_params: [0; 1024],
        qp_delta_map: ptr::null_mut(),
        qp_delta_map_size: 0,
        reserved3: 0,
        me_hint_counts_per_block: [0; 2],
        me_external_hints: ptr::null_mut(),
        reserved1: [0; 6],
        reserved2: [ptr::null_mut(); 2],
        me_hint_ref_pic_dpb_idx: [-1; 2],
        state_buffer_idx: 0,
        reserved4: [0; 5],
        new_sps_pps_buffer: ptr::null_mut(),
        reserved5: [0; 246],
        reserved6: [ptr::null_mut(); 60],
    };
    let status = unsafe { (api.encode_pic)(encoder, &mut params) };
    if status != NV_ENC_SUCCESS && status != 1 {
        return Err(EncoderError::EncodeFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    Ok(())
}

fn lock_bitstream(
    api: &ApiTable,
    encoder: *mut c_void,
    output: *mut c_void,
) -> Result<(Vec<u8>, u64, bool), EncoderError> {
    let mut params = LockBitstream {
        version: NV_ENC_LOCK_BITSTREAM_VER,
        flags: 1,
        output_bitstream: output,
        slice_offsets: ptr::null_mut(),
        frame_idx: 0,
        h_w_encode_status: 0,
        num_slices: 0,
        bitstream_size_in_bytes: 0,
        output_time_stamp: 0,
        output_duration: 0,
        bitstream_buffer_ptr: ptr::null_mut(),
        picture_type: 0,
        picture_struct: 0,
        frame_avg_qp: 0,
        frame_satd: 0,
        ltr_frame_idx: 0,
        ltr_frame_bitmap: 0,
        temporal_id: 0,
        reserved: [0; 12],
        intra_mb_count: 0,
        inter_mb_count: 0,
        average_mvx: 0,
        average_mvy: 0,
        alpha_layer_size_in_bytes: 0,
        reserved1: [0; 218],
        reserved2: [ptr::null_mut(); 64],
    };
    let status = unsafe { (api.lock_bs)(encoder, &mut params) };
    if status != NV_ENC_SUCCESS {
        return Err(EncoderError::BitstreamReadFailed {
            vendor: "nvenc",
            status: status as i64,
        });
    }
    let len = params.bitstream_size_in_bytes as usize;
    assert!(len <= 16 * 1024 * 1024, "bitstream within sanity cap");
    let mut data: Vec<u8> = Vec::with_capacity(len);
    if len > 0 && !params.bitstream_buffer_ptr.is_null() {
        let src =
            unsafe { std::slice::from_raw_parts(params.bitstream_buffer_ptr as *const u8, len) };
        data.extend_from_slice(src);
    }
    let pts = params.output_time_stamp;
    let is_idr = params.picture_type == NV_ENC_PIC_TYPE_IDR || (params.picture_type & 0x0F) == 0;
    let _ = unsafe { (api.unlock_bs)(encoder, output) };
    Ok((data, pts, is_idr))
}

impl Drop for NvencD3D11Handoff {
    fn drop(&mut self) {
        let keys: Vec<u32> = self.slots.keys().copied().collect();
        for key in keys {
            if let Some(state) = self.slots.remove(&key) {
                self.cleanup_slot(state);
            }
        }
        self.handle_to_slot.clear();
        self.in_flight_order.clear();
        self.completed_ring.clear();
        if !self.encoder.is_null() {
            let _ = unsafe { (self.api.destroy_encoder)(self.encoder) };
            self.encoder = ptr::null_mut();
        }
    }
}

impl NvencD3D11Handoff {
    fn cleanup_slot(&self, state: SlotState) {
        if !state.bitstream_buffer.is_null() {
            let _ = unsafe { (self.api.destroy_buf)(self.encoder, state.bitstream_buffer) };
        }
        if !state.registered_resource.is_null() {
            let _ =
                unsafe { (self.api.unregister_resource)(self.encoder, state.registered_resource) };
        }
        if !state.completion_event.is_invalid() {
            let mut params = EventParams {
                version: NV_ENC_EVENT_PARAMS_VER,
                reserved: 0,
                completion_event: state.completion_event.0,
                reserved1: [0; 254],
                reserved2: [ptr::null_mut(); 64],
            };
            let _ = unsafe { (self.api.unregister_event)(self.encoder, &mut params) };
            let _ = unsafe { CloseHandle(state.completion_event) };
        }
    }
}

impl NvencHandoff for NvencD3D11Handoff {
    fn register_slot(
        &mut self,
        shared_handle: u64,
        key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError> {
        self.register_slot_impl(shared_handle, key, dims)
    }

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        _key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError> {
        assert!(dims.width > 0, "dims width positive");
        assert!(slot.shared_handle != 0, "slot handle non-zero");
        let state = self
            .slots
            .get_mut(&slot.slot_index)
            .ok_or(EncoderError::SlotUnknown {
                slot_index: slot.slot_index,
            })?;
        if state.in_flight {
            return Err(EncoderError::EncodeFailed {
                vendor: "nvenc",
                status: -10,
            });
        }
        let mapped = map_input(&self.api, self.encoder, state.registered_resource)?;
        if self.first_frame_pts_us.is_none() {
            self.first_frame_pts_us = Some(pic_params.pts_us);
        }
        let result = encode_picture(
            &self.api,
            self.encoder,
            EncodePictureArgs {
                input: mapped,
                output: state.bitstream_buffer,
                event: state.completion_event,
                dims,
                pts_us: pic_params.pts_us,
                force_keyframe: pic_params.force_keyframe || self.completed_count == 0,
            },
        );
        let _ = unsafe { (self.api.unmap_input)(self.encoder, mapped) };
        result?;
        state.pending_pts_us = pic_params.pts_us;
        state.pending_force_keyframe = pic_params.force_keyframe;
        state.in_flight = true;
        self.in_flight_order.push_back(slot.slot_index);
        assert!(
            self.in_flight_order.len() <= self.slots.len(),
            "in-flight bounded by registered slots"
        );
        assert_eq!(
            self.in_flight_order.back(),
            Some(&slot.slot_index),
            "newest submission at back of in-flight order"
        );
        Ok(())
    }

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream> {
        let _ = slot;
        if let Some(bs) = self.completed_ring.pop_front() {
            assert!(!bs.data.is_empty(), "ring payload non-empty");
            assert!(bs.data.len() <= MAX_BITSTREAM_BYTES, "ring payload bounded");
            return Some(bs);
        }
        let drained = self.drain_one_completed(0);
        if let Some(bs) = drained {
            assert!(!bs.data.is_empty(), "drained payload non-empty");
            return Some(bs);
        }
        None
    }

    fn unregister_slot(&mut self, slot: HandoffSlot) {
        if let Some(state) = self.slots.remove(&slot.slot_index) {
            self.handle_to_slot.remove(&slot.shared_handle);
            self.in_flight_order.retain(|&idx| idx != slot.slot_index);
            self.cleanup_slot(state);
        }
    }

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError> {
        assert!(submission.shared_handle != 0, "submission handle non-zero");
        assert!(submission.dims.width > 0, "submission width positive");
        self.drain_completions_into_ring();
        if self.completed_ring.len() >= COMPLETION_RING_CAPACITY {
            self.dropped_count = self.dropped_count.saturating_add(1);
            return Err(RingError::FullDropped {
                dropped_so_far: self.dropped_count,
            });
        }
        let slot = self.ensure_slot_for_handle(submission.shared_handle, submission.dims)?;
        let pic = PicParams::new(
            submission.sequence.saturating_mul(self.frame_interval_us),
            false,
        );
        let async_result = NvencHandoff::encode_shared_async(
            self,
            slot,
            submission.keyed_mutex_key,
            submission.dims,
            pic,
        );
        if let Err(e) = async_result {
            return Err(RingError::BackendFailed {
                source: crate::backend::BackendError::PlatformUnsupported {
                    reason: match e {
                        EncoderError::SlotUnknown { .. } => "nvenc slot unknown",
                        EncoderError::EncodeFailed { .. } => "nvenc encode failed",
                        _ => "nvenc session error",
                    },
                },
            });
        }
        self.drain_completions_into_ring();
        if self.in_flight_order.len() >= NVENC_PIPELINE_DEPTH
            && self.completed_ring.len() < COMPLETION_RING_CAPACITY
        {
            let timeout_ms = (self.frame_interval_us.saturating_add(999) / 1000) as u32;
            assert!(timeout_ms <= 1000, "blocking drain capped at one second");
            if let Some(bs) = self.drain_one_completed(timeout_ms) {
                self.completed_ring.push_back(bs);
            }
        }
        let encoded_bytes_estimate = self.pending_completion() as u32;
        callback.on_complete(submission.sequence, encoded_bytes_estimate);
        Ok(())
    }
}

impl NvencD3D11Handoff {
    fn ensure_slot_for_handle(
        &mut self,
        shared_handle: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, RingError> {
        if let Some(&idx) = self.handle_to_slot.get(&shared_handle) {
            return Ok(HandoffSlot::new(idx, shared_handle));
        }
        self.register_slot_impl(shared_handle, 0, dims)
            .map_err(|e| RingError::BackendFailed {
                source: crate::backend::BackendError::PlatformUnsupported {
                    reason: match e {
                        EncoderError::RegisterFailed { .. } => "nvenc register failed",
                        EncoderError::SessionInitFailed { .. } => "nvenc session init",
                        EncoderError::DimensionsOutOfRange { .. } => "nvenc dims out of range",
                        _ => "nvenc register slot",
                    },
                },
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_version_layout_is_stable() {
        assert_eq!(NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER, struct_version(1));
        assert_eq!(NV_ENCODE_API_FUNCTION_LIST_VER, struct_version(2));
    }

    #[test]
    fn guid_h264_layout_matches_nvenc_spec() {
        assert_eq!(NV_ENC_CODEC_H264_GUID.data1, 0x6bc82762);
        assert_eq!(NV_ENC_CODEC_H264_GUID.data4[7], 0xbf);
    }

    #[test]
    fn nvencapi_version_packs_major_and_minor() {
        let expected = 13_u32;
        assert_eq!(NVENCAPI_VERSION, expected);
    }
}
