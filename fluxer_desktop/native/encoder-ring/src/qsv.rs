// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::Arc;

use libloading::{Library, Symbol};
use windows::Win32::Graphics::Direct3D11::{ID3D11Device, ID3D11Multithread};
use windows::core::Interface;

use crate::encoder_handoff::{
    EncodedBitstream, EncoderCompletionCallback, EncoderDims, EncoderError, EncoderFrameRate,
    EncoderSubmission, HandoffSlot, PicParams, QsvHandoff, apply_dts_offset, compute_dts_offset_us,
};
use crate::ring::RingError;

pub const QSV_DLL_NAME_VPL: &str = "libvpl.dll";
pub const QSV_DLL_NAME_MFX: &str = "libmfxhw64.dll";

const MFX_IMPL_HARDWARE: i32 = 0x0002;
const MFX_IMPL_VIA_D3D11: i32 = 0x0300;
const MFX_IMPL_TYPE_HARDWARE: u32 = 2;
const MFX_ACCEL_MODE_VIA_D3D11: u32 = 0x0300;
const MFX_HANDLE_D3D11_DEVICE: u32 = 3;
const MFX_FOURCC_NV12: u32 = u32::from_le_bytes(*b"NV12");
const MFX_CODEC_AVC: u32 = u32::from_le_bytes(*b"AVC ");
const MFX_RATECONTROL_CBR: u16 = 1;
const MFX_PICSTRUCT_PROGRESSIVE: u16 = 0x01;
const MFX_CHROMAFORMAT_YUV420: u16 = 1;
const MFX_IOPATTERN_IN_VIDEO_MEMORY: u16 = 0x01;

const MFX_VARIANT_TYPE_U32: u32 = 5;
const MFX_VARIANT_VERSION_MINOR: u8 = 1;
const MFX_VARIANT_VERSION_MAJOR: u8 = 1;

const MFX_ERR_NONE: i32 = 0;
const MFX_WRN_IN_EXECUTION: i32 = 1;
const MFX_ERR_MORE_DATA: i32 = -10;

const FILTER_PROPERTY_IMPL: &[u8] = b"mfxImplDescription.Impl\0";
const FILTER_PROPERTY_ACCEL: &[u8] = b"mfxImplDescription.AccelerationMode\0";

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct MfxVersion {
    minor: u16,
    major: u16,
}

#[repr(C)]
#[derive(Default)]
struct MfxFrameInfo {
    reserved: [u32; 4],
    channel_id: u16,
    bit_depth_luma: u16,
    bit_depth_chroma: u16,
    shift: u16,
    frame_id_temporal: u16,
    frame_id_priority: u16,
    frame_id_view_or_dependency: u16,
    frame_id_quality: u16,
    four_cc: u32,
    width: u16,
    height: u16,
    crop_x: u16,
    crop_y: u16,
    crop_w: u16,
    crop_h: u16,
    frame_rate_extn: u32,
    frame_rate_extd: u32,
    reserved3: u16,
    aspect_ratio_w: u16,
    aspect_ratio_h: u16,
    pic_struct: u16,
    chroma_format: u16,
    reserved2: u16,
}

#[repr(C)]
#[derive(Default)]
struct MfxInfoMfx {
    reserved: [u32; 7],
    low_power: u16,
    brc_param_multiplier: u16,
    frame_info: MfxFrameInfo,
    codec_id: u32,
    codec_profile: u16,
    codec_level: u16,
    num_thread: u16,
    target_usage: u16,
    gop_pic_size: u16,
    gop_ref_dist: u16,
    gop_opt_flag: u16,
    idr_interval: u16,
    rate_control_method: u16,
    init_qp: u16,
    buffer_size_in_kb: u16,
    target_kbps: u16,
    max_kbps: u16,
    num_slice: u16,
    num_ref_frame: u16,
    encoded_order: u16,
    union_pad: [u16; 15],
}

#[repr(C)]
struct MfxVideoParam {
    alloc_id: u32,
    reserved: [u32; 2],
    reserved3: u16,
    async_depth: u16,
    mfx: MfxInfoMfx,
    protected: u16,
    io_pattern: u16,
    ext_param: *mut c_void,
    num_ext_param: u16,
    reserved2: u16,
}

#[repr(C)]
struct MfxBitstream {
    encrypted_data: *mut c_void,
    num_extparam: u16,
    ext_param: *mut c_void,
    reserved: [u32; 6],
    decode_time_stamp: u64,
    time_stamp: u64,
    data: *mut u8,
    data_offset: u32,
    data_length: u32,
    max_length: u32,
    pic_struct: u16,
    frame_type: u16,
    data_flag: u16,
    reserved2: u16,
}

#[repr(C)]
struct MfxFrameData {
    ext_param: *mut c_void,
    num_extparam: u16,
    reserved: [u32; 8],
    mem_type: u16,
    pitch_high: u16,
    time_stamp: u64,
    frame_order: u32,
    locked: u16,
    pitch_low: u16,
    plane_ptrs: [*mut u8; 7],
    mem_id: *mut c_void,
    corrupted: u16,
    data_flag: u16,
}

#[repr(C)]
struct MfxFrameSurface1 {
    reserved: [u32; 4],
    interface_ptr: *mut c_void,
    info: MfxFrameInfo,
    data: MfxFrameData,
}

#[repr(C)]
#[derive(Clone, Copy)]
union MfxVariantData {
    u32_: u32,
    u64_: u64,
    ptr: *mut c_void,
    pad: [u8; 16],
}

#[repr(C)]
#[derive(Clone, Copy)]
struct MfxVariant {
    version: MfxStructVersion,
    type_: u32,
    data: MfxVariantData,
}

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct MfxStructVersion {
    minor: u8,
    major: u8,
}

type MfxInit = unsafe extern "C" fn(i32, *mut MfxVersion, *mut *mut c_void) -> i32;
type MfxClose = unsafe extern "C" fn(*mut c_void) -> i32;
type MfxSetHandle = unsafe extern "C" fn(*mut c_void, u32, *mut c_void) -> i32;
type MfxEncodeInit = unsafe extern "C" fn(*mut c_void, *mut MfxVideoParam) -> i32;
type MfxEncodeClose = unsafe extern "C" fn(*mut c_void) -> i32;
type MfxEncodeQuery =
    unsafe extern "C" fn(*mut c_void, *mut MfxVideoParam, *mut MfxVideoParam) -> i32;
type MfxEncodeFrameAsync = unsafe extern "C" fn(
    *mut c_void,
    *mut c_void,
    *mut MfxFrameSurface1,
    *mut MfxBitstream,
    *mut *mut c_void,
) -> i32;
type MfxSyncOperation = unsafe extern "C" fn(*mut c_void, *mut c_void, u32) -> i32;

type MfxLoad = unsafe extern "C" fn() -> *mut c_void;
type MfxUnload = unsafe extern "C" fn(*mut c_void);
type MfxCreateConfig = unsafe extern "C" fn(*mut c_void) -> *mut c_void;
type MfxSetConfigFilterProperty = unsafe extern "C" fn(*mut c_void, *const u8, MfxVariant) -> i32;
type MfxCreateSession = unsafe extern "C" fn(*mut c_void, u32, *mut *mut c_void) -> i32;

struct ApiTable {
    init: MfxInit,
    close: MfxClose,
    set_handle: MfxSetHandle,
    encode_init: MfxEncodeInit,
    encode_close: MfxEncodeClose,
    encode_query: MfxEncodeQuery,
    encode_frame_async: MfxEncodeFrameAsync,
    sync_operation: MfxSyncOperation,
}

struct DispatcherTable {
    load: MfxLoad,
    unload: MfxUnload,
    create_config: MfxCreateConfig,
    set_config_property: MfxSetConfigFilterProperty,
    create_session: MfxCreateSession,
}

struct SlotState {
    pending_pts_us: u64,
    pending_force_keyframe: bool,
    sync_point: *mut c_void,
    bitstream: MfxBitstream,
    bitstream_buf: Vec<u8>,
    surface: MfxFrameSurface1,
    in_flight: bool,
}

pub struct QsvD3D11Handoff {
    _library: Arc<Library>,
    api: ApiTable,
    dispatcher: Option<DispatcherTable>,
    loader: *mut c_void,
    session: *mut c_void,
    slots: HashMap<u32, SlotState>,
    next_slot_index: u32,
    dts_offset_us: i64,
    completed_count: u64,
    target_kbps: u16,
    frame_rate: EncoderFrameRate,
}

unsafe impl Send for QsvD3D11Handoff {}

impl QsvD3D11Handoff {
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
        let library = load_runtime()?;
        let api = load_api(&library)?;
        let dispatcher_opt = load_dispatcher(&library);
        let (loader, session) = open_session(&api, dispatcher_opt.as_ref())?;
        set_d3d11_handle(&api, session, &device)?;
        let target_kbps = (bitrate_bps / 1000).clamp(500, 60_000) as u16;
        encode_init(&api, session, dims, target_kbps, frame_rate)?;
        let dts_offset_us = compute_dts_offset_us(0, 0, frame_rate.frame_interval_us());
        let handoff = Self {
            _library: Arc::new(library),
            api,
            dispatcher: dispatcher_opt,
            loader,
            session,
            slots: HashMap::new(),
            next_slot_index: 0,
            dts_offset_us,
            completed_count: 0,
            target_kbps,
            frame_rate,
        };
        assert!(!handoff.session.is_null(), "session non-null");
        Ok(handoff)
    }
}

fn load_runtime() -> Result<Library, EncoderError> {
    let vpl_result = unsafe { Library::new(QSV_DLL_NAME_VPL) };
    match vpl_result {
        Ok(lib) => Ok(lib),
        Err(_) => {
            unsafe { Library::new(QSV_DLL_NAME_MFX) }.map_err(|_| EncoderError::SdkNotFound {
                vendor: "qsv",
                dll: QSV_DLL_NAME_MFX,
            })
        }
    }
}

fn load_api(library: &Library) -> Result<ApiTable, EncoderError> {
    let init: Symbol<'_, MfxInit> =
        unsafe { library.get(b"MFXInit\0") }.map_err(|_| EncoderError::SymbolMissing {
            vendor: "qsv",
            symbol: "MFXInit",
        })?;
    let close: Symbol<'_, MfxClose> =
        unsafe { library.get(b"MFXClose\0") }.map_err(|_| EncoderError::SymbolMissing {
            vendor: "qsv",
            symbol: "MFXClose",
        })?;
    let set_handle: Symbol<'_, MfxSetHandle> = unsafe { library.get(b"MFXVideoCORE_SetHandle\0") }
        .map_err(|_| EncoderError::SymbolMissing {
            vendor: "qsv",
            symbol: "MFXVideoCORE_SetHandle",
        })?;
    let encode_init: Symbol<'_, MfxEncodeInit> = unsafe { library.get(b"MFXVideoENCODE_Init\0") }
        .map_err(|_| EncoderError::SymbolMissing {
        vendor: "qsv",
        symbol: "MFXVideoENCODE_Init",
    })?;
    let encode_close: Symbol<'_, MfxEncodeClose> = unsafe {
        library.get(b"MFXVideoENCODE_Close\0")
    }
    .map_err(|_| EncoderError::SymbolMissing {
        vendor: "qsv",
        symbol: "MFXVideoENCODE_Close",
    })?;
    let encode_query: Symbol<'_, MfxEncodeQuery> = unsafe {
        library.get(b"MFXVideoENCODE_Query\0")
    }
    .map_err(|_| EncoderError::SymbolMissing {
        vendor: "qsv",
        symbol: "MFXVideoENCODE_Query",
    })?;
    let encode_frame_async: Symbol<'_, MfxEncodeFrameAsync> = unsafe {
        library.get(b"MFXVideoENCODE_EncodeFrameAsync\0")
    }
    .map_err(|_| EncoderError::SymbolMissing {
        vendor: "qsv",
        symbol: "MFXVideoENCODE_EncodeFrameAsync",
    })?;
    let sync_operation: Symbol<'_, MfxSyncOperation> = unsafe {
        library.get(b"MFXVideoCORE_SyncOperation\0")
    }
    .map_err(|_| EncoderError::SymbolMissing {
        vendor: "qsv",
        symbol: "MFXVideoCORE_SyncOperation",
    })?;
    Ok(ApiTable {
        init: *init,
        close: *close,
        set_handle: *set_handle,
        encode_init: *encode_init,
        encode_close: *encode_close,
        encode_query: *encode_query,
        encode_frame_async: *encode_frame_async,
        sync_operation: *sync_operation,
    })
}

fn load_dispatcher(library: &Library) -> Option<DispatcherTable> {
    let load: Symbol<'_, MfxLoad> = unsafe { library.get(b"MFXLoad\0") }.ok()?;
    let unload: Symbol<'_, MfxUnload> = unsafe { library.get(b"MFXUnload\0") }.ok()?;
    let create_config: Symbol<'_, MfxCreateConfig> =
        unsafe { library.get(b"MFXCreateConfig\0") }.ok()?;
    let set_config_property: Symbol<'_, MfxSetConfigFilterProperty> =
        unsafe { library.get(b"MFXSetConfigFilterProperty\0") }.ok()?;
    let create_session: Symbol<'_, MfxCreateSession> =
        unsafe { library.get(b"MFXCreateSession\0") }.ok()?;
    Some(DispatcherTable {
        load: *load,
        unload: *unload,
        create_config: *create_config,
        set_config_property: *set_config_property,
        create_session: *create_session,
    })
}

fn open_session(
    api: &ApiTable,
    dispatcher: Option<&DispatcherTable>,
) -> Result<(*mut c_void, *mut c_void), EncoderError> {
    if let Some(d) = dispatcher {
        match modern_session(d) {
            Ok((loader, session)) => return Ok((loader, session)),
            Err(e) => {
                let session = init_session_legacy(api)?;
                let _ = e;
                return Ok((ptr::null_mut(), session));
            }
        }
    }
    let session = init_session_legacy(api)?;
    Ok((ptr::null_mut(), session))
}

fn modern_session(d: &DispatcherTable) -> Result<(*mut c_void, *mut c_void), EncoderError> {
    let loader = unsafe { (d.load)() };
    if loader.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-mfxload",
            status: -1,
        });
    }
    assert!(!loader.is_null(), "MFXLoad returned non-null");
    if let Err(e) = set_filter_u32(d, loader, FILTER_PROPERTY_IMPL, MFX_IMPL_TYPE_HARDWARE) {
        unsafe { (d.unload)(loader) };
        return Err(e);
    }
    if let Err(e) = set_filter_u32(d, loader, FILTER_PROPERTY_ACCEL, MFX_ACCEL_MODE_VIA_D3D11) {
        unsafe { (d.unload)(loader) };
        return Err(e);
    }
    let mut session: *mut c_void = ptr::null_mut();
    let status = unsafe { (d.create_session)(loader, 0, &mut session) };
    if status != MFX_ERR_NONE || session.is_null() {
        unsafe { (d.unload)(loader) };
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-create-session",
            status: status as i64,
        });
    }
    assert!(!session.is_null(), "modern session non-null");
    Ok((loader, session))
}

fn set_filter_u32(
    d: &DispatcherTable,
    loader: *mut c_void,
    property: &'static [u8],
    value_u32: u32,
) -> Result<(), EncoderError> {
    assert!(!loader.is_null(), "loader non-null");
    assert!(!property.is_empty(), "property non-empty");
    let cfg = unsafe { (d.create_config)(loader) };
    if cfg.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-create-config",
            status: -1,
        });
    }
    let variant = MfxVariant {
        version: MfxStructVersion {
            minor: MFX_VARIANT_VERSION_MINOR,
            major: MFX_VARIANT_VERSION_MAJOR,
        },
        type_: MFX_VARIANT_TYPE_U32,
        data: MfxVariantData { u32_: value_u32 },
    };
    let status = unsafe { (d.set_config_property)(cfg, property.as_ptr(), variant) };
    if status != MFX_ERR_NONE {
        let vendor = if property == FILTER_PROPERTY_ACCEL {
            "qsv-accel-mode-rejected"
        } else {
            "qsv-set-config-property"
        };
        return Err(EncoderError::SessionInitFailed {
            vendor,
            status: status as i64,
        });
    }
    Ok(())
}

fn init_session_legacy(api: &ApiTable) -> Result<*mut c_void, EncoderError> {
    let session_via_d3d11 = try_mfxinit(api, MFX_IMPL_HARDWARE | MFX_IMPL_VIA_D3D11);
    if let Ok(s) = session_via_d3d11 {
        return Ok(s);
    }
    try_mfxinit(api, MFX_IMPL_HARDWARE)
}

fn try_mfxinit(api: &ApiTable, impl_flags: i32) -> Result<*mut c_void, EncoderError> {
    let mut version = MfxVersion { major: 1, minor: 0 };
    let mut session: *mut c_void = ptr::null_mut();
    let status = unsafe { (api.init)(impl_flags, &mut version, &mut session) };
    if status != MFX_ERR_NONE {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-mfxinit",
            status: status as i64,
        });
    }
    if session.is_null() {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-mfxinit",
            status: -1,
        });
    }
    assert!(!session.is_null(), "session non-null after MFXInit");
    Ok(session)
}

fn set_d3d11_handle(
    api: &ApiTable,
    session: *mut c_void,
    device: &ID3D11Device,
) -> Result<(), EncoderError> {
    assert!(!session.is_null(), "session non-null");
    let raw = device.as_raw();
    assert!(!raw.is_null(), "device raw non-null");
    if let Ok(mt) = device.cast::<ID3D11Multithread>() {
        let _ = unsafe { mt.SetMultithreadProtected(true) };
    }
    let status = unsafe { (api.set_handle)(session, MFX_HANDLE_D3D11_DEVICE, raw) };
    if status != MFX_ERR_NONE && status != MFX_WRN_IN_EXECUTION {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-set-handle",
            status: status as i64,
        });
    }
    Ok(())
}

fn build_video_params(
    dims: EncoderDims,
    target_kbps: u16,
    frame_rate: EncoderFrameRate,
) -> MfxVideoParam {
    let info = MfxFrameInfo {
        four_cc: MFX_FOURCC_NV12,
        width: align16(dims.width as u16),
        height: align16(dims.height as u16),
        crop_w: dims.width as u16,
        crop_h: dims.height as u16,
        frame_rate_extn: frame_rate.numerator,
        frame_rate_extd: frame_rate.denominator,
        aspect_ratio_w: 1,
        aspect_ratio_h: 1,
        pic_struct: MFX_PICSTRUCT_PROGRESSIVE,
        chroma_format: MFX_CHROMAFORMAT_YUV420,
        ..Default::default()
    };
    let mfx = MfxInfoMfx {
        frame_info: info,
        codec_id: MFX_CODEC_AVC,
        target_usage: 4,
        gop_pic_size: frame_rate.gop_pic_size(),
        gop_ref_dist: 1,
        rate_control_method: MFX_RATECONTROL_CBR,
        target_kbps,
        max_kbps: target_kbps,
        num_slice: 1,
        num_ref_frame: 1,
        ..Default::default()
    };
    MfxVideoParam {
        alloc_id: 0,
        reserved: [0; 2],
        reserved3: 0,
        async_depth: 1,
        mfx,
        protected: 0,
        io_pattern: MFX_IOPATTERN_IN_VIDEO_MEMORY,
        ext_param: ptr::null_mut(),
        num_ext_param: 0,
        reserved2: 0,
    }
}

fn align16(v: u16) -> u16 {
    (v + 15) & !15
}

fn encode_init(
    api: &ApiTable,
    session: *mut c_void,
    dims: EncoderDims,
    target_kbps: u16,
    frame_rate: EncoderFrameRate,
) -> Result<(), EncoderError> {
    let mut params = build_video_params(dims, target_kbps, frame_rate);
    let mut query_out = build_video_params(dims, target_kbps, frame_rate);
    let q_status = unsafe { (api.encode_query)(session, &mut params, &mut query_out) };
    let q_ok = q_status == MFX_ERR_NONE
        || q_status == MFX_WRN_IN_EXECUTION
        || q_status == -3
        || q_status > 0;
    if !q_ok {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-encode-query",
            status: q_status as i64,
        });
    }
    let status = unsafe { (api.encode_init)(session, &mut query_out) };
    if status != MFX_ERR_NONE && status != MFX_WRN_IN_EXECUTION {
        return Err(EncoderError::SessionInitFailed {
            vendor: "qsv-encode-init",
            status: status as i64,
        });
    }
    Ok(())
}

fn build_slot_state(
    shared_handle: u64,
    dims: EncoderDims,
    target_kbps: u16,
    frame_rate: EncoderFrameRate,
) -> SlotState {
    let max_bs = (dims.width as usize * dims.height as usize * 3 / 2).max(512 * 1024);
    let mut bitstream_buf = vec![0u8; max_bs];
    let bs_ptr = bitstream_buf.as_mut_ptr();
    let bitstream = MfxBitstream {
        encrypted_data: ptr::null_mut(),
        num_extparam: 0,
        ext_param: ptr::null_mut(),
        reserved: [0; 6],
        decode_time_stamp: 0,
        time_stamp: 0,
        data: bs_ptr,
        data_offset: 0,
        data_length: 0,
        max_length: max_bs as u32,
        pic_struct: 0,
        frame_type: 0,
        data_flag: 0,
        reserved2: 0,
    };
    let info = MfxFrameInfo {
        four_cc: MFX_FOURCC_NV12,
        width: align16(dims.width as u16),
        height: align16(dims.height as u16),
        crop_w: dims.width as u16,
        crop_h: dims.height as u16,
        frame_rate_extn: frame_rate.numerator,
        frame_rate_extd: frame_rate.denominator,
        aspect_ratio_w: 1,
        aspect_ratio_h: 1,
        pic_struct: MFX_PICSTRUCT_PROGRESSIVE,
        chroma_format: MFX_CHROMAFORMAT_YUV420,
        ..Default::default()
    };
    let _ = target_kbps;
    let surface = MfxFrameSurface1 {
        reserved: [0; 4],
        interface_ptr: ptr::null_mut(),
        info,
        data: MfxFrameData {
            ext_param: ptr::null_mut(),
            num_extparam: 0,
            reserved: [0; 8],
            mem_type: 0,
            pitch_high: 0,
            time_stamp: 0,
            frame_order: 0,
            locked: 0,
            pitch_low: 0,
            plane_ptrs: [ptr::null_mut(); 7],
            mem_id: shared_handle as *mut c_void,
            corrupted: 0,
            data_flag: 0,
        },
    };
    SlotState {
        pending_pts_us: 0,
        pending_force_keyframe: false,
        sync_point: ptr::null_mut(),
        bitstream,
        bitstream_buf,
        surface,
        in_flight: false,
    }
}

impl Drop for QsvD3D11Handoff {
    fn drop(&mut self) {
        self.slots.clear();
        if !self.session.is_null() {
            let _ = unsafe { (self.api.encode_close)(self.session) };
            let _ = unsafe { (self.api.close)(self.session) };
            self.session = ptr::null_mut();
        }
        if let Some(d) = self.dispatcher.take()
            && !self.loader.is_null()
        {
            unsafe { (d.unload)(self.loader) };
            self.loader = ptr::null_mut();
        }
    }
}

impl QsvHandoff for QsvD3D11Handoff {
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
            build_slot_state(shared_handle, dims, self.target_kbps, self.frame_rate),
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
        state.surface.data.time_stamp = pic_params.pts_us;
        state.bitstream.data_length = 0;
        state.bitstream.data_offset = 0;
        let mut sync: *mut c_void = ptr::null_mut();
        let status = unsafe {
            (self.api.encode_frame_async)(
                self.session,
                ptr::null_mut(),
                &mut state.surface,
                &mut state.bitstream,
                &mut sync,
            )
        };
        state.pending_pts_us = pic_params.pts_us;
        state.pending_force_keyframe = pic_params.force_keyframe;
        if status == MFX_ERR_MORE_DATA {
            state.in_flight = false;
            return Ok(());
        }
        if status != MFX_ERR_NONE {
            return Err(EncoderError::EncodeFailed {
                vendor: "qsv",
                status: status as i64,
            });
        }
        state.sync_point = sync;
        state.in_flight = !sync.is_null();
        Ok(())
    }

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream> {
        let session_ptr = self.session;
        let dts_offset_us = self.dts_offset_us;
        let completed_count = self.completed_count;
        let state = self.slots.get_mut(&slot.slot_index)?;
        if !state.in_flight || state.sync_point.is_null() {
            return None;
        }
        let status = unsafe { (self.api.sync_operation)(session_ptr, state.sync_point, 0) };
        if status == MFX_WRN_IN_EXECUTION || status != MFX_ERR_NONE {
            return None;
        }
        let len = state.bitstream.data_length as usize;
        if len == 0 {
            state.in_flight = false;
            return None;
        }
        let mut data: Vec<u8> = Vec::with_capacity(len);
        let offset = state.bitstream.data_offset as usize;
        data.extend_from_slice(&state.bitstream_buf[offset..offset + len]);
        let pts = state.bitstream.time_stamp;
        let is_keyframe = (state.bitstream.frame_type & 0x1) != 0 || completed_count == 0;
        let dts = apply_dts_offset(pts, dts_offset_us);
        state.in_flight = false;
        state.sync_point = ptr::null_mut();
        self.completed_count = self.completed_count.saturating_add(1);
        Some(EncodedBitstream::new(data, pts, dts, is_keyframe))
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
                what: "qsv::register_slot in encode_shared",
            })?;
        let pts_us = submission.capture_pts_us.unwrap_or_else(|| {
            submission
                .sequence
                .saturating_mul(self.frame_rate.frame_interval_us())
        });
        let pic = PicParams::new(pts_us, false);
        QsvHandoff::encode_shared_async(
            self,
            slot,
            submission.keyed_mutex_key,
            submission.dims,
            pic,
        )
        .map_err(|_| RingError::NotImplemented {
            what: "qsv::encode_shared_async",
        })?;
        if let Some(bs) = QsvHandoff::poll_completed(self, slot) {
            callback.on_complete(submission.sequence, bs.data.len() as u32);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fourcc_nv12_packs_correctly() {
        assert_eq!(MFX_FOURCC_NV12, 0x3231564E);
    }

    #[test]
    fn align16_rounds_up() {
        assert_eq!(align16(1080), 1088);
        assert_eq!(align16(1920), 1920);
        assert_eq!(align16(0), 0);
    }

    #[test]
    fn target_kbps_clamps_against_overflow() {
        let bitrate: u32 = u32::MAX;
        let kbps = (bitrate / 1000).clamp(500, 60_000) as u16;
        assert_eq!(kbps, 60_000);
    }

    #[test]
    fn accel_mode_constant_matches_onevpl_spec() {
        assert_eq!(MFX_ACCEL_MODE_VIA_D3D11, 0x0300);
        assert_ne!(MFX_ACCEL_MODE_VIA_D3D11, 0x0200);
    }

    #[test]
    fn impl_type_hardware_constant_matches_onevpl_spec() {
        assert_eq!(MFX_IMPL_TYPE_HARDWARE, 2);
    }

    #[test]
    fn variant_type_u32_constant_matches_onevpl_spec() {
        assert_eq!(MFX_VARIANT_TYPE_U32, 5);
        assert_ne!(MFX_VARIANT_TYPE_U32, 8);
    }

    #[test]
    fn d3d11_device_handle_type_matches_onevpl_spec() {
        assert_eq!(MFX_HANDLE_D3D11_DEVICE, 3);
        assert_ne!(MFX_HANDLE_D3D11_DEVICE, 2);
    }

    #[test]
    fn variant_version_matches_onevpl_spec() {
        assert_eq!(MFX_VARIANT_VERSION_MAJOR, 1);
        assert_eq!(MFX_VARIANT_VERSION_MINOR, 1);
    }

    #[test]
    fn filter_property_name_is_null_terminated_accel() {
        assert!(FILTER_PROPERTY_ACCEL.ends_with(b"\0"));
        let view = &FILTER_PROPERTY_ACCEL[..FILTER_PROPERTY_ACCEL.len() - 1];
        assert_eq!(view, b"mfxImplDescription.AccelerationMode");
    }

    #[test]
    fn filter_property_name_is_null_terminated_impl() {
        assert!(FILTER_PROPERTY_IMPL.ends_with(b"\0"));
        let view = &FILTER_PROPERTY_IMPL[..FILTER_PROPERTY_IMPL.len() - 1];
        assert_eq!(view, b"mfxImplDescription.Impl");
    }

    #[test]
    fn variant_payload_holds_u32_value() {
        let v = MfxVariant {
            version: MfxStructVersion {
                minor: MFX_VARIANT_VERSION_MINOR,
                major: MFX_VARIANT_VERSION_MAJOR,
            },
            type_: MFX_VARIANT_TYPE_U32,
            data: MfxVariantData {
                u32_: MFX_ACCEL_MODE_VIA_D3D11,
            },
        };
        let read = unsafe { v.data.u32_ };
        assert_eq!(read, MFX_ACCEL_MODE_VIA_D3D11);
        assert_eq!(v.type_, 5);
    }

    #[derive(Default)]
    struct FilterRecord {
        property: Vec<u8>,
        value_u32: u32,
        variant_type: u32,
    }
}
