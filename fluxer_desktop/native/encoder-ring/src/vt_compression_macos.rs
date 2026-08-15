// SPDX-License-Identifier: AGPL-3.0-or-later

use core::ffi::c_void;
use core::ptr::{self, NonNull};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::encoder_handoff::{
    EncodedBitstream, EncoderDims, EncoderError, EncoderFrameRate, EncoderSubmission, HandoffSlot,
    MAX_BITSTREAM_BYTES, PicParams, VideoToolboxHandoff,
};
use crate::ring::RingError;

type CfDictionaryRef = *const c_void;
type CfStringRef = *const c_void;
type CfNumberRef = *const c_void;
type CfBooleanRef = *const c_void;
type CfAllocatorRef = *const c_void;
type CmTime = OsCmTime;
pub(crate) type CvPixelBufferRef = *mut c_void;
type CmSampleBufferRef = *mut c_void;
type CmBlockBufferRef = *mut c_void;
type VtCompressionSessionRef = *mut c_void;
type VtPixelTransferSessionRef = *mut c_void;
type OsStatus = i32;
pub(crate) type IoSurfaceRef = *mut c_void;

const KCM_VIDEO_CODEC_TYPE_H264: u32 = u32::from_be_bytes(*b"avc1");
const NOERR: OsStatus = 0;
const KCM_TIME_FLAGS_VALID: u32 = 1;
const KCF_NUMBER_SINT32_TYPE: i32 = 3;
const COMPLETION_RING_CAPACITY: usize = 16;
const KVT_INVALID_SESSION_ERR: OsStatus = -12903;
const VT_SESSION_REBUILD_MAX: u64 = 3;
const VT_ENCODE_ATTEMPTS_PER_FRAME_MAX: usize = 2;

#[repr(C)]
#[derive(Copy, Clone)]
struct OsCmTime {
    value: i64,
    timescale: i32,
    flags: u32,
    epoch: i64,
}

#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    static kCFAllocatorDefault: CfAllocatorRef;
    static kCFBooleanTrue: CfBooleanRef;
    fn CFNumberCreate(
        allocator: CfAllocatorRef,
        the_type: i32,
        value_ptr: *const c_void,
    ) -> CfNumberRef;
    fn CFRelease(cf: *const c_void);
    fn CFRetain(cf: *const c_void) -> *const c_void;
}

#[link(name = "CoreVideo", kind = "framework")]
unsafe extern "C" {
    fn CVPixelBufferCreateWithIOSurface(
        allocator: CfAllocatorRef,
        surface: IoSurfaceRef,
        pixel_buffer_attributes: CfDictionaryRef,
        pixel_buffer_out: *mut CvPixelBufferRef,
    ) -> i32;
    fn CVPixelBufferRelease(buffer: CvPixelBufferRef);
}

#[link(name = "CoreMedia", kind = "framework")]
unsafe extern "C" {
    fn CMSampleBufferGetDataBuffer(sample_buffer: CmSampleBufferRef) -> CmBlockBufferRef;
    fn CMBlockBufferGetDataLength(block_buffer: CmBlockBufferRef) -> usize;
    fn CMBlockBufferCopyDataBytes(
        block_buffer: CmBlockBufferRef,
        offset_to_data: usize,
        data_length: usize,
        destination: *mut c_void,
    ) -> OsStatus;
}

#[link(name = "VideoToolbox", kind = "framework")]
unsafe extern "C" {
    static kVTCompressionPropertyKey_RealTime: CfStringRef;
    static kVTCompressionPropertyKey_ProfileLevel: CfStringRef;
    static kVTProfileLevel_H264_Baseline_AutoLevel: CfStringRef;
    static kVTCompressionPropertyKey_AverageBitRate: CfStringRef;
    static kVTCompressionPropertyKey_ExpectedFrameRate: CfStringRef;
    static kVTCompressionPropertyKey_AllowFrameReordering: CfStringRef;
    fn VTCompressionSessionCreate(
        allocator: CfAllocatorRef,
        width: i32,
        height: i32,
        codec_type: u32,
        encoder_specification: CfDictionaryRef,
        source_image_buffer_attributes: CfDictionaryRef,
        compressed_data_allocator: CfAllocatorRef,
        output_callback: Option<VtCompressionOutputCallback>,
        output_callback_refcon: *mut c_void,
        session_out: *mut VtCompressionSessionRef,
    ) -> OsStatus;
    fn VTCompressionSessionEncodeFrame(
        session: VtCompressionSessionRef,
        image_buffer: CvPixelBufferRef,
        presentation_time_stamp: CmTime,
        duration: CmTime,
        frame_properties: CfDictionaryRef,
        source_frame_refcon: *mut c_void,
        info_flags_out: *mut u32,
    ) -> OsStatus;
    fn VTCompressionSessionCompleteFrames(
        session: VtCompressionSessionRef,
        complete_until: CmTime,
    ) -> OsStatus;
    fn VTCompressionSessionInvalidate(session: VtCompressionSessionRef);
    fn VTSessionSetProperty(
        session: *mut c_void,
        property_key: CfStringRef,
        property_value: *const c_void,
    ) -> OsStatus;
    fn VTPixelTransferSessionCreate(
        allocator: CfAllocatorRef,
        session_out: *mut VtPixelTransferSessionRef,
    ) -> OsStatus;
    fn VTPixelTransferSessionTransferImage(
        session: VtPixelTransferSessionRef,
        source: CvPixelBufferRef,
        destination: CvPixelBufferRef,
    ) -> OsStatus;
    fn VTPixelTransferSessionInvalidate(session: VtPixelTransferSessionRef);
}

type VtCompressionOutputCallback = unsafe extern "C" fn(
    output_callback_refcon: *mut c_void,
    source_frame_refcon: *mut c_void,
    status: OsStatus,
    info_flags: u32,
    sample_buffer: CmSampleBufferRef,
);

fn is_invalid_session_error(err: &EncoderError) -> bool {
    match err {
        EncoderError::EncodeFailed { vendor, status } => {
            assert!(!vendor.is_empty(), "encode error vendor non-empty");
            *vendor == "vt-compression-encode" && *status == KVT_INVALID_SESSION_ERR as i64
        }
        _ => false,
    }
}

fn cf_num_i32(v: i32) -> CfNumberRef {
    let ptr: *const i32 = &v;
    unsafe {
        CFNumberCreate(
            kCFAllocatorDefault,
            KCF_NUMBER_SINT32_TYPE,
            ptr as *const c_void,
        )
    }
}

pub struct VtPixelTransfer {
    session: NonNull<c_void>,
}

unsafe impl Send for VtPixelTransfer {}

impl VtPixelTransfer {
    pub fn new() -> Result<Self, EncoderError> {
        let mut raw: VtPixelTransferSessionRef = ptr::null_mut();
        let status = unsafe { VTPixelTransferSessionCreate(kCFAllocatorDefault, &mut raw) };
        if status != NOERR || raw.is_null() {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-pixel-transfer",
                status: status as i64,
            });
        }
        let session = NonNull::new(raw).ok_or(EncoderError::SessionInitFailed {
            vendor: "vt-pixel-transfer",
            status: -1,
        })?;
        assert!(
            session.as_ptr() as usize != 0,
            "transfer session non-null after create"
        );
        Ok(Self { session })
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn transfer(
        &self,
        source: CvPixelBufferRef,
        destination: CvPixelBufferRef,
    ) -> Result<(), EncoderError> {
        assert!(!source.is_null(), "source pixel buffer non-null");
        assert!(!destination.is_null(), "destination pixel buffer non-null");
        let status = unsafe {
            VTPixelTransferSessionTransferImage(self.session.as_ptr(), source, destination)
        };
        if status != NOERR {
            return Err(EncoderError::EncodeFailed {
                vendor: "vt-pixel-transfer",
                status: status as i64,
            });
        }
        Ok(())
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn wrap_iosurface(
        surface: IoSurfaceRef,
        width: u32,
        height: u32,
    ) -> Result<CvPixelBufferRef, EncoderError> {
        assert!(!surface.is_null(), "wrap source IOSurface non-null");
        assert!(width > 0 && height > 0, "wrap dims positive");
        let mut pb: CvPixelBufferRef = ptr::null_mut();
        let status = unsafe {
            CVPixelBufferCreateWithIOSurface(kCFAllocatorDefault, surface, ptr::null(), &mut pb)
        };
        if status != 0 || pb.is_null() {
            return Err(EncoderError::EncodeFailed {
                vendor: "cv-create-iosurface",
                status: status as i64,
            });
        }
        assert!(!pb.is_null(), "wrap pb non-null after create");
        Ok(pb)
    }
}

impl Drop for VtPixelTransfer {
    fn drop(&mut self) {
        let p = self.session.as_ptr();
        if !p.is_null() {
            unsafe { VTPixelTransferSessionInvalidate(p) };
            unsafe { CFRelease(p) };
        }
    }
}

struct CompletionRing {
    buffer: VecDeque<EncodedBitstream>,
}

impl CompletionRing {
    fn new() -> Self {
        Self {
            buffer: VecDeque::with_capacity(COMPLETION_RING_CAPACITY),
        }
    }

    fn push(&mut self, item: EncodedBitstream) {
        if self.buffer.len() >= COMPLETION_RING_CAPACITY {
            self.buffer.pop_front();
        }
        self.buffer.push_back(item);
        assert!(
            self.buffer.len() <= COMPLETION_RING_CAPACITY,
            "completion ring bounded"
        );
    }

    fn pop(&mut self) -> Option<EncodedBitstream> {
        let item = self.buffer.pop_front();
        assert!(
            self.buffer.len() <= COMPLETION_RING_CAPACITY,
            "completion ring bounded after pop"
        );
        item
    }

    fn len(&self) -> usize {
        let n = self.buffer.len();
        assert!(n <= COMPLETION_RING_CAPACITY, "completion ring len bounded");
        n
    }
}

struct SharedState {
    completed: CompletionRing,
    accepted: u64,
    last_pts_us: u64,
}

struct VtCallbackContext {
    state: Arc<Mutex<SharedState>>,
    failed_completions: AtomicU64,
}

unsafe extern "C" fn vt_compression_output(
    output_callback_refcon: *mut c_void,
    source_frame_refcon: *mut c_void,
    status: OsStatus,
    _info_flags: u32,
    sample_buffer: CmSampleBufferRef,
) {
    if output_callback_refcon.is_null() {
        return;
    }
    let ctx = unsafe { &*(output_callback_refcon as *const VtCallbackContext) };
    if status != NOERR {
        ctx.failed_completions.fetch_add(1, Ordering::Relaxed);
        return;
    }
    if sample_buffer.is_null() {
        return;
    }
    let block = unsafe { CMSampleBufferGetDataBuffer(sample_buffer) };
    if block.is_null() {
        return;
    }
    let len = unsafe { CMBlockBufferGetDataLength(block) };
    if len == 0 || len > MAX_BITSTREAM_BYTES {
        return;
    }
    let mut bytes: Vec<u8> = vec![0u8; len];
    let copy_status =
        unsafe { CMBlockBufferCopyDataBytes(block, 0, len, bytes.as_mut_ptr() as *mut c_void) };
    if copy_status != NOERR {
        return;
    }
    let pts_us = source_frame_refcon as usize as u64;
    let payload =
        match std::panic::catch_unwind(|| EncodedBitstream::new(bytes, pts_us, pts_us, true)) {
            Ok(p) => p,
            Err(_) => return,
        };
    if let Ok(mut guard) = ctx.state.lock() {
        guard.completed.push(payload);
    }
}

struct SlotSurface {
    surface: IoSurfaceRef,
    pixel_buffer: CvPixelBufferRef,
}

pub struct VtCompressionHandoff {
    session: NonNull<c_void>,
    callback_context: Box<VtCallbackContext>,
    shared: Arc<Mutex<SharedState>>,
    surfaces: HashMap<u32, SlotSurface>,
    dims: EncoderDims,
    next_slot: u32,
    frame_interval_us: u64,
    frame_rate: EncoderFrameRate,
    session_rebuilds: u64,
}

unsafe impl Send for VtCompressionHandoff {}

impl VtCompressionHandoff {
    pub fn try_new(dims: EncoderDims) -> Result<Self, EncoderError> {
        Self::try_new_with_frame_rate(dims, EncoderFrameRate::from_fps(30))
    }

    pub fn try_new_with_frame_rate(
        dims: EncoderDims,
        frame_rate: EncoderFrameRate,
    ) -> Result<Self, EncoderError> {
        assert!(
            dims.width > 0 && dims.height > 0,
            "compression dims positive"
        );
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
        let shared = Arc::new(Mutex::new(SharedState {
            completed: CompletionRing::new(),
            accepted: 0,
            last_pts_us: 0,
        }));
        let mut callback_context = Box::new(VtCallbackContext {
            state: shared.clone(),
            failed_completions: AtomicU64::new(0),
        });
        let cb_ctx_ptr = callback_context.as_mut() as *mut VtCallbackContext as *mut c_void;
        let session = Self::create_session(dims, cb_ctx_ptr, frame_rate)?;
        let handoff = Self {
            session,
            callback_context,
            shared,
            surfaces: HashMap::new(),
            dims,
            next_slot: 0,
            frame_interval_us: frame_rate.frame_interval_us(),
            frame_rate,
            session_rebuilds: 0,
        };
        assert!(
            handoff.session.as_ptr() as usize != 0,
            "session non-null after init"
        );
        assert_eq!(handoff.next_slot, 0, "next slot fresh");
        Ok(handoff)
    }

    fn create_session(
        dims: EncoderDims,
        cb_ctx_ptr: *mut c_void,
        frame_rate: EncoderFrameRate,
    ) -> Result<NonNull<c_void>, EncoderError> {
        assert!(dims.width > 0, "session dims width positive");
        assert!(!cb_ctx_ptr.is_null(), "session callback context non-null");
        assert!(frame_rate.numerator > 0, "frame rate numerator positive");
        let mut raw: VtCompressionSessionRef = ptr::null_mut();
        let status = unsafe {
            VTCompressionSessionCreate(
                kCFAllocatorDefault,
                dims.width as i32,
                dims.height as i32,
                KCM_VIDEO_CODEC_TYPE_H264,
                ptr::null(),
                ptr::null(),
                kCFAllocatorDefault,
                Some(vt_compression_output),
                cb_ctx_ptr,
                &mut raw,
            )
        };
        if status != NOERR || raw.is_null() {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression",
                status: status as i64,
            });
        }
        let session = NonNull::new(raw).ok_or(EncoderError::SessionInitFailed {
            vendor: "vt-compression",
            status: -1,
        })?;
        Self::configure(session.as_ptr(), frame_rate)?;
        Ok(session)
    }

    fn try_rebuild_session(&mut self) -> bool {
        assert!(
            self.session_rebuilds <= VT_SESSION_REBUILD_MAX,
            "rebuild count within cap"
        );
        if self.session_rebuilds >= VT_SESSION_REBUILD_MAX {
            return false;
        }
        let cb_ctx_ptr = self.callback_context.as_mut() as *mut VtCallbackContext as *mut c_void;
        let Ok(new_session) = Self::create_session(self.dims, cb_ctx_ptr, self.frame_rate) else {
            return false;
        };
        let old = self.session.as_ptr();
        assert!(!old.is_null(), "old session non-null before replace");
        unsafe {
            VTCompressionSessionInvalidate(old);
            CFRelease(old);
        }
        self.session = new_session;
        self.session_rebuilds = self.session_rebuilds.saturating_add(1);
        assert!(
            self.session_rebuilds <= VT_SESSION_REBUILD_MAX,
            "rebuild count stays within cap"
        );
        true
    }

    fn configure(session: *mut c_void, frame_rate: EncoderFrameRate) -> Result<(), EncoderError> {
        assert!(!session.is_null(), "configure session non-null");
        assert!(frame_rate.numerator > 0, "frame rate numerator positive");
        let real_time_true = unsafe { kCFBooleanTrue };
        let st1 = unsafe {
            VTSessionSetProperty(session, kVTCompressionPropertyKey_RealTime, real_time_true)
        };
        if st1 != NOERR {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression-realtime",
                status: st1 as i64,
            });
        }
        let st2 = unsafe {
            VTSessionSetProperty(
                session,
                kVTCompressionPropertyKey_ProfileLevel,
                kVTProfileLevel_H264_Baseline_AutoLevel,
            )
        };
        if st2 != NOERR {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression-profile",
                status: st2 as i64,
            });
        }
        let bitrate = cf_num_i32(2_000_000);
        let st3 = unsafe {
            VTSessionSetProperty(session, kVTCompressionPropertyKey_AverageBitRate, bitrate)
        };
        unsafe { CFRelease(bitrate) };
        if st3 != NOERR {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression-bitrate",
                status: st3 as i64,
            });
        }
        let fps_value = frame_rate.gop_pic_size() as i32;
        let fps = cf_num_i32(fps_value);
        let st4 = unsafe {
            VTSessionSetProperty(session, kVTCompressionPropertyKey_ExpectedFrameRate, fps)
        };
        unsafe { CFRelease(fps) };
        if st4 != NOERR {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression-fps",
                status: st4 as i64,
            });
        }
        let no_reorder = unsafe { CFRetain(kCFBooleanTrue) };
        let st5 = unsafe {
            VTSessionSetProperty(
                session,
                kVTCompressionPropertyKey_AllowFrameReordering,
                no_reorder,
            )
        };
        unsafe { CFRelease(no_reorder) };
        if st5 != NOERR {
            return Err(EncoderError::SessionInitFailed {
                vendor: "vt-compression-reorder",
                status: st5 as i64,
            });
        }
        Ok(())
    }

    pub fn pending_completion(&self) -> usize {
        let guard = match self.shared.lock() {
            Ok(g) => g,
            Err(_) => return 0,
        };
        let n = guard.completed.len();
        assert!(n <= COMPLETION_RING_CAPACITY, "pending count bounded");
        n
    }

    pub fn accepted_count(&self) -> u64 {
        let guard = match self.shared.lock() {
            Ok(g) => g,
            Err(_) => return 0,
        };
        let n = guard.accepted;
        assert!(
            self.surfaces.len() as u64 <= u32::MAX as u64,
            "surfaces table bounded"
        );
        n
    }

    pub fn failed_completion_count(&self) -> u64 {
        let n = self
            .callback_context
            .failed_completions
            .load(Ordering::Relaxed);
        assert!(
            self.session_rebuilds <= VT_SESSION_REBUILD_MAX,
            "rebuild count within cap"
        );
        n
    }

    pub fn session_rebuild_count(&self) -> u64 {
        let n = self.session_rebuilds;
        assert!(n <= VT_SESSION_REBUILD_MAX, "rebuild count within cap");
        n
    }

    pub fn wait_for_completion(&self) -> Result<(), EncoderError> {
        let until = CmTime {
            value: 0,
            timescale: 0,
            flags: 0,
            epoch: 0,
        };
        let status = unsafe { VTCompressionSessionCompleteFrames(self.session.as_ptr(), until) };
        if status != NOERR {
            return Err(EncoderError::EncodeFailed {
                vendor: "vt-compression-complete",
                status: status as i64,
            });
        }
        Ok(())
    }

    fn encode_frame_once(
        &self,
        pixel_buffer: CvPixelBufferRef,
        pic_params: PicParams,
    ) -> Result<(), EncoderError> {
        assert!(!pixel_buffer.is_null(), "encode pixel buffer non-null");
        assert!(self.frame_interval_us > 0, "frame interval positive");
        let pts = CmTime {
            value: pic_params.pts_us as i64,
            timescale: 1_000_000,
            flags: KCM_TIME_FLAGS_VALID,
            epoch: 0,
        };
        let dur = CmTime {
            value: self.frame_interval_us as i64,
            timescale: 1_000_000,
            flags: KCM_TIME_FLAGS_VALID,
            epoch: 0,
        };
        let mut flags_out: u32 = 0;
        let refcon = pic_params.pts_us as usize as *mut c_void;
        let enc_status = unsafe {
            VTCompressionSessionEncodeFrame(
                self.session.as_ptr(),
                pixel_buffer,
                pts,
                dur,
                ptr::null(),
                refcon,
                &mut flags_out,
            )
        };
        if enc_status != NOERR {
            return Err(EncoderError::EncodeFailed {
                vendor: "vt-compression-encode",
                status: enc_status as i64,
            });
        }
        Ok(())
    }

    fn encode_one(&mut self, slot: HandoffSlot, pic_params: PicParams) -> Result<(), EncoderError> {
        assert!(slot.shared_handle != 0, "encode slot handle non-zero");
        let pixel_buffer = match self.surfaces.get(&slot.slot_index) {
            Some(entry) => {
                assert!(!entry.surface.is_null(), "cached slot surface non-null");
                assert!(
                    !entry.pixel_buffer.is_null(),
                    "cached pixel buffer non-null"
                );
                entry.pixel_buffer
            }
            None => {
                return Err(EncoderError::SlotUnknown {
                    slot_index: slot.slot_index,
                });
            }
        };
        let mut attempts: usize = 0;
        while attempts < VT_ENCODE_ATTEMPTS_PER_FRAME_MAX {
            attempts += 1;
            assert!(
                attempts <= VT_ENCODE_ATTEMPTS_PER_FRAME_MAX,
                "encode attempts bounded"
            );
            match self.encode_frame_once(pixel_buffer, pic_params) {
                Ok(()) => {
                    if let Ok(mut guard) = self.shared.lock() {
                        guard.accepted = guard.accepted.saturating_add(1);
                        guard.last_pts_us = pic_params.pts_us;
                    }
                    return Ok(());
                }
                Err(err) => {
                    if !is_invalid_session_error(&err) {
                        return Err(err);
                    }
                    if !self.try_rebuild_session() {
                        break;
                    }
                }
            }
        }
        Err(EncoderError::EncodeFailed {
            vendor: "vt-compression-invalid-session",
            status: KVT_INVALID_SESSION_ERR as i64,
        })
    }
}

impl Drop for VtCompressionHandoff {
    fn drop(&mut self) {
        for (_, entry) in self.surfaces.drain() {
            if !entry.pixel_buffer.is_null() {
                unsafe { CVPixelBufferRelease(entry.pixel_buffer) };
            }
        }
        let p = self.session.as_ptr();
        if !p.is_null() {
            unsafe { VTCompressionSessionInvalidate(p) };
            unsafe { CFRelease(p) };
        }
        let _ = &self.callback_context;
    }
}

impl VideoToolboxHandoff for VtCompressionHandoff {
    fn register_slot(
        &mut self,
        iosurface_handle: u64,
        _key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError> {
        if iosurface_handle == 0 {
            return Err(EncoderError::SlotUnknown {
                slot_index: u32::MAX,
            });
        }
        if dims.width == 0 || dims.height == 0 {
            return Err(EncoderError::DimensionsOutOfRange {
                width: dims.width,
                height: dims.height,
            });
        }
        let idx = self.next_slot;
        let surface = iosurface_handle as IoSurfaceRef;
        if surface.is_null() {
            return Err(EncoderError::SlotUnknown { slot_index: idx });
        }
        let mut pixel_buffer: CvPixelBufferRef = ptr::null_mut();
        let status = unsafe {
            CVPixelBufferCreateWithIOSurface(
                kCFAllocatorDefault,
                surface,
                ptr::null(),
                &mut pixel_buffer,
            )
        };
        if status != 0 || pixel_buffer.is_null() {
            return Err(EncoderError::RegisterFailed {
                vendor: "vt-cv-create",
                status: status as i64,
            });
        }
        self.surfaces.insert(
            idx,
            SlotSurface {
                surface,
                pixel_buffer,
            },
        );
        self.next_slot = self.next_slot.saturating_add(1);
        let slot = HandoffSlot::new(idx, iosurface_handle);
        assert_eq!(
            slot.shared_handle, iosurface_handle,
            "slot handle preserved"
        );
        assert!(self.next_slot > idx, "slot counter advanced");
        Ok(slot)
    }

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        _key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError> {
        if dims.width == 0 || dims.height == 0 {
            return Err(EncoderError::DimensionsOutOfRange {
                width: dims.width,
                height: dims.height,
            });
        }
        self.encode_one(slot, pic_params)
    }

    fn poll_completed(&mut self, _slot: HandoffSlot) -> Option<EncodedBitstream> {
        let mut guard = self.shared.lock().ok()?;
        let item = guard.completed.pop();
        if let Some(ref payload) = item {
            assert!(!payload.data.is_empty(), "polled payload non-empty");
            assert!(
                payload.data.len() <= MAX_BITSTREAM_BYTES,
                "polled payload bounded"
            );
        }
        item
    }

    fn unregister_slot(&mut self, slot: HandoffSlot) {
        assert!(slot.shared_handle != 0, "unregister handle non-zero");
        if let Some(entry) = self.surfaces.remove(&slot.slot_index) {
            assert!(
                !entry.pixel_buffer.is_null(),
                "cached pixel buffer non-null on unregister"
            );
            unsafe { CVPixelBufferRelease(entry.pixel_buffer) };
        }
    }

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn crate::encoder_handoff::EncoderCompletionCallback,
    ) -> Result<(), RingError> {
        if submission.shared_handle == 0 {
            return Err(RingError::UnknownSlot);
        }
        if submission.dims.width == 0 || submission.dims.height == 0 {
            return Err(RingError::BackendFailed {
                source: crate::backend::BackendError::DimensionsOutOfRange {
                    width: submission.dims.width,
                    height: submission.dims.height,
                },
            });
        }
        let slot = HandoffSlot::new(
            (submission.sequence & 0x3f) as u32,
            submission.shared_handle,
        );
        let pts_us = match submission.capture_pts_us {
            Some(capture_pts_us) => capture_pts_us,
            None => submission.sequence.saturating_mul(self.frame_interval_us),
        };
        let params = PicParams::new(pts_us, false);
        let encoded_bytes_estimate = self.pending_completion() as u32;
        if let Err(e) = self.encode_one(slot, params) {
            return Err(RingError::BackendFailed {
                source: crate::backend::BackendError::PlatformUnsupported {
                    reason: match e {
                        EncoderError::SlotUnknown { .. } => "vt slot unknown",
                        EncoderError::EncodeFailed { .. } => "vt encode failed",
                        _ => "vt session error",
                    },
                },
            });
        }
        callback.on_complete(submission.sequence, encoded_bytes_estimate);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn output_callback_counts_failed_completions() {
        let ctx = VtCallbackContext {
            state: Arc::new(Mutex::new(SharedState {
                completed: CompletionRing::new(),
                accepted: 0,
                last_pts_us: 0,
            })),
            failed_completions: AtomicU64::new(0),
        };
        let ctx_ptr = &ctx as *const VtCallbackContext as *mut c_void;
        unsafe {
            vt_compression_output(
                ctx_ptr,
                ptr::null_mut(),
                KVT_INVALID_SESSION_ERR,
                0,
                ptr::null_mut(),
            );
            vt_compression_output(ctx_ptr, ptr::null_mut(), -1, 0, ptr::null_mut());
        }
        assert_eq!(ctx.failed_completions.load(Ordering::Relaxed), 2);
        let guard = ctx.state.lock().expect("state lock");
        assert_eq!(
            guard.completed.len(),
            0,
            "failed completions enqueue nothing"
        );
    }

    #[test]
    fn invalid_session_error_detection_is_exact() {
        let invalid = EncoderError::EncodeFailed {
            vendor: "vt-compression-encode",
            status: KVT_INVALID_SESSION_ERR as i64,
        };
        assert!(is_invalid_session_error(&invalid));
        let other_status = EncoderError::EncodeFailed {
            vendor: "vt-compression-encode",
            status: -1,
        };
        assert!(!is_invalid_session_error(&other_status));
        let other_vendor = EncoderError::EncodeFailed {
            vendor: "vt-cv-create",
            status: KVT_INVALID_SESSION_ERR as i64,
        };
        assert!(!is_invalid_session_error(&other_vendor));
    }
}
