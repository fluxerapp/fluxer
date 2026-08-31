// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod frame_pool;
#[cfg(feature = "wgpu")]
pub mod gpu_loss;

use parking_lot::RwLock;
use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::Arc;
use std::sync::OnceLock;

use crate::frame_pool::PooledFrame;

#[cfg(target_os = "linux")]
use std::os::fd::{IntoRawFd, OwnedFd};

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DmabufDesc {
    pub plane_count: u8,
    pub width: u32,
    pub height: u32,
    pub drm_format: u32,
    pub modifier: u64,
    pub strides: [u32; 4],
    pub offsets: [u32; 4],
    pub device_uuid: [u8; 16],
    pub timestamp_us: i64,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SharedTextureDesc {
    pub handle: u64,
    pub width: u32,
    pub height: u32,
    pub dxgi_format: u32,
    pub timestamp_us: i64,
}

pub const FRAME_DATA_BYTES_CAP: usize = 1 << 30;

pub trait SharedFrameBytes: Send + Sync {
    fn bytes(&self) -> &[u8];
}

enum FrameDataRepr {
    Owned(Vec<u8>),
    Shared {
        source: Arc<dyn SharedFrameBytes>,
        len: usize,
        _capacity_token: Option<PooledFrame>,
    },
}

pub struct FrameData {
    repr: FrameDataRepr,
}

impl FrameData {
    pub fn from_owned(data: Vec<u8>) -> Self {
        assert!(data.len() <= FRAME_DATA_BYTES_CAP);
        Self {
            repr: FrameDataRepr::Owned(data),
        }
    }

    pub fn from_shared(
        source: Arc<dyn SharedFrameBytes>,
        len: usize,
        capacity_token: Option<PooledFrame>,
    ) -> Self {
        assert!(len <= FRAME_DATA_BYTES_CAP);
        assert!(len <= source.bytes().len());
        Self {
            repr: FrameDataRepr::Shared {
                source,
                len,
                _capacity_token: capacity_token,
            },
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        match &self.repr {
            FrameDataRepr::Owned(data) => data.as_slice(),
            FrameDataRepr::Shared { source, len, .. } => {
                let bytes = source.bytes();
                assert!(*len <= bytes.len());
                &bytes[..*len]
            }
        }
    }

    pub fn len(&self) -> usize {
        let len = match &self.repr {
            FrameDataRepr::Owned(data) => data.len(),
            FrameDataRepr::Shared { len, .. } => *len,
        };
        assert!(len <= FRAME_DATA_BYTES_CAP);
        len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_shared(&self) -> bool {
        matches!(self.repr, FrameDataRepr::Shared { .. })
    }

    pub fn into_vec(self) -> Vec<u8> {
        match self.repr {
            FrameDataRepr::Owned(data) => data,
            FrameDataRepr::Shared { source, len, .. } => {
                let bytes = source.bytes();
                assert!(len <= bytes.len());
                bytes[..len].to_vec()
            }
        }
    }
}

impl From<Vec<u8>> for FrameData {
    fn from(data: Vec<u8>) -> Self {
        Self::from_owned(data)
    }
}

impl std::ops::Deref for FrameData {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}

pub struct Nv12Frame {
    pub data: FrameData,
    pub width: u32,
    pub height: u32,
    pub stride_y: u32,
    pub stride_uv: u32,
    pub timestamp_us: i64,
}

pub struct BgraFrame {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub timestamp_us: i64,
}

#[cfg(target_os = "macos")]
pub struct MacCvPixelBufferFrame {
    pixel_buffer: *mut c_void,
    pub width: u32,
    pub height: u32,
    pub pixel_format: u32,
    pub timestamp_us: i64,
}

#[cfg(target_os = "macos")]
unsafe impl Send for MacCvPixelBufferFrame {}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacCvPixelBufferFrame {}

#[cfg(target_os = "macos")]
impl MacCvPixelBufferFrame {
    pub unsafe fn from_retained(
        pixel_buffer: *mut c_void,
        width: u32,
        height: u32,
        pixel_format: u32,
        timestamp_us: i64,
    ) -> Self {
        Self {
            pixel_buffer,
            width,
            height,
            pixel_format,
            timestamp_us,
        }
    }

    pub fn pixel_buffer_ptr(&self) -> *mut c_void {
        self.pixel_buffer
    }

    pub fn into_raw_pixel_buffer(mut self) -> *mut c_void {
        let pb = self.pixel_buffer;
        self.pixel_buffer = std::ptr::null_mut();
        std::mem::forget(self);
        pb
    }
}

#[cfg(target_os = "macos")]
impl Drop for MacCvPixelBufferFrame {
    fn drop(&mut self) {
        if !self.pixel_buffer.is_null() {
            unsafe { CFRelease(self.pixel_buffer as *const c_void) };
            self.pixel_buffer = std::ptr::null_mut();
        }
    }
}

#[cfg(target_os = "macos")]
#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    fn CFRelease(cf: *const c_void);
}

#[cfg(target_os = "linux")]
pub struct DmabufFrame {
    pub desc: DmabufDesc,
    pub fds: Vec<OwnedFd>,
}

pub enum ScreenFrame {
    Nv12(Nv12Frame),
    Bgra(BgraFrame),
    #[cfg(target_os = "macos")]
    MacCvPixelBuffer(MacCvPixelBufferFrame),
    #[cfg(target_os = "linux")]
    Dmabuf(DmabufFrame),
    #[cfg(target_os = "windows")]
    SharedTexture(SharedTextureDesc),
}

impl ScreenFrame {
    pub fn timestamp_us(&self) -> i64 {
        match self {
            Self::Nv12(f) => f.timestamp_us,
            Self::Bgra(f) => f.timestamp_us,
            #[cfg(target_os = "macos")]
            Self::MacCvPixelBuffer(f) => f.timestamp_us,
            #[cfg(target_os = "linux")]
            Self::Dmabuf(f) => f.desc.timestamp_us,
            #[cfg(target_os = "windows")]
            Self::SharedTexture(d) => d.timestamp_us,
        }
    }

    pub fn dimensions(&self) -> (u32, u32) {
        match self {
            Self::Nv12(f) => (f.width, f.height),
            Self::Bgra(f) => (f.width, f.height),
            #[cfg(target_os = "macos")]
            Self::MacCvPixelBuffer(f) => (f.width, f.height),
            #[cfg(target_os = "linux")]
            Self::Dmabuf(f) => (f.desc.width, f.desc.height),
            #[cfg(target_os = "windows")]
            Self::SharedTexture(d) => (d.width, d.height),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnqueueOutcome {
    Accepted,
    Coalesced,
    Rejected,
}

pub trait ScreenFrameSink: Send + Sync {
    fn enqueue(&self, frame: ScreenFrame) -> EnqueueOutcome;
}

pub const NATIVE_SCREEN_FRAME_SINK_HANDLE_MAGIC: u32 = u32::from_be_bytes(*b"FXSH");
pub const NATIVE_SCREEN_FRAME_SINK_HANDLE_VERSION: u32 = 2;

pub const NATIVE_SCREEN_FRAME_SINK_ACCEPTED: u32 = 1;
pub const NATIVE_SCREEN_FRAME_SINK_COALESCED: u32 = 2;
pub const NATIVE_SCREEN_FRAME_SINK_REJECTED: u32 = 3;

pub type NativeScreenFrameSinkRetainFn = unsafe extern "C" fn(context: *const c_void);
pub type NativeScreenFrameSinkReleaseFn = unsafe extern "C" fn(context: *const c_void);
pub type NativeScreenFrameSinkEnqueueNv12Fn = unsafe extern "C" fn(
    context: *const c_void,
    data: *const u8,
    data_len: usize,
    width: u32,
    height: u32,
    stride_y: u32,
    stride_uv: u32,
    timestamp_us: i64,
) -> u32;
pub type NativeScreenFrameSinkEnqueueBgraFn = unsafe extern "C" fn(
    context: *const c_void,
    data: *const u8,
    data_len: usize,
    width: u32,
    height: u32,
    stride: u32,
    timestamp_us: i64,
) -> u32;
pub type NativeScreenFrameSinkEnqueueMacCvPixelBufferFn = unsafe extern "C" fn(
    context: *const c_void,
    pixel_buffer: *mut c_void,
    width: u32,
    height: u32,
    pixel_format: u32,
    timestamp_us: i64,
) -> u32;
pub type NativeScreenFrameSinkEnqueueDmabufFn = unsafe extern "C" fn(
    context: *const c_void,
    desc: DmabufDesc,
    fds: *const i32,
    fd_count: usize,
) -> u32;
pub type NativeScreenFrameSinkEnqueueSharedTextureFn =
    unsafe extern "C" fn(context: *const c_void, desc: SharedTextureDesc) -> u32;
pub type NativeScreenFrameSinkEnqueueScreenAudioFn = unsafe extern "C" fn(
    context: *const c_void,
    samples: *const f32,
    num_frames: u32,
    channels: u32,
    sample_rate_hz: u32,
    timestamp_us: i64,
) -> u32;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct NativeScreenFrameSinkHandle {
    magic: u32,
    version: u32,
    context: *const c_void,
    retain: NativeScreenFrameSinkRetainFn,
    release: NativeScreenFrameSinkReleaseFn,
    enqueue_nv12: Option<NativeScreenFrameSinkEnqueueNv12Fn>,
    enqueue_bgra: Option<NativeScreenFrameSinkEnqueueBgraFn>,
    enqueue_mac_cv_pixel_buffer: Option<NativeScreenFrameSinkEnqueueMacCvPixelBufferFn>,
    enqueue_dmabuf: Option<NativeScreenFrameSinkEnqueueDmabufFn>,
    enqueue_shared_texture: Option<NativeScreenFrameSinkEnqueueSharedTextureFn>,
    enqueue_screen_audio: Option<NativeScreenFrameSinkEnqueueScreenAudioFn>,
}

unsafe impl Send for NativeScreenFrameSinkHandle {}
unsafe impl Sync for NativeScreenFrameSinkHandle {}

impl NativeScreenFrameSinkHandle {
    fn is_valid(&self) -> bool {
        self.magic == NATIVE_SCREEN_FRAME_SINK_HANDLE_MAGIC
            && self.version == NATIVE_SCREEN_FRAME_SINK_HANDLE_VERSION
            && !self.context.is_null()
    }

    pub fn retain_ref(&self) -> Option<NativeScreenFrameSinkHandleRef> {
        if !self.is_valid() {
            return None;
        }
        unsafe { (self.retain)(self.context) };
        Some(NativeScreenFrameSinkHandleRef { handle: *self })
    }

    pub fn native_outcome(outcome: EnqueueOutcome) -> u32 {
        match outcome {
            EnqueueOutcome::Accepted => NATIVE_SCREEN_FRAME_SINK_ACCEPTED,
            EnqueueOutcome::Coalesced => NATIVE_SCREEN_FRAME_SINK_COALESCED,
            EnqueueOutcome::Rejected => NATIVE_SCREEN_FRAME_SINK_REJECTED,
        }
    }

    pub fn outcome_from_native(value: u32) -> EnqueueOutcome {
        match value {
            NATIVE_SCREEN_FRAME_SINK_ACCEPTED => EnqueueOutcome::Accepted,
            NATIVE_SCREEN_FRAME_SINK_COALESCED => EnqueueOutcome::Coalesced,
            _ => EnqueueOutcome::Rejected,
        }
    }
}

pub struct NativeScreenFrameSinkHandleRef {
    handle: NativeScreenFrameSinkHandle,
}

unsafe impl Send for NativeScreenFrameSinkHandleRef {}
unsafe impl Sync for NativeScreenFrameSinkHandleRef {}

impl NativeScreenFrameSinkHandleRef {
    pub fn enqueue_nv12_copy(
        &self,
        data: &[u8],
        width: u32,
        height: u32,
        stride_y: u32,
        stride_uv: u32,
        timestamp_us: i64,
    ) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_nv12 else {
            return EnqueueOutcome::Rejected;
        };
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(
                self.handle.context,
                data.as_ptr(),
                data.len(),
                width,
                height,
                stride_y,
                stride_uv,
                timestamp_us,
            )
        })
    }

    pub fn enqueue_bgra_copy(
        &self,
        data: &[u8],
        width: u32,
        height: u32,
        stride: u32,
        timestamp_us: i64,
    ) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_bgra else {
            return EnqueueOutcome::Rejected;
        };
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(
                self.handle.context,
                data.as_ptr(),
                data.len(),
                width,
                height,
                stride,
                timestamp_us,
            )
        })
    }

    #[cfg(target_os = "macos")]
    pub fn supports_mac_cv_pixel_buffer(&self) -> bool {
        self.handle.enqueue_mac_cv_pixel_buffer.is_some()
    }

    #[cfg(target_os = "macos")]
    pub unsafe fn enqueue_mac_cv_pixel_buffer(
        &self,
        pixel_buffer: *mut c_void,
        width: u32,
        height: u32,
        pixel_format: u32,
        timestamp_us: i64,
    ) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_mac_cv_pixel_buffer else {
            return EnqueueOutcome::Rejected;
        };
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(
                self.handle.context,
                pixel_buffer,
                width,
                height,
                pixel_format,
                timestamp_us,
            )
        })
    }

    #[cfg(target_os = "linux")]
    pub fn supports_dmabuf(&self) -> bool {
        self.handle.enqueue_dmabuf.is_some()
    }

    #[cfg(target_os = "linux")]
    pub fn enqueue_dmabuf_take_fds(&self, desc: DmabufDesc, fds: Vec<OwnedFd>) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_dmabuf else {
            return EnqueueOutcome::Rejected;
        };
        let fd_count = fds.len();
        if fd_count == 0 || fd_count > 4 || usize::from(desc.plane_count) != fd_count {
            return EnqueueOutcome::Rejected;
        }
        let mut raw_fds = [-1; 4];
        for (raw_fd, fd) in raw_fds.iter_mut().zip(fds) {
            *raw_fd = fd.into_raw_fd();
        }
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(self.handle.context, desc, raw_fds.as_ptr(), fd_count)
        })
    }

    #[cfg(target_os = "windows")]
    pub fn enqueue_shared_texture(&self, desc: SharedTextureDesc) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_shared_texture else {
            return EnqueueOutcome::Rejected;
        };
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(self.handle.context, desc)
        })
    }

    pub fn supports_screen_audio(&self) -> bool {
        self.handle.enqueue_screen_audio.is_some()
    }

    pub fn enqueue_screen_audio_f32(
        &self,
        samples: &[f32],
        num_frames: u32,
        channels: u32,
        sample_rate_hz: u32,
        timestamp_us: i64,
    ) -> EnqueueOutcome {
        let Some(enqueue) = self.handle.enqueue_screen_audio else {
            return EnqueueOutcome::Rejected;
        };
        if channels == 0 || num_frames == 0 {
            return EnqueueOutcome::Rejected;
        }
        let expected_samples = (num_frames as usize).checked_mul(channels as usize);
        if expected_samples != Some(samples.len()) {
            return EnqueueOutcome::Rejected;
        }
        NativeScreenFrameSinkHandle::outcome_from_native(unsafe {
            enqueue(
                self.handle.context,
                samples.as_ptr(),
                num_frames,
                channels,
                sample_rate_hz,
                timestamp_us,
            )
        })
    }
}

impl Drop for NativeScreenFrameSinkHandleRef {
    fn drop(&mut self) {
        if self.handle.is_valid() {
            unsafe { (self.handle.release)(self.handle.context) };
        }
    }
}

pub const STAGING_PAIR_LEN: usize = 2;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StagingSlotState {
    Empty,
    Submitted,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StagingError {
    WouldOverwrite,
}

pub trait StagingBackend {
    fn write<F: FnOnce(&mut [u8])>(&mut self, fill: F);
    fn read<R, F: FnOnce(&[u8]) -> R>(&self, read: F) -> R;
    fn is_ready(&self) -> bool;
    fn is_idle(&self) -> bool;
}

pub struct CpuStagingBackend {
    buffer: Vec<u8>,
}

impl CpuStagingBackend {
    pub fn new(byte_len: usize) -> Self {
        assert!(byte_len > 0, "staging backend length must be positive");
        assert!(
            byte_len <= 1 << 30,
            "staging backend length exceeds sanity cap"
        );
        Self {
            buffer: vec![0u8; byte_len],
        }
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
}

impl StagingBackend for CpuStagingBackend {
    fn write<F: FnOnce(&mut [u8])>(&mut self, fill: F) {
        fill(&mut self.buffer);
    }

    fn read<R, F: FnOnce(&[u8]) -> R>(&self, read: F) -> R {
        read(&self.buffer)
    }

    fn is_ready(&self) -> bool {
        true
    }

    fn is_idle(&self) -> bool {
        true
    }
}

pub struct StagingSurfacePair<B: StagingBackend> {
    backends: [B; STAGING_PAIR_LEN],
    slot_states: [StagingSlotState; STAGING_PAIR_LEN],
    next_submit: u64,
    next_map: u64,
    skipped_count: u64,
    overwrite_dropped_count: u64,
}

impl<B: StagingBackend> StagingSurfacePair<B> {
    pub fn new(backends: [B; STAGING_PAIR_LEN]) -> Self {
        assert_eq!(
            STAGING_PAIR_LEN, 2,
            "OBS staging pair is exactly two surfaces"
        );
        assert!(
            backends[0].is_idle(),
            "backend zero must be idle at construction"
        );
        assert!(
            backends[1].is_idle(),
            "backend one must be idle at construction"
        );
        Self {
            backends,
            slot_states: [StagingSlotState::Empty; STAGING_PAIR_LEN],
            next_submit: 0,
            next_map: 0,
            skipped_count: 0,
            overwrite_dropped_count: 0,
        }
    }

    pub fn next_submit_sequence(&self) -> u64 {
        self.next_submit
    }

    pub fn next_map_sequence(&self) -> u64 {
        self.next_map
    }

    pub fn skipped_count(&self) -> u64 {
        self.skipped_count
    }

    pub fn overwrite_dropped_count(&self) -> u64 {
        self.overwrite_dropped_count
    }

    pub fn submit<F: FnOnce(&mut [u8])>(
        &mut self,
        sequence: u64,
        fill: F,
    ) -> Result<(), StagingError> {
        assert_eq!(sequence, self.next_submit, "submit must use next sequence");
        assert!(
            self.next_map <= self.next_submit,
            "map sequence must trail submit"
        );
        let slot = (sequence & 1) as usize;
        assert!(slot < STAGING_PAIR_LEN, "slot index within pair bounds");
        if self.slot_states[slot] != StagingSlotState::Empty {
            self.overwrite_dropped_count = self.overwrite_dropped_count.wrapping_add(1);
            return Err(StagingError::WouldOverwrite);
        }
        assert!(
            self.backends[slot].is_idle(),
            "backend reports busy on empty slot"
        );
        self.backends[slot].write(fill);
        self.slot_states[slot] = StagingSlotState::Submitted;
        self.next_submit = self.next_submit.wrapping_add(1);
        Ok(())
    }

    pub fn try_map<R, F: FnOnce(&[u8]) -> R>(&mut self, sequence: u64, read: F) -> Option<R> {
        assert!(
            self.next_map < self.next_submit,
            "cannot map an unsubmitted sequence"
        );
        assert_eq!(
            sequence, self.next_map,
            "try_map must use next pending sequence"
        );
        let slot = (sequence & 1) as usize;
        assert!(slot < STAGING_PAIR_LEN, "slot index within pair bounds");
        assert_eq!(
            self.slot_states[slot],
            StagingSlotState::Submitted,
            "double map of unsubmitted slot"
        );
        if !self.backends[slot].is_ready() {
            self.skipped_count = self.skipped_count.wrapping_add(1);
            self.slot_states[slot] = StagingSlotState::Empty;
            self.next_map = self.next_map.wrapping_add(1);
            return None;
        }
        let value = self.backends[slot].read(read);
        self.slot_states[slot] = StagingSlotState::Empty;
        self.next_map = self.next_map.wrapping_add(1);
        Some(value)
    }

    pub fn backend(&self, slot: usize) -> &B {
        assert!(slot < STAGING_PAIR_LEN, "slot index within pair bounds");
        assert!(
            slot < self.backends.len(),
            "slot index within backend storage"
        );
        &self.backends[slot]
    }
}

type SinkMap = HashMap<String, Arc<dyn ScreenFrameSink>>;
static REGISTRY: OnceLock<RwLock<SinkMap>> = OnceLock::new();

fn registry() -> &'static RwLock<SinkMap> {
    REGISTRY.get_or_init(|| RwLock::new(HashMap::new()))
}

pub fn register_sink(capture_id: impl Into<String>, sink: Arc<dyn ScreenFrameSink>) {
    registry().write().insert(capture_id.into(), sink);
}

pub fn unregister_sink(capture_id: &str) -> Option<Arc<dyn ScreenFrameSink>> {
    registry().write().remove(capture_id)
}

pub fn get_sink(capture_id: &str) -> Option<Arc<dyn ScreenFrameSink>> {
    registry().read().get(capture_id).cloned()
}

pub fn clear_all_sinks() {
    registry().write().clear();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};

    struct CountingSink {
        accepted: AtomicU32,
    }

    impl ScreenFrameSink for CountingSink {
        fn enqueue(&self, _frame: ScreenFrame) -> EnqueueOutcome {
            self.accepted.fetch_add(1, Ordering::Relaxed);
            EnqueueOutcome::Accepted
        }
    }

    fn fresh_id(label: &str) -> String {
        format!("test:{}:{}", label, std::process::id())
    }

    fn solid_nv12_frame(width: u32, height: u32) -> ScreenFrame {
        let y = (width * height) as usize;
        let uv = (width * (height / 2)) as usize;
        let mut data = vec![0u8; y + uv];
        data[..y].fill(16);
        data[y..].fill(128);
        ScreenFrame::Nv12(Nv12Frame {
            data: data.into(),
            width,
            height,
            stride_y: width,
            stride_uv: width,
            timestamp_us: 0,
        })
    }

    #[test]
    fn sink_registration_round_trips_through_registry() {
        let id = fresh_id("round-trip");
        let sink = Arc::new(CountingSink {
            accepted: AtomicU32::new(0),
        });
        register_sink(id.clone(), sink.clone());

        let resolved = get_sink(&id).expect("sink must be resolvable");
        assert_eq!(
            resolved.enqueue(solid_nv12_frame(64, 64)),
            EnqueueOutcome::Accepted,
        );
        assert_eq!(sink.accepted.load(Ordering::Relaxed), 1);

        let removed = unregister_sink(&id).expect("sink must be removable");
        assert_eq!(
            removed.enqueue(solid_nv12_frame(64, 64)),
            EnqueueOutcome::Accepted,
        );
        assert!(get_sink(&id).is_none());
        assert!(unregister_sink(&id).is_none());
    }

    #[test]
    fn screen_frame_exposes_timestamp_and_dimensions() {
        let frame = solid_nv12_frame(640, 480);
        assert_eq!(frame.dimensions(), (640, 480));
        assert_eq!(frame.timestamp_us(), 0);
    }

    struct RejectingSink;
    impl ScreenFrameSink for RejectingSink {
        fn enqueue(&self, _frame: ScreenFrame) -> EnqueueOutcome {
            EnqueueOutcome::Rejected
        }
    }

    #[test]
    fn concurrent_producers_all_deliver_to_a_single_sink() {
        let id = fresh_id("concurrent-producers");
        let sink = Arc::new(CountingSink {
            accepted: AtomicU32::new(0),
        });
        register_sink(id.clone(), sink.clone());

        let producers = 8;
        let frames_per_producer = 64;
        let handles: Vec<_> = (0..producers)
            .map(|p| {
                let id = id.clone();
                std::thread::spawn(move || {
                    for f in 0..frames_per_producer {
                        let resolved = get_sink(&id).expect("sink missing during producer race");
                        assert_eq!(
                            resolved.enqueue(solid_nv12_frame(32, 32)),
                            EnqueueOutcome::Accepted,
                            "producer {} frame {}",
                            p,
                            f
                        );
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().expect("producer panicked");
        }
        unregister_sink(&id);
        assert_eq!(
            sink.accepted.load(Ordering::Relaxed),
            producers * frames_per_producer
        );
    }

    #[test]
    fn sinks_for_distinct_ids_do_not_cross_route() {
        let id_a = fresh_id("isolation-a");
        let id_b = fresh_id("isolation-b");
        let sink_a = Arc::new(CountingSink {
            accepted: AtomicU32::new(0),
        });
        let sink_b: Arc<dyn ScreenFrameSink> = Arc::new(RejectingSink);
        register_sink(id_a.clone(), sink_a.clone());
        register_sink(id_b.clone(), sink_b.clone());

        assert_eq!(
            get_sink(&id_a).unwrap().enqueue(solid_nv12_frame(8, 8)),
            EnqueueOutcome::Accepted
        );
        assert_eq!(
            get_sink(&id_b).unwrap().enqueue(solid_nv12_frame(8, 8)),
            EnqueueOutcome::Rejected
        );
        assert_eq!(sink_a.accepted.load(Ordering::Relaxed), 1);

        unregister_sink(&id_a);
        unregister_sink(&id_b);
    }

    #[test]
    fn unregister_returns_the_active_arc_and_inflight_holders_keep_working() {
        let id = fresh_id("unregister-race");
        let sink = Arc::new(CountingSink {
            accepted: AtomicU32::new(0),
        });
        register_sink(id.clone(), sink.clone());

        let inflight = get_sink(&id).expect("sink available");
        let removed = unregister_sink(&id).expect("unregister returns sink");
        assert!(Arc::ptr_eq(&inflight, &removed));
        assert!(get_sink(&id).is_none());

        assert_eq!(
            inflight.enqueue(solid_nv12_frame(8, 8)),
            EnqueueOutcome::Accepted
        );
        assert_eq!(sink.accepted.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn bgra_frame_exposes_timestamp_and_dimensions() {
        let frame = ScreenFrame::Bgra(BgraFrame {
            data: vec![0; 4 * 16 * 16],
            width: 16,
            height: 16,
            stride: 64,
            timestamp_us: 9_999,
        });
        assert_eq!(frame.dimensions(), (16, 16));
        assert_eq!(frame.timestamp_us(), 9_999);
    }

    struct StaticSharedBytes {
        bytes: Box<[u8]>,
    }

    impl SharedFrameBytes for StaticSharedBytes {
        fn bytes(&self) -> &[u8] {
            &self.bytes
        }
    }

    #[test]
    fn frame_data_owned_round_trips_without_sharing() {
        let data = FrameData::from(vec![1u8, 2, 3]);
        assert!(!data.is_shared());
        assert!(!data.is_empty());
        assert_eq!(data.len(), 3);
        assert_eq!(data.as_slice(), &[1, 2, 3]);
        assert_eq!(data.into_vec(), vec![1, 2, 3]);
    }

    #[test]
    fn frame_data_shared_round_trips_prefix_of_source_bytes() {
        let source = Arc::new(StaticSharedBytes {
            bytes: vec![7u8; 16].into_boxed_slice(),
        });
        let data = FrameData::from_shared(source.clone(), 12, None);
        assert!(data.is_shared());
        assert_eq!(data.len(), 12);
        assert_eq!(data.as_slice(), &[7u8; 12]);
        assert_eq!(Arc::strong_count(&source), 2);
        assert_eq!(data.into_vec(), vec![7u8; 12]);
        assert_eq!(Arc::strong_count(&source), 1);
    }

    #[test]
    #[should_panic]
    fn frame_data_shared_rejects_len_beyond_source_bytes() {
        let source = Arc::new(StaticSharedBytes {
            bytes: vec![0u8; 4].into_boxed_slice(),
        });
        let _ = FrameData::from_shared(source, 5, None);
    }

    #[test]
    fn frame_data_shared_capacity_token_returns_slot_on_drop_with_counters_intact() {
        let pool = frame_pool::CpuFrameBuilder::build_pool_with_capacity(2, 4).expect("pool init");
        let source = Arc::new(StaticSharedBytes {
            bytes: vec![9u8; 8].into_boxed_slice(),
        });
        let token = pool.try_acquire().expect("first slot");
        let data = FrameData::from_shared(source, 8, Some(token));
        assert_eq!(pool.currently_in_flight(), 1);

        let second = pool.try_acquire().expect("second slot");
        assert!(pool.try_acquire().is_none());
        assert_eq!(pool.skipped_total(), 1);
        assert_eq!(pool.acquired_total(), 2);

        drop(data);
        assert_eq!(pool.currently_in_flight(), 1);
        let third = pool.try_acquire().expect("slot returned by frame drop");
        assert_eq!(pool.acquired_total(), 3);
        assert_eq!(pool.skipped_total(), 1);

        drop(second);
        drop(third);
        assert_eq!(pool.currently_in_flight(), 0);
    }

    #[test]
    fn nv12_frame_with_shared_payload_exposes_timestamp_and_dimensions() {
        let source = Arc::new(StaticSharedBytes {
            bytes: vec![0u8; 64 * 96].into_boxed_slice(),
        });
        let frame = ScreenFrame::Nv12(Nv12Frame {
            data: FrameData::from_shared(source, 64 * 96, None),
            width: 64,
            height: 64,
            stride_y: 64,
            stride_uv: 64,
            timestamp_us: 77,
        });
        assert_eq!(frame.dimensions(), (64, 64));
        assert_eq!(frame.timestamp_us(), 77);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn mac_cv_pixel_buffer_with_null_pointer_drops_without_calling_release() {
        let frame =
            unsafe { MacCvPixelBufferFrame::from_retained(std::ptr::null_mut(), 8, 8, 0, 0) };
        assert_eq!(frame.pixel_buffer_ptr(), std::ptr::null_mut());
        drop(frame);
    }

    const STAGING_TEST_LEN: usize = 64;

    fn fresh_staging_pair() -> StagingSurfacePair<CpuStagingBackend> {
        let backends = [
            CpuStagingBackend::new(STAGING_TEST_LEN),
            CpuStagingBackend::new(STAGING_TEST_LEN),
        ];
        assert_eq!(backends[0].len(), STAGING_TEST_LEN);
        assert_eq!(backends[1].len(), STAGING_TEST_LEN);
        StagingSurfacePair::new(backends)
    }

    #[test]
    fn submit_then_map_returns_the_data_from_two_frames_ago() {
        let mut pair = fresh_staging_pair();
        assert!(pair.submit(0, |buf| buf.fill(0xA1)).is_ok());
        assert!(pair.submit(1, |buf| buf.fill(0xB2)).is_ok());

        let first = pair
            .try_map(0, |buf| buf[0])
            .expect("first map ready under CPU backend");
        assert_eq!(first, 0xA1);

        assert!(pair.submit(2, |buf| buf.fill(0xC3)).is_ok());
        let second = pair
            .try_map(1, |buf| buf[0])
            .expect("second map ready under CPU backend");
        assert_eq!(second, 0xB2);

        let third = pair
            .try_map(2, |buf| buf[0])
            .expect("third map ready under CPU backend");
        assert_eq!(third, 0xC3);
    }

    #[test]
    fn two_submits_in_a_row_preserve_distinct_slots() {
        let mut pair = fresh_staging_pair();
        assert!(pair.submit(0, |buf| buf.fill(0x11)).is_ok());
        assert!(pair.submit(1, |buf| buf.fill(0x22)).is_ok());
        assert_eq!(pair.next_submit_sequence(), 2);

        let first = pair
            .try_map(0, |buf| buf[0])
            .expect("slot zero still holds first submission");
        assert_eq!(first, 0x11);
        let second = pair
            .try_map(1, |buf| buf[0])
            .expect("slot one still holds second submission");
        assert_eq!(second, 0x22);
    }

    #[test]
    fn third_unmapped_submit_returns_would_overwrite_and_drops_newest() {
        let mut pair = fresh_staging_pair();
        assert!(pair.submit(0, |buf| buf.fill(0x33)).is_ok());
        assert!(pair.submit(1, |buf| buf.fill(0x44)).is_ok());
        assert_eq!(pair.overwrite_dropped_count(), 0);

        let outcome = pair.submit(2, |buf| buf.fill(0xFF));
        assert_eq!(outcome, Err(StagingError::WouldOverwrite));
        assert_eq!(pair.overwrite_dropped_count(), 1);

        let preserved = pair
            .try_map(0, |buf| buf[0])
            .expect("slot zero kept its original submission");
        assert_eq!(preserved, 0x33);
    }

    struct GatedBackend {
        buffer: Vec<u8>,
        ready: bool,
    }

    impl GatedBackend {
        fn new(len: usize) -> Self {
            assert!(len > 0, "gated backend len positive");
            assert!(len <= 1 << 16, "gated backend len within sanity cap");
            Self {
                buffer: vec![0u8; len],
                ready: true,
            }
        }
    }

    impl StagingBackend for GatedBackend {
        fn write<F: FnOnce(&mut [u8])>(&mut self, fill: F) {
            fill(&mut self.buffer);
        }

        fn read<R, F: FnOnce(&[u8]) -> R>(&self, read: F) -> R {
            read(&self.buffer)
        }

        fn is_ready(&self) -> bool {
            self.ready
        }

        fn is_idle(&self) -> bool {
            true
        }
    }

    #[test]
    fn try_map_returning_none_advances_and_bumps_skipped_counter() {
        let mut a = GatedBackend::new(STAGING_TEST_LEN);
        let mut b = GatedBackend::new(STAGING_TEST_LEN);
        a.ready = false;
        b.ready = true;
        let mut pair = StagingSurfacePair::new([a, b]);

        assert!(pair.submit(0, |buf| buf.fill(0x55)).is_ok());
        assert!(pair.submit(1, |buf| buf.fill(0x66)).is_ok());

        let skipped = pair.try_map(0, |buf| buf[0]);
        assert!(skipped.is_none());
        assert_eq!(pair.skipped_count(), 1);
        assert_eq!(pair.next_map_sequence(), 1);

        let mapped = pair.try_map(1, |buf| buf[0]).expect("slot one ready");
        assert_eq!(mapped, 0x66);
        assert_eq!(pair.next_map_sequence(), 2);

        assert!(pair.submit(2, |buf| buf.fill(0x77)).is_ok());
        assert!(pair.submit(3, |buf| buf.fill(0x88)).is_ok());
        let after_skip = pair.try_map(2, |buf| buf[0]);
        assert!(after_skip.is_none());
        assert_eq!(pair.skipped_count(), 2);
    }

    #[test]
    #[should_panic(expected = "submit must use next sequence")]
    fn submit_with_out_of_order_sequence_panics() {
        let mut pair = fresh_staging_pair();
        let _ = pair.submit(7, |buf| buf.fill(0));
    }

    #[test]
    #[should_panic(expected = "try_map must use next pending sequence")]
    fn try_map_with_wrong_sequence_panics() {
        let mut pair = fresh_staging_pair();
        assert!(pair.submit(0, |buf| buf.fill(0)).is_ok());
        let _ = pair.try_map(5, |buf| buf[0]);
    }

    #[test]
    #[should_panic(expected = "cannot map an unsubmitted sequence")]
    fn try_map_without_any_submit_panics() {
        let mut pair = fresh_staging_pair();
        let _ = pair.try_map(0, |buf| buf[0]);
    }

    #[test]
    #[should_panic(expected = "try_map must use next pending sequence")]
    fn try_map_twice_on_the_same_sequence_panics() {
        let mut pair = fresh_staging_pair();
        assert!(pair.submit(0, |buf| buf.fill(0xAB)).is_ok());
        assert!(pair.submit(1, |buf| buf.fill(0xCD)).is_ok());
        let first = pair.try_map(0, |buf| buf[0]).expect("first map");
        assert_eq!(first, 0xAB);
        let _ = pair.try_map(0, |buf| buf[0]);
    }

    static SCREEN_AUDIO_ENQUEUE_CALLS: AtomicU32 = AtomicU32::new(0);

    unsafe extern "C" fn screen_audio_test_retain(_context: *const c_void) {}
    unsafe extern "C" fn screen_audio_test_release(_context: *const c_void) {}
    unsafe extern "C" fn screen_audio_test_enqueue(
        _context: *const c_void,
        _samples: *const f32,
        _num_frames: u32,
        _channels: u32,
        _sample_rate_hz: u32,
        _timestamp_us: i64,
    ) -> u32 {
        SCREEN_AUDIO_ENQUEUE_CALLS.fetch_add(1, Ordering::Relaxed);
        NATIVE_SCREEN_FRAME_SINK_ACCEPTED
    }

    fn screen_audio_test_handle_ref() -> NativeScreenFrameSinkHandleRef {
        let handle = NativeScreenFrameSinkHandle {
            magic: NATIVE_SCREEN_FRAME_SINK_HANDLE_MAGIC,
            version: NATIVE_SCREEN_FRAME_SINK_HANDLE_VERSION,
            context: (&SCREEN_AUDIO_ENQUEUE_CALLS as *const AtomicU32).cast::<c_void>(),
            retain: screen_audio_test_retain,
            release: screen_audio_test_release,
            enqueue_nv12: None,
            enqueue_bgra: None,
            enqueue_mac_cv_pixel_buffer: None,
            enqueue_dmabuf: None,
            enqueue_shared_texture: None,
            enqueue_screen_audio: Some(screen_audio_test_enqueue),
        };
        NativeScreenFrameSinkHandleRef { handle }
    }

    #[test]
    fn enqueue_screen_audio_rejects_malformed_inputs_without_panicking() {
        SCREEN_AUDIO_ENQUEUE_CALLS.store(0, Ordering::Relaxed);
        let sink = screen_audio_test_handle_ref();

        assert_eq!(
            sink.enqueue_screen_audio_f32(&[0.0, 0.0], 1, 0, 48_000, 0),
            EnqueueOutcome::Rejected
        );
        assert_eq!(
            sink.enqueue_screen_audio_f32(&[], 0, 2, 48_000, 0),
            EnqueueOutcome::Rejected
        );
        assert_eq!(
            sink.enqueue_screen_audio_f32(&[0.0, 0.0, 0.0], 1, 2, 48_000, 0),
            EnqueueOutcome::Rejected
        );
        assert_eq!(SCREEN_AUDIO_ENQUEUE_CALLS.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn enqueue_screen_audio_accepts_whole_stereo_frames() {
        SCREEN_AUDIO_ENQUEUE_CALLS.store(0, Ordering::Relaxed);
        let sink = screen_audio_test_handle_ref();

        assert_eq!(
            sink.enqueue_screen_audio_f32(&[0.1, 0.2, 0.3, 0.4], 2, 2, 48_000, 7),
            EnqueueOutcome::Accepted
        );
        assert_eq!(SCREEN_AUDIO_ENQUEUE_CALLS.load(Ordering::Relaxed), 1);
    }
}
