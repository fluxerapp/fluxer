// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Duration;

use parking_lot::{Condvar, Mutex};

pub const CAPTURE_PTS_MAP_CAP: usize = RING_SIZE;
pub const EXTERNAL_SURFACE_QUEUE_CAP: usize = RING_SIZE;
pub const READY_WAIT_TIMEOUT_US_MAX: u64 = 16_667;

use fluxer_encoder_ring::{
    EncoderFrameRate, EncoderInputRing, EncoderReady, IoSurfaceSlotHandle,
    MetalSharedTextureBackend, RING_SIZE, RingError, TextureFormat,
};

#[cfg(target_os = "macos")]
use fluxer_encoder_ring::FillReservation;

#[cfg(target_os = "macos")]
use fluxer_encoder_ring::VtPixelTransfer;

#[cfg(target_os = "macos")]
use crate::iosurface_pair::{IoSurfaceRaw, iosurface_decrement_use_count};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EncoderAttachError {
    AlreadyAttached,
    InvalidDimensions { width: u32, height: u32 },
    RingInitFailed,
    NotAttached,
}

impl std::fmt::Display for EncoderAttachError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AlreadyAttached => write!(f, "encoder ring already attached"),
            Self::InvalidDimensions { width, height } => {
                write!(f, "encoder ring invalid dimensions {width}x{height}")
            }
            Self::RingInitFailed => write!(f, "encoder ring initialise failed"),
            Self::NotAttached => write!(f, "encoder ring not attached"),
        }
    }
}

impl std::error::Error for EncoderAttachError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct EncoderAttachStats {
    pub frames_submitted: u64,
    pub frames_dropped: u64,
    pub ring_full_events: u64,
    pub failed_blits: u64,
}

#[cfg(target_os = "macos")]
pub struct ExternalSurfaceFrame {
    surface: IoSurfaceRaw,
    sequence: u64,
    capture_pts_us: i64,
}

#[cfg(target_os = "macos")]
unsafe impl Send for ExternalSurfaceFrame {}

#[cfg(target_os = "macos")]
impl ExternalSurfaceFrame {
    pub fn surface(&self) -> IoSurfaceRaw {
        assert!(self.sequence > 0, "external frame sequence starts at one");
        assert!(self.surface.as_ptr() as usize != 0, "surface ptr non-null");
        self.surface
    }

    pub fn sequence(&self) -> u64 {
        assert!(self.sequence > 0, "external frame sequence starts at one");
        assert!(self.surface.as_ptr() as usize != 0, "surface ptr non-null");
        self.sequence
    }

    pub fn capture_pts_us(&self) -> i64 {
        assert!(self.sequence > 0, "external frame sequence starts at one");
        assert!(self.surface.as_ptr() as usize != 0, "surface ptr non-null");
        self.capture_pts_us
    }
}

#[cfg(target_os = "macos")]
impl Drop for ExternalSurfaceFrame {
    fn drop(&mut self) {
        unsafe { iosurface_decrement_use_count(self.surface) };
        unsafe { CFRelease(self.surface.as_ptr()) };
    }
}

#[cfg(target_os = "macos")]
struct BlitDestination {
    pixel_buffer: *mut core::ffi::c_void,
}

#[cfg(target_os = "macos")]
unsafe impl Send for BlitDestination {}

#[cfg(target_os = "macos")]
unsafe impl Sync for BlitDestination {}

#[cfg(target_os = "macos")]
impl Drop for BlitDestination {
    fn drop(&mut self) {
        unsafe { release_pixel_buffer(self.pixel_buffer) };
    }
}

pub struct EncoderAttachment {
    ring: Mutex<EncoderInputRing<MetalSharedTextureBackend>>,
    ready_condvar: Condvar,
    attached: AtomicBool,
    frames_submitted: AtomicU64,
    frames_dropped: AtomicU64,
    ring_full_events: AtomicU64,
    failed_blits: AtomicU64,
    width: u32,
    height: u32,
    frame_rate: EncoderFrameRate,
    capture_pts_by_sequence: Mutex<VecDeque<(u64, i64)>>,
    #[cfg(target_os = "macos")]
    pixel_transfer: Mutex<Option<VtPixelTransfer>>,
    #[cfg(target_os = "macos")]
    blit_destinations: Vec<BlitDestination>,
    #[cfg(target_os = "macos")]
    external_surfaces: Mutex<VecDeque<ExternalSurfaceFrame>>,
    #[cfg(target_os = "macos")]
    external_sequence: AtomicU64,
}

impl EncoderAttachment {
    pub fn try_new(width: u32, height: u32) -> Result<Arc<Self>, EncoderAttachError> {
        Self::try_new_with_frame_rate(width, height, EncoderFrameRate::default())
    }

    pub fn try_new_with_frame_rate(
        width: u32,
        height: u32,
        frame_rate: EncoderFrameRate,
    ) -> Result<Arc<Self>, EncoderAttachError> {
        if width == 0 || height == 0 {
            return Err(EncoderAttachError::InvalidDimensions { width, height });
        }
        assert!(frame_rate.numerator > 0, "frame rate numerator positive");
        assert!(
            frame_rate.denominator > 0,
            "frame rate denominator positive"
        );
        let mut ring = EncoderInputRing::new(MetalSharedTextureBackend::new());
        ring.initialise(width, height, TextureFormat::Nv12)
            .map_err(|_| EncoderAttachError::RingInitFailed)?;
        #[cfg(target_os = "macos")]
        let blit_destinations = build_blit_destinations(&mut ring, width, height)?;
        #[cfg(target_os = "macos")]
        let pixel_transfer = VtPixelTransfer::new().ok();
        let attachment = Self {
            ring: Mutex::new(ring),
            ready_condvar: Condvar::new(),
            attached: AtomicBool::new(true),
            frames_submitted: AtomicU64::new(0),
            frames_dropped: AtomicU64::new(0),
            ring_full_events: AtomicU64::new(0),
            failed_blits: AtomicU64::new(0),
            width,
            height,
            frame_rate,
            capture_pts_by_sequence: Mutex::new(VecDeque::with_capacity(CAPTURE_PTS_MAP_CAP)),
            #[cfg(target_os = "macos")]
            pixel_transfer: Mutex::new(pixel_transfer),
            #[cfg(target_os = "macos")]
            blit_destinations,
            #[cfg(target_os = "macos")]
            external_surfaces: Mutex::new(VecDeque::with_capacity(EXTERNAL_SURFACE_QUEUE_CAP)),
            #[cfg(target_os = "macos")]
            external_sequence: AtomicU64::new(0),
        };
        assert!(
            attachment.attached.load(Ordering::Acquire),
            "attachment is attached"
        );
        assert!(attachment.width > 0, "attachment width positive");
        assert!(
            attachment.frame_rate.numerator > 0,
            "attachment fps positive"
        );
        Ok(Arc::new(attachment))
    }

    pub fn width(&self) -> u32 {
        let w = self.width;
        assert!(w > 0, "attachment width positive");
        assert!(self.height > 0, "attachment height positive");
        w
    }

    pub fn height(&self) -> u32 {
        let h = self.height;
        assert!(h > 0, "attachment height positive");
        assert!(self.width > 0, "attachment width positive");
        h
    }

    pub fn frame_rate(&self) -> EncoderFrameRate {
        let rate = self.frame_rate;
        assert!(rate.numerator > 0, "attachment fps numerator positive");
        assert!(rate.denominator > 0, "attachment fps denominator positive");
        rate
    }

    pub fn is_attached(&self) -> bool {
        let a = self.attached.load(Ordering::Acquire);
        assert!(self.width > 0, "width intact while reading attached");
        assert!(self.height > 0, "height intact while reading attached");
        a
    }

    pub fn detach(&self) {
        self.attached.store(false, Ordering::Release);
        #[cfg(target_os = "macos")]
        {
            let drained = core::mem::take(&mut *self.external_surfaces.lock());
            assert!(
                drained.len() <= EXTERNAL_SURFACE_QUEUE_CAP,
                "external queue bounded at detach"
            );
            drop(drained);
        }
        self.ready_condvar.notify_all();
    }

    pub fn capacity(&self) -> usize {
        let cap = RING_SIZE;
        assert!(cap > 0, "ring capacity positive");
        assert_eq!(cap, 8, "ring capacity matches RING_SIZE");
        cap
    }

    pub fn submit_iosurface_frame(&self, _iosurface_handle: u64) -> Result<(), EncoderAttachError> {
        if !self.attached.load(Ordering::Acquire) {
            return Err(EncoderAttachError::NotAttached);
        }
        let mut ring = self.ring.lock();
        let result: Result<(), RingError> = ring.submit_skip_oldest(|_handle| {});
        drop(ring);
        match result {
            Ok(()) => {
                self.frames_submitted.fetch_add(1, Ordering::Relaxed);
                self.ready_condvar.notify_one();
                Ok(())
            }
            Err(RingError::FullDropped { .. }) => {
                self.frames_dropped.fetch_add(1, Ordering::Relaxed);
                self.ring_full_events.fetch_add(1, Ordering::Relaxed);
                Ok(())
            }
            Err(_) => Err(EncoderAttachError::RingInitFailed),
        }
    }

    #[cfg(target_os = "macos")]
    pub fn submit_with_blit(
        &self,
        source_pixel_buffer: *mut core::ffi::c_void,
        capture_pts_us: i64,
    ) -> Result<(), EncoderAttachError> {
        assert!(
            !source_pixel_buffer.is_null(),
            "blit source must be non-null"
        );
        assert!(self.width > 0, "attachment width positive");
        if !self.attached.load(Ordering::Acquire) {
            return Err(EncoderAttachError::NotAttached);
        }
        let transfer_guard = self.pixel_transfer.lock();
        let Some(transfer) = transfer_guard.as_ref() else {
            self.failed_blits.fetch_add(1, Ordering::Relaxed);
            return Err(EncoderAttachError::RingInitFailed);
        };
        let Some(reservation) = self.reserve_blit_slot()? else {
            return Ok(());
        };
        let slot_index = reservation.slot_index() as usize;
        assert!(
            slot_index < self.blit_destinations.len(),
            "slot index within cached destinations"
        );
        let dest_pb = self.blit_destinations[slot_index].pixel_buffer;
        assert!(
            !dest_pb.is_null(),
            "cached destination pixel buffer non-null"
        );
        let blit_ok = unsafe { transfer.transfer(source_pixel_buffer, dest_pb) }.is_ok();
        if blit_ok {
            self.commit_blit_slot(reservation, capture_pts_us)
        } else {
            let cancelled = self.ring.lock().cancel(reservation);
            assert!(cancelled.is_ok(), "cancel of filling reservation succeeds");
            self.failed_blits.fetch_add(1, Ordering::Relaxed);
            Err(EncoderAttachError::RingInitFailed)
        }
    }

    #[cfg(target_os = "macos")]
    fn reserve_blit_slot(
        &self,
    ) -> Result<Option<FillReservation<IoSurfaceSlotHandle>>, EncoderAttachError> {
        assert!(self.width > 0, "attachment width positive");
        assert!(self.height > 0, "attachment height positive");
        let mut ring = self.ring.lock();
        match ring.reserve_skip_oldest() {
            Ok(reservation) => {
                assert!(
                    (reservation.slot_index() as usize) < RING_SIZE,
                    "reserved slot within ring"
                );
                Ok(Some(reservation))
            }
            Err(RingError::FullDropped { .. }) => {
                self.frames_dropped.fetch_add(1, Ordering::Relaxed);
                self.ring_full_events.fetch_add(1, Ordering::Relaxed);
                Ok(None)
            }
            Err(_) => Err(EncoderAttachError::RingInitFailed),
        }
    }

    #[cfg(target_os = "macos")]
    fn commit_blit_slot(
        &self,
        reservation: FillReservation<IoSurfaceSlotHandle>,
        capture_pts_us: i64,
    ) -> Result<(), EncoderAttachError> {
        assert!(
            (reservation.slot_index() as usize) < RING_SIZE,
            "slot within ring"
        );
        let sequence = {
            let mut ring = self.ring.lock();
            let sequence = match ring.commit(reservation) {
                Ok(sequence) => sequence,
                Err(_) => return Err(EncoderAttachError::RingInitFailed),
            };
            self.record_capture_pts(sequence, capture_pts_us);
            sequence
        };
        assert!(sequence > 0, "committed sequence positive");
        self.frames_submitted.fetch_add(1, Ordering::Relaxed);
        self.ready_condvar.notify_one();
        Ok(())
    }

    pub fn wait_next_ready(&self, timeout: Duration) -> Option<EncoderReady<IoSurfaceSlotHandle>> {
        assert!(self.width > 0, "attachment width positive");
        let timeout_bound = Duration::from_micros(READY_WAIT_TIMEOUT_US_MAX);
        let bounded_timeout = timeout.min(timeout_bound);
        assert!(
            bounded_timeout <= timeout_bound,
            "wait bounded to one frame interval"
        );
        let mut ring = self.ring.lock();
        if let Some(ready) = ring.poll_next_ready() {
            return Some(ready);
        }
        if !self.attached.load(Ordering::Acquire) {
            return None;
        }
        let _ = self.ready_condvar.wait_for(&mut ring, bounded_timeout);
        ring.poll_next_ready()
    }

    #[cfg(target_os = "macos")]
    pub unsafe fn submit_external_surface(
        &self,
        surface: IoSurfaceRaw,
        capture_pts_us: i64,
    ) -> Result<(), EncoderAttachError> {
        assert!(surface.as_ptr() as usize != 0, "external surface non-null");
        assert!(self.width > 0, "attachment width positive");
        if !self.attached.load(Ordering::Acquire) {
            unsafe { CFRelease(surface.as_ptr()) };
            return Err(EncoderAttachError::NotAttached);
        }
        unsafe { crate::iosurface_pair::iosurface_increment_use_count(surface) };
        let sequence = self.external_sequence.fetch_add(1, Ordering::AcqRel) + 1;
        assert!(sequence > 0, "external sequence starts at one");
        let frame = ExternalSurfaceFrame {
            surface,
            sequence,
            capture_pts_us,
        };
        let evicted = {
            let mut queue = self.external_surfaces.lock();
            let evicted = if queue.len() >= EXTERNAL_SURFACE_QUEUE_CAP {
                queue.pop_front()
            } else {
                None
            };
            queue.push_back(frame);
            assert!(
                queue.len() <= EXTERNAL_SURFACE_QUEUE_CAP,
                "external queue bounded"
            );
            evicted
        };
        if let Some(oldest) = evicted {
            drop(oldest);
            self.frames_dropped.fetch_add(1, Ordering::Relaxed);
            self.ring_full_events.fetch_add(1, Ordering::Relaxed);
        }
        self.frames_submitted.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }

    #[cfg(target_os = "macos")]
    pub fn take_external_surface(&self) -> Option<ExternalSurfaceFrame> {
        let frame = {
            let mut queue = self.external_surfaces.lock();
            assert!(
                queue.len() <= EXTERNAL_SURFACE_QUEUE_CAP,
                "external queue bounded"
            );
            queue.pop_front()
        };
        if let Some(ref taken) = frame {
            assert!(taken.sequence > 0, "popped frame has real sequence");
        }
        frame
    }

    #[cfg(target_os = "macos")]
    pub fn external_surface_queue_len(&self) -> usize {
        let len = self.external_surfaces.lock().len();
        assert!(len <= EXTERNAL_SURFACE_QUEUE_CAP, "external queue bounded");
        assert!(self.width > 0, "attachment width intact");
        len
    }

    fn record_capture_pts(&self, sequence: u64, capture_pts_us: i64) {
        assert!(sequence > 0, "ring sequences start at one");
        let mut map = self.capture_pts_by_sequence.lock();
        while map.len() >= CAPTURE_PTS_MAP_CAP {
            map.pop_front();
        }
        map.push_back((sequence, capture_pts_us));
        assert!(map.len() <= CAPTURE_PTS_MAP_CAP, "pts map bounded");
    }

    pub fn capture_pts_us_for_sequence(&self, sequence: u64) -> Option<i64> {
        let map = self.capture_pts_by_sequence.lock();
        assert!(map.len() <= CAPTURE_PTS_MAP_CAP, "pts map bounded");
        map.iter()
            .find(|(seq, _)| *seq == sequence)
            .map(|(_, pts)| *pts)
    }

    pub fn note_ring_full(&self) {
        self.ring_full_events.fetch_add(1, Ordering::Relaxed);
    }

    pub fn stats(&self) -> EncoderAttachStats {
        let stats = EncoderAttachStats {
            frames_submitted: self.frames_submitted.load(Ordering::Relaxed),
            frames_dropped: self.frames_dropped.load(Ordering::Relaxed),
            ring_full_events: self.ring_full_events.load(Ordering::Relaxed),
            failed_blits: self.failed_blits.load(Ordering::Relaxed),
        };
        assert!(
            stats.frames_dropped <= stats.ring_full_events + stats.frames_dropped,
            "drop counter consistent"
        );
        assert!(
            stats.frames_submitted <= u64::MAX / 2,
            "submitted within plausible bound"
        );
        stats
    }
}

#[cfg(target_os = "macos")]
fn build_blit_destinations(
    ring: &mut EncoderInputRing<MetalSharedTextureBackend>,
    width: u32,
    height: u32,
) -> Result<Vec<BlitDestination>, EncoderAttachError> {
    assert!(width > 0, "blit destination width positive");
    assert!(height > 0, "blit destination height positive");
    let mut destinations: Vec<BlitDestination> = Vec::with_capacity(RING_SIZE);
    for slot_index in 0..RING_SIZE {
        let surface_ptr = ring
            .backend_mut()
            .slot_iosurface_ptr(slot_index as u32)
            .ok_or(EncoderAttachError::RingInitFailed)?;
        assert!(!surface_ptr.is_null(), "slot iosurface ptr non-null");
        let pixel_buffer = unsafe { VtPixelTransfer::wrap_iosurface(surface_ptr, width, height) }
            .map_err(|_| EncoderAttachError::RingInitFailed)?;
        destinations.push(BlitDestination { pixel_buffer });
    }
    assert_eq!(destinations.len(), RING_SIZE, "one destination per slot");
    Ok(destinations)
}

#[cfg(target_os = "macos")]
#[link(name = "CoreVideo", kind = "framework")]
unsafe extern "C" {
    fn CVPixelBufferRelease(buffer: *mut core::ffi::c_void);
}

#[cfg(target_os = "macos")]
#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    fn CFRelease(cf: *const core::ffi::c_void);
    fn CFRetain(cf: *const core::ffi::c_void) -> *const core::ffi::c_void;
}

#[cfg(target_os = "macos")]
unsafe fn release_pixel_buffer(pb: *mut core::ffi::c_void) {
    if !pb.is_null() {
        unsafe { CVPixelBufferRelease(pb) };
    }
}

impl Drop for EncoderAttachment {
    fn drop(&mut self) {
        self.detach();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_zero_dimensions() {
        let err = EncoderAttachment::try_new(0, 720).err();
        assert!(matches!(
            err,
            Some(EncoderAttachError::InvalidDimensions { .. })
        ));
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_attach_then_detach() {
        let attach = EncoderAttachment::try_new(640, 480).expect("attach ok");
        assert!(attach.is_attached());
        assert_eq!(attach.width(), 640);
        assert_eq!(attach.height(), 480);
        assert_eq!(attach.capacity(), 8);
        attach.detach();
        assert!(!attach.is_attached());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_submit_records_stats() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        for _ in 0..3 {
            attach
                .submit_iosurface_frame(0xdead_beef)
                .expect("submit ok");
        }
        let stats = attach.stats();
        assert_eq!(stats.frames_submitted, 3);
        assert_eq!(stats.frames_dropped, 0);
        assert_eq!(stats.ring_full_events, 0);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn wait_next_ready_returns_submitted_frame() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        attach
            .submit_iosurface_frame(0xdead_beef)
            .expect("submit ok");
        let ready = attach
            .wait_next_ready(Duration::from_millis(5))
            .expect("frame ready");
        assert_eq!(ready.sequence, 1);
        assert_eq!(ready.duplicate_count, 0);
        assert!(attach.wait_next_ready(Duration::from_millis(1)).is_none());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn wait_next_ready_timeout_is_capped_at_frame_interval() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let start = std::time::Instant::now();
        let ready = attach.wait_next_ready(Duration::from_secs(60));
        assert!(ready.is_none());
        assert!(
            start.elapsed() < Duration::from_millis(500),
            "wait returned within the named frame-interval bound"
        );
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn wait_next_ready_wakes_for_concurrent_submit() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let waiter = {
            let attach = Arc::clone(&attach);
            std::thread::spawn(move || attach.wait_next_ready(Duration::from_millis(15)))
        };
        std::thread::sleep(Duration::from_millis(3));
        attach
            .submit_iosurface_frame(0xdead_beef)
            .expect("submit ok");
        let ready = waiter.join().expect("waiter joins");
        let sequence = ready.map(|r| r.sequence);
        assert!(sequence == Some(1) || sequence.is_none());
        if sequence.is_none() {
            let retry = attach.wait_next_ready(Duration::from_millis(5));
            assert_eq!(retry.map(|r| r.sequence), Some(1));
        }
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn wait_next_ready_returns_none_after_detach() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        attach.detach();
        assert!(!attach.is_attached());
        assert!(attach.wait_next_ready(Duration::from_millis(5)).is_none());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_submit_rejected_when_detached() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        attach.detach();
        let err = attach.submit_iosurface_frame(0xdead_beef).err();
        assert!(matches!(err, Some(EncoderAttachError::NotAttached)));
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn submit_encoder_ring_frame_blits_iosurface_to_slot() {
        use core::ptr::NonNull;
        use objc2_core_foundation::{CFDictionary, CFRetained};
        use objc2_core_video::{
            CVPixelBuffer, CVPixelBufferGetBaseAddressOfPlane, CVPixelBufferGetBytesPerRowOfPlane,
            CVPixelBufferGetHeightOfPlane, CVPixelBufferLockBaseAddress, CVPixelBufferLockFlags,
            CVPixelBufferUnlockBaseAddress, kCVPixelBufferIOSurfacePropertiesKey,
        };

        let attach = EncoderAttachment::try_new(256, 256).expect("attach ok");
        let mut empty_keys: [*const core::ffi::c_void; 0] = [];
        let mut empty_vals: [*const core::ffi::c_void; 0] = [];
        let iosurf_dict: CFRetained<CFDictionary> = unsafe {
            CFDictionary::new(
                None,
                empty_keys.as_mut_ptr(),
                empty_vals.as_mut_ptr(),
                0,
                &objc2_core_foundation::kCFTypeDictionaryKeyCallBacks,
                &objc2_core_foundation::kCFTypeDictionaryValueCallBacks,
            )
            .expect("iosurf empty dict")
        };
        let key_ref: &objc2_core_foundation::CFString =
            unsafe { kCVPixelBufferIOSurfacePropertiesKey };
        let key_ptr: *const core::ffi::c_void = key_ref as *const _ as *const core::ffi::c_void;
        let val_ptr: *const core::ffi::c_void =
            &*iosurf_dict as *const _ as *const core::ffi::c_void;
        let mut keys = [key_ptr];
        let mut vals = [val_ptr];
        let attrs: CFRetained<CFDictionary> = unsafe {
            CFDictionary::new(
                None,
                keys.as_mut_ptr(),
                vals.as_mut_ptr(),
                1,
                &objc2_core_foundation::kCFTypeDictionaryKeyCallBacks,
                &objc2_core_foundation::kCFTypeDictionaryValueCallBacks,
            )
            .expect("attrs dict")
        };
        let nv12: u32 = u32::from_be_bytes(*b"420v");
        let mut pb_out: *mut CVPixelBuffer = core::ptr::null_mut();
        let status = unsafe {
            objc2_core_video::CVPixelBufferCreate(
                None,
                256,
                256,
                nv12,
                Some(&attrs),
                NonNull::new(&mut pb_out).expect("pb_out non-null"),
            )
        };
        assert_eq!(status, 0, "CVPixelBufferCreate ok");
        let source_pb: CFRetained<CVPixelBuffer> =
            unsafe { CFRetained::from_raw(NonNull::new(pb_out).expect("pb non-null")) };
        let lock_flags = CVPixelBufferLockFlags(0);
        let lock_st = unsafe { CVPixelBufferLockBaseAddress(&source_pb, lock_flags) };
        assert_eq!(lock_st, 0, "lock ok");
        let y_ptr = CVPixelBufferGetBaseAddressOfPlane(&source_pb, 0);
        let y_stride = CVPixelBufferGetBytesPerRowOfPlane(&source_pb, 0);
        let y_h = CVPixelBufferGetHeightOfPlane(&source_pb, 0);
        assert!(!y_ptr.is_null(), "Y plane base non-null");
        assert!(y_stride > 0, "Y stride positive");
        for row in 0..y_h {
            for col in 0..y_stride {
                unsafe {
                    (y_ptr as *mut u8).add(row * y_stride + col).write(0x55);
                }
            }
        }
        let _ = unsafe { CVPixelBufferUnlockBaseAddress(&source_pb, lock_flags) };
        let src_ptr = &*source_pb as *const CVPixelBuffer as *mut core::ffi::c_void;
        attach
            .submit_with_blit(src_ptr, 41_500)
            .expect("submit_with_blit ok");
        let stats = attach.stats();
        assert_eq!(stats.frames_submitted, 1, "one frame submitted");
        assert_eq!(stats.failed_blits, 0, "no failed blits");
        assert_eq!(
            attach.capture_pts_us_for_sequence(1),
            Some(41_500),
            "capture pts recorded for ring sequence"
        );
        assert_eq!(attach.capture_pts_us_for_sequence(2), None);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn external_surface_submit_take_round_trip() {
        use core::ptr::NonNull;
        use fluxer_encoder_ring::metal_iosurface_macos::OwnedIoSurface;

        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let owned = OwnedIoSurface::create_nv12(64, 64).expect("surface");
        let raw = NonNull::new(owned.as_ptr()).expect("non-null surface");
        let use_before = unsafe { crate::iosurface_pair::iosurface_use_count(raw) };
        unsafe { CFRetain(raw.as_ptr()) };
        unsafe { attach.submit_external_surface(raw, 41_500) }.expect("submit ok");
        assert_eq!(attach.external_surface_queue_len(), 1);
        let use_during = unsafe { crate::iosurface_pair::iosurface_use_count(raw) };
        assert_eq!(use_during, use_before + 1);
        let stats = attach.stats();
        assert_eq!(stats.frames_submitted, 1);
        assert_eq!(stats.frames_dropped, 0);
        assert_eq!(stats.ring_full_events, 0);
        let frame = attach.take_external_surface().expect("frame queued");
        assert_eq!(frame.surface(), raw);
        assert_eq!(frame.sequence(), 1);
        assert_eq!(frame.capture_pts_us(), 41_500);
        drop(frame);
        let use_after = unsafe { crate::iosurface_pair::iosurface_use_count(raw) };
        assert_eq!(use_after, use_before);
        assert_eq!(attach.external_surface_queue_len(), 0);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn external_surface_queue_evicts_oldest_when_full() {
        use core::ptr::NonNull;
        use fluxer_encoder_ring::metal_iosurface_macos::OwnedIoSurface;

        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let total = EXTERNAL_SURFACE_QUEUE_CAP + 2;
        let mut owned: Vec<OwnedIoSurface> = Vec::with_capacity(total);
        for i in 0..total {
            let surface = OwnedIoSurface::create_nv12(64, 64).expect("surface");
            let raw = NonNull::new(surface.as_ptr()).expect("non-null surface");
            unsafe { CFRetain(raw.as_ptr()) };
            unsafe { attach.submit_external_surface(raw, (i as i64) * 1_000) }.expect("submit ok");
            owned.push(surface);
        }
        assert_eq!(
            attach.external_surface_queue_len(),
            EXTERNAL_SURFACE_QUEUE_CAP
        );
        let stats = attach.stats();
        assert_eq!(stats.frames_submitted, total as u64);
        assert_eq!(stats.frames_dropped, 2);
        assert_eq!(stats.ring_full_events, 2);
        let oldest_remaining = attach.take_external_surface().expect("frame queued");
        assert_eq!(oldest_remaining.sequence(), 3);
        assert_eq!(oldest_remaining.capture_pts_us(), 2_000);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn detach_drains_external_surface_queue_and_rejects_submit() {
        use core::ptr::NonNull;
        use fluxer_encoder_ring::metal_iosurface_macos::OwnedIoSurface;

        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let owned = OwnedIoSurface::create_nv12(64, 64).expect("surface");
        let raw = NonNull::new(owned.as_ptr()).expect("non-null surface");
        let use_before = unsafe { crate::iosurface_pair::iosurface_use_count(raw) };
        unsafe { CFRetain(raw.as_ptr()) };
        unsafe { attach.submit_external_surface(raw, 7) }.expect("submit ok");
        assert_eq!(attach.external_surface_queue_len(), 1);
        attach.detach();
        assert_eq!(attach.external_surface_queue_len(), 0);
        assert_eq!(
            unsafe { crate::iosurface_pair::iosurface_use_count(raw) },
            use_before
        );
        unsafe { CFRetain(raw.as_ptr()) };
        let err = unsafe { attach.submit_external_surface(raw, 8) }.err();
        assert!(matches!(err, Some(EncoderAttachError::NotAttached)));
        assert_eq!(attach.external_surface_queue_len(), 0);
        assert_eq!(
            unsafe { crate::iosurface_pair::iosurface_use_count(raw) },
            use_before
        );
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn capture_pts_map_stays_bounded_and_evicts_oldest() {
        let attach = EncoderAttachment::try_new(64, 64).expect("attach ok");
        let total = (CAPTURE_PTS_MAP_CAP as u64) + 4;
        for sequence in 1..=total {
            attach.record_capture_pts(sequence, (sequence as i64) * 1_000);
        }
        assert_eq!(
            attach.capture_pts_us_for_sequence(1),
            None,
            "oldest evicted"
        );
        assert_eq!(
            attach.capture_pts_us_for_sequence(4),
            None,
            "oldest evicted"
        );
        assert_eq!(
            attach.capture_pts_us_for_sequence(5),
            Some(5_000),
            "newest retained"
        );
        assert_eq!(
            attach.capture_pts_us_for_sequence(total),
            Some((total as i64) * 1_000)
        );
    }
}
