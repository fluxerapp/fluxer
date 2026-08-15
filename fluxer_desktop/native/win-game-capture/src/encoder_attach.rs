// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Duration;

use parking_lot::{Condvar, Mutex};

use fluxer_encoder_ring::d3d11::D3D11SharedHandle;
use fluxer_encoder_ring::{
    D3D11KeyedMutexBackend, EncoderFrameRate, EncoderInputRing, EncoderReady, RING_SIZE, RingError,
    TextureFormat,
};

#[cfg(target_os = "windows")]
use fluxer_encoder_ring::{EncoderDims, FillReservation, NvencD3D11Handoff};

#[cfg(target_os = "windows")]
use windows::Win32::Graphics::Direct3D11::{ID3D11DeviceContext, ID3D11Texture2D};

pub const READY_WAIT_TIMEOUT_CAP: Duration = Duration::from_millis(17);

const EVICTIONS_PER_RESERVE_MAX: u64 = 2;

#[cfg(target_os = "windows")]
const NVENC_BITRATE_BPS_DEFAULT: u32 = 8_000_000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EncoderAttachError {
    AlreadyAttached,
    InvalidDimensions { width: u32, height: u32 },
    RingInitFailed,
    NotAttached,
    BlitFailed,
    SlotUnavailable,
    DeviceUnavailable,
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
            Self::BlitFailed => write!(f, "blit into ring slot failed"),
            Self::SlotUnavailable => write!(f, "no ring slot texture available"),
            Self::DeviceUnavailable => write!(f, "ring backend device unavailable"),
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

pub struct EncoderAttachment {
    ring: Mutex<EncoderInputRing<D3D11KeyedMutexBackend>>,
    ready_condvar: Condvar,
    attached: AtomicBool,
    frames_submitted: AtomicU64,
    frames_dropped: AtomicU64,
    ring_full_events: AtomicU64,
    failed_blits: AtomicU64,
    width: u32,
    height: u32,
    frame_rate: EncoderFrameRate,
    #[cfg(target_os = "windows")]
    context: ID3D11DeviceContext,
    #[cfg(target_os = "windows")]
    nvenc: Mutex<Option<NvencD3D11Handoff>>,
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
        let backend =
            D3D11KeyedMutexBackend::new().map_err(|_| EncoderAttachError::RingInitFailed)?;
        let mut ring = EncoderInputRing::new(backend);
        ring.initialise(width, height, TextureFormat::Nv12)
            .map_err(|_| EncoderAttachError::RingInitFailed)?;
        #[cfg(target_os = "windows")]
        let context = ring
            .backend_mut()
            .context()
            .ok_or(EncoderAttachError::DeviceUnavailable)?;
        #[cfg(target_os = "windows")]
        let nvenc = init_nvenc_pre_registered(&mut ring, width, height, frame_rate);
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
            #[cfg(target_os = "windows")]
            context,
            #[cfg(target_os = "windows")]
            nvenc: Mutex::new(nvenc),
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
        self.ready_condvar.notify_all();
        assert!(
            !self.attached.load(Ordering::Acquire),
            "detach flag cleared"
        );
        assert!(self.width > 0, "attachment intact across detach");
    }

    pub fn capacity(&self) -> usize {
        let cap = RING_SIZE;
        assert!(cap > 0, "ring capacity positive");
        assert_eq!(cap, 8, "ring capacity matches RING_SIZE");
        cap
    }

    pub fn note_ring_full(&self) {
        self.ring_full_events.fetch_add(1, Ordering::Relaxed);
    }

    #[cfg(target_os = "windows")]
    pub fn nvenc_pre_registered(&self) -> bool {
        let registered = self.nvenc.lock().is_some();
        assert!(self.width > 0, "width intact while reading nvenc state");
        assert!(self.height > 0, "height intact while reading nvenc state");
        registered
    }

    pub fn stats(&self) -> EncoderAttachStats {
        let stats = EncoderAttachStats {
            frames_submitted: self.frames_submitted.load(Ordering::Relaxed),
            frames_dropped: self.frames_dropped.load(Ordering::Relaxed),
            ring_full_events: self.ring_full_events.load(Ordering::Relaxed),
            failed_blits: self.failed_blits.load(Ordering::Relaxed),
        };
        assert!(
            stats.frames_submitted <= u64::MAX / 2,
            "submitted plausible"
        );
        assert!(stats.frames_dropped <= u64::MAX / 2, "dropped plausible");
        stats
    }

    fn account_evictions(&self, evicted_delta: u64) {
        assert!(
            evicted_delta <= EVICTIONS_PER_RESERVE_MAX,
            "evictions bounded per reserve"
        );
        assert!(self.width > 0, "attachment intact while accounting");
        if evicted_delta == 0 {
            return;
        }
        self.frames_dropped
            .fetch_add(evicted_delta, Ordering::Relaxed);
        self.ring_full_events
            .fetch_add(evicted_delta, Ordering::Relaxed);
    }

    pub fn submit_notify(&self) -> Result<(), EncoderAttachError> {
        if !self.attached.load(Ordering::Acquire) {
            return Err(EncoderAttachError::NotAttached);
        }
        let mut ring = self.ring.lock();
        let pre_dropped = ring.dropped_count();
        let result: Result<(), RingError> = ring.submit_skip_oldest(|_handle| {});
        let evicted_delta = ring.dropped_count().saturating_sub(pre_dropped);
        drop(ring);
        self.account_evictions(evicted_delta);
        match result {
            Ok(()) => {
                self.frames_submitted.fetch_add(1, Ordering::Relaxed);
                self.ready_condvar.notify_one();
                Ok(())
            }
            Err(RingError::FullDropped { .. }) => Ok(()),
            Err(_) => Err(EncoderAttachError::RingInitFailed),
        }
    }

    pub fn wait_next_ready(&self, timeout: Duration) -> Option<EncoderReady<D3D11SharedHandle>> {
        assert!(self.width > 0, "attachment width positive");
        assert!(self.height > 0, "attachment height positive");
        if !self.attached.load(Ordering::Acquire) {
            return None;
        }
        let capped_timeout = timeout.min(READY_WAIT_TIMEOUT_CAP);
        let mut ring = self.ring.lock();
        if let Some(ready) = ring.poll_next_ready() {
            return Some(ready);
        }
        let _ = self.ready_condvar.wait_for(&mut ring, capped_timeout);
        if !self.attached.load(Ordering::Acquire) {
            return None;
        }
        ring.poll_next_ready()
    }

    #[cfg(target_os = "windows")]
    fn cancel_reservation(&self, reservation: FillReservation<D3D11SharedHandle>) {
        assert!(
            (reservation.slot_index() as usize) < RING_SIZE,
            "cancelled slot within ring"
        );
        assert!(self.width > 0, "attachment intact while cancelling");
        let mut ring = self.ring.lock();
        let _ = ring.cancel(reservation);
    }

    #[cfg(target_os = "windows")]
    pub fn submit_capture_frame_with_blit(
        &self,
        capture_texture: &ID3D11Texture2D,
        capture_width: u32,
        capture_height: u32,
    ) -> Result<(), EncoderAttachError> {
        assert!(capture_width > 0, "capture width positive");
        assert!(capture_height > 0, "capture height positive");
        assert!(self.width > 0, "attachment width positive");
        assert!(self.height > 0, "attachment height positive");
        if !self.attached.load(Ordering::Acquire) {
            return Err(EncoderAttachError::NotAttached);
        }
        let mut ring = self.ring.lock();
        let pre_dropped = ring.dropped_count();
        let reserved = ring.reserve_skip_oldest();
        let evicted_delta = ring.dropped_count().saturating_sub(pre_dropped);
        let dest_texture = match &reserved {
            Ok(reservation) => ring
                .backend_mut()
                .texture_for_slot(reservation.slot_index()),
            Err(_) => None,
        };
        drop(ring);
        self.account_evictions(evicted_delta);
        let reservation = match reserved {
            Ok(reservation) => reservation,
            Err(RingError::FullDropped { .. }) => return Ok(()),
            Err(_) => return Err(EncoderAttachError::RingInitFailed),
        };
        let Some(dest_texture) = dest_texture else {
            self.cancel_reservation(reservation);
            self.failed_blits.fetch_add(1, Ordering::Relaxed);
            return Err(EncoderAttachError::BlitFailed);
        };
        unsafe {
            self.context.CopyResource(&dest_texture, capture_texture);
        }
        let mut ring = self.ring.lock();
        match ring.commit(reservation) {
            Ok(sequence) => {
                drop(ring);
                assert!(sequence > 0, "committed sequence positive");
                self.frames_submitted.fetch_add(1, Ordering::Relaxed);
                self.ready_condvar.notify_one();
                Ok(())
            }
            Err(_) => Err(EncoderAttachError::RingInitFailed),
        }
    }
}

#[cfg(target_os = "windows")]
fn collect_ring_slot_handles(
    ring: &mut EncoderInputRing<D3D11KeyedMutexBackend>,
) -> Option<Vec<D3D11SharedHandle>> {
    assert_eq!(
        ring.free_count(),
        RING_SIZE,
        "collect requires a fresh ring"
    );
    let mut reservations: Vec<FillReservation<D3D11SharedHandle>> = Vec::with_capacity(RING_SIZE);
    let mut handles: Vec<D3D11SharedHandle> = Vec::with_capacity(RING_SIZE);
    for _ in 0..RING_SIZE {
        let Ok(reservation) = ring.reserve() else {
            break;
        };
        handles.push(reservation.handle.clone());
        reservations.push(reservation);
    }
    let mut all_cancelled = true;
    for reservation in reservations.drain(..) {
        if ring.cancel(reservation).is_err() {
            all_cancelled = false;
        }
    }
    if !all_cancelled {
        return None;
    }
    assert_eq!(ring.free_count(), RING_SIZE, "all reservations returned");
    assert!(handles.len() <= RING_SIZE, "collected handles bounded");
    if handles.len() == RING_SIZE {
        Some(handles)
    } else {
        None
    }
}

#[cfg(target_os = "windows")]
fn init_nvenc_pre_registered(
    ring: &mut EncoderInputRing<D3D11KeyedMutexBackend>,
    width: u32,
    height: u32,
    frame_rate: EncoderFrameRate,
) -> Option<NvencD3D11Handoff> {
    assert!(width > 0, "nvenc init width positive");
    assert!(height > 0, "nvenc init height positive");
    assert!(frame_rate.numerator > 0, "nvenc frame rate positive");
    let device = ring.backend_mut().device()?;
    let handles = collect_ring_slot_handles(ring)?;
    assert_eq!(handles.len(), RING_SIZE, "pre-register covers whole ring");
    let dims = EncoderDims::new(width, height);
    let mut handoff =
        NvencD3D11Handoff::new_with_frame_rate(device, dims, NVENC_BITRATE_BPS_DEFAULT, frame_rate)
            .ok()?;
    handoff.pre_register_slots(&handles, dims).ok()?;
    Some(handoff)
}

impl Drop for EncoderAttachment {
    fn drop(&mut self) {
        self.attached.store(false, Ordering::Release);
        self.ready_condvar.notify_all();
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
        let err2 = EncoderAttachment::try_new(640, 0).err();
        assert!(matches!(
            err2,
            Some(EncoderAttachError::InvalidDimensions { .. })
        ));
    }

    #[test]
    fn ready_wait_timeout_cap_is_one_frame_interval() {
        assert!(READY_WAIT_TIMEOUT_CAP >= Duration::from_millis(1));
        assert!(READY_WAIT_TIMEOUT_CAP <= Duration::from_millis(33));
    }

    #[cfg(not(target_os = "windows"))]
    #[test]
    fn off_windows_init_fails() {
        let err = EncoderAttachment::try_new(640, 480).err();
        assert!(matches!(err, Some(EncoderAttachError::RingInitFailed)));
    }
}
