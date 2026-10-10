// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use crate::backend::{BackendError, KeyedMutexBackend, NUM_SLOTS_DEFAULT, TextureFormat};
use crate::{MAX_FRAME_HEIGHT, MAX_FRAME_WIDTH};

pub const METAL_IOSURFACE_SEED_BASE: u64 = 0;

#[derive(Clone)]
pub struct IoSurfaceSlotHandle {
    inner: Arc<IoSurfaceSlotInner>,
}

impl IoSurfaceSlotHandle {
    pub fn slot_index(&self) -> u32 {
        let idx = self.inner.slot_index;
        assert!((idx as usize) < NUM_SLOTS_DEFAULT, "slot_index in range");
        assert!(
            self.inner.iosurface_handle != 0,
            "iosurface_handle non-zero"
        );
        idx
    }

    pub fn iosurface_handle(&self) -> u64 {
        let h = self.inner.iosurface_handle;
        assert!(h != 0, "iosurface_handle non-zero on read");
        assert!(
            (self.inner.slot_index as usize) < NUM_SLOTS_DEFAULT,
            "slot_index intact"
        );
        h
    }

    #[cfg(target_os = "macos")]
    pub fn iosurface_ptr(&self) -> *mut core::ffi::c_void {
        let p = self.inner.iosurface_ptr;
        assert!(!p.is_null(), "iosurface_ptr non-null on read");
        assert!(
            self.inner.iosurface_handle != 0,
            "handle non-zero alongside ptr"
        );
        p
    }

    pub fn current_key(&self) -> u64 {
        let key = self.inner.current_key.load(Ordering::Acquire);
        assert!(
            self.inner.iosurface_handle != 0,
            "handle intact during key read"
        );
        assert!(
            (self.inner.slot_index as usize) < NUM_SLOTS_DEFAULT,
            "slot_index intact"
        );
        key
    }

    pub fn is_acquired(&self) -> bool {
        let a = self.inner.acquired.load(Ordering::Acquire);
        assert!(
            self.inner.iosurface_handle != 0,
            "handle intact during acquired probe"
        );
        assert!(
            (self.inner.slot_index as usize) < NUM_SLOTS_DEFAULT,
            "slot_index intact"
        );
        a
    }
}

struct IoSurfaceSlotInner {
    slot_index: u32,
    iosurface_handle: u64,
    #[cfg(target_os = "macos")]
    iosurface_ptr: *mut core::ffi::c_void,
    current_key: AtomicU64,
    acquired: AtomicBool,
    completed: AtomicBool,
}

#[cfg(target_os = "macos")]
unsafe impl Send for IoSurfaceSlotInner {}
#[cfg(target_os = "macos")]
unsafe impl Sync for IoSurfaceSlotInner {}

pub struct MetalSharedTextureBackend {
    width: u32,
    height: u32,
    format: TextureFormat,
    slots_created: bool,
    slot_handles: Vec<u64>,
    #[cfg(target_os = "macos")]
    iosurfaces: Vec<crate::metal_iosurface_macos::OwnedIoSurface>,
}

impl MetalSharedTextureBackend {
    pub fn new() -> Self {
        let backend = Self {
            width: 0,
            height: 0,
            format: TextureFormat::Nv12,
            slots_created: false,
            slot_handles: Vec::with_capacity(NUM_SLOTS_DEFAULT),
            #[cfg(target_os = "macos")]
            iosurfaces: Vec::with_capacity(NUM_SLOTS_DEFAULT),
        };
        assert!(!backend.slots_created, "fresh backend has no slots");
        assert!(
            backend.slot_handles.is_empty(),
            "fresh backend handles empty"
        );
        backend
    }

    pub fn width(&self) -> u32 {
        assert!(self.width <= MAX_FRAME_WIDTH, "width within cap");
        assert!(self.height <= MAX_FRAME_HEIGHT, "height within cap");
        self.width
    }

    pub fn height(&self) -> u32 {
        assert!(self.height <= MAX_FRAME_HEIGHT, "height within cap");
        assert!(self.width <= MAX_FRAME_WIDTH, "width within cap");
        self.height
    }

    pub fn slot_iosurface_handle(&self, slot_index: u32) -> Option<u64> {
        let idx = slot_index as usize;
        if idx >= self.slot_handles.len() {
            return None;
        }
        let h = self.slot_handles[idx];
        assert!(h != 0, "stored iosurface handle non-zero");
        Some(h)
    }

    #[cfg(target_os = "macos")]
    pub fn slot_iosurface_ptr(&self, slot_index: u32) -> Option<*mut core::ffi::c_void> {
        let idx = slot_index as usize;
        if idx >= self.iosurfaces.len() {
            return None;
        }
        let p = self.iosurfaces[idx].as_ptr();
        assert!(!p.is_null(), "stored iosurface ptr non-null");
        assert!(
            idx < self.slot_handles.len(),
            "ptr slot mirrors handle slot"
        );
        Some(p)
    }

    #[cfg(target_os = "macos")]
    pub fn slot_iosurface_mut(
        &mut self,
        slot_index: u32,
    ) -> Option<&mut crate::metal_iosurface_macos::OwnedIoSurface> {
        let idx = slot_index as usize;
        if idx >= self.iosurfaces.len() {
            return None;
        }
        assert!(
            idx < self.slot_handles.len(),
            "mut slot mirrors handle slot"
        );
        Some(&mut self.iosurfaces[idx])
    }

    #[cfg(not(target_os = "macos"))]
    fn allocate_slot_handles(
        &mut self,
        _width: u32,
        _height: u32,
        _format: TextureFormat,
    ) -> Result<Vec<u64>, BackendError> {
        Err(BackendError::PlatformUnsupported {
            reason: "MetalSharedTextureBackend requires macOS",
        })
    }

    #[cfg(target_os = "macos")]
    fn allocate_slot_handles(
        &mut self,
        width: u32,
        height: u32,
        format: TextureFormat,
    ) -> Result<Vec<u64>, BackendError> {
        if !matches!(format, TextureFormat::Nv12) {
            return Err(BackendError::UnsupportedFormat { format });
        }
        let mut handles: Vec<u64> = Vec::with_capacity(NUM_SLOTS_DEFAULT);
        let mut owned: Vec<crate::metal_iosurface_macos::OwnedIoSurface> =
            Vec::with_capacity(NUM_SLOTS_DEFAULT);
        for slot in 0..NUM_SLOTS_DEFAULT {
            let surface = crate::metal_iosurface_macos::OwnedIoSurface::create_nv12(width, height)
                .map_err(|_| BackendError::PlatformUnsupported {
                    reason: "IOSurfaceCreate failed",
                })?;
            let raw = surface.handle();
            assert!(raw != 0, "IOSurface raw non-zero for slot");
            assert!(slot < NUM_SLOTS_DEFAULT, "slot index in range");
            handles.push(raw);
            owned.push(surface);
        }
        self.iosurfaces = owned;
        Ok(handles)
    }
}

impl Default for MetalSharedTextureBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl KeyedMutexBackend for MetalSharedTextureBackend {
    type SlotHandle = IoSurfaceSlotHandle;
    const NUM_SLOTS: usize = NUM_SLOTS_DEFAULT;

    fn create_slots(
        &mut self,
        width: u32,
        height: u32,
        format: TextureFormat,
    ) -> Result<Vec<IoSurfaceSlotHandle>, BackendError> {
        if width == 0 || height == 0 || width > MAX_FRAME_WIDTH || height > MAX_FRAME_HEIGHT {
            return Err(BackendError::DimensionsOutOfRange { width, height });
        }
        if !matches!(format, TextureFormat::Nv12) {
            return Err(BackendError::UnsupportedFormat { format });
        }
        assert!(!self.slots_created, "slots created once");
        let raw_handles = self.allocate_slot_handles(width, height, format)?;
        assert_eq!(
            raw_handles.len(),
            NUM_SLOTS_DEFAULT,
            "allocator returns NUM_SLOTS handles"
        );
        let mut handles: Vec<IoSurfaceSlotHandle> = Vec::with_capacity(NUM_SLOTS_DEFAULT);
        for (idx, raw) in raw_handles.iter().enumerate() {
            assert!(*raw != 0, "raw iosurface handle non-zero");
            #[cfg(target_os = "macos")]
            let surface_ptr = self.iosurfaces[idx].as_ptr();
            #[cfg(target_os = "macos")]
            assert!(!surface_ptr.is_null(), "surface ptr non-null at slot setup");
            let inner = IoSurfaceSlotInner {
                slot_index: idx as u32,
                iosurface_handle: *raw,
                #[cfg(target_os = "macos")]
                iosurface_ptr: surface_ptr,
                current_key: AtomicU64::new(METAL_IOSURFACE_SEED_BASE),
                acquired: AtomicBool::new(false),
                completed: AtomicBool::new(false),
            };
            handles.push(IoSurfaceSlotHandle {
                inner: Arc::new(inner),
            });
        }
        self.width = width;
        self.height = height;
        self.format = format;
        self.slot_handles = raw_handles;
        self.slots_created = true;
        assert_eq!(
            handles.len(),
            NUM_SLOTS_DEFAULT,
            "returned handle vector length"
        );
        assert!(self.slots_created, "slots_created flipped");
        Ok(handles)
    }

    fn acquire_write(&mut self, slot: &IoSurfaceSlotHandle, key: u64) -> Result<(), BackendError> {
        assert!(self.slots_created, "slots must exist before acquire");
        let current = slot.inner.current_key.load(Ordering::Acquire);
        if current != key {
            return Err(BackendError::KeyMismatch {
                expected: key,
                observed: current,
            });
        }
        let was_acquired = slot.inner.acquired.swap(true, Ordering::AcqRel);
        if was_acquired {
            return Err(BackendError::AcquireWhileWriting {
                slot_index: slot.inner.slot_index,
            });
        }
        #[cfg(target_os = "macos")]
        {
            let idx = slot.inner.slot_index as usize;
            if idx >= self.iosurfaces.len() {
                slot.inner.acquired.store(false, Ordering::Release);
                return Err(BackendError::PlatformUnsupported {
                    reason: "slot index out of range for IOSurface vector",
                });
            }
            if let Err(e) = self.iosurfaces[idx].lock_for_writing() {
                slot.inner.acquired.store(false, Ordering::Release);
                return Err(e);
            }
        }
        slot.inner.completed.store(false, Ordering::Release);
        assert!(
            slot.inner.acquired.load(Ordering::Acquire),
            "acquired flag set"
        );
        assert!(
            !slot.inner.completed.load(Ordering::Acquire),
            "completed cleared"
        );
        Ok(())
    }

    fn release_write(
        &mut self,
        slot: &IoSurfaceSlotHandle,
        next_key: u64,
    ) -> Result<(), BackendError> {
        assert!(self.slots_created, "slots must exist before release");
        let was_acquired = slot.inner.acquired.swap(false, Ordering::AcqRel);
        if !was_acquired {
            return Err(BackendError::ReleaseWithoutAcquire {
                slot_index: slot.inner.slot_index,
            });
        }
        #[cfg(target_os = "macos")]
        {
            let idx = slot.inner.slot_index as usize;
            if idx >= self.iosurfaces.len() {
                return Err(BackendError::PlatformUnsupported {
                    reason: "slot index out of range for IOSurface vector",
                });
            }
            self.iosurfaces[idx].unlock_after_writing()?;
        }
        slot.inner.current_key.store(next_key, Ordering::Release);
        slot.inner.completed.store(true, Ordering::Release);
        assert!(
            !slot.inner.acquired.load(Ordering::Acquire),
            "acquired cleared"
        );
        assert!(
            slot.inner.completed.load(Ordering::Acquire),
            "completed flag set"
        );
        Ok(())
    }

    fn poll_complete(&mut self, slot: &IoSurfaceSlotHandle) -> bool {
        let done = slot.inner.completed.load(Ordering::Acquire);
        assert!(self.slots_created, "slots exist for poll");
        assert!(
            (slot.inner.slot_index as usize) < Self::NUM_SLOTS,
            "slot index in range"
        );
        done
    }

    fn mark_consumed(&mut self, slot: &IoSurfaceSlotHandle) {
        assert!(self.slots_created, "slots exist for mark_consumed");
        assert!(
            (slot.inner.slot_index as usize) < Self::NUM_SLOTS,
            "slot index in range"
        );
        slot.inner.completed.store(false, Ordering::Release);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_slots_rejects_zero_dims() {
        let mut backend = MetalSharedTextureBackend::new();
        let err = backend.create_slots(0, 1080, TextureFormat::Nv12).err();
        assert!(matches!(
            err,
            Some(BackendError::DimensionsOutOfRange { .. })
        ));
    }

    #[test]
    fn create_slots_rejects_unsupported_format() {
        let mut backend = MetalSharedTextureBackend::new();
        let err = backend.create_slots(1920, 1080, TextureFormat::P010).err();
        assert!(matches!(err, Some(BackendError::UnsupportedFormat { .. })));
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_acquire_release_round_trip_marks_complete() {
        let mut backend = MetalSharedTextureBackend::new();
        let slots = backend
            .create_slots(64, 64, TextureFormat::Nv12)
            .expect("create");
        let slot = slots[0].clone();
        backend.acquire_write(&slot, 0).expect("acquire");
        assert!(slot.is_acquired());
        backend.release_write(&slot, 1).expect("release");
        assert!(!slot.is_acquired());
        assert!(backend.poll_complete(&slot));
        backend.mark_consumed(&slot);
        assert!(!backend.poll_complete(&slot));
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn macos_double_acquire_rejects() {
        let mut backend = MetalSharedTextureBackend::new();
        let slots = backend
            .create_slots(64, 64, TextureFormat::Nv12)
            .expect("create");
        let slot = slots[0].clone();
        backend.acquire_write(&slot, 0).expect("first acquire");
        let err = backend.acquire_write(&slot, 0).err();
        assert!(matches!(
            err,
            Some(BackendError::AcquireWhileWriting { .. })
        ));
    }
}
