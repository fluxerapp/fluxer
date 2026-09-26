// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{fluxer_av_free, fluxer_vips_free, fluxer_webp_free};
use libc::{c_void, size_t};
use std::{collections::TryReserveError, ptr::NonNull, slice};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NativeAllocator {
    AV,
    Vips,
    WebP,
}

impl NativeAllocator {
    fn free(self, ptr: NonNull<c_void>) {
        match self {
            Self::AV => unsafe { fluxer_av_free(ptr.as_ptr()) },
            Self::Vips => unsafe { fluxer_vips_free(ptr.as_ptr()) },
            Self::WebP => unsafe { fluxer_webp_free(ptr.as_ptr()) },
        }
    }
}

#[derive(Debug)]
pub struct NativeBuffer {
    ptr: NonNull<c_void>,
    len: size_t,
    resident_bytes: size_t,
    allocator: NativeAllocator,
}

unsafe impl Send for NativeBuffer {}

impl NativeBuffer {
    pub(crate) unsafe fn from_av_owned_with_resident_bytes(
        ptr: *mut c_void,
        len: size_t,
        resident_bytes: size_t,
    ) -> Option<Self> {
        Self::from_raw_owned(ptr, len, resident_bytes, NativeAllocator::AV)
    }

    pub(crate) unsafe fn from_vips_owned(ptr: *mut c_void, len: size_t) -> Option<Self> {
        Self::from_raw_owned(ptr, len, len, NativeAllocator::Vips)
    }

    pub(crate) unsafe fn from_vips_owned_with_resident_bytes(
        ptr: *mut c_void,
        len: size_t,
        resident_bytes: size_t,
    ) -> Option<Self> {
        Self::from_raw_owned(ptr, len, resident_bytes, NativeAllocator::Vips)
    }

    pub(crate) unsafe fn from_webp_owned(ptr: *mut c_void, len: size_t) -> Option<Self> {
        Self::from_raw_owned(ptr, len, len, NativeAllocator::WebP)
    }

    fn from_raw_owned(
        ptr: *mut c_void,
        len: size_t,
        resident_bytes: size_t,
        allocator: NativeAllocator,
    ) -> Option<Self> {
        let ptr = NonNull::new(ptr)?;
        if len == 0 || resident_bytes < len {
            allocator.free(ptr);
            return None;
        }
        if len > isize::MAX as usize {
            allocator.free(ptr);
            return None;
        }
        Some(Self {
            ptr,
            len,
            resident_bytes,
            allocator,
        })
    }

    pub fn len(&self) -> usize {
        assert!(self.len > 0, "owned native buffer must not be empty");
        self.len
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    pub fn resident_bytes(&self) -> usize {
        assert!(self.resident_bytes >= self.len);
        self.resident_bytes
    }

    pub fn as_slice(&self) -> &[u8] {
        assert!(self.len > 0, "owned native buffer must not be empty");
        assert!(
            self.len <= isize::MAX as usize,
            "owned native buffer length must fit isize"
        );
        unsafe { slice::from_raw_parts(self.ptr.cast::<u8>().as_ptr(), self.len) }
    }

    pub fn try_to_vec(&self) -> Result<Vec<u8>, TryReserveError> {
        let mut buffer = Vec::new();
        buffer.try_reserve_exact(self.len)?;
        buffer.extend_from_slice(self.as_slice());
        Ok(buffer)
    }
}

impl Drop for NativeBuffer {
    fn drop(&mut self) {
        self.allocator.free(self.ptr);
    }
}

impl AsRef<[u8]> for NativeBuffer {
    fn as_ref(&self) -> &[u8] {
        self.as_slice()
    }
}
