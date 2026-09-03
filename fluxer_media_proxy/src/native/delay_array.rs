// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{NativeStatus, VipsImageHandle, fluxer_free_int_array, fluxer_vips_read_delays_ms};
use libc::c_int;
use std::{ptr::NonNull, slice};

pub struct VipsDelayArray {
    ptr: NonNull<c_int>,
    len: usize,
}

impl VipsDelayArray {
    pub fn read(image: &VipsImageHandle<'_>, n_pages: c_int) -> Result<Self, NativeStatus> {
        assert!(n_pages > 0, "native delay page count must be positive");
        let mut out_ptr = std::ptr::null_mut();
        let mut out_len = 0;
        let status = NativeStatus::from_code(unsafe {
            fluxer_vips_read_delays_ms(image.as_ptr(), n_pages, &mut out_ptr, &mut out_len)
        });
        if status != NativeStatus::Ok {
            return Err(status);
        }
        let Some(ptr) = NonNull::new(out_ptr) else {
            return Err(NativeStatus::CodecFailure);
        };
        let expected_len =
            usize::try_from(n_pages).expect("positive native delay page count must fit usize");
        let len = match usize::try_from(out_len) {
            Ok(len) if len == expected_len => len,
            _ => {
                unsafe { fluxer_free_int_array(ptr.as_ptr()) };
                return Err(NativeStatus::CodecFailure);
            }
        };
        let byte_len = match len.checked_mul(std::mem::size_of::<c_int>()) {
            Some(byte_len) if byte_len <= isize::MAX as usize => byte_len,
            _ => {
                unsafe { fluxer_free_int_array(ptr.as_ptr()) };
                return Err(NativeStatus::CodecFailure);
            }
        };
        debug_assert!(byte_len > 0);
        Ok(Self { ptr, len })
    }

    pub fn as_slice(&self) -> &[c_int] {
        assert!(self.len > 0, "owned native delay array must not be empty");
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl Drop for VipsDelayArray {
    fn drop(&mut self) {
        unsafe { fluxer_free_int_array(self.ptr.as_ptr()) };
    }
}
