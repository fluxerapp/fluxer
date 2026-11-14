// SPDX-License-Identifier: AGPL-3.0-or-later

use std::alloc::{Layout, alloc, alloc_zeroed, dealloc};
use std::ffi::{c_int, c_void};

const ALIGN: usize = 16;

unsafe fn shim_alloc(size: usize, zeroed: bool) -> *mut c_void {
    let Some(full) = size.checked_add(ALIGN) else {
        return std::ptr::null_mut();
    };
    let Ok(layout) = Layout::from_size_align(full, ALIGN) else {
        return std::ptr::null_mut();
    };
    unsafe {
        let ptr = if zeroed {
            alloc_zeroed(layout)
        } else {
            alloc(layout)
        };
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        ptr.cast::<usize>().write(full);
        ptr.add(ALIGN).cast()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxwebp_shim_malloc(size: usize) -> *mut c_void {
    unsafe { shim_alloc(size, false) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxwebp_shim_calloc(nmemb: usize, size: usize) -> *mut c_void {
    match nmemb.checked_mul(size) {
        Some(total) => unsafe { shim_alloc(total, true) },
        None => std::ptr::null_mut(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxwebp_shim_free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let base = ptr.cast::<u8>().sub(ALIGN);
        let full = base.cast::<usize>().read();
        dealloc(base, Layout::from_size_align_unchecked(full, ALIGN));
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxwebp_shim_qsort(
    base: *mut c_void,
    n: usize,
    size: usize,
    compar: unsafe extern "C" fn(*const c_void, *const c_void) -> c_int,
) {
    if n < 2 || size == 0 {
        return;
    }
    unsafe {
        let bytes = std::slice::from_raw_parts_mut(base.cast::<u8>(), n * size);
        let copy = bytes.to_vec();
        let mut idx: Vec<usize> = (0..n).collect();
        idx.sort_by(|&a, &b| {
            let r = compar(
                copy.as_ptr().add(a * size).cast(),
                copy.as_ptr().add(b * size).cast(),
            );
            r.cmp(&0)
        });
        for (dst, &src) in idx.iter().enumerate() {
            bytes[dst * size..dst * size + size]
                .copy_from_slice(&copy[src * size..src * size + size]);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_log(x: f64) -> f64 {
    x.ln()
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_log10(x: f64) -> f64 {
    x.log10()
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_expf(x: f32) -> f32 {
    x.exp()
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_logf(x: f32) -> f32 {
    x.ln()
}

#[unsafe(no_mangle)]
pub extern "C" fn fluxwebp_shim_round(x: f64) -> f64 {
    x.round()
}
