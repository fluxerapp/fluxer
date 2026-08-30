// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native (non-wasm32) C ABI: a pointer-width twin of the wasm ABI in
// `wasm.rs`, usable from 64-bit hosts via FFI. The emoji context TSV is
// per call because its `S` records carry byte offsets into `input`.

#![cfg(not(target_arch = "wasm32"))]

#[repr(C)]
pub struct FluxerMdBuffer {
    pub data: *mut u8,
    pub data_len: usize,
    pub error: *mut u8,
    pub error_len: usize,
}

/// Parses `input` with the given flags and emoji context TSV and writes an
/// owned result into `out`. Returns 0 on success and 1 on failure.
///
/// # Safety
/// - `input_ptr` must be valid for reads of `input_len` bytes (or
///   `input_len == 0`); the same holds for `tsv_ptr`/`tsv_len`.
/// - `out` must be valid for writes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxer_md_parse(
    input_ptr: *const u8,
    input_len: usize,
    flags: u32,
    tsv_ptr: *const u8,
    tsv_len: usize,
    out: *mut FluxerMdBuffer,
) -> u32 {
    let Ok(input) = std::str::from_utf8(unsafe { slice(input_ptr, input_len) }) else {
        return unsafe { write_error(out, "invalid markdown input") };
    };
    let Ok(emoji_context) = std::str::from_utf8(unsafe { slice(tsv_ptr, tsv_len) }) else {
        return unsafe { write_error(out, "invalid emoji context") };
    };
    let parsed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::parse_markdown_json(input, flags, emoji_context)
    }));
    match parsed {
        Ok(Ok(json)) => unsafe { write_data(out, json.into_bytes()) },
        Ok(Err(_)) => unsafe { write_error(out, "markdown parse failed") },
        Err(_) => unsafe { write_error(out, "markdown parser panicked") },
    }
}

/// Releases the payload of a buffer previously filled by
/// [`fluxer_md_parse`] and zeroes it. Null or zeroed buffers are no-ops.
///
/// # Safety
/// `out` must be valid for reads/writes and its pointers must originate
/// from `fluxer_md_parse`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn fluxer_md_buffer_free(out: *mut FluxerMdBuffer) {
    if out.is_null() {
        return;
    }
    let buffer = unsafe { &mut *out };
    unsafe {
        free_slice(buffer.data, buffer.data_len);
        free_slice(buffer.error, buffer.error_len);
    }
    buffer.data = std::ptr::null_mut();
    buffer.data_len = 0;
    buffer.error = std::ptr::null_mut();
    buffer.error_len = 0;
}

unsafe fn slice<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    if len == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

unsafe fn free_slice(ptr: *mut u8, len: usize) {
    if !ptr.is_null() {
        drop(unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, len)) });
    }
}

unsafe fn write_data(out: *mut FluxerMdBuffer, bytes: Vec<u8>) -> u32 {
    unsafe {
        *out = FluxerMdBuffer {
            data_len: bytes.len(),
            data: leak(bytes),
            error: std::ptr::null_mut(),
            error_len: 0,
        };
    }
    0
}

unsafe fn write_error(out: *mut FluxerMdBuffer, message: &str) -> u32 {
    let bytes = message.as_bytes().to_vec();
    unsafe {
        *out = FluxerMdBuffer {
            data: std::ptr::null_mut(),
            data_len: 0,
            error_len: bytes.len(),
            error: leak(bytes),
        };
    }
    1
}

fn leak(bytes: Vec<u8>) -> *mut u8 {
    let mut boxed = bytes.into_boxed_slice();
    let ptr = boxed.as_mut_ptr();
    std::mem::forget(boxed);
    ptr
}
