// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{media_process::native_runtime::ensure_vips_init, native};
use std::ffi::CString;

#[test]
fn blocked_libvips_loaders_cannot_decode_untrusted_input() {
    ensure_vips_init().expect("libvips initialises");
    let ppm = b"P6\n2 2\n255\n\x00\x00\x00\xff\xff\xff\x00\x00\x00\xff\xff\xff".to_vec();
    let options = CString::new("access=sequential").expect("static string has no NUL");
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(ppm.as_ptr().cast(), ppm.len(), options.as_ptr())
    };
    assert!(raw.is_null());
    unsafe { native::fluxer_vips_error_clear() };
}
