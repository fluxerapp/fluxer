// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{media_process::native_runtime::ensure_vips_init, native};
use libc::{c_int, c_void, size_t};
use std::{ffi::CString, ptr};

pub const ADVERSARIAL_TEXT_INPUTS: &[&str] = &[
    "",
    " ",
    "%",
    "%0",
    "%00",
    "%2F",
    "%2f",
    "%5C",
    "%FF",
    "%C3",
    "%C3%A9",
    "%%%%",
    "%20%2e%2e%2f",
    "a_deadbeef",
    "A_DEADBEEF",
    "deadbeef",
    "deadbee",
    "deadbeef0",
    "0123456789",
    "a_",
    "\"quoted\"",
    "photo name.png",
    "r\u{e9}sum\u{e9}.png",
    "token=",
    "token=abc&token=def",
    "\u{7f}",
    "~!@#$^&*()_+",
];

pub fn synthetic_png(width: u32, height: u32) -> Vec<u8> {
    synthetic_image(width, height, ".png[strip]")
}

pub fn synthetic_image(width: u32, height: u32, suffix: &str) -> Vec<u8> {
    ensure_vips_init().unwrap();
    let mut pixels = vec![0u8; width as usize * height as usize * 4];
    for y in 0..height {
        for x in 0..width {
            let i = (y as usize * width as usize + x as usize) * 4;
            pixels[i] = (x * 255 / width.max(1)) as u8;
            pixels[i + 1] = (y * 255 / height.max(1)) as u8;
            pixels[i + 2] = 120;
            pixels[i + 3] = 255;
        }
    }
    let image = unsafe {
        native::fluxer_vips_image_new_from_memory_copy(
            pixels.as_ptr().cast(),
            pixels.len(),
            width as c_int,
            height as c_int,
            4,
            native::fluxer_vips_format_uchar,
        )
    };
    let image = unsafe { native::VipsImageHandle::from_raw_owned(image) }.unwrap();
    let suffix = CString::new(suffix).unwrap();
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_vips_image_write_to_buffer(
            image.as_ptr(),
            suffix.as_ptr(),
            &mut out_ptr,
            &mut out_size,
        )
    };
    assert_eq!(0, rc);
    unsafe { native::buffer::NativeBuffer::from_vips_owned(out_ptr, out_size) }
        .unwrap()
        .try_to_vec()
        .unwrap()
}
