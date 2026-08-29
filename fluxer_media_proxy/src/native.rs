// SPDX-License-Identifier: AGPL-3.0-or-later

use libc::{c_char, c_double, c_int, c_longlong, c_void, size_t};
use std::{
    marker::{PhantomData, PhantomPinned},
    slice,
};

#[repr(C)]
pub struct VipsImage {
    _data: [u8; 0],
    _marker: PhantomData<(*mut u8, PhantomPinned)>,
}

#[repr(C)]
pub struct WebpAnimLimits {
    pub max_frames: c_int,
    pub max_duration_ms: c_int,
    pub deadline_unix_ms: c_longlong,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FluxerNsfwFrameOut {
    pub data: *mut c_void,
    pub len: size_t,
}

impl FluxerNsfwFrameOut {
    pub const fn empty() -> Self {
        Self {
            data: std::ptr::null_mut(),
            len: 0,
        }
    }
}

pub type VipsWriteCb = unsafe extern "C" fn(*mut c_void, *const c_void, size_t) -> c_int;

unsafe extern "C" {
    pub static fluxer_vips_format_uchar: c_int;

    pub fn fluxer_vips_init(argv0: *const c_char) -> c_int;
    pub fn fluxer_vips_error_clear();
    pub fn fluxer_vips_error_buffer() -> *const c_char;
    pub fn fluxer_vips_tune_for_server(per_pipeline_threads: c_int);
    pub fn fluxer_vips_probe_animated(
        buf: *const c_void,
        len: size_t,
        width: *mut c_int,
        height: *mut c_int,
        pages: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_image_new_from_buffer(
        buf: *const c_void,
        len: size_t,
        option_string: *const c_char,
    ) -> *mut VipsImage;
    pub fn fluxer_vips_image_new_from_memory_copy(
        data: *const c_void,
        size: size_t,
        width: c_int,
        height: c_int,
        bands: c_int,
        format: c_int,
    ) -> *mut VipsImage;
    pub fn fluxer_vips_image_write_to_buffer(
        image: *mut VipsImage,
        suffix: *const c_char,
        buf: *mut *mut c_void,
        size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_vips_image_write_to_callback(
        image: *mut VipsImage,
        suffix: *const c_char,
        cb: Option<VipsWriteCb>,
        user_data: *mut c_void,
    ) -> c_int;
    pub fn fluxer_vips_image_get_width(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_height(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_bands(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_int(
        image: *mut VipsImage,
        field: *const c_char,
        out: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_set_page_height(image: *mut VipsImage, page_height: c_int);
    pub fn fluxer_vips_read_delays_ms(
        image: *mut VipsImage,
        n_pages: c_int,
        out_delays: *mut *mut c_int,
        out_len: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_autorot(input: *mut VipsImage, out: *mut *mut VipsImage) -> c_int;
    pub fn fluxer_vips_extract_area(
        input: *mut VipsImage,
        out: *mut *mut VipsImage,
        left: c_int,
        top: c_int,
        width: c_int,
        height: c_int,
    ) -> c_int;
    pub fn fluxer_vips_resize(
        input: *mut VipsImage,
        out: *mut *mut VipsImage,
        scale: c_double,
    ) -> c_int;
    pub fn fluxer_vips_thumbnail_buffer_ex(
        buf: *const c_void,
        len: size_t,
        out: *mut *mut VipsImage,
        width: c_int,
        height: c_int,
        n: c_int,
        crop_mode: c_int,
    ) -> c_int;
    pub fn fluxer_vips_extract_rgba(
        input: *mut VipsImage,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_vips_unref(image: *mut VipsImage);
    pub fn fluxer_vips_free(mem: *mut c_void);
    pub fn fluxer_free_int_array(values: *mut c_int);

    pub fn fluxer_vips_image_is_hdr(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_tone_map_hdr_to_sdr(
        input: *mut VipsImage,
        out: *mut *mut VipsImage,
    ) -> c_int;
    pub fn fluxer_heif_decode_animated_ex2(
        buf: *const c_void,
        len: size_t,
        out: *mut *mut VipsImage,
        n_max_pages: c_int,
        max_total_pixels: size_t,
        was_hdr: *mut c_int,
        had_hdr_gain_map: *mut c_int,
    ) -> c_int;
    pub fn fluxer_heif_aux_type_is_hdr_gain_map_for_test(typ: *const c_char) -> c_int;
    pub fn fluxer_heif_has_tmap_item_for_test(buf: *const c_void, len: size_t) -> c_int;
    pub fn fluxer_avif_parse_track_delays_for_test(
        buf: *const c_void,
        len: size_t,
        out_delays_ms: *mut *mut c_int,
        out_n_samples: *mut c_int,
    ) -> c_int;
    pub fn fluxer_avif_free_delays(delays: *mut c_int);
    pub fn fluxer_vips_set_anim_metadata_for_test(
        image: *mut VipsImage,
        page_height: c_int,
        n_pages: c_int,
        delays_ms: *const c_int,
    );
    pub fn fluxer_hdr_to_sdr_test(
        r: u16,
        g: u16,
        b: u16,
        bit_depth: c_int,
        transfer: c_int,
        out_rgb: *mut u8,
    ) -> c_int;

    pub fn fluxer_ffmpeg_resize_gif(
        gif_data: *const c_void,
        gif_len: size_t,
        target_width: c_int,
        target_height: c_int,
        deadline_unix_ms: c_longlong,
        max_frames: c_longlong,
        max_total_pixels: c_longlong,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_ffmpeg_video_thumbnail(
        media_data: *const c_void,
        media_len: size_t,
        suffix: *const c_char,
        max_packets: c_int,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_av_probe(
        media_data: *const c_void,
        media_len: size_t,
        out_has_video: *mut c_int,
        out_has_audio: *mut c_int,
        out_duration_seconds: *mut c_double,
    ) -> c_int;
    pub fn fluxer_av_extract_frames_for_nsfw(
        media_data: *const c_void,
        media_len: size_t,
        timestamps_secs: *const c_double,
        n_timestamps: size_t,
        out_frames: *mut FluxerNsfwFrameOut,
    ) -> c_int;
    pub fn fluxer_nsfw_frames_free(frames: *mut FluxerNsfwFrameOut, n: size_t);
    pub fn fluxer_ffmpeg_decode_apng(
        apng_data: *const c_void,
        apng_len: size_t,
        out: *mut *mut VipsImage,
        max_frames: c_int,
        max_total_pixels: size_t,
    ) -> c_int;
    pub fn fluxer_webp_encode_animated(
        image: *mut VipsImage,
        quality: c_int,
        lossless: c_int,
        effort: c_int,
        alpha_q: c_int,
        smart_subsample: c_int,
        loop_count: c_int,
        full_canvas_frames: c_int,
        limits: *const WebpAnimLimits,
        scratch: *mut u8,
        scratch_cap: size_t,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_webp_free(mem: *mut c_void);
}

pub const THUMB_CROP_NONE: c_int = 0;
pub const THUMB_CROP_CENTRE: c_int = 1;

pub struct VipsImageHandle(*mut VipsImage);

impl VipsImageHandle {
    pub fn new(ptr: *mut VipsImage) -> Option<Self> {
        if ptr.is_null() { None } else { Some(Self(ptr)) }
    }

    pub fn as_ptr(&self) -> *mut VipsImage {
        self.0
    }
}

impl Drop for VipsImageHandle {
    fn drop(&mut self) {
        unsafe { fluxer_vips_unref(self.0) };
    }
}

pub struct VipsBuffer {
    ptr: *mut c_void,
    len: size_t,
}

impl VipsBuffer {
    pub fn new(ptr: *mut c_void, len: size_t) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Self { ptr, len })
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_ptr(&self) -> *const c_void {
        self.ptr.cast_const()
    }

    pub fn to_vec(&self) -> Vec<u8> {
        unsafe { slice::from_raw_parts(self.ptr.cast::<u8>(), self.len).to_vec() }
    }
}

impl Drop for VipsBuffer {
    fn drop(&mut self) {
        unsafe { fluxer_vips_free(self.ptr) };
    }
}

pub struct WebpBuffer {
    ptr: *mut c_void,
    len: size_t,
}

impl WebpBuffer {
    pub fn new(ptr: *mut c_void, len: size_t) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Self { ptr, len })
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn to_vec(&self) -> Vec<u8> {
        unsafe { slice::from_raw_parts(self.ptr.cast::<u8>(), self.len).to_vec() }
    }
}

impl Drop for WebpBuffer {
    fn drop(&mut self) {
        unsafe { fluxer_webp_free(self.ptr) };
    }
}
