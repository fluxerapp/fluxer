// SPDX-License-Identifier: AGPL-3.0-or-later

use libc::{c_char, c_double, c_int, c_longlong, c_void, size_t};
use std::{
    marker::{PhantomData, PhantomPinned},
    ptr::NonNull,
};

pub mod buffer;
pub mod delay_array;
pub mod nsfw_frame_output;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(i32)]
pub enum NativeStatus {
    Ok = 0,
    Unsupported = 1,
    CodecFailure = -1,
    DeadlineExceeded = -2,
    WorkLimitExceeded = -3,
    InvalidDimensions = -4,
    OutputLimitExceeded = -5,
    AllocationFailed = -6,
}

impl NativeStatus {
    pub fn from_code(code: c_int) -> Self {
        match code {
            0 => Self::Ok,
            1 => Self::Unsupported,
            -1 => Self::CodecFailure,
            -2 => Self::DeadlineExceeded,
            -3 => Self::WorkLimitExceeded,
            -4 => Self::InvalidDimensions,
            -5 => Self::OutputLimitExceeded,
            -6 => Self::AllocationFailed,
            _ => panic!("native function returned undeclared status {code}"),
        }
    }
}

#[repr(C)]
pub struct VipsImage {
    _data: [u8; 0],
    _marker: PhantomData<(*mut u8, PhantomPinned)>,
}

#[repr(C)]
pub struct WebpAnimLimits {
    pub max_frames: c_int,
    pub max_duration_ms: c_int,
    pub deadline_monotonic_ms: c_longlong,
}

#[repr(C)]
pub struct FluxerAVMetadataOut {
    pub has_video: c_int,
    pub has_audio: c_int,
    pub frame_count: c_int,
    pub duration_seconds: c_double,
    pub display_width: c_int,
    pub display_height: c_int,
    pub rgba_width: c_int,
    pub rgba_height: c_int,
    pub rgba: *mut c_void,
    pub rgba_size: size_t,
}

impl FluxerAVMetadataOut {
    pub const fn empty() -> Self {
        Self {
            has_video: 0,
            has_audio: 0,
            frame_count: 0,
            duration_seconds: 0.0,
            display_width: 0,
            display_height: 0,
            rgba_width: 0,
            rgba_height: 0,
            rgba: std::ptr::null_mut(),
            rgba_size: 0,
        }
    }
}

#[repr(C)]
pub struct FluxerHEIFPrimaryStillDecodeFacts {
    pub hdr_tone_mapped: c_int,
    pub hdr_gain_map_detected: c_int,
}

impl FluxerHEIFPrimaryStillDecodeFacts {
    pub const fn empty() -> Self {
        Self {
            hdr_tone_mapped: 0,
            hdr_gain_map_detected: 0,
        }
    }
}

#[repr(C)]
pub struct FluxerNSFWFrameOut {
    data: *mut c_void,
    len: size_t,
}

impl FluxerNSFWFrameOut {
    const fn empty() -> Self {
        Self {
            data: std::ptr::null_mut(),
            len: 0,
        }
    }
}

pub type VipsWriteCb = unsafe extern "C" fn(*mut c_void, *const c_void, size_t) -> c_int;

unsafe extern "C" {
    pub static fluxer_vips_format_uchar: c_int;
    pub static fluxer_vips_format_ushort: c_int;
    pub static fluxer_vips_format_float: c_int;

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
    pub fn fluxer_apng_probe(
        buf: *const c_void,
        len: size_t,
        max_frames: c_int,
        max_total_pixels: size_t,
        width: *mut c_int,
        height: *mut c_int,
        frames: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_image_new_from_buffer(
        buf: *const c_void,
        len: size_t,
        option_string: *const c_char,
    ) -> *mut VipsImage;
    pub fn fluxer_vips_image_new_from_memory(
        data: *const c_void,
        size: size_t,
        width: c_int,
        height: c_int,
        bands: c_int,
        format: c_int,
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
        deadline_monotonic_ms: c_longlong,
        cb: Option<VipsWriteCb>,
        user_data: *mut c_void,
    ) -> c_int;
    pub fn fluxer_vips_image_get_width(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_height(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_orientation_swap(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_bands(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_get_format(image: *mut VipsImage) -> c_int;
    pub fn fluxer_vips_image_has_field(image: *mut VipsImage, field: *const c_char) -> c_int;
    pub fn fluxer_vips_image_get_int(
        image: *mut VipsImage,
        field: *const c_char,
        out: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_set_page_height(image: *mut VipsImage, page_height: c_int);
    pub fn fluxer_vips_set_animation_loop_count(image: *mut VipsImage, loop_count: c_int) -> c_int;
    fn fluxer_vips_read_delays_ms(
        image: *mut VipsImage,
        n_pages: c_int,
        out_delays: *mut *mut c_int,
        out_len: *mut c_int,
    ) -> c_int;
    pub fn fluxer_vips_autorot(
        input: *mut VipsImage,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
    ) -> c_int;
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
    pub fn fluxer_vips_join_animation_pages(
        source: *mut VipsImage,
        pages: *mut *mut VipsImage,
        n_pages: c_int,
        max_pages: c_int,
        max_total_pixels: size_t,
        out: *mut *mut VipsImage,
    ) -> c_int;
    pub fn fluxer_vips_thumbnail_buffer_ex(
        buf: *const c_void,
        len: size_t,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
        width: c_int,
        height: c_int,
        n: c_int,
        crop_mode: c_int,
        max_pages: c_int,
        max_total_pixels: size_t,
    ) -> c_int;
    pub fn fluxer_vips_image_to_rgba(input: *mut VipsImage, out: *mut *mut VipsImage) -> c_int;
    pub fn fluxer_vips_extract_rgba(
        input: *mut VipsImage,
        deadline_monotonic_ms: c_longlong,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    fn fluxer_vips_unref(image: *mut VipsImage);
    fn fluxer_vips_free(mem: *mut c_void);
    fn fluxer_av_free(mem: *mut c_void);
    fn fluxer_free_int_array(values: *mut c_int);

    pub fn fluxer_heif_validate(
        buf: *const c_void,
        len: size_t,
        deadline_monotonic_ms: c_longlong,
    ) -> c_int;
    pub fn fluxer_heif_decode_primary_still(
        buf: *const c_void,
        len: size_t,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
        max_pixels: size_t,
        max_dimension: c_int,
        facts: *mut FluxerHEIFPrimaryStillDecodeFacts,
    ) -> c_int;
    pub fn fluxer_ffmpeg_decode_heif_sequence(
        heif_data: *const c_void,
        heif_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
        max_frames: c_int,
        max_total_pixels: size_t,
        out_frame_count: *mut c_int,
    ) -> c_int;

    pub fn fluxer_ffmpeg_resize_gif(
        gif_data: *const c_void,
        gif_len: size_t,
        decoder_threads: c_int,
        target_width: c_int,
        target_height: c_int,
        deadline_monotonic_ms: c_longlong,
        max_source_frames: c_int,
        max_encode_frames: c_int,
        max_encode_duration_ms: c_int,
        max_total_pixels: size_t,
        max_output_size: size_t,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
        out_capacity: *mut size_t,
    ) -> c_int;
    pub fn fluxer_validate_gif_animation(
        gif_data: *const c_void,
        gif_len: size_t,
        max_frames: c_int,
        max_duration_ms: c_int,
        max_total_pixels: size_t,
    ) -> c_int;
    pub fn fluxer_ffmpeg_video_thumbnail_ex(
        media_data: *const c_void,
        media_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        suffix: *const c_char,
        max_packets: c_int,
        max_width: c_int,
        max_height: c_int,
        max_output_size: size_t,
        out_display_width: *mut c_int,
        out_display_height: *mut c_int,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
        out_capacity: *mut size_t,
    ) -> c_int;
    pub fn fluxer_av_metadata(
        media_data: *const c_void,
        media_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        max_packets: c_int,
        max_width: c_int,
        max_height: c_int,
        out: *mut FluxerAVMetadataOut,
    ) -> c_int;
    pub fn fluxer_ffmpeg_extract_apng_frames_for_nsfw(
        apng_data: *const c_void,
        apng_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        frame_indices: *const c_int,
        n_indices: size_t,
        max_frames: c_int,
        max_total_pixels: size_t,
        max_frame_output_size: size_t,
        out_frames: *mut FluxerNSFWFrameOut,
    ) -> c_int;
    pub fn fluxer_ffmpeg_extract_gif_frames_for_nsfw(
        gif_data: *const c_void,
        gif_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        frame_indices: *const c_int,
        n_indices: size_t,
        max_frames: c_int,
        max_total_pixels: size_t,
        max_frame_output_size: size_t,
        out_frames: *mut FluxerNSFWFrameOut,
    ) -> c_int;
    pub fn fluxer_av_extract_frames_for_nsfw(
        media_data: *const c_void,
        media_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        timestamps_secs: *const c_double,
        n_timestamps: size_t,
        max_frame_output_size: size_t,
        out_frames: *mut FluxerNSFWFrameOut,
    ) -> c_int;
    fn fluxer_nsfw_frames_free(frames: *mut FluxerNSFWFrameOut, n: size_t);
    pub fn fluxer_ffmpeg_decode_apng(
        apng_data: *const c_void,
        apng_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
        max_frames: c_int,
        max_total_pixels: size_t,
        require_complete: c_int,
        out_num_plays: *mut u32,
    ) -> c_int;
    pub fn fluxer_ffmpeg_decode_bmp(
        bmp_data: *const c_void,
        bmp_len: size_t,
        decoder_threads: c_int,
        deadline_monotonic_ms: c_longlong,
        out: *mut *mut VipsImage,
        max_total_pixels: size_t,
    ) -> c_int;
    pub fn fluxer_webp_encode_animated(
        image: *mut VipsImage,
        quality: c_int,
        lossless: c_int,
        effort: c_int,
        alpha_q: c_int,
        smart_subsample: c_int,
        thread_level: c_int,
        loop_count: c_int,
        full_canvas_frames: c_int,
        limits: *const WebpAnimLimits,
        max_output_size: size_t,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_webp_transform_animated(
        webp_data: *const c_void,
        webp_len: size_t,
        max_width: c_int,
        max_height: c_int,
        quality: c_int,
        lossless: c_int,
        effort: c_int,
        alpha_q: c_int,
        smart_subsample: c_int,
        thread_level: c_int,
        max_source_frames: c_int,
        max_total_pixels: size_t,
        limits: *const WebpAnimLimits,
        max_output_size: size_t,
        out_buf: *mut *mut c_void,
        out_size: *mut size_t,
    ) -> c_int;
    pub fn fluxer_webp_extract_frames_for_nsfw(
        webp_data: *const c_void,
        webp_len: size_t,
        thread_level: c_int,
        deadline_monotonic_ms: c_longlong,
        frame_indices: *const c_int,
        n_indices: size_t,
        max_frames: c_int,
        max_total_pixels: size_t,
        max_frame_output_size: size_t,
        out_frames: *mut FluxerNSFWFrameOut,
    ) -> c_int;
    fn fluxer_webp_free(mem: *mut c_void);
}

pub const THUMB_CROP_NONE: c_int = 0;
pub const THUMB_CROP_CENTRE: c_int = 1;

pub struct VipsImageHandle<'source> {
    ptr: NonNull<VipsImage>,
    source: PhantomData<&'source [u8]>,
}

impl VipsImageHandle<'static> {
    pub(crate) unsafe fn from_raw_owned(ptr: *mut VipsImage) -> Option<Self> {
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            source: PhantomData,
        })
    }
}

impl<'source> VipsImageHandle<'source> {
    pub(crate) unsafe fn from_raw_borrowing(
        ptr: *mut VipsImage,
        source: &'source [u8],
    ) -> Option<Self> {
        if source.is_empty() {
            assert!(ptr.is_null(), "empty libvips source returned an image");
            return None;
        }
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            source: PhantomData,
        })
    }

    pub(crate) unsafe fn adopt_derived_raw(&self, ptr: *mut VipsImage) -> Option<Self> {
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            source: PhantomData,
        })
    }

    pub fn as_ptr(&self) -> *mut VipsImage {
        self.ptr.as_ptr()
    }
}

impl Drop for VipsImageHandle<'_> {
    fn drop(&mut self) {
        unsafe { fluxer_vips_unref(self.ptr.as_ptr()) };
    }
}
