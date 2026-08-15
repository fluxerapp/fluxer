// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::{self, AssetExtension, Limits},
    metrics, mime, native, nsfw, thumbhash,
};
use base64::{Engine as _, engine::general_purpose};
use libc::{c_int, c_void, size_t};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::{ffi::CString, ptr, slice, sync::OnceLock, sync::atomic::Ordering};
use thiserror::Error;

const ANIMATED_ENCODE_FLUSH_HEADROOM_MS: i64 = 3_000;
const PNG_SIGNATURE: &[u8; 8] = b"\x89PNG\r\n\x1a\n";
const APNG_FRAME_PNG_SUFFIX: &str = ".png[strip,compression=9,filter=all]";
static PNG_CRC_TABLE: OnceLock<[u32; 256]> = OnceLock::new();

#[derive(Clone, Debug)]
pub struct ImageOptions {
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub format: AssetExtension,
    pub quality: String,
    pub animated: bool,
    pub effort_override: Option<u8>,
    pub cover_crop: bool,
    pub deadline_ms: Option<i64>,
    pub max_encode_frames: Option<u32>,
    pub max_encode_duration_ms: Option<u32>,
}

impl Default for ImageOptions {
    fn default() -> Self {
        Self {
            width: None,
            height: None,
            format: AssetExtension::Webp,
            quality: "high".to_owned(),
            animated: false,
            effort_override: None,
            cover_crop: false,
            deadline_ms: None,
            max_encode_frames: None,
            max_encode_duration_ms: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ProcessedMedia {
    pub bytes: Vec<u8>,
    pub content_type: &'static str,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ImageDimensions {
    pub width: u32,
    pub height: u32,
    pub pages: u32,
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum MediaError {
    #[error("native media init failed")]
    VipsInitFailed,
    #[error("media decode failed")]
    MediaDecodeFailed,
    #[error("media encode failed")]
    MediaEncodeFailed,
    #[error("media transform failed")]
    MediaTransformFailed,
    #[error("invalid image dimensions")]
    InvalidImageDimensions,
    #[error("unsupported media type")]
    UnsupportedMediaType,
    #[error("unsupported output format")]
    UnsupportedOutputFormat,
    #[error("stream too long")]
    StreamTooLong,
    #[error("request timed out")]
    RequestTimeout,
    #[error("nsfw scan unavailable")]
    NsfwScanUnavailable,
}

#[derive(Clone, Copy, Debug)]
struct AnimatedProbe {
    width: c_int,
    height: c_int,
    pages: c_int,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct AnimLimits {
    pub deadline_unix_ms: Option<i64>,
    pub max_frames: Option<u32>,
    pub max_duration_ms: Option<u32>,
}

static VIPS_INIT: OnceLock<Result<(), MediaError>> = OnceLock::new();

pub fn warmup_vips() -> Result<(), MediaError> {
    ensure_vips_init()
}

pub fn ensure_vips_init() -> Result<(), MediaError> {
    *VIPS_INIT.get_or_init(|| {
        let argv0 = CString::new("fluxer-media-proxy").expect("static string has no NUL");
        let rc = unsafe { native::fluxer_vips_init(argv0.as_ptr()) };
        if rc != 0 {
            unsafe { native::fluxer_vips_error_clear() };
            return Err(MediaError::VipsInitFailed);
        }
        unsafe { native::fluxer_vips_tune_for_server(1) };
        Ok(())
    })
}

fn clear_vips_error() {
    unsafe { native::fluxer_vips_error_clear() };
}

fn last_vips_error() -> String {
    let ptr = unsafe { native::fluxer_vips_error_buffer() };
    if ptr.is_null() {
        return String::new();
    }
    let cstr = unsafe { std::ffi::CStr::from_ptr(ptr) };
    cstr.to_string_lossy().trim().chars().take(512).collect()
}

fn vips_buffer_to_vec(buffer: &native::VipsBuffer) -> Result<Vec<u8>, MediaError> {
    if buffer.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    Ok(buffer.to_vec())
}

fn webp_buffer_to_vec(buffer: &native::WebpBuffer) -> Result<Vec<u8>, MediaError> {
    if buffer.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    Ok(buffer.to_vec())
}

fn quality_number(quality: &str) -> u8 {
    match quality {
        "low" => 65,
        "lossless" => 100,
        _ => 85,
    }
}

fn is_auto_quality(quality: &str) -> bool {
    quality == "auto"
}

fn is_lossless(quality: &str) -> bool {
    quality == "lossless"
}

fn effort_for(quality: &str, animated: bool) -> u8 {
    if animated || quality == "low" { 2 } else { 4 }
}

fn output_suffix(
    format: AssetExtension,
    quality: &str,
    page_height: Option<c_int>,
    effort_override: Option<u8>,
) -> Result<CString, MediaError> {
    if matches!(
        format,
        AssetExtension::Avif
            | AssetExtension::Heic
            | AssetExtension::Heif
            | AssetExtension::Jxl
            | AssetExtension::Svg
    ) {
        return Err(MediaError::UnsupportedOutputFormat);
    }
    let q = quality_number(quality);
    let animated = page_height.is_some();
    let lossless = if is_lossless(quality) {
        "true"
    } else {
        "false"
    };
    let effort = effort_override
        .map(|v| v.min(9))
        .unwrap_or_else(|| effort_for(quality, animated));
    let suffix = match format {
        AssetExtension::Jpeg => format!(".jpg[Q={q},strip,interlace=true,optimize_coding=true]"),
        AssetExtension::Webp => match page_height {
            Some(ph) => format!(
                ".webp[Q={q},lossless={lossless},strip,effort={effort},smart_subsample=true,alpha_q=90,page-height={ph}]"
            ),
            None => format!(
                ".webp[Q={q},lossless={lossless},strip,effort={effort},smart_subsample=true,alpha_q=90]"
            ),
        },
        AssetExtension::Png | AssetExtension::Apng => match page_height {
            Some(ph) if format == AssetExtension::Apng => {
                format!(".png[strip,compression=9,filter=all,page-height={ph}]")
            }
            _ => ".png[strip,compression=9,filter=all]".to_owned(),
        },
        AssetExtension::Gif => match page_height {
            Some(ph) => {
                format!(".gif[strip,dither=1.0,effort=7,interframe_maxerror=8.0,page-height={ph}]")
            }
            None => ".gif[strip,dither=1.0,effort=7]".to_owned(),
        },
        AssetExtension::Avif
        | AssetExtension::Heic
        | AssetExtension::Heif
        | AssetExtension::Jxl
        | AssetExtension::Svg => {
            unreachable!("guarded above")
        }
    };
    CString::new(suffix).map_err(|_| MediaError::MediaEncodeFailed)
}

fn validate_dimensions_u32(width: u32, height: u32) -> Result<(), MediaError> {
    let max_dim = Limits::image_dimension();
    if width == 0 || height == 0 || width > max_dim || height > max_dim {
        return Err(MediaError::InvalidImageDimensions);
    }
    let pixels = width as usize * height as usize;
    if pixels > Limits::image_pixels() {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(())
}

fn validate_dimensions(width: c_int, height: c_int) -> Result<(), MediaError> {
    if width <= 0 || height <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    validate_dimensions_u32(width as u32, height as u32)
}

fn validate_vips_image(image: &native::VipsImageHandle) -> Result<(), MediaError> {
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    let mut page_height = 0;
    let has_page_height = unsafe {
        let field = CString::new("page-height").expect("static string has no NUL");
        native::fluxer_vips_image_get_int(image.as_ptr(), field.as_ptr(), &mut page_height) == 0
    };
    if has_page_height && page_height > 0 {
        validate_dimensions(width, page_height)?;
        if height <= 0 || height % page_height != 0 {
            return Err(MediaError::InvalidImageDimensions);
        }
        let frames = height / page_height;
        if frames as u32 > Limits::animated_frames() {
            return Err(MediaError::InvalidImageDimensions);
        }
        let total_budget = Limits::animated_total_pixels();
        let per_frame = width as usize * page_height as usize;
        if per_frame > total_budget || per_frame.saturating_mul(frames as usize) > total_budget {
            return Err(MediaError::InvalidImageDimensions);
        }
    } else {
        validate_dimensions(width, height)?;
    }
    let bands = unsafe { native::fluxer_vips_image_get_bands(image.as_ptr()) };
    if bands <= 0 || bands > 16 {
        return Err(MediaError::InvalidImageDimensions);
    }
    Ok(())
}

fn resize_loaded_image(
    image: native::VipsImageHandle,
    options: &ImageOptions,
) -> Result<native::VipsImageHandle, MediaError> {
    let (Some(target_width), Some(target_height)) = (options.width, options.height) else {
        return resize_loaded_image_fit_inside(image, options);
    };
    if options.animated || !options.cover_crop {
        return resize_loaded_image_fit_inside(image, options);
    }

    let source_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let source_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    validate_dimensions(source_width, source_height)?;

    let scale_w = target_width as f64 / source_width as f64;
    let scale_h = target_height as f64 / source_height as f64;
    let scale = scale_w.max(scale_h).min(1.0);
    let current = resize_loaded_image_by_scale(image, scale)?;
    let scaled_width = unsafe { native::fluxer_vips_image_get_width(current.as_ptr()) };
    let scaled_height = unsafe { native::fluxer_vips_image_get_height(current.as_ptr()) };
    validate_dimensions(scaled_width, scaled_height)?;

    let final_width = scaled_width.min(target_width as c_int);
    let final_height = scaled_height.min(target_height as c_int);
    if final_width == scaled_width && final_height == scaled_height {
        return Ok(current);
    }

    let had_page_height = page_height(&current).is_some();
    let left = (scaled_width - final_width) / 2;
    let top = (scaled_height - final_height) / 2;
    let mut cropped_raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_extract_area(
            current.as_ptr(),
            &mut cropped_raw,
            left,
            top,
            final_width,
            final_height,
        )
    };
    if rc != 0 || cropped_raw.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    drop(current);
    let cropped =
        native::VipsImageHandle::new(cropped_raw).ok_or(MediaError::MediaTransformFailed)?;
    if had_page_height {
        unsafe { native::fluxer_vips_set_page_height(cropped.as_ptr(), final_height) };
    }
    validate_vips_image(&cropped)?;
    Ok(cropped)
}

fn resize_loaded_image_fit_inside(
    image: native::VipsImageHandle,
    options: &ImageOptions,
) -> Result<native::VipsImageHandle, MediaError> {
    if options.width.is_none() && options.height.is_none() {
        return Ok(image);
    }
    let source_width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    let source_height = if options.animated {
        page_height(&image).unwrap_or(total_height)
    } else {
        total_height
    };
    validate_dimensions(source_width, source_height)?;

    let scale_w = options
        .width
        .map(|width| width as f64 / source_width as f64)
        .unwrap_or(f64::INFINITY);
    let scale_h = options
        .height
        .map(|height| height as f64 / source_height as f64)
        .unwrap_or(f64::INFINITY);
    let scale = scale_w.min(scale_h).min(1.0);
    resize_loaded_image_by_scale(image, scale)
}

fn resize_loaded_image_by_scale(
    image: native::VipsImageHandle,
    scale: f64,
) -> Result<native::VipsImageHandle, MediaError> {
    if scale >= 0.999 {
        return Ok(image);
    }
    let old_page_count = page_height(&image).and_then(|old_page_height| {
        let old_total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
        (old_page_height > 0 && old_total_height > 0 && old_total_height % old_page_height == 0)
            .then_some(old_total_height / old_page_height)
    });
    let mut resized_raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_resize(image.as_ptr(), &mut resized_raw, scale) };
    if rc != 0 || resized_raw.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    drop(image);
    let resized =
        native::VipsImageHandle::new(resized_raw).ok_or(MediaError::MediaTransformFailed)?;
    if let Some(page_count) = old_page_count {
        let new_total_height = unsafe { native::fluxer_vips_image_get_height(resized.as_ptr()) };
        if page_count > 0 && new_total_height > 0 && new_total_height % page_count == 0 {
            unsafe {
                native::fluxer_vips_set_page_height(resized.as_ptr(), new_total_height / page_count)
            };
        }
    }
    validate_vips_image(&resized)?;
    Ok(resized)
}

fn probe_animated(input: &[u8]) -> Result<Option<AnimatedProbe>, MediaError> {
    let mut width = 0;
    let mut height = 0;
    let mut pages = 0;
    let rc = unsafe {
        native::fluxer_vips_probe_animated(
            input.as_ptr().cast(),
            input.len(),
            &mut width,
            &mut height,
            &mut pages,
        )
    };
    if rc != 0 {
        clear_vips_error();
        return Ok(None);
    }
    if pages < 0 || width <= 0 || height <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    validate_dimensions(width, height)?;
    let page_count = pages as usize;
    if page_count > Limits::animated_frames() as usize {
        return Err(MediaError::InvalidImageDimensions);
    }
    if page_count > 1 {
        let per_frame = width as usize * height as usize;
        let max_total = Limits::animated_total_pixels();
        if per_frame > max_total || per_frame.saturating_mul(page_count) > max_total {
            return Err(MediaError::InvalidImageDimensions);
        }
    }
    Ok(Some(AnimatedProbe {
        width,
        height,
        pages,
    }))
}

pub fn probe_image_dims(input: &[u8]) -> Result<ImageDimensions, MediaError> {
    ensure_vips_init()?;
    if let Some(probe) = probe_animated(input)? {
        return Ok(ImageDimensions {
            width: probe.width as u32,
            height: probe.height as u32,
            pages: probe.pages.max(1) as u32,
        });
    }
    let image = load_image(input, "access=sequential")?;
    validate_vips_image(&image)?;
    Ok(ImageDimensions {
        width: unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) as u32 },
        height: unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) as u32 },
        pages: page_count(&image).unwrap_or(1) as u32,
    })
}

fn load_image(input: &[u8], options: &str) -> Result<native::VipsImageHandle, MediaError> {
    let options = CString::new(options).map_err(|_| MediaError::MediaDecodeFailed)?;
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(
            input.as_ptr().cast(),
            input.len(),
            options.as_ptr(),
        )
    };
    native::VipsImageHandle::new(raw).ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaDecodeFailed
    })
}

fn page_height(image: &native::VipsImageHandle) -> Option<c_int> {
    let mut page_height = 0;
    let field = CString::new("page-height").expect("static string has no NUL");
    let rc = unsafe {
        native::fluxer_vips_image_get_int(image.as_ptr(), field.as_ptr(), &mut page_height)
    };
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    (rc == 0 && page_height > 0 && total_height > 0 && total_height % page_height == 0)
        .then_some(page_height)
}

fn page_count(image: &native::VipsImageHandle) -> Option<c_int> {
    let ph = page_height(image)?;
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    Some(height / ph)
}

fn animated_probe_from_image(image: &native::VipsImageHandle) -> Option<AnimatedProbe> {
    let page_height = page_height(image)?;
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let pages = page_count(image)?;
    (width > 0 && page_height > 0 && pages > 0).then_some(AnimatedProbe {
        width,
        height: page_height,
        pages,
    })
}

fn anim_limits_from_options(options: &ImageOptions) -> AnimLimits {
    AnimLimits {
        deadline_unix_ms: options.deadline_ms.map(|deadline| {
            if deadline > ANIMATED_ENCODE_FLUSH_HEADROOM_MS {
                deadline - ANIMATED_ENCODE_FLUSH_HEADROOM_MS
            } else {
                deadline
            }
        }),
        max_frames: options.max_encode_frames,
        max_duration_ms: options.max_encode_duration_ms,
    }
}

struct StreamingWriteCtx {
    out: Vec<u8>,
    cap: usize,
    failed: bool,
}

unsafe extern "C" fn streaming_write_cb(
    user_data: *mut c_void,
    bytes: *const c_void,
    len: size_t,
) -> c_int {
    if user_data.is_null() || bytes.is_null() {
        return -1;
    }
    let ctx = unsafe { &mut *(user_data as *mut StreamingWriteCtx) };
    if ctx.out.len().saturating_add(len) > ctx.cap {
        ctx.failed = true;
        return -1;
    }
    let chunk = unsafe { slice::from_raw_parts(bytes.cast::<u8>(), len) };
    ctx.out.extend_from_slice(chunk);
    0
}

#[derive(Clone, Copy, Debug)]
struct PngChunk<'a> {
    kind: [u8; 4],
    data: &'a [u8],
}

#[derive(Debug)]
struct ApngFrame {
    idat_chunks: Vec<Vec<u8>>,
    delay_ms: u32,
}

fn parse_png_chunks(bytes: &[u8]) -> Result<Vec<PngChunk<'_>>, MediaError> {
    if bytes.len() < PNG_SIGNATURE.len() || &bytes[..PNG_SIGNATURE.len()] != PNG_SIGNATURE {
        return Err(MediaError::MediaEncodeFailed);
    }
    let mut chunks = Vec::new();
    let mut offset = PNG_SIGNATURE.len();
    while offset + 12 <= bytes.len() {
        let len = u32::from_be_bytes([
            bytes[offset],
            bytes[offset + 1],
            bytes[offset + 2],
            bytes[offset + 3],
        ]) as usize;
        let kind: [u8; 4] = bytes[offset + 4..offset + 8]
            .try_into()
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let data_start = offset + 8;
        let data_end = data_start
            .checked_add(len)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let crc_end = data_end
            .checked_add(4)
            .ok_or(MediaError::MediaEncodeFailed)?;
        if crc_end > bytes.len() {
            return Err(MediaError::MediaEncodeFailed);
        }
        chunks.push(PngChunk {
            kind,
            data: &bytes[data_start..data_end],
        });
        offset = crc_end;
        if kind == *b"IEND" {
            return Ok(chunks);
        }
    }
    Err(MediaError::MediaEncodeFailed)
}

fn png_ihdr_dimensions(ihdr: &[u8]) -> Option<(u32, u32)> {
    if ihdr.len() != 13 {
        return None;
    }
    Some((
        u32::from_be_bytes(ihdr[0..4].try_into().ok()?),
        u32::from_be_bytes(ihdr[4..8].try_into().ok()?),
    ))
}

fn png_crc32(kind: &[u8; 4], payload: &[u8]) -> u32 {
    let table = PNG_CRC_TABLE.get_or_init(|| {
        let mut table = [0u32; 256];
        for (slot, value) in table.iter_mut().zip(0u32..=255) {
            let mut crc = value;
            for _ in 0..8 {
                let mask = 0u32.wrapping_sub(crc & 1);
                crc = (crc >> 1) ^ (0xedb8_8320u32 & mask);
            }
            *slot = crc;
        }
        table
    });
    let mut crc = 0xffff_ffffu32;
    for byte in kind.iter().copied().chain(payload.iter().copied()) {
        crc = table[((crc ^ byte as u32) & 0xff) as usize] ^ (crc >> 8);
    }
    crc ^ 0xffff_ffffu32
}

fn append_be_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_be_bytes());
}

fn append_be_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_be_bytes());
}

fn append_png_chunk(out: &mut Vec<u8>, kind: &[u8; 4], payload: &[u8]) -> Result<(), MediaError> {
    let payload_len = u32::try_from(payload.len()).map_err(|_| MediaError::StreamTooLong)?;
    if out.len().saturating_add(12).saturating_add(payload.len()) > constants::MAX_MEDIA_PROXY_BYTES
    {
        return Err(MediaError::StreamTooLong);
    }
    append_be_u32(out, payload_len);
    out.extend_from_slice(kind);
    out.extend_from_slice(payload);
    append_be_u32(out, png_crc32(kind, payload));
    Ok(())
}

fn gcd_u32(mut a: u32, mut b: u32) -> u32 {
    while b != 0 {
        let rem = a % b;
        a = b;
        b = rem;
    }
    a.max(1)
}

fn apng_delay_fraction(delay_ms: u32) -> (u16, u16) {
    let delay_ms = delay_ms.clamp(20, u16::MAX as u32 * 1_000);
    let divisor = gcd_u32(delay_ms, 1_000);
    let num = delay_ms / divisor;
    let den = 1_000 / divisor;
    if num <= u16::MAX as u32 && den <= u16::MAX as u32 {
        return (num as u16, den as u16);
    }
    (delay_ms.div_ceil(1_000).min(u16::MAX as u32) as u16, 1)
}

fn vips_delays_ms(image: &native::VipsImageHandle, n_pages: usize) -> Vec<u32> {
    let mut out_ptr: *mut c_int = ptr::null_mut();
    let mut out_len: c_int = 0;
    let rc = unsafe {
        native::fluxer_vips_read_delays_ms(
            image.as_ptr(),
            n_pages.min(c_int::MAX as usize) as c_int,
            &mut out_ptr,
            &mut out_len,
        )
    };
    if rc != 0 || out_ptr.is_null() || out_len <= 0 {
        return vec![100; n_pages];
    }
    let copy_len = (out_len as usize).min(n_pages);
    let mut delays = vec![100; n_pages];
    let src = unsafe { slice::from_raw_parts(out_ptr, copy_len) };
    for (dst, src) in delays.iter_mut().zip(src.iter().copied()) {
        *dst = if src >= 20 { src as u32 } else { 100 };
    }
    unsafe { native::fluxer_free_int_array(out_ptr) };
    delays
}

fn encode_png_strip(
    image: &native::VipsImageHandle,
    frame_index: usize,
    width: c_int,
    page_height: c_int,
) -> Result<Vec<u8>, MediaError> {
    let top = c_int::try_from(frame_index)
        .ok()
        .and_then(|index| index.checked_mul(page_height))
        .ok_or(MediaError::InvalidImageDimensions)?;
    let mut strip_raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_extract_area(image.as_ptr(), &mut strip_raw, 0, top, width, page_height)
    };
    if rc != 0 || strip_raw.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let strip = native::VipsImageHandle::new(strip_raw).ok_or(MediaError::MediaTransformFailed)?;
    let expected_rgba_size = (width as usize)
        .checked_mul(page_height as usize)
        .and_then(|pixels| pixels.checked_mul(4))
        .ok_or(MediaError::InvalidImageDimensions)?;
    let mut rgba_ptr: *mut c_void = ptr::null_mut();
    let mut rgba_size: size_t = 0;
    let rc =
        unsafe { native::fluxer_vips_extract_rgba(strip.as_ptr(), &mut rgba_ptr, &mut rgba_size) };
    if rc != 0 || rgba_ptr.is_null() || rgba_size != expected_rgba_size {
        if !rgba_ptr.is_null() {
            unsafe { native::fluxer_vips_free(rgba_ptr) };
        }
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let rgba_buffer =
        native::VipsBuffer::new(rgba_ptr, rgba_size).ok_or(MediaError::MediaTransformFailed)?;
    let rgba_image_raw = unsafe {
        native::fluxer_vips_image_new_from_memory_copy(
            rgba_buffer.as_ptr(),
            rgba_buffer.len(),
            width,
            page_height,
            4,
            native::fluxer_vips_format_uchar,
        )
    };
    let rgba_image =
        native::VipsImageHandle::new(rgba_image_raw).ok_or(MediaError::MediaTransformFailed)?;
    let suffix = CString::new(APNG_FRAME_PNG_SUFFIX).expect("static string has no NUL");
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_vips_image_write_to_buffer(
            rgba_image.as_ptr(),
            suffix.as_ptr(),
            &mut out_ptr,
            &mut out_size,
        )
    };
    if rc != 0 || out_ptr.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaEncodeFailed);
    }
    let buffer = native::VipsBuffer::new(out_ptr, out_size).ok_or(MediaError::MediaEncodeFailed)?;
    vips_buffer_to_vec(&buffer)
}

fn png_frame_parts(
    png: &[u8],
    expected_width: u32,
    expected_height: u32,
) -> Result<(Vec<u8>, Vec<Vec<u8>>), MediaError> {
    let chunks = parse_png_chunks(png)?;
    let ihdr = chunks
        .iter()
        .find(|chunk| chunk.kind == *b"IHDR")
        .ok_or(MediaError::MediaEncodeFailed)?
        .data
        .to_vec();
    if png_ihdr_dimensions(&ihdr) != Some((expected_width, expected_height)) {
        return Err(MediaError::MediaEncodeFailed);
    }
    let idat_chunks = chunks
        .iter()
        .filter(|chunk| chunk.kind == *b"IDAT")
        .map(|chunk| chunk.data.to_vec())
        .collect::<Vec<_>>();
    if idat_chunks.is_empty() {
        return Err(MediaError::MediaEncodeFailed);
    }
    Ok((ihdr, idat_chunks))
}

fn encode_animated_apng(
    image: &native::VipsImageHandle,
    page_height: c_int,
    limits: AnimLimits,
) -> Result<Vec<u8>, MediaError> {
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    validate_dimensions(width, page_height)?;
    if total_height <= 0 || total_height % page_height != 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let n_pages = usize::try_from(total_height / page_height)
        .map_err(|_| MediaError::InvalidImageDimensions)?;
    if n_pages == 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let delays = vips_delays_ms(image, n_pages);
    let max_frames = limits.max_frames.unwrap_or(0) as usize;
    let max_duration_ms = limits.max_duration_ms.unwrap_or(0);
    let deadline_unix_ms = limits.deadline_unix_ms.unwrap_or(0);
    let expected_width = width as u32;
    let expected_height = page_height as u32;

    let mut ihdr: Option<Vec<u8>> = None;
    let mut frames = Vec::new();
    let mut timestamp_ms = 0u32;
    for frame_index in 0..n_pages {
        if max_frames > 0 && frames.len() >= max_frames {
            break;
        }
        if max_duration_ms > 0 && timestamp_ms >= max_duration_ms {
            break;
        }
        if deadline_unix_ms > 0 && metrics::now_ms() >= deadline_unix_ms {
            break;
        }

        let frame_png = match encode_png_strip(image, frame_index, width, page_height) {
            Ok(bytes) => bytes,
            Err(err) if frames.is_empty() => return Err(err),
            Err(_) => break,
        };
        let (frame_ihdr, idat_chunks) =
            png_frame_parts(&frame_png, expected_width, expected_height)?;
        if let Some(existing_ihdr) = ihdr.as_ref() {
            if existing_ihdr.as_slice() != frame_ihdr.as_slice() {
                return Err(MediaError::MediaEncodeFailed);
            }
        } else {
            ihdr = Some(frame_ihdr);
        }
        let delay_ms = delays.get(frame_index).copied().unwrap_or(100);
        frames.push(ApngFrame {
            idat_chunks,
            delay_ms,
        });
        timestamp_ms = timestamp_ms.saturating_add(delay_ms);
    }
    if frames.is_empty() {
        return Err(MediaError::MediaEncodeFailed);
    }

    let ihdr = ihdr.ok_or(MediaError::MediaEncodeFailed)?;
    let estimated_len = frames
        .iter()
        .flat_map(|frame| frame.idat_chunks.iter())
        .map(Vec::len)
        .try_fold(256usize, usize::checked_add)
        .unwrap_or(constants::MAX_MEDIA_PROXY_BYTES);
    let mut out = Vec::with_capacity(estimated_len.min(constants::MAX_MEDIA_PROXY_BYTES));
    out.extend_from_slice(PNG_SIGNATURE);
    append_png_chunk(&mut out, b"IHDR", &ihdr)?;
    let mut actl = Vec::with_capacity(8);
    append_be_u32(&mut actl, frames.len().min(u32::MAX as usize) as u32);
    append_be_u32(&mut actl, 0);
    append_png_chunk(&mut out, b"acTL", &actl)?;

    let mut sequence_number = 0u32;
    for (frame_index, frame) in frames.iter().enumerate() {
        let (delay_num, delay_den) = apng_delay_fraction(frame.delay_ms);
        let mut fctl = Vec::with_capacity(26);
        append_be_u32(&mut fctl, sequence_number);
        sequence_number = sequence_number.wrapping_add(1);
        append_be_u32(&mut fctl, expected_width);
        append_be_u32(&mut fctl, expected_height);
        append_be_u32(&mut fctl, 0);
        append_be_u32(&mut fctl, 0);
        append_be_u16(&mut fctl, delay_num);
        append_be_u16(&mut fctl, delay_den);
        fctl.push(0);
        fctl.push(0);
        append_png_chunk(&mut out, b"fcTL", &fctl)?;

        for idat in &frame.idat_chunks {
            if frame_index == 0 {
                append_png_chunk(&mut out, b"IDAT", idat)?;
            } else {
                let mut fdat = Vec::with_capacity(idat.len().saturating_add(4));
                append_be_u32(&mut fdat, sequence_number);
                sequence_number = sequence_number.wrapping_add(1);
                fdat.extend_from_slice(idat);
                append_png_chunk(&mut out, b"fdAT", &fdat)?;
            }
        }
    }
    append_png_chunk(&mut out, b"IEND", &[])?;
    Ok(out)
}

fn encode_vips_image(
    image: &native::VipsImageHandle,
    format: AssetExtension,
    quality: &str,
    page_height: Option<c_int>,
    effort_override: Option<u8>,
    limits: AnimLimits,
    full_canvas_animation: bool,
) -> Result<Vec<u8>, MediaError> {
    if format == AssetExtension::Webp && page_height.is_some_and(|ph| ph > 0) {
        return encode_animated_webp(
            image,
            quality,
            effort_override,
            limits,
            full_canvas_animation,
        );
    }
    if format == AssetExtension::Apng
        && let Some(ph) = page_height
        && ph > 0
    {
        return encode_animated_apng(image, ph, limits);
    }
    let suffix = output_suffix(format, quality, page_height, effort_override)?;
    let mut ctx = StreamingWriteCtx {
        out: Vec::with_capacity(16 * 1024),
        cap: constants::MAX_MEDIA_PROXY_BYTES,
        failed: false,
    };
    let rc = unsafe {
        native::fluxer_vips_image_write_to_callback(
            image.as_ptr(),
            suffix.as_ptr(),
            Some(streaming_write_cb),
            (&mut ctx as *mut StreamingWriteCtx).cast(),
        )
    };
    if rc != 0 {
        clear_vips_error();
        return if ctx.failed {
            Err(MediaError::StreamTooLong)
        } else {
            Err(MediaError::MediaEncodeFailed)
        };
    }
    Ok(ctx.out)
}

fn encode_animated_webp(
    image: &native::VipsImageHandle,
    quality: &str,
    effort_override: Option<u8>,
    limits: AnimLimits,
    full_canvas_frames: bool,
) -> Result<Vec<u8>, MediaError> {
    let q = quality_number(quality);
    let effort = effort_override
        .map(|v| v.min(9))
        .unwrap_or_else(|| effort_for(quality, true));
    let c_limits = native::WebpAnimLimits {
        max_frames: limits
            .max_frames
            .map(|v| v.min(c_int::MAX as u32) as c_int)
            .unwrap_or(0),
        max_duration_ms: limits
            .max_duration_ms
            .map(|v| v.min(c_int::MAX as u32) as c_int)
            .unwrap_or(0),
        deadline_unix_ms: limits.deadline_unix_ms.unwrap_or(0),
    };
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_webp_encode_animated(
            image.as_ptr(),
            q as c_int,
            if is_lossless(quality) { 1 } else { 0 },
            effort as c_int,
            90,
            1,
            0,
            if full_canvas_frames { 1 } else { 0 },
            &c_limits,
            ptr::null_mut(),
            0,
            &mut out_ptr,
            &mut out_size,
        )
    };
    if rc != 0 || out_ptr.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaEncodeFailed);
    }
    let out = native::WebpBuffer::new(out_ptr, out_size).ok_or(MediaError::MediaEncodeFailed)?;
    webp_buffer_to_vec(&out)
}

fn source_is_palette_animation(input: &[u8]) -> bool {
    matches!(mime::sniff(input).mime, "image/gif" | "image/apng")
}

const AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_BYTES: usize = 4 * 1024 * 1024;
const AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_PIXELS: usize = 16 * 1024 * 1024;

fn animated_probe_pixels(probe: AnimatedProbe) -> Option<usize> {
    if probe.width <= 0 || probe.height <= 0 || probe.pages <= 0 {
        return None;
    }
    (probe.width as usize)
        .checked_mul(probe.height as usize)?
        .checked_mul(probe.pages as usize)
}

fn is_auto_palette_animated_webp(
    format: AssetExtension,
    animated: bool,
    input: &[u8],
    quality: &str,
) -> bool {
    is_auto_quality(quality)
        && animated
        && format == AssetExtension::Webp
        && source_is_palette_animation(input)
}

fn should_auto_lossless_animated(
    format: AssetExtension,
    animated: bool,
    input: &[u8],
    quality: &str,
    probe: Option<AnimatedProbe>,
) -> bool {
    if !is_auto_palette_animated_webp(format, animated, input, quality) {
        return false;
    }
    if input.len() > AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_BYTES {
        return false;
    }
    if let Some(probe) = probe
        && animated_probe_pixels(probe)
            .is_none_or(|pixels| pixels > AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_PIXELS)
    {
        return false;
    }
    true
}

fn effective_quality(
    format: AssetExtension,
    animated: bool,
    input: &[u8],
    quality: &str,
    probe: Option<AnimatedProbe>,
) -> String {
    if should_auto_lossless_animated(format, animated, input, quality, probe) {
        "lossless".to_owned()
    } else if is_auto_quality(quality) {
        "high".to_owned()
    } else {
        quality.to_owned()
    }
}

fn effective_effort_override(
    format: AssetExtension,
    animated: bool,
    input: &[u8],
    quality: &str,
    probe: Option<AnimatedProbe>,
    override_value: Option<u8>,
) -> Option<u8> {
    if override_value.is_some() {
        return override_value;
    }
    if !is_auto_palette_animated_webp(format, animated, input, quality) {
        return None;
    }
    if should_auto_lossless_animated(format, animated, input, quality, probe) {
        None
    } else {
        Some(0)
    }
}

fn output_is_sdr(format: AssetExtension) -> bool {
    matches!(
        format,
        AssetExtension::Jpeg
            | AssetExtension::Webp
            | AssetExtension::Png
            | AssetExtension::Gif
            | AssetExtension::Apng
    )
}

fn source_maybe_hdr(input: &[u8]) -> bool {
    matches!(
        mime::sniff(input).mime,
        "image/avif" | "image/heic" | "image/heif"
    )
}

fn tone_map_if_hdr(image: native::VipsImageHandle) -> Result<native::VipsImageHandle, MediaError> {
    let is_hdr = unsafe { native::fluxer_vips_image_is_hdr(image.as_ptr()) };
    if is_hdr <= 0 {
        return Ok(image);
    }
    let mut mapped = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_tone_map_hdr_to_sdr(image.as_ptr(), &mut mapped) };
    if rc != 0 || mapped.is_null() {
        clear_vips_error();
        return Ok(image);
    }
    metrics::GLOBAL
        .hdr_tone_map_count
        .fetch_add(1, Ordering::Relaxed);
    drop(image);
    native::VipsImageHandle::new(mapped).ok_or(MediaError::MediaTransformFailed)
}

fn try_decode_heif(
    input: &[u8],
    animated: bool,
) -> Result<Option<native::VipsImageHandle>, MediaError> {
    let mut raw = ptr::null_mut();
    let mut was_hdr = 0;
    let mut had_gain_map = 0;
    let rc = unsafe {
        native::fluxer_heif_decode_animated_ex2(
            input.as_ptr().cast(),
            input.len(),
            &mut raw,
            if animated { -1 } else { 1 },
            Limits::animated_total_pixels(),
            &mut was_hdr,
            &mut had_gain_map,
        )
    };
    if rc != 0 || raw.is_null() {
        clear_vips_error();
        metrics::GLOBAL
            .avif_libheif_decode_failures
            .fetch_add(1, Ordering::Relaxed);
        return Ok(None);
    }
    metrics::GLOBAL
        .avif_libheif_decode_count
        .fetch_add(1, Ordering::Relaxed);
    if had_gain_map != 0 {
        metrics::GLOBAL
            .heif_hdr_gain_map_count
            .fetch_add(1, Ordering::Relaxed);
    }
    Ok(native::VipsImageHandle::new(raw))
}

fn try_decode_apng(
    input: &[u8],
    animated: bool,
) -> Result<Option<native::VipsImageHandle>, MediaError> {
    let mut raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_ffmpeg_decode_apng(
            input.as_ptr().cast(),
            input.len(),
            &mut raw,
            if animated {
                Limits::animated_frames().min(c_int::MAX as u32) as c_int
            } else {
                1
            },
            Limits::animated_total_pixels(),
        )
    };
    if rc != 0 || raw.is_null() {
        clear_vips_error();
        return Ok(None);
    }
    Ok(native::VipsImageHandle::new(raw))
}

#[derive(Clone, Copy, Debug)]
struct GifResizeDims {
    width: c_int,
    height: c_int,
}

fn gif_resize_dims(sniffed: mime::SniffInfo, options: &ImageOptions) -> Option<GifResizeDims> {
    if sniffed.width == 0 || sniffed.height == 0 {
        return None;
    }
    let src_w = sniffed.width;
    let src_h = sniffed.height;
    let scale = match (options.width, options.height) {
        (Some(0), _) | (_, Some(0)) => return None,
        (Some(w), Some(h)) => (w as f64 / src_w as f64).min(h as f64 / src_h as f64),
        (Some(w), None) => w as f64 / src_w as f64,
        (None, Some(h)) => h as f64 / src_h as f64,
        (None, None) => return None,
    }
    .min(1.0);
    let target_w = ((src_w as f64) * scale).round().max(1.0) as u32;
    let target_h = ((src_h as f64) * scale).round().max(1.0) as u32;
    if target_w == src_w && target_h == src_h {
        return None;
    }
    Some(GifResizeDims {
        width: target_w as c_int,
        height: target_h as c_int,
    })
}

fn resize_animated_gif_with_ffmpeg(
    input: &[u8],
    dims: GifResizeDims,
    options: &ImageOptions,
) -> Result<Vec<u8>, MediaError> {
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_ffmpeg_resize_gif(
            input.as_ptr().cast(),
            input.len(),
            dims.width,
            dims.height,
            options.deadline_ms.unwrap_or(0),
            &mut out_ptr,
            &mut out_size,
        )
    };
    if rc == -2 {
        return Err(MediaError::RequestTimeout);
    }
    if rc != 0 || out_ptr.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let out = native::WebpBuffer::new(out_ptr, out_size).ok_or(MediaError::MediaTransformFailed)?;
    webp_buffer_to_vec(&out)
}

fn source_supports_pages(mime: &str) -> bool {
    matches!(
        mime,
        "image/webp" | "image/gif" | "image/apng" | "image/heic" | "image/heif" | "image/avif"
    )
}

fn should_use_resize_path(options: &ImageOptions, probe: Option<AnimatedProbe>) -> bool {
    if options.width.is_none() && options.height.is_none() {
        return false;
    }
    if options.animated {
        return true;
    }
    match probe {
        Some(p) => p.pages <= 1,
        None => true,
    }
}

fn effective_transform_format(
    sniffed_mime: &str,
    requested: AssetExtension,
    animated: bool,
) -> AssetExtension {
    if animated && sniffed_mime == "image/apng" && requested == AssetExtension::Png {
        AssetExtension::Apng
    } else {
        requested
    }
}

pub fn transform_image(input: &[u8], options: &ImageOptions) -> Result<ProcessedMedia, MediaError> {
    if input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    if let Some(width) = options.width
        && (width == 0 || width > Limits::image_dimension())
    {
        return Err(MediaError::InvalidImageDimensions);
    }
    if let Some(height) = options.height
        && (height == 0 || height > Limits::image_dimension())
    {
        return Err(MediaError::InvalidImageDimensions);
    }
    ensure_vips_init()?;
    let sniffed = mime::sniff(input);
    let format = effective_transform_format(sniffed.mime, options.format, options.animated);
    let full_canvas_animation = options.animated
        && format == AssetExtension::Webp
        && matches!(sniffed.mime, "image/gif" | "image/apng");
    if options.animated
        && format == AssetExtension::Gif
        && !options.cover_crop
        && sniffed.mime == "image/gif"
    {
        let bytes = if let Some(dims) = gif_resize_dims(sniffed, options) {
            resize_animated_gif_with_ffmpeg(input, dims, options)?
        } else {
            input.to_vec()
        };
        return Ok(ProcessedMedia {
            bytes,
            content_type: "image/gif",
        });
    }
    let decoded_apng = if sniffed.mime == "image/apng" {
        try_decode_apng(input, options.animated)?
    } else {
        None
    };
    let animated_probe = if let Some(image) = decoded_apng.as_ref() {
        animated_probe_from_image(image)
    } else if options.animated {
        probe_animated(input)?
    } else {
        None
    };
    let effective_quality = effective_quality(
        format,
        options.animated,
        input,
        &options.quality,
        animated_probe,
    );
    let effective_effort_override = effective_effort_override(
        format,
        options.animated,
        input,
        &options.quality,
        animated_probe,
        options.effort_override,
    );
    let tone_map_eligible = source_maybe_hdr(input) && output_is_sdr(format);
    let use_heif_path = matches!(sniffed.mime, "image/avif" | "image/heic" | "image/heif")
        && (options.animated || tone_map_eligible);
    if use_heif_path
        && let Some(mut image) = try_decode_heif(input, options.animated || sniffed.animated)?
    {
        validate_vips_image(&image)?;
        if tone_map_eligible {
            image = tone_map_if_hdr(image)?;
        }
        image = resize_loaded_image(image, options)?;
        let page_height = if options.animated {
            page_height(&image)
        } else {
            None
        };
        let bytes = encode_vips_image(
            &image,
            format,
            &effective_quality,
            page_height,
            effective_effort_override,
            anim_limits_from_options(options),
            full_canvas_animation,
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    if let Some(mut image) = decoded_apng {
        validate_vips_image(&image)?;
        image = resize_loaded_image(image, options)?;
        let page_height = if options.animated {
            page_height(&image)
        } else {
            None
        };
        let bytes = encode_vips_image(
            &image,
            format,
            &effective_quality,
            page_height,
            effective_effort_override,
            anim_limits_from_options(options),
            full_canvas_animation,
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    if should_use_resize_path(options, animated_probe) {
        let mut raw = ptr::null_mut();
        let crop = if options.cover_crop && options.width.is_some() && options.height.is_some() {
            native::THUMB_CROP_CENTRE
        } else {
            native::THUMB_CROP_NONE
        };
        let n_pages: c_int = if options.animated && source_supports_pages(sniffed.mime) {
            -1
        } else {
            1
        };
        let rc = unsafe {
            native::fluxer_vips_thumbnail_buffer_ex(
                input.as_ptr().cast(),
                input.len(),
                &mut raw,
                options.width.unwrap_or(0) as c_int,
                options.height.unwrap_or(0) as c_int,
                n_pages,
                crop,
            )
        };
        if rc != 0 || raw.is_null() {
            let err = last_vips_error();
            clear_vips_error();
            tracing::error!(
                target: "fluxer_media_proxy::transform_debug",
                stage = "thumbnail_buffer_ex",
                sniffed_mime = %sniffed.mime,
                animated = options.animated,
                w = options.width.unwrap_or(0),
                h = options.height.unwrap_or(0),
                pages = animated_probe.map(|p| p.pages).unwrap_or(0),
                vips_err = %err,
                "transform failed"
            );
            return Err(MediaError::MediaTransformFailed);
        }
        let mut image =
            native::VipsImageHandle::new(raw).ok_or(MediaError::MediaTransformFailed)?;
        validate_vips_image(&image)?;
        if tone_map_eligible {
            image = tone_map_if_hdr(image)?;
        }
        let page_height = if options.animated {
            page_height(&image)
        } else {
            None
        };
        let bytes = encode_vips_image(
            &image,
            format,
            &effective_quality,
            page_height,
            effective_effort_override,
            anim_limits_from_options(options),
            full_canvas_animation,
        )?;
        return Ok(ProcessedMedia {
            bytes,
            content_type: format.mime(),
        });
    }

    let loader_options = if options.animated && source_supports_pages(sniffed.mime) {
        if sniffed.mime == "image/jpeg" {
            "n=-1,access=sequential"
        } else {
            "n=-1,access=sequential,fail=true"
        }
    } else if sniffed.mime == "image/jpeg" {
        "access=sequential"
    } else {
        "access=sequential,fail=true"
    };
    let loaded = load_image(input, loader_options)?;
    validate_vips_image(&loaded)?;
    let mut oriented_raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_autorot(loaded.as_ptr(), &mut oriented_raw) };
    if rc != 0 || oriented_raw.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let mut base =
        native::VipsImageHandle::new(oriented_raw).ok_or(MediaError::MediaTransformFailed)?;
    validate_vips_image(&base)?;
    if tone_map_eligible {
        base = tone_map_if_hdr(base)?;
    }
    let page_height = if options.animated {
        page_height(&base)
    } else {
        None
    };
    let bytes = encode_vips_image(
        &base,
        format,
        &effective_quality,
        page_height,
        effective_effort_override,
        anim_limits_from_options(options),
        full_canvas_animation,
    )?;
    Ok(ProcessedMedia {
        bytes,
        content_type: format.mime(),
    })
}

pub fn encode_thumbhash(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    ensure_vips_init()?;
    let mut raw = ptr::null_mut();
    let rc = unsafe {
        native::fluxer_vips_thumbnail_buffer_ex(
            input.as_ptr().cast(),
            input.len(),
            &mut raw,
            thumbhash::MAX_DIM as c_int,
            thumbhash::MAX_DIM as c_int,
            1,
            native::THUMB_CROP_NONE,
        )
    };
    if rc != 0 || raw.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let image = native::VipsImageHandle::new(raw).ok_or(MediaError::MediaTransformFailed)?;
    validate_vips_image(&image)?;
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) as u32 };
    let height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) as u32 };
    if width == 0 || height == 0 || width > thumbhash::MAX_DIM || height > thumbhash::MAX_DIM {
        return Err(MediaError::InvalidImageDimensions);
    }
    let mut rgba_ptr: *mut c_void = ptr::null_mut();
    let mut rgba_size: size_t = 0;
    let rc =
        unsafe { native::fluxer_vips_extract_rgba(image.as_ptr(), &mut rgba_ptr, &mut rgba_size) };
    if rc != 0 || rgba_ptr.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaTransformFailed);
    }
    let rgba = native::VipsBuffer::new(rgba_ptr, rgba_size)
        .ok_or(MediaError::MediaTransformFailed)?
        .to_vec();
    thumbhash::encode_rgba(&rgba, width, height).map_err(|_| MediaError::InvalidImageDimensions)
}

#[derive(Clone, Debug)]
pub struct MetadataOptions {
    pub placeholder: bool,
    pub nsfw: nsfw::Config,
}

impl Default for MetadataOptions {
    fn default() -> Self {
        Self {
            placeholder: true,
            nsfw: nsfw::Config::disabled(),
        }
    }
}

#[derive(Serialize)]
struct MetadataResponse {
    content_type: String,
    size: usize,
    content_hash: String,
    format: String,
    width: Option<u32>,
    height: Option<u32>,
    animated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    duration: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    placeholder: Option<String>,
    nsfw: bool,
    nsfw_probability: f32,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct AvProbe {
    pub has_video: bool,
    pub has_audio: bool,
    pub duration_seconds: Option<f64>,
}

pub fn probe_av_media(input: &[u8]) -> Option<AvProbe> {
    if input.is_empty() {
        return None;
    }
    let mut has_video: c_int = 0;
    let mut has_audio: c_int = 0;
    let mut duration_seconds: f64 = 0.0;
    let rc = unsafe {
        native::fluxer_av_probe(
            input.as_ptr().cast(),
            input.len(),
            &mut has_video,
            &mut has_audio,
            &mut duration_seconds,
        )
    };
    if rc != 0 {
        return None;
    }
    Some(AvProbe {
        has_video: has_video != 0,
        has_audio: has_audio != 0,
        duration_seconds: if duration_seconds.is_finite() && duration_seconds > 0.0 {
            Some(duration_seconds)
        } else {
            None
        },
    })
}

fn metadata_content_type(sniffed_mime: &'static str, av_probe: Option<&AvProbe>) -> &'static str {
    if sniffed_mime == "video/mp4"
        && av_probe.is_some_and(|probe| probe.has_audio && !probe.has_video)
    {
        return "audio/mp4";
    }
    sniffed_mime
}

fn metadata_format(sniffed_mime: &str) -> String {
    match sniffed_mime {
        "audio/mpeg" => "mp3".to_owned(),
        "video/quicktime" => "mov".to_owned(),
        "video/x-matroska" => "mkv".to_owned(),
        "image/svg+xml" => "svg".to_owned(),
        "audio/mp4" => "m4a".to_owned(),
        "image/avif-sequence" => "avif".to_owned(),
        "image/apng" => "apng".to_owned(),
        other => other
            .rsplit_once('/')
            .map(|(_, suffix)| suffix.strip_prefix("x-").unwrap_or(suffix))
            .unwrap_or("bin")
            .to_owned(),
    }
}

fn validate_metadata_image_dimensions(
    width: u32,
    height: u32,
    frames: u32,
) -> Result<(), MediaError> {
    validate_dimensions_u32(width, height)?;
    let frame_count = frames.max(1);
    if frame_count > Limits::animated_frames() {
        return Err(MediaError::InvalidImageDimensions);
    }
    if frame_count > 1 {
        let w = width as usize;
        let h = height as usize;
        let per_frame = w.saturating_mul(h);
        let max_total = Limits::animated_total_pixels();
        let fc = frame_count as usize;
        if per_frame > max_total / fc.max(1) {
            return Err(MediaError::InvalidImageDimensions);
        }
    }
    Ok(())
}

async fn scan_for_nsfw(
    client: &reqwest::Client,
    cfg: &nsfw::Config,
    sniffed_mime: &str,
    animated: bool,
    input: &[u8],
) -> Result<nsfw::Result, MediaError> {
    let cat = mime::category(sniffed_mime);
    if cat == Some(mime::Category::Video) {
        let frames = match extract_video_frames_for_nsfw(input) {
            Ok(frames) => frames,
            Err(err) => {
                tracing::warn!("nsfw video frame extract failed: {err:?}");
                return Err(MediaError::NsfwScanUnavailable);
            }
        };
        return match nsfw::check_buffers(client, cfg, &frames).await {
            Ok(verdict) => Ok(verdict),
            Err(err) => {
                tracing::warn!("nsfw video classify failed: {err}");
                Err(MediaError::NsfwScanUnavailable)
            }
        };
    }
    if cat == Some(mime::Category::Image) {
        if animated {
            match extract_animated_image_frames_for_nsfw(input) {
                Ok(frames) => match nsfw::check_buffers(client, cfg, &frames).await {
                    Ok(verdict) => return Ok(verdict),
                    Err(err) => {
                        tracing::warn!("nsfw animated classify failed: {err}");
                        return Err(MediaError::NsfwScanUnavailable);
                    }
                },
                Err(err) => {
                    tracing::warn!(
                        "nsfw animated frame extract failed: {err:?} - falling back to static scan"
                    );
                }
            }
        }
        let jpeg = match encode_static_image_for_nsfw(input) {
            Ok(bytes) => bytes,
            Err(err) => {
                tracing::warn!("nsfw static JPEG encode failed: {err:?}");
                return Err(MediaError::NsfwScanUnavailable);
            }
        };
        return match nsfw::check(client, cfg, &jpeg).await {
            Ok(verdict) => Ok(verdict),
            Err(err) => {
                tracing::warn!("nsfw static classify failed: {err}");
                Err(MediaError::NsfwScanUnavailable)
            }
        };
    }
    Ok(nsfw::Result {
        probability: 0.0,
        is_nsfw: false,
    })
}

fn extract_animated_image_frames_for_nsfw(input: &[u8]) -> Result<Vec<Vec<u8>>, MediaError> {
    ensure_vips_init()?;
    let loaded = load_image(input, "n=-1,access=sequential")?;
    let total_height = unsafe { native::fluxer_vips_image_get_height(loaded.as_ptr()) };
    let width = unsafe { native::fluxer_vips_image_get_width(loaded.as_ptr()) };
    if total_height <= 0 || width <= 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let page_h = page_height(&loaded).unwrap_or(total_height);
    let n_pages: u32 = if page_h <= 0 {
        1
    } else {
        (total_height / page_h).max(1) as u32
    };
    let indices: [u32; 3] = if n_pages <= 1 {
        [0, 0, 0]
    } else if n_pages == 2 {
        [0, 1, 1]
    } else {
        [0, n_pages / 2, n_pages - 1]
    };
    let mut unique: Vec<u32> = Vec::with_capacity(3);
    for idx in indices {
        if !unique.contains(&idx) {
            unique.push(idx);
        }
    }
    let mut out: Vec<Vec<u8>> = Vec::with_capacity(unique.len());
    for idx in unique {
        let top = (idx as c_int) * page_h.max(1);
        let mut sub_raw = ptr::null_mut();
        let rc = unsafe {
            native::fluxer_vips_extract_area(
                loaded.as_ptr(),
                &mut sub_raw,
                0,
                top,
                width,
                page_h.max(1),
            )
        };
        if rc != 0 || sub_raw.is_null() {
            clear_vips_error();
            continue;
        }
        let sub = native::VipsImageHandle::new(sub_raw).ok_or(MediaError::MediaTransformFailed)?;
        let suffix = output_suffix(AssetExtension::Jpeg, "low", None, None)?;
        let mut out_ptr: *mut c_void = ptr::null_mut();
        let mut out_size: size_t = 0;
        let rc = unsafe {
            native::fluxer_vips_image_write_to_buffer(
                sub.as_ptr(),
                suffix.as_ptr(),
                &mut out_ptr,
                &mut out_size,
            )
        };
        if rc != 0 || out_ptr.is_null() {
            clear_vips_error();
            continue;
        }
        let buf =
            native::VipsBuffer::new(out_ptr, out_size).ok_or(MediaError::MediaEncodeFailed)?;
        out.push(vips_buffer_to_vec(&buf)?);
    }
    if out.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(out)
}

fn compute_frame_sample_timestamps(
    duration_seconds: Option<f64>,
    prng: &mut rand_chacha::ChaCha8Rng,
) -> [f64; 3] {
    use rand::RngExt;
    let valid: Option<f64> = duration_seconds.filter(|d| d.is_finite() && *d > 0.0);
    let fallback: f64 = valid.unwrap_or(1.0);

    let clamp = |v: f64| -> f64 {
        if !v.is_finite() {
            return 0.0;
        }
        match valid {
            Some(max_v) => v.clamp(0.0, max_v),
            None => v.max(0.0),
        }
    };

    let start_base = clamp((fallback * 0.1 + 0.5).clamp(1.0, 2.0));
    let middle_base = clamp(fallback / 2.0);
    let end_candidate = if fallback > 2.0 {
        fallback - 1.0
    } else {
        fallback * 0.95
    };
    let min_end = start_base + 0.5;
    let end_base = clamp(end_candidate.max(min_end));

    let mut jitter = |v: f64| -> f64 {
        let radius = (v.abs() * 0.1).max(0.05);
        let r: f64 = prng.random();
        clamp(v + (r * 2.0 - 1.0) * radius)
    };

    [jitter(start_base), jitter(middle_base), jitter(end_base)]
}

fn nsfw_frame_seed(input: &[u8]) -> u64 {
    let take = input.len().min(4096);
    wyhash::wyhash(&input[..take], 0)
}

fn extract_video_frames_for_nsfw(input: &[u8]) -> Result<Vec<Vec<u8>>, MediaError> {
    use rand::SeedableRng as _;
    if input.is_empty() || input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    ensure_vips_init()?;

    let duration = probe_av_media(input).and_then(|p| p.duration_seconds);
    let seed = nsfw_frame_seed(input);
    let mut prng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
    let timestamps = compute_frame_sample_timestamps(duration, &mut prng);

    let mut slots: [native::FluxerNsfwFrameOut; 3] = [native::FluxerNsfwFrameOut::empty(); 3];
    let produced = unsafe {
        native::fluxer_av_extract_frames_for_nsfw(
            input.as_ptr().cast(),
            input.len(),
            timestamps.as_ptr(),
            timestamps.len(),
            slots.as_mut_ptr(),
        )
    };
    let mut out: Vec<Vec<u8>> = Vec::with_capacity(3);
    if produced > 0 {
        for slot in slots.iter() {
            if !slot.data.is_null() && slot.len > 0 {
                let bytes =
                    unsafe { slice::from_raw_parts(slot.data.cast::<u8>(), slot.len).to_vec() };
                out.push(bytes);
            }
        }
    }
    unsafe {
        native::fluxer_nsfw_frames_free(slots.as_mut_ptr(), slots.len());
    }
    if out.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(out)
}

pub async fn metadata_json_with_options(
    input: &[u8],
    _filename: &str,
    options: MetadataOptions,
    nsfw_client: &reqwest::Client,
) -> Result<String, MediaError> {
    if input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    let sniffed = mime::sniff(input);
    if !mime::is_supported_media_mime(sniffed.mime) {
        return Err(MediaError::UnsupportedMediaType);
    }
    let initial_category = mime::category(sniffed.mime).ok_or(MediaError::UnsupportedMediaType)?;
    let is_image = initial_category == mime::Category::Image;
    let dims = if is_image {
        Some(probe_image_dims(input)?)
    } else {
        None
    };
    let frames_count = dims.map(|d| d.pages).unwrap_or(sniffed.frames);
    let mut width = dims.map(|d| d.width).unwrap_or(sniffed.width);
    let mut height = dims.map(|d| d.height).unwrap_or(sniffed.height);
    if is_image && (width > 0 || height > 0 || frames_count > 1) {
        validate_metadata_image_dimensions(width, height, frames_count)?;
    }

    let av_probe = if matches!(
        initial_category,
        mime::Category::Video | mime::Category::Audio
    ) {
        Some(probe_av_media(input).ok_or(MediaError::MediaDecodeFailed)?)
    } else {
        None
    };
    let content_type = metadata_content_type(sniffed.mime, av_probe.as_ref());
    let category = mime::category(content_type).ok_or(MediaError::UnsupportedMediaType)?;
    if let Some(probe) = av_probe.as_ref()
        && category == mime::Category::Audio
        && !probe.has_audio
    {
        return Err(MediaError::MediaDecodeFailed);
    }

    let mut video_thumb_jpeg: Option<Vec<u8>> = None;
    if category == mime::Category::Video
        && av_probe.as_ref().is_some_and(|p| p.has_video)
        && let Ok(thumb) = extract_video_thumbnail(input, AssetExtension::Jpeg)
    {
        if let Ok(d) = probe_image_dims(&thumb.bytes) {
            width = d.width;
            height = d.height;
        }
        video_thumb_jpeg = Some(thumb.bytes);
    }

    let placeholder = if options.placeholder {
        let source: Option<&[u8]> = if is_image {
            Some(input)
        } else {
            video_thumb_jpeg.as_deref()
        };
        source
            .and_then(|src| encode_thumbhash(src).ok())
            .map(|bytes| general_purpose::STANDARD.encode(bytes))
    } else {
        None
    };

    let nsfw_enabled = nsfw::is_enabled(&options.nsfw);
    let should_scan = nsfw_enabled
        && (is_image
            || (category == mime::Category::Video
                && av_probe.as_ref().is_some_and(|p| p.has_video)));
    let (nsfw_flag, nsfw_probability) = if should_scan {
        let verdict = scan_for_nsfw(
            nsfw_client,
            &options.nsfw,
            sniffed.mime,
            sniffed.animated || dims.is_some_and(|d| d.pages > 1),
            input,
        )
        .await?;
        (verdict.is_nsfw, verdict.probability)
    } else {
        (false, 0.0)
    };

    let duration = av_probe.as_ref().and_then(|p| {
        p.duration_seconds
            .filter(|d| d.is_finite() && *d > 0.0)
            .map(|d| d.ceil() as u32)
    });
    let (response_width, response_height) = if width > 0 && height > 0 {
        (Some(width), Some(height))
    } else {
        (None, None)
    };

    let format = metadata_format(content_type);
    let content_hash = hex::encode(Sha256::digest(input));
    let response = MetadataResponse {
        content_type: content_type.to_owned(),
        size: input.len(),
        content_hash,
        format,
        width: response_width,
        height: response_height,
        animated: sniffed.animated || dims.is_some_and(|d| d.pages > 1),
        duration,
        placeholder,
        nsfw: nsfw_flag,
        nsfw_probability,
    };
    serde_json::to_string(&response).map_err(|_| MediaError::MediaEncodeFailed)
}

pub async fn metadata_json(input: &[u8], filename: &str) -> Result<String, MediaError> {
    let client = reqwest::Client::builder()
        .user_agent(constants::OUTBOUND_USER_AGENT)
        .build()
        .expect("reqwest::Client::builder with only user_agent always builds");
    metadata_json_with_options(input, filename, MetadataOptions::default(), &client).await
}

pub fn encode_static_image_for_nsfw(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    let mut options = ImageOptions {
        width: Some(512),
        height: Some(512),
        format: AssetExtension::Jpeg,
        quality: "low".to_owned(),
        animated: false,
        cover_crop: false,
        ..Default::default()
    };
    options.animated = false;
    transform_image(input, &options).map(|media| media.bytes)
}

pub fn extract_video_thumbnail(
    input: &[u8],
    format: AssetExtension,
) -> Result<ProcessedMedia, MediaError> {
    if input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    ensure_vips_init()?;
    if !matches!(
        format,
        AssetExtension::Jpeg
            | AssetExtension::Png
            | AssetExtension::Webp
            | AssetExtension::Gif
            | AssetExtension::Apng
    ) {
        return Err(MediaError::UnsupportedOutputFormat);
    }
    let suffix = output_suffix(format, "high", None, None)?;
    let mut out_ptr: *mut c_void = ptr::null_mut();
    let mut out_size: size_t = 0;
    let rc = unsafe {
        native::fluxer_ffmpeg_video_thumbnail(
            input.as_ptr().cast(),
            input.len(),
            suffix.as_ptr(),
            constants::MAX_VIDEO_PACKETS_FOR_THUMBNAIL as c_int,
            &mut out_ptr,
            &mut out_size,
        )
    };
    if rc != 0 || out_ptr.is_null() {
        clear_vips_error();
        return Err(MediaError::MediaDecodeFailed);
    }
    let out = native::VipsBuffer::new(out_ptr, out_size).ok_or(MediaError::MediaDecodeFailed)?;
    let bytes = vips_buffer_to_vec(&out)?;
    Ok(ProcessedMedia {
        bytes,
        content_type: format.mime(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn synthetic_png(width: u32, height: u32) -> Vec<u8> {
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
        let image = native::VipsImageHandle::new(image).unwrap();
        let suffix = CString::new(".png[strip]").unwrap();
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
        native::VipsBuffer::new(out_ptr, out_size).unwrap().to_vec()
    }

    fn synthetic_wav() -> Vec<u8> {
        let sample_rate = 8_000u32;
        let channels = 1u16;
        let bits_per_sample = 8u16;
        let data_len = sample_rate;
        let byte_rate = sample_rate * u32::from(channels) * u32::from(bits_per_sample) / 8;
        let block_align = channels * bits_per_sample / 8;
        let mut wav = Vec::with_capacity(44 + data_len as usize);
        wav.extend_from_slice(b"RIFF");
        wav.extend_from_slice(&(36 + data_len).to_le_bytes());
        wav.extend_from_slice(b"WAVEfmt ");
        wav.extend_from_slice(&16u32.to_le_bytes());
        wav.extend_from_slice(&1u16.to_le_bytes());
        wav.extend_from_slice(&channels.to_le_bytes());
        wav.extend_from_slice(&sample_rate.to_le_bytes());
        wav.extend_from_slice(&byte_rate.to_le_bytes());
        wav.extend_from_slice(&block_align.to_le_bytes());
        wav.extend_from_slice(&bits_per_sample.to_le_bytes());
        wav.extend_from_slice(b"data");
        wav.extend_from_slice(&data_len.to_le_bytes());
        wav.extend(std::iter::repeat_n(128u8, data_len as usize));
        wav
    }

    fn fixture_audio_only_mp4() -> Vec<u8> {
        base64::engine::general_purpose::STANDARD
            .decode("AAAAHGZ0eXBpc29tAAACAGlzb21pc28ybXA0MQAAAxptb292AAAAbG12aGQAAAAAAAAAAAAAAAAAAAPoAAAA+gABAAABAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAACRXRyYWsAAABcdGtoZAAAAAMAAAAAAAAAAAAAAAEAAAAAAAAA+gAAAAAAAAAAAAAAAQEAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAACRlZHRzAAAAHGVsc3QAAAAAAAAAAQAAAPoAAAQAAAEAAAAAAb1tZGlhAAAAIG1kaGQAAAAAAAAAAAAAAAAAAB9AAAAL0FXEAAAAAAAtaGRscgAAAAAAAAAAc291bgAAAAAAAAAAAAAAAFNvdW5kSGFuZGxlcgAAAAFobWluZgAAABBzbWhkAAAAAAAAAAAAAAAkZGluZgAAABxkcmVmAAAAAAAAAAEAAAAMdXJsIAAAAAEAAAEsc3RibAAAAH5zdHNkAAAAAAAAAAEAAABubXA0YQAAAAAAAAABAAAAAAAAAAAAAQAQAAAAAB9AAAAAAAA2ZXNkcwAAAAADgICAJQABAASAgIAXQBUAAAAAAD6AAAACZQWAgIAFFYhW5QAGgICAAQIAAAAUYnRydAAAAAAAAD6AAAACZQAAACBzdHRzAAAAAAAAAAIAAAACAAAEAAAAAAEAAAPQAAAAHHN0c2MAAAAAAAAAAQAAAAEAAAADAAAAAQAAACBzdHN6AAAAAAAAAAAAAAADAAAAFQAAAAQAAAAEAAAAFHN0Y28AAAAAAAAAAQAAA0YAAAAac2dwZAEAAAByb2xsAAAAAgAAAAH//wAAABxzYmdwAAAAAHJvbGwAAAABAAAAAwAAAAEAAABhdWR0YQAAAFltZXRhAAAAAAAAACFoZGxyAAAAAAAAAABtZGlyYXBwbAAAAAAAAAAAAAAAACxpbHN0AAAAJKl0b28AAAAcZGF0YQAAAAEAAAAATGF2ZjYxLjcuMTAyAAAACGZyZWUAAAAlbWRhdN4CAExhdmM2MS4xOS4xMDEAAjBADgEYIAcBGCAH")
            .unwrap()
    }

    fn fixture_audio_mp4_with_attached_picture() -> Vec<u8> {
        base64::engine::general_purpose::STANDARD
            .decode("AAAAHGZ0eXBpc29tAAACAGlzb21pc28ybXA0MQAABBNtb292AAAAbG12aGQAAAAAAAAAAAAAAAAAAAPoAAAA+gABAAABAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAACRXRyYWsAAABcdGtoZAAAAAMAAAAAAAAAAAAAAAEAAAAAAAAA+gAAAAAAAAAAAAAAAQEAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAACRlZHRzAAAAHGVsc3QAAAAAAAAAAQAAAPoAAAQAAAEAAAAAAb1tZGlhAAAAIG1kaGQAAAAAAAAAAAAAAAAAAB9AAAAL0FXEAAAAAAAtaGRscgAAAAAAAAAAc291bgAAAAAAAAAAAAAAAFNvdW5kSGFuZGxlcgAAAAFobWluZgAAABBzbWhkAAAAAAAAAAAAAAAkZGluZgAAABxkcmVmAAAAAAAAAAEAAAAMdXJsIAAAAAEAAAEsc3RibAAAAH5zdHNkAAAAAAAAAAEAAABubXA0YQAAAAAAAAABAAAAAAAAAAAAAQAQAAAAAB9AAAAAAAA2ZXNkcwAAAAADgICAJQABAASAgIAXQBUAAAAAAAJlAAACZQWAgIAFFYhW5QAGgICAAQIAAAAUYnRydAAAAAAAAAJlAAACZQAAACBzdHRzAAAAAAAAAAIAAAACAAAEAAAAAAEAAAPQAAAAHHN0c2MAAAAAAAAAAQAAAAEAAAADAAAAAQAAACBzdHN6AAAAAAAAAAAAAAADAAAAFQAAAAQAAAAEAAAAFHN0Y28AAAAAAAAAAQAABD8AAAAac2dwZAEAAAByb2xsAAAAAgAAAAH//wAAABxzYmdwAAAAAHJvbGwAAAABAAAAAwAAAAEAAAFadWR0YQAAAVJtZXRhAAAAAAAAACFoZGxyAAAAAAAAAABtZGlyYXBwbAAAAAAAAAAAAAAAASVpbHN0AAAAJKl0b28AAAAcZGF0YQAAAAEAAAAATGF2ZjYxLjcuMTAyAAAA+WNvdnIAAADxZGF0YQAAAA0AAAAA/9j/4AAQSkZJRgABAgAAAQABAAD//gAQTGF2YzYxLjE5LjEwMQD/2wBDAAgEBAQEBAUFBQUFBQYGBgYGBgYGBgYGBgYHBwcICAgHBwcGBgcHCAgICAkJCQgICAgJCQoKCgwMCwsODg4RERT/xABMAAEBAAAAAAAAAAAAAAAAAAAABgEBAQAAAAAAAAAAAAAAAAAABgcQAQAAAAAAAAAAAAAAAAAAAAARAQAAAAAAAAAAAAAAAAAAAAD/wAARCAACAAIDASIAAhEAAxEA/9oADAMBAAIRAxEAPwCLAFF/f//ZAAAACGZyZWUAAAAlbWRhdN4CAExhdmM2MS4xOS4xMDEAAjBADgEYIAcBGCAH")
            .unwrap()
    }

    fn animated_gif_fixture() -> Vec<u8> {
        base64::engine::general_purpose::STANDARD
            .decode("R0lGODlhIAAgAPEAAAAAAP8AAP///wAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJFAAAACwAAAAAIAAgAAACPYyPGcDtD5Q8sLY5rcVSV654EJiID4mYDkqpDGu4LyxHtAwv+O3mtb9j1YbEovGITCqXzKbzCY1Kp9QqsQAAIfkECRQAAAAsAAAAACAAIACDAAAAAAD/AAD/AAD/AAD/AAD/AAD/AAD/AAD/AAD/////AAAAAAAAAAAAAAAAAAAABFsQyEmrvTjrzbv/YCiOZGmeaKqubOu+MBUIA1EQgxCYBpH8wATBQDoEj7+DyIBsEj8BX/NIACGmTdAAiwRJuUBQARz0ksOf7TlhXbOhX24VxCQ/QUauctRrDiURADs=")
            .unwrap()
    }

    fn metadata_value(input: &[u8], filename: &str) -> serde_json::Value {
        let meta = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async {
                let client = reqwest::Client::new();
                metadata_json_with_options(input, filename, MetadataOptions::default(), &client)
                    .await
                    .unwrap()
            });
        serde_json::from_str(&meta).unwrap()
    }

    fn read_u24_le(bytes: &[u8]) -> Option<u32> {
        (bytes.len() >= 3)
            .then(|| bytes[0] as u32 | ((bytes[1] as u32) << 8) | ((bytes[2] as u32) << 16))
    }

    fn webp_chunk_payloads<'a>(bytes: &'a [u8], fourcc: &[u8; 4]) -> Vec<&'a [u8]> {
        if bytes.len() < 12 || &bytes[0..4] != b"RIFF" || &bytes[8..12] != b"WEBP" {
            return Vec::new();
        }
        let mut chunks = Vec::new();
        let mut offset = 12usize;
        while offset + 8 <= bytes.len() {
            let chunk_size = u32::from_le_bytes([
                bytes[offset + 4],
                bytes[offset + 5],
                bytes[offset + 6],
                bytes[offset + 7],
            ]) as usize;
            let payload_start = offset + 8;
            let Some(payload_end) = payload_start.checked_add(chunk_size) else {
                break;
            };
            if payload_end > bytes.len() {
                break;
            }
            if &bytes[offset..offset + 4] == fourcc {
                chunks.push(&bytes[payload_start..payload_end]);
            }
            offset = payload_end + (chunk_size & 1);
        }
        chunks
    }

    fn webp_canvas_size(bytes: &[u8]) -> Option<(u32, u32, u8)> {
        let vp8x = webp_chunk_payloads(bytes, b"VP8X").into_iter().next()?;
        if vp8x.len() < 10 {
            return None;
        }
        let width = read_u24_le(&vp8x[4..7])? + 1;
        let height = read_u24_le(&vp8x[7..10])? + 1;
        Some((width, height, vp8x[0]))
    }

    fn first_webp_anim_frame_size(bytes: &[u8]) -> Option<(u32, u32)> {
        let anmf = webp_chunk_payloads(bytes, b"ANMF").into_iter().next()?;
        if anmf.len() < 16 {
            return None;
        }
        let width = read_u24_le(&anmf[6..9])? + 1;
        let height = read_u24_le(&anmf[9..12])? + 1;
        Some((width, height))
    }

    fn gif_frame_delays_cs(bytes: &[u8]) -> Vec<u16> {
        if bytes.len() < 13 || (&bytes[..6] != b"GIF89a" && &bytes[..6] != b"GIF87a") {
            return Vec::new();
        }
        let mut offset = 13usize;
        if bytes[10] & 0x80 != 0 {
            let entries = 1usize << ((bytes[10] & 0x07) + 1);
            offset = offset.saturating_add(entries.saturating_mul(3));
        }
        let mut delays = Vec::new();
        while offset < bytes.len() {
            match bytes[offset] {
                0x21 => {
                    if offset + 1 >= bytes.len() {
                        break;
                    }
                    if bytes[offset + 1] == 0xf9 {
                        if offset + 7 >= bytes.len() || bytes[offset + 2] != 4 {
                            break;
                        }
                        delays.push(u16::from_le_bytes([bytes[offset + 4], bytes[offset + 5]]));
                        offset += 8;
                        continue;
                    }
                    offset += 2;
                    while offset < bytes.len() {
                        let len = bytes[offset] as usize;
                        offset += 1;
                        if len == 0 {
                            break;
                        }
                        offset = offset.saturating_add(len);
                    }
                }
                0x2c => {
                    if offset + 9 >= bytes.len() {
                        break;
                    }
                    let local_entries = if bytes[offset + 9] & 0x80 != 0 {
                        1usize << ((bytes[offset + 9] & 0x07) + 1)
                    } else {
                        0
                    };
                    offset += 10 + local_entries * 3 + 1;
                    while offset < bytes.len() {
                        let len = bytes[offset] as usize;
                        offset += 1;
                        if len == 0 {
                            break;
                        }
                        offset = offset.saturating_add(len);
                    }
                }
                0x3b => break,
                _ => break,
            }
        }
        delays
    }

    #[test]
    fn animated_webp_default_effort_matches_fast_tier() {
        assert_eq!(2, effort_for("low", true));
        assert_eq!(2, effort_for("high", true));
        assert_eq!(4, effort_for("high", false));
    }

    #[test]
    fn auto_animated_palette_quality_matches_v1_thresholds() {
        let gif_header = b"GIF89a\x01\x00\x01\x00";
        let small_probe = AnimatedProbe {
            width: 300,
            height: 225,
            pages: 100,
        };
        let large_probe = AnimatedProbe {
            width: 480,
            height: 480,
            pages: 240,
        };

        assert_eq!(
            "lossless",
            effective_quality(
                AssetExtension::Webp,
                true,
                gif_header,
                "auto",
                Some(small_probe)
            )
        );
        assert_eq!(
            "high",
            effective_quality(
                AssetExtension::Webp,
                true,
                gif_header,
                "auto",
                Some(large_probe)
            )
        );
        assert_eq!(
            "high",
            effective_quality(
                AssetExtension::Webp,
                true,
                gif_header,
                "high",
                Some(small_probe)
            )
        );
        assert_eq!(
            "low",
            effective_quality(
                AssetExtension::Webp,
                true,
                gif_header,
                "low",
                Some(small_probe)
            )
        );
        assert_eq!(
            None,
            effective_effort_override(
                AssetExtension::Webp,
                true,
                gif_header,
                "auto",
                Some(small_probe),
                None
            )
        );
        assert_eq!(
            Some(0),
            effective_effort_override(
                AssetExtension::Webp,
                true,
                gif_header,
                "auto",
                Some(large_probe),
                None
            )
        );
        assert_eq!(
            Some(2),
            effective_effort_override(
                AssetExtension::Webp,
                true,
                gif_header,
                "auto",
                Some(large_probe),
                Some(2)
            )
        );
    }

    #[test]
    fn animated_encode_deadline_keeps_flush_headroom() {
        assert_eq!(
            Some(17_000),
            anim_limits_from_options(&ImageOptions {
                deadline_ms: Some(20_000),
                ..Default::default()
            })
            .deadline_unix_ms
        );
        assert_eq!(
            Some(1_500),
            anim_limits_from_options(&ImageOptions {
                deadline_ms: Some(1_500),
                ..Default::default()
            })
            .deadline_unix_ms
        );
        assert_eq!(
            None,
            anim_limits_from_options(&ImageOptions::default()).deadline_unix_ms
        );
    }

    #[test]
    fn animated_gif_encodes_to_animated_webp_with_alpha() {
        let gif = animated_gif_fixture();

        let animated_webp = transform_image(
            &gif,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Webp,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", animated_webp.content_type);
        assert!(animated_webp.bytes.starts_with(b"RIFF"));
        assert_eq!(&animated_webp.bytes[8..12], b"WEBP");
        assert!(animated_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
        assert!(
            animated_webp.bytes.windows(4).any(|chunk| chunk == b"ALPH")
                || animated_webp.bytes.windows(4).any(|chunk| chunk == b"VP8L")
        );
        let (canvas_width, canvas_height, feature_flags) =
            webp_canvas_size(&animated_webp.bytes).unwrap();
        assert_eq!((32, 32), (canvas_width, canvas_height));
        assert_ne!(0, feature_flags & 0x02);
        assert_ne!(0, feature_flags & 0x10);
        assert_eq!(
            Some((32, 32)),
            first_webp_anim_frame_size(&animated_webp.bytes)
        );

        let static_webp = transform_image(
            &gif,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Webp,
                quality: "lossless".to_owned(),
                animated: false,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", static_webp.content_type);
        assert!(static_webp.bytes.starts_with(b"RIFF"));
        assert_eq!(&static_webp.bytes[8..12], b"WEBP");

        let animated_gif = transform_image(
            &gif,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Gif,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/gif", animated_gif.content_type);
        assert!(
            animated_gif.bytes.starts_with(b"GIF89a") || animated_gif.bytes.starts_with(b"GIF87a")
        );

        let animated_png = transform_image(
            &gif,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Apng,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/apng", animated_png.content_type);
        let apng_chunks = parse_png_chunks(&animated_png.bytes).unwrap();
        assert_eq!(Some((32, 32)), png_ihdr_dimensions(apng_chunks[0].data));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fcTL"));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fdAT"));
    }

    #[test]
    fn animated_gif_resize_preserves_last_frame_delay() {
        let gif = animated_gif_fixture();
        assert_eq!(vec![20, 20], gif_frame_delays_cs(&gif));

        let resized = transform_image(
            &gif,
            &ImageOptions {
                width: Some(16),
                height: Some(16),
                format: AssetExtension::Gif,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();

        assert_eq!("image/gif", resized.content_type);
        assert_eq!(vec![20, 20], gif_frame_delays_cs(&resized.bytes));
    }

    #[test]
    fn animated_gif_two_bounds_fit_inside_without_distortion() {
        let dims = gif_resize_dims(
            mime::SniffInfo {
                mime: "image/gif",
                animated: true,
                width: 320,
                height: 240,
                ..Default::default()
            },
            &ImageOptions {
                width: Some(240),
                height: Some(240),
                format: AssetExtension::Gif,
                animated: true,
                ..Default::default()
            },
        )
        .expect("4:3 GIF should be reduced to fit in a 240px square");

        assert_eq!(240, dims.width);
        assert_eq!(180, dims.height);
    }

    #[test]
    fn animated_apng_input_transforms_through_ffmpeg_decode_path() {
        let fixture_b64 = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAACXBIWXMAAAABAAAAAQBPJcTWAAAACGFjVEwAAAACAAAAAPONk3AAAAAaZmNUTAAAAAAAAAAQAAAAEAAAAAAAAAAAAAEABQAAaBqIGAAAAK1JREFUeJxjqGf4hxUxIKF/DH+BiOH/fxAiU4Mcgx0aYkBCtgy2QMRgZwdCg1yDCQMvEL1oMACix00mQCTFwANE/gx8QHSrUwuI0DXsbpkCVJ3IwBDEwADUsLprLkTD2ZoeoOp8OQZgaIEQxG5IiNqCXQARQw5pqJOooCFPj+Fcn96x9gxTBpS49NBk2DwlhBoaaOkHZUOGqYvDnrWJt6kRF6xADUAE1ABEuCIOAPEY5L3Pr8FWAAAAGmZjVEwAAAABAAAAAQAAAAEAAAAAAAAAAAABAAUAAMpQnTkAAAAQZmRBVAAAAAJ4nGOoZ/gHAAJ/AX511aUxAAAAAElFTkSuQmCC";
        let apng = base64::engine::general_purpose::STANDARD
            .decode(fixture_b64)
            .unwrap();

        let animated_webp = transform_image(
            &apng,
            &ImageOptions {
                width: Some(8),
                format: AssetExtension::Webp,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", animated_webp.content_type);
        assert!(animated_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
        assert_eq!(
            Some((8, 8)),
            first_webp_anim_frame_size(&animated_webp.bytes)
        );

        let static_webp = transform_image(
            &apng,
            &ImageOptions {
                width: Some(8),
                format: AssetExtension::Webp,
                quality: "lossless".to_owned(),
                animated: false,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", static_webp.content_type);
        assert!(!static_webp.bytes.windows(4).any(|chunk| chunk == b"ANIM"));
        assert_eq!(
            ImageDimensions {
                width: 8,
                height: 8,
                pages: 1
            },
            probe_image_dims(&static_webp.bytes).unwrap()
        );

        let animated_gif = transform_image(
            &apng,
            &ImageOptions {
                width: Some(8),
                format: AssetExtension::Gif,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/gif", animated_gif.content_type);
        assert!(
            animated_gif.bytes.starts_with(b"GIF89a") || animated_gif.bytes.starts_with(b"GIF87a")
        );

        let animated_png = transform_image(
            &apng,
            &ImageOptions {
                width: Some(8),
                format: AssetExtension::Apng,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/apng", animated_png.content_type);
        assert!(animated_png.bytes.starts_with(b"\x89PNG\r\n\x1a\n"));
        let apng_chunks = parse_png_chunks(&animated_png.bytes).unwrap();
        assert_eq!(Some((8, 8)), png_ihdr_dimensions(apng_chunks[0].data));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fcTL"));
        assert!(apng_chunks.iter().any(|chunk| chunk.kind == *b"fdAT"));
        assert_eq!(
            Some(2),
            apng_chunks
                .iter()
                .find(|chunk| chunk.kind == *b"acTL")
                .and_then(|chunk| chunk.data.get(..4))
                .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
        );

        let animated_png_alias = transform_image(
            &apng,
            &ImageOptions {
                width: Some(8),
                format: AssetExtension::Png,
                quality: "lossless".to_owned(),
                animated: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/apng", animated_png_alias.content_type);
        let alias_chunks = parse_png_chunks(&animated_png_alias.bytes).unwrap();
        assert_eq!(Some((8, 8)), png_ihdr_dimensions(alias_chunks[0].data));
        assert!(alias_chunks.iter().any(|chunk| chunk.kind == *b"acTL"));
    }

    #[test]
    fn avif_direct_decode_applies_resize_and_crop() {
        let fixture_b64 = "AAAAIGZ0eXBhdmlmAAAAAGF2aWZtaWYxbWlhZk1BMUIAAAD5bWV0YQAAAAAAAAAvaGRscgAAAAAAAAAAcGljdAAAAAAAAAAAAAAAAFBpY3R1cmVIYW5kbGVyAAAAAA5waXRtAAAAAAABAAAAHmlsb2MAAAAARAAAAQABAAAAAQAAASEAAAFZAAAAKGlpbmYAAAAAAAEAAAAaaW5mZQIAAAAAAQAAYXYwMUNvbG9yAAAAAGppcHJwAAAAS2lwY28AAAAUaXNwZQAAAAAAAABAAAAAMAAAABBwaXhpAAAAAAMICAgAAAAMYXYxQ4EADAAAAAATY29scm5jbHgAAgACAAIAAAAAF2lwbWEAAAAAAAAAAQABBAECgwQAAAFhbWRhdAoGGBV/vbAIMs4CRgAABBBBQEqBANtxpEnkS8i7Ewu1Oa+E52+0gHxmN6DekBiIYovbIpo+I+L2MbaIuGgpmhiq3wmhtHx3Lyb9HWhe08jL3lTmL0L92z3pFGZiyNiXjoWSnt6Vs2YF9Ogt2S1YudcnVbcGESJSHNs+6UmubDO+hIB+aL08iAZr/qkVPsTgHY5xL3y7b0B4W8BuTdfXeVy/nJ8V2xmFc1fc4DXzEalW69hTvoJEKuitiwnHu32Gr1Qbjk88s36/tv1BQ2bbYX/QIFDJwLoME7YrHOzOB0zEmhjjdKZkNDwlG0u7YsB5EvaXAnkkgF6l5yaKb8tv2ZBYJO+kDNE7uK8kt5dEIlsrravn8byytjhCTzx5rRLwkj6obavPpIgh/z/z9mG1oxZ2zWugKXunGbw64JUJ+fUiTa2frsG0dGb02dKJ4rPXq9ZQY/B4G3nuZg==";
        let avif = base64::engine::general_purpose::STANDARD
            .decode(fixture_b64)
            .unwrap();

        let resized = transform_image(
            &avif,
            &ImageOptions {
                width: Some(32),
                format: AssetExtension::Webp,
                quality: "high".to_owned(),
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", resized.content_type);
        assert_eq!(
            ImageDimensions {
                width: 32,
                height: 24,
                pages: 1
            },
            probe_image_dims(&resized.bytes).unwrap()
        );

        let cropped = transform_image(
            &avif,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Png,
                quality: "high".to_owned(),
                cover_crop: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/png", cropped.content_type);
        assert_eq!(
            ImageDimensions {
                width: 32,
                height: 32,
                pages: 1
            },
            probe_image_dims(&cropped.bytes).unwrap()
        );
    }

    #[test]
    fn transforms_png_to_webp() {
        let png = synthetic_png(32, 24);
        let out = transform_image(
            &png,
            &ImageOptions {
                width: Some(16),
                format: AssetExtension::Webp,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!("image/webp", out.content_type);
        assert!(out.bytes.starts_with(b"RIFF"));
    }

    #[test]
    fn transforms_static_png_with_animated_flag_does_not_pass_n_to_pngload() {
        let png = synthetic_png(48, 48);
        let out = transform_image(
            &png,
            &ImageOptions {
                width: Some(32),
                height: Some(32),
                format: AssetExtension::Webp,
                animated: true,
                ..Default::default()
            },
        )
        .expect("static-png + animated=true must transform without erroring");
        assert_eq!("image/webp", out.content_type);
        assert!(out.bytes.starts_with(b"RIFF"));
    }

    #[test]
    fn source_supports_pages_matches_libvips_loader_list() {
        assert!(source_supports_pages("image/webp"));
        assert!(source_supports_pages("image/gif"));
        assert!(source_supports_pages("image/apng"));
        assert!(source_supports_pages("image/heif"));
        assert!(source_supports_pages("image/avif"));
        assert!(!source_supports_pages("image/png"));
        assert!(!source_supports_pages("image/jpeg"));
        assert!(!source_supports_pages("image/bmp"));
        assert!(!source_supports_pages("application/octet-stream"));
    }

    #[test]
    fn metadata_json_includes_dimensions_and_placeholder() {
        let png = synthetic_png(16, 16);
        let meta = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async {
                let client = reqwest::Client::new();
                metadata_json_with_options(&png, "test.png", MetadataOptions::default(), &client)
                    .await
                    .unwrap()
            });
        assert!(meta.contains("\"format\":\"png\""));
        assert!(meta.contains("\"width\":16"));
        assert!(meta.contains("\"height\":16"));
        assert!(meta.contains("\"placeholder\":\""));
        assert!(meta.contains("\"nsfw\":false"));
        assert!(meta.contains("\"nsfw_probability\":0"));
    }

    #[test]
    fn metadata_json_returns_unavailable_when_nsfw_service_fails() {
        let png = synthetic_png(16, 16);
        let err = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                let client = reqwest::Client::new();
                metadata_json_with_options(
                    &png,
                    "test.png",
                    MetadataOptions {
                        placeholder: false,
                        nsfw: nsfw::Config {
                            endpoint: "http://127.0.0.1:9".to_owned(),
                            threshold: 0.85,
                            timeout_ms: 50,
                            connect_timeout_ms: 50,
                        },
                    },
                    &client,
                )
                .await
                .unwrap_err()
            });
        assert_eq!(err, MediaError::NsfwScanUnavailable);
    }

    #[test]
    fn metadata_json_uses_null_dimensions_for_audio() {
        let wav = synthetic_wav();
        let value = metadata_value(&wav, "test.wav");
        assert_eq!(value["content_type"], "audio/wav");
        assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
        assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
        assert_eq!(value["duration"], 1);
    }

    #[test]
    fn metadata_json_classifies_audio_only_mp4_as_audio() {
        let mp4 = fixture_audio_only_mp4();
        assert_eq!("video/mp4", mime::sniff(&mp4).mime);
        let value = metadata_value(&mp4, "renamed.mp4");
        assert_eq!(value["content_type"], "audio/mp4");
        assert_eq!(value["format"], "m4a");
        assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
        assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
        assert_eq!(value["duration"], 1);
    }

    #[test]
    fn metadata_json_treats_mp4_attached_picture_as_audio_cover_art() {
        let mp4 = fixture_audio_mp4_with_attached_picture();
        let probe = probe_av_media(&mp4).unwrap();
        assert!(probe.has_audio);
        assert!(!probe.has_video);
        let value = metadata_value(&mp4, "renamed.mp4");
        assert_eq!(value["content_type"], "audio/mp4");
        assert_eq!(value["format"], "m4a");
        assert_eq!(value.get("width"), Some(&serde_json::Value::Null));
        assert_eq!(value.get("height"), Some(&serde_json::Value::Null));
    }

    #[test]
    fn thumbhash_for_valid_image_is_non_empty() {
        let png = synthetic_png(16, 16);
        let hash = encode_thumbhash(&png).unwrap();
        assert!(!hash.is_empty());
    }

    #[test]
    fn compute_frame_sample_timestamps_distributes_start_middle_end() {
        use rand::SeedableRng as _;
        let seed = [7u8; 32];
        let mut prng = rand_chacha::ChaCha8Rng::from_seed(seed);
        let ts = compute_frame_sample_timestamps(Some(10.0), &mut prng);
        for t in ts {
            assert!((0.0..10.0).contains(&t), "ts {t} out of range");
        }
        assert!(
            ts[0] < ts[1],
            "start {} should precede middle {}",
            ts[0],
            ts[1]
        );
        assert!(
            ts[1] < ts[2],
            "middle {} should precede end {}",
            ts[1],
            ts[2]
        );
    }

    #[test]
    fn nsfw_frame_seed_is_deterministic_per_input() {
        let a = b"hello world this is a video header blob".to_vec();
        let b = b"hello world this is a video header blob".to_vec();
        let c = b"hello world this is a different blob xx".to_vec();
        assert_eq!(nsfw_frame_seed(&a), nsfw_frame_seed(&b));
        assert_ne!(nsfw_frame_seed(&a), nsfw_frame_seed(&c));
    }

    #[test]
    fn extract_video_frames_for_nsfw_returns_multiple_frames() {
        let fixture = std::path::Path::new("tests/fixtures/big-buck-bunny-720p-10s.mp4");
        let alt = std::path::Path::new(".benchmark-cache/media/big-buck-bunny-720p-10s.mp4");
        let path = if fixture.exists() {
            fixture
        } else if alt.exists() {
            alt
        } else {
            eprintln!("skipping: no video fixture available");
            return;
        };
        let bytes = std::fs::read(path).expect("read fixture");
        let frames = extract_video_frames_for_nsfw(&bytes).expect("extract frames");
        assert!(
            !frames.is_empty() && frames.len() <= 3,
            "expected 1-3 frames, got {}",
            frames.len()
        );
        for f in &frames {
            assert!(f.len() > 100, "JPEG frame too small ({} bytes)", f.len());
            assert_eq!(&f[..2], &[0xFF, 0xD8], "not a JPEG");
        }
    }

    fn ffmpeg_gen_mp4(args: &[&str]) -> Option<Vec<u8>> {
        let dir = tempfile::tempdir().ok()?;
        let out = dir.path().join("fixture.mp4");
        let status = std::process::Command::new("ffmpeg")
            .args(["-nostdin", "-loglevel", "error", "-y"])
            .args(args)
            .arg(out.to_str()?)
            .status()
            .ok()?;
        if !status.success() {
            return None;
        }
        std::fs::read(&out).ok()
    }

    fn ffmpeg_gen_rotated_mp4(display_rotation: &str, source_args: &[&str]) -> Option<Vec<u8>> {
        let dir = tempfile::tempdir().ok()?;
        let source = dir.path().join("source.mp4");
        let out = dir.path().join("rotated.mp4");
        let source_status = std::process::Command::new("ffmpeg")
            .args(["-nostdin", "-loglevel", "error", "-y"])
            .args(source_args)
            .arg(source.to_str()?)
            .status()
            .ok()?;
        if !source_status.success() {
            return None;
        }
        let rotate_status = std::process::Command::new("ffmpeg")
            .args(["-nostdin", "-loglevel", "error", "-y", "-noautorotate"])
            .args(["-display_rotation", display_rotation])
            .args(["-i", source.to_str()?])
            .args(["-c", "copy", "-f", "mp4"])
            .arg(out.to_str()?)
            .status()
            .ok()?;
        if !rotate_status.success() {
            return None;
        }
        std::fs::read(&out).ok()
    }

    fn png_dimensions(bytes: &[u8]) -> Option<(u32, u32)> {
        if bytes.len() < 24 || &bytes[..8] != b"\x89PNG\r\n\x1a\n" || &bytes[12..16] != b"IHDR" {
            return None;
        }
        let width = u32::from_be_bytes([bytes[16], bytes[17], bytes[18], bytes[19]]);
        let height = u32::from_be_bytes([bytes[20], bytes[21], bytes[22], bytes[23]]);
        Some((width, height))
    }

    #[test]
    fn video_thumbnail_corrects_display_geometry() {
        let Some(plain) = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=320x240:rate=10:duration=1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ]) else {
            eprintln!("skipping: ffmpeg CLI not available");
            return;
        };
        let thumb = extract_video_thumbnail(&plain, AssetExtension::Png).expect("plain thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((320, 240)),
            "square-pixel video should keep its coded dimensions"
        );

        let anamorphic = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1280x720:rate=10:duration=1",
            "-vf",
            "setsar=2/1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ])
        .expect("anamorphic fixture");
        let thumb = extract_video_thumbnail(&anamorphic, AssetExtension::Png)
            .expect("anamorphic thumbnail");
        let (w, h) = png_dimensions(&thumb.bytes).expect("anamorphic png dimensions");
        assert_eq!(h, 720, "anamorphic height preserved");
        assert!(
            (i64::from(w) - 2560).abs() <= 2,
            "anamorphic width should expand to the ~2560 display width, got {w}"
        );

        let narrow = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1280x720:rate=10:duration=1",
            "-vf",
            "setsar=1/2",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ])
        .expect("narrow anamorphic fixture");
        let thumb = extract_video_thumbnail(&narrow, AssetExtension::Png)
            .expect("narrow anamorphic thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((1280, 1440)),
            "sub-square pixel video should grow height to its display size"
        );

        let rotated = ffmpeg_gen_rotated_mp4(
            "90",
            &[
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=640x480:rate=10:duration=1",
                "-pix_fmt",
                "yuv420p",
                "-f",
                "mp4",
            ],
        )
        .expect("rotated fixture");
        let thumb =
            extract_video_thumbnail(&rotated, AssetExtension::Png).expect("rotated thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((480, 640)),
            "rotation-metadata video should present in its display (portrait) orientation"
        );

        let rotated_counterclockwise = ffmpeg_gen_rotated_mp4(
            "-90",
            &[
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=640x480:rate=10:duration=1",
                "-pix_fmt",
                "yuv420p",
                "-f",
                "mp4",
            ],
        )
        .expect("counterclockwise rotated fixture");
        let thumb = extract_video_thumbnail(&rotated_counterclockwise, AssetExtension::Png)
            .expect("counterclockwise rotated thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((480, 640)),
            "either quarter-turn direction should swap dimensions"
        );

        let rotated_anamorphic = ffmpeg_gen_rotated_mp4(
            "90",
            &[
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=320x180:rate=10:duration=1",
                "-vf",
                "setsar=2/1",
                "-pix_fmt",
                "yuv420p",
                "-f",
                "mp4",
            ],
        )
        .expect("rotated anamorphic fixture");
        let thumb = extract_video_thumbnail(&rotated_anamorphic, AssetExtension::Png)
            .expect("rotated anamorphic thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((180, 640)),
            "SAR correction should happen in coded space before rotation"
        );

        let single = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=200x150:rate=1:duration=1",
            "-frames:v",
            "1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ])
        .expect("single-frame fixture");
        let thumb =
            extract_video_thumbnail(&single, AssetExtension::Png).expect("single-frame thumbnail");
        assert_eq!(
            png_dimensions(&thumb.bytes),
            Some((200, 150)),
            "single-frame clip should still produce a thumbnail"
        );
    }

    #[test]
    fn video_metadata_placeholder_and_dimensions_are_display_corrected() {
        let Some(rotated) = ffmpeg_gen_rotated_mp4(
            "90",
            &[
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=640x480:rate=10:duration=1",
                "-pix_fmt",
                "yuv420p",
                "-f",
                "mp4",
            ],
        ) else {
            eprintln!("skipping: ffmpeg CLI not available");
            return;
        };
        let meta = metadata_value(&rotated, "rotated.mp4");
        assert_eq!(
            meta["width"].as_u64(),
            Some(480),
            "stored width is the display (portrait) width"
        );
        assert_eq!(
            meta["height"].as_u64(),
            Some(640),
            "stored height is the display (portrait) height"
        );
        assert!(
            meta["placeholder"].as_str().is_some_and(|s| !s.is_empty()),
            "placeholder should be generated from the display-corrected frame"
        );

        let anamorphic = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=1280x720:rate=10:duration=1",
            "-vf",
            "setsar=2/1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ])
        .expect("anamorphic fixture");
        let meta = metadata_value(&anamorphic, "anamorphic.mp4");
        assert_eq!(
            meta["height"].as_u64(),
            Some(720),
            "stored height preserved"
        );
        let w = meta["width"].as_u64().expect("stored width");
        assert!(
            (w as i64 - 2560).abs() <= 2,
            "stored width expands to the display width, got {w}"
        );
    }
}
