// SPDX-License-Identifier: AGPL-3.0-or-later

use super::encoding::{
    AnimLimits, frame_delays_ms, resolve_animation_loop_count, truncated_frame_count,
};
use super::loaded_image::validate_dimensions;
use super::native_runtime::clear_vips_error;
use super::streaming_encoder::write_vips_image_to_vec;
use super::{MediaError, PNG_SIGNATURE, ensure_deadline_pending, native_status_error};
use crate::{
    media_limits::MediaLimits,
    native::{self, NativeStatus, VipsImageHandle},
};
use libc::c_int;
use std::{ffi::CString, ptr, sync::OnceLock};

const APNG_FRAME_PNG_SUFFIX: &str = ".png[strip,compression=9,filter=all]";
const MAX_U32_GCD_STEPS: usize = 64;
const PNG_CRC_DEADLINE_CHUNK_BYTES: usize = 64 * 1024;
static PNG_CRC_TABLE: OnceLock<[u32; 256]> = OnceLock::new();

#[derive(Clone, Copy)]
struct PNGChunk<'a> {
    kind: [u8; 4],
    data: &'a [u8],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PNGFrameCursorState {
    BeforeImageData,
    ReadingImageData,
    Complete,
}

struct PNGFrameCursor<'a> {
    bytes: &'a [u8],
    offset: usize,
    chunks_remaining: usize,
    ihdr: [u8; 13],
    state: PNGFrameCursorState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct APNGDelayFraction {
    numerator: u16,
    denominator: u16,
}

impl<'a> PNGFrameCursor<'a> {
    fn new(bytes: &'a [u8], expected_width: u32, expected_height: u32) -> Result<Self, MediaError> {
        if bytes.get(..PNG_SIGNATURE.len()) != Some(PNG_SIGNATURE.as_slice()) {
            return Err(MediaError::MediaEncodeFailed);
        }
        let chunk_bytes = bytes
            .len()
            .checked_sub(PNG_SIGNATURE.len())
            .ok_or(MediaError::MediaEncodeFailed)?;
        let mut cursor = Self {
            bytes,
            offset: PNG_SIGNATURE.len(),
            chunks_remaining: chunk_bytes / 12,
            ihdr: [0; 13],
            state: PNGFrameCursorState::BeforeImageData,
        };
        let first = cursor.next_chunk()?.ok_or(MediaError::MediaEncodeFailed)?;
        if first.kind != *b"IHDR" {
            return Err(MediaError::MediaEncodeFailed);
        }
        let ihdr: [u8; 13] = first
            .data
            .try_into()
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let width = u32::from_be_bytes(
            ihdr[..4]
                .try_into()
                .map_err(|_| MediaError::MediaEncodeFailed)?,
        );
        let height = u32::from_be_bytes(
            ihdr[4..8]
                .try_into()
                .map_err(|_| MediaError::MediaEncodeFailed)?,
        );
        if width != expected_width || height != expected_height {
            return Err(MediaError::MediaEncodeFailed);
        }
        cursor.ihdr = ihdr;
        Ok(cursor)
    }

    fn ihdr(&self) -> &[u8; 13] {
        &self.ihdr
    }

    fn next_idat(&mut self) -> Result<Option<&'a [u8]>, MediaError> {
        if self.state == PNGFrameCursorState::Complete {
            return Ok(None);
        }
        loop {
            let chunk = self.next_chunk()?.ok_or(MediaError::MediaEncodeFailed)?;
            match &chunk.kind {
                b"IHDR" => return Err(MediaError::MediaEncodeFailed),
                b"IDAT" => {
                    self.state = PNGFrameCursorState::ReadingImageData;
                    return Ok(Some(chunk.data));
                }
                b"IEND" => {
                    if self.state != PNGFrameCursorState::ReadingImageData {
                        return Err(MediaError::MediaEncodeFailed);
                    }
                    self.state = PNGFrameCursorState::Complete;
                    return Ok(None);
                }
                _ => {}
            }
        }
    }

    fn next_chunk(&mut self) -> Result<Option<PNGChunk<'a>>, MediaError> {
        if self.state == PNGFrameCursorState::Complete {
            return Ok(None);
        }
        self.chunks_remaining = self
            .chunks_remaining
            .checked_sub(1)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let length_end = self
            .offset
            .checked_add(4)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let kind_end = length_end
            .checked_add(4)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let length_bytes: [u8; 4] = self
            .bytes
            .get(self.offset..length_end)
            .ok_or(MediaError::MediaEncodeFailed)?
            .try_into()
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let kind: [u8; 4] = self
            .bytes
            .get(length_end..kind_end)
            .ok_or(MediaError::MediaEncodeFailed)?
            .try_into()
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let data_len = usize::try_from(u32::from_be_bytes(length_bytes))
            .map_err(|_| MediaError::MediaEncodeFailed)?;
        let data_end = kind_end
            .checked_add(data_len)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let chunk_end = data_end
            .checked_add(4)
            .ok_or(MediaError::MediaEncodeFailed)?;
        let data = self
            .bytes
            .get(kind_end..data_end)
            .ok_or(MediaError::MediaEncodeFailed)?;
        if self.bytes.get(data_end..chunk_end).is_none() {
            return Err(MediaError::MediaEncodeFailed);
        }
        self.offset = chunk_end;
        if kind == *b"IEND" && (!data.is_empty() || chunk_end != self.bytes.len()) {
            return Err(MediaError::MediaEncodeFailed);
        }
        Ok(Some(PNGChunk { kind, data }))
    }
}

fn update_png_crc32(
    table: &[u32; 256],
    mut crc: u32,
    bytes: &[u8],
    deadline: Option<i64>,
) -> Result<u32, MediaError> {
    for chunk in bytes.chunks(PNG_CRC_DEADLINE_CHUNK_BYTES) {
        ensure_deadline_pending(deadline)?;
        for byte in chunk.iter().copied() {
            crc = table[((crc ^ byte as u32) & 0xff) as usize] ^ (crc >> 8);
        }
    }
    Ok(crc)
}

fn png_crc32_parts(
    kind: &[u8; 4],
    prefix: &[u8],
    payload: &[u8],
    deadline: Option<i64>,
) -> Result<u32, MediaError> {
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
    let crc = update_png_crc32(table, 0xffff_ffffu32, kind, deadline)?;
    let crc = update_png_crc32(table, crc, prefix, deadline)?;
    let crc = update_png_crc32(table, crc, payload, deadline)?;
    ensure_deadline_pending(deadline)?;
    Ok(crc ^ 0xffff_ffffu32)
}

pub(super) fn png_crc32(
    kind: &[u8; 4],
    payload: &[u8],
    deadline: Option<i64>,
) -> Result<u32, MediaError> {
    png_crc32_parts(kind, &[], payload, deadline)
}

fn append_be_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_be_bytes());
}

fn reserve_output(
    media_limits: &MediaLimits,
    out: &mut Vec<u8>,
    additional: usize,
) -> Result<(), MediaError> {
    let output_len = out
        .len()
        .checked_add(additional)
        .ok_or(MediaError::StreamTooLong)?;
    if output_len > media_limits.max_media_proxy_bytes() {
        return Err(MediaError::StreamTooLong);
    }
    if output_len <= out.capacity() {
        return Ok(());
    }
    let next_capacity = out
        .capacity()
        .saturating_mul(2)
        .max(output_len)
        .min(media_limits.max_media_proxy_bytes());
    let additional_capacity = next_capacity
        .checked_sub(out.len())
        .ok_or(MediaError::StreamTooLong)?;
    out.try_reserve_exact(additional_capacity)
        .map_err(|_| MediaError::AllocationFailed)
}

fn append_png_chunk(
    media_limits: &MediaLimits,
    out: &mut Vec<u8>,
    kind: &[u8; 4],
    payload: &[u8],
    deadline: Option<i64>,
) -> Result<(), MediaError> {
    let payload_len = u32::try_from(payload.len()).map_err(|_| MediaError::StreamTooLong)?;
    let additional = 12usize
        .checked_add(payload.len())
        .ok_or(MediaError::StreamTooLong)?;
    reserve_output(media_limits, out, additional)?;
    append_be_u32(out, payload_len);
    out.extend_from_slice(kind);
    out.extend_from_slice(payload);
    append_be_u32(out, png_crc32(kind, payload, deadline)?);
    Ok(())
}

fn append_fdat_chunk(
    media_limits: &MediaLimits,
    out: &mut Vec<u8>,
    sequence_number: u32,
    idat: &[u8],
    deadline: Option<i64>,
) -> Result<(), MediaError> {
    let sequence = sequence_number.to_be_bytes();
    let payload_len = idat
        .len()
        .checked_add(sequence.len())
        .ok_or(MediaError::StreamTooLong)?;
    let payload_len_u32 = u32::try_from(payload_len).map_err(|_| MediaError::StreamTooLong)?;
    let additional = 12usize
        .checked_add(payload_len)
        .ok_or(MediaError::StreamTooLong)?;
    reserve_output(media_limits, out, additional)?;
    append_be_u32(out, payload_len_u32);
    out.extend_from_slice(b"fdAT");
    out.extend_from_slice(&sequence);
    out.extend_from_slice(idat);
    append_be_u32(out, png_crc32_parts(b"fdAT", &sequence, idat, deadline)?);
    Ok(())
}

fn gcd_u32(mut a: u32, mut b: u32) -> u32 {
    assert!(a > 0, "validated APNG delay must be nonzero");
    for _ in 0..MAX_U32_GCD_STEPS {
        if b == 0 {
            return a;
        }
        let rem = a % b;
        a = b;
        b = rem;
    }
    panic!("u32 Euclidean algorithm exceeded its iteration bound")
}

fn apng_delay_fraction(delay_ms: u32) -> APNGDelayFraction {
    let divisor = gcd_u32(delay_ms, 1_000);
    let num = delay_ms / divisor;
    let den = 1_000 / divisor;
    if num <= u16::MAX as u32 && den <= u16::MAX as u32 {
        return APNGDelayFraction {
            numerator: num as u16,
            denominator: den as u16,
        };
    }
    APNGDelayFraction {
        numerator: delay_ms.div_ceil(1_000).min(u16::MAX as u32) as u16,
        denominator: 1,
    }
}

fn encode_png_strip(
    media_limits: &MediaLimits,
    image: &VipsImageHandle<'_>,
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
    let strip = unsafe { image.adopt_derived_raw(strip_raw) };
    if let Some(error) = native_status_error(
        NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    let strip = strip.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })?;
    let mut rgba_raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_image_to_rgba(strip.as_ptr(), &mut rgba_raw) };
    let rgba_image = unsafe { strip.adopt_derived_raw(rgba_raw) };
    if let Some(error) = native_status_error(
        NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        clear_vips_error();
        return Err(error);
    }
    let rgba_image = rgba_image.ok_or_else(|| {
        clear_vips_error();
        MediaError::MediaTransformFailed
    })?;
    let suffix = CString::new(APNG_FRAME_PNG_SUFFIX).expect("static string has no NUL");
    write_vips_image_to_vec(
        &rgba_image,
        &suffix,
        media_limits.max_media_proxy_bytes(),
        None,
    )
}

pub(super) fn rewrite_actl_frame_count(
    encoded: &mut [u8],
    actl_offset: usize,
    frame_count: usize,
    deadline: Option<i64>,
) -> Result<(), MediaError> {
    let frame_count = u32::try_from(frame_count).map_err(|_| MediaError::StreamTooLong)?;
    if frame_count == 0 {
        return Err(MediaError::MediaEncodeFailed);
    }
    let kind_start = actl_offset
        .checked_add(4)
        .ok_or(MediaError::MediaEncodeFailed)?;
    let payload_start = kind_start
        .checked_add(4)
        .ok_or(MediaError::MediaEncodeFailed)?;
    let payload_end = payload_start
        .checked_add(8)
        .ok_or(MediaError::MediaEncodeFailed)?;
    let crc_end = payload_end
        .checked_add(4)
        .ok_or(MediaError::MediaEncodeFailed)?;
    if encoded.get(kind_start..payload_start) != Some(b"acTL".as_slice()) {
        return Err(MediaError::MediaEncodeFailed);
    }
    let mut actl = [0u8; 8];
    actl.copy_from_slice(
        encoded
            .get(payload_start..payload_end)
            .ok_or(MediaError::MediaEncodeFailed)?,
    );
    actl[..4].copy_from_slice(&frame_count.to_be_bytes());
    let crc = png_crc32(b"acTL", &actl, deadline)?;
    encoded
        .get_mut(payload_start..payload_end)
        .ok_or(MediaError::MediaEncodeFailed)?
        .copy_from_slice(&actl);
    encoded
        .get_mut(payload_end..crc_end)
        .ok_or(MediaError::MediaEncodeFailed)?
        .copy_from_slice(&crc.to_be_bytes());
    Ok(())
}

pub(super) fn encode_animated_apng(
    image: &VipsImageHandle<'_>,
    page_height: c_int,
    limits: AnimLimits,
    media_limits: &MediaLimits,
    carried_loop_count: Option<u32>,
) -> Result<Vec<u8>, MediaError> {
    let width = unsafe { native::fluxer_vips_image_get_width(image.as_ptr()) };
    let total_height = unsafe { native::fluxer_vips_image_get_height(image.as_ptr()) };
    validate_dimensions(media_limits, width, page_height)?;
    if total_height <= 0 || total_height % page_height != 0 {
        return Err(MediaError::InvalidImageDimensions);
    }
    let page_count = total_height / page_height;
    let n_pages = usize::try_from(page_count).map_err(|_| MediaError::InvalidImageDimensions)?;
    if n_pages == 0 || page_count as u32 > media_limits.animated_frames() {
        return Err(MediaError::InvalidImageDimensions);
    }
    let frame_pixels = usize::try_from(width)
        .ok()
        .and_then(|width| {
            usize::try_from(page_height)
                .ok()
                .and_then(|height| width.checked_mul(height))
        })
        .ok_or(MediaError::InvalidImageDimensions)?;
    let animation_pixels = frame_pixels
        .checked_mul(n_pages)
        .ok_or(MediaError::InvalidImageDimensions)?;
    if animation_pixels > media_limits.animated_total_pixels() {
        return Err(MediaError::InvalidImageDimensions);
    }

    let delays = frame_delays_ms(image, page_count)?;
    let deadline = limits.deadline_unix_ms;
    let flush_deadline = limits.flush_deadline_unix_ms;
    let frame_count = truncated_frame_count(&delays, limits.max_frames, limits.max_duration_ms);
    if frame_count == 0 {
        return Err(MediaError::MediaEncodeFailed);
    }
    let num_plays = resolve_animation_loop_count(image, carried_loop_count);
    let expected_width = width as u32;
    let expected_height = page_height as u32;

    let mut expected_ihdr: Option<[u8; 13]> = None;
    let mut out: Option<Vec<u8>> = None;
    let mut actl_offset: Option<usize> = None;
    let mut sequence_number = 0u32;
    let mut encoded_frames = 0usize;
    for frame_index in 0..frame_count {
        // Overrunning the encode deadline shortens the animation instead of failing the request;
        // the flush headroom is reserved so the frames already written still reach the client.
        if ensure_deadline_pending(deadline).is_err() {
            break;
        }
        let frame_png = encode_png_strip(media_limits, image, frame_index, width, page_height)?;
        let mut frame = PNGFrameCursor::new(&frame_png, expected_width, expected_height)?;
        let frame_ihdr = *frame.ihdr();
        if let Some(existing_ihdr) = expected_ihdr.as_ref() {
            if existing_ihdr != &frame_ihdr {
                return Err(MediaError::MediaEncodeFailed);
            }
        } else {
            expected_ihdr = Some(frame_ihdr);
            let estimated_len = frame_png
                .len()
                .checked_add(256)
                .ok_or(MediaError::StreamTooLong)?
                .min(media_limits.max_media_proxy_bytes());
            let mut encoded = Vec::new();
            encoded
                .try_reserve_exact(estimated_len)
                .map_err(|_| MediaError::AllocationFailed)?;
            reserve_output(media_limits, &mut encoded, PNG_SIGNATURE.len())?;
            encoded.extend_from_slice(PNG_SIGNATURE);
            append_png_chunk(
                media_limits,
                &mut encoded,
                b"IHDR",
                &frame_ihdr,
                flush_deadline,
            )?;
            let mut actl = [0u8; 8];
            actl[..4].copy_from_slice(
                &u32::try_from(frame_count)
                    .map_err(|_| MediaError::StreamTooLong)?
                    .to_be_bytes(),
            );
            actl[4..].copy_from_slice(&num_plays.to_be_bytes());
            actl_offset = Some(encoded.len());
            append_png_chunk(media_limits, &mut encoded, b"acTL", &actl, flush_deadline)?;
            out = Some(encoded);
        }

        let delay_ms = delays
            .get(frame_index)
            .copied()
            .ok_or(MediaError::MediaEncodeFailed)?;
        let delay_fraction = apng_delay_fraction(delay_ms);
        let mut fctl = [0u8; 26];
        fctl[..4].copy_from_slice(&sequence_number.to_be_bytes());
        sequence_number = sequence_number
            .checked_add(1)
            .ok_or(MediaError::StreamTooLong)?;
        fctl[4..8].copy_from_slice(&expected_width.to_be_bytes());
        fctl[8..12].copy_from_slice(&expected_height.to_be_bytes());
        fctl[20..22].copy_from_slice(&delay_fraction.numerator.to_be_bytes());
        fctl[22..24].copy_from_slice(&delay_fraction.denominator.to_be_bytes());
        let encoded = out.as_mut().ok_or(MediaError::MediaEncodeFailed)?;
        append_png_chunk(media_limits, encoded, b"fcTL", &fctl, flush_deadline)?;

        while let Some(idat) = frame.next_idat()? {
            if frame_index == 0 {
                append_png_chunk(media_limits, encoded, b"IDAT", idat, flush_deadline)?;
            } else {
                append_fdat_chunk(media_limits, encoded, sequence_number, idat, flush_deadline)?;
                sequence_number = sequence_number
                    .checked_add(1)
                    .ok_or(MediaError::StreamTooLong)?;
            }
        }
        encoded_frames += 1;
    }
    let mut encoded = out.ok_or(MediaError::MediaEncodeFailed)?;
    if encoded_frames < frame_count {
        rewrite_actl_frame_count(
            &mut encoded,
            actl_offset.ok_or(MediaError::MediaEncodeFailed)?,
            encoded_frames,
            flush_deadline,
        )?;
    }
    append_png_chunk(media_limits, &mut encoded, b"IEND", &[], flush_deadline)?;
    ensure_deadline_pending(flush_deadline)?;
    Ok(encoded)
}
