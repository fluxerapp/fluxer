// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{media_process::native_runtime::ensure_vips_init, native};
use base64::Engine as _;
use libc::{c_int, c_void, size_t};
use std::{ffi::CString, ptr};

pub fn minimal_gif() -> Vec<u8> {
    vec![
        0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 0x01, 0x00, 0x01, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00,
        0x00, 0xff, 0xff, 0xff, 0x2c, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x02,
        0x02, 0x44, 0x01, 0x00, 0x3b,
    ]
}

pub fn apng_header(frames: u32) -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(b"\x89PNG\r\n\x1a\n");
    data.extend_from_slice(&13_u32.to_be_bytes());
    data.extend_from_slice(b"IHDR");
    data.extend_from_slice(&2_u32.to_be_bytes());
    data.extend_from_slice(&3_u32.to_be_bytes());
    data.extend_from_slice(&[8, 6, 0, 0, 0]);
    data.extend_from_slice(&[0; 4]);
    data.extend_from_slice(&8_u32.to_be_bytes());
    data.extend_from_slice(b"acTL");
    data.extend_from_slice(&frames.to_be_bytes());
    data.extend_from_slice(&0_u32.to_be_bytes());
    data.extend_from_slice(&[0; 4]);
    data
}

pub fn synthetic_png(width: u32, height: u32) -> Vec<u8> {
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
    unsafe { native::buffer::NativeBuffer::from_vips_owned(out_ptr, out_size) }
        .unwrap()
        .try_to_vec()
        .unwrap()
}

pub fn synthetic_bmp(width: u32, height: u32) -> Vec<u8> {
    assert!(width > 0 && height > 0);
    let row_bytes = (width as usize * 3).next_multiple_of(4);
    let pixel_bytes = row_bytes * height as usize;
    let file_bytes = 54 + pixel_bytes;
    let mut bytes = Vec::with_capacity(file_bytes);
    bytes.extend_from_slice(b"BM");
    bytes.extend_from_slice(&(file_bytes as u32).to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes());
    bytes.extend_from_slice(&54u32.to_le_bytes());
    bytes.extend_from_slice(&40u32.to_le_bytes());
    bytes.extend_from_slice(&(width as i32).to_le_bytes());
    bytes.extend_from_slice(&(height as i32).to_le_bytes());
    bytes.extend_from_slice(&1u16.to_le_bytes());
    bytes.extend_from_slice(&24u16.to_le_bytes());
    bytes.extend_from_slice(&0u32.to_le_bytes());
    bytes.extend_from_slice(&(pixel_bytes as u32).to_le_bytes());
    bytes.extend_from_slice(&2835i32.to_le_bytes());
    bytes.extend_from_slice(&2835i32.to_le_bytes());
    bytes.extend_from_slice(&0u32.to_le_bytes());
    bytes.extend_from_slice(&0u32.to_le_bytes());
    for y in (0..height).rev() {
        let mut row = Vec::with_capacity(row_bytes);
        for x in 0..width {
            row.push(120);
            row.push((y * 255 / height) as u8);
            row.push((x * 255 / width) as u8);
        }
        row.resize(row_bytes, 0);
        bytes.extend_from_slice(&row);
    }
    bytes
}

pub fn animated_gif_fixture() -> Vec<u8> {
    base64::engine::general_purpose::STANDARD
        .decode("R0lGODlhIAAgAPEAAAAAAP8AAP///wAAACH/C05FVFNDQVBFMi4wAwEAAAAh+QQJFAAAACwAAAAAIAAgAAACPYyPGcDtD5Q8sLY5rcVSV654EJiID4mYDkqpDGu4LyxHtAwv+O3mtb9j1YbEovGITCqXzKbzCY1Kp9QqsQAAIfkECRQAAAAsAAAAACAAIACDAAAAAAD/AAD/AAD/AAD/AAD/AAD/AAD/AAD/AAD/////AAAAAAAAAAAAAAAAAAAABFsQyEmrvTjrzbv/YCiOZGmeaKqubOu+MBUIA1EQgxCYBpH8wATBQDoEj7+DyIBsEj8BX/NIACGmTdAAiwRJuUBQARz0ksOf7TlhXbOhX24VxCQ/QUauctRrDiURADs=")
        .unwrap()
}

pub fn animated_gif_frames(frames: usize, delay_cs: u16) -> Vec<u8> {
    let mut gif = Vec::new();
    gif.extend_from_slice(b"GIF89a");
    gif.extend_from_slice(&16u16.to_le_bytes());
    gif.extend_from_slice(&16u16.to_le_bytes());
    gif.extend_from_slice(&[0x80, 0x00, 0x00]);
    gif.extend_from_slice(&[0x00, 0x00, 0x00, 0xff, 0xff, 0xff]);
    gif.extend_from_slice(b"\x21\xff\x0bNETSCAPE2.0");
    gif.extend_from_slice(&[0x03, 0x01, 0x00, 0x00, 0x00]);
    for index in 0..frames {
        gif.extend_from_slice(&[0x21, 0xf9, 0x04, 0x00]);
        gif.extend_from_slice(&delay_cs.to_le_bytes());
        gif.extend_from_slice(&[0x00, 0x00]);
        gif.extend_from_slice(&[0x2c, 0x00, 0x00, 0x00, 0x00]);
        gif.extend_from_slice(&1u16.to_le_bytes());
        gif.extend_from_slice(&1u16.to_le_bytes());
        gif.extend_from_slice(&[0x00, 0x02, 0x02]);
        gif.extend_from_slice(if index % 2 == 0 {
            &[0x44, 0x01]
        } else {
            &[0x4c, 0x01]
        });
        gif.push(0x00);
    }
    gif.push(0x3b);
    gif
}

pub fn read_u24_le(bytes: &[u8]) -> Option<u32> {
    (bytes.len() >= 3)
        .then(|| bytes[0] as u32 | ((bytes[1] as u32) << 8) | ((bytes[2] as u32) << 16))
}

pub fn webp_chunk_payloads<'a>(bytes: &'a [u8], fourcc: &[u8; 4]) -> Vec<&'a [u8]> {
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

pub fn webp_canvas_size(bytes: &[u8]) -> Option<(u32, u32, u8)> {
    let vp8x = webp_chunk_payloads(bytes, b"VP8X").into_iter().next()?;
    if vp8x.len() < 10 {
        return None;
    }
    let width = read_u24_le(&vp8x[4..7])? + 1;
    let height = read_u24_le(&vp8x[7..10])? + 1;
    Some((width, height, vp8x[0]))
}

pub fn first_webp_anim_frame_size(bytes: &[u8]) -> Option<(u32, u32)> {
    let anmf = webp_chunk_payloads(bytes, b"ANMF").into_iter().next()?;
    if anmf.len() < 16 {
        return None;
    }
    let width = read_u24_le(&anmf[6..9])? + 1;
    let height = read_u24_le(&anmf[9..12])? + 1;
    Some((width, height))
}

pub fn webp_animation_loop_count(bytes: &[u8]) -> Option<u16> {
    let anim = webp_chunk_payloads(bytes, b"ANIM").into_iter().next()?;
    (anim.len() >= 6).then(|| u16::from_le_bytes([anim[4], anim[5]]))
}

pub fn webp_with_metadata_chunk(bytes: &[u8], fourcc: &[u8; 4], flag: u8) -> Option<Vec<u8>> {
    if bytes.len() < 20 || &bytes[0..4] != b"RIFF" || &bytes[8..12] != b"WEBP" {
        return None;
    }
    if &bytes[12..16] != b"VP8X" {
        return None;
    }
    let vp8x_size = u32::from_le_bytes([bytes[16], bytes[17], bytes[18], bytes[19]]) as usize;
    let vp8x_end = 20usize.checked_add(vp8x_size + (vp8x_size & 1))?;
    if vp8x_end > bytes.len() {
        return None;
    }
    let payload: [u8; 4] = [0x00, 0x01, 0x02, 0x03];
    let mut out = Vec::with_capacity(bytes.len() + 12);
    out.extend_from_slice(&bytes[..vp8x_end]);
    out[20] |= flag;
    out.extend_from_slice(fourcc);
    out.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    out.extend_from_slice(&payload);
    out.extend_from_slice(&bytes[vp8x_end..]);
    let riff_size = u32::try_from(out.len() - 8).ok()?;
    out[4..8].copy_from_slice(&riff_size.to_le_bytes());
    Some(out)
}

pub fn gif_frame_delays_cs(bytes: &[u8]) -> Vec<u16> {
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

pub fn gif_loop_count(bytes: &[u8]) -> Option<u16> {
    if bytes.len() < 13 || (&bytes[..6] != b"GIF89a" && &bytes[..6] != b"GIF87a") {
        return None;
    }
    let mut offset = 13usize;
    if bytes[10] & 0x80 != 0 {
        let entries = 1usize << ((bytes[10] & 0x07) + 1);
        offset = offset.saturating_add(entries.saturating_mul(3));
    }
    while offset + 19 <= bytes.len() {
        if bytes[offset] == 0x21
            && bytes[offset + 1] == 0xff
            && bytes[offset + 2] == 11
            && (&bytes[offset + 3..offset + 14] == b"NETSCAPE2.0"
                || &bytes[offset + 3..offset + 14] == b"ANIMEXTS1.0")
            && bytes[offset + 14] == 3
            && bytes[offset + 15] == 1
        {
            return Some(u16::from_le_bytes([bytes[offset + 16], bytes[offset + 17]]));
        }
        offset += 1;
    }
    None
}

pub fn png_dimensions(bytes: &[u8]) -> Option<(u32, u32)> {
    if bytes.len() < 24 || &bytes[..8] != b"\x89PNG\r\n\x1a\n" || &bytes[12..16] != b"IHDR" {
        return None;
    }
    let width = u32::from_be_bytes([bytes[16], bytes[17], bytes[18], bytes[19]]);
    let height = u32::from_be_bytes([bytes[20], bytes[21], bytes[22], bytes[23]]);
    Some((width, height))
}
