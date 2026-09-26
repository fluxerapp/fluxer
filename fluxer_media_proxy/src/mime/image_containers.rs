// SPDX-License-Identifier: AGPL-3.0-or-later

use super::SniffInfo;
use crate::media_type::MediaType;

const WEBP_CHUNK_HEADER_BYTES: usize = 20;
const WEBP_EXTENDED_HEADER_BYTES: usize = 30;
const WEBP_LOSSLESS_HEADER_BYTES: usize = 25;
const GIF_LOGICAL_SCREEN_BYTES: usize = 13;
const GIF_IMAGE_DESCRIPTOR_BYTES: usize = 9;
const GIF_STRUCTURE_BLOCK_LIMIT: usize = 262_144;
const PNG_SNIFF_CHUNK_LIMIT: usize = 262_144;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum GIFAnimation {
    Incomplete,
    Static,
    Animated,
    Invalid,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum PNGAnimation {
    Incomplete,
    Static,
    Animated(u32),
    Invalid,
}

fn gif_color_table_end(data: &[u8], offset: usize, packed: u8) -> Option<usize> {
    assert!(offset <= data.len());
    if packed & 0x80 == 0 {
        return Some(offset);
    }
    let entries = 1usize << ((packed & 0x07) + 1);
    let table_bytes = entries
        .checked_mul(3)
        .expect("GIF color table byte count must fit usize");
    let end = offset.checked_add(table_bytes)?;
    (end <= data.len()).then_some(end)
}

fn gif_u16(data: &[u8], offset: usize) -> u16 {
    assert!(offset <= data.len());
    assert!(data.len() - offset >= 2);
    u16::from_le_bytes(
        data[offset..offset + 2]
            .try_into()
            .expect("validated GIF u16 slice"),
    )
}

fn gif_validate_image_geometry(
    data: &[u8],
    offset: usize,
    canvas_width: u16,
    canvas_height: u16,
) -> Result<(), GIFAnimation> {
    assert!(offset <= data.len());
    assert!(data.len() - offset >= GIF_IMAGE_DESCRIPTOR_BYTES);
    let left = gif_u16(data, offset);
    let top = gif_u16(data, offset + 2);
    let width = gif_u16(data, offset + 4);
    let height = gif_u16(data, offset + 6);
    if width == 0 {
        return Err(GIFAnimation::Invalid);
    }
    if height == 0 {
        return Err(GIFAnimation::Invalid);
    }
    if u32::from(left) + u32::from(width) > u32::from(canvas_width) {
        return Err(GIFAnimation::Invalid);
    }
    if u32::from(top) + u32::from(height) > u32::from(canvas_height) {
        return Err(GIFAnimation::Invalid);
    }
    Ok(())
}

fn gif_skip_sub_blocks(
    data: &[u8],
    mut offset: usize,
    block_count: &mut usize,
) -> Result<usize, GIFAnimation> {
    assert!(offset <= data.len());
    loop {
        if *block_count >= GIF_STRUCTURE_BLOCK_LIMIT {
            return Err(GIFAnimation::Invalid);
        }
        let Some(&length) = data.get(offset) else {
            return Err(GIFAnimation::Incomplete);
        };
        *block_count += 1;
        offset += 1;
        if length == 0 {
            return Ok(offset);
        }
        let Some(end) = offset.checked_add(usize::from(length)) else {
            return Err(GIFAnimation::Invalid);
        };
        if end > data.len() {
            return Err(GIFAnimation::Incomplete);
        }
        offset = end;
    }
}

fn gif_skip_image(
    data: &[u8],
    offset: usize,
    canvas_width: u16,
    canvas_height: u16,
    block_count: &mut usize,
) -> Result<usize, GIFAnimation> {
    assert!(offset <= data.len());
    let Some(descriptor_end) = offset.checked_add(GIF_IMAGE_DESCRIPTOR_BYTES) else {
        return Err(GIFAnimation::Invalid);
    };
    if descriptor_end > data.len() {
        return Err(GIFAnimation::Incomplete);
    }
    gif_validate_image_geometry(data, offset, canvas_width, canvas_height)?;
    let packed = data[offset + 8];
    let Some(table_end) = gif_color_table_end(data, descriptor_end, packed) else {
        return Err(GIFAnimation::Incomplete);
    };
    let Some(&minimum_code_size) = data.get(table_end) else {
        return Err(GIFAnimation::Incomplete);
    };
    if !(2..=8).contains(&minimum_code_size) {
        return Err(GIFAnimation::Invalid);
    }
    gif_skip_sub_blocks(data, table_end + 1, block_count)
}

pub(super) fn gif_animation(data: &[u8]) -> GIFAnimation {
    if data.len() < 6 {
        return GIFAnimation::Incomplete;
    }
    match &data[..6] {
        b"GIF87a" | b"GIF89a" => {}
        _ => return GIFAnimation::Invalid,
    }
    if data.len() < GIF_LOGICAL_SCREEN_BYTES {
        return GIFAnimation::Incomplete;
    }
    let canvas_width = gif_u16(data, 6);
    let canvas_height = gif_u16(data, 8);
    if canvas_width == 0 {
        return GIFAnimation::Invalid;
    }
    if canvas_height == 0 {
        return GIFAnimation::Invalid;
    }
    let Some(mut offset) = gif_color_table_end(data, GIF_LOGICAL_SCREEN_BYTES, data[10]) else {
        return GIFAnimation::Incomplete;
    };
    let mut image_count = 0usize;
    let mut block_count = 0usize;
    loop {
        if block_count >= GIF_STRUCTURE_BLOCK_LIMIT {
            return GIFAnimation::Invalid;
        }
        let Some(&introducer) = data.get(offset) else {
            return GIFAnimation::Incomplete;
        };
        block_count += 1;
        offset += 1;
        match introducer {
            0x3b if image_count == 1 => return GIFAnimation::Static,
            0x3b => return GIFAnimation::Invalid,
            0x21 => {
                if data.get(offset).is_none() {
                    return GIFAnimation::Incomplete;
                }
                offset = match gif_skip_sub_blocks(data, offset + 1, &mut block_count) {
                    Ok(next) => next,
                    Err(outcome) => return outcome,
                };
            }
            0x2c => {
                offset = match gif_skip_image(
                    data,
                    offset,
                    canvas_width,
                    canvas_height,
                    &mut block_count,
                ) {
                    Ok(next) => next,
                    Err(outcome) => return outcome,
                };
                image_count += 1;
                if image_count == 2 {
                    return GIFAnimation::Animated;
                }
            }
            _ => return GIFAnimation::Invalid,
        }
    }
}

pub(super) fn png_animation(data: &[u8]) -> PNGAnimation {
    if data.len() < 8 {
        return PNGAnimation::Incomplete;
    }
    if &data[..8] != b"\x89PNG\r\n\x1a\n" {
        return PNGAnimation::Invalid;
    }
    let mut offset = 8usize;
    let mut first_chunk = true;
    let mut chunk_count = 0usize;
    loop {
        if chunk_count >= PNG_SNIFF_CHUNK_LIMIT {
            return PNGAnimation::Invalid;
        }
        let Some(length_end) = offset.checked_add(4) else {
            return PNGAnimation::Invalid;
        };
        let Some(header_end) = offset.checked_add(8) else {
            return PNGAnimation::Invalid;
        };
        if header_end > data.len() {
            break;
        }
        chunk_count += 1;
        let length_bytes = data[offset..length_end]
            .try_into()
            .expect("validated PNG chunk length slice");
        let length = usize::try_from(u32::from_be_bytes(length_bytes))
            .expect("PNG chunk length must fit the configured platform");
        let kind = &data[length_end..header_end];
        if first_chunk {
            if kind != b"IHDR" {
                return PNGAnimation::Invalid;
            }
            if length != 13 {
                return PNGAnimation::Invalid;
            }
        }
        first_chunk = false;
        if kind == b"acTL" {
            if length != 8 {
                return PNGAnimation::Invalid;
            }
            let Some(chunk_end) = offset.checked_add(16) else {
                return PNGAnimation::Invalid;
            };
            if chunk_end > data.len() {
                return PNGAnimation::Incomplete;
            }
            let frame_end = header_end
                .checked_add(4)
                .expect("validated PNG animation frame count end");
            let frame_bytes = data[header_end..frame_end]
                .try_into()
                .expect("validated PNG animation frame count slice");
            let frames = u32::from_be_bytes(frame_bytes);
            return if frames > 0 {
                PNGAnimation::Animated(frames)
            } else {
                PNGAnimation::Invalid
            };
        }
        match kind {
            b"IDAT" | b"IEND" => return PNGAnimation::Static,
            _ => {}
        }
        let Some(next) = offset
            .checked_add(12)
            .and_then(|next| next.checked_add(length))
        else {
            return PNGAnimation::Invalid;
        };
        if next > data.len() {
            return PNGAnimation::Incomplete;
        }
        offset = next;
    }
    PNGAnimation::Incomplete
}

fn webp_u24(data: &[u8]) -> u32 {
    u32::from(data[0]) | (u32::from(data[1]) << 8) | (u32::from(data[2]) << 16)
}

pub(super) fn webp_sniff(data: &[u8]) -> SniffInfo {
    let mut out = SniffInfo {
        mime: MediaType::WebP.mime(),
        ..Default::default()
    };
    if data.len() < WEBP_CHUNK_HEADER_BYTES {
        return out;
    }
    match &data[12..16] {
        b"VP8X" => {
            let chunk_size = u32::from_le_bytes(
                data[16..20]
                    .try_into()
                    .expect("validated WebP chunk size slice"),
            );
            if chunk_size != 10 || data.len() < WEBP_EXTENDED_HEADER_BYTES {
                return out;
            }
            let flags = data[20];
            out.animated = flags & 0x02 != 0;
            out.frames = if out.animated { 2 } else { 1 };
            out.has_alpha = flags & 0x10 != 0;
            out.width = webp_u24(&data[24..27])
                .checked_add(1)
                .expect("24-bit WebP width must fit u32");
            out.height = webp_u24(&data[27..30])
                .checked_add(1)
                .expect("24-bit WebP height must fit u32");
        }
        b"VP8 " if data.len() >= WEBP_EXTENDED_HEADER_BYTES => {
            if &data[23..26] == b"\x9d\x01\x2a" {
                out.width = u32::from(
                    u16::from_le_bytes(
                        data[26..28].try_into().expect("validated WebP width slice"),
                    ) & 0x3fff,
                );
                out.height = u32::from(
                    u16::from_le_bytes(
                        data[28..30]
                            .try_into()
                            .expect("validated WebP height slice"),
                    ) & 0x3fff,
                );
            }
        }
        b"VP8L" if data.len() >= WEBP_LOSSLESS_HEADER_BYTES && data[20] == 0x2f => {
            let bits = u32::from_le_bytes(
                data[21..25]
                    .try_into()
                    .expect("validated lossless WebP dimensions slice"),
            );
            out.width = (bits & 0x3fff)
                .checked_add(1)
                .expect("14-bit WebP width must fit u32");
            out.height = ((bits >> 14) & 0x3fff)
                .checked_add(1)
                .expect("14-bit WebP height must fit u32");
            out.has_alpha = bits & (1 << 28) != 0;
        }
        _ => {}
    }
    out
}

pub(super) fn webp_sniff_complete(data: &[u8]) -> bool {
    if data.len() < WEBP_CHUNK_HEADER_BYTES {
        return false;
    }
    match &data[12..16] {
        b"VP8X" => {
            let chunk_size = u32::from_le_bytes(
                data[16..20]
                    .try_into()
                    .expect("validated WebP animation chunk size slice"),
            );
            chunk_size != 10 || data.len() >= WEBP_EXTENDED_HEADER_BYTES
        }
        b"VP8 " => data.len() >= WEBP_EXTENDED_HEADER_BYTES,
        b"VP8L" => data.len() >= WEBP_LOSSLESS_HEADER_BYTES,
        _ => true,
    }
}
