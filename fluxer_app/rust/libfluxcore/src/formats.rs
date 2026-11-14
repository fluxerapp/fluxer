// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn is_animated_image_bytes(input: &[u8]) -> bool {
    if is_gif(input) {
        return is_animated_gif(input);
    }
    if is_png(input) {
        return has_apng_actl(input);
    }
    if is_webp(input) {
        return has_webp_anim(input);
    }
    if is_avif_file(input) {
        return has_avif_anim(input);
    }
    false
}

pub fn sniff_image_format_bytes(input: &[u8]) -> u8 {
    if is_png(input) {
        1
    } else if is_gif(input) {
        2
    } else if is_webp(input) {
        3
    } else if is_avif_file(input) {
        4
    } else if input.starts_with(&[0xff, 0xd8, 0xff]) {
        5
    } else {
        0
    }
}

fn is_gif(input: &[u8]) -> bool {
    input.starts_with(b"GIF89a") || input.starts_with(b"GIF87a")
}

fn is_png(input: &[u8]) -> bool {
    input.starts_with(b"\x89PNG\r\n\x1a\n")
}

fn is_webp(input: &[u8]) -> bool {
    input.len() >= 12 && &input[0..4] == b"RIFF" && &input[8..12] == b"WEBP"
}

fn is_avif_file(input: &[u8]) -> bool {
    input.len() >= 12
        && &input[4..8] == b"ftyp"
        && (&input[8..12] == b"avif" || &input[8..12] == b"avis")
}

fn has_avif_anim(input: &[u8]) -> bool {
    if !is_avif_file(input) {
        return false;
    }
    if &input[8..12] == b"avis" {
        return true;
    }
    let box_end = read_u32_be(input, 0).map_or(0, |size| (size as usize).min(input.len()));
    input
        .get(16..box_end)
        .is_some_and(|brands| brands.as_chunks::<4>().0.contains(b"avis"))
}

fn has_apng_actl(input: &[u8]) -> bool {
    if !is_png(input) {
        return false;
    }

    let mut offset = 8usize;
    while offset + 12 <= input.len() {
        let Some(length) =
            read_u32_be(input, offset).and_then(|length| usize::try_from(length).ok())
        else {
            return false;
        };
        let chunk_type = &input[offset + 4..offset + 8];
        if chunk_type == b"acTL" {
            return true;
        }
        let Some(next_offset) = offset
            .checked_add(12)
            .and_then(|value| value.checked_add(length))
        else {
            return false;
        };
        if next_offset > input.len() {
            return false;
        }
        offset = next_offset;
    }
    false
}

fn has_webp_anim(input: &[u8]) -> bool {
    if !is_webp(input) {
        return false;
    }

    let mut offset = 12usize;
    while offset + 8 <= input.len() {
        let chunk_id = &input[offset..offset + 4];
        let Some(size) = read_u32_le(input, offset + 4).and_then(|size| usize::try_from(size).ok())
        else {
            return false;
        };
        if chunk_id == b"ANIM" {
            return true;
        }
        let padding = size % 2;
        let Some(next_offset) = offset
            .checked_add(8)
            .and_then(|value| value.checked_add(size))
            .and_then(|value| value.checked_add(padding))
        else {
            return false;
        };
        if next_offset > input.len() {
            return false;
        }
        offset = next_offset;
    }
    false
}

fn skip_gif_sub_blocks(input: &[u8], offset: &mut usize) -> bool {
    while *offset < input.len() {
        let size = input[*offset] as usize;
        *offset += 1;
        if size == 0 {
            return true;
        }
        let Some(next_offset) = offset.checked_add(size) else {
            return false;
        };
        if next_offset > input.len() {
            return false;
        }
        *offset = next_offset;
    }
    false
}

fn is_animated_gif(input: &[u8]) -> bool {
    if !is_gif(input) || input.len() < 13 {
        return false;
    }

    let mut offset = 13usize;
    let flags = input[10];
    if flags & 0x80 != 0 {
        let table_size = 3usize.saturating_mul(1usize << ((flags & 0x07) + 1));
        let Some(next_offset) = offset.checked_add(table_size) else {
            return false;
        };
        if next_offset > input.len() {
            return false;
        }
        offset = next_offset;
    }

    let mut frame_count = 0u32;
    while offset < input.len() {
        let block = input[offset];
        offset += 1;
        match block {
            0x2c => {
                if offset + 9 > input.len() {
                    return false;
                }
                let descriptor_packed = input[offset + 8];
                offset += 9;
                if descriptor_packed & 0x80 != 0 {
                    let table_size =
                        3usize.saturating_mul(1usize << ((descriptor_packed & 0x07) + 1));
                    let Some(next_offset) = offset.checked_add(table_size) else {
                        return false;
                    };
                    if next_offset > input.len() {
                        return false;
                    }
                    offset = next_offset;
                }
                if offset >= input.len() {
                    return false;
                }
                offset += 1;
                if !skip_gif_sub_blocks(input, &mut offset) {
                    return false;
                }
                frame_count += 1;
                if frame_count > 1 {
                    return true;
                }
            }
            0x21 => {
                if offset >= input.len() {
                    return false;
                }
                offset += 1;
                if !skip_gif_sub_blocks(input, &mut offset) {
                    return false;
                }
            }
            0x3b => return false,
            _ => return false,
        }
    }
    false
}

fn read_u32_le(input: &[u8], offset: usize) -> Option<u32> {
    let bytes = input.get(offset..offset + 4)?;
    Some(u32::from_le_bytes(bytes.try_into().ok()?))
}

fn read_u32_be(input: &[u8], offset: usize) -> Option<u32> {
    let bytes = input.get(offset..offset + 4)?;
    Some(u32::from_be_bytes(bytes.try_into().ok()?))
}
