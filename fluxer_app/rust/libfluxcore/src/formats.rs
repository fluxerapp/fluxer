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
    if is_svg(input) {
        return has_smil_animation(input);
    }
    false
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
    is_avif_file(input) && &input[8..12] == b"avis"
}

/// Returns true if the bytes look like an SVG document.
///
/// Checks for the XML/SVG prologs that all conforming SVG files begin with.
fn is_svg(input: &[u8]) -> bool {
    // Skip optional UTF-8 BOM
    let input = input.strip_prefix(b"\xef\xbb\xbf").unwrap_or(input);
    // Skip optional XML declaration
    let input = if input.starts_with(b"<?xml") {
        match find_bytes(input, b"?>") {
            Some(end) => &input[end + 2..],
            None => return false,
        }
    } else {
        input
    };
    let input = skip_whitespace(input);
    input.starts_with(b"<svg") || input.starts_with(b"<!DOCTYPE svg")
}

/// Returns true if the SVG bytes contain at least one SMIL animation element.
///
/// SMIL animation is indicated by the presence of any of the four standard
/// animation elements defined in SVG 1.1 § 19: `<animate>`,
/// `<animateTransform>`, `<animateMotion>`, and `<set>`.
fn has_smil_animation(input: &[u8]) -> bool {
    // We perform byte-level substring searches — no XML parser needed.
    // Each pattern is the opening tag prefix of a SMIL element.  A match
    // anywhere in the file is sufficient to classify the image as animated.
    const PATTERNS: &[&[u8]] = &[
        b"<animate ",
        b"<animate>",
        b"<animate\t",
        b"<animate\n",
        b"<animate\r",
        b"<animateTransform",
        b"<animateMotion",
        b"<set ",
        b"<set>",
        b"<set\t",
        b"<set\n",
        b"<set\r",
    ];
    for pattern in PATTERNS {
        if find_bytes(input, pattern).is_some() {
            return true;
        }
    }
    false
}

/// Returns the byte offset of the first occurrence of `needle` in `haystack`,
/// or `None` if it is not present.
fn find_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    if needle.is_empty() {
        return Some(0);
    }
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

fn skip_whitespace(input: &[u8]) -> &[u8] {
    let pos = input
        .iter()
        .position(|&b| !b.is_ascii_whitespace())
        .unwrap_or(input.len());
    &input[pos..]
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

#[cfg(test)]
mod tests {
    use super::*;

    fn png_with_chunks(chunks: &[(&[u8; 4], &[u8])]) -> Vec<u8> {
        let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
        for (chunk_type, payload) in chunks {
            out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
            out.extend_from_slice(*chunk_type);
            out.extend_from_slice(payload);
            out.extend_from_slice(&0u32.to_be_bytes());
        }
        out
    }

    #[test]
    fn detects_apng_animation_chunk() {
        let png = png_with_chunks(&[(b"acTL", &[0; 8])]);
        assert!(is_animated_image_bytes(&png));
    }

    #[test]
    fn detects_webp_animation_chunk() {
        let mut webp = b"RIFF\x12\x00\x00\x00WEBPVP8X\x00\x00\x00\x00ANIM\x00\x00\x00\x00".to_vec();
        webp[4..8].copy_from_slice(&18u32.to_le_bytes());
        assert!(is_animated_image_bytes(&webp));
    }

    #[test]
    fn detects_two_frame_gif() {
        let gif = [
            0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 1, 0, 1, 0, 0, 0, 0, 0x2c, 0, 0, 0, 0, 1, 0, 1, 0,
            0, 2, 0, 0x2c, 0, 0, 0, 0, 1, 0, 1, 0, 0, 2, 0, 0x3b,
        ];
        assert!(is_animated_image_bytes(&gif));
    }

    #[test]
    fn treats_single_frame_gif_as_static() {
        let gif = [
            0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 1, 0, 1, 0, 0, 0, 0, 0x2c, 0, 0, 0, 0, 1, 0, 1, 0,
            0, 2, 0, 0x3b,
        ];
        assert!(!is_animated_image_bytes(&gif));
    }

    #[test]
    fn detects_avif_sequence_brand() {
        let avif = b"\x00\x00\x00\x18ftypavif\x00\x00\x00\x00avis";
        assert!(!is_animated_image_bytes(avif));

        let avis = b"\x00\x00\x00\x18ftypavis\x00\x00\x00\x00avif";
        assert!(is_animated_image_bytes(avis));
    }

    #[test]
    fn rejects_truncated_chunks_without_panicking() {
        assert!(!is_animated_image_bytes(
            b"\x89PNG\r\n\x1a\n\xff\xff\xff\xffbad!"
        ));
        assert!(!is_animated_image_bytes(b"RIFF\xff\xff\xff\xffWEBPbad!"));
    }

    #[test]
    fn detects_svg_with_animate_element() {
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">
  <circle r="10" cx="50" cy="50">
    <animate attributeName="r" from="10" to="40" dur="1s" repeatCount="indefinite"/>
  </circle>
</svg>"#;
        assert!(is_animated_image_bytes(svg));
    }

    #[test]
    fn detects_svg_with_animate_transform() {
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg">
  <rect width="50" height="50">
    <animateTransform attributeName="transform" type="rotate" from="0" to="360" dur="2s" repeatCount="indefinite"/>
  </rect>
</svg>"#;
        assert!(is_animated_image_bytes(svg));
    }

    #[test]
    fn detects_svg_with_animate_motion() {
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg">
  <circle r="5">
    <animateMotion path="M 0 0 L 100 100" dur="1s" repeatCount="indefinite"/>
  </circle>
</svg>"#;
        assert!(is_animated_image_bytes(svg));
    }

    #[test]
    fn detects_svg_with_set_element() {
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg">
  <circle r="10" cx="50" cy="50">
    <set attributeName="fill" to="red" begin="2s"/>
  </circle>
</svg>"#;
        assert!(is_animated_image_bytes(svg));
    }

    #[test]
    fn treats_static_svg_as_non_animated() {
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">
  <circle r="10" cx="50" cy="50" fill="blue"/>
</svg>"#;
        assert!(!is_animated_image_bytes(svg));
    }

    #[test]
    fn detects_svg_with_xml_declaration() {
        let svg = br#"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg">
  <rect><animate attributeName="x" from="0" to="100" dur="1s"/></rect>
</svg>"#;
        assert!(is_animated_image_bytes(svg));
    }

    #[test]
    fn detects_svg_with_utf8_bom() {
        let mut svg = b"\xef\xbb\xbf".to_vec();
        svg.extend_from_slice(br#"<svg xmlns="http://www.w3.org/2000/svg">
  <circle><animate attributeName="r" from="5" to="20" dur="1s"/></circle>
</svg>"#);
        assert!(is_animated_image_bytes(&svg));
    }

    #[test]
    fn non_svg_xml_is_not_animated() {
        let xml = br#"<?xml version="1.0"?><root><animate attributeName="x" from="0" to="1"/></root>"#;
        assert!(!is_animated_image_bytes(xml));
    }
}
