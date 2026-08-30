// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::constants::AssetExtension;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Category {
    Image,
    Video,
    Audio,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SniffInfo {
    pub mime: &'static str,
    pub animated: bool,
    pub frames: u32,
    pub duration_ms: u32,
    pub width: u32,
    pub height: u32,
    pub has_alpha: bool,
    pub color_space: &'static str,
}

impl Default for SniffInfo {
    fn default() -> Self {
        Self {
            mime: "application/octet-stream",
            animated: false,
            frames: 1,
            duration_ms: 0,
            width: 0,
            height: 0,
            has_alpha: false,
            color_space: "unknown",
        }
    }
}

pub fn normalize(raw: Option<&str>) -> Option<&str> {
    let value = raw?;
    let semi = value.find(';').unwrap_or(value.len());
    let trimmed = value[..semi].trim_matches([' ', '\t']);
    (!trimmed.is_empty()).then_some(trimmed)
}

pub fn category(mime_type: &str) -> Option<Category> {
    if mime_type.starts_with("image/") {
        Some(Category::Image)
    } else if mime_type.starts_with("video/") {
        Some(Category::Video)
    } else if mime_type.starts_with("audio/") {
        Some(Category::Audio)
    } else {
        None
    }
}

pub fn is_supported_media_mime(mime_type_raw: &str) -> bool {
    let Some(mime_type) = normalize(Some(mime_type_raw)) else {
        return false;
    };
    matches!(
        mime_type,
        "image/jpeg"
            | "image/png"
            | "image/apng"
            | "image/gif"
            | "image/webp"
            | "image/avif"
            | "image/heic"
            | "image/heif"
            | "image/jxl"
            | "image/svg+xml"
            | "image/tiff"
            | "image/bmp"
            | "video/mp4"
            | "video/webm"
            | "video/quicktime"
            | "video/3gpp"
            | "video/x-matroska"
            | "video/x-msvideo"
            | "video/x-flv"
            | "video/ogg"
            | "video/mp2t"
            | "video/mpeg"
            | "video/x-ms-wmv"
            | "audio/mpeg"
            | "audio/wav"
            | "audio/flac"
            | "audio/ogg"
            | "audio/aac"
            | "audio/mp4"
            | "audio/webm"
            | "audio/aiff"
    )
}

pub fn extension_mime(filename: &str) -> Option<&'static str> {
    let ext = filename.rsplit_once('.')?.1;
    if let Some(image_ext) = AssetExtension::parse(ext) {
        return Some(image_ext.mime());
    }
    match ext.to_ascii_lowercase().as_str() {
        "mp4" | "m4v" => Some("video/mp4"),
        "webm" => Some("video/webm"),
        "mov" => Some("video/quicktime"),
        "ogv" => Some("video/ogg"),
        "mkv" => Some("video/x-matroska"),
        "3gp" => Some("video/3gpp"),
        "avi" => Some("video/x-msvideo"),
        "flv" => Some("video/x-flv"),
        "ts" => Some("video/mp2t"),
        "mpg" | "mpeg" => Some("video/mpeg"),
        "wmv" => Some("video/x-ms-wmv"),
        "mp3" => Some("audio/mpeg"),
        "m4a" | "m4b" => Some("audio/mp4"),
        "ogg" | "oga" | "opus" => Some("audio/ogg"),
        "aac" => Some("audio/aac"),
        "weba" => Some("audio/webm"),
        "aiff" | "aif" => Some("audio/aiff"),
        "flac" => Some("audio/flac"),
        "wav" => Some("audio/wav"),
        "tif" | "tiff" => Some("image/tiff"),
        "bmp" => Some("image/bmp"),
        "css" => Some("text/css; charset=utf-8"),
        _ => None,
    }
}

fn starts(data: &[u8], prefix: &[u8]) -> bool {
    data.len() >= prefix.len() && &data[..prefix.len()] == prefix
}

pub fn sniff(data: &[u8]) -> SniffInfo {
    if starts(data, b"\x89PNG\r\n\x1a\n") {
        let mut out = SniffInfo {
            mime: "image/png",
            has_alpha: true,
            ..Default::default()
        };
        if data.len() >= 26 && &data[12..16] == b"IHDR" {
            out.width = u32::from_be_bytes(data[16..20].try_into().unwrap());
            out.height = u32::from_be_bytes(data[20..24].try_into().unwrap());
            out.has_alpha = data[25] == 4 || data[25] == 6;
        }
        if png_has_actl_before_idat(data) {
            out.mime = "image/apng";
            out.animated = true;
            out.frames = 2;
        }
        return out;
    }
    if starts(data, b"\xff\xd8\xff") {
        return SniffInfo {
            mime: "image/jpeg",
            ..Default::default()
        };
    }
    if starts(data, b"GIF87a") || starts(data, b"GIF89a") {
        let animated = gif_has_netscape_loop(data);
        return SniffInfo {
            mime: "image/gif",
            animated,
            frames: if animated { 2 } else { 1 },
            width: if data.len() >= 10 {
                u16::from_le_bytes(data[6..8].try_into().unwrap()) as u32
            } else {
                0
            },
            height: if data.len() >= 10 {
                u16::from_le_bytes(data[8..10].try_into().unwrap()) as u32
            } else {
                0
            },
            ..Default::default()
        };
    }
    if data.len() >= 12 && starts(data, b"RIFF") && &data[8..12] == b"WEBP" {
        return webp_sniff(data);
    }
    if let Some(info) = iso_bmff_sniff(data) {
        return info;
    }
    if starts(data, b"\xff\x0a") || starts(data, b"\x00\x00\x00\x0cJXL \r\n\x87\n") {
        return SniffInfo {
            mime: "image/jxl",
            ..Default::default()
        };
    }
    if starts(data, b"II*\0") || starts(data, b"MM\0*") {
        return SniffInfo {
            mime: "image/tiff",
            ..Default::default()
        };
    }
    if starts(data, b"BM") {
        return SniffInfo {
            mime: "image/bmp",
            ..Default::default()
        };
    }
    if starts(data, b"\x1a\x45\xdf\xa3") {
        return matroska_sniff(data);
    }
    if starts(data, b"FLV") {
        return SniffInfo {
            mime: "video/x-flv",
            ..Default::default()
        };
    }
    if starts(data, b"RIFF") && data.len() >= 12 && &data[8..12] == b"AVI " {
        return SniffInfo {
            mime: "video/x-msvideo",
            ..Default::default()
        };
    }
    if starts(
        data,
        b"\x30\x26\xb2\x75\x8e\x66\xcf\x11\xa6\xd9\x00\xaa\x00\x62\xce\x6c",
    ) {
        return SniffInfo {
            mime: "video/x-ms-wmv",
            ..Default::default()
        };
    }
    if starts(data, b"\x00\x00\x01\xba") || starts(data, b"\x00\x00\x01\xb3") {
        return SniffInfo {
            mime: "video/mpeg",
            ..Default::default()
        };
    }
    if mpeg_ts_sniff(data) {
        return SniffInfo {
            mime: "video/mp2t",
            ..Default::default()
        };
    }
    if starts(data, b"ID3")
        || starts(data, b"\xff\xfb")
        || starts(data, b"\xff\xf3")
        || starts(data, b"\xff\xf2")
    {
        return SniffInfo {
            mime: "audio/mpeg",
            ..Default::default()
        };
    }
    if starts(data, b"OggS") {
        return ogg_sniff(data);
    }
    if starts(data, b"fLaC") {
        return SniffInfo {
            mime: "audio/flac",
            ..Default::default()
        };
    }
    if starts(data, b"RIFF") && data.len() >= 12 && &data[8..12] == b"WAVE" {
        return SniffInfo {
            mime: "audio/wav",
            ..Default::default()
        };
    }
    if starts(data, b"%PDF-") {
        return SniffInfo {
            mime: "application/pdf",
            ..Default::default()
        };
    }
    if looks_like_svg(data) {
        return SniffInfo {
            mime: "image/svg+xml",
            ..Default::default()
        };
    }
    SniffInfo::default()
}

const PNG_MAX_HEADER_CHUNKS: usize = 64;
const GIF_HEADER_SCAN_LIMIT: usize = 4096;
const WEBP_MAX_HEADER_CHUNKS: usize = 16;

fn png_has_actl_before_idat(data: &[u8]) -> bool {
    let mut offset = 8;
    for _ in 0..PNG_MAX_HEADER_CHUNKS {
        let Some(header) = data.get(offset..).and_then(|rest| rest.get(..8)) else {
            return false;
        };
        match &header[4..8] {
            b"acTL" => return true,
            b"IDAT" | b"IEND" => return false,
            _ => {}
        }
        let length = u32::from_be_bytes(header[0..4].try_into().unwrap()) as usize;
        let Some(next) = offset
            .checked_add(12)
            .and_then(|base| base.checked_add(length))
        else {
            return false;
        };
        offset = next;
    }
    false
}

fn gif_has_netscape_loop(data: &[u8]) -> bool {
    let limit = data.len().min(GIF_HEADER_SCAN_LIMIT);
    let Some(&packed) = data.get(10) else {
        return false;
    };
    let mut offset = 13;
    if packed & 0x80 != 0 {
        offset += 3 << ((packed & 0x07) + 1);
    }
    while offset < limit {
        if data[offset] != 0x21 {
            return false;
        }
        let Some(&label) = data.get(offset + 1) else {
            return false;
        };
        let mut sub = offset + 2;
        if label == 0xff
            && data.get(sub) == Some(&11)
            && data.get(sub + 1..sub + 12) == Some(b"NETSCAPE2.0".as_slice())
        {
            return true;
        }
        loop {
            let Some(&len) = data.get(sub) else {
                return false;
            };
            sub += 1 + len as usize;
            if len == 0 {
                break;
            }
            if sub >= limit {
                return false;
            }
        }
        offset = sub;
    }
    false
}

fn webp_sniff(data: &[u8]) -> SniffInfo {
    let mut out = SniffInfo {
        mime: "image/webp",
        ..Default::default()
    };
    let mut offset = 12;
    for _ in 0..WEBP_MAX_HEADER_CHUNKS {
        let Some(header) = data.get(offset..).and_then(|rest| rest.get(..8)) else {
            break;
        };
        let size = u32::from_le_bytes(header[4..8].try_into().unwrap()) as usize;
        match &header[0..4] {
            b"VP8X" if size >= 10 => {
                if let Some(&flags) = data.get(offset + 8) {
                    out.animated |= flags & 0x02 != 0;
                    out.has_alpha |= flags & 0x10 != 0;
                }
            }
            b"ANIM" => out.animated = true,
            b"ALPH" => out.has_alpha = true,
            b"VP8 " | b"VP8L" | b"ANMF" => break,
            _ => {}
        }
        let Some(next) = offset
            .checked_add(8)
            .and_then(|base| base.checked_add(size))
            .and_then(|base| base.checked_add(size & 1))
        else {
            break;
        };
        offset = next;
    }
    if out.animated {
        out.frames = 2;
    }
    out
}

fn brand_equals(brand: &[u8], literal: &[u8; 4]) -> bool {
    brand.len() == 4 && brand == literal
}

fn iso_bmff_sniff(data: &[u8]) -> Option<SniffInfo> {
    if data.len() < 12 || &data[4..8] != b"ftyp" {
        return None;
    }
    let box_size = u32::from_be_bytes(data[0..4].try_into().unwrap()) as usize;
    let scan_end = if box_size >= 16 && box_size <= data.len() {
        box_size
    } else {
        data.len().min(128)
    };
    let mut saw_avif = false;
    let mut saw_heif = false;
    let mut saw_audio_mp4 = false;
    let mut saw_mp4 = false;
    let mut saw_quicktime = false;
    let mut saw_3gp = false;
    let mut i = 8;
    while i + 4 <= scan_end {
        let brand = &data[i..i + 4];
        if brand_equals(brand, b"avif")
            || brand_equals(brand, b"avis")
            || brand_equals(brand, b"avio")
        {
            saw_avif = true;
        }
        if matches!(
            brand,
            b"heic"
                | b"heix"
                | b"heif"
                | b"heim"
                | b"heis"
                | b"hevc"
                | b"hevx"
                | b"hevm"
                | b"hevs"
                | b"mif1"
                | b"msf1"
        ) {
            saw_heif = true;
        }
        if matches!(
            brand,
            b"mp41"
                | b"mp42"
                | b"isom"
                | b"iso2"
                | b"iso3"
                | b"iso4"
                | b"iso5"
                | b"iso6"
                | b"M4V "
                | b"M4P "
                | b"dash"
                | b"msdh"
                | b"msix"
                | b"mj2s"
        ) {
            saw_mp4 = true;
        }
        if matches!(brand, b"M4A " | b"M4B " | b"M4P ") {
            saw_audio_mp4 = true;
        }
        if brand_equals(brand, b"qt  ") {
            saw_quicktime = true;
        }
        if brand.starts_with(b"3gp") || brand.starts_with(b"3g2") {
            saw_3gp = true;
        }
        i += 4;
    }
    if saw_avif {
        return Some(SniffInfo {
            mime: "image/avif",
            animated: data[8..scan_end].windows(4).any(|w| w == b"avis"),
            ..Default::default()
        });
    }
    if saw_heif {
        return Some(SniffInfo {
            mime: "image/heic",
            ..Default::default()
        });
    }
    if saw_quicktime {
        return Some(SniffInfo {
            mime: "video/quicktime",
            ..Default::default()
        });
    }
    if saw_3gp {
        return Some(SniffInfo {
            mime: "video/3gpp",
            ..Default::default()
        });
    }
    if saw_audio_mp4 {
        return Some(SniffInfo {
            mime: "audio/mp4",
            ..Default::default()
        });
    }
    if saw_mp4 {
        return Some(SniffInfo {
            mime: "video/mp4",
            ..Default::default()
        });
    }
    None
}

fn mpeg_ts_sniff(data: &[u8]) -> bool {
    data.len() >= 188
        && data[0] == 0x47
        && (data.len() < 376 || data[188] == 0x47)
        && (data.len() < 564 || data[376] == 0x47)
}

fn looks_like_svg(data: &[u8]) -> bool {
    let mut window = &data[..data.len().min(4096)];
    if window.starts_with(b"\xef\xbb\xbf") {
        window = &window[3..];
    }
    window.windows(4).any(|w| w == b"<svg")
        && (window.windows(5).any(|w| w == b"xmlns")
            || window.starts_with(b"<svg")
            || window.starts_with(b"<?xml"))
}

fn ogg_sniff(data: &[u8]) -> SniffInfo {
    let window = &data[..data.len().min(8192)];
    if window.windows(6).any(|w| w == b"theora" || w == b"Theora") {
        SniffInfo {
            mime: "video/ogg",
            ..Default::default()
        }
    } else {
        SniffInfo {
            mime: "audio/ogg",
            ..Default::default()
        }
    }
}

fn matroska_sniff(data: &[u8]) -> SniffInfo {
    let window = &data[..data.len().min(4096)];
    if window.windows(4).any(|w| w == b"webm") {
        SniffInfo {
            mime: "video/webm",
            ..Default::default()
        }
    } else {
        SniffInfo {
            mime: "video/x-matroska",
            ..Default::default()
        }
    }
}

pub fn detect(data: &[u8], filename: &str, header_mime: Option<&str>) -> String {
    let sniffed = sniff(data);
    if sniffed.mime != "application/octet-stream" {
        if sniffed.mime == "video/mp4" && extension_mime(filename) == Some("audio/mp4") {
            return "audio/mp4".to_owned();
        }
        return sniffed.mime.to_owned();
    }
    if let Some(m) = extension_mime(filename) {
        return m.to_owned();
    }
    if let Some(m) = normalize(header_mime) {
        return m.to_owned();
    }
    "application/octet-stream".to_owned()
}

pub fn filename_for_mime(mime_type: &str, fallback: &str) -> String {
    if fallback.contains('.') {
        return fallback.to_owned();
    }
    let ext = match mime_type {
        "image/jpeg" => "jpg",
        "image/png" => "png",
        "image/webp" => "webp",
        "image/gif" => "gif",
        "video/mp4" => "mp4",
        _ => "bin",
    };
    format!("{fallback}.{ext}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sniffs_common_image_formats() {
        assert_eq!("image/png", sniff(b"\x89PNG\r\n\x1a\nxxxx").mime);
        assert_eq!("image/jpeg", sniff(b"\xff\xd8\xff").mime);
        assert_eq!("image/gif", sniff(b"GIF89a\x01\x00\x01\x00").mime);
        assert_eq!("image/tiff", sniff(b"II*\0xxxx").mime);
        assert_eq!("image/tiff", sniff(b"MM\0*xxxx").mime);
        assert_eq!("image/bmp", sniff(b"BMxxxx").mime);
    }

    fn png_chunk(kind: &[u8; 4], payload: &[u8]) -> Vec<u8> {
        let mut out = (payload.len() as u32).to_be_bytes().to_vec();
        out.extend(kind);
        out.extend(payload);
        out.extend([0u8; 4]);
        out
    }

    fn png_file(chunks: &[Vec<u8>]) -> Vec<u8> {
        let mut ihdr = 8u32.to_be_bytes().to_vec();
        ihdr.extend(8u32.to_be_bytes());
        ihdr.extend([8, 6, 0, 0, 0]);
        let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
        out.extend(png_chunk(b"IHDR", &ihdr));
        for chunk in chunks {
            out.extend(chunk);
        }
        out
    }

    fn gif_file(blocks: &[Vec<u8>]) -> Vec<u8> {
        let mut out = b"GIF89a".to_vec();
        out.extend(4u16.to_le_bytes());
        out.extend(4u16.to_le_bytes());
        out.extend([0x80, 0, 0]);
        out.extend([0u8; 6]);
        for block in blocks {
            out.extend(block);
        }
        out.push(0x3b);
        out
    }

    fn gif_netscape_block() -> Vec<u8> {
        let mut out = vec![0x21, 0xff, 11];
        out.extend(b"NETSCAPE2.0");
        out.extend([3, 1, 0, 0, 0]);
        out
    }

    fn gif_comment_block(payload: &[u8]) -> Vec<u8> {
        let mut out = vec![0x21, 0xfe];
        for part in payload.chunks(255) {
            out.push(part.len() as u8);
            out.extend(part);
        }
        out.push(0);
        out
    }

    fn gif_image_block(payload: &[u8]) -> Vec<u8> {
        let mut out = vec![0x2c];
        out.extend([0u8; 8]);
        out.push(0);
        out.push(8);
        for part in payload.chunks(255) {
            out.push(part.len() as u8);
            out.extend(part);
        }
        out.push(0);
        out
    }

    fn riff_chunk(kind: &[u8; 4], payload: &[u8]) -> Vec<u8> {
        let mut out = kind.to_vec();
        out.extend((payload.len() as u32).to_le_bytes());
        out.extend(payload);
        if payload.len() % 2 == 1 {
            out.push(0);
        }
        out
    }

    fn webp_vp8x(flags: u8) -> Vec<u8> {
        let mut payload = vec![flags, 0, 0, 0];
        payload.extend([7, 0, 0, 7, 0, 0]);
        riff_chunk(b"VP8X", &payload)
    }

    fn webp_file(chunks: &[Vec<u8>]) -> Vec<u8> {
        let mut body = b"WEBP".to_vec();
        for chunk in chunks {
            body.extend(chunk);
        }
        let mut out = b"RIFF".to_vec();
        out.extend((body.len() as u32).to_le_bytes());
        out.extend(body);
        out
    }

    fn padded_marker(marker: &[u8], len: usize) -> Vec<u8> {
        let mut out = vec![0x5a; len];
        out[len / 2..len / 2 + marker.len()].copy_from_slice(marker);
        out
    }

    #[test]
    fn sniffs_apng_via_actl_chunk() {
        let info = sniff(&png_file(&[
            png_chunk(b"acTL", &[0, 0, 0, 2, 0, 0, 0, 0]),
            png_chunk(b"IDAT", &[0u8; 16]),
        ]));
        assert_eq!("image/apng", info.mime);
        assert!(info.animated);
    }

    #[test]
    fn sniffs_apng_when_actl_follows_a_large_icc_profile() {
        let icc = vec![0x11u8; 300_000];
        let info = sniff(&png_file(&[
            png_chunk(b"iCCP", &icc),
            png_chunk(b"acTL", &[0, 0, 0, 2, 0, 0, 0, 0]),
            png_chunk(b"IDAT", &[0u8; 16]),
        ]));
        assert_eq!("image/apng", info.mime);
        assert!(info.animated);
    }

    #[test]
    fn does_not_treat_actl_bytes_inside_png_image_data_as_animation() {
        let info = sniff(&png_file(&[png_chunk(
            b"IDAT",
            &padded_marker(b"acTL", 1_000_000),
        )]));
        assert_eq!("image/png", info.mime);
        assert!(!info.animated);
    }

    #[test]
    fn sniffs_animated_gif_via_netscape_extension() {
        let info = sniff(&gif_file(&[
            gif_comment_block(b"made by a tool"),
            gif_netscape_block(),
            gif_image_block(&[0u8; 8]),
        ]));
        assert_eq!("image/gif", info.mime);
        assert!(info.animated);
        assert_eq!(2, info.frames);
    }

    #[test]
    fn does_not_treat_netscape_bytes_inside_gif_image_data_as_animation() {
        let info = sniff(&gif_file(&[gif_image_block(&padded_marker(
            b"NETSCAPE2.0",
            1_000_000,
        ))]));
        assert_eq!("image/gif", info.mime);
        assert!(!info.animated);
        assert_eq!(1, info.frames);
    }

    #[test]
    fn sniffs_animated_webp_via_anim_chunk() {
        let info = sniff(&webp_file(&[
            webp_vp8x(0x02),
            riff_chunk(b"ANIM", &[0, 0, 0, 0, 0, 0]),
            riff_chunk(b"ANMF", &[0u8; 32]),
        ]));
        assert_eq!("image/webp", info.mime);
        assert!(info.animated);
    }

    #[test]
    fn sniffs_webp_alpha_via_alph_chunk() {
        let info = sniff(&webp_file(&[
            webp_vp8x(0x10),
            riff_chunk(b"ALPH", &[0u8; 8]),
            riff_chunk(b"VP8 ", &[0u8; 16]),
        ]));
        assert!(info.has_alpha);
        assert!(!info.animated);
    }

    #[test]
    fn does_not_treat_anim_bytes_inside_webp_image_data_as_animation() {
        let info = sniff(&webp_file(&[riff_chunk(
            b"VP8 ",
            &padded_marker(b"ANIM", 1_000_000),
        )]));
        assert_eq!("image/webp", info.mime);
        assert!(!info.animated);
        assert!(!info.has_alpha);
    }

    #[test]
    fn sniffs_ftyp_boxes_for_heic_avif_mp4_variants() {
        assert_eq!(
            "image/avif",
            sniff(b"\x00\x00\x00\x20ftypavifsome bytes").mime
        );
        assert_eq!(
            "image/avif",
            sniff(b"\x00\x00\x00\x20ftypavissome bytes").mime
        );
        assert!(sniff(b"\x00\x00\x00\x20ftypavissome bytes").animated);
        assert_eq!(
            "image/heic",
            sniff(b"\x00\x00\x00\x20ftypmif1some bytes").mime
        );
        assert_eq!(
            "image/heic",
            sniff(b"\x00\x00\x00\x20ftypheicsome bytes").mime
        );
        assert_eq!(
            "video/mp4",
            sniff(b"\x00\x00\x00\x20ftypiso5some bytes").mime
        );
        assert_eq!(
            "video/mp4",
            sniff(b"\x00\x00\x00\x20ftypM4V some bytes").mime
        );
        assert_eq!(
            "audio/mp4",
            sniff(b"\x00\x00\x00\x20ftypM4A some bytes").mime
        );
        assert_eq!(
            "video/quicktime",
            sniff(b"\x00\x00\x00\x20ftypqt  some bytes").mime
        );
    }

    #[test]
    fn detect_prefers_m4a_extension_over_generic_mp4_brand() {
        assert_eq!(
            "audio/mp4",
            detect(
                b"\x00\x00\x00\x20ftypisom\x00\x00\x02\x00isomiso2mp41",
                "track.m4a",
                None
            )
        );
    }

    #[test]
    fn sniffs_matroska_vs_webm() {
        let mkv = b"\x1a\x45\xdf\xa3\x9f\x42\x86\x81\x01\x42\xf7\x81\x01\x42\xf2\x81\x04\x42\xf3\x81\x08\x42\x82\x88matroska";
        assert_eq!("video/x-matroska", sniff(mkv).mime);
        assert_eq!(
            "video/webm",
            sniff(b"\x1a\x45\xdf\xa3 here is the webm doctype").mime
        );
    }

    #[test]
    fn sniffs_audio_variants() {
        assert_eq!("audio/mpeg", sniff(b"ID3\x04\x00\x00").mime);
        assert_eq!("audio/mpeg", sniff(b"\xff\xfb\x90\x00").mime);
        assert_eq!("audio/ogg", sniff(b"OggS\x00\x02").mime);
        assert_eq!("video/ogg", sniff(b"OggS\x00\x02xxxx\x80theora").mime);
        assert_eq!("audio/flac", sniff(b"fLaC\x00\x00").mime);
        assert_eq!("audio/wav", sniff(b"RIFF\x00\x00\x00\x00WAVEdata").mime);
    }

    #[test]
    fn extension_mime_covers_common_audio_and_video_containers() {
        assert_eq!(Some("video/ogg"), extension_mime("movie.ogv"));
        assert_eq!(Some("audio/ogg"), extension_mime("voice.opus"));
        assert_eq!(Some("audio/flac"), extension_mime("track.flac"));
        assert_eq!(Some("video/x-matroska"), extension_mime("clip.mkv"));
        assert_eq!(Some("image/tiff"), extension_mime("scan.tiff"));
        assert_eq!(Some("image/bmp"), extension_mime("bitmap.bmp"));
    }

    #[test]
    fn sniffs_pdf() {
        assert_eq!("application/pdf", sniff(b"%PDF-1.7\n").mime);
    }
}
