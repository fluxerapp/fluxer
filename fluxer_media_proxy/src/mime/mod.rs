// SPDX-License-Identifier: AGPL-3.0-or-later

mod image_containers;
mod iso_bmff;
mod registry;
mod stream_containers;

#[cfg(test)]
mod tests;

use self::image_containers::{
    GIFAnimation, PNGAnimation, gif_animation, png_animation, webp_sniff, webp_sniff_complete,
};
use self::iso_bmff::iso_bmff_sniff;
use self::stream_containers::{is_adts, looks_like_svg, matroska_sniff, mpeg_ts_sniff, ogg_sniff};
use crate::media_type::MediaType;

pub use self::registry::{category, extension_mime, normalize, passthrough_mime};

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

#[derive(Clone, Copy, Debug)]
pub struct MediaPrefixSniff {
    pub media: SniffInfo,
    pub complete: bool,
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

fn starts(data: &[u8], prefix: &[u8]) -> bool {
    data.len() >= prefix.len() && &data[..prefix.len()] == prefix
}

pub fn sniff(data: &[u8]) -> SniffInfo {
    if starts(data, b"\x89PNG\r\n\x1a\n") {
        let mut out = SniffInfo {
            mime: MediaType::PNG.mime(),
            has_alpha: true,
            ..Default::default()
        };
        if data.len() >= 26 && &data[12..16] == b"IHDR" {
            out.width =
                u32::from_be_bytes(data[16..20].try_into().expect("validated PNG width slice"));
            out.height =
                u32::from_be_bytes(data[20..24].try_into().expect("validated PNG height slice"));
            out.has_alpha = data[25] == 4 || data[25] == 6;
        }
        if let PNGAnimation::Animated(frames) = png_animation(data) {
            out.mime = MediaType::APNG.mime();
            out.animated = frames > 1;
            out.frames = frames;
        }
        return out;
    }
    if starts(data, b"\xff\xd8\xff") {
        return SniffInfo {
            mime: MediaType::JPEG.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"GIF87a") || starts(data, b"GIF89a") {
        let animated = gif_animation(data) == GIFAnimation::Animated;
        return SniffInfo {
            mime: MediaType::GIF.mime(),
            animated,
            frames: if animated { 2 } else { 1 },
            width: if data.len() >= 10 {
                u32::from(u16::from_le_bytes(
                    data[6..8].try_into().expect("validated GIF width slice"),
                ))
            } else {
                0
            },
            height: if data.len() >= 10 {
                u32::from(u16::from_le_bytes(
                    data[8..10].try_into().expect("validated GIF height slice"),
                ))
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
            mime: MediaType::JXL.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"II*\0") || starts(data, b"MM\0*") {
        return SniffInfo {
            mime: MediaType::TIFF.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"BM") {
        return SniffInfo {
            mime: MediaType::BMP.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"\x1a\x45\xdf\xa3") {
        return matroska_sniff(data);
    }
    if starts(data, b"FLV") {
        return SniffInfo {
            mime: MediaType::FLVVideo.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"RIFF") && data.len() >= 12 && &data[8..12] == b"AVI " {
        return SniffInfo {
            mime: MediaType::AVIVideo.mime(),
            ..Default::default()
        };
    }
    if starts(
        data,
        b"\x30\x26\xb2\x75\x8e\x66\xcf\x11\xa6\xd9\x00\xaa\x00\x62\xce\x6c",
    ) {
        return SniffInfo {
            mime: MediaType::WMVVideo.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"\x00\x00\x01\xba") || starts(data, b"\x00\x00\x01\xb3") {
        return SniffInfo {
            mime: MediaType::MPEGVideo.mime(),
            ..Default::default()
        };
    }
    if mpeg_ts_sniff(data) {
        return SniffInfo {
            mime: MediaType::MPEGTSVideo.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"ID3")
        || starts(data, b"\xff\xfb")
        || starts(data, b"\xff\xf3")
        || starts(data, b"\xff\xf2")
    {
        return SniffInfo {
            mime: MediaType::MPEGAudio.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"OggS") {
        return ogg_sniff(data);
    }
    if starts(data, b"fLaC") {
        return SniffInfo {
            mime: MediaType::FLACAudio.mime(),
            ..Default::default()
        };
    }
    if is_adts(data) {
        return SniffInfo {
            mime: MediaType::AACAudio.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"RIFF") && data.len() >= 12 && &data[8..12] == b"WAVE" {
        return SniffInfo {
            mime: MediaType::WAVAudio.mime(),
            ..Default::default()
        };
    }
    if starts(data, b"FORM") && data.len() >= 12 && matches!(&data[8..12], b"AIFF" | b"AIFC") {
        return SniffInfo {
            mime: MediaType::AIFFAudio.mime(),
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
            mime: MediaType::SVG.mime(),
            ..Default::default()
        };
    }
    SniffInfo::default()
}

pub fn sniff_prefix(data: &[u8], total_len: usize) -> MediaPrefixSniff {
    assert!(data.len() <= total_len);
    let sniffed = sniff(data);
    if data.len() == total_len {
        return MediaPrefixSniff {
            media: sniffed,
            complete: true,
        };
    }
    let complete =
        if sniffed.mime == MediaType::PNG.mime() || sniffed.mime == MediaType::APNG.mime() {
            png_animation(data) != PNGAnimation::Incomplete
        } else if sniffed.mime == MediaType::WebP.mime() {
            webp_sniff_complete(data)
        } else if sniffed.mime == MediaType::GIF.mime() {
            gif_animation(data) != GIFAnimation::Incomplete
        } else {
            matches!(
                MediaType::from_mime(sniffed.mime),
                Some(
                    MediaType::JPEG
                        | MediaType::JXL
                        | MediaType::TIFF
                        | MediaType::BMP
                        | MediaType::SVG
                )
            )
        };
    MediaPrefixSniff {
        media: sniffed,
        complete,
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
