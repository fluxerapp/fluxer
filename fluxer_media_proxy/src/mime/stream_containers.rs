// SPDX-License-Identifier: AGPL-3.0-or-later

use super::SniffInfo;
use crate::media_type::MediaType;

pub(super) fn mpeg_ts_sniff(data: &[u8]) -> bool {
    data.len() >= 188
        && data[0] == 0x47
        && (data.len() < 376 || data[188] == 0x47)
        && (data.len() < 564 || data[376] == 0x47)
}

pub(super) fn is_adts(data: &[u8]) -> bool {
    if data.len() < 7 || data[0] != 0xff || data[1] & 0xf6 != 0xf0 {
        return false;
    }
    let sample_rate_index = (data[2] >> 2) & 0x0f;
    if sample_rate_index == 0x0f {
        return false;
    }
    let frame_length = (usize::from(data[3] & 0x03) << 11)
        | (usize::from(data[4]) << 3)
        | (usize::from(data[5]) >> 5);
    frame_length >= 7 && frame_length <= data.len()
}

pub(super) fn looks_like_svg(data: &[u8]) -> bool {
    let mut window = &data[..data.len().min(4096)];
    if window.starts_with(b"\xef\xbb\xbf") {
        window = &window[3..];
    }
    window.windows(4).any(|w| w == b"<svg")
        && (window.windows(5).any(|w| w == b"xmlns")
            || window.starts_with(b"<svg")
            || window.starts_with(b"<?xml"))
}

pub(super) fn ogg_sniff(data: &[u8]) -> SniffInfo {
    let window = &data[..data.len().min(8192)];
    if window.windows(6).any(|w| w == b"theora" || w == b"Theora") {
        SniffInfo {
            mime: MediaType::OGGVideo.mime(),
            ..Default::default()
        }
    } else {
        SniffInfo {
            mime: MediaType::OGGAudio.mime(),
            ..Default::default()
        }
    }
}

pub(super) fn matroska_sniff(data: &[u8]) -> SniffInfo {
    let window = &data[..data.len().min(4096)];
    if window.windows(4).any(|w| w == b"webm") {
        SniffInfo {
            mime: MediaType::WebMVideo.mime(),
            ..Default::default()
        }
    } else {
        SniffInfo {
            mime: MediaType::MatroskaVideo.mime(),
            ..Default::default()
        }
    }
}
