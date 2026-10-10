// SPDX-License-Identifier: AGPL-3.0-or-later

use url::Url;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaKind {
    Image,
    Video,
    Audio,
}

pub fn detect_media_kind(url: &Url) -> Option<MediaKind> {
    let path = url.path().to_ascii_lowercase();
    let dot_idx = path.rfind('.')?;
    let ext = &path[dot_idx..];

    match ext {
        ".png" | ".jpg" | ".jpeg" | ".gif" | ".webp" | ".avif" | ".svg" | ".bmp" | ".apng"
        | ".heic" | ".heif" | ".tif" | ".tiff" => Some(MediaKind::Image),

        ".mp4" | ".webm" | ".mov" | ".avi" | ".mkv" | ".m4v" | ".ogv" | ".mpeg" | ".mpg"
        | ".3gp" | ".3g2" | ".m3u8" | ".ts" => Some(MediaKind::Video),

        ".mp3" | ".ogg" | ".wav" | ".flac" | ".m4a" | ".aac" | ".opus" | ".weba" | ".oga"
        | ".aif" | ".aiff" | ".amr" | ".mid" | ".midi" => Some(MediaKind::Audio),

        _ => None,
    }
}
