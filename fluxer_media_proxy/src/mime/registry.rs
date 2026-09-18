// SPDX-License-Identifier: AGPL-3.0-or-later

use super::Category;
use crate::media_type::MediaType;
use http::HeaderValue;

pub const INERT_CONTENT_TYPE: &str = "text/plain; charset=utf-8";

const JAVASCRIPT_ESSENCES: [&str; 16] = [
    "application/ecmascript",
    "application/javascript",
    "application/x-ecmascript",
    "application/x-javascript",
    "text/ecmascript",
    "text/javascript",
    "text/javascript1.0",
    "text/javascript1.1",
    "text/javascript1.2",
    "text/javascript1.3",
    "text/javascript1.4",
    "text/javascript1.5",
    "text/jscript",
    "text/livescript",
    "text/x-ecmascript",
    "text/x-javascript",
];

pub fn is_javascript_content_type(content_type: &str) -> bool {
    content_type.split(',').any(|member| {
        member
            .trim_start_matches([' ', '\t'])
            .split([' ', '\t', ';', '('])
            .next()
            .is_some_and(|essence| {
                JAVASCRIPT_ESSENCES
                    .iter()
                    .any(|javascript| essence.eq_ignore_ascii_case(javascript))
            })
    })
}

pub fn normalize(raw: Option<&str>) -> Option<&str> {
    let value = raw?;
    let semi = value.find(';').unwrap_or(value.len());
    let trimmed = value[..semi].trim_matches([' ', '\t']);
    (!trimmed.is_empty() && HeaderValue::from_bytes(trimmed.as_bytes()).is_ok()).then_some(trimmed)
}

pub fn category(mime_type: &str) -> Option<Category> {
    let prefix = mime_type.as_bytes().get(..6);
    if prefix.is_some_and(|prefix| prefix.eq_ignore_ascii_case(b"image/")) {
        Some(Category::Image)
    } else if prefix.is_some_and(|prefix| prefix.eq_ignore_ascii_case(b"video/")) {
        Some(Category::Video)
    } else if prefix.is_some_and(|prefix| prefix.eq_ignore_ascii_case(b"audio/")) {
        Some(Category::Audio)
    } else {
        None
    }
}

pub fn passthrough_mime(raw: Option<&str>) -> Option<&'static str> {
    let normalized = normalize(raw)?;
    if normalized.eq_ignore_ascii_case("application/pdf") {
        return Some("application/pdf");
    }
    if normalized.eq_ignore_ascii_case("text/css") {
        return Some("text/css; charset=utf-8");
    }
    MediaType::from_mime(normalized).map(MediaType::mime)
}

pub fn extension_mime(filename: &str) -> Option<&'static str> {
    let ext = filename.rsplit_once('.')?.1;
    if ext.eq_ignore_ascii_case("css") {
        return Some("text/css; charset=utf-8");
    }
    MediaType::from_extension(ext).map(MediaType::mime)
}
