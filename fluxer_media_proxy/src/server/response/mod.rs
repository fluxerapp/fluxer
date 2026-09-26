// SPDX-License-Identifier: AGPL-3.0-or-later

pub(in crate::server) mod error;

use crate::{
    byte_budget::BudgetedBytes,
    disposition, http_headers, range,
    server::format_policy::{extension_from_mime, image_extension_from_filename},
};
use axum::{
    body::Body,
    http::{HeaderValue, Method, StatusCode, header},
    response::Response,
};
use bytes::Bytes;
use std::borrow::Cow;

pub(in crate::server) fn retained_response_bytes(data: BudgetedBytes) -> Bytes {
    Bytes::from_owner(data)
}

pub(in crate::server) struct MediaResponse<'a> {
    pub(in crate::server) method: Method,
    pub(in crate::server) data: BudgetedBytes,
    pub(in crate::server) content_type: &'a str,
    pub(in crate::server) range_header: Option<&'a str>,
    pub(in crate::server) disposition: Option<HeaderValue>,
}

pub(in crate::server) fn media_response(response: MediaResponse<'_>) -> Response {
    let MediaResponse {
        method,
        data,
        content_type,
        range_header,
        disposition,
    } = response;
    let total_len = data.len();
    let byte_range = match range::parse_range(range_header, total_len) {
        range::RangeSelection::Full => None,
        range::RangeSelection::Partial(byte_range) => Some(byte_range),
        range::RangeSelection::Unsatisfiable => {
            let mut response = Response::new(Body::empty());
            *response.status_mut() = StatusCode::RANGE_NOT_SATISFIABLE;
            http_headers::add_unsatisfiable_headers(response.headers_mut(), total_len);
            return response;
        }
    };
    let retained = retained_response_bytes(data);
    let (status, body_bytes) = if let Some(r) = byte_range {
        (StatusCode::PARTIAL_CONTENT, retained.slice(r.start..=r.end))
    } else {
        (StatusCode::OK, retained)
    };
    let body_len = body_bytes.len();
    let mut response = if method == Method::HEAD {
        Response::new(Body::empty())
    } else {
        Response::new(Body::from(body_bytes))
    };
    *response.status_mut() = status;
    http_headers::add_media_headers(response.headers_mut(), total_len, content_type, byte_range);
    response
        .headers_mut()
        .insert(header::CONTENT_LENGTH, HeaderValue::from(body_len));
    if let Some(value) = disposition {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

pub(in crate::server) fn content_disposition_header(
    content_type: &str,
    requested_download: bool,
    filename: Option<&str>,
) -> HeaderValue {
    let decision = disposition::decide(content_type, requested_download);
    let filename = filename
        .map(|name| download_filename_for_content_type(name, content_type, requested_download));
    disposition::header(decision, filename.as_deref())
        .map(disposition::ContentDisposition::into_header_value)
        .unwrap_or_else(|_| decision.header_value())
}

fn download_filename_for_content_type<'a>(
    filename: &'a str,
    content_type: &str,
    requested_download: bool,
) -> Cow<'a, str> {
    if !requested_download || filename.is_empty() {
        return Cow::Borrowed(filename);
    }
    let Some(expected_ext) = extension_from_mime(content_type) else {
        return Cow::Borrowed(filename);
    };
    if image_extension_from_filename(filename) == Some(expected_ext) {
        return Cow::Borrowed(filename);
    }
    let ext = expected_ext.name();
    let Some((stem, _)) = filename.rsplit_once('.') else {
        return Cow::Owned(format!("{filename}.{ext}"));
    };
    if stem.is_empty() {
        Cow::Owned(format!("{filename}.{ext}"))
    } else {
        Cow::Owned(format!("{stem}.{ext}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::params::{filename_from_storage_key, url_filename};

    fn disposition_string(
        content_type: &str,
        requested_download: bool,
        filename: Option<&str>,
    ) -> String {
        content_disposition_header(content_type, requested_download, filename)
            .to_str()
            .expect("content disposition is ascii")
            .to_owned()
    }

    #[test]
    fn explicit_download_disposition_uses_response_image_extension() {
        assert_eq!(
            "attachment; filename=\"welcome.gif\"",
            disposition_string("image/gif", true, Some("welcome.png"))
        );
        assert_eq!(
            "attachment; filename=\"welcome.gif\"",
            disposition_string("image/gif", true, Some("welcome"))
        );
        assert_eq!(
            "attachment; filename=\"photo.jpg\"",
            disposition_string("image/jpeg", true, Some("photo.jpg"))
        );
        assert_eq!(
            "attachment; filename=\"photo.jpg\"",
            disposition_string(
                "image/jpeg",
                true,
                Some(filename_from_storage_key("attachments/123/456/photo.jpg"))
            )
        );
        assert_eq!(
            "inline; filename=\"welcome.png\"",
            disposition_string("image/gif", false, Some("welcome.png"))
        );
    }

    #[test]
    fn a_long_external_filename_keeps_its_leading_bytes_in_the_header() {
        let url = format!(
            "https://example.test/lead{}tail.png",
            "a".repeat(disposition::CONTENT_DISPOSITION_FILENAME_BYTES_MAX)
        );
        let filename = url_filename(&url);
        let value = disposition_string("image/png", false, Some(&filename));
        assert!(
            value.starts_with("inline; filename=\"lead"),
            "{}",
            &value[..32]
        );
        assert!(!value.contains("tail.png"));
        assert!(value.len() <= disposition::PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
    }

    #[test]
    fn a_long_multi_byte_external_filename_still_carries_a_disposition() {
        let filename = "\u{e9}".repeat(1100);
        let inline = disposition_string("image/png", false, Some(&filename));
        assert!(inline.starts_with("inline; filename=\""));
        assert!(inline.len() <= disposition::PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
        let attachment = disposition_string("application/octet-stream", false, Some(&filename));
        assert!(attachment.starts_with("attachment; filename=\""));
        assert!(attachment.len() <= disposition::PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES);
    }
}
