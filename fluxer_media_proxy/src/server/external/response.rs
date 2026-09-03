// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ExternalBody, FetchedExternal, byte_range::ExternalPartial};
use crate::{
    constants, http_headers,
    server::{
        download_stream::DownloadStreamPolicy,
        format_policy::{content_type_is_trustworthy, is_svg_content_type},
        response::retained_response_bytes,
    },
};
use axum::{
    body::Body,
    http::{HeaderValue, Method, StatusCode, header},
    response::Response,
};
use bytes::Bytes;
use futures_util::StreamExt as _;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ExternalStreamDecision {
    Stream(Option<u64>),
    Buffer,
}

// A passthrough streams on the strength of its content type alone. An upstream that declares no
// length still streams; the declared length only decides whether the response can name one.
pub(super) fn external_stream_decision(
    allow_stream: bool,
    content_length: Option<u64>,
    content_type: &str,
) -> ExternalStreamDecision {
    if !allow_stream
        || !content_type_is_trustworthy(content_type)
        || is_svg_content_type(content_type)
        || content_length.is_some_and(|length| length > constants::MAX_MEDIA_PROXY_BYTES as u64)
    {
        return ExternalStreamDecision::Buffer;
    }
    ExternalStreamDecision::Stream(content_length)
}

pub(super) struct ExternalPartialResponse {
    pub(super) method: Method,
    pub(super) fetched: FetchedExternal,
    pub(super) partial: ExternalPartial,
    pub(super) disposition: Option<HeaderValue>,
    pub(super) stream_policy: DownloadStreamPolicy,
}

pub(super) fn external_partial_response(request: ExternalPartialResponse) -> Response {
    let ExternalPartialResponse {
        method,
        fetched,
        partial,
        disposition,
        stream_policy,
    } = request;
    let FetchedExternal {
        body, content_type, ..
    } = fetched;
    let body_len = partial
        .content_length()
        .or_else(|| external_body_length(&body));
    let mut response = Response::new(external_body(method, body, body_len, stream_policy));
    *response.status_mut() = StatusCode::PARTIAL_CONTENT;
    http_headers::add_media_headers(
        response.headers_mut(),
        body_length_hint(body_len),
        &content_type,
        None,
    );
    if let Some(body_len) = body_len {
        response
            .headers_mut()
            .insert(header::CONTENT_LENGTH, HeaderValue::from(body_len));
    }
    if let Some(value) = partial.header_value() {
        response.headers_mut().insert(header::CONTENT_RANGE, value);
    }
    if let Some(value) = disposition {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

pub(super) struct ExternalStreamingResponse<'a> {
    pub(super) method: Method,
    pub(super) response: reqwest::Response,
    pub(super) prefix: Bytes,
    pub(super) content_length: Option<u64>,
    pub(super) content_type: &'a str,
    pub(super) disposition: Option<HeaderValue>,
    pub(super) stream_policy: DownloadStreamPolicy,
}

pub(super) fn external_streaming_response(request: ExternalStreamingResponse<'_>) -> Response {
    let ExternalStreamingResponse {
        method,
        response,
        prefix,
        content_length,
        content_type,
        disposition,
        stream_policy,
    } = request;
    let body = if method == Method::HEAD {
        Body::empty()
    } else {
        guarded_stream(response, prefix, content_length, stream_policy)
    };
    let mut http_response = Response::new(body);
    *http_response.status_mut() = StatusCode::OK;
    http_headers::add_media_headers(
        http_response.headers_mut(),
        body_length_hint(content_length),
        content_type,
        None,
    );
    if let Some(content_length) = content_length {
        http_response
            .headers_mut()
            .insert(header::CONTENT_LENGTH, HeaderValue::from(content_length));
    }
    if let Some(value) = disposition {
        http_response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    http_response
}

fn external_body_length(body: &ExternalBody) -> Option<u64> {
    match body {
        ExternalBody::Buffered(data) => Some(data.len() as u64),
        ExternalBody::Streaming { content_length, .. } => *content_length,
    }
}

fn external_body(
    method: Method,
    body: ExternalBody,
    expected_length: Option<u64>,
    stream_policy: DownloadStreamPolicy,
) -> Body {
    if method == Method::HEAD {
        return Body::empty();
    }
    match body {
        ExternalBody::Buffered(data) => Body::from(retained_response_bytes(data)),
        ExternalBody::Streaming {
            response, prefix, ..
        } => guarded_stream(response, prefix, expected_length, stream_policy),
    }
}

fn guarded_stream(
    response: reqwest::Response,
    prefix: Bytes,
    expected_length: Option<u64>,
    stream_policy: DownloadStreamPolicy,
) -> Body {
    let has_prefix = !prefix.is_empty();
    let prefixed =
        futures_util::stream::iter(has_prefix.then_some(Ok::<Bytes, reqwest::Error>(prefix)))
            .chain(response.bytes_stream());
    let body = Body::from_stream(prefixed);
    match expected_length {
        Some(length) => stream_policy.guard(body, body_length_hint(Some(length))),
        None => stream_policy.guard_capped(body, constants::MAX_MEDIA_PROXY_BYTES),
    }
}

fn body_length_hint(length: Option<u64>) -> usize {
    length
        .and_then(|length| usize::try_from(length).ok())
        .unwrap_or(constants::MAX_MEDIA_PROXY_BYTES)
}
