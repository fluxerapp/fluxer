// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    http_headers, range, server::download_stream::DownloadStreamPolicy, storage::StreamObject,
};
use axum::{
    body::Body,
    http::{HeaderValue, Method, StatusCode, header},
    response::Response,
};

pub(in crate::server) fn passthrough_head_response(
    content_type: &str,
    total_len: usize,
    byte_range: Option<range::ByteRange>,
    disposition: Option<HeaderValue>,
) -> Response {
    let body_len = byte_range.map(|r| r.end - r.start + 1).unwrap_or(total_len);
    let mut response = Response::new(Body::empty());
    *response.status_mut() = if byte_range.is_some() {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::OK
    };
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

pub(super) struct StreamingMediaResponse<'a> {
    pub(super) method: Method,
    pub(super) object: StreamObject,
    pub(super) total_len: usize,
    pub(super) content_type: &'a str,
    pub(super) disposition: Option<HeaderValue>,
    pub(super) stream_policy: DownloadStreamPolicy,
}

pub(super) fn streaming_media_response(response: StreamingMediaResponse<'_>) -> Response {
    let StreamingMediaResponse {
        method,
        object,
        total_len,
        content_type,
        disposition,
        stream_policy,
    } = response;
    let status = if object.status == StatusCode::PARTIAL_CONTENT {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::OK
    };
    // Advertise the range the store actually satisfied, not one recomputed locally: an origin is
    // free to answer a different span than the client asked for, and Content-Range must describe
    // the bytes in this body.
    let effective_byte_range = if status == StatusCode::PARTIAL_CONTENT {
        object.byte_range
    } else {
        None
    };
    let expected_body_len = effective_byte_range
        .map(|r| r.end - r.start + 1)
        .unwrap_or(total_len);
    let body_len = object
        .content_length
        .and_then(|value| usize::try_from(value).ok())
        .unwrap_or(expected_body_len);
    let response_content_type = if content_type.is_empty() {
        object.content_type.as_str()
    } else {
        content_type
    };
    let mut response = if method == Method::HEAD {
        Response::new(Body::empty())
    } else {
        Response::new(stream_policy.guard(object.body, body_len))
    };
    *response.status_mut() = status;
    http_headers::add_media_headers(
        response.headers_mut(),
        total_len,
        response_content_type,
        effective_byte_range,
    );
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

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::Bytes;
    use http_body_util::BodyExt as _;
    use std::io;

    fn stored_stream(chunks: Vec<&'static [u8]>, content_length: u64) -> StreamObject {
        StreamObject {
            body: Body::from_stream(futures_util::stream::iter(
                chunks
                    .into_iter()
                    .map(|chunk| Ok::<_, io::Error>(Bytes::from_static(chunk))),
            )),
            status: StatusCode::OK,
            content_length: Some(content_length),
            content_type: "text/plain".to_owned(),
            byte_range: None,
            total_length: Some(content_length),
        }
    }

    fn stored_response(object: StreamObject) -> Response {
        streaming_media_response(StreamingMediaResponse {
            method: Method::GET,
            object,
            total_len: 5,
            content_type: "text/plain",
            disposition: None,
            stream_policy: DownloadStreamPolicy::for_passthrough(30_000),
        })
    }

    async fn body_error_kind(response: Response) -> io::ErrorKind {
        let mut error = response
            .into_body()
            .collect()
            .await
            .map(|collected| collected.to_bytes())
            .expect_err("the stored stream fails")
            .into_inner();
        loop {
            error = match error.downcast::<io::Error>() {
                Ok(error) => return error.kind(),
                Err(error) => error
                    .downcast::<axum::Error>()
                    .expect("the stored stream fails with an io error")
                    .into_inner(),
            };
        }
    }

    #[tokio::test]
    async fn a_stored_stream_that_ends_short_of_its_content_length_fails_with_unexpected_eof() {
        let response = stored_response(stored_stream(vec![b"hel"], 5));
        assert_eq!(
            Some("5"),
            response
                .headers()
                .get(header::CONTENT_LENGTH)
                .and_then(|value| value.to_str().ok())
        );
        assert_eq!(
            io::ErrorKind::UnexpectedEof,
            body_error_kind(response).await
        );
    }

    #[tokio::test]
    async fn a_stored_stream_that_overruns_its_content_length_fails_with_invalid_data() {
        let response = stored_response(stored_stream(vec![b"hello", b"world"], 5));
        assert_eq!(io::ErrorKind::InvalidData, body_error_kind(response).await);
    }
}
