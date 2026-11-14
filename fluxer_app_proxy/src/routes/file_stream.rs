// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::Body,
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use fluxer_common::http_range::{RangeSelection, parse_range};
use std::path::Path;
use tokio::io::{AsyncReadExt, AsyncSeekExt};
use tokio_util::io::ReaderStream;

const STREAM_CHUNK_BYTES: usize = 64 * 1024;

pub(crate) async fn stream_file(
    path: &Path,
    request_headers: &HeaderMap,
    entity_tag: Option<&str>,
) -> std::io::Result<Response> {
    let mut file = tokio::fs::File::open(path).await?;
    let metadata = file.metadata().await?;
    if !metadata.is_file() {
        return Err(std::io::Error::other("not a regular file"));
    }
    let file_size = metadata.len();

    let (status, start, length) = match requested_range(request_headers, entity_tag, file_size) {
        RangeSelection::Full => (StatusCode::OK, 0, file_size),
        RangeSelection::Partial(range) => (
            StatusCode::PARTIAL_CONTENT,
            range.start as u64,
            (range.end - range.start + 1) as u64,
        ),
        RangeSelection::Unsatisfiable => return Ok(range_not_satisfiable(file_size)),
    };

    if start > 0 {
        file.seek(std::io::SeekFrom::Start(start)).await?;
    }

    let stream = ReaderStream::with_capacity(file.take(length), STREAM_CHUNK_BYTES);
    let mut response = Response::new(Body::from_stream(stream));
    *response.status_mut() = status;
    let headers = response.headers_mut();
    headers.insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
    headers.insert(header::CONTENT_LENGTH, HeaderValue::from(length));
    if status == StatusCode::PARTIAL_CONTENT
        && let Ok(value) =
            HeaderValue::from_str(&format!("bytes {start}-{}/{file_size}", start + length - 1))
    {
        headers.insert(header::CONTENT_RANGE, value);
    }
    Ok(response)
}

fn range_not_satisfiable(file_size: u64) -> Response {
    let mut response = StatusCode::RANGE_NOT_SATISFIABLE.into_response();
    let headers = response.headers_mut();
    headers.insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
    if let Ok(value) = HeaderValue::from_str(&format!("bytes */{file_size}")) {
        headers.insert(header::CONTENT_RANGE, value);
    }
    response
}

fn requested_range(
    headers: &HeaderMap,
    entity_tag: Option<&str>,
    file_size: u64,
) -> RangeSelection {
    let Some(raw) = headers
        .get(header::RANGE)
        .and_then(|value| value.to_str().ok())
    else {
        return RangeSelection::Full;
    };
    if !if_range_matches(headers, entity_tag) {
        return RangeSelection::Full;
    }
    let Ok(file_size) = usize::try_from(file_size) else {
        return RangeSelection::Full;
    };
    parse_range(Some(raw), file_size)
}

fn if_range_matches(headers: &HeaderMap, entity_tag: Option<&str>) -> bool {
    let Some(raw) = headers
        .get(header::IF_RANGE)
        .and_then(|value| value.to_str().ok())
    else {
        return true;
    };
    entity_tag.is_some_and(|tag| raw.trim() == tag)
}
