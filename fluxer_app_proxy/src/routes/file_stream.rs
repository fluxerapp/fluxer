// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::Body,
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::Path;
use tokio::io::{AsyncReadExt, AsyncSeekExt};
use tokio_util::io::ReaderStream;

const STREAM_CHUNK_BYTES: usize = 64 * 1024;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ByteRange {
    start: u64,
    end: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RequestedRange {
    Whole,
    Partial(ByteRange),
    Unsatisfiable,
}

pub(super) async fn stream_file(
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
        RequestedRange::Whole => (StatusCode::OK, 0, file_size),
        RequestedRange::Partial(range) => (
            StatusCode::PARTIAL_CONTENT,
            range.start,
            range.end - range.start + 1,
        ),
        RequestedRange::Unsatisfiable => return Ok(range_not_satisfiable(file_size)),
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
) -> RequestedRange {
    let Some(raw) = headers
        .get(header::RANGE)
        .and_then(|value| value.to_str().ok())
    else {
        return RequestedRange::Whole;
    };
    if !if_range_matches(headers, entity_tag) {
        return RequestedRange::Whole;
    }
    parse_byte_range(raw, file_size)
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

fn parse_byte_range(raw: &str, file_size: u64) -> RequestedRange {
    let Some(spec) = raw.trim_matches([' ', '\t']).strip_prefix("bytes=") else {
        return RequestedRange::Whole;
    };
    if spec.contains(',') {
        return RequestedRange::Whole;
    }
    let Some((start_part, end_part)) = spec.split_once('-') else {
        return RequestedRange::Whole;
    };
    let start_part = start_part.trim_matches([' ', '\t']);
    let end_part = end_part.trim_matches([' ', '\t']);
    if start_part.is_empty() && end_part.is_empty() {
        return RequestedRange::Whole;
    }
    if file_size == 0 {
        return RequestedRange::Unsatisfiable;
    }

    if start_part.is_empty() {
        let Ok(suffix_len) = end_part.parse::<u64>() else {
            return RequestedRange::Whole;
        };
        if suffix_len == 0 {
            return RequestedRange::Unsatisfiable;
        }
        return RequestedRange::Partial(ByteRange {
            start: file_size - suffix_len.min(file_size),
            end: file_size - 1,
        });
    }

    let Ok(start) = start_part.parse::<u64>() else {
        return RequestedRange::Whole;
    };
    let requested_end = if end_part.is_empty() {
        file_size - 1
    } else if let Ok(end) = end_part.parse::<u64>() {
        end
    } else {
        return RequestedRange::Whole;
    };
    if start >= file_size || requested_end < start {
        return RequestedRange::Unsatisfiable;
    }
    RequestedRange::Partial(ByteRange {
        start,
        end: requested_end.min(file_size - 1),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    struct StreamedFile {
        root: std::path::PathBuf,
    }

    impl StreamedFile {
        fn with_bytes(bytes: &[u8]) -> Self {
            static NEXT_FIXTURE: AtomicU64 = AtomicU64::new(0);
            let unique = NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed);
            let pid = std::process::id();
            let root = std::env::temp_dir().join(format!("fluxer-file-stream-{pid}-{unique}"));
            std::fs::create_dir_all(&root).unwrap();
            std::fs::write(root.join("payload.bin"), bytes).unwrap();
            Self { root }
        }

        fn path(&self) -> std::path::PathBuf {
            self.root.join("payload.bin")
        }
    }

    impl Drop for StreamedFile {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.root);
        }
    }

    fn range_headers(value: &str) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(header::RANGE, HeaderValue::from_str(value).unwrap());
        headers
    }

    fn header_of(response: &Response, name: header::HeaderName) -> Option<String> {
        response
            .headers()
            .get(name)
            .and_then(|value| value.to_str().ok())
            .map(ToOwned::to_owned)
    }

    async fn body_of(response: Response) -> Vec<u8> {
        axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap()
            .to_vec()
    }

    #[tokio::test]
    async fn a_whole_file_is_streamed_with_a_length_and_a_range_offer() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &HeaderMap::new(), None)
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            header_of(&response, header::CONTENT_LENGTH).as_deref(),
            Some("10")
        );
        assert_eq!(
            header_of(&response, header::ACCEPT_RANGES).as_deref(),
            Some("bytes")
        );
        assert_eq!(body_of(response).await, b"0123456789");
    }

    #[tokio::test]
    async fn a_resumed_download_gets_only_the_bytes_it_asked_for() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &range_headers("bytes=4-6"), None)
            .await
            .unwrap();

        assert_eq!(
            response.status(),
            StatusCode::PARTIAL_CONTENT,
            "a client resuming a partial download re-fetches the whole file on a 200"
        );
        assert_eq!(
            header_of(&response, header::CONTENT_RANGE).as_deref(),
            Some("bytes 4-6/10")
        );
        assert_eq!(
            header_of(&response, header::CONTENT_LENGTH).as_deref(),
            Some("3")
        );
        assert_eq!(body_of(response).await, b"456");
    }

    #[tokio::test]
    async fn an_open_ended_range_runs_to_the_last_byte() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &range_headers("bytes=7-"), None)
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::PARTIAL_CONTENT);
        assert_eq!(
            header_of(&response, header::CONTENT_RANGE).as_deref(),
            Some("bytes 7-9/10")
        );
        assert_eq!(body_of(response).await, b"789");
    }

    #[tokio::test]
    async fn a_suffix_range_returns_the_tail() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &range_headers("bytes=-3"), None)
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::PARTIAL_CONTENT);
        assert_eq!(
            header_of(&response, header::CONTENT_RANGE).as_deref(),
            Some("bytes 7-9/10")
        );
        assert_eq!(body_of(response).await, b"789");
    }

    #[tokio::test]
    async fn a_range_past_the_end_is_refused_with_the_real_size() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &range_headers("bytes=50-60"), None)
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::RANGE_NOT_SATISFIABLE);
        assert_eq!(
            header_of(&response, header::CONTENT_RANGE).as_deref(),
            Some("bytes */10")
        );
    }

    #[tokio::test]
    async fn a_multi_range_request_falls_back_to_the_whole_file() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let response = stream_file(&fixture.path(), &range_headers("bytes=0-1,5-6"), None)
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(body_of(response).await, b"0123456789");
    }

    #[tokio::test]
    async fn a_range_against_a_stale_validator_resends_the_whole_file() {
        let fixture = StreamedFile::with_bytes(b"0123456789");
        let mut headers = range_headers("bytes=4-6");
        headers.insert(
            header::IF_RANGE,
            HeaderValue::from_static("\"from-a-previous-build\""),
        );

        let response = stream_file(&fixture.path(), &headers, Some("\"current\""))
            .await
            .unwrap();

        assert_eq!(
            response.status(),
            StatusCode::OK,
            "stitching a range from a redeployed build onto a stale prefix corrupts the download"
        );
        assert_eq!(body_of(response).await, b"0123456789");
    }

    #[tokio::test]
    async fn a_range_against_the_current_validator_is_honoured() {
        let fixture = StreamedFile::with_bytes(b"0123456789");
        let mut headers = range_headers("bytes=4-6");
        headers.insert(header::IF_RANGE, HeaderValue::from_static("\"current\""));

        let response = stream_file(&fixture.path(), &headers, Some("\"current\""))
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::PARTIAL_CONTENT);
        assert_eq!(body_of(response).await, b"456");
    }

    #[tokio::test]
    async fn a_missing_file_is_reported_as_not_found() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let err = stream_file(&fixture.root.join("absent.bin"), &HeaderMap::new(), None)
            .await
            .expect_err("a missing file must not stream an empty body");

        assert_eq!(err.kind(), std::io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn a_directory_is_never_streamed_as_a_file() {
        let fixture = StreamedFile::with_bytes(b"0123456789");

        let err = stream_file(&fixture.root, &HeaderMap::new(), None)
            .await
            .expect_err("a directory answered as 200 promises a length no body can deliver");

        assert_ne!(err.kind(), std::io::ErrorKind::NotFound);
    }

    #[test]
    fn a_malformed_range_is_ignored() {
        assert_eq!(
            parse_byte_range("bytes=abc-def", 100),
            RequestedRange::Whole
        );
        assert_eq!(parse_byte_range("bytes=", 100), RequestedRange::Whole);
        assert_eq!(parse_byte_range("items=0-9", 100), RequestedRange::Whole);
    }

    #[test]
    fn an_empty_file_satisfies_no_range() {
        assert_eq!(
            parse_byte_range("bytes=0-9", 0),
            RequestedRange::Unsatisfiable
        );
        assert_eq!(
            parse_byte_range("bytes=-5", 0),
            RequestedRange::Unsatisfiable
        );
    }

    #[test]
    fn a_zero_length_suffix_is_unsatisfiable() {
        assert_eq!(
            parse_byte_range("bytes=-0", 100),
            RequestedRange::Unsatisfiable
        );
    }

    #[test]
    fn a_suffix_longer_than_the_file_is_clamped_to_the_whole_file() {
        assert_eq!(
            parse_byte_range("bytes=-9999", 100),
            RequestedRange::Partial(ByteRange { start: 0, end: 99 })
        );
    }

    #[test]
    fn an_end_past_the_last_byte_is_clamped() {
        assert_eq!(
            parse_byte_range("bytes=90-9999", 100),
            RequestedRange::Partial(ByteRange { start: 90, end: 99 })
        );
    }

    #[test]
    fn an_inverted_range_is_unsatisfiable() {
        assert_eq!(
            parse_byte_range("bytes=10-5", 100),
            RequestedRange::Unsatisfiable
        );
    }
}
