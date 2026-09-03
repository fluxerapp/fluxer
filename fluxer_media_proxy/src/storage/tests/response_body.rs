// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::ByteBudget,
    range::ByteRange,
    storage::{
        StorageError,
        response_body::{
            ByteStream, LocalStreamBufferPool, StreamResponseValidation, exact_byte_stream,
            exact_response_stream, local_reader_stream, read_exact_bytes, read_response_bytes,
            validate_stream_response,
        },
    },
};
use bytes::Bytes;
use futures_util::{TryStreamExt as _, stream};
use http::{HeaderMap, HeaderValue, StatusCode, header};

fn byte_stream(chunks: Vec<Result<Bytes, std::io::Error>>) -> ByteStream {
    Box::pin(stream::iter(chunks))
}

fn provider_response(body: Vec<u8>) -> reqwest::Response {
    reqwest::Response::from(http::Response::new(body))
}

#[tokio::test]
async fn exact_reader_accepts_exact_length_and_rejects_short_or_long_sources() {
    let budget = ByteBudget::new(64);
    let exact = read_exact_bytes(&b"abcdef"[..], 6, &budget)
        .await
        .expect("exact reader");
    assert_eq!(exact.as_ref(), b"abcdef");

    assert!(matches!(
        read_exact_bytes(&b"abc"[..], 4, &budget).await,
        Err(StorageError::ObjectChanged)
    ));
    assert!(matches!(
        read_exact_bytes(&b"abcde"[..], 4, &budget).await,
        Err(StorageError::ObjectChanged)
    ));
    assert!(matches!(
        read_exact_bytes(&b"a"[..], 1, &ByteBudget::new(0)).await,
        Err(StorageError::BufferBudgetExhausted)
    ));
}

#[tokio::test]
async fn local_reader_stream_replenishes_owned_buffers_and_terminates() {
    let pool = LocalStreamBufferPool::new(4, 1).expect("local stream buffer pool");
    let stream =
        local_reader_stream(&b"abcdefgh"[..], pool.clone(), 4).expect("local reader stream");
    let data = stream
        .try_fold(Vec::new(), |mut data, chunk| async move {
            data.extend_from_slice(&chunk);
            Ok(data)
        })
        .await
        .expect("stream bytes");
    assert_eq!(data, b"abcdefgh");

    let held =
        local_reader_stream(&b"abcd"[..], pool.clone(), 4).expect("held local reader stream");
    let saturated = local_reader_stream(&b"xyz"[..], pool, 4)
        .expect("a saturated pool still serves the stream from an unpooled buffer");
    let data = saturated
        .try_fold(Vec::new(), |mut data, chunk| async move {
            data.extend_from_slice(&chunk);
            Ok(data)
        })
        .await
        .expect("stream bytes");
    assert_eq!(data, b"xyz");
    drop(held);
}

#[tokio::test]
async fn a_saturated_buffer_pool_serves_a_live_stream_from_a_small_fallback_buffer() {
    use futures_util::StreamExt as _;

    const CHUNK: usize = 16 * 1024;
    static SOURCE: [u8; CHUNK * 2] = [b'z'; CHUNK * 2];

    let pool = LocalStreamBufferPool::new(CHUNK, 1).expect("local stream buffer pool");
    let mut stream = local_reader_stream(&SOURCE[..], pool, CHUNK).expect("local reader stream");
    let pooled = stream
        .next()
        .await
        .expect("a first chunk")
        .expect("a readable first chunk");
    assert_eq!(CHUNK, pooled.len());
    let fallback = stream
        .next()
        .await
        .expect("a second chunk")
        .expect("a saturated pool must not fail a stream that already sent a chunk");
    assert!(
        fallback.len() < pooled.len(),
        "a fallback buffer must not hold a full pooled chunk: {}",
        fallback.len()
    );
    drop(pooled);
}

#[tokio::test]
async fn a_saturated_pool_still_replenishes_mid_stream_instead_of_truncating() {
    let pool = LocalStreamBufferPool::new(8, 1).expect("local stream buffer pool");
    let held = local_reader_stream(&b"12345678"[..], pool.clone(), 8).expect("held local stream");

    let source = vec![b'z'; 64 * 1024];
    let stream = local_reader_stream(std::io::Cursor::new(source.clone()), pool, 8)
        .expect("a saturated pool still starts the stream");
    let data = stream
        .try_fold(Vec::new(), |mut data, chunk| async move {
            data.extend_from_slice(&chunk);
            Ok(data)
        })
        .await
        .expect("a saturated pool must stream the whole body, not fail partway");
    assert_eq!(data.len(), source.len());
    assert_eq!(data, source);
    drop(held);
}

#[tokio::test]
async fn exact_stream_rejects_short_long_and_erroring_sources() {
    let exact = exact_byte_stream(
        byte_stream(vec![
            Ok(Bytes::new()),
            Ok(Bytes::from_static(b"ab")),
            Ok(Bytes::from_static(b"cd")),
        ]),
        4,
    );
    let chunks = exact.try_collect::<Vec<_>>().await.expect("exact stream");
    assert_eq!(
        chunks,
        vec![Bytes::from_static(b"ab"), Bytes::from_static(b"cd")]
    );

    let short = exact_byte_stream(byte_stream(vec![Ok(Bytes::from_static(b"abc"))]), 4);
    let error = short
        .try_collect::<Vec<_>>()
        .await
        .expect_err("short stream");
    assert_eq!(error.kind(), std::io::ErrorKind::UnexpectedEof);

    let long = exact_byte_stream(byte_stream(vec![Ok(Bytes::from_static(b"abcde"))]), 4);
    let error = long.try_collect::<Vec<_>>().await.expect_err("long stream");
    assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);

    let source_error = exact_byte_stream(
        byte_stream(vec![Err(std::io::Error::other("source failed"))]),
        1,
    );
    let error = source_error
        .try_collect::<Vec<_>>()
        .await
        .expect_err("source error");
    assert_eq!(error.kind(), std::io::ErrorKind::Other);
}

#[test]
fn stream_response_validation_requires_exact_headers_status_and_range() {
    let mut full_headers = HeaderMap::new();
    full_headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("10"));
    assert!(
        validate_stream_response(StreamResponseValidation {
            status: StatusCode::OK,
            headers: &full_headers,
            total_length: 10,
            expected_length: 10,
            byte_range: None,
        })
        .is_ok()
    );

    let range = ByteRange { start: 2, end: 5 };
    let mut partial_headers = HeaderMap::new();
    partial_headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("4"));
    partial_headers.insert(
        header::CONTENT_RANGE,
        HeaderValue::from_static("bytes 2-5/10"),
    );
    assert!(
        validate_stream_response(StreamResponseValidation {
            status: StatusCode::PARTIAL_CONTENT,
            headers: &partial_headers,
            total_length: 10,
            expected_length: 4,
            byte_range: Some(range),
        })
        .is_ok()
    );

    partial_headers.insert(
        header::CONTENT_RANGE,
        HeaderValue::from_static("bytes 3-6/10"),
    );
    assert!(matches!(
        validate_stream_response(StreamResponseValidation {
            status: StatusCode::PARTIAL_CONTENT,
            headers: &partial_headers,
            total_length: 10,
            expected_length: 4,
            byte_range: Some(range),
        }),
        Err(StorageError::ObjectChanged)
    ));

    full_headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("9"));
    assert!(matches!(
        validate_stream_response(StreamResponseValidation {
            status: StatusCode::OK,
            headers: &full_headers,
            total_length: 10,
            expected_length: 10,
            byte_range: None,
        }),
        Err(StorageError::ObjectChanged)
    ));
}

#[tokio::test]
async fn provider_response_readers_hold_the_declared_length_and_the_byte_budget() {
    let budget = ByteBudget::new(1 << 20);
    let exact = read_response_bytes(provider_response(b"abcdef".to_vec()), 6, &budget)
        .await
        .expect("response bytes");
    assert_eq!(exact.as_ref(), b"abcdef");

    assert!(matches!(
        read_response_bytes(provider_response(b"abc".to_vec()), 4, &budget).await,
        Err(StorageError::ObjectChanged)
    ));
    assert!(matches!(
        read_response_bytes(provider_response(b"abcde".to_vec()), 4, &budget).await,
        Err(StorageError::ObjectChanged)
    ));
    assert!(matches!(
        read_response_bytes(provider_response(b"a".to_vec()), 1, &ByteBudget::new(0)).await,
        Err(StorageError::BufferBudgetExhausted)
    ));

    let streamed = exact_response_stream(provider_response(b"abcdef".to_vec()), 6)
        .try_fold(Vec::new(), |mut data, chunk| async move {
            data.extend_from_slice(&chunk);
            Ok(data)
        })
        .await
        .expect("response stream bytes");
    assert_eq!(streamed, b"abcdef");

    let error = exact_response_stream(provider_response(b"abc".to_vec()), 6)
        .try_collect::<Vec<_>>()
        .await
        .expect_err("truncated response stream");
    assert_eq!(error.kind(), std::io::ErrorKind::UnexpectedEof);
}

#[tokio::test]
async fn exact_stream_accepts_small_transport_chunks_and_bounds_empty_ones() {
    const CHUNK_BYTES: usize = 64;
    const BODY_BYTES: usize = 8 * 1024;
    let chunks: Vec<Result<Bytes, std::io::Error>> = (0..BODY_BYTES / CHUNK_BYTES)
        .map(|_| Ok(Bytes::from(vec![7u8; CHUNK_BYTES])))
        .collect();
    let streamed = exact_byte_stream(byte_stream(chunks), BODY_BYTES as u64)
        .try_fold(Vec::new(), |mut data, chunk| async move {
            data.extend_from_slice(&chunk);
            Ok(data)
        })
        .await
        .expect("small transport chunks");
    assert_eq!(streamed.len(), BODY_BYTES);

    let empty: Vec<Result<Bytes, std::io::Error>> = (0..4096).map(|_| Ok(Bytes::new())).collect();
    let error = exact_byte_stream(byte_stream(empty), 4)
        .try_collect::<Vec<_>>()
        .await
        .expect_err("empty chunk flood");
    assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);
}
