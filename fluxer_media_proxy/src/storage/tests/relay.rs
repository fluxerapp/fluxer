// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{fake_s3, store};
use crate::storage::{RelayBody, RelayBodyChunks, RelayPutOptions, s3::UNSIGNED_PAYLOAD};
use bytes::Bytes;
use http::header;

fn chunks(frames: &'static [&'static [u8]]) -> RelayBodyChunks {
    Box::pin(futures_util::stream::iter(
        frames
            .iter()
            .map(|frame| Ok(Bytes::from_static(frame)))
            .collect::<Vec<_>>(),
    ))
}

#[tokio::test]
async fn relay_put_s3_streams_body_with_unsigned_payload() {
    let fake = fake_s3().await;
    fake.set_put_etag("\"etag-123\"");
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    let etag = store
        .relay_put_object(
            "uploads",
            "guild/streamed.bin",
            RelayPutOptions {
                body: RelayBody::Streamed(chunks(&[b"hello ", b"world"])),
                content_length: 11,
                content_type: Some("application/octet-stream".to_owned()),
                upload_id: Some("upload-1".to_owned()),
                part_number: Some(2),
                timeout_ms: 5_000,
            },
        )
        .await
        .unwrap();

    assert_eq!(Some("\"etag-123\"".to_owned()), etag);
    let (_, uri, headers, body) = fake.last_request();
    assert_eq!("/uploads/guild/streamed.bin", uri.path());
    assert_eq!(Some("partNumber=2&uploadId=upload-1"), uri.query());
    assert_eq!(
        UNSIGNED_PAYLOAD,
        headers.get("x-amz-content-sha256").unwrap()
    );
    assert_eq!("11", headers.get(header::CONTENT_LENGTH).unwrap());
    assert!(headers.get(header::TRANSFER_ENCODING).is_none());
    assert!(
        headers
            .get(header::AUTHORIZATION)
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/")
    );
    assert_eq!(b"hello world", body.as_ref());
}

#[tokio::test]
async fn relay_put_s3_fails_when_stream_ends_short() {
    let fake = fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    let result = store
        .relay_put_object(
            "uploads",
            "guild/short.bin",
            RelayPutOptions {
                body: RelayBody::Streamed(chunks(&[b"only"])),
                content_length: 32,
                content_type: None,
                upload_id: None,
                part_number: None,
                timeout_ms: 5_000,
            },
        )
        .await;
    assert!(result.is_err());
}
