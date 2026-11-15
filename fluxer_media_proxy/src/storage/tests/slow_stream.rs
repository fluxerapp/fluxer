// SPDX-License-Identifier: AGPL-3.0-or-later

use super::fake_s3;
use crate::{
    metrics::{http_client::HTTPClientMetrics, storage::StorageMetrics},
    storage::Store,
};
use std::{sync::Arc, time::Duration};

async fn slow_s3(chunks: usize, gap: Duration) -> String {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let app = axum::Router::new().fallback(axum::routing::any(move || async move {
        let stream = futures_util::stream::unfold(0usize, move |sent| async move {
            if sent == chunks {
                return None;
            }
            tokio::time::sleep(gap).await;
            Some((
                Ok::<_, std::io::Error>(bytes::Bytes::from_static(b"x")),
                sent + 1,
            ))
        });
        axum::response::Response::builder()
            .header(http::header::CONTENT_LENGTH, chunks.to_string())
            .header(http::header::CONTENT_TYPE, "video/mp4")
            .body(axum::body::Body::from_stream(stream))
            .unwrap()
    }));
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    format!("http://{addr}")
}

async fn read_slow_object(
    socket_io_timeout_ms: u64,
    chunks: usize,
    gap: Duration,
) -> Result<usize, String> {
    let dir = tempfile::tempdir().unwrap();
    let mut cfg = fake_s3().await.config(dir.path());
    cfg.storage.s3_endpoint = slow_s3(chunks, gap).await;
    cfg.socket_io_timeout_ms = socket_io_timeout_ms;
    let store = Store::try_new(
        cfg,
        Arc::new(StorageMetrics::default()),
        Arc::new(HTTPClientMetrics::default()),
    )
    .unwrap();
    let object = store
        .stream_object("fluxer", "attachments/1/2/big.mp4", None)
        .await
        .map_err(|error| error.to_string())?;
    axum::body::to_bytes(object.body, usize::MAX)
        .await
        .map(|bytes| bytes.len())
        .map_err(|error| error.to_string())
}

#[tokio::test]
async fn stored_download_outlives_the_socket_timeout_while_bytes_keep_flowing() {
    let read = read_slow_object(300, 12, Duration::from_millis(100)).await;
    assert_eq!(read, Ok(12));
}

#[tokio::test]
async fn stored_download_still_stops_when_the_store_stalls() {
    let read = read_slow_object(300, 2, Duration::from_millis(900)).await;
    assert!(read.is_err(), "a stalled read must fail, got {read:?}");
}
