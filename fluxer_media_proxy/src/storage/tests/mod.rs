// SPDX-License-Identifier: AGPL-3.0-or-later

mod keys;
mod local;
mod relay;
mod response_body;
mod s3;
mod s3_read_endpoint;

use crate::{
    config::{
        BucketStyle, Config, DeploymentMode, MediaServingConfig, StorageBackend, StorageConfig,
        UploadRelayConfig,
    },
    constants,
    metrics::{Metrics, http_client::HTTPClientMetrics, storage::StorageMetrics},
    secret::{SecretBytes, SecretString},
    storage::Store,
};
use axum::response::Response;
use bytes::Bytes;
use http::{HeaderMap, HeaderName, HeaderValue, Method, StatusCode, header};
use parking_lot::Mutex;
use std::{collections::HashMap, path::Path, sync::Arc, time::Duration};

pub(crate) type CapturedRequest = (Method, http::Uri, HeaderMap, Bytes);

fn test_config(root: &Path) -> Config {
    Config {
        node_env: "test".to_owned(),
        bind_host: "127.0.0.1".to_owned(),
        port: 0,
        secret_key: SecretString::new("secret".to_owned()),
        public_endpoint: None,
        mode: DeploymentMode::Mp,
        read_only: false,
        shutdown_grace_ms: 0,
        socket_io_timeout_ms: 0,
        storage: StorageConfig {
            backend: StorageBackend::Local,
            root: root.display().to_string(),
            s3_endpoint: String::new(),
            s3_region: "us-east-1".to_owned(),
            s3_access_key_id: String::new(),
            s3_secret_access_key: String::new(),
            s3_session_token: String::new(),
            s3_force_path_style: true,
            s3_read_endpoint: None,
            s3_read_bucket: "cdn".to_owned(),
            s3_read_bucket_style: BucketStyle::Path,
            s3_read_signed: false,
            bucket_cdn: "cdn".to_owned(),
            bucket_uploads: "uploads".to_owned(),
            bucket_static: "static".to_owned(),
        },
        media: MediaServingConfig {
            max_native_transforms: 2,
            worker_queue_capacity: 16,
            nsfw_service_endpoint: String::new(),
            nsfw_threshold: 0.85,
            transform_cache_capacity_bytes: 0,
            transform_cache_max_entry_bytes: 0,
            transform_cache_ttl_ms: 0,
            transform_timeout_ms: 1000,
            max_encode_frames: constants::MAX_ANIMATED_FRAMES_DEFAULT,
            max_encode_duration_ms: 30_000,
        },
        upload_relay: UploadRelayConfig {
            secret: SecretBytes::new(Vec::new()),
            max_body_bytes: 1024,
            s3_timeout_ms: 1000,
            buffered_retry_max_bytes: 0,
            buffered_retry_total_bytes: 0,
            spool_dir: std::env::temp_dir(),
            spool_chunk_bytes: 64 * 1024,
            spool_max_total_bytes: 1 << 30,
        },
        bunny_ip_gate_enabled: false,
        bunny_ip_gate_trusted_proxies: Vec::new(),
        bunny_ip_gate_refresh_secs: 3_600,
    }
}

fn store(cfg: Config) -> Store {
    store_with_storage_metrics(cfg, Arc::new(StorageMetrics::new()))
}

fn store_with_storage_metrics(cfg: Config, metrics: Arc<StorageMetrics>) -> Store {
    Store::new(cfg, metrics, Arc::new(HTTPClientMetrics::new()))
}

fn rendered_counter(metrics: &Metrics, name: &str) -> u64 {
    let rendered = metrics.render();
    rendered
        .lines()
        .find_map(|line| line.strip_prefix(name)?.trim().parse().ok())
        .expect("counter is rendered")
}

#[derive(Clone, Default)]
pub(crate) struct FakeObject {
    pub(crate) body: Vec<u8>,
    pub(crate) head_length: Option<u64>,
    pub(crate) etag: Option<String>,
    pub(crate) content_type: Option<String>,
    pub(crate) last_modified: Option<String>,
    pub(crate) status: Option<u16>,
    pub(crate) read_status: Option<u16>,
    pub(crate) delay: Option<Duration>,
}

#[derive(Clone)]
pub(crate) struct FakeS3 {
    endpoint: String,
    objects: Arc<Mutex<HashMap<String, FakeObject>>>,
    requests: Arc<Mutex<Vec<CapturedRequest>>>,
    put_etag: Arc<Mutex<Option<String>>>,
}

impl FakeS3 {
    pub(crate) fn put_object(&self, path: &str, object: FakeObject) {
        self.objects.lock().insert(path.to_owned(), object);
    }

    pub(crate) fn set_put_etag(&self, etag: &str) {
        *self.put_etag.lock() = Some(etag.to_owned());
    }

    pub(crate) fn endpoint(&self) -> &str {
        &self.endpoint
    }

    pub(crate) fn requests(&self) -> Vec<CapturedRequest> {
        self.requests.lock().clone()
    }

    pub(crate) fn last_request(&self) -> CapturedRequest {
        self.requests
            .lock()
            .last()
            .cloned()
            .expect("fake s3 recorded a request")
    }

    pub(crate) fn config(&self, root: &Path) -> Config {
        let mut cfg = test_config(root);
        cfg.storage.backend = StorageBackend::S3;
        cfg.storage.s3_endpoint = self.endpoint.clone();
        cfg.storage.s3_access_key_id = "AKIAIOSFODNN7EXAMPLE".to_owned();
        cfg.storage.s3_secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY".to_owned();
        cfg
    }
}

pub(crate) async fn fake_s3() -> FakeS3 {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let fake = FakeS3 {
        endpoint: format!("http://{addr}"),
        objects: Arc::new(Mutex::new(HashMap::new())),
        requests: Arc::new(Mutex::new(Vec::new())),
        put_etag: Arc::new(Mutex::new(None)),
    };
    let handler_state = fake.clone();
    let app = axum::Router::new().fallback(axum::routing::any(
        move |request: axum::extract::Request| {
            let state = handler_state.clone();
            async move { serve_fake_s3(state, request).await }
        },
    ));
    tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    fake
}

async fn serve_fake_s3(state: FakeS3, request: axum::extract::Request) -> Response {
    let (parts, body) = request.into_parts();
    let bytes = axum::body::to_bytes(body, 1 << 20)
        .await
        .unwrap_or_default();
    state.requests.lock().push((
        parts.method.clone(),
        parts.uri.clone(),
        parts.headers.clone(),
        bytes,
    ));
    if parts.method == Method::PUT {
        let mut response = Response::new(axum::body::Body::empty());
        if let Some(etag) = state.put_etag.lock().clone() {
            response.headers_mut().insert(
                header::ETAG,
                HeaderValue::from_str(&etag).expect("fake etag is ASCII"),
            );
        }
        return response;
    }
    let key = parts.uri.path().trim_start_matches('/').to_owned();
    let Some(object) = state.objects.lock().get(&key).cloned() else {
        return fake_s3_response(
            StatusCode::NOT_FOUND,
            Vec::new(),
            b"<Error>NoSuchKey</Error>",
        );
    };
    if let Some(delay) = object.delay {
        tokio::time::sleep(delay).await;
    }
    if let Some(status) = object.status {
        return fake_s3_response(
            StatusCode::from_u16(status).expect("fake status is valid"),
            Vec::new(),
            b"<Error>InternalError</Error>",
        );
    }
    if let Some(status) = object.read_status
        && parts.method != Method::HEAD
    {
        return fake_s3_response(
            StatusCode::from_u16(status).expect("fake read status is valid"),
            Vec::new(),
            b"<Error>NoSuchKey</Error>",
        );
    }
    let total_length = object.head_length.unwrap_or(object.body.len() as u64);
    let mut headers = vec![(
        header::CONTENT_TYPE,
        object
            .content_type
            .clone()
            .unwrap_or_else(|| "application/octet-stream".to_owned()),
    )];
    if let Some(etag) = object.etag.clone() {
        headers.push((header::ETAG, etag));
    }
    if let Some(last_modified) = object.last_modified.clone() {
        headers.push((header::LAST_MODIFIED, last_modified));
    }
    if let Some(requested) = parts.headers.get(header::IF_MATCH)
        && object.etag.as_deref() != requested.to_str().ok()
    {
        return fake_s3_response(
            StatusCode::PRECONDITION_FAILED,
            Vec::new(),
            b"<Error>PreconditionFailed</Error>",
        );
    }
    if parts.method == Method::HEAD {
        let advertised = vec![0u8; usize::try_from(total_length).expect("fake length fits usize")];
        return fake_s3_response(StatusCode::OK, headers, &advertised);
    }
    let Some(requested_range) = parts.headers.get(header::RANGE) else {
        return fake_s3_response(StatusCode::OK, headers, &object.body);
    };
    let (start, end) = parse_fake_range(requested_range.to_str().expect("range is ASCII"));
    headers.push((
        header::CONTENT_RANGE,
        format!("bytes {start}-{end}/{total_length}"),
    ));
    fake_s3_response(
        StatusCode::PARTIAL_CONTENT,
        headers,
        &object.body[start..=end.min(object.body.len().saturating_sub(1))],
    )
}

fn parse_fake_range(value: &str) -> (usize, usize) {
    let spec = value
        .strip_prefix("bytes=")
        .expect("fake s3 only serves byte ranges");
    let (start, end) = spec.split_once('-').expect("fake range has both bounds");
    (
        start.parse().expect("range start is a number"),
        end.parse().expect("range end is a number"),
    )
}

fn fake_s3_response(
    status: StatusCode,
    headers: Vec<(HeaderName, String)>,
    body: &[u8],
) -> Response {
    let mut response = Response::new(axum::body::Body::from(body.to_vec()));
    *response.status_mut() = status;
    for (name, value) in headers {
        response.headers_mut().insert(
            name,
            HeaderValue::from_str(&value).expect("fake header value is ASCII"),
        );
    }
    response
}
