// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_hash,
    bunny_ip_gate::{self, BunnyIpGate},
    byte_cache::Cache,
    coalescer::{ByteCoalescer, CoalescerError},
    config::{Config, DeploymentMode},
    constants::{self, AssetExtension, AssetKind},
    disposition, external_path, http_client, http_headers, media_process, metrics, mime,
    output_format, public_net_policy, range,
    request_log::{self, ErrorReason, Stage},
    signing,
    spool::{SpoolError, spool_to_temp},
    storage::{HeadResult, RelayBody, RelayPutOptions, StorageError, Store, StreamObject},
    timed_semaphore::TimedSemaphore,
    upload_relay,
};
use anyhow::Context as _;
use axum::{
    Router,
    body::{Body, to_bytes},
    extract::{Path, Query, State},
    http::{HeaderMap, HeaderValue, Method, Request, StatusCode, header},
    middleware,
    response::Response,
    routing::{any, get, post, put},
};
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;
use http_body_util::BodyExt;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::{
    borrow::Cow,
    collections::HashMap,
    net::SocketAddr,
    sync::{Arc, OnceLock},
    time::{Duration, Instant},
};
use tokio::net::TcpListener;
use tracing::{info, warn};

#[derive(Clone)]
struct AppState {
    cfg: Config,
    store: Store,
    client: http_client::HttpClient,
    nsfw_client: reqwest::Client,
    transform_cache: Arc<Cache>,
    coalescer: Arc<ByteCoalescer>,
    native_transforms: TimedSemaphore,
}

pub async fn run(cfg: Config) -> anyhow::Result<()> {
    metrics::init_global();
    media_process::warmup_vips()?;
    let addr: SocketAddr = format!("{}:{}", cfg.bind_host, cfg.port).parse()?;
    let state = Arc::new(AppState {
        store: Store::try_new(cfg.clone())?,
        client: http_client::build(http_client::Options {
            connect_timeout_ms: cfg.socket_io_timeout_ms.max(1),
            timeout_ms: cfg.socket_io_timeout_ms.max(1),
            restrict_to_public: true,
            ..http_client::Options::default()
        })?,
        nsfw_client: reqwest::Client::builder()
            .connect_timeout(std::time::Duration::from_millis(1_500))
            .pool_idle_timeout(std::time::Duration::from_secs(30))
            .user_agent(constants::OUTBOUND_USER_AGENT)
            .build()?,
        transform_cache: Arc::new(Cache::new(
            cfg.transform_cache_capacity_bytes,
            cfg.transform_cache_max_entry_bytes,
            cfg.transform_cache_ttl_ms,
        )),
        coalescer: Arc::new(ByteCoalescer::new()),
        native_transforms: TimedSemaphore::new(cfg.max_native_transforms),
        cfg,
    });
    let bunny_gate = if state.cfg.bunny_ip_gate_enabled {
        let gate = Arc::new(BunnyIpGate::new(
            bunny_ip_gate::build_refresh_client()?,
            state.cfg.bunny_ip_gate_trusted_proxies.clone(),
        ));
        let count = gate
            .refresh_once()
            .await
            .context("initial bunny ip allowlist fetch failed")?;
        info!(
            count,
            trusted_proxies = state.cfg.bunny_ip_gate_trusted_proxies.len(),
            refresh_secs = state.cfg.bunny_ip_gate_refresh_secs,
            "bunny ip gate enabled"
        );
        Arc::clone(&gate)
            .spawn_background_refresher(Duration::from_secs(state.cfg.bunny_ip_gate_refresh_secs));
        Some(gate)
    } else {
        None
    };
    let mut router = Router::new()
        .route("/_health", get(health))
        .route("/_metrics", get(metrics_handler))
        .route("/_metadata", post(metadata_handler))
        .route("/_thumbnail", post(thumbnail_handler))
        .route("/_frames", post(frames_handler))
        .route("/v1/relay/{*key}", put(relay_put).options(relay_options))
        .fallback(any(catch_all))
        .layer(middleware::from_fn(add_version_header))
        .layer(middleware::from_fn(request_log::trace));
    if let Some(gate) = bunny_gate {
        router = router.layer(middleware::from_fn_with_state(
            gate,
            bunny_ip_gate::gate_middleware,
        ));
    }
    router = router.layer(middleware::from_fn(add_security_header_middleware));
    let app = router.with_state(state);
    let listener = TcpListener::bind(addr).await?;
    info!(%addr, "media proxy listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .with_graceful_shutdown(shutdown_signal())
    .await?;
    Ok(())
}

async fn shutdown_signal() {
    let ctrl_c = async {
        let _ = tokio::signal::ctrl_c().await;
    };
    #[cfg(unix)]
    let terminate = async {
        let Ok(mut sigterm) =
            tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
        else {
            return;
        };
        sigterm.recv().await;
    };
    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();
    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }
}

async fn health() -> &'static str {
    "OK"
}

fn build_version() -> &'static str {
    static BUILD_VERSION: OnceLock<String> = OnceLock::new();
    BUILD_VERSION
        .get_or_init(|| {
            std::env::var("BUILD_VERSION")
                .ok()
                .filter(|value| !value.trim().is_empty())
                .unwrap_or_else(|| "dev".to_owned())
        })
        .as_str()
}

async fn add_version_header(request: Request<Body>, next: middleware::Next) -> Response {
    let mut response = next.run(request).await;
    if let Ok(value) = HeaderValue::from_str(build_version()) {
        response.headers_mut().insert("x-fluxer-version", value);
    }
    response
}

async fn add_security_header_middleware(
    request: Request<Body>,
    next: middleware::Next,
) -> Response {
    let mut response = next.run(request).await;
    http_headers::add_security_headers(response.headers_mut());
    response
}

async fn metrics_handler() -> Response {
    let mut response = Response::new(Body::from(metrics::render()));
    http_headers::add_security_headers(response.headers_mut());
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/plain; version=0.0.4; charset=utf-8"),
    );
    response
        .headers_mut()
        .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-store"));
    response
}

#[derive(Debug, Deserialize)]
struct MetadataRequest {
    version: Option<i64>,
    #[serde(rename = "type")]
    typ: String,
    nsfw: String,
    base64: Option<String>,
    upload_filename: Option<String>,
    filename: Option<String>,
    bucket: Option<String>,
    key: Option<String>,
    url: Option<String>,
    with_base64: Option<bool>,
}

#[derive(Debug, Deserialize)]
struct FramesRequest {
    version: Option<i64>,
    #[serde(rename = "type")]
    typ: String,
    base64: Option<String>,
    upload_filename: Option<String>,
    filename: Option<String>,
    bucket: Option<String>,
    key: Option<String>,
    url: Option<String>,
}

impl FramesRequest {
    fn into_metadata_request(self) -> MetadataRequest {
        MetadataRequest {
            version: self.version,
            typ: self.typ,
            nsfw: "allow".to_owned(),
            base64: self.base64,
            upload_filename: self.upload_filename,
            filename: self.filename,
            bucket: self.bucket,
            key: self.key,
            url: self.url,
            with_base64: None,
        }
    }
}

async fn metadata_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, &app.cfg.secret_key) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, status.canonical_reason().unwrap_or("Bad Request")),
    };
    let req: MetadataRequest = match serde_json::from_slice::<MetadataRequest>(&body) {
        Ok(req) if req.version == Some(2) => req,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let should_scan_nsfw = match req.nsfw.as_str() {
        "block" | "flag" => true,
        "allow" => false,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let mut input = match load_metadata_input(&app, &req).await {
        Ok(input) => input,
        Err(status) => return text(status, status.canonical_reason().unwrap_or("Bad Request")),
    };
    if req.with_base64.unwrap_or(false) && metadata_input_is_svg(&input) {
        input = match rasterize_metadata_svg(&app, input).await {
            Ok(input) => input,
            Err(response) => return response,
        };
    }
    let json = match media_process::metadata_json_with_options(
        &input.data,
        &input.filename,
        media_process::MetadataOptions {
            placeholder: true,
            nsfw: if should_scan_nsfw {
                nsfw_config(&app)
            } else {
                crate::nsfw::Config::disabled()
            },
        },
        &app.nsfw_client,
    )
    .await
    {
        Ok(json) => json,
        Err(err) => {
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "metadata_extraction_failed",
                format!("filename={} err={err:?}", input.filename),
            );
        }
    };
    let mut value: serde_json::Value =
        serde_json::from_str(&json).unwrap_or_else(|_| serde_json::json!({}));
    if req.with_base64.unwrap_or(false) {
        value["base64"] = serde_json::Value::String(general_purpose::STANDARD.encode(&input.data));
    }
    json_response(StatusCode::OK, value.to_string())
}

struct InputData {
    data: Bytes,
    filename: String,
}

fn metadata_input_is_svg(input: &InputData) -> bool {
    mime::sniff(&input.data).mime == "image/svg+xml"
        || image_extension_from_filename(&input.filename) == Some(AssetExtension::Svg)
}

fn replace_image_extension(filename: &str, ext: AssetExtension) -> String {
    let last_slash = filename.rfind('/').map(|idx| idx + 1).unwrap_or(0);
    let last_dot = filename[last_slash..]
        .rfind('.')
        .map(|idx| last_slash + idx);
    match last_dot {
        Some(idx) => format!("{}.{}", &filename[..idx], ext.name()),
        None => format!("{}.{}", filename, ext.name()),
    }
}

async fn rasterize_metadata_svg(
    app: &Arc<AppState>,
    input: InputData,
) -> Result<InputData, Response> {
    let options = media_process::ImageOptions {
        format: AssetExtension::Webp,
        quality: "lossless".to_owned(),
        animated: false,
        deadline_ms: Some(metrics::now_ms() + app.cfg.transform_timeout_ms as i64),
        max_encode_frames: Some(app.cfg.max_encode_frames),
        max_encode_duration_ms: Some(app.cfg.max_encode_duration_ms),
        ..Default::default()
    };
    match run_transform(app, input.data, options).await {
        Ok(media) => Ok(InputData {
            data: media.bytes.into(),
            filename: replace_image_extension(&input.filename, AssetExtension::Webp),
        }),
        Err(err) if transform_error_is_timeout(&err) => Err(text_with_source(
            StatusCode::GATEWAY_TIMEOUT,
            "Gateway Timeout",
            "metadata_svg_rasterize_timeout",
            input.filename,
        )),
        Err(err) => Err(text_with_source(
            StatusCode::BAD_REQUEST,
            "Bad Request",
            "metadata_svg_rasterize_failed",
            format!("filename={} err={err:?}", input.filename),
        )),
    }
}

async fn load_metadata_input(
    app: &AppState,
    req: &MetadataRequest,
) -> Result<InputData, StatusCode> {
    match req.typ.as_str() {
        "base64" => {
            let raw = req.base64.as_deref().ok_or(StatusCode::BAD_REQUEST)?;
            let b64 = raw.rsplit_once(',').map(|(_, v)| v).unwrap_or(raw);
            if b64.len() > constants::MAX_INTERNAL_REQUEST_BODY_BYTES {
                warn!(reason = "metadata_base64_too_large", len = b64.len());
                return Err(StatusCode::BAD_REQUEST);
            }
            let data = general_purpose::STANDARD.decode(b64).map_err(|err| {
                warn!(reason = "metadata_base64_decode", ?err);
                StatusCode::BAD_REQUEST
            })?;
            if data.len() > constants::MAX_MEDIA_PROXY_BYTES {
                warn!(reason = "metadata_decoded_too_large", len = data.len());
                return Err(StatusCode::BAD_REQUEST);
            }
            Ok(InputData {
                data: Bytes::from(data),
                filename: req
                    .filename
                    .clone()
                    .unwrap_or_else(|| "inline.bin".to_owned()),
            })
        }
        "upload" => {
            let key = req
                .upload_filename
                .as_deref()
                .ok_or(StatusCode::BAD_REQUEST)?;
            let object = app
                .store
                .read_object(&app.cfg.bucket_uploads, key)
                .await
                .map_err(|err| {
                    warn!(reason = "metadata_upload_read", key, %err);
                    StatusCode::BAD_REQUEST
                })?;
            Ok(InputData {
                data: object.data,
                filename: req.filename.clone().unwrap_or_else(|| key.to_owned()),
            })
        }
        "s3" => {
            let bucket = req.bucket.as_deref().ok_or(StatusCode::BAD_REQUEST)?;
            let key = req.key.as_deref().ok_or(StatusCode::BAD_REQUEST)?;
            let object = app.store.read_object(bucket, key).await.map_err(|err| {
                warn!(reason = "metadata_s3_read", bucket, key, %err);
                StatusCode::BAD_REQUEST
            })?;
            Ok(InputData {
                data: object.data,
                filename: req.filename.clone().unwrap_or_else(|| key.to_owned()),
            })
        }
        "external" => {
            let url = req.url.as_deref().ok_or(StatusCode::BAD_REQUEST)?;
            let fetched = fetch_external(app, url).await.map_err(|err| {
                warn!(reason = "metadata_external_fetch", url, ?err);
                match err {
                    ExternalFetchError::BlockedUrl => StatusCode::BAD_REQUEST,
                    ExternalFetchError::PayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
                    ExternalFetchError::TooManyRedirects | ExternalFetchError::FetchFailed => {
                        StatusCode::BAD_GATEWAY
                    }
                }
            })?;
            if !fetched.status.is_success() {
                warn!(
                    reason = "metadata_external_status",
                    url,
                    status = fetched.status.as_u16()
                );
                return Err(map_internal_metadata_upstream_status(fetched.status));
            }
            let data = fetched
                .body
                .into_buffered(&fetched.url)
                .await
                .map_err(|err| {
                    warn!(reason = "metadata_external_body", url, ?err);
                    match err {
                        ExternalFetchError::PayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
                        _ => StatusCode::BAD_GATEWAY,
                    }
                })?;
            Ok(InputData {
                data,
                filename: req
                    .filename
                    .clone()
                    .unwrap_or_else(|| url_filename(&fetched.url)),
            })
        }
        _ => Err(StatusCode::BAD_REQUEST),
    }
}

#[derive(Debug, Deserialize)]
struct UploadFileRequest {
    upload_filename: String,
}

async fn thumbnail_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, &app.cfg.secret_key) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, "Bad Request"),
    };
    let req: UploadFileRequest = match serde_json::from_slice(&body) {
        Ok(req) => req,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let object = match app
        .store
        .read_object(&app.cfg.bucket_uploads, &req.upload_filename)
        .await
    {
        Ok(object) => object,
        Err(err) => return storage_error_response(&req.upload_filename, err),
    };
    let media = if mime::category(&object.content_type) == Some(mime::Category::Video) {
        match media_process::extract_video_thumbnail(&object.data, AssetExtension::Webp) {
            Ok(media) => media,
            Err(err) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "video_thumbnail_failed",
                    err,
                );
            }
        }
    } else {
        let options = media_process::ImageOptions {
            width: Some(512),
            height: Some(512),
            format: AssetExtension::Webp,
            cover_crop: false,
            ..Default::default()
        };
        match run_transform(&app, object.data.clone(), options).await {
            Ok(media) => media,
            Err(err) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "image_thumbnail_failed",
                    err,
                );
            }
        }
    };
    media_response(
        Method::GET,
        media.bytes.into(),
        media.content_type,
        None,
        None,
    )
}

async fn frames_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, &app.cfg.secret_key) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let req: FramesRequest = match serde_json::from_slice::<FramesRequest>(&body) {
        Ok(req) if req.version.is_none_or(|v| v == 2) => req,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let req = req.into_metadata_request();
    let input = match load_metadata_input(&app, &req).await {
        Ok(input) => input,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    match media_process::extract_video_thumbnail(&input.data, AssetExtension::Jpeg) {
        Ok(frame) => {
            let encoded = general_purpose::STANDARD.encode(frame.bytes);
            json_response(
                StatusCode::OK,
                format!(
                    "{{\"frames\":[{{\"timestamp\":0,\"mime_type\":\"image/jpeg\",\"base64\":\"{}\"}}]}}",
                    encoded
                ),
            )
        }
        Err(_) => json_response(StatusCode::OK, "{\"frames\":[]}".to_owned()),
    }
}

async fn relay_options() -> Response {
    let mut response = Response::new(Body::empty());
    *response.status_mut() = StatusCode::NO_CONTENT;
    http_headers::add_security_headers(response.headers_mut());
    relay_cors(response.headers_mut());
    response.headers_mut().insert(
        header::ACCESS_CONTROL_MAX_AGE,
        HeaderValue::from_static("600"),
    );
    response
}

async fn relay_put(
    State(app): State<Arc<AppState>>,
    Path(key): Path<String>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if app.cfg.mode != DeploymentMode::Upload {
        return text(StatusCode::NOT_FOUND, "Not Found");
    }
    let key = external_path::percent_decode_string(&key, false);
    let Some(token_raw) = params.get("t") else {
        return relay_error(upload_relay::RelayError::MissingToken);
    };
    let token = match upload_relay::decode_token(
        token_raw,
        &app.cfg.upload_relay_secret,
        upload_relay::now_unix(),
    ) {
        Ok(token) => token,
        Err(err) => return relay_error(upload_relay::map_token_error(err)),
    };
    let part_number =
        match upload_relay::query_part_number(params.get("partNumber").map(String::as_str)) {
            Ok(part_number) => part_number,
            Err(err) => return relay_error(err),
        };
    let content_length = headers
        .get(header::CONTENT_LENGTH)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.parse::<u64>().ok());
    if let Err(err) = upload_relay::validate_relay_request(
        &token,
        upload_relay::RelayRequest {
            uploads_bucket: &app.cfg.bucket_uploads,
            request_key: &key,
            request_method: request.method(),
            query_upload_id: params.get("uploadId").map(String::as_str),
            query_part_number: part_number,
            content_length,
            max_body_bytes: app.cfg.upload_relay_max_body_bytes,
        },
    ) {
        return relay_error(err);
    }
    let (body, body_length, client_failure) = match content_length {
        Some(declared) => {
            let (tx, rx) = tokio::sync::mpsc::channel(RELAY_STREAM_BUFFER_FRAMES);
            let failure = Arc::new(OnceLock::new());
            spawn_relay_body_feeder(request.into_body(), declared, tx, Arc::clone(&failure));
            (RelayBody::Streamed(rx), declared, Some(failure))
        }
        None => {
            let body_length_limit = token.mb.min(app.cfg.upload_relay_max_body_bytes);
            let spooled = match spool_to_temp(
                request.into_body(),
                content_length,
                body_length_limit,
                &app.cfg.upload_relay_spool_dir,
                app.cfg.upload_relay_spool_chunk_bytes,
                app.cfg.upload_relay_spool_max_total_bytes,
            )
            .await
            {
                Ok(spooled) => spooled,
                Err(SpoolError::PayloadTooLarge) => {
                    return relay_error(upload_relay::RelayError::PayloadTooLarge);
                }
                Err(SpoolError::PayloadShortRead) | Err(SpoolError::Body(_)) => {
                    return relay_error(upload_relay::RelayError::ClientUploadFailed);
                }
                Err(SpoolError::BudgetExhausted) => {
                    return relay_error(upload_relay::RelayError::UpstreamRetryable);
                }
                Err(SpoolError::Io(_)) => {
                    return relay_error(upload_relay::RelayError::InternalError);
                }
            };
            let (file, spooled_length) = spooled.into_parts();
            (RelayBody::Spooled(file), spooled_length, None)
        }
    };
    let content_type = token.ct.clone().or_else(|| {
        headers
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .map(ToOwned::to_owned)
    });
    let timeout_ms = if client_failure.is_some() {
        app.cfg.upload_relay_s3_timeout_ms.saturating_add(
            body_length
                .div_ceil(RELAY_STREAM_MIN_CLIENT_BYTES_PER_SEC)
                .saturating_mul(1000),
        )
    } else {
        app.cfg.upload_relay_s3_timeout_ms
    };
    let options = RelayPutOptions {
        body,
        content_length: body_length,
        content_type,
        upload_id: params.get("uploadId").cloned(),
        part_number,
        timeout_ms,
    };
    match app
        .store
        .relay_put_object(&app.cfg.bucket_uploads, &key, options)
        .await
    {
        Ok(etag) => {
            let mut response = Response::new(Body::empty());
            *response.status_mut() = StatusCode::OK;
            http_headers::add_security_headers(response.headers_mut());
            relay_cors(response.headers_mut());
            if let Some(etag) = etag {
                response.headers_mut().insert(
                    header::ETAG,
                    HeaderValue::from_str(&etag).unwrap_or_else(|_| HeaderValue::from_static("")),
                );
            }
            response
        }
        Err(err) => {
            if let Some(client_err) = client_failure.as_ref().and_then(|failure| failure.get()) {
                return relay_error(*client_err);
            }
            warn!(error = %err, "upload relay upstream S3 PUT failed");
            relay_error(upload_relay::RelayError::UpstreamS3Error)
        }
    }
}

const RELAY_STREAM_BUFFER_FRAMES: usize = 8;
const RELAY_STREAM_MIN_CLIENT_BYTES_PER_SEC: u64 = 16 * 1024;

fn spawn_relay_body_feeder(
    mut body: Body,
    declared_length: u64,
    tx: tokio::sync::mpsc::Sender<Result<Bytes, std::io::Error>>,
    failure: Arc<OnceLock<upload_relay::RelayError>>,
) {
    tokio::spawn(async move {
        let mut written: u64 = 0;
        while let Some(frame_result) = body.frame().await {
            let frame = match frame_result {
                Ok(frame) => frame,
                Err(_) => {
                    let _ = failure.set(upload_relay::RelayError::ClientUploadFailed);
                    let _ = tx
                        .send(Err(std::io::Error::new(
                            std::io::ErrorKind::ConnectionAborted,
                            "client body read failed",
                        )))
                        .await;
                    return;
                }
            };
            let Ok(chunk) = frame.into_data() else {
                continue;
            };
            if chunk.is_empty() {
                continue;
            }
            let next = written.saturating_add(chunk.len() as u64);
            if next > declared_length {
                let _ = failure.set(upload_relay::RelayError::PayloadTooLarge);
                let _ = tx
                    .send(Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "payload exceeded declared length",
                    )))
                    .await;
                return;
            }
            written = next;
            if tx.send(Ok(chunk)).await.is_err() {
                return;
            }
        }
        if written != declared_length {
            let _ = failure.set(upload_relay::RelayError::ClientUploadFailed);
            let _ = tx
                .send(Err(std::io::Error::new(
                    std::io::ErrorKind::UnexpectedEof,
                    "payload shorter than declared length",
                )))
                .await;
        }
    });
}

async fn catch_all(
    State(app): State<Arc<AppState>>,
    Query(params): Query<HashMap<String, String>>,
    request: Request<Body>,
) -> Response {
    let method = request.method().clone();
    if method != Method::GET && method != Method::HEAD {
        return text(StatusCode::METHOD_NOT_ALLOWED, "Method Not Allowed");
    }
    let path = request.uri().path().to_owned();
    if app.cfg.mode == DeploymentMode::Static {
        let key = decode_storage_key(&path);
        return serve_stored_raw(
            &app,
            method,
            &app.cfg.bucket_static,
            &key,
            request.headers(),
        )
        .await;
    }
    if let Some(rest) = path.strip_prefix("/external/") {
        return serve_external(&app, method, rest, &params, request.headers()).await;
    }
    if path.starts_with("/attachments/") {
        let key = decode_storage_key(&path);
        return serve_attachment(&app, method, &key, &params, request.headers()).await;
    }
    if path.starts_with("/themes/") && path.ends_with(".css") {
        let key = decode_storage_key(&path);
        return serve_stored_with_override(
            &app,
            method,
            &app.cfg.bucket_cdn,
            &key,
            "text/css; charset=utf-8",
            request.headers(),
        )
        .await;
    }
    if let Some(key) = parse_entrance_sound_path(&path) {
        return serve_stored_raw(&app, method, &app.cfg.bucket_cdn, &key, request.headers()).await;
    }
    if let Some(asset) = parse_guild_member_asset_path(&path) {
        return serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_simple_asset_path(&path, AssetKind::Emoji) {
        return serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_simple_asset_path(&path, AssetKind::Sticker) {
        return serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_standard_asset_path(&path) {
        return serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    text(StatusCode::NOT_FOUND, "Not Found")
}

struct ParsedAssetPath {
    storage_key: String,
    original_ext: AssetExtension,
    hash: String,
    kind: AssetKind,
    forced_output_format: Option<AssetExtension>,
}

fn parse_standard_asset_path(path: &str) -> Option<ParsedAssetPath> {
    let mut parts = path.trim_start_matches('/').split('/');
    let prefix = parts.next()?;
    let id = parts.next()?;
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let kind = match prefix {
        "avatars" => AssetKind::Avatar,
        "icons" => AssetKind::GuildIcon,
        "branding" => AssetKind::GuildIcon,
        "banners" => AssetKind::Banner,
        "splashes" => AssetKind::Splash,
        "embed-splashes" => AssetKind::EmbedSplash,
        _ => return None,
    };
    if id.is_empty() {
        return None;
    }
    let parsed = parse_asset_filename(filename)?;
    let storage_hash = asset_hash::strip_animation_prefix(parsed.hash);
    Some(ParsedAssetPath {
        storage_key: format!("{prefix}/{id}/{storage_hash}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: None,
    })
}

fn parse_guild_member_asset_path(path: &str) -> Option<ParsedAssetPath> {
    let mut parts = path.trim_start_matches('/').split('/');
    if parts.next()? != "guilds" {
        return None;
    }
    let guild_id = parts.next()?;
    if parts.next()? != "users" {
        return None;
    }
    let user_id = parts.next()?;
    let prefix = parts.next()?;
    let filename = parts.next()?;
    if guild_id.is_empty() || user_id.is_empty() || parts.next().is_some() {
        return None;
    }
    let kind = match prefix {
        "avatars" => AssetKind::Avatar,
        "banners" => AssetKind::Banner,
        _ => return None,
    };
    let parsed = parse_asset_filename(filename)?;
    let storage_hash = asset_hash::strip_animation_prefix(parsed.hash);
    Some(ParsedAssetPath {
        storage_key: format!("guilds/{guild_id}/users/{user_id}/{prefix}/{storage_hash}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: None,
    })
}

fn parse_simple_asset_path(path: &str, kind: AssetKind) -> Option<ParsedAssetPath> {
    let expected_prefix = match kind {
        AssetKind::Emoji => "emojis",
        AssetKind::Sticker => "stickers",
        _ => return None,
    };
    let mut parts = path.trim_start_matches('/').split('/');
    let prefix = parts.next()?;
    if prefix != expected_prefix {
        return None;
    }
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let parsed = parse_asset_filename(filename)?;
    let id_no_ext = filename.split_once('.')?.0;
    if id_no_ext.is_empty() {
        return None;
    }
    Some(ParsedAssetPath {
        storage_key: format!("{prefix}/{id_no_ext}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: (kind == AssetKind::Sticker).then_some(AssetExtension::Webp),
    })
}

fn parse_entrance_sound_path(path: &str) -> Option<String> {
    let mut parts = path.trim_start_matches('/').split('/');
    if parts.next()? != "entrance-sounds" {
        return None;
    }
    let user_id = parts.next()?;
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    if user_id.is_empty() || !user_id.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let (hash, ext) = filename.split_once('.')?;
    if hash.is_empty() || !hash.bytes().all(|b| b.is_ascii_alphanumeric()) {
        return None;
    }
    if !matches!(ext, "mp3" | "ogg" | "m4a" | "wav") {
        return None;
    }
    Some(format!("entrance-sounds/{user_id}/{filename}"))
}

struct ParsedAssetFilename<'a> {
    hash: &'a str,
    ext: AssetExtension,
}

fn parse_asset_filename(filename: &str) -> Option<ParsedAssetFilename<'_>> {
    let (hash, ext_raw) = filename.split_once('.')?;
    if hash.is_empty() || ext_raw.is_empty() || ext_raw.contains('.') {
        return None;
    }
    if !hash.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        return None;
    }
    if !ext_raw.bytes().all(|b| b.is_ascii_alphanumeric()) {
        return None;
    }
    Some(ParsedAssetFilename {
        hash,
        ext: AssetExtension::parse(ext_raw)?,
    })
}

fn asset_filename_hint(asset: &ParsedAssetPath) -> String {
    let hash = asset_hash::strip_animation_prefix(&asset.hash);
    format!("{hash}.{}", asset.original_ext.name())
}

async fn serve_external(
    app: &Arc<AppState>,
    method: Method,
    rest: &str,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let Some((sig, proxy_path)) = rest.split_once('/') else {
        return text(StatusCode::BAD_REQUEST, "Bad Request");
    };
    if !signing::verify_signature(proxy_path, sig, app.cfg.secret_key.as_bytes()) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let url = match external_path::reconstruct_original_url(proxy_path) {
        Ok(url) => url,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let url_ext_is_svg =
        image_extension_from_filename(&url_filename(&url)) == Some(AssetExtension::Svg);
    let wants_transform = url_ext_is_svg
        || params.contains_key("width")
        || params.contains_key("height")
        || params.contains_key("format")
        || params.contains_key("quality")
        || animated_param(params, false);
    let client_range = headers
        .get(header::RANGE)
        .and_then(|v| v.to_str().ok())
        .and_then(|raw| raw.strip_prefix("bytes="))
        .filter(|rv| !rv.is_empty() && rv.bytes().all(|b| b.is_ascii_graphic()));
    let forward_range = if wants_transform { None } else { client_range };
    let allow_stream = !wants_transform;
    let mut fetched = match fetch_external_with_range(app, &url, forward_range, allow_stream).await
    {
        Ok(fetched) if fetched.status.is_success() => fetched,
        Ok(fetched) => {
            return text_with_source(
                map_upstream_status(fetched.status),
                "Upstream fetch failed",
                "external_upstream_status",
                format!("url={url} upstream_status={}", fetched.status.as_u16()),
            );
        }
        Err(ExternalFetchError::BlockedUrl) => {
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "external_blocked_url",
                &url,
            );
        }
        Err(ExternalFetchError::PayloadTooLarge) => {
            return text_with_source(
                StatusCode::PAYLOAD_TOO_LARGE,
                "Payload Too Large",
                "external_payload_too_large",
                &url,
            );
        }
        Err(err @ ExternalFetchError::TooManyRedirects)
        | Err(err @ ExternalFetchError::FetchFailed) => {
            return text_with_source(
                StatusCode::BAD_GATEWAY,
                "Bad Gateway",
                "external_fetch_failed",
                format!("url={url} err={err:?}"),
            );
        }
    };
    if forward_range.is_some()
        && fetched.status == StatusCode::PARTIAL_CONTENT
        && is_svg_content_type(&fetched.content_type)
    {
        fetched = match fetch_external_with_range(app, &url, None, false).await {
            Ok(fetched) if fetched.status.is_success() => fetched,
            Ok(fetched) => {
                return text_with_source(
                    map_upstream_status(fetched.status),
                    "Upstream fetch failed",
                    "external_upstream_status",
                    format!("url={url} upstream_status={}", fetched.status.as_u16()),
                );
            }
            Err(ExternalFetchError::BlockedUrl) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "external_blocked_url",
                    &url,
                );
            }
            Err(ExternalFetchError::PayloadTooLarge) => {
                return text_with_source(
                    StatusCode::PAYLOAD_TOO_LARGE,
                    "Payload Too Large",
                    "external_payload_too_large",
                    &url,
                );
            }
            Err(err @ ExternalFetchError::TooManyRedirects)
            | Err(err @ ExternalFetchError::FetchFailed) => {
                return text_with_source(
                    StatusCode::BAD_GATEWAY,
                    "Bad Gateway",
                    "external_fetch_failed",
                    format!("url={url} err={err:?}"),
                );
            }
        };
    }
    let filename = url_filename(&fetched.url);
    let requested_download = bool_param(params, "download", false);
    if forward_range.is_some() && fetched.status == StatusCode::PARTIAL_CONTENT {
        let disposition =
            content_disposition_header(&fetched.content_type, requested_download, Some(&filename));
        return external_partial_response(method, fetched, Some(disposition));
    }
    let FetchedExternal {
        url: fetched_url,
        body,
        content_type,
        ..
    } = fetched;
    let data = match body {
        ExternalBody::Streaming {
            response,
            content_length,
        } if client_range.is_none() => {
            let disposition =
                content_disposition_header(&content_type, requested_download, Some(&filename));
            return external_streaming_response(
                method,
                response,
                content_length,
                &content_type,
                Some(disposition),
            );
        }
        body => match body.into_buffered(&fetched_url).await {
            Ok(data) => data,
            Err(ExternalFetchError::PayloadTooLarge) => {
                return text_with_source(
                    StatusCode::PAYLOAD_TOO_LARGE,
                    "Payload Too Large",
                    "external_payload_too_large",
                    &fetched_url,
                );
            }
            Err(err) => {
                return text_with_source(
                    StatusCode::BAD_GATEWAY,
                    "Bad Gateway",
                    "external_fetch_failed",
                    format!("url={fetched_url} err={err:?}"),
                );
            }
        },
    };
    serve_bytes_or_transform(
        app,
        ServeBytesRequest {
            method,
            data,
            content_type,
            cache_identity: &fetched_url,
            filename: &filename,
            route: TransformRoute::External,
            params,
            headers,
        },
    )
    .await
}

struct FetchedExternal {
    url: String,
    status: StatusCode,
    body: ExternalBody,
    content_type: String,
    content_range: Option<String>,
}

enum ExternalBody {
    Buffered(Bytes),
    Streaming {
        response: reqwest::Response,
        content_length: u64,
    },
}

impl ExternalBody {
    async fn into_buffered(self, url: &str) -> Result<Bytes, ExternalFetchError> {
        match self {
            Self::Buffered(data) => Ok(data),
            Self::Streaming { response, .. } => buffer_external_response(response, url).await,
        }
    }
}

fn external_stream_length(
    allow_stream: bool,
    content_length: Option<u64>,
    content_type: &str,
) -> Option<u64> {
    if !allow_stream {
        return None;
    }
    let len = content_length?;
    if len > constants::MAX_MEDIA_PROXY_BYTES as u64 {
        return None;
    }
    if !content_type_is_trustworthy(content_type) {
        return None;
    }
    if is_svg_content_type(content_type) {
        return None;
    }
    Some(len)
}

fn external_partial_response(
    method: Method,
    fetched: FetchedExternal,
    disposition: Option<String>,
) -> Response {
    let FetchedExternal {
        body,
        content_type,
        content_range,
        ..
    } = fetched;
    let (body, body_len) = match body {
        ExternalBody::Buffered(data) => {
            let body_len = data.len() as u64;
            let body = if method == Method::HEAD {
                Body::empty()
            } else {
                Body::from(data)
            };
            (body, body_len)
        }
        ExternalBody::Streaming {
            response,
            content_length,
        } => {
            let body = if method == Method::HEAD {
                Body::empty()
            } else {
                Body::from_stream(response.bytes_stream())
            };
            (body, content_length)
        }
    };
    let mut response = Response::new(body);
    *response.status_mut() = StatusCode::PARTIAL_CONTENT;
    http_headers::add_media_headers(
        response.headers_mut(),
        usize::try_from(body_len).unwrap_or(constants::MAX_MEDIA_PROXY_BYTES),
        &content_type,
        None,
    );
    response
        .headers_mut()
        .insert(header::CONTENT_LENGTH, HeaderValue::from(body_len));
    if let Some(cr) = content_range.as_deref()
        && let Ok(value) = HeaderValue::from_str(cr)
    {
        response.headers_mut().insert(header::CONTENT_RANGE, value);
    }
    if let Some(disposition) = disposition
        && let Ok(value) = HeaderValue::from_str(&disposition)
    {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

fn external_streaming_response(
    method: Method,
    response: reqwest::Response,
    content_length: u64,
    content_type: &str,
    disposition: Option<String>,
) -> Response {
    let body = if method == Method::HEAD {
        Body::empty()
    } else {
        Body::from_stream(response.bytes_stream())
    };
    let mut http_response = Response::new(body);
    *http_response.status_mut() = StatusCode::OK;
    http_headers::add_media_headers(
        http_response.headers_mut(),
        usize::try_from(content_length).unwrap_or(constants::MAX_MEDIA_PROXY_BYTES),
        content_type,
        None,
    );
    http_response
        .headers_mut()
        .insert(header::CONTENT_LENGTH, HeaderValue::from(content_length));
    if let Some(disposition) = disposition
        && let Ok(value) = HeaderValue::from_str(&disposition)
    {
        http_response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    http_response
}

#[derive(Debug)]
enum ExternalFetchError {
    BlockedUrl,
    PayloadTooLarge,
    TooManyRedirects,
    FetchFailed,
}

async fn fetch_external(app: &AppState, url: &str) -> Result<FetchedExternal, ExternalFetchError> {
    fetch_external_with_range(app, url, None, false).await
}

async fn fetch_external_with_range(
    app: &AppState,
    url: &str,
    range: Option<&str>,
    allow_stream: bool,
) -> Result<FetchedExternal, ExternalFetchError> {
    let start_ms = metrics::now_ms();
    let result = fetch_external_inner(app, url, range, allow_stream).await;
    request_log::record_stage(Stage::Fetch, (metrics::now_ms() - start_ms).max(0) as u64);
    result
}

async fn fetch_external_inner(
    app: &AppState,
    url: &str,
    range: Option<&str>,
    allow_stream: bool,
) -> Result<FetchedExternal, ExternalFetchError> {
    let mut current_url = url.to_owned();
    let mut visited: Vec<String> = Vec::new();
    for _ in 0..=5 {
        if visited.iter().any(|seen| seen == &current_url) {
            warn!(url = %current_url, "redirect loop detected");
            return Err(ExternalFetchError::TooManyRedirects);
        }
        visited.push(current_url.clone());
        if let Err(err) = public_net_policy::validate_url(&current_url) {
            warn!(?err, url = %current_url, "blocked external fetch");
            metrics::GLOBAL
                .blocked_url_attempts
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            return Err(ExternalFetchError::BlockedUrl);
        }
        let mut request = app.client.get(&current_url);
        if let Some(rv) = range {
            request = request.header(header::RANGE, format!("bytes={rv}"));
        }
        let response = request.send().await.map_err(|err| {
            warn!(url = %current_url, %err, "external send failed");
            ExternalFetchError::FetchFailed
        })?;
        let status = response.status();
        if is_redirect_status(status) {
            let Some(location) = response
                .headers()
                .get(header::LOCATION)
                .and_then(|value| value.to_str().ok())
            else {
                warn!(url = %current_url, status = status.as_u16(), "redirect missing Location");
                return Err(ExternalFetchError::FetchFailed);
            };
            current_url =
                public_net_policy::resolve_redirect(&current_url, location).map_err(|err| {
                    warn!(url = %current_url, %location, ?err, "redirect target blocked");
                    ExternalFetchError::BlockedUrl
                })?;
            continue;
        }
        let content_type = response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_owned();
        let content_range = response
            .headers()
            .get(header::CONTENT_RANGE)
            .and_then(|value| value.to_str().ok())
            .map(ToOwned::to_owned);
        if let Some(len) = response.content_length()
            && len > constants::MAX_MEDIA_PROXY_BYTES as u64
        {
            warn!(url = %current_url, len, "external payload too large");
            return Err(ExternalFetchError::PayloadTooLarge);
        }
        if status.is_success()
            && let Some(content_length) =
                external_stream_length(allow_stream, response.content_length(), &content_type)
        {
            return Ok(FetchedExternal {
                url: current_url,
                status,
                body: ExternalBody::Streaming {
                    response,
                    content_length,
                },
                content_type,
                content_range,
            });
        }
        let data = buffer_external_response(response, &current_url).await?;
        return Ok(FetchedExternal {
            url: current_url,
            status,
            body: ExternalBody::Buffered(data),
            content_type,
            content_range,
        });
    }
    Err(ExternalFetchError::TooManyRedirects)
}

async fn buffer_external_response(
    mut response: reqwest::Response,
    url: &str,
) -> Result<Bytes, ExternalFetchError> {
    let initial_capacity = response
        .content_length()
        .map(|len| len.min(constants::MAX_MEDIA_PROXY_BYTES as u64) as usize)
        .unwrap_or(0);
    let mut buf: Vec<u8> = Vec::with_capacity(initial_capacity);
    while let Some(chunk) = response.chunk().await.map_err(|err| {
        warn!(url = %url, %err, "external body read failed");
        ExternalFetchError::FetchFailed
    })? {
        let Some(next_len) = buf
            .len()
            .checked_add(chunk.len())
            .filter(|len| *len <= constants::MAX_MEDIA_PROXY_BYTES)
        else {
            warn!(url = %url, "external payload too large");
            return Err(ExternalFetchError::PayloadTooLarge);
        };
        debug_assert!(next_len <= constants::MAX_MEDIA_PROXY_BYTES);
        buf.extend_from_slice(&chunk);
    }
    Ok(Bytes::from(buf))
}

fn map_upstream_status(status: StatusCode) -> StatusCode {
    match status.as_u16() {
        400 => StatusCode::BAD_REQUEST,
        401 => StatusCode::UNAUTHORIZED,
        403 => StatusCode::FORBIDDEN,
        404 => StatusCode::NOT_FOUND,
        405 => StatusCode::METHOD_NOT_ALLOWED,
        406 => StatusCode::NOT_ACCEPTABLE,
        408 => StatusCode::REQUEST_TIMEOUT,
        409 => StatusCode::CONFLICT,
        410 => StatusCode::GONE,
        411 => StatusCode::LENGTH_REQUIRED,
        412 => StatusCode::PRECONDITION_FAILED,
        413 => StatusCode::PAYLOAD_TOO_LARGE,
        414 => StatusCode::URI_TOO_LONG,
        415 => StatusCode::UNSUPPORTED_MEDIA_TYPE,
        416 => StatusCode::RANGE_NOT_SATISFIABLE,
        428 => StatusCode::from_u16(428).expect("428 is a valid status code"),
        429 => StatusCode::TOO_MANY_REQUESTS,
        _ => StatusCode::BAD_GATEWAY,
    }
}

fn map_internal_metadata_upstream_status(status: StatusCode) -> StatusCode {
    match status.as_u16() {
        429 => StatusCode::SERVICE_UNAVAILABLE,
        _ => map_upstream_status(status),
    }
}

fn is_redirect_status(status: StatusCode) -> bool {
    matches!(
        status,
        StatusCode::MOVED_PERMANENTLY
            | StatusCode::FOUND
            | StatusCode::SEE_OTHER
            | StatusCode::TEMPORARY_REDIRECT
            | StatusCode::PERMANENT_REDIRECT
    )
}

async fn serve_asset_image(
    app: &Arc<AppState>,
    method: Method,
    asset: ParsedAssetPath,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let requested_download = bool_param(params, "download", false);
    let asset_filename = asset_filename_hint(&asset);
    let size = constants::parse_image_size(params.get("size").map(String::as_str));
    let selected = output_format::select_url_variant(output_format::Input {
        kind: asset.kind,
        original: asset.original_ext,
        requested_size: Some(size),
        manual_format_override: asset
            .forced_output_format
            .or_else(|| asset_manual_format_override(params, asset.original_ext)),
    });
    let animated = asset_wants_animated(params, &asset.hash);
    let object =
        match read_cdn_object_with_fallback(app, &asset.storage_key, asset.original_ext).await {
            Ok(object) => object,
            Err(err) => return storage_error_response(&asset.storage_key, err),
        };
    let sniffed_source_ext = extension_from_mime(mime::sniff(&object.data).mime);
    let source_format = if sniffed_source_ext == Some(AssetExtension::Apng) {
        AssetExtension::Apng
    } else {
        extension_from_mime(&object.content_type)
            .or(sniffed_source_ext)
            .unwrap_or(asset.original_ext)
    };
    let serve_content_type = if object.content_type.is_empty()
        || object
            .content_type
            .eq_ignore_ascii_case("application/octet-stream")
        || extension_from_mime(&object.content_type).is_none()
    {
        source_format.mime().to_owned()
    } else {
        object.content_type.clone()
    };
    let out_ext =
        effective_animated_image_output_format(Some(source_format), selected.format, animated);
    let quality = params
        .get("quality")
        .cloned()
        .unwrap_or_else(|| default_transform_quality(out_ext, animated, "high").to_owned());
    let width = selected.size;
    let height = selected.size;
    if same_format_loaded_image_request_can_use_original(
        &object.data,
        OriginalImageRequest {
            source_ext: Some(source_format),
            explicit_out_ext: asset_manual_format_override(params, asset.original_ext),
            out_ext,
            width,
            height,
            has_quality: params.contains_key("quality"),
            effort: None,
            animated,
        },
    ) {
        return media_response(
            method,
            object.data,
            &serve_content_type,
            headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
            Some(content_disposition_header(
                &serve_content_type,
                requested_download,
                Some(&asset_filename),
            )),
        );
    }
    let options = media_process::ImageOptions {
        width,
        height,
        format: out_ext,
        quality: quality.clone(),
        animated,
        effort_override: None,
        cover_crop: matches!(asset.kind, AssetKind::Emoji | AssetKind::Sticker),
        deadline_ms: Some(metrics::now_ms() + app.cfg.transform_timeout_ms as i64),
        max_encode_frames: Some(app.cfg.max_encode_frames),
        max_encode_duration_ms: Some(app.cfg.max_encode_duration_ms),
    };
    let cache_key = format!(
        "asset:{}:{}:{}:{}:{}",
        asset.storage_key,
        selected.size.unwrap_or(0),
        out_ext.name(),
        quality,
        animated
    );
    if let Some(cached) = app.transform_cache.get(&cache_key) {
        metrics::GLOBAL
            .transform_cache_hits
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        return media_response(
            method,
            cached,
            out_ext.mime(),
            headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
            Some(content_disposition_header(
                out_ext.mime(),
                requested_download,
                Some(&asset_filename),
            )),
        );
    }
    metrics::GLOBAL
        .transform_cache_misses
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let coalescer_deadline = deadline_instant(options.deadline_ms);
    let transformed = match app
        .coalescer
        .run_once_until(cache_key.clone(), coalescer_deadline, || {
            let app = app.clone();
            let data = object.data.clone();
            let options = options.clone();
            async move {
                coalesced_work_result(run_transform(&app, data, options).await)
                    .map(|media| media.bytes)
            }
        })
        .await
    {
        Ok(bytes) => bytes,
        Err(CoalescerError::RequestTimeout) => {
            return text_with_reason(
                StatusCode::GATEWAY_TIMEOUT,
                "Gateway Timeout",
                "coalescer_timeout_asset_image",
            );
        }
        Err(CoalescerError::WorkFailed) => {
            let src_ct = object.content_type.as_str();
            let src_is_displayable = src_ct.starts_with("image/")
                && src_ct != "image/avif"
                && src_ct != "image/heic"
                && src_ct != "image/heif"
                && source_format != AssetExtension::Svg
                && !is_svg_content_type(src_ct);
            if !src_is_displayable {
                return text_with_source(
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "Transcode Failed",
                    "transcode_failed",
                    format!(
                        "asset key={} src_ct={} out={} size={:?} animated={}",
                        asset.storage_key,
                        src_ct,
                        out_ext.name(),
                        selected.size,
                        animated,
                    ),
                );
            }
            return media_response(
                method,
                object.data,
                &object.content_type,
                headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
                None,
            );
        }
    };
    app.transform_cache.put(cache_key, transformed.clone());
    media_response(
        method,
        transformed,
        out_ext.mime(),
        headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        Some(content_disposition_header(
            out_ext.mime(),
            requested_download,
            Some(&asset_filename),
        )),
    )
}

async fn read_cdn_object_with_fallback(
    app: &Arc<AppState>,
    key: &str,
    original_ext: AssetExtension,
) -> Result<crate::storage::Object, StorageError> {
    match app.store.read_object(&app.cfg.bucket_cdn, key).await {
        Ok(object) => Ok(object),
        Err(StorageError::NotFound) => {
            let fallback_key = format!("{key}.{}", original_ext.name());
            app.store
                .read_object(&app.cfg.bucket_cdn, &fallback_key)
                .await
        }
        Err(err) => Err(err),
    }
}

async fn serve_attachment(
    app: &Arc<AppState>,
    method: Method,
    key: &str,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let filename = filename_from_storage_key(key);
    let animated = animated_param(params, false);
    let wants_transform = params.contains_key("width")
        || params.contains_key("height")
        || params.contains_key("format")
        || params.contains_key("quality")
        || animated;
    if !wants_transform {
        return serve_stored_passthrough_stream(
            app,
            method,
            &app.cfg.bucket_cdn,
            key,
            headers,
            PassthroughDisposition::Attachment {
                requested_download: bool_param(params, "download", false),
                filename,
            },
        )
        .await;
    }
    let object = match app.store.read_object(&app.cfg.bucket_cdn, key).await {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    serve_bytes_or_transform(
        app,
        ServeBytesRequest {
            method,
            data: object.data,
            content_type: object.content_type,
            cache_identity: key,
            filename,
            route: TransformRoute::Attachment,
            params,
            headers,
        },
    )
    .await
}

async fn serve_stored_raw(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
) -> Response {
    serve_stored_passthrough_stream(
        app,
        method,
        bucket,
        key,
        headers,
        PassthroughDisposition::None,
    )
    .await
}

enum PassthroughDisposition<'a> {
    None,
    Attachment {
        requested_download: bool,
        filename: &'a str,
    },
}

async fn serve_stored_passthrough_stream(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
    disposition: PassthroughDisposition<'_>,
) -> Response {
    let head = match app.store.head_object(bucket, key).await {
        Ok(head) => head,
        Err(err) => return storage_error_response(key, err),
    };
    if head.content_length > constants::MAX_MEDIA_PROXY_BYTES as u64 {
        return storage_error_response(key, StorageError::StreamTooLong);
    }
    let content_type = passthrough_content_type(&head, key);
    if is_svg_content_type(&content_type)
        || image_extension_from_filename(key) == Some(AssetExtension::Svg)
    {
        let object = match app.store.read_object(bucket, key).await {
            Ok(object) => object,
            Err(err) => return storage_error_response(key, err),
        };
        let cache_identity = format!("{bucket}/{key}");
        return serve_stored_svg_rasterized(
            app,
            method,
            object.data,
            &cache_identity,
            headers,
            &disposition,
        )
        .await;
    }
    let total_len = match usize::try_from(head.content_length) {
        Ok(value) => value,
        Err(_) => return storage_error_response(key, StorageError::StreamTooLong),
    };
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());
    let parsed_range = range::parse_range(range_header, total_len);
    if parsed_range.unsatisfiable {
        let mut response = Response::new(Body::empty());
        *response.status_mut() = StatusCode::RANGE_NOT_SATISFIABLE;
        http_headers::add_unsatisfiable_headers(response.headers_mut(), total_len);
        return response;
    }
    let normalized_range = parsed_range
        .range
        .map(|r| format!("bytes={}-{}", r.start, r.end));
    if method == Method::HEAD {
        return passthrough_head_response(
            &content_type,
            total_len,
            parsed_range.range,
            passthrough_disposition_header(&disposition, &content_type),
        );
    }
    let object = match app
        .store
        .stream_object(bucket, key, normalized_range.as_deref())
        .await
    {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    streaming_media_response(
        method,
        object,
        total_len,
        parsed_range.range,
        &content_type,
        passthrough_disposition_header(&disposition, &content_type),
    )
}

fn passthrough_content_type(head: &HeadResult, key: &str) -> String {
    let extension_mime = mime::extension_mime(key);
    if extension_mime == Some("audio/mp4")
        && mime::normalize(Some(&head.content_type)) == Some("video/mp4")
    {
        return "audio/mp4".to_owned();
    }
    if content_type_is_trustworthy(&head.content_type) {
        head.content_type.clone()
    } else {
        extension_mime
            .or_else(|| {
                mime::normalize(Some(&head.content_type)).filter(|value| {
                    !value.is_empty() && !value.eq_ignore_ascii_case("application/octet-stream")
                })
            })
            .unwrap_or("application/octet-stream")
            .to_owned()
    }
}

fn passthrough_disposition_header(
    disposition: &PassthroughDisposition<'_>,
    content_type: &str,
) -> Option<String> {
    match disposition {
        PassthroughDisposition::None => None,
        PassthroughDisposition::Attachment {
            requested_download,
            filename,
        } => Some(content_disposition_header(
            content_type,
            *requested_download,
            Some(filename),
        )),
    }
}

async fn serve_stored_svg_rasterized(
    app: &Arc<AppState>,
    method: Method,
    data: Bytes,
    cache_identity: &str,
    headers: &HeaderMap,
    disposition: &PassthroughDisposition<'_>,
) -> Response {
    let format = AssetExtension::Webp;
    let quality = "lossless".to_owned();
    let options = media_process::ImageOptions {
        format,
        quality: quality.clone(),
        animated: false,
        deadline_ms: Some(metrics::now_ms() + app.cfg.transform_timeout_ms as i64),
        max_encode_frames: Some(app.cfg.max_encode_frames),
        max_encode_duration_ms: Some(app.cfg.max_encode_duration_ms),
        ..Default::default()
    };
    let cache_key = transform_cache_key(TransformCacheKeyInput {
        route: TransformRoute::Stored,
        cache_identity,
        width: None,
        height: None,
        format,
        quality: &quality,
        animated: false,
        effort: None,
    });
    if let Some(cached) = app.transform_cache.get(&cache_key) {
        metrics::GLOBAL
            .transform_cache_hits
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        return media_response(
            method,
            cached,
            format.mime(),
            headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
            passthrough_disposition_header(disposition, format.mime()),
        );
    }
    metrics::GLOBAL
        .transform_cache_misses
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let coalescer_deadline = deadline_instant(options.deadline_ms);
    let transformed = match app
        .coalescer
        .run_once_until(cache_key.clone(), coalescer_deadline, || {
            let app = app.clone();
            let data = data.clone();
            let options = options.clone();
            async move {
                coalesced_work_result(run_transform(&app, data, options).await)
                    .map(|media| media.bytes)
            }
        })
        .await
    {
        Ok(bytes) => bytes,
        Err(CoalescerError::RequestTimeout) => {
            return text_with_reason(
                StatusCode::GATEWAY_TIMEOUT,
                "Gateway Timeout",
                "coalescer_timeout_svg_rasterize",
            );
        }
        Err(CoalescerError::WorkFailed) => {
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "svg_rasterize_failed",
                cache_identity,
            );
        }
    };
    app.transform_cache.put(cache_key, transformed.clone());
    media_response(
        method,
        transformed,
        format.mime(),
        headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        passthrough_disposition_header(disposition, format.mime()),
    )
}

fn passthrough_head_response(
    content_type: &str,
    total_len: usize,
    byte_range: Option<range::ByteRange>,
    disposition: Option<String>,
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
    if let Some(disposition) = disposition
        && let Ok(value) = HeaderValue::from_str(&disposition)
    {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

fn streaming_media_response(
    method: Method,
    object: StreamObject,
    total_len: usize,
    byte_range: Option<range::ByteRange>,
    content_type: &str,
    disposition: Option<String>,
) -> Response {
    let status = if object.status == StatusCode::PARTIAL_CONTENT {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::OK
    };
    let effective_byte_range = if status == StatusCode::PARTIAL_CONTENT {
        byte_range
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
        Response::new(object.body)
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
    if let Some(disposition) = disposition
        && let Ok(value) = HeaderValue::from_str(&disposition)
    {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

async fn serve_stored_with_override(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    content_type: &str,
    headers: &HeaderMap,
) -> Response {
    let object = match app.store.read_object(bucket, key).await {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    media_response(
        method,
        object.data,
        content_type,
        headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        None,
    )
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TransformRoute {
    Attachment,
    External,
    Stored,
}

struct ServeBytesRequest<'a> {
    method: Method,
    data: Bytes,
    content_type: String,
    cache_identity: &'a str,
    filename: &'a str,
    route: TransformRoute,
    params: &'a HashMap<String, String>,
    headers: &'a HeaderMap,
}

async fn serve_bytes_or_transform(app: &Arc<AppState>, request: ServeBytesRequest<'_>) -> Response {
    let ServeBytesRequest {
        method,
        data,
        content_type,
        cache_identity,
        filename,
        route,
        params,
        headers,
    } = request;
    let animated = animated_param(params, false);
    let sniffed_prefix = mime::sniff(&data[..data.len().min(8192)]);
    let content_type = if sniffed_prefix.mime == "image/svg+xml" {
        "image/svg+xml".to_owned()
    } else if content_type_is_trustworthy(&content_type) {
        content_type
    } else {
        mime::detect(&data[..data.len().min(8192)], filename, Some(&content_type))
    };
    let source_is_svg = is_svg_content_type(&content_type)
        || image_extension_from_filename(filename) == Some(AssetExtension::Svg);
    let wants_transform = source_is_svg
        || params.contains_key("width")
        || params.contains_key("height")
        || params.contains_key("format")
        || params.contains_key("quality")
        || animated;
    let requested_download = bool_param(params, "download", false);

    if !wants_transform {
        let disposition =
            content_disposition_header(&content_type, requested_download, Some(filename));
        return media_response(
            method,
            data,
            &content_type,
            headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
            Some(disposition),
        );
    }

    let explicit_requested_format = match explicit_output_format(params) {
        Ok(format) => format,
        Err(()) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let width = match parse_optional_dimension_param(params, "width") {
        Ok(width) => width,
        Err(()) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let height = match parse_optional_dimension_param(params, "height") {
        Ok(height) => height,
        Err(()) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let media_kind = mime::category(&content_type);
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());

    if media_kind == Some(mime::Category::Video) {
        let Some(requested_format) = explicit_requested_format else {
            if route == TransformRoute::Attachment {
                return text(StatusCode::BAD_REQUEST, "Bad Request");
            }
            let disposition =
                content_disposition_header(&content_type, requested_download, Some(filename));
            return media_response(method, data, &content_type, range_header, Some(disposition));
        };
        let format = output_format::coerce_unsupported_format(requested_format);
        let quality = params
            .get("quality")
            .cloned()
            .unwrap_or_else(|| "lossless".to_owned());
        let cache_key = transform_cache_key(TransformCacheKeyInput {
            route,
            cache_identity,
            width,
            height,
            format,
            quality: &quality,
            animated,
            effort: None,
        });
        if let Some(cached) = app.transform_cache.get(&cache_key) {
            metrics::GLOBAL
                .transform_cache_hits
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let disposition =
                content_disposition_header(format.mime(), requested_download, Some(filename));
            return media_response(
                method,
                cached,
                format.mime(),
                range_header,
                Some(disposition),
            );
        }
        metrics::GLOBAL
            .transform_cache_misses
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let deadline_ms = Some(metrics::now_ms() + app.cfg.transform_timeout_ms as i64);
        let coalescer_deadline = deadline_instant(deadline_ms);
        let transformed = match app
            .coalescer
            .run_once_until(cache_key.clone(), coalescer_deadline, || {
                let app = app.clone();
                let data = data.clone();
                let quality = quality.clone();
                async move {
                    coalesced_work_result(
                        run_video_transform(
                            &app,
                            data,
                            format,
                            width,
                            height,
                            quality,
                            deadline_ms,
                        )
                        .await,
                    )
                    .map(|media| media.bytes)
                }
            })
            .await
        {
            Ok(bytes) => bytes,
            Err(CoalescerError::RequestTimeout) => {
                return text_with_reason(
                    StatusCode::GATEWAY_TIMEOUT,
                    "Gateway Timeout",
                    "coalescer_timeout_video",
                );
            }
            Err(CoalescerError::WorkFailed) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "video_transform_failed",
                    format!(
                        "fmt={} w={:?} h={:?} q={}",
                        format.name(),
                        width,
                        height,
                        quality
                    ),
                );
            }
        };
        app.transform_cache.put(cache_key, transformed.clone());
        let disposition =
            content_disposition_header(format.mime(), requested_download, Some(filename));
        return media_response(
            method,
            transformed,
            format.mime(),
            range_header,
            Some(disposition),
        );
    }

    if media_kind != Some(mime::Category::Image) {
        if route == TransformRoute::Attachment && explicit_requested_format.is_some() {
            return text(StatusCode::BAD_REQUEST, "Bad Request");
        }
        let disposition =
            content_disposition_header(&content_type, requested_download, Some(filename));
        return media_response(method, data, &content_type, range_header, Some(disposition));
    }

    let sniffed_source_format = extension_from_mime(mime::sniff(&data).mime);
    let source_format = if sniffed_source_format == Some(AssetExtension::Apng) {
        Some(AssetExtension::Apng)
    } else {
        extension_from_mime(&content_type).or_else(|| image_extension_from_filename(filename))
    };
    let default_out_ext = match route {
        TransformRoute::Attachment => {
            image_extension_from_filename(filename).unwrap_or(AssetExtension::Webp)
        }
        TransformRoute::External => external_default_output_extension(filename, &content_type),
        TransformRoute::Stored => {
            image_extension_from_filename(filename).unwrap_or(AssetExtension::Webp)
        }
    };
    let requested_format = explicit_requested_format.unwrap_or(default_out_ext);
    let requested_supported_format = output_format::coerce_unsupported_format(requested_format);
    let format =
        effective_animated_image_output_format(source_format, requested_supported_format, animated);
    let response_content_type = transform_response_content_type(
        explicit_requested_format,
        requested_format,
        format,
        &content_type,
    );
    let quality = params
        .get("quality")
        .cloned()
        .unwrap_or_else(|| default_transform_quality(format, animated, "lossless").to_owned());
    let effort = (route == TransformRoute::Attachment)
        .then(|| parse_effort(params))
        .flatten();
    if same_format_loaded_image_request_can_use_original(
        &data,
        OriginalImageRequest {
            source_ext: source_format,
            explicit_out_ext: explicit_requested_format,
            out_ext: format,
            width,
            height,
            has_quality: params.contains_key("quality"),
            effort,
            animated,
        },
    ) {
        let serve_ct = if content_type.is_empty()
            || content_type.eq_ignore_ascii_case("application/octet-stream")
            || extension_from_mime(&content_type).is_none()
        {
            source_format
                .map(|ext| ext.mime().to_owned())
                .unwrap_or_else(|| content_type.clone())
        } else {
            content_type.clone()
        };
        let disposition = content_disposition_header(&serve_ct, requested_download, Some(filename));
        return media_response(method, data, &serve_ct, range_header, Some(disposition));
    }
    let options = media_process::ImageOptions {
        width,
        height,
        format,
        quality: quality.clone(),
        animated,
        effort_override: effort,
        cover_crop: params.contains_key("width") && params.contains_key("height"),
        deadline_ms: Some(metrics::now_ms() + app.cfg.transform_timeout_ms as i64),
        max_encode_frames: Some(app.cfg.max_encode_frames),
        max_encode_duration_ms: Some(app.cfg.max_encode_duration_ms),
    };
    let cache_key = transform_cache_key(TransformCacheKeyInput {
        route,
        cache_identity,
        width: options.width,
        height: options.height,
        format: options.format,
        quality: &options.quality,
        animated: options.animated,
        effort,
    });
    if let Some(cached) = app.transform_cache.get(&cache_key) {
        metrics::GLOBAL
            .transform_cache_hits
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let disposition =
            content_disposition_header(response_content_type, requested_download, Some(filename));
        return media_response(
            method,
            cached,
            response_content_type,
            range_header,
            Some(disposition),
        );
    }
    metrics::GLOBAL
        .transform_cache_misses
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let coalescer_deadline = deadline_instant(options.deadline_ms);
    let media = match app
        .coalescer
        .run_once_until(cache_key.clone(), coalescer_deadline, || {
            let app = app.clone();
            let data = data.clone();
            let options = options.clone();
            async move {
                coalesced_work_result(run_transform(&app, data, options).await)
                    .map(|media| media.bytes)
            }
        })
        .await
    {
        Ok(bytes) => {
            app.transform_cache.put(cache_key, bytes.clone());
            ProcessedBytes {
                bytes,
                content_type: response_content_type.to_owned(),
            }
        }
        Err(CoalescerError::RequestTimeout) => {
            return text_with_reason(
                StatusCode::GATEWAY_TIMEOUT,
                "Gateway Timeout",
                "coalescer_timeout_image",
            );
        }
        Err(CoalescerError::WorkFailed) => {
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "image_transform_failed",
                format!(
                    "route={:?} cache_identity={} fmt={} w={:?} h={:?} q={} animated={}",
                    route,
                    cache_identity,
                    options.format.name(),
                    options.width,
                    options.height,
                    options.quality,
                    options.animated,
                ),
            );
        }
    };
    let disposition =
        content_disposition_header(&media.content_type, requested_download, Some(filename));
    media_response(
        method,
        media.bytes,
        &media.content_type,
        range_header,
        Some(disposition),
    )
}

struct ProcessedBytes {
    bytes: Bytes,
    content_type: String,
}

async fn run_transform(
    app: &Arc<AppState>,
    data: Bytes,
    options: media_process::ImageOptions,
) -> anyhow::Result<media_process::ProcessedMedia> {
    let deadline = deadline_instant(options.deadline_ms);
    let wait_start = metrics::now_ms();
    let _permit = app.native_transforms.wait_until(deadline).await?;
    let waited = (metrics::now_ms() - wait_start).max(0) as u64;
    metrics::GLOBAL.native_transform_wait.observe(waited);
    let start = metrics::now_ms();
    let result =
        tokio::task::spawn_blocking(move || media_process::transform_image(&data, &options))
            .await??;
    let elapsed = (metrics::now_ms() - start).max(0) as u64;
    metrics::GLOBAL.transform_image_duration.observe(elapsed);
    request_log::record_stage(Stage::Transform, elapsed);
    Ok(result)
}

async fn run_video_transform(
    app: &Arc<AppState>,
    data: Bytes,
    format: AssetExtension,
    width: Option<u32>,
    height: Option<u32>,
    quality: String,
    deadline_ms: Option<i64>,
) -> anyhow::Result<media_process::ProcessedMedia> {
    let deadline = deadline_instant(deadline_ms);
    let wait_start = metrics::now_ms();
    let _permit = app.native_transforms.wait_until(deadline).await?;
    let waited = (metrics::now_ms() - wait_start).max(0) as u64;
    metrics::GLOBAL.native_transform_wait.observe(waited);
    let start = metrics::now_ms();
    let result = tokio::task::spawn_blocking(move || {
        let thumbnail = media_process::extract_video_thumbnail(&data, format)?;
        if width.is_none() && height.is_none() {
            return Ok(thumbnail);
        }
        media_process::transform_image(
            &thumbnail.bytes,
            &media_process::ImageOptions {
                width,
                height,
                format,
                quality,
                animated: false,
                deadline_ms,
                ..Default::default()
            },
        )
    })
    .await??;
    let elapsed = (metrics::now_ms() - start).max(0) as u64;
    metrics::GLOBAL.transform_image_duration.observe(elapsed);
    request_log::record_stage(Stage::Transform, elapsed);
    Ok(result)
}

fn coalesced_work_result<T>(result: anyhow::Result<T>) -> anyhow::Result<T> {
    match result {
        Ok(value) => Ok(value),
        Err(error) if transform_error_is_timeout(&error) => {
            Err(anyhow::Error::new(CoalescerError::RequestTimeout))
        }
        Err(error) => Err(error),
    }
}

fn transform_error_is_timeout(error: &anyhow::Error) -> bool {
    error.downcast_ref::<media_process::MediaError>()
        == Some(&media_process::MediaError::RequestTimeout)
        || error.downcast_ref::<crate::timed_semaphore::TimedSemaphoreError>()
            == Some(&crate::timed_semaphore::TimedSemaphoreError::RequestTimeout)
}

fn media_response(
    method: Method,
    data: Bytes,
    content_type: &str,
    range_header: Option<&str>,
    disposition: Option<String>,
) -> Response {
    let total_len = data.len();
    let parsed_range = range::parse_range(range_header, data.len());
    if parsed_range.unsatisfiable {
        let mut response = Response::new(Body::empty());
        *response.status_mut() = StatusCode::RANGE_NOT_SATISFIABLE;
        http_headers::add_unsatisfiable_headers(response.headers_mut(), data.len());
        return response;
    }
    let (status, body_bytes, byte_range) = if let Some(r) = parsed_range.range {
        let bytes = data.slice(r.start..=r.end);
        (StatusCode::PARTIAL_CONTENT, bytes, Some(r))
    } else {
        (StatusCode::OK, data, None)
    };
    let mut response = if method == Method::HEAD {
        Response::new(Body::empty())
    } else {
        Response::new(Body::from(body_bytes.clone()))
    };
    *response.status_mut() = status;
    http_headers::add_media_headers(response.headers_mut(), total_len, content_type, byte_range);
    response
        .headers_mut()
        .insert(header::CONTENT_LENGTH, HeaderValue::from(body_bytes.len()));
    if let Some(disposition) = disposition
        && let Ok(value) = HeaderValue::from_str(&disposition)
    {
        response
            .headers_mut()
            .insert(header::CONTENT_DISPOSITION, value);
    }
    response
}

fn check_internal_auth(headers: &HeaderMap, secret: &str) -> bool {
    let Some(auth) = headers
        .get(header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())
    else {
        return false;
    };
    let expected = format!("Bearer {secret}");
    if auth.len() != expected.len() {
        return false;
    }
    auth.bytes()
        .zip(expected.bytes())
        .fold(0u8, |diff, (a, b)| diff | (a ^ b))
        == 0
}

async fn read_limited_body(request: Request<Body>) -> Result<Bytes, StatusCode> {
    to_bytes(
        request.into_body(),
        constants::MAX_INTERNAL_REQUEST_BODY_BYTES + 1,
    )
    .await
    .map_err(|_| StatusCode::BAD_REQUEST)
    .and_then(|body| {
        if body.len() > constants::MAX_INTERNAL_REQUEST_BODY_BYTES {
            Err(StatusCode::PAYLOAD_TOO_LARGE)
        } else {
            Ok(body)
        }
    })
}

fn storage_status(err: &StorageError) -> StatusCode {
    match err {
        StorageError::NotFound => StatusCode::NOT_FOUND,
        StorageError::ReadOnlyStorage => StatusCode::FORBIDDEN,
        StorageError::InvalidBucket | StorageError::InvalidKey => StatusCode::BAD_REQUEST,
        StorageError::StreamTooLong => StatusCode::PAYLOAD_TOO_LARGE,
        _ => StatusCode::BAD_GATEWAY,
    }
}

fn storage_error_response(key: &str, err: StorageError) -> Response {
    let status = storage_status(&err);
    let body = if status == StatusCode::NOT_FOUND {
        "Not Found"
    } else {
        canonical_reason_str(status)
    };
    text_with_source(
        status,
        body,
        "storage_error",
        format!("key={key} err={err}"),
    )
}

fn relay_error(err: upload_relay::RelayError) -> Response {
    let status = match err {
        upload_relay::RelayError::MissingToken
        | upload_relay::RelayError::InvalidToken
        | upload_relay::RelayError::RelayTokenExpired => StatusCode::UNAUTHORIZED,
        upload_relay::RelayError::PayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
        upload_relay::RelayError::UpstreamRetryable => StatusCode::SERVICE_UNAVAILABLE,
        upload_relay::RelayError::UpstreamS3Error => StatusCode::BAD_GATEWAY,
        _ => StatusCode::BAD_REQUEST,
    };
    let mut response = text(status, status.canonical_reason().unwrap_or("Bad Request"));
    relay_cors(response.headers_mut());
    response
}

fn relay_cors(headers: &mut HeaderMap) {
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        HeaderValue::from_static("*"),
    );
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_METHODS,
        HeaderValue::from_static("PUT, OPTIONS"),
    );
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_HEADERS,
        HeaderValue::from_static(
            "Content-Type, Content-Length, Authorization, X-Fluxer-Features, X-Client-Context",
        ),
    );
    headers.insert(
        header::ACCESS_CONTROL_EXPOSE_HEADERS,
        HeaderValue::from_static("ETag, X-Fluxer-Version"),
    );
}

fn text(status: StatusCode, body: &str) -> Response {
    text_inner(status, body, None)
}

fn text_with_source(
    status: StatusCode,
    body: &str,
    code: &'static str,
    source: impl std::fmt::Debug,
) -> Response {
    text_inner(status, body, Some(ErrorReason::with_source(code, source)))
}

fn text_with_reason(status: StatusCode, body: &str, code: &'static str) -> Response {
    text_inner(status, body, Some(ErrorReason::new(code)))
}

fn text_inner(status: StatusCode, body: &str, reason: Option<ErrorReason>) -> Response {
    let mut response = Response::new(Body::from(body.to_owned()));
    *response.status_mut() = status;
    http_headers::add_security_headers(response.headers_mut());
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/plain; charset=utf-8"),
    );
    response.headers_mut().insert(
        header::X_CONTENT_TYPE_OPTIONS,
        HeaderValue::from_static("nosniff"),
    );
    if status.is_client_error() || status.is_server_error() {
        response
            .extensions_mut()
            .insert(reason.unwrap_or_else(|| ErrorReason::new(canonical_reason_str(status))));
    }
    response
}

fn canonical_reason_str(status: StatusCode) -> &'static str {
    status.canonical_reason().unwrap_or("error")
}

fn json_response(status: StatusCode, body: String) -> Response {
    let mut response = Response::new(Body::from(body));
    *response.status_mut() = status;
    http_headers::add_security_headers(response.headers_mut());
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("application/json"),
    );
    response
}

fn decode_storage_key(path: &str) -> String {
    external_path::percent_decode_string(path.trim_start_matches('/'), false)
}

fn bool_param(params: &HashMap<String, String>, key: &str, default_value: bool) -> bool {
    params
        .get(key)
        .map(|raw| raw.eq_ignore_ascii_case("true") || raw == "1")
        .unwrap_or(default_value)
}

fn animated_param(params: &HashMap<String, String>, default_value: bool) -> bool {
    params
        .get("animated")
        .or_else(|| params.get("animatd"))
        .map(|raw| raw.eq_ignore_ascii_case("true") || raw == "1")
        .unwrap_or(default_value)
}

fn explicit_output_format(params: &HashMap<String, String>) -> Result<Option<AssetExtension>, ()> {
    let Some(raw) = params.get("format") else {
        return Ok(None);
    };
    AssetExtension::parse(raw).map(Some).ok_or(())
}

#[cfg(test)]
fn parse_dimension(raw: Option<&str>) -> Option<u32> {
    raw.and_then(|v| v.parse::<u32>().ok())
        .filter(|v| *v > 0 && *v <= constants::Limits::image_dimension())
}

fn parse_optional_dimension_param(
    params: &HashMap<String, String>,
    key: &str,
) -> Result<Option<u32>, ()> {
    let Some(raw) = params.get(key) else {
        return Ok(None);
    };
    if raw.is_empty() {
        return Err(());
    }
    let value = raw.parse::<u32>().map_err(|_| ())?;
    if value == 0 || value > constants::Limits::image_dimension() {
        return Err(());
    }
    Ok(Some(value))
}

fn parse_effort(params: &HashMap<String, String>) -> Option<u8> {
    let raw = params.get("effort")?;
    if raw.is_empty() {
        return None;
    }
    raw.parse::<u8>().ok().map(|value| value.min(9))
}

fn deadline_instant(deadline_ms: Option<i64>) -> Option<Instant> {
    let deadline_ms = deadline_ms?;
    let remaining_ms = deadline_ms.saturating_sub(metrics::now_ms()).max(0) as u64;
    Some(Instant::now() + Duration::from_millis(remaining_ms))
}

fn effective_animated_image_output_format(
    source_ext: Option<AssetExtension>,
    requested_out_ext: AssetExtension,
    animated: bool,
) -> AssetExtension {
    if animated
        && source_ext == Some(AssetExtension::Apng)
        && requested_out_ext == AssetExtension::Png
    {
        return AssetExtension::Apng;
    }
    if animated
        && source_ext == Some(AssetExtension::Gif)
        && requested_out_ext == AssetExtension::Webp
    {
        return AssetExtension::Gif;
    }
    requested_out_ext
}

fn default_transform_quality(format: AssetExtension, animated: bool, static_default: &str) -> &str {
    if animated && format == AssetExtension::Webp {
        "auto"
    } else {
        static_default
    }
}

fn is_v1_asset_manual_format(ext: AssetExtension) -> bool {
    matches!(
        ext,
        AssetExtension::Png
            | AssetExtension::Jpeg
            | AssetExtension::Webp
            | AssetExtension::Gif
            | AssetExtension::Apng
            | AssetExtension::Avif
    )
}

fn asset_manual_format_override(
    params: &HashMap<String, String>,
    url_ext: AssetExtension,
) -> Option<AssetExtension> {
    let raw = params.get("format").or_else(|| params.get("fmt"));
    if let Some(raw) = raw {
        if raw.eq_ignore_ascii_case("auto") {
            return None;
        }
        if let Some(parsed) = AssetExtension::parse(raw)
            && is_v1_asset_manual_format(parsed)
        {
            return Some(parsed);
        }
    }
    is_v1_asset_manual_format(url_ext).then_some(url_ext)
}

fn asset_wants_animated(params: &HashMap<String, String>, hash: &str) -> bool {
    animated_param(params, asset_hash::has_animation_prefix(hash))
}

fn animated_image_request_can_use_original(
    source_ext: AssetExtension,
    explicit_out_ext: Option<AssetExtension>,
    out_ext: AssetExtension,
    width: Option<u32>,
    height: Option<u32>,
    animated: bool,
) -> bool {
    if !animated || width.is_some() || height.is_some() {
        return false;
    }
    if !matches!(
        source_ext,
        AssetExtension::Gif | AssetExtension::Webp | AssetExtension::Apng
    ) {
        return false;
    }
    let requested = explicit_out_ext.unwrap_or(out_ext);
    requested == source_ext
}

fn same_format_image_request_base_allows_original(
    source_ext: AssetExtension,
    explicit_out_ext: Option<AssetExtension>,
    out_ext: AssetExtension,
    has_quality: bool,
    effort: Option<u8>,
) -> bool {
    if effort.is_some() {
        return false;
    }
    if !output_format::is_output_format_supported(source_ext) {
        return false;
    }
    if out_ext != source_ext {
        return false;
    }
    let _ = explicit_out_ext;
    if has_quality && source_ext != AssetExtension::Gif {
        return false;
    }
    true
}

#[derive(Clone, Copy)]
struct OriginalImageRequest {
    source_ext: Option<AssetExtension>,
    explicit_out_ext: Option<AssetExtension>,
    out_ext: AssetExtension,
    width: Option<u32>,
    height: Option<u32>,
    has_quality: bool,
    effort: Option<u8>,
    animated: bool,
}

fn same_format_loaded_image_request_can_use_original(
    data: &[u8],
    request: OriginalImageRequest,
) -> bool {
    let Some(source_ext) = request.source_ext else {
        return false;
    };
    if animated_image_request_can_use_original(
        source_ext,
        request.explicit_out_ext,
        request.out_ext,
        request.width,
        request.height,
        request.animated,
    ) {
        return true;
    }
    if !same_format_image_request_base_allows_original(
        source_ext,
        request.explicit_out_ext,
        request.out_ext,
        request.has_quality,
        request.effort,
    ) {
        return false;
    }
    if !(request.animated
        || request.explicit_out_ext.is_some()
        || request.has_quality
        || request.width.is_some()
        || request.height.is_some())
    {
        return false;
    }
    let sniffed = mime::sniff(data);
    if sniffed.width == 0 || sniffed.height == 0 {
        return false;
    }
    if let Some(target_w) = request.width
        && target_w < sniffed.width
    {
        return false;
    }
    if let Some(target_h) = request.height
        && target_h < sniffed.height
    {
        return false;
    }
    true
}

fn content_type_is_trustworthy(content_type: &str) -> bool {
    if content_type.is_empty() {
        return false;
    }
    if content_type.eq_ignore_ascii_case("application/octet-stream") {
        return false;
    }
    matches!(
        mime::category(content_type),
        Some(mime::Category::Image | mime::Category::Video | mime::Category::Audio)
    )
}

fn is_svg_content_type(content_type: &str) -> bool {
    mime::normalize(Some(content_type))
        .is_some_and(|value| value.eq_ignore_ascii_case("image/svg+xml"))
}

fn extension_from_mime(content_type: &str) -> Option<AssetExtension> {
    match mime::normalize(Some(content_type))? {
        "image/jpeg" => Some(AssetExtension::Jpeg),
        "image/png" => Some(AssetExtension::Png),
        "image/webp" => Some(AssetExtension::Webp),
        "image/gif" => Some(AssetExtension::Gif),
        "image/apng" => Some(AssetExtension::Apng),
        "image/avif" => Some(AssetExtension::Avif),
        "image/heic" => Some(AssetExtension::Heic),
        "image/heif" => Some(AssetExtension::Heif),
        "image/jxl" => Some(AssetExtension::Jxl),
        "image/svg+xml" => Some(AssetExtension::Svg),
        _ => None,
    }
}

fn image_extension_from_filename(filename: &str) -> Option<AssetExtension> {
    AssetExtension::parse(extension_of(filename)?)
}

fn external_default_output_extension(filename: &str, content_type: &str) -> AssetExtension {
    extension_from_mime(content_type)
        .or_else(|| image_extension_from_filename(filename))
        .unwrap_or(AssetExtension::Webp)
}

fn transform_response_content_type(
    explicit_out_ext: Option<AssetExtension>,
    requested_out_ext: AssetExtension,
    out_ext: AssetExtension,
    fallback_content_type: &str,
) -> &str {
    if explicit_out_ext.is_some()
        || out_ext != requested_out_ext
        || is_svg_content_type(fallback_content_type)
    {
        out_ext.mime()
    } else {
        fallback_content_type
    }
}

struct TransformCacheKeyInput<'a> {
    route: TransformRoute,
    cache_identity: &'a str,
    width: Option<u32>,
    height: Option<u32>,
    format: AssetExtension,
    quality: &'a str,
    animated: bool,
    effort: Option<u8>,
}

fn transform_cache_key(input: TransformCacheKeyInput<'_>) -> String {
    let prefix = match input.route {
        TransformRoute::Attachment => "attachment",
        TransformRoute::External => "external",
        TransformRoute::Stored => "stored",
    };
    let identity = match input.route {
        TransformRoute::Attachment | TransformRoute::Stored => input.cache_identity.to_owned(),
        TransformRoute::External => sha256_hex(input.cache_identity.as_bytes()),
    };
    format!(
        "{prefix}:{identity}|w={}|h={}|fmt={}|q={}|anim={}|effort={}",
        input.width.unwrap_or(0),
        input.height.unwrap_or(0),
        input.format.name(),
        input.quality,
        input.animated,
        input.effort.unwrap_or(255),
    )
}

fn sha256_hex(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

fn last_segment(value: &str) -> &str {
    value
        .rsplit('/')
        .next()
        .filter(|s| !s.is_empty())
        .unwrap_or("file.bin")
}

fn filename_from_storage_key(key: &str) -> &str {
    last_segment(key)
}

fn strip_query_fragment(value: &str) -> &str {
    let query = value.find('?').unwrap_or(value.len());
    let fragment = value.find('#').unwrap_or(value.len());
    &value[..query.min(fragment)]
}

fn url_filename(url: &str) -> String {
    let clean = strip_query_fragment(url);
    let filename = last_segment(clean);
    if filename.is_empty() {
        "external.bin".to_owned()
    } else {
        filename.to_owned()
    }
}

fn extension_of(filename: &str) -> Option<&str> {
    filename.rsplit_once('.').map(|(_, ext)| ext)
}

fn content_disposition_header(
    content_type: &str,
    requested_download: bool,
    filename: Option<&str>,
) -> String {
    let filename = filename
        .map(|name| download_filename_for_content_type(name, content_type, requested_download));
    disposition::format_header(
        disposition::decide(content_type, requested_download),
        filename.as_deref(),
    )
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

fn nsfw_config(app: &AppState) -> crate::nsfw::Config {
    crate::nsfw::Config {
        endpoint: app.cfg.nsfw_service_endpoint.clone(),
        threshold: app.cfg.nsfw_threshold,
        timeout_ms: 5_000,
        connect_timeout_ms: 1_500,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use base64::engine::general_purpose::STANDARD;

    fn upload_relay_test_config(
        storage_root: &std::path::Path,
        spool_dir: &std::path::Path,
        relay_secret: &[u8],
    ) -> Config {
        Config::load_from_iter([
            (
                "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                "secret".to_owned(),
            ),
            ("FLUXER_MEDIA_PROXY_MODE".to_owned(), "upload".to_owned()),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
                "local".to_owned(),
            ),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
                storage_root.display().to_string(),
            ),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64".to_owned(),
                base64::Engine::encode(&STANDARD, relay_secret),
            ),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES".to_owned(),
                "4096".to_owned(),
            ),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR".to_owned(),
                spool_dir.display().to_string(),
            ),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES".to_owned(),
                (1u64 << 30).to_string(),
            ),
        ])
        .unwrap()
    }

    fn test_app_state(cfg: Config) -> Arc<AppState> {
        Arc::new(AppState {
            store: Store::new(cfg.clone()),
            client: http_client::build_default(),
            nsfw_client: reqwest::Client::new(),
            transform_cache: Arc::new(Cache::new(
                cfg.transform_cache_capacity_bytes,
                cfg.transform_cache_max_entry_bytes,
                cfg.transform_cache_ttl_ms,
            )),
            coalescer: Arc::new(ByteCoalescer::new()),
            native_transforms: TimedSemaphore::new(cfg.max_native_transforms),
            cfg,
        })
    }

    #[test]
    fn entrance_sound_path_parses_valid_keys() {
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/1130650140672000000/eb417d05ad2e14c4.wav"),
            Some("entrance-sounds/1130650140672000000/eb417d05ad2e14c4.wav".to_owned())
        );
        for ext in ["mp3", "ogg", "m4a", "wav"] {
            assert_eq!(
                parse_entrance_sound_path(&format!("/entrance-sounds/42/abc123.{ext}")),
                Some(format!("entrance-sounds/42/abc123.{ext}"))
            );
        }
    }

    #[test]
    fn entrance_sound_path_rejects_invalid_keys() {
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/abc.flac"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/abc/abc.wav"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/abc.wav/x"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/../secret.wav"),
            None
        );
        assert_eq!(parse_entrance_sound_path("/entrance-sounds/42"), None);
        assert_eq!(parse_entrance_sound_path("/entrance-sounds//abc.wav"), None);
        assert_eq!(parse_entrance_sound_path("/avatars/42/abc.wav"), None);
    }

    #[test]
    fn internal_auth_uses_bearer_secret() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::AUTHORIZATION,
            HeaderValue::from_static("Bearer secret"),
        );
        assert!(check_internal_auth(&headers, "secret"));
        assert!(!check_internal_auth(&headers, "other"));
    }

    #[test]
    fn dimensions_are_bounded() {
        assert_eq!(Some(128), parse_dimension(Some("128")));
        assert_eq!(None, parse_dimension(Some("0")));
        assert_eq!(None, parse_dimension(Some("999999999")));
    }

    #[test]
    fn metadata_base64_svg_detection_uses_bytes_or_filename() {
        let svg_bytes = InputData {
            data: Bytes::from_static(br#"<svg xmlns="http://www.w3.org/2000/svg"></svg>"#),
            filename: "upload.bin".to_owned(),
        };
        assert!(metadata_input_is_svg(&svg_bytes));

        let svg_filename = InputData {
            data: Bytes::from_static(b"not svg"),
            filename: "icons/logo.svg".to_owned(),
        };
        assert!(metadata_input_is_svg(&svg_filename));

        let png_filename = InputData {
            data: Bytes::from_static(b"not svg"),
            filename: "icons/logo.png".to_owned(),
        };
        assert!(!metadata_input_is_svg(&png_filename));
    }

    #[test]
    fn replace_image_extension_only_changes_last_path_segment() {
        assert_eq!(
            "avatars/user.icon.webp",
            replace_image_extension("avatars/user.icon.svg", AssetExtension::Webp)
        );
        assert_eq!(
            "avatars.v1/user.webp",
            replace_image_extension("avatars.v1/user", AssetExtension::Webp)
        );
    }

    #[tokio::test]
    async fn metadata_base64_svg_rasterizes_to_webp_bytes() {
        let tmp = tempfile::tempdir().unwrap();
        let cfg =
            upload_relay_test_config(tmp.path(), tmp.path(), b"01234567890123456789012345678901");
        let app = test_app_state(cfg);
        let input = InputData {
            data: Bytes::from_static(
                br#"<svg xmlns="http://www.w3.org/2000/svg" width="8" height="8"><rect width="8" height="8" fill="red"/></svg>"#,
            ),
            filename: "icons/logo.svg".to_owned(),
        };
        let raster = match rasterize_metadata_svg(&app, input).await {
            Ok(raster) => raster,
            Err(response) => panic!("unexpected status {}", response.status()),
        };

        assert_eq!("icons/logo.webp", raster.filename);
        assert_eq!("image/webp", mime::sniff(&raster.data).mime);
    }

    #[test]
    fn passthrough_content_type_preserves_non_media_metadata() {
        let head = HeadResult {
            content_length: 10,
            content_type: "application/zip".to_owned(),
        };
        assert_eq!(
            "application/zip",
            passthrough_content_type(&head, "downloads/app.zip")
        );
    }

    #[test]
    fn passthrough_content_type_prefers_known_extension_over_bad_metadata() {
        let head = HeadResult {
            content_length: 10,
            content_type: "text/plain".to_owned(),
        };
        assert_eq!("image/png", passthrough_content_type(&head, "image.png"));
    }

    #[test]
    fn passthrough_content_type_prefers_m4a_extension_over_mp4_metadata() {
        let head = HeadResult {
            content_length: 10,
            content_type: "video/mp4".to_owned(),
        };
        assert_eq!("audio/mp4", passthrough_content_type(&head, "track.m4a"));
    }

    #[test]
    fn asset_size_query_is_clamped_by_kind() {
        let params = HashMap::from([("size".to_owned(), "4096".to_owned())]);
        let size = constants::parse_image_size(params.get("size").map(String::as_str));
        let selected = output_format::select_url_variant(output_format::Input {
            kind: AssetKind::Avatar,
            original: AssetExtension::Webp,
            requested_size: Some(size),
            manual_format_override: asset_manual_format_override(&params, AssetExtension::Webp),
        });
        assert_eq!(Some(1024), selected.size);
    }

    #[test]
    fn standard_asset_path_strips_virtual_animation_prefix_and_extension() {
        let parsed =
            parse_standard_asset_path("/avatars/1216100949629702144/a_d2f35261.webp").unwrap();
        assert_eq!("avatars/1216100949629702144/d2f35261", parsed.storage_key);
        assert_eq!(AssetExtension::Webp, parsed.original_ext);
        assert_eq!(AssetKind::Avatar, parsed.kind);
        assert!(asset_wants_animated(&HashMap::new(), &parsed.hash));
        assert!(!asset_wants_animated(
            &HashMap::from([("animated".to_owned(), "false".to_owned())]),
            &parsed.hash
        ));
    }

    #[test]
    fn guild_member_and_simple_asset_paths_match_v1_storage_keys() {
        let guild =
            parse_guild_member_asset_path("/guilds/1/users/2/avatars/a_memberhash.gif").unwrap();
        assert_eq!("guilds/1/users/2/avatars/memberhash", guild.storage_key);
        assert_eq!(AssetKind::Avatar, guild.kind);
        assert_eq!(AssetExtension::Gif, guild.original_ext);
        assert!(guild.forced_output_format.is_none());

        let emoji =
            parse_simple_asset_path("/emojis/1501314428688998182.webp", AssetKind::Emoji).unwrap();
        assert_eq!("emojis/1501314428688998182", emoji.storage_key);
        assert_eq!(AssetKind::Emoji, emoji.kind);
        assert!(emoji.forced_output_format.is_none());

        let sticker =
            parse_simple_asset_path("/stickers/1501314428688998182.png", AssetKind::Sticker)
                .unwrap();
        assert_eq!("stickers/1501314428688998182", sticker.storage_key);
        assert_eq!(Some(AssetExtension::Webp), sticker.forced_output_format);
    }

    #[test]
    fn asset_manual_format_override_is_v1_compatible() {
        assert_eq!(
            Some(AssetExtension::Webp),
            asset_manual_format_override(&HashMap::new(), AssetExtension::Webp)
        );
        assert_eq!(
            None,
            asset_manual_format_override(
                &HashMap::from([("format".to_owned(), "auto".to_owned())]),
                AssetExtension::Webp
            )
        );
        assert_eq!(
            Some(AssetExtension::Png),
            asset_manual_format_override(
                &HashMap::from([("fmt".to_owned(), "png".to_owned())]),
                AssetExtension::Webp
            )
        );
        assert_eq!(
            Some(AssetExtension::Webp),
            asset_manual_format_override(
                &HashMap::from([("format".to_owned(), "svg".to_owned())]),
                AssetExtension::Webp
            )
        );
    }

    #[test]
    fn text_responses_set_nosniff_header() {
        let response = text(StatusCode::NOT_FOUND, "Not Found");
        assert_eq!(
            "nosniff",
            response
                .headers()
                .get(header::X_CONTENT_TYPE_OPTIONS)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            http_headers::STRICT_TRANSPORT_SECURITY,
            response
                .headers()
                .get("strict-transport-security")
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert!(
            response
                .headers()
                .contains_key(header::CONTENT_SECURITY_POLICY)
        );
        assert!(response.headers().contains_key("permissions-policy"));
    }

    #[test]
    fn external_stream_length_requires_safe_passthrough() {
        assert_eq!(
            Some(1024),
            external_stream_length(true, Some(1024), "video/mp4")
        );
        assert_eq!(None, external_stream_length(false, Some(1024), "video/mp4"));
        assert_eq!(None, external_stream_length(true, None, "video/mp4"));
        assert_eq!(
            None,
            external_stream_length(true, Some(1024), "application/octet-stream")
        );
        assert_eq!(
            None,
            external_stream_length(true, Some(1024), "image/svg+xml")
        );
        assert_eq!(
            None,
            external_stream_length(
                true,
                Some(constants::MAX_MEDIA_PROXY_BYTES as u64 + 1),
                "video/mp4"
            )
        );
        assert_eq!(
            Some(constants::MAX_MEDIA_PROXY_BYTES as u64),
            external_stream_length(
                true,
                Some(constants::MAX_MEDIA_PROXY_BYTES as u64),
                "video/mp4"
            )
        );
    }

    #[tokio::test]
    async fn external_streaming_response_passes_body_through() {
        let upstream = reqwest::Response::from(
            http::Response::builder()
                .status(StatusCode::OK)
                .body("streamed bytes")
                .unwrap(),
        );
        let response = external_streaming_response(
            Method::GET,
            upstream,
            14,
            "video/mp4",
            Some("inline; filename=\"clip.mp4\"".to_owned()),
        );

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            "14",
            response
                .headers()
                .get(header::CONTENT_LENGTH)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            "video/mp4",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .unwrap()
                .to_str()
                .unwrap()
        );
        let body = to_bytes(response.into_body(), 64).await.unwrap();
        assert_eq!(b"streamed bytes", body.as_ref());
    }

    #[tokio::test]
    async fn external_partial_response_streams_partial_body() {
        let upstream = reqwest::Response::from(
            http::Response::builder()
                .status(StatusCode::PARTIAL_CONTENT)
                .body("abcd")
                .unwrap(),
        );
        let fetched = FetchedExternal {
            url: "https://media.example.test/clip.webm".to_owned(),
            status: StatusCode::PARTIAL_CONTENT,
            body: ExternalBody::Streaming {
                response: upstream,
                content_length: 4,
            },
            content_type: "video/webm".to_owned(),
            content_range: Some("bytes 0-3/10".to_owned()),
        };
        let response = external_partial_response(Method::GET, fetched, None);

        assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
        assert_eq!(
            "bytes 0-3/10",
            response
                .headers()
                .get(header::CONTENT_RANGE)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            "4",
            response
                .headers()
                .get(header::CONTENT_LENGTH)
                .unwrap()
                .to_str()
                .unwrap()
        );
        let body = to_bytes(response.into_body(), 64).await.unwrap();
        assert_eq!(b"abcd", body.as_ref());
    }

    #[test]
    fn external_partial_response_uses_media_headers() {
        let fetched = FetchedExternal {
            url: "https://media.example.test/clip.webm".to_owned(),
            status: StatusCode::PARTIAL_CONTENT,
            body: ExternalBody::Buffered(Bytes::from_static(b"abcd")),
            content_type: "video/webm".to_owned(),
            content_range: Some("bytes 0-3/10".to_owned()),
        };
        let response = external_partial_response(
            Method::GET,
            fetched,
            Some("inline; filename=\"clip.webm\"".to_owned()),
        );

        assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
        assert_eq!(
            "*",
            response
                .headers()
                .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            "video/webm",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            "bytes 0-3/10",
            response
                .headers()
                .get(header::CONTENT_RANGE)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            "inline; filename=\"clip.webm\"",
            response
                .headers()
                .get(header::CONTENT_DISPOSITION)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert!(
            response
                .headers()
                .contains_key(header::CONTENT_SECURITY_POLICY)
        );
        assert!(response.headers().contains_key("strict-transport-security"));
        assert!(response.headers().contains_key("permissions-policy"));
        assert!(
            response
                .headers()
                .contains_key(header::X_CONTENT_TYPE_OPTIONS)
        );
        assert!(response.headers().contains_key("CDN-Cache-Control"));
    }

    #[test]
    fn relay_cors_allows_client_context_headers() {
        let mut headers = HeaderMap::new();
        relay_cors(&mut headers);
        let allow_headers = headers
            .get(header::ACCESS_CONTROL_ALLOW_HEADERS)
            .unwrap()
            .to_str()
            .unwrap();

        assert!(
            allow_headers
                .split(',')
                .any(|name| name.trim().eq_ignore_ascii_case("x-fluxer-features"))
        );
        assert!(
            allow_headers
                .split(',')
                .any(|name| name.trim().eq_ignore_ascii_case("x-client-context"))
        );
    }

    #[tokio::test]
    async fn relay_put_accepts_unknown_content_length_body() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_root = tmp.path().canonicalize().unwrap();
        let storage_root = tmp_root.join("storage");
        let spool_dir = tmp_root.join("spool");
        tokio::fs::create_dir_all(&spool_dir).await.unwrap();
        let relay_secret = [7u8; 32];
        let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
        let key = "guild/diagnostics.txt";
        let token = upload_relay::encode_token(
            &upload_relay::TokenPayload {
                b: "uploads".to_owned(),
                k: key.to_owned(),
                m: upload_relay::TokenMethod::Put,
                u: None,
                p: None,
                ct: Some("text/plain".to_owned()),
                mb: 4096,
                e: upload_relay::now_unix() + 60,
            },
            &relay_secret,
        )
        .unwrap();
        let body = Bytes::from_static(b"diagnostics bundle");
        let request = Request::builder()
            .method(Method::PUT)
            .body(Body::from(body.clone()))
            .unwrap();
        assert!(request.headers().get(header::CONTENT_LENGTH).is_none());

        let response = relay_put(
            State(test_app_state(cfg)),
            Path(key.to_owned()),
            Query(HashMap::from([("t".to_owned(), token)])),
            HeaderMap::new(),
            request,
        )
        .await;

        assert_eq!(StatusCode::OK, response.status());
        let stored = tokio::fs::read(storage_root.join("uploads").join(key))
            .await
            .unwrap();
        assert_eq!(body.as_ref(), stored.as_slice());
    }

    fn relay_test_token(key: &str, relay_secret: &[u8]) -> String {
        upload_relay::encode_token(
            &upload_relay::TokenPayload {
                b: "uploads".to_owned(),
                k: key.to_owned(),
                m: upload_relay::TokenMethod::Put,
                u: None,
                p: None,
                ct: Some("application/octet-stream".to_owned()),
                mb: 4096,
                e: upload_relay::now_unix() + 60,
            },
            relay_secret,
        )
        .unwrap()
    }

    fn content_length_headers(declared: u64) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_LENGTH,
            HeaderValue::from_str(&declared.to_string()).unwrap(),
        );
        headers
    }

    #[tokio::test]
    async fn relay_put_streams_known_length_body_without_spooling() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_root = tmp.path().canonicalize().unwrap();
        let storage_root = tmp_root.join("storage");
        let spool_dir = tmp_root.join("spool");
        tokio::fs::create_dir_all(&spool_dir).await.unwrap();
        let relay_secret = [7u8; 32];
        let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
        let key = "guild/streamed.bin";
        let token = relay_test_token(key, &relay_secret);
        let body = Bytes::from_static(b"streamed straight through");

        let response = relay_put(
            State(test_app_state(cfg)),
            Path(key.to_owned()),
            Query(HashMap::from([("t".to_owned(), token)])),
            content_length_headers(body.len() as u64),
            Request::builder()
                .method(Method::PUT)
                .body(Body::from(body.clone()))
                .unwrap(),
        )
        .await;

        assert_eq!(StatusCode::OK, response.status());
        let stored = tokio::fs::read(storage_root.join("uploads").join(key))
            .await
            .unwrap();
        assert_eq!(body.as_ref(), stored.as_slice());
        let mut spool_entries = tokio::fs::read_dir(&spool_dir).await.unwrap();
        assert!(spool_entries.next_entry().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn relay_put_rejects_streaming_body_longer_than_declared() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_root = tmp.path().canonicalize().unwrap();
        let storage_root = tmp_root.join("storage");
        let spool_dir = tmp_root.join("spool");
        tokio::fs::create_dir_all(&spool_dir).await.unwrap();
        let relay_secret = [7u8; 32];
        let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
        let key = "guild/overrun.bin";
        let token = relay_test_token(key, &relay_secret);

        let response = relay_put(
            State(test_app_state(cfg)),
            Path(key.to_owned()),
            Query(HashMap::from([("t".to_owned(), token)])),
            content_length_headers(4),
            Request::builder()
                .method(Method::PUT)
                .body(Body::from(Bytes::from_static(b"way past four bytes")))
                .unwrap(),
        )
        .await;

        assert_eq!(StatusCode::PAYLOAD_TOO_LARGE, response.status());
        assert!(
            tokio::fs::try_exists(storage_root.join("uploads").join(key))
                .await
                .ok()
                != Some(true)
        );
    }

    #[tokio::test]
    async fn relay_put_rejects_streaming_body_shorter_than_declared() {
        let tmp = tempfile::tempdir().unwrap();
        let tmp_root = tmp.path().canonicalize().unwrap();
        let storage_root = tmp_root.join("storage");
        let spool_dir = tmp_root.join("spool");
        tokio::fs::create_dir_all(&spool_dir).await.unwrap();
        let relay_secret = [7u8; 32];
        let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
        let key = "guild/short.bin";
        let token = relay_test_token(key, &relay_secret);

        let response = relay_put(
            State(test_app_state(cfg)),
            Path(key.to_owned()),
            Query(HashMap::from([("t".to_owned(), token)])),
            content_length_headers(32),
            Request::builder()
                .method(Method::PUT)
                .body(Body::from(Bytes::from_static(b"tiny")))
                .unwrap(),
        )
        .await;

        assert_eq!(StatusCode::BAD_REQUEST, response.status());
        assert!(
            tokio::fs::try_exists(storage_root.join("uploads").join(key))
                .await
                .ok()
                != Some(true)
        );
    }

    #[test]
    fn upstream_status_is_clamped_to_known_whitelist() {
        assert_eq!(
            StatusCode::NOT_FOUND,
            map_upstream_status(StatusCode::NOT_FOUND)
        );
        assert_eq!(
            StatusCode::TOO_MANY_REQUESTS,
            map_upstream_status(StatusCode::TOO_MANY_REQUESTS)
        );
        assert_eq!(
            StatusCode::from_u16(428).unwrap(),
            map_upstream_status(StatusCode::from_u16(428).unwrap())
        );
        assert_eq!(
            StatusCode::BAD_GATEWAY,
            map_upstream_status(StatusCode::from_u16(451).unwrap())
        );
        assert_eq!(
            StatusCode::BAD_GATEWAY,
            map_upstream_status(StatusCode::INTERNAL_SERVER_ERROR)
        );
        assert_eq!(
            StatusCode::BAD_GATEWAY,
            map_upstream_status(StatusCode::SERVICE_UNAVAILABLE)
        );
    }

    #[test]
    fn internal_metadata_does_not_surface_origin_429() {
        assert_eq!(
            StatusCode::SERVICE_UNAVAILABLE,
            map_internal_metadata_upstream_status(StatusCode::TOO_MANY_REQUESTS)
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            map_internal_metadata_upstream_status(StatusCode::NOT_FOUND)
        );
    }

    #[test]
    fn asset_filename_rejects_unstable_shapes() {
        assert!(parse_asset_filename("hash.extra.png").is_none());
        assert!(parse_asset_filename("hash-with-dash.png").is_none());
        assert!(parse_asset_filename(".png").is_none());
        assert!(parse_asset_filename("hash.").is_none());
    }

    #[test]
    fn animated_gif_requests_downgrade_webp_output_to_gif() {
        assert_eq!(
            AssetExtension::Apng,
            effective_animated_image_output_format(
                Some(AssetExtension::Apng),
                AssetExtension::Png,
                true
            )
        );
        assert_eq!(
            AssetExtension::Png,
            effective_animated_image_output_format(
                Some(AssetExtension::Apng),
                AssetExtension::Png,
                false
            )
        );
        assert_eq!(
            AssetExtension::Gif,
            effective_animated_image_output_format(
                Some(AssetExtension::Gif),
                AssetExtension::Webp,
                true
            )
        );
        assert_eq!(
            AssetExtension::Webp,
            effective_animated_image_output_format(
                Some(AssetExtension::Gif),
                AssetExtension::Webp,
                false
            )
        );
        assert_eq!(
            AssetExtension::Webp,
            effective_animated_image_output_format(
                Some(AssetExtension::Webp),
                AssetExtension::Webp,
                true
            )
        );
        assert_eq!(
            AssetExtension::Png,
            effective_animated_image_output_format(
                Some(AssetExtension::Png),
                AssetExtension::Png,
                true
            )
        );
        assert_eq!(
            AssetExtension::Png,
            effective_animated_image_output_format(
                Some(AssetExtension::Png),
                AssetExtension::Png,
                false
            )
        );
        assert!(animated_param(
            &HashMap::from([("animatd".to_owned(), "true".to_owned())]),
            false
        ));
    }

    #[test]
    fn attachment_and_external_query_helpers_match_v1_edges() {
        for name in [
            "clip.mp4",
            "clip.m4v",
            "clip.webm",
            "clip.mov",
            "clip.ogv",
            "clip.mkv",
            "clip.3gp",
            "clip.avi",
            "clip.flv",
            "clip.ts",
            "clip.mpg",
            "clip.mpeg",
            "clip.wmv",
        ] {
            assert_eq!(None, image_extension_from_filename(name));
        }
        assert_eq!(
            AssetExtension::Gif,
            external_default_output_extension("welcome.png", "image/gif")
        );
        assert_eq!(
            AssetExtension::Png,
            external_default_output_extension("welcome.png", "application/octet-stream")
        );
        assert_eq!(
            "image/gif",
            transform_response_content_type(
                None,
                AssetExtension::Gif,
                AssetExtension::Gif,
                "image/gif"
            )
        );
        assert_eq!(
            "image/webp",
            transform_response_content_type(
                None,
                AssetExtension::Heic,
                AssetExtension::Webp,
                "image/heic"
            )
        );
        assert_eq!(
            "image/webp",
            transform_response_content_type(
                None,
                AssetExtension::Webp,
                AssetExtension::Webp,
                "image/svg+xml; charset=utf-8"
            )
        );
        assert_eq!(
            "image/gif",
            transform_response_content_type(
                Some(AssetExtension::Webp),
                AssetExtension::Webp,
                AssetExtension::Gif,
                "image/gif"
            )
        );
        assert_eq!(
            "file.png",
            url_filename("https://example.test/a/file.png?x=1#frag")
        );
        assert_eq!(
            "photo.png",
            filename_from_storage_key("attachments/123/456/photo.png")
        );
        assert_eq!(
            "file.bin",
            filename_from_storage_key("attachments/123/456/")
        );
        assert!(
            explicit_output_format(&HashMap::from([("format".to_owned(), "auto".to_owned())]))
                .is_err()
        );
    }

    #[test]
    fn explicit_download_disposition_uses_response_image_extension() {
        assert_eq!(
            "attachment; filename=\"welcome.gif\"",
            content_disposition_header("image/gif", true, Some("welcome.png"))
        );
        assert_eq!(
            "attachment; filename=\"welcome.gif\"",
            content_disposition_header("image/gif", true, Some("welcome"))
        );
        assert_eq!(
            "attachment; filename=\"photo.jpg\"",
            content_disposition_header("image/jpeg", true, Some("photo.jpg"))
        );
        assert_eq!(
            "attachment; filename=\"photo.jpg\"",
            content_disposition_header(
                "image/jpeg",
                true,
                Some(filename_from_storage_key("attachments/123/456/photo.jpg"))
            )
        );
        assert_eq!(
            "inline; filename=\"welcome.png\"",
            content_disposition_header("image/gif", false, Some("welcome.png"))
        );
    }

    #[test]
    fn same_format_gif_noop_requests_use_original_bytes() {
        let gif_header = b"GIF89a\x2c\x01\xe1\x00";
        assert!(same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: None,
                out_ext: AssetExtension::Gif,
                width: Some(300),
                height: Some(225),
                has_quality: false,
                effort: None,
                animated: true,
            }
        ));
        assert!(same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: None,
                out_ext: AssetExtension::Gif,
                width: Some(301),
                height: None,
                has_quality: false,
                effort: None,
                animated: true,
            }
        ));
        assert!(!same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: None,
                out_ext: AssetExtension::Gif,
                width: Some(299),
                height: None,
                has_quality: false,
                effort: None,
                animated: true,
            }
        ));
        assert!(!same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: Some(AssetExtension::Webp),
                out_ext: AssetExtension::Webp,
                width: None,
                height: None,
                has_quality: false,
                effort: None,
                animated: true,
            }
        ));
        assert!(!same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: Some(AssetExtension::Webp),
                out_ext: AssetExtension::Webp,
                width: Some(300),
                height: None,
                has_quality: false,
                effort: None,
                animated: false,
            }
        ));
        assert!(!same_format_loaded_image_request_can_use_original(
            gif_header,
            OriginalImageRequest {
                source_ext: Some(AssetExtension::Gif),
                explicit_out_ext: None,
                out_ext: AssetExtension::Gif,
                width: Some(300),
                height: None,
                has_quality: false,
                effort: Some(1),
                animated: false,
            }
        ));
    }
}
