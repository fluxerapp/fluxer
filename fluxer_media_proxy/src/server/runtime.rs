// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    middleware::{
        HttpRequestDrain, add_security_header_middleware, add_version_header, track_active_request,
    },
    routes,
    state::AppState,
};
use crate::{
    aggregate_error::aggregate_results,
    config::{Config, DeploymentMode, PolicyMode},
    media_process, mime, request_log,
};
use axum::{
    Router,
    http::{Extensions, HeaderMap, StatusCode, Version, header},
    middleware,
    routing::{any, get, post, put},
};
use std::{net::SocketAddr, sync::Arc, time::Duration};
use tokio::{net::TcpListener, time::timeout_at};
use tower_http::compression::{CompressionLayer, CompressionLevel};
use tracing::info;

pub async fn run(cfg: Config) -> anyhow::Result<()> {
    media_process::native_runtime::ensure_vips_init()?;
    let addr: SocketAddr = format!("{}:{}", cfg.bind_host, cfg.port).parse()?;
    let state = Arc::new(AppState::try_new(cfg)?);
    if let Some(read_endpoint) = state.cfg.storage.s3_read_endpoint.as_deref() {
        info!(
            endpoint = read_endpoint,
            bucket = state.cfg.storage.s3_read_bucket,
            style = ?state.cfg.storage.s3_read_bucket_style,
            signed = state.cfg.storage.s3_read_signed,
            "object body reads served from the S3 read endpoint"
        );
    }
    if state.cfg.cors.mode != PolicyMode::Off {
        let allowed_origins = state
            .cfg
            .cors
            .allowed_origins
            .iter()
            .filter_map(|origin| origin.to_str().ok())
            .collect::<Vec<_>>()
            .join(",");
        info!(
            mode = ?state.cfg.cors.mode,
            allowed_origins = %allowed_origins,
            "cross-origin read policy active"
        );
    }
    if state.cfg.attachment_signature.mode != PolicyMode::Off {
        info!(
            mode = ?state.cfg.attachment_signature.mode,
            secrets = state.cfg.attachment_signature.secret_count(),
            "attachment url signature policy active"
        );
    }
    let drain = HttpRequestDrain::new();
    let app = build_router(Arc::clone(&state), drain.clone());
    let listener = TcpListener::bind(addr).await?;
    info!(%addr, "media proxy listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .with_graceful_shutdown(fluxer_common::shutdown::wait_for_shutdown())
    .await?;
    drain_and_shutdown(&state, &drain).await
}

const STATIC_COMPRESSIBLE_CONTENT_TYPES: [&str; 9] = [
    "application/javascript",
    "text/javascript",
    "text/css",
    "text/html",
    "application/json",
    "application/manifest+json",
    "application/xml",
    "application/wasm",
    "image/svg+xml",
];

const STATIC_COMPRESSION_MIN_BYTES: u64 = 1024;

fn is_compressible_static_response(
    status: StatusCode,
    _version: Version,
    headers: &HeaderMap,
    _extensions: &Extensions,
) -> bool {
    let content_type = headers
        .get(header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok());
    status == StatusCode::OK
        && headers
            .get(header::CONTENT_LENGTH)
            .and_then(|value| value.to_str().ok())
            .and_then(|value| value.parse::<u64>().ok())
            .is_some_and(|length| length >= STATIC_COMPRESSION_MIN_BYTES)
        && mime::normalize(content_type).is_some_and(|content_type| {
            STATIC_COMPRESSIBLE_CONTENT_TYPES
                .iter()
                .any(|compressible| compressible.eq_ignore_ascii_case(content_type))
        })
}

fn build_router(state: Arc<AppState>, drain: HttpRequestDrain) -> Router {
    let router = Router::new()
        .route("/_health", get(routes::ops::health))
        .route("/_metrics", get(routes::ops::metrics_handler))
        .route("/_metadata", post(routes::internal::metadata_handler))
        .route("/_sniff", post(routes::internal::sniff_handler))
        .route("/_thumbnail", post(routes::internal::thumbnail_handler))
        .route("/_frames", post(routes::internal::frames_handler))
        .route("/_strip", post(routes::internal::strip_handler))
        .route(
            "/_audio_duration",
            post(routes::internal::audio_duration_handler),
        )
        .route(
            "/v1/relay/{*key}",
            put(routes::relay::relay_put).options(routes::relay::relay_options),
        )
        .fallback(any(routes::dispatch::catch_all));
    let router = if state.cfg.mode == DeploymentMode::Static {
        router.layer(
            CompressionLayer::new()
                .no_br()
                .no_deflate()
                .no_zstd()
                .quality(CompressionLevel::Precise(5))
                .compress_when(is_compressible_static_response),
        )
    } else {
        router
    };
    router
        .layer(middleware::from_fn(add_version_header))
        .layer(middleware::from_fn_with_state(
            state.metrics.request(),
            request_log::trace,
        ))
        .layer(middleware::from_fn_with_state(
            state.cfg.mode,
            add_security_header_middleware,
        ))
        .layer(middleware::from_fn_with_state(drain, track_active_request))
        .with_state(state)
}

async fn drain_and_shutdown(state: &Arc<AppState>, drain: &HttpRequestDrain) -> anyhow::Result<()> {
    let grace_ms = state.cfg.shutdown_grace_ms;
    let deadline = tokio::time::Instant::now() + Duration::from_millis(grace_ms);
    info!(
        active_requests = drain.active_requests(),
        grace_ms, "media proxy draining"
    );
    let requests = timeout_at(deadline, drain.wait_for_requests_drained())
        .await
        .map_err(|_| anyhow::anyhow!("media proxy http request shutdown exceeded {grace_ms} ms"));
    let transforms = state.media.transforms();
    transforms.cache().begin_shutdown();
    transforms.tasks().begin_shutdown();
    let coalescer = timeout_at(deadline, transforms.cache().wait_for_shutdown())
        .await
        .map_err(|_| {
            anyhow::anyhow!("media proxy transform coalescer shutdown exceeded {grace_ms} ms")
        });
    let native_tasks = timeout_at(deadline, transforms.tasks().wait_for_shutdown())
        .await
        .map_err(|_| anyhow::anyhow!("media proxy native task shutdown exceeded {grace_ms} ms"));
    aggregate_results("media proxy shutdown", [requests, coalescer, native_tasks])
}
