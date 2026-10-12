// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::ServiceMetrics;
use axum::Router;
use axum::body::Body;
use axum::extract::{ConnectInfo, Request, State};
use axum::http::{HeaderValue, StatusCode, header};
use axum::middleware::{self, Next};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use fluxer_common::metrics::is_loopback_peer;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::net::TcpListener;

#[derive(Clone)]
pub struct HttpState {
    pub is_serving: Arc<AtomicBool>,
    pub render: Arc<dyn Fn() -> String + Send + Sync>,
}

pub async fn run_http(
    addr: SocketAddr,
    is_serving: Arc<AtomicBool>,
    metrics: Arc<ServiceMetrics>,
    service_name: String,
) -> anyhow::Result<()> {
    let render = Arc::new(move || metrics.render_prometheus(&service_name));
    let app = ops_router(is_serving, render).layer(middleware::from_fn(add_version_header));
    let listener = TcpListener::bind(addr).await?;
    tracing::info!(addr = %addr, "health HTTP server listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await?;
    Ok(())
}

pub fn ops_router(
    is_serving: Arc<AtomicBool>,
    render: Arc<dyn Fn() -> String + Send + Sync>,
) -> Router {
    Router::new()
        .route("/_health", get(readiness_check))
        .route("/_healthz", get(|| async { "OK" }))
        .route("/_metrics", get(metrics_handler))
        .with_state(HttpState { is_serving, render })
}

async fn readiness_check(State(state): State<HttpState>) -> impl IntoResponse {
    if state.is_serving.load(Ordering::SeqCst) {
        (StatusCode::OK, "OK")
    } else {
        (StatusCode::SERVICE_UNAVAILABLE, "NOT READY")
    }
}

async fn metrics_handler(
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    State(state): State<HttpState>,
) -> Response {
    if !is_loopback_peer(&peer) {
        return (StatusCode::FORBIDDEN, "FORBIDDEN").into_response();
    }
    let body = (state.render)();
    (
        [(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/plain; version=0.0.4; charset=utf-8"),
        )],
        body,
    )
        .into_response()
}

fn build_version() -> &'static str {
    static BUILD_VERSION: OnceLock<String> = OnceLock::new();
    BUILD_VERSION
        .get_or_init(|| {
            crate::config::optional_env("BUILD_VERSION").unwrap_or_else(|| "dev".to_owned())
        })
        .as_str()
}

async fn add_version_header(request: Request<Body>, next: Next) -> Response {
    let mut response = next.run(request).await;
    if let Ok(value) = HeaderValue::from_str(build_version()) {
        response.headers_mut().insert("x-fluxer-version", value);
    }
    response
}
