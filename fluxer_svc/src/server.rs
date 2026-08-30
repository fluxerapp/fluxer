// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::ServiceMetrics;
use axum::Router;
use axum::body::Body;
use axum::extract::{ConnectInfo, Request, State};
use axum::http::{HeaderValue, StatusCode, header};
use axum::middleware::{self, Next};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::net::TcpListener;

#[derive(Clone)]
pub struct HttpState {
    pub is_serving: Arc<AtomicBool>,
    pub metrics: Arc<ServiceMetrics>,
    pub service_name: String,
}

pub async fn run_http(
    addr: SocketAddr,
    is_serving: Arc<AtomicBool>,
    metrics: Arc<ServiceMetrics>,
    service_name: String,
) -> anyhow::Result<()> {
    let state = HttpState {
        is_serving,
        metrics,
        service_name,
    };
    let app = Router::new()
        .route("/_health", get(readiness_check))
        .route("/_healthz", get(|| async { "OK" }))
        .route("/_metrics", get(metrics_handler))
        .with_state(state)
        .layer(middleware::from_fn(add_version_header));
    let listener = TcpListener::bind(addr).await?;
    tracing::info!(addr = %addr, "health HTTP server listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await?;
    Ok(())
}

async fn readiness_check(State(state): State<HttpState>) -> impl IntoResponse {
    if state.is_serving.load(Ordering::SeqCst) {
        (StatusCode::OK, "OK")
    } else {
        (StatusCode::SERVICE_UNAVAILABLE, "NOT READY")
    }
}

fn is_loopback_peer(peer: &SocketAddr) -> bool {
    peer.ip().to_canonical().is_loopback()
}

async fn metrics_handler(
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    State(state): State<HttpState>,
) -> Response {
    if !is_loopback_peer(&peer) {
        return (StatusCode::FORBIDDEN, "FORBIDDEN").into_response();
    }
    let body = state.metrics.render_prometheus(&state.service_name);
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
            std::env::var("BUILD_VERSION")
                .ok()
                .filter(|v| !v.trim().is_empty())
                .unwrap_or_else(|| "dev".to_owned())
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

#[cfg(test)]
mod tests {
    use super::is_loopback_peer;
    use std::net::SocketAddr;

    fn peer(value: &str) -> SocketAddr {
        value.parse().expect("valid socket address")
    }

    #[test]
    fn accepts_loopback_peers() {
        assert!(is_loopback_peer(&peer("127.0.0.1:5000")));
        assert!(is_loopback_peer(&peer("127.0.0.2:5000")));
        assert!(is_loopback_peer(&peer("[::1]:5000")));
        assert!(is_loopback_peer(&peer("[::ffff:127.0.0.1]:5000")));
    }

    #[test]
    fn rejects_remote_peers() {
        assert!(!is_loopback_peer(&peer("8.8.8.8:5000")));
        assert!(!is_loopback_peer(&peer("10.0.0.5:5000")));
        assert!(!is_loopback_peer(&peer("172.18.0.4:5000")));
        assert!(!is_loopback_peer(&peer("[fe80::1]:5000")));
        assert!(!is_loopback_peer(&peer("[::ffff:8.8.8.8]:5000")));
    }
}
