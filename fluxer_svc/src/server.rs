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
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::net::TcpListener;

#[derive(Clone)]
pub struct HttpState {
    pub is_serving: Arc<AtomicBool>,
    pub render: MetricsRender,
}

pub type MetricsRender = Arc<dyn Fn() -> String + Send + Sync>;

pub async fn run_http(
    addr: SocketAddr,
    metrics_addr: Option<SocketAddr>,
    is_serving: Arc<AtomicBool>,
    metrics: Arc<ServiceMetrics>,
    service_name: String,
    build_version: String,
) -> anyhow::Result<()> {
    let render: MetricsRender = Arc::new(move || metrics.render_prometheus(&service_name));
    let version = HeaderValue::from_str(&build_version).ok();
    let app = ops_router(is_serving, render.clone())
        .layer(middleware::from_fn(move |request, next| {
            add_version_header(version.clone(), request, next)
        }))
        .layer(fluxer_common::svc_span::CurrentSpanLayer::new());
    let listener = TcpListener::bind(addr).await?;
    tracing::info!(addr = %addr, "health HTTP server listening");
    let public = async {
        axum::serve(
            listener,
            app.into_make_service_with_connect_info::<SocketAddr>(),
        )
        .await
        .map_err(anyhow::Error::from)
    };
    let Some(metrics_addr) = metrics_addr else {
        return public.await;
    };
    let metrics_listener = TcpListener::bind(metrics_addr).await?;
    tracing::info!(addr = %metrics_addr, "metrics HTTP server listening");
    let internal = async {
        axum::serve(metrics_listener, metrics_router(render))
            .await
            .map_err(anyhow::Error::from)
    };
    tokio::try_join!(public, internal)?;
    Ok(())
}

pub fn metrics_router(render: MetricsRender) -> Router {
    Router::new()
        .route("/_metrics", get(metrics_port_handler))
        .with_state(render)
}

pub fn ops_router(is_serving: Arc<AtomicBool>, render: MetricsRender) -> Router {
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
    metrics_response((state.render)())
}

async fn metrics_port_handler(State(render): State<MetricsRender>) -> Response {
    metrics_response(render())
}

fn metrics_response(body: String) -> Response {
    (
        [(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/plain; version=0.0.4; charset=utf-8"),
        )],
        body,
    )
        .into_response()
}

async fn add_version_header(
    version: Option<HeaderValue>,
    request: Request<Body>,
    next: Next,
) -> Response {
    let mut response = next.run(request).await;
    if let Some(value) = version {
        response.headers_mut().insert("x-fluxer-version", value);
    }
    response
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower::ServiceExt;

    fn render() -> MetricsRender {
        Arc::new(|| "fluxer_test_total 1\n".to_owned())
    }

    fn request_from(path: &str, peer: &str) -> Request<Body> {
        let mut request = Request::get(path).body(Body::empty()).expect("request");
        request.extensions_mut().insert(ConnectInfo(
            peer.parse::<SocketAddr>().expect("valid socket address"),
        ));
        request
    }

    #[tokio::test]
    async fn metrics_port_serves_a_non_loopback_peer() {
        let response = metrics_router(render())
            .oneshot(request_from("/_metrics", "10.244.1.7:5000"))
            .await
            .expect("response");
        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            "text/plain; version=0.0.4; charset=utf-8",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .expect("content type")
        );
    }

    #[tokio::test]
    async fn metrics_port_answers_404_for_other_paths() {
        for path in ["/", "/_health", "/_healthz", "/_metrics/extra"] {
            let response = metrics_router(render())
                .oneshot(request_from(path, "10.244.1.7:5000"))
                .await
                .expect("response");
            assert_eq!(StatusCode::NOT_FOUND, response.status(), "{path}");
        }
    }

    #[tokio::test]
    async fn public_port_refuses_metrics_to_a_non_loopback_peer() {
        let router = ops_router(Arc::new(AtomicBool::new(true)), render());
        let refused = router
            .clone()
            .oneshot(request_from("/_metrics", "10.244.1.7:5000"))
            .await
            .expect("response");
        assert_eq!(StatusCode::FORBIDDEN, refused.status());
        let served = router
            .oneshot(request_from("/_metrics", "127.0.0.1:5000"))
            .await
            .expect("response");
        assert_eq!(StatusCode::OK, served.status());
    }
}
