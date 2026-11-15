// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    http_headers,
    server::{response::error::text, state::AppState},
};
use axum::{
    body::Body,
    extract::{ConnectInfo, State},
    http::{HeaderValue, StatusCode, header},
    response::Response,
};
use std::{net::SocketAddr, sync::Arc};

pub(in crate::server) async fn health(State(app): State<Arc<AppState>>) -> String {
    format!(
        "OK attachment_signature={}",
        app.cfg.attachment_signature.mode.label()
    )
}

pub(in crate::server) async fn metrics_handler(
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    State(app): State<Arc<AppState>>,
) -> Response {
    if !is_loopback_peer(&peer) {
        return text(StatusCode::FORBIDDEN, "FORBIDDEN");
    }
    let mut response = Response::new(Body::from(app.metrics.render()));
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

use fluxer_common::metrics::is_loopback_peer;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use axum::body::to_bytes;

    fn test_peer(value: &str) -> SocketAddr {
        value.parse().expect("valid socket address")
    }

    fn test_app_state() -> Arc<AppState> {
        let cfg = Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .expect("test config");
        Arc::new(AppState::for_tests(cfg))
    }

    #[tokio::test]
    async fn metrics_refuse_a_non_loopback_peer() {
        let response = metrics_handler(
            ConnectInfo(test_peer("8.8.8.8:5000")),
            State(test_app_state()),
        )
        .await;

        assert_eq!(StatusCode::FORBIDDEN, response.status());
        assert_eq!(
            "text/plain; charset=utf-8",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .expect("content type")
        );
        assert_eq!(
            "no-store",
            response
                .headers()
                .get(header::CACHE_CONTROL)
                .expect("cache policy")
        );
        let body = to_bytes(response.into_body(), 64).await.expect("body");
        assert_eq!(b"FORBIDDEN", body.as_ref());
    }
}
