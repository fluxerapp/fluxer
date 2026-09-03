// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{http_headers, server::state::AppState};
use axum::{
    body::Body,
    extract::{ConnectInfo, State},
    http::{HeaderValue, StatusCode, header},
    response::Response,
};
use std::{net::SocketAddr, sync::Arc};

pub(in crate::server) async fn health() -> &'static str {
    "OK"
}

fn is_loopback_peer(peer: &SocketAddr) -> bool {
    peer.ip().to_canonical().is_loopback()
}

pub(in crate::server) async fn metrics_handler(
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    State(app): State<Arc<AppState>>,
) -> Response {
    if !is_loopback_peer(&peer) {
        let mut denied = Response::new(Body::from("FORBIDDEN"));
        *denied.status_mut() = StatusCode::FORBIDDEN;
        http_headers::add_security_headers(denied.headers_mut());
        denied
            .headers_mut()
            .insert(header::CONTENT_TYPE, HeaderValue::from_static("text/plain"));
        return denied;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_peer(value: &str) -> SocketAddr {
        value.parse().expect("valid socket address")
    }

    #[test]
    fn metrics_guard_accepts_loopback_peers() {
        assert!(is_loopback_peer(&test_peer("127.0.0.1:5000")));
        assert!(is_loopback_peer(&test_peer("127.0.0.2:5000")));
        assert!(is_loopback_peer(&test_peer("[::1]:5000")));
        assert!(is_loopback_peer(&test_peer("[::ffff:127.0.0.1]:5000")));
    }

    #[test]
    fn metrics_guard_rejects_remote_peers() {
        assert!(!is_loopback_peer(&test_peer("8.8.8.8:5000")));
        assert!(!is_loopback_peer(&test_peer("10.0.0.5:5000")));
        assert!(!is_loopback_peer(&test_peer("172.18.0.4:5000")));
        assert!(!is_loopback_peer(&test_peer("[fe80::1]:5000")));
        assert!(!is_loopback_peer(&test_peer("[::ffff:8.8.8.8]:5000")));
    }
}
