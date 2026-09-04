// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::state::AppState;
use axum::{
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Response},
};

pub async fn health() -> &'static str {
    "OK"
}

pub async fn ready(State(state): State<AppState>) -> Response {
    readiness_report(state.discovery_cache.has_snapshot().await).into_response()
}

fn readiness_report(discovery_cached: bool) -> (StatusCode, &'static str) {
    if discovery_cached {
        (StatusCode::OK, "OK")
    } else {
        (StatusCode::SERVICE_UNAVAILABLE, "NOT READY: discovery")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::AppProxyConfig;
    use crate::discovery_cache::DiscoveryCache;
    use crate::state::build_http_client;
    use axum::Router;
    use axum::body::Body;
    use axum::http::{HeaderValue, Request as HttpRequest, header};
    use fluxer_common::config::GeoipSourceConfig;
    use fluxer_common::geoip::{GeoipConfig, GeoipResolver};
    use std::sync::Arc;
    use tower::ServiceExt;

    const DISCOVERY_BODY: &str = r#"{"api_code_version":"proxy-test"}"#;

    async fn spawn_discovery_origin() -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let router = Router::new().fallback(|| async {
            let mut response = Response::new(Body::from(DISCOVERY_BODY));
            response.headers_mut().insert(
                header::CONTENT_TYPE,
                HeaderValue::from_static("application/json"),
            );
            response
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        format!("http://{addr}/")
    }

    async fn probe_state() -> AppState {
        let mut config = AppProxyConfig::from_env();
        config.discovery_upstream_url = spawn_discovery_origin().await;
        let csp = Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        AppState {
            config: Arc::new(config),
            csp,
            http_client: build_http_client().unwrap(),
            discovery_cache: Arc::new(DiscoveryCache::new()),
            geoip: Arc::new(GeoipResolver::from_config(&GeoipConfig {
                geoip_source: GeoipSourceConfig::Filesystem {
                    maxmind_db_path: None,
                },
                geoip_s3_config: None,
                trust_client_ip_header: false,
                client_ip_header_name: "x-forwarded-for".to_owned(),
            })),
            index_html: None,
            budgets: crate::state::AppProxyBudgets::default(),
        }
    }

    async fn warm_discovery(state: &AppState) {
        state
            .discovery_cache
            .refresh(&state.http_client, &state.config.discovery_upstream_url)
            .await
            .unwrap();
    }

    async fn probe(state: AppState, path: &str) -> (StatusCode, String) {
        let response = crate::routes::build_router(state)
            .oneshot(
                HttpRequest::builder()
                    .uri(path)
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        let status = response.status();
        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        (status, String::from_utf8(body.to_vec()).unwrap())
    }

    #[tokio::test]
    async fn liveness_stays_constant_while_the_proxy_cannot_serve() {
        let state = probe_state().await;
        assert_eq!(
            probe(state, "/_health").await,
            (StatusCode::OK, "OK".to_owned())
        );
    }

    #[tokio::test]
    async fn readiness_fails_while_the_discovery_cache_is_empty() {
        let state = probe_state().await;
        let (status, body) = probe(state, "/_ready").await;
        assert_eq!(status, StatusCode::SERVICE_UNAVAILABLE);
        assert!(body.contains("discovery"), "{body}");
    }

    #[tokio::test]
    async fn readiness_passes_once_a_discovery_snapshot_is_cached() {
        let state = probe_state().await;
        warm_discovery(&state).await;
        assert_eq!(
            probe(state, "/_ready").await,
            (StatusCode::OK, "OK".to_owned())
        );
    }

    #[test]
    fn readiness_tracks_only_the_discovery_snapshot() {
        assert_eq!(readiness_report(true), (StatusCode::OK, "OK"));
        assert_eq!(
            readiness_report(false),
            (StatusCode::SERVICE_UNAVAILABLE, "NOT READY: discovery")
        );
    }
}
