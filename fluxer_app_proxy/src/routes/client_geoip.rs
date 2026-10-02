// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::geoip::build_geoip_response;
use crate::state::AppState;
use axum::{
    Json,
    extract::State,
    http::{HeaderMap, HeaderValue, header},
    response::{IntoResponse, Response},
};

pub const CLIENT_GEOIP_PATH: &str = "/_geoip";
const CLIENT_GEOIP_CACHE_CONTROL: &str = "no-store, private";

pub async fn client_geoip(State(state): State<AppState>, headers: HeaderMap) -> Response {
    let mut response = Json(build_geoip_response(state.geoip.lookup(&headers))).into_response();
    response.headers_mut().insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(CLIENT_GEOIP_CACHE_CONTROL),
    );
    response
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::AppProxyConfig;
    use crate::discovery_cache::DiscoveryCache;
    use crate::routes::build_router;
    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use fluxer_common::config::GeoipSourceConfig;
    use fluxer_common::geoip::{GeoipConfig, GeoipLookup, GeoipResolver};
    use std::sync::Arc;
    use tower::ServiceExt;

    fn geoip_state() -> AppState {
        let config = AppProxyConfig::from_env();
        let csp = Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        AppState {
            config: Arc::new(config),
            csp,
            http_client: reqwest::Client::new(),
            discovery_cache: Arc::new(DiscoveryCache::new()),
            geoip: Arc::new(GeoipResolver::from_config(&GeoipConfig {
                geoip_source: GeoipSourceConfig::Filesystem {
                    maxmind_db_path: None,
                },
                geoip_s3_config: None,
                trust_client_ip_header: true,
                client_ip_header_name: "x-forwarded-for".to_owned(),
            })),
            index_html: Some(Arc::from("<html><head></head><body></body></html>")),
            local_asset_prefixes: None,
            budgets: crate::state::AppProxyBudgets::default(),
        }
    }

    #[tokio::test]
    async fn the_geoip_endpoint_answers_for_the_requesting_client_and_is_never_stored() {
        let response = build_router(geoip_state())
            .oneshot(
                Request::get(CLIENT_GEOIP_PATH)
                    .header("x-forwarded-for", "203.0.113.10")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        let headers = response.headers();
        assert_eq!(
            headers.get(header::CACHE_CONTROL).unwrap(),
            CLIENT_GEOIP_CACHE_CONTROL
        );
        assert!(
            headers
                .get(header::CONTENT_TYPE)
                .unwrap()
                .to_str()
                .unwrap()
                .starts_with("application/json"),
            "the SPA fallback answered the geoip endpoint with the app shell"
        );
        assert!(headers.get("x-fluxer-app-shell").is_none());

        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        let body: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(body, build_geoip_response(GeoipLookup::default()));
        assert_eq!(body["countryCode"], serde_json::Value::Null);
    }
}
