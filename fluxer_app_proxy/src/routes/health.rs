// SPDX-License-Identifier: AGPL-3.0-or-later

pub async fn health() -> &'static str {
    "OK"
}

pub async fn ready() -> &'static str {
    "OK"
}

#[cfg(test)]
mod tests {
    use crate::config::AppProxyConfig;
    use crate::state::{AppState, SpaIndexSource, build_http_client};
    use axum::body::Body;
    use axum::http::{Request as HttpRequest, StatusCode};
    use std::sync::Arc;
    use tower::ServiceExt;

    fn probe_state() -> AppState {
        let config = AppProxyConfig::from_env();
        let csp = Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        let budgets = crate::state::AppProxyBudgets::default();
        let local_files =
            crate::local_files::LocalFileStore::load_blocking(std::path::Path::new("."), &budgets)
                .expect("the test static directory must exist");
        AppState {
            config: Arc::new(config),
            csp,
            http_client: build_http_client().unwrap(),
            spa_index_source: SpaIndexSource::bundled(""),
            local_asset_prefixes: None,
            budgets,
            local_files,
        }
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
    async fn liveness_answers_without_any_upstream() {
        assert_eq!(
            probe(probe_state(), "/_health").await,
            (StatusCode::OK, "OK".to_owned())
        );
    }

    #[tokio::test]
    async fn readiness_needs_no_discovery_upstream() {
        assert_eq!(
            probe(probe_state(), "/_ready").await,
            (StatusCode::OK, "OK".to_owned())
        );
    }
}
