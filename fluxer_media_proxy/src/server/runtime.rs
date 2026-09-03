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
    bunny_ip_gate::{self, BunnyIpGate},
    config::Config,
    media_process, request_log,
};
use anyhow::Context as _;
use axum::{
    Router, middleware,
    routing::{any, get, post, put},
};
use std::{net::SocketAddr, sync::Arc, time::Duration};
use tokio::{net::TcpListener, time::timeout_at};
use tracing::info;

pub async fn run(cfg: Config) -> anyhow::Result<()> {
    media_process::warmup_vips()?;
    let addr: SocketAddr = format!("{}:{}", cfg.bind_host, cfg.port).parse()?;
    let state = Arc::new(AppState::try_new(cfg)?);
    let bunny_gate = start_bunny_ip_gate(&state).await?;
    if let Some(read_endpoint) = state.cfg.storage.s3_read_endpoint.as_deref() {
        info!(
            endpoint = read_endpoint,
            bucket = state.cfg.storage.s3_read_bucket,
            style = ?state.cfg.storage.s3_read_bucket_style,
            signed = state.cfg.storage.s3_read_signed,
            "object body reads served from the S3 read endpoint"
        );
    }
    let drain = HttpRequestDrain::new();
    let app = build_router(Arc::clone(&state), bunny_gate, drain.clone());
    let listener = TcpListener::bind(addr).await?;
    info!(%addr, "media proxy listening");
    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .with_graceful_shutdown(shutdown_signal())
    .await?;
    drain_and_shutdown(&state, &drain).await
}

fn build_router(
    state: Arc<AppState>,
    bunny_gate: Option<Arc<BunnyIpGate>>,
    drain: HttpRequestDrain,
) -> Router {
    let mut router = Router::new()
        .route("/_health", get(routes::ops::health))
        .route("/_metrics", get(routes::ops::metrics_handler))
        .route("/_metadata", post(routes::internal::metadata_handler))
        .route("/_thumbnail", post(routes::internal::thumbnail_handler))
        .route("/_frames", post(routes::internal::frames_handler))
        .route(
            "/v1/relay/{*key}",
            put(routes::relay::relay_put).options(routes::relay::relay_options),
        )
        .fallback(any(routes::dispatch::catch_all))
        .layer(middleware::from_fn(add_version_header))
        .layer(middleware::from_fn_with_state(
            state.metrics.request(),
            request_log::trace,
        ));
    if let Some(gate) = bunny_gate {
        router = router.layer(middleware::from_fn_with_state(
            gate,
            bunny_ip_gate::gate_middleware,
        ));
    }
    router
        .layer(middleware::from_fn_with_state(
            state.cfg.mode,
            add_security_header_middleware,
        ))
        .layer(middleware::from_fn_with_state(drain, track_active_request))
        .with_state(state)
}

async fn start_bunny_ip_gate(state: &Arc<AppState>) -> anyhow::Result<Option<Arc<BunnyIpGate>>> {
    if !state.cfg.bunny_ip_gate_enabled {
        return Ok(None);
    }
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
    Ok(Some(gate))
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

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Method, Request, StatusCode},
    };

    const DECLARED_ROUTES: &[(&str, &str, &str)] = &[
        ("/_health", "/_health", "GET,HEAD"),
        ("/_metrics", "/_metrics", "GET,HEAD"),
        ("/_metadata", "/_metadata", "POST"),
        ("/_thumbnail", "/_thumbnail", "POST"),
        ("/_frames", "/_frames", "POST"),
        (
            "/v1/relay/{*key}",
            "/v1/relay/uploads/file.png",
            "PUT,OPTIONS",
        ),
    ];

    fn test_router() -> Router {
        let cfg = Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .expect("test config");
        build_router(
            Arc::new(AppState::for_tests(cfg)),
            None,
            HttpRequestDrain::new(),
        )
    }

    async fn probe(path: &str, method: Method) -> axum::response::Response {
        tower::ServiceExt::oneshot(
            test_router(),
            Request::builder()
                .method(method)
                .uri(path)
                .body(Body::empty())
                .expect("probe request"),
        )
        .await
        .expect("router response")
    }

    #[tokio::test]
    async fn router_exposes_exactly_the_declared_route_and_method_table() {
        for (declared, path, allowed) in DECLARED_ROUTES {
            let response = probe(path, Method::DELETE).await;
            assert_eq!(
                StatusCode::METHOD_NOT_ALLOWED,
                response.status(),
                "{declared} must reject an undeclared method"
            );
            assert_eq!(
                *allowed,
                response
                    .headers()
                    .get("allow")
                    .expect("method router sets allow")
                    .to_str()
                    .expect("ascii allow header"),
                "{declared} method table"
            );
        }
    }

    #[tokio::test]
    async fn unknown_paths_reach_the_catch_all_fallback() {
        let response = probe("/avatars/1/hash.png", Method::DELETE).await;
        assert_eq!(StatusCode::METHOD_NOT_ALLOWED, response.status());
        assert!(response.headers().get("allow").is_none());
        let response = probe("/nope", Method::GET).await;
        assert_eq!(StatusCode::NOT_FOUND, response.status());
    }
}
