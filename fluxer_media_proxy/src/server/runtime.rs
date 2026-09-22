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
    config::{Config, PolicyMode},
    media_process, request_log,
};
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
    .with_graceful_shutdown(shutdown_signal())
    .await?;
    drain_and_shutdown(&state, &drain).await
}

fn build_router(state: Arc<AppState>, drain: HttpRequestDrain) -> Router {
    Router::new()
        .route("/_health", get(routes::ops::health))
        .route("/_metrics", get(routes::ops::metrics_handler))
        .route("/_metadata", post(routes::internal::metadata_handler))
        .route("/_sniff", post(routes::internal::sniff_handler))
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
    use crate::http_headers;
    use axum::{
        body::Body,
        extract::ConnectInfo,
        http::{Method, Request, StatusCode, header},
        response::Response,
    };
    use base64::{Engine as _, engine::general_purpose};

    const ENFORCE: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
        (
            "FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS",
            "https://web.fluxer.app",
        ),
    ];

    const DECLARED_ROUTES: &[(&str, &str, &str)] = &[
        ("/_health", "/_health", "GET,HEAD"),
        ("/_metrics", "/_metrics", "GET,HEAD"),
        ("/_metadata", "/_metadata", "POST"),
        ("/_sniff", "/_sniff", "POST"),
        ("/_thumbnail", "/_thumbnail", "POST"),
        ("/_frames", "/_frames", "POST"),
        (
            "/v1/relay/{*key}",
            "/v1/relay/uploads/file.png",
            "PUT,OPTIONS",
        ),
    ];

    fn test_config(mode: &str, extra: &[(&str, &str)]) -> Config {
        let mut env = vec![
            (
                "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                "secret".to_owned(),
            ),
            ("FLUXER_MEDIA_PROXY_MODE".to_owned(), mode.to_owned()),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64".to_owned(),
                general_purpose::STANDARD.encode([7u8; 32]),
            ),
        ];
        env.extend(
            extra
                .iter()
                .map(|(key, value)| ((*key).to_owned(), (*value).to_owned())),
        );
        Config::load_from_iter(env).expect("test config")
    }

    fn test_router_for(cfg: Config) -> Router {
        build_router(Arc::new(AppState::for_tests(cfg)), HttpRequestDrain::new())
    }

    async fn send(router: Router, request: Request<Body>) -> Response {
        tower::ServiceExt::oneshot(router, request)
            .await
            .expect("router response")
    }

    async fn probe(router: Router, path: &str, method: Method) -> Response {
        send(
            router,
            Request::builder()
                .method(method)
                .uri(path)
                .body(Body::empty())
                .expect("probe request"),
        )
        .await
    }

    fn header_text(response: &Response, name: impl header::AsHeaderName) -> Option<&str> {
        response
            .headers()
            .get(name)
            .map(|value| value.to_str().expect("header is ascii"))
    }

    const SIGNATURE_SECRET: [u8; 32] = [11u8; 32];
    const SIGNATURE_REPORT: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "report"),
        (
            "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
            "CwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCws=",
        ),
    ];
    const SIGNATURE_ENFORCE: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "enforce"),
        (
            "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
            "CwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCws=",
        ),
    ];
    const ATTACHMENT_PATH: &str = "/attachments/1/2/file.bin";
    const ATTACHMENT_KEY: &str = "attachments/1/2/file.bin";

    async fn body_text(response: Response) -> String {
        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .expect("response body");
        String::from_utf8(body.to_vec()).expect("body is utf-8")
    }

    #[tokio::test]
    async fn health_names_the_attachment_signature_mode() {
        for (extra, expected) in [
            (&[][..], "off"),
            (SIGNATURE_REPORT, "report"),
            (SIGNATURE_ENFORCE, "enforce"),
        ] {
            let router = test_router_for(test_config("mp", extra));
            let response = probe(router, "/_health", Method::GET).await;
            assert_eq!(StatusCode::OK, response.status(), "{expected}");
            assert_eq!(
                format!("OK attachment_signature={expected}"),
                body_text(response).await,
                "{expected}"
            );
        }
    }

    #[tokio::test]
    async fn an_enforced_refusal_is_countable_apart_from_a_genuine_missing_object() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let mut env: Vec<(String, String)> = SIGNATURE_ENFORCE
            .iter()
            .map(|(key, value)| ((*key).to_owned(), (*value).to_owned()))
            .collect();
        env.push((
            "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
            "secret".to_owned(),
        ));
        env.push((
            "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
            "local".to_owned(),
        ));
        env.push((
            "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
            root.display().to_string(),
        ));
        let cfg = Config::load_from_iter(env).expect("signature router config");
        let state = Arc::new(AppState::for_tests(cfg));
        let router = build_router(Arc::clone(&state), HttpRequestDrain::new());

        let refused = probe(router.clone(), ATTACHMENT_PATH, Method::GET).await;
        assert_eq!(StatusCode::NOT_FOUND, refused.status());

        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("the clock is after the unix epoch")
            .as_secs();
        let signed = fluxer_common::attachment_url_signature::with_signature(
            ATTACHMENT_PATH,
            ATTACHMENT_KEY,
            now,
            now,
            &SIGNATURE_SECRET,
        );
        let missing = probe(router, &signed, Method::GET).await;
        assert_eq!(StatusCode::NOT_FOUND, missing.status());

        let text = state.metrics.render();
        assert!(
            text.contains("fluxer_media_proxy_requests_4xx_total{kind=\"attachment\"} 2\n"),
            "{text}"
        );
        assert!(
            text.contains(
                "fluxer_media_proxy_attachment_signature_verdicts_total{verdict=\"missing\"} 1\n"
            ),
            "{text}"
        );
        assert!(
            text.contains(
                "fluxer_media_proxy_attachment_signature_verdicts_total{verdict=\"valid\"} 1\n"
            ),
            "{text}"
        );
    }

    #[tokio::test]
    async fn router_exposes_exactly_the_declared_route_and_method_table() {
        for mode in ["mp", "static", "upload", "relay"] {
            let router = test_router_for(test_config(mode, &[]));
            for (declared, path, allowed) in DECLARED_ROUTES {
                let response = probe(router.clone(), path, Method::DELETE).await;
                assert_eq!(
                    StatusCode::METHOD_NOT_ALLOWED,
                    response.status(),
                    "{mode} {declared} must reject an undeclared method"
                );
                assert_eq!(
                    Some(*allowed),
                    header_text(&response, header::ALLOW),
                    "{mode} {declared} method table"
                );
            }
        }
    }

    #[tokio::test]
    async fn unknown_paths_reach_the_catch_all_fallback() {
        let router = test_router_for(test_config("mp", &[]));
        let response = probe(router.clone(), "/avatars/1/hash.png", Method::DELETE).await;
        assert_eq!(StatusCode::METHOD_NOT_ALLOWED, response.status());
        assert!(response.headers().get("allow").is_none());
        let response = probe(router, "/nope", Method::GET).await;
        assert_eq!(StatusCode::NOT_FOUND, response.status());
    }

    #[tokio::test]
    async fn relay_mode_router_serves_health_relay_and_internal_but_not_reads() {
        let router = test_router_for(test_config("relay", &[]));

        let health = probe(router.clone(), "/_health", Method::GET).await;
        assert_eq!(StatusCode::OK, health.status());

        let options = probe(
            router.clone(),
            "/v1/relay/uploads/file.png",
            Method::OPTIONS,
        )
        .await;
        assert_eq!(StatusCode::NO_CONTENT, options.status());
        assert_eq!(
            Some("*"),
            header_text(&options, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );
        assert_eq!(
            Some("PUT, OPTIONS"),
            header_text(&options, header::ACCESS_CONTROL_ALLOW_METHODS)
        );

        let put = probe(router.clone(), "/v1/relay/uploads/file.png", Method::PUT).await;
        assert_eq!(StatusCode::UNAUTHORIZED, put.status());

        let metadata = probe(router.clone(), "/_metadata", Method::POST).await;
        assert_eq!(StatusCode::UNAUTHORIZED, metadata.status());

        let sniff = probe(router.clone(), "/_sniff", Method::POST).await;
        assert_eq!(StatusCode::UNAUTHORIZED, sniff.status());

        let read = probe(router, "/attachments/1/2/stream.js", Method::GET).await;
        assert_eq!(StatusCode::NOT_FOUND, read.status());
        assert_eq!(
            None,
            header_text(&read, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );
    }

    #[tokio::test]
    async fn internal_routes_and_the_relay_ignore_the_cors_policy() {
        let off = test_router_for(test_config("upload", &[]));
        let enforce = test_router_for(test_config("upload", ENFORCE));
        let request = |method: Method, path: &str| {
            Request::builder()
                .method(method)
                .uri(path)
                .header(header::ORIGIN, "https://evil.example")
                .extension(ConnectInfo(SocketAddr::from(([127, 0, 0, 1], 40000))))
                .body(Body::empty())
                .expect("cors probe request")
        };
        for (method, path) in [
            (Method::GET, "/_health"),
            (Method::POST, "/_metadata"),
            (Method::POST, "/_sniff"),
            (Method::OPTIONS, "/v1/relay/uploads/file.png"),
            (Method::PUT, "/v1/relay/uploads/file.png"),
            (Method::GET, "/_metrics"),
        ] {
            let label = format!("{method} {path}");
            let expected = send(off.clone(), request(method.clone(), path)).await;
            let actual = send(enforce.clone(), request(method, path)).await;
            assert_eq!(expected.status(), actual.status(), "{label}");
            assert_eq!(expected.headers(), actual.headers(), "{label}");
            for vary in actual.headers().get_all(header::VARY) {
                assert!(
                    !vary.to_str().expect("ascii vary").contains("Origin"),
                    "{label}"
                );
            }
        }
    }

    #[tokio::test]
    async fn a_refused_read_through_the_router_has_security_and_version_headers() {
        let state = Arc::new(AppState::for_tests(test_config("mp", ENFORCE)));
        let router = build_router(Arc::clone(&state), HttpRequestDrain::new());
        let response = send(
            router,
            Request::builder()
                .method(Method::GET)
                .uri("/avatars/1/hash.png")
                .header(header::ORIGIN, "null".to_owned())
                .body(Body::empty())
                .expect("refused read request"),
        )
        .await;
        assert_eq!(StatusCode::FORBIDDEN, response.status());
        assert!(response.headers().contains_key("x-fluxer-version"));
        assert_eq!(
            Some(http_headers::STRICT_TRANSPORT_SECURITY),
            header_text(&response, header::STRICT_TRANSPORT_SECURITY)
        );
        assert!(
            state
                .metrics
                .render()
                .contains("fluxer_media_proxy_requests_4xx_total{kind=\"asset_image\"} 1\n")
        );
    }
}
