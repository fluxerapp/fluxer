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
    config::{Config, DeploymentMode, PolicyMode},
    media_process, mime, request_log,
};
use axum::{
    Router,
    http::{Extensions, HeaderMap, StatusCode, Version, header},
    middleware,
    routing::{any, get, post, put},
};
use std::{net::SocketAddr, sync::Arc, time::Duration};
use tokio::{net::TcpListener, time::timeout_at};
use tower_http::compression::{CompressionLayer, CompressionLevel};
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

const STATIC_COMPRESSIBLE_CONTENT_TYPES: [&str; 9] = [
    "application/javascript",
    "text/javascript",
    "text/css",
    "text/html",
    "application/json",
    "application/manifest+json",
    "application/xml",
    "application/wasm",
    "image/svg+xml",
];

const STATIC_COMPRESSION_MIN_BYTES: u64 = 1024;

fn is_compressible_static_response(
    status: StatusCode,
    _version: Version,
    headers: &HeaderMap,
    _extensions: &Extensions,
) -> bool {
    let content_type = headers
        .get(header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok());
    status == StatusCode::OK
        && headers
            .get(header::CONTENT_LENGTH)
            .and_then(|value| value.to_str().ok())
            .and_then(|value| value.parse::<u64>().ok())
            .is_some_and(|length| length >= STATIC_COMPRESSION_MIN_BYTES)
        && mime::normalize(content_type).is_some_and(|content_type| {
            STATIC_COMPRESSIBLE_CONTENT_TYPES
                .iter()
                .any(|compressible| compressible.eq_ignore_ascii_case(content_type))
        })
}

fn build_router(state: Arc<AppState>, drain: HttpRequestDrain) -> Router {
    let router = Router::new()
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
        .fallback(any(routes::dispatch::catch_all));
    let router = if state.cfg.mode == DeploymentMode::Static {
        router.layer(
            CompressionLayer::new()
                .no_br()
                .no_deflate()
                .no_zstd()
                .quality(CompressionLevel::Precise(5))
                .compress_when(is_compressible_static_response),
        )
    } else {
        router
    };
    router
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
    use crate::{
        http_headers,
        storage::tests::{FakeObject, FakeS3, fake_s3},
    };
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

    async fn static_router_serving(objects: &[(&str, &str, &[u8])]) -> (FakeS3, Router) {
        let fake = fake_s3().await;
        let tmp = tempfile::tempdir().expect("config root");
        let mut cfg = fake.config(tmp.path());
        cfg.mode = DeploymentMode::Static;
        for (key, content_type, body) in objects {
            fake.put_object(
                &format!("{}/{key}", cfg.storage.bucket_static),
                FakeObject {
                    body: body.to_vec(),
                    content_type: Some((*content_type).to_owned()),
                    ..FakeObject::default()
                },
            );
        }
        (fake, test_router_for(cfg))
    }

    async fn fetch(
        router: Router,
        path: &str,
        accept_encoding: Option<&str>,
        range: Option<&str>,
    ) -> (Response, Vec<u8>) {
        let mut builder = Request::builder().uri(path);
        if let Some(value) = accept_encoding {
            builder = builder.header(header::ACCEPT_ENCODING, value);
        }
        if let Some(value) = range {
            builder = builder.header(header::RANGE, value);
        }
        let response = send(router, builder.body(Body::empty()).expect("request")).await;
        let (parts, body) = response.into_parts();
        let bytes = axum::body::to_bytes(body, usize::MAX)
            .await
            .expect("response body")
            .to_vec();
        (Response::from_parts(parts, Body::empty()), bytes)
    }

    fn decoded(encoding: Option<&str>, body: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        match encoding {
            Some("gzip") => {
                std::io::Read::read_to_end(&mut flate2::read::GzDecoder::new(body), &mut out)
                    .expect("gzip body");
            }
            None => out.extend_from_slice(body),
            Some(other) => panic!("unexpected content-encoding {other}"),
        }
        out
    }

    fn compressible_bytes(len: usize) -> Vec<u8> {
        (0..len).map(|index| b"asm_module_"[index % 11]).collect()
    }

    #[tokio::test]
    async fn static_mode_gzips_scripts_styles_and_wasm_for_clients_that_accept_it() {
        let body = compressible_bytes(64 * 1024);
        let objects = [
            ("assets/0123456789abcdef.wasm", "application/wasm"),
            (
                "assets/0123456789abcdef.js",
                "application/javascript; charset=utf-8",
            ),
            ("assets/0123456789abcdef.css", "text/css; charset=utf-8"),
            (
                "assets/0123456789abcdef.js.map",
                "application/json; charset=utf-8",
            ),
            ("emoji/1f600.svg", "image/svg+xml"),
        ];
        let stored: Vec<(&str, &str, &[u8])> = objects
            .iter()
            .map(|(key, content_type)| (*key, *content_type, &body[..]))
            .collect();
        let (_fake, router) = static_router_serving(&stored).await;
        for (key, _) in objects {
            let path = format!("/{key}");
            for (accept, expected) in [
                ("br, gzip, zstd", Some("gzip")),
                ("gzip, deflate", Some("gzip")),
                ("gzip;q=0.5, br;q=1", Some("gzip")),
                ("br", None),
                ("deflate", None),
                ("identity", None),
                ("zstd", None),
            ] {
                let (response, bytes) = fetch(router.clone(), &path, Some(accept), None).await;
                assert_eq!(StatusCode::OK, response.status(), "{key} {accept}");
                let encoding = header_text(&response, header::CONTENT_ENCODING);
                assert_eq!(expected, encoding, "{key} {accept}");
                assert_eq!(body, decoded(encoding, &bytes), "{key} {accept}");
                assert_eq!(
                    vec!["Accept-Encoding"],
                    response
                        .headers()
                        .get_all(header::VARY)
                        .iter()
                        .map(|value| value.to_str().expect("vary is ascii"))
                        .collect::<Vec<_>>(),
                    "{key} {accept}"
                );
                if expected.is_some() {
                    assert!(bytes.len() < body.len() / 4, "{key} {accept}");
                    assert_eq!(None, header_text(&response, header::CONTENT_LENGTH));
                    assert_eq!(None, header_text(&response, header::ACCEPT_RANGES));
                } else {
                    assert_eq!(
                        Some(body.len().to_string().as_str()),
                        header_text(&response, header::CONTENT_LENGTH),
                        "{key} {accept}"
                    );
                }
            }
            let (response, bytes) = fetch(router.clone(), &path, None, None).await;
            assert_eq!(
                None,
                header_text(&response, header::CONTENT_ENCODING),
                "{key}"
            );
            assert_eq!(body, bytes, "{key}");
        }
    }

    #[tokio::test]
    async fn static_mode_serves_archives_images_ranges_and_small_files_as_stored() {
        let body = compressible_bytes(64 * 1024);
        let small = compressible_bytes(512);
        let (_fake, router) = static_router_serving(&[
            ("assets/0123456789abcdef.tar.gz", "application/gzip", &body),
            ("avatars/0.png", "image/png", &body),
            (
                "desktop/fluxer-setup.exe",
                "application/octet-stream",
                &body,
            ),
            (
                "assets/fedcba9876543210.js",
                "application/javascript; charset=utf-8",
                &small,
            ),
            ("assets/0123456789abcdef.wasm", "application/wasm", &body),
        ])
        .await;
        for (path, stored) in [
            ("/assets/0123456789abcdef.tar.gz", &body),
            ("/avatars/0.png", &body),
            ("/desktop/fluxer-setup.exe", &body),
            ("/assets/fedcba9876543210.js", &small),
        ] {
            let (response, bytes) = fetch(router.clone(), path, Some("br, gzip"), None).await;
            assert_eq!(StatusCode::OK, response.status(), "{path}");
            assert_eq!(
                None,
                header_text(&response, header::CONTENT_ENCODING),
                "{path}"
            );
            assert_eq!(stored, &bytes, "{path}");
        }
        let (response, bytes) = fetch(
            router,
            "/assets/0123456789abcdef.wasm",
            Some("br, gzip"),
            Some("bytes=100-199"),
        )
        .await;
        assert_eq!(StatusCode::PARTIAL_CONTENT, response.status());
        assert_eq!(None, header_text(&response, header::CONTENT_ENCODING));
        assert_eq!(&body[100..200], &bytes[..]);
    }

    #[tokio::test]
    async fn static_head_matches_get_representation_headers_without_a_body() {
        let body = compressible_bytes(1024);
        let small = compressible_bytes(1023);
        let (_fake, router) = static_router_serving(&[
            ("assets/0123456789abcdef.wasm", "application/wasm", &body),
            (
                "assets/fedcba9876543210.js",
                "application/javascript",
                &small,
            ),
            ("assets/0123456789abcdef.tar.gz", "application/gzip", &body),
        ])
        .await;
        let listener = TcpListener::bind("127.0.0.1:0")
            .await
            .expect("local listener");
        let address = listener.local_addr().expect("local address");
        let server = tokio::spawn(async move {
            axum::serve(
                listener,
                router.into_make_service_with_connect_info::<SocketAddr>(),
            )
            .await
            .expect("local server");
        });
        let client = reqwest::Client::builder()
            .no_proxy()
            .no_gzip()
            .no_brotli()
            .no_deflate()
            .no_zstd()
            .build()
            .expect("http client");
        for (path, accept_encoding, range, expected_encoding, expected_body) in [
            (
                "/assets/0123456789abcdef.wasm",
                "gzip",
                None,
                Some("gzip"),
                &body[..],
            ),
            (
                "/assets/0123456789abcdef.wasm",
                "identity",
                None,
                None,
                &body[..],
            ),
            (
                "/assets/fedcba9876543210.js",
                "gzip",
                None,
                None,
                &small[..],
            ),
            (
                "/assets/0123456789abcdef.tar.gz",
                "gzip",
                None,
                None,
                &body[..],
            ),
            (
                "/assets/0123456789abcdef.wasm",
                "gzip",
                Some("bytes=100-199"),
                None,
                &body[100..200],
            ),
        ] {
            let url = format!("http://{address}{path}");
            let mut get = client
                .get(&url)
                .header(header::ACCEPT_ENCODING, accept_encoding);
            let mut head = client
                .head(&url)
                .header(header::ACCEPT_ENCODING, accept_encoding);
            if let Some(range) = range {
                get = get.header(header::RANGE, range);
                head = head.header(header::RANGE, range);
            }
            let get = get.send().await.expect("get response");
            let status = get.status();
            let get_headers = get.headers().clone();
            let get_body = get.bytes().await.expect("get body");
            let encoding = get_headers
                .get(header::CONTENT_ENCODING)
                .map(|value| value.to_str().expect("encoding is ascii"));
            assert_eq!(expected_encoding, encoding, "{path} {accept_encoding}");
            assert_eq!(expected_body, decoded(encoding, &get_body));
            let head = head.send().await.expect("head response");
            assert_eq!(status, head.status(), "{path} {accept_encoding}");
            for name in [
                header::CONTENT_ENCODING,
                header::CONTENT_LENGTH,
                header::CONTENT_TYPE,
                header::CONTENT_RANGE,
                header::ACCEPT_RANGES,
                header::VARY,
            ] {
                assert_eq!(
                    get_headers.get(&name),
                    head.headers().get(&name),
                    "{path} {accept_encoding} {name}"
                );
            }
            assert!(head.bytes().await.expect("head body").is_empty());
        }
        server.abort();
        let _ = server.await;
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
