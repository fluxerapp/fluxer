// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::{DeploymentMode, PolicyMode},
    constants::AssetKind,
    request_log::RequestId,
    server::{
        asset_path::{
            StorageKeyDecodeError, decode_storage_key, parse_entrance_sound_path,
            parse_guild_member_asset_path, parse_simple_asset_path, parse_standard_asset_path,
        },
        attachment_signature,
        cors::{self, OriginCheck},
        external::serve_external,
        response::error::{text, text_with_source},
        state::AppState,
        stored,
    },
};
use axum::{
    body::Body,
    extract::{Query, State},
    http::{HeaderMap, Method, Request, StatusCode},
    response::Response,
};
use fluxer_common::attachment_url_signature::{SIGNATURE_PARAMETER_NAMES, Verdict};
use std::{collections::HashMap, sync::Arc};

struct PublicRead<'a> {
    method: Method,
    path: &'a str,
    query: Option<&'a str>,
    params: &'a HashMap<String, String>,
    headers: &'a HeaderMap,
    request_id: Option<&'a RequestId>,
}

pub(in crate::server) async fn catch_all(
    State(app): State<Arc<AppState>>,
    Query(params): Query<HashMap<String, String>>,
    request: Request<Body>,
) -> Response {
    let method = request.method().clone();
    if method != Method::GET && method != Method::HEAD {
        return text(StatusCode::METHOD_NOT_ALLOWED, "Method Not Allowed");
    }
    if app.cfg.mode == DeploymentMode::Relay {
        return text(StatusCode::NOT_FOUND, "Not Found");
    }
    let path = request.uri().path().to_owned();
    if app.cfg.mode == DeploymentMode::Static {
        let key = match decode_storage_key(&path) {
            Ok(key) => key,
            Err(err) => return storage_key_decode_response(err),
        };
        return stored::serve_stored_raw(
            &app,
            method,
            &app.cfg.storage.bucket_static,
            &key,
            request.headers(),
        )
        .await;
    }
    let query = request.uri().query().map(ToOwned::to_owned);
    let request_id = request.extensions().get::<RequestId>().cloned();
    let read = PublicRead {
        method: method.clone(),
        path: &path,
        query: query.as_deref(),
        params: &params,
        headers: request.headers(),
        request_id: request_id.as_ref(),
    };
    if app.cfg.cors.mode == PolicyMode::Off {
        return serve_public_read(&app, read).await;
    }
    let check = cors::check_origin(&app.cfg.cors.allowed_origins, request.headers());
    if check == OriginCheck::Refused {
        if app.cfg.cors.mode == PolicyMode::Enforce {
            return cors::refused_response(request.headers());
        }
        cors::log_would_refuse(&method, &path, request.headers());
    }
    let mut response = serve_public_read(&app, read).await;
    if app.cfg.cors.mode == PolicyMode::Enforce {
        cors::narrow_response(&check, response.headers_mut());
    }
    response
}

async fn serve_public_read(app: &Arc<AppState>, read: PublicRead<'_>) -> Response {
    let PublicRead {
        method,
        path,
        query,
        params,
        headers,
        request_id,
    } = read;
    if let Some(rest) = path.strip_prefix("/external/") {
        return serve_external(app, method, rest, params, headers).await;
    }
    if path.starts_with("/attachments/") {
        let key = match decode_storage_key(path) {
            Ok(key) => key,
            Err(err) => return storage_key_decode_response(err),
        };
        let signature_mode = app.cfg.attachment_signature.mode;
        let mut checked = None;
        if signature_mode != PolicyMode::Off {
            let verification =
                attachment_signature::check(&app.cfg.attachment_signature, path, query);
            app.metrics
                .attachment_signature()
                .record(verification.verdict);
            if verification.verdict != Verdict::Valid {
                if signature_mode == PolicyMode::Enforce {
                    return attachment_signature::denied_response(verification.verdict);
                }
                attachment_signature::log_would_deny(
                    &app.signature_log,
                    &method,
                    verification.verdict,
                    headers,
                    request_id,
                );
            }
            checked = Some(verification);
        }
        let params = without_signature_parameters(params);
        let mut response = stored::serve_attachment(app, method, &key, &params, headers).await;
        if let Some(verification) = checked {
            attachment_signature::limit_cache_lifetime(&mut response, verification);
        }
        return response;
    }
    if path.starts_with("/themes/") && path.ends_with(".css") {
        let key = match decode_storage_key(path) {
            Ok(key) => key,
            Err(err) => return storage_key_decode_response(err),
        };
        return stored::serve_stored_with_override(
            app,
            method,
            &app.cfg.storage.bucket_cdn,
            &key,
            "text/css; charset=utf-8",
            headers,
        )
        .await;
    }
    if let Some(key) = parse_entrance_sound_path(path) {
        return stored::serve_stored_raw(app, method, &app.cfg.storage.bucket_cdn, &key, headers)
            .await;
    }
    if let Some(asset) = parse_guild_member_asset_path(path) {
        return stored::serve_asset_image(app, method, asset, params, headers).await;
    }
    if let Some(asset) = parse_simple_asset_path(path, AssetKind::Emoji) {
        return stored::serve_asset_image(app, method, asset, params, headers).await;
    }
    if let Some(asset) = parse_simple_asset_path(path, AssetKind::Sticker) {
        return stored::serve_asset_image(app, method, asset, params, headers).await;
    }
    if let Some(asset) = parse_standard_asset_path(path) {
        return stored::serve_asset_image(app, method, asset, params, headers).await;
    }
    text(StatusCode::NOT_FOUND, "Not Found")
}

fn storage_key_decode_response(err: StorageKeyDecodeError) -> Response {
    text_with_source(
        StatusCode::BAD_REQUEST,
        "Bad Request",
        "invalid_storage_key",
        err,
    )
}

fn without_signature_parameters(params: &HashMap<String, String>) -> HashMap<String, String> {
    params
        .iter()
        .filter(|(name, _)| !SIGNATURE_PARAMETER_NAMES.contains(&name.as_str()))
        .map(|(name, value)| (name.clone(), value.clone()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use axum::{
        body::to_bytes,
        http::{HeaderValue, header},
    };
    use base64::{Engine as _, engine::general_purpose};
    use bytes::Bytes;
    use fluxer_common::attachment_url_signature::{
        ATTACHMENT_URL_TTL_SECS, UrlKind, issue_window, sign, with_data_package_signature,
        with_signature,
    };
    use std::{
        path::Path,
        sync::Mutex,
        time::{SystemTime, UNIX_EPOCH},
    };
    use tracing_subscriber::fmt::MakeWriter;

    #[derive(Clone, Default)]
    struct CapturedLog(Arc<Mutex<Vec<u8>>>);

    impl CapturedLog {
        fn text(&self) -> String {
            String::from_utf8(self.0.lock().expect("captured log is not poisoned").clone())
                .expect("captured log is utf-8")
        }
    }

    impl std::io::Write for CapturedLog {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0
                .lock()
                .expect("captured log is not poisoned")
                .extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    impl<'a> MakeWriter<'a> for CapturedLog {
        type Writer = Self;

        fn make_writer(&'a self) -> Self::Writer {
            self.clone()
        }
    }

    const STORED_BYTES: &[u8] = b"raw-stored-bytes";
    const THEME_BYTES: &[u8] = b"body{color:#fff}";
    const WEB: &str = "https://web.fluxer.app";
    const EVIL: &str = "https://evil.example";
    const OPAQUE_ORIGIN: &[u8] = &[0xC3, 0x28];
    const ORIGIN_VARY: &str = "Accept-Encoding, Origin";
    const ATTACHMENT_PATH: &str = "/attachments/1/2/file.bin";
    const THEME_PATH: &str = "/themes/dark.css";
    const ENTRANCE_SOUND_PATH: &str = "/entrance-sounds/42/abc123def456.wav";
    const ASSET_PATH: &str = "/avatars/123456789012345678/a1b2c3d4e5f6.png";
    const BAD_SIGNATURE_PATH: &str = "/external/badsig/https/example.test/a.png";
    const ENFORCE: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
        ("FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS", WEB),
    ];
    const REPORT: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "report"),
        ("FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS", WEB),
    ];

    fn dispatch_config(mode: &str, storage_root: &Path, extra: &[(&str, &str)]) -> Config {
        let mut env = vec![
            (
                "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                "secret".to_owned(),
            ),
            ("FLUXER_MEDIA_PROXY_MODE".to_owned(), mode.to_owned()),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
                "local".to_owned(),
            ),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
                storage_root.display().to_string(),
            ),
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
        Config::load_from_iter(env).expect("dispatch test config")
    }

    fn dispatch_app(mode: &str, storage_root: &Path, extra: &[(&str, &str)]) -> Arc<AppState> {
        Arc::new(AppState::for_tests(dispatch_config(
            mode,
            storage_root,
            extra,
        )))
    }

    fn write_object(storage_root: &Path, bucket: &str, key: &str, body: &[u8]) {
        let path = storage_root.join(bucket).join(key);
        std::fs::create_dir_all(path.parent().expect("object parent")).expect("create bucket dir");
        std::fs::write(path, body).expect("write object");
    }

    fn write_stored_representations(storage_root: &Path) {
        write_object(
            storage_root,
            "cdn",
            "attachments/1/2/file.bin",
            STORED_BYTES,
        );
        write_object(storage_root, "cdn", "themes/dark.css", THEME_BYTES);
    }

    fn read_request(
        method: Method,
        path: &str,
        origins: &[&[u8]],
        range: Option<&str>,
    ) -> Request<Body> {
        let mut builder = Request::builder().method(method).uri(path);
        for origin in origins {
            builder = builder.header(
                header::ORIGIN,
                HeaderValue::from_bytes(origin).expect("origin header value"),
            );
        }
        if let Some(range) = range {
            builder = builder.header(header::RANGE, range);
        }
        builder.body(Body::empty()).expect("dispatch request")
    }

    async fn dispatch_request(app: &Arc<AppState>, request: Request<Body>) -> Response {
        catch_all(State(Arc::clone(app)), Query(HashMap::new()), request).await
    }

    async fn dispatch(app: &Arc<AppState>, method: Method, path: &str) -> Response {
        dispatch_request(app, read_request(method, path, &[], None)).await
    }

    async fn dispatch_body(app: &Arc<AppState>, path: &str) -> Bytes {
        let response = dispatch(app, Method::GET, path).await;
        assert_eq!(StatusCode::OK, response.status(), "{path} must be served");
        to_bytes(response.into_body(), usize::MAX)
            .await
            .expect("dispatch body")
    }

    async fn body_of(response: Response) -> Bytes {
        to_bytes(response.into_body(), usize::MAX)
            .await
            .expect("response body")
    }

    fn vary_values(response: &Response) -> Vec<String> {
        response
            .headers()
            .get_all(header::VARY)
            .iter()
            .map(|value| value.to_str().expect("vary is ascii").to_owned())
            .collect()
    }

    fn refused_origin_shapes() -> [(&'static str, Vec<&'static [u8]>); 4] {
        [
            ("foreign", vec![EVIL.as_bytes()]),
            ("null", vec![b"null".as_slice()]),
            ("duplicated", vec![WEB.as_bytes(), WEB.as_bytes()]),
            ("malformed", vec![OPAQUE_ORIGIN]),
        ]
    }

    fn stored_representation_cases() -> [(Method, &'static str, Option<&'static str>, StatusCode); 7]
    {
        [
            (Method::GET, ATTACHMENT_PATH, None, StatusCode::OK),
            (
                Method::GET,
                ATTACHMENT_PATH,
                Some("bytes=0-3"),
                StatusCode::PARTIAL_CONTENT,
            ),
            (
                Method::GET,
                ATTACHMENT_PATH,
                Some("bytes=100-200"),
                StatusCode::RANGE_NOT_SATISFIABLE,
            ),
            (Method::HEAD, ATTACHMENT_PATH, None, StatusCode::OK),
            (
                Method::HEAD,
                ATTACHMENT_PATH,
                Some("bytes=0-3"),
                StatusCode::PARTIAL_CONTENT,
            ),
            (Method::GET, THEME_PATH, None, StatusCode::OK),
            (
                Method::GET,
                THEME_PATH,
                Some("bytes=999-"),
                StatusCode::RANGE_NOT_SATISFIABLE,
            ),
        ]
    }

    #[tokio::test]
    async fn static_mode_short_circuits_every_path_to_the_static_bucket() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "static", "external/v2/signed.png", STORED_BYTES);
        write_object(
            root,
            "static",
            "avatars/1216100949629702144/a1b2c3d4e5f6.png",
            STORED_BYTES,
        );
        write_object(root, "cdn", "themes/only-in-cdn.css", STORED_BYTES);
        let app = dispatch_app("static", root, &[]);

        assert_eq!(
            STORED_BYTES,
            dispatch_body(&app, "/external/v2/signed.png").await
        );
        assert_eq!(
            STORED_BYTES,
            dispatch_body(&app, "/avatars/1216100949629702144/a1b2c3d4e5f6.png").await
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&app, Method::GET, "/themes/only-in-cdn.css")
                .await
                .status(),
            "static mode never reaches the cdn bucket"
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&app, Method::GET, "/attachments/1/2/file.bin")
                .await
                .status()
        );
    }

    const STATIC_CACHE_CONTROL: &str = "public, max-age=31536000";

    const STATIC_KEYS: [&str; 4] = [
        "avatars/0.png",
        "web/favicon.ico",
        "emoji/1f600.svg",
        "web/NOTICE.md",
    ];

    fn static_mode_app(root: &Path, extra: &[(&str, &str)]) -> Arc<AppState> {
        for key in STATIC_KEYS {
            write_object(root, "static", key, STORED_BYTES);
        }
        dispatch_app("static", root, extra)
    }

    fn header_value(response: &Response, name: impl header::AsHeaderName) -> Option<String> {
        response
            .headers()
            .get(name)
            .map(|value| value.to_str().expect("header is ascii").to_owned())
    }

    #[tokio::test]
    async fn static_mode_caches_every_asset_forever() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = static_mode_app(root.as_path(), &[]);
        for key in STATIC_KEYS {
            let response = dispatch(&app, Method::GET, &format!("/{key}")).await;
            assert_eq!(StatusCode::OK, response.status(), "key={key}");
            assert_eq!(
                Some(STATIC_CACHE_CONTROL.to_owned()),
                header_value(&response, header::CACHE_CONTROL),
                "key={key}"
            );
        }
    }

    #[tokio::test]
    async fn static_mode_omits_expires_and_relies_on_cache_control() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = static_mode_app(root.as_path(), &[]);
        let response = dispatch(&app, Method::GET, "/avatars/0.png").await;
        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(None, header_value(&response, header::EXPIRES));
        assert_eq!(
            Some(STATIC_CACHE_CONTROL.to_owned()),
            header_value(&response, "CDN-Cache-Control")
        );
    }

    #[tokio::test]
    async fn media_mode_matches_the_v1_route_order() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "cdn", "themes/dark.css", THEME_BYTES);
        write_object(
            root,
            "cdn",
            "entrance-sounds/42/abc123def456.wav",
            STORED_BYTES,
        );
        write_object(root, "static", "themes/dark.css", STORED_BYTES);
        let app = dispatch_app("mp", root, &[]);

        let response = dispatch(&app, Method::GET, "/themes/dark.css").await;
        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            "text/css; charset=utf-8",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .expect("content type")
                .to_str()
                .expect("ascii content type")
        );
        assert_eq!(
            THEME_BYTES,
            to_bytes(response.into_body(), usize::MAX)
                .await
                .expect("theme body")
        );

        assert_eq!(
            STORED_BYTES,
            dispatch_body(&app, "/entrance-sounds/42/abc123def456.wav").await
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&app, Method::GET, "/themes/dark.png")
                .await
                .status(),
            "the themes branch only matches .css"
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&app, Method::GET, "/nothing/here").await.status()
        );
        assert_eq!(
            StatusCode::METHOD_NOT_ALLOWED,
            dispatch(&app, Method::POST, "/themes/dark.css")
                .await
                .status()
        );
    }

    #[tokio::test]
    async fn relay_mode_answers_every_read_path_with_404() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "cdn", "attachments/1/2/stream.js", STORED_BYTES);
        write_object(root, "cdn", "themes/dark.css", THEME_BYTES);
        write_object(
            root,
            "cdn",
            "entrance-sounds/42/abc123def456.wav",
            STORED_BYTES,
        );
        let app = dispatch_app("relay", root, &[]);
        for path in [
            "/attachments/1/2/stream.js",
            "/external/sig/https/example.com/a.png",
            THEME_PATH,
            ENTRANCE_SOUND_PATH,
            ASSET_PATH,
            "/emojis/1.png",
            "/stickers/1.webp",
            "/",
            "/nothing/here",
        ] {
            for method in [Method::GET, Method::HEAD] {
                let response = dispatch(&app, method.clone(), path).await;
                assert_eq!(StatusCode::NOT_FOUND, response.status(), "{method} {path}");
                assert_eq!(
                    Some("text/plain; charset=utf-8".to_owned()),
                    header_value(&response, header::CONTENT_TYPE),
                    "{method} {path}"
                );
                assert_eq!(
                    Some("no-store".to_owned()),
                    header_value(&response, header::CACHE_CONTROL),
                    "{method} {path}"
                );
                assert_eq!(
                    None,
                    header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                    "{method} {path}"
                );
            }
        }
        assert!(
            app.metrics
                .render()
                .contains("fluxer_media_proxy_storage_hits_total 0\n")
        );
    }

    #[tokio::test]
    async fn upload_mode_serves_what_relay_mode_refuses() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "cdn", "attachments/1/2/stream.js", STORED_BYTES);
        let upload = dispatch_app("upload", root, &[]);
        let relay = dispatch_app("relay", root, &[]);

        assert_eq!(
            STORED_BYTES,
            dispatch_body(&upload, "/attachments/1/2/stream.js").await
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&relay, Method::GET, "/attachments/1/2/stream.js")
                .await
                .status()
        );
    }

    #[tokio::test]
    async fn relay_mode_rejects_a_bad_storage_key_as_404_not_400() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        let upload = dispatch_app("upload", root, &[]);
        let relay = dispatch_app("relay", root, &[]);

        assert_eq!(
            StatusCode::BAD_REQUEST,
            dispatch(&upload, Method::GET, "/attachments/%2e%2e/x")
                .await
                .status()
        );
        assert_eq!(
            StatusCode::NOT_FOUND,
            dispatch(&relay, Method::GET, "/attachments/%2e%2e/x")
                .await
                .status()
        );
    }

    #[tokio::test]
    async fn relay_mode_keeps_405_for_other_methods() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = dispatch_app("relay", root.as_path(), &[]);
        for path in ["/attachments/1/2/stream.js", "/nothing/here"] {
            for method in [
                Method::POST,
                Method::PUT,
                Method::DELETE,
                Method::PATCH,
                Method::OPTIONS,
            ] {
                assert_eq!(
                    StatusCode::METHOD_NOT_ALLOWED,
                    dispatch(&app, method.clone(), path).await.status(),
                    "{method} {path}"
                );
            }
        }
    }

    #[tokio::test]
    async fn enforce_echoes_an_allowed_origin_on_every_stored_representation() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let app = dispatch_app("mp", root, ENFORCE);
        for (method, path, range, status) in stored_representation_cases() {
            let label = format!("{method} {path} range={range:?}");
            let response =
                dispatch_request(&app, read_request(method, path, &[WEB.as_bytes()], range)).await;
            assert_eq!(status, response.status(), "{label}");
            assert_eq!(
                Some(WEB.to_owned()),
                header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                "{label}"
            );
            assert_eq!(
                vec![ORIGIN_VARY.to_owned()],
                vary_values(&response),
                "{label}"
            );
        }
    }

    #[tokio::test]
    async fn enforce_serves_origin_less_reads_without_allow_origin() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let enforce = dispatch_app("mp", root, ENFORCE);
        for (method, path, range, status) in stored_representation_cases() {
            let label = format!("{method} {path} range={range:?}");
            let expected =
                dispatch_request(&off, read_request(method.clone(), path, &[], range)).await;
            let actual = dispatch_request(&enforce, read_request(method, path, &[], range)).await;
            assert_eq!(status, expected.status(), "{label}");
            assert_eq!(status, actual.status(), "{label}");
            assert_eq!(
                None,
                header_value(&actual, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                "{label}"
            );
            assert_eq!(
                vec![ORIGIN_VARY.to_owned()],
                vary_values(&actual),
                "{label}"
            );
            assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
        }
    }

    #[tokio::test]
    async fn enforce_refuses_foreign_null_and_malformed_origins_before_any_read() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        write_object(
            root,
            "cdn",
            "entrance-sounds/42/abc123def456.wav",
            STORED_BYTES,
        );
        let asset = parse_standard_asset_path(ASSET_PATH).expect("asset path");
        write_object(
            root,
            "cdn",
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(64, 64),
        );
        let app = dispatch_app("mp", root, ENFORCE);
        for (shape, origins) in refused_origin_shapes() {
            for path in [
                ATTACHMENT_PATH,
                THEME_PATH,
                ENTRANCE_SOUND_PATH,
                ASSET_PATH,
                BAD_SIGNATURE_PATH,
                "/nothing/here",
            ] {
                for method in [Method::GET, Method::HEAD] {
                    let label = format!("{shape} {method} {path}");
                    let response =
                        dispatch_request(&app, read_request(method, path, &origins, None)).await;
                    assert_eq!(StatusCode::FORBIDDEN, response.status(), "{label}");
                    assert_eq!(
                        None,
                        header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                        "{label}"
                    );
                }
            }
        }
        assert!(
            app.metrics
                .render()
                .contains("fluxer_media_proxy_storage_hits_total 0\n")
        );
    }

    #[tokio::test]
    async fn enforce_leaves_read_errors_without_cors_but_varies_them_on_origin() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = dispatch_app("mp", root.as_path(), ENFORCE);
        let with_origin: &[&[u8]] = &[WEB.as_bytes()];
        let without_origin: &[&[u8]] = &[];
        for origins in [with_origin, without_origin] {
            for (path, status) in [
                ("/attachments/1/2/missing.bin", StatusCode::NOT_FOUND),
                ("/attachments/%2e%2e/x", StatusCode::BAD_REQUEST),
                ("/external/nosignature", StatusCode::BAD_REQUEST),
                (BAD_SIGNATURE_PATH, StatusCode::UNAUTHORIZED),
            ] {
                for method in [Method::GET, Method::HEAD] {
                    let label = format!("{method} {path} origins={}", origins.len());
                    let response =
                        dispatch_request(&app, read_request(method, path, origins, None)).await;
                    assert_eq!(status, response.status(), "{label}");
                    assert_eq!(
                        None,
                        header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                        "{label}"
                    );
                    assert_eq!(
                        vec![ORIGIN_VARY.to_owned()],
                        vary_values(&response),
                        "{label}"
                    );
                }
            }
        }
    }

    #[tokio::test]
    async fn enforce_refuses_a_foreign_origin_on_a_transform_cache_hit() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = dispatch_app("mp", root.as_path(), ENFORCE);
        let asset = parse_standard_asset_path(ASSET_PATH).expect("asset path");
        app.store
            .write_object(
                &app.cfg.storage.bucket_cdn,
                &asset.storage_key,
                &crate::test_fixtures::synthetic_png(512, 512),
                "image/png",
            )
            .await
            .expect("write asset");
        for _ in 0..2 {
            let response = dispatch_request(
                &app,
                read_request(Method::GET, ASSET_PATH, &[WEB.as_bytes()], None),
            )
            .await;
            assert_eq!(StatusCode::OK, response.status());
            assert_eq!(
                Some(WEB.to_owned()),
                header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN)
            );
            assert_eq!(vec![ORIGIN_VARY.to_owned()], vary_values(&response));
            assert!(!body_of(response).await.is_empty());
        }
        assert!(
            app.metrics
                .render()
                .contains("fluxer_media_proxy_transform_cache_hits_total 1\n")
        );
        let response = dispatch_request(
            &app,
            read_request(Method::GET, ASSET_PATH, &[EVIL.as_bytes()], None),
        )
        .await;
        assert_eq!(StatusCode::FORBIDDEN, response.status());
        assert!(
            app.metrics
                .render()
                .contains("fluxer_media_proxy_transform_cache_hits_total 1\n"),
            "a refused origin never reaches the transform cache"
        );
    }

    #[tokio::test]
    async fn enforce_applies_to_upload_mode_reads() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let app = dispatch_app("upload", root, ENFORCE);

        let allowed = dispatch_request(
            &app,
            read_request(Method::GET, ATTACHMENT_PATH, &[WEB.as_bytes()], None),
        )
        .await;
        assert_eq!(StatusCode::OK, allowed.status());
        assert_eq!(
            Some(WEB.to_owned()),
            header_value(&allowed, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );

        let foreign = dispatch_request(
            &app,
            read_request(Method::GET, ATTACHMENT_PATH, &[EVIL.as_bytes()], None),
        )
        .await;
        assert_eq!(StatusCode::FORBIDDEN, foreign.status());

        let origin_less = dispatch(&app, Method::GET, ATTACHMENT_PATH).await;
        assert_eq!(StatusCode::OK, origin_less.status());
        assert_eq!(
            None,
            header_value(&origin_less, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );
        assert_eq!(STORED_BYTES, body_of(origin_less).await);
    }

    #[tokio::test]
    async fn report_mode_changes_no_response_byte() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let report = dispatch_app("mp", root, REPORT);
        let origin_shapes: [&[&[u8]]; 4] = [
            &[EVIL.as_bytes()],
            &[b"null".as_slice()],
            &[WEB.as_bytes()],
            &[],
        ];
        for origins in origin_shapes {
            for (method, path, range, status) in [
                (Method::GET, ATTACHMENT_PATH, None, StatusCode::OK),
                (
                    Method::GET,
                    ATTACHMENT_PATH,
                    Some("bytes=0-3"),
                    StatusCode::PARTIAL_CONTENT,
                ),
                (
                    Method::GET,
                    ATTACHMENT_PATH,
                    Some("bytes=100-200"),
                    StatusCode::RANGE_NOT_SATISFIABLE,
                ),
                (Method::HEAD, ATTACHMENT_PATH, None, StatusCode::OK),
                (
                    Method::GET,
                    "/attachments/1/2/missing.bin",
                    None,
                    StatusCode::NOT_FOUND,
                ),
            ] {
                let label = format!("{method} {path} range={range:?} origins={origins:?}");
                let expected =
                    dispatch_request(&off, read_request(method.clone(), path, origins, range))
                        .await;
                let actual =
                    dispatch_request(&report, read_request(method, path, origins, range)).await;
                assert_eq!(status, expected.status(), "{label}");
                assert_eq!(expected.status(), actual.status(), "{label}");
                assert_eq!(expected.headers(), actual.headers(), "{label}");
                assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
            }
        }
    }

    #[tokio::test]
    async fn report_mode_logs_one_would_deny_line_for_each_refused_origin_only() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let report = dispatch_app("mp", root, REPORT);
        let cases: [(&[&[u8]], Option<&str>); 4] = [
            (&[EVIL.as_bytes()], Some("origin=https://evil.example")),
            (&[b"null".as_slice()], Some("origin=null")),
            (&[WEB.as_bytes()], None),
            (&[], None),
        ];
        for (origins, expected_origin) in cases {
            let label = format!("origins={origins:?}");
            let captured = CapturedLog::default();
            let subscriber = tracing_subscriber::fmt()
                .with_writer(captured.clone())
                .with_ansi(false)
                .with_max_level(tracing::Level::TRACE)
                .finish();
            let response = {
                let _guard = tracing::subscriber::set_default(subscriber);
                dispatch_request(
                    &report,
                    read_request(Method::GET, ATTACHMENT_PATH, origins, None),
                )
                .await
            };
            assert_eq!(StatusCode::OK, response.status(), "{label}");
            let log = captured.text();
            let would_deny: Vec<&str> = log
                .lines()
                .filter(|line| line.contains("reason=\"cors_origin_would_deny\""))
                .collect();
            match expected_origin {
                Some(origin) => {
                    assert_eq!(1, would_deny.len(), "{label}\n{log}");
                    assert!(would_deny[0].contains(" WARN "), "{label}\n{log}");
                    assert!(would_deny[0].trim_end().ends_with(origin), "{label}\n{log}");
                }
                None => assert!(would_deny.is_empty(), "{label}\n{log}"),
            }
        }
    }

    #[tokio::test]
    async fn off_mode_never_inspects_origin() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_stored_representations(root);
        let app = dispatch_app("mp", root, &[]);
        for (shape, origins) in refused_origin_shapes() {
            let response = dispatch_request(
                &app,
                read_request(Method::GET, ATTACHMENT_PATH, &origins, None),
            )
            .await;
            assert_eq!(StatusCode::OK, response.status(), "{shape}");
            assert_eq!(
                Some("*".to_owned()),
                header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                "{shape}"
            );
            assert_eq!(
                vec!["Accept-Encoding".to_owned()],
                vary_values(&response),
                "{shape}"
            );
        }
    }

    #[tokio::test]
    async fn static_mode_is_unchanged_with_every_cors_key_set() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = static_mode_app(root.as_path(), ENFORCE);
        let origin_shapes: [&[&[u8]]; 4] = [
            &[EVIL.as_bytes()],
            &[b"null".as_slice()],
            &[WEB.as_bytes()],
            &[],
        ];
        for origins in origin_shapes {
            let label = format!("origins={origins:?}");
            let response = dispatch_request(
                &app,
                read_request(Method::GET, "/avatars/0.png", origins, None),
            )
            .await;
            assert_eq!(StatusCode::OK, response.status(), "{label}");
            assert_eq!(
                Some("*".to_owned()),
                header_value(&response, header::ACCESS_CONTROL_ALLOW_ORIGIN),
                "{label}"
            );
            assert_eq!(
                vec!["Accept-Encoding".to_owned()],
                vary_values(&response),
                "{label}"
            );
            assert_eq!(
                Some(STATIC_CACHE_CONTROL.to_owned()),
                header_value(&response, header::CACHE_CONTROL),
                "{label}"
            );
        }
    }

    const SIGNATURE_SECRET: [u8; 32] = [11u8; 32];
    const OTHER_SIGNATURE_SECRET: [u8; 32] = [12u8; 32];
    const SIGNATURE_ENFORCE: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "enforce"),
        (
            "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
            "CwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCws=",
        ),
    ];
    const SIGNATURE_REPORT: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "report"),
        (
            "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
            "CwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCws=",
        ),
    ];
    const SIGNATURE_OFF_WITH_KEYS: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "off"),
        (
            "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
            "CwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCwsLCws=",
        ),
    ];
    const UNAVAILABLE: &[u8] = b"This content is no longer available.";
    const ATTACHMENT_KEY: &str = "attachments/1/2/file.bin";
    const IMAGE_PATH: &str = "/attachments/1/2/pic.png";
    const IMAGE_KEY: &str = "attachments/1/2/pic.png";

    fn now_secs() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("the clock is after the unix epoch")
            .as_secs()
    }

    fn params_from(query: Option<&str>) -> HashMap<String, String> {
        query
            .map(|query| {
                query
                    .split('&')
                    .filter(|field| !field.is_empty())
                    .filter_map(|field| field.split_once('='))
                    .map(|(name, value)| (name.to_owned(), value.to_owned()))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn with_extra(signed: String, extra: &str) -> String {
        if extra.is_empty() {
            return signed;
        }
        format!("{signed}&{extra}")
    }

    fn signed_uri(path: &str, key: &str, extra: &str) -> String {
        let now = now_secs();
        with_extra(
            with_signature(path, key, now, now, &SIGNATURE_SECRET),
            extra,
        )
    }

    fn data_package_uri(path: &str, key: &str, extra: &str) -> String {
        with_extra(
            with_data_package_signature(path, key, 1, 1, &SIGNATURE_SECRET),
            extra,
        )
    }

    fn foreign_secret_data_package_uri(path: &str, key: &str) -> String {
        with_data_package_signature(path, key, 1, 1, &OTHER_SIGNATURE_SECRET)
    }

    fn expired_uri(path: &str, key: &str) -> String {
        let expires = now_secs() - 1;
        let issued = expires - ATTACHMENT_URL_TTL_SECS;
        format!(
            "{path}?ex={expires:08x}&is={issued:08x}&hm={}",
            sign(key, expires, issued, UrlKind::Ordinary, &SIGNATURE_SECRET)
        )
    }

    fn foreign_key_uri(path: &str) -> String {
        let now = now_secs();
        let (issued, expires) = issue_window(now, now);
        format!(
            "{path}?ex={expires:08x}&is={issued:08x}&hm={}",
            sign(
                "attachments/9/9/other.bin",
                expires,
                issued,
                UrlKind::Ordinary,
                &SIGNATURE_SECRET
            )
        )
    }

    fn foreign_secret_uri(path: &str, key: &str) -> String {
        let now = now_secs();
        let (issued, expires) = issue_window(now, now);
        format!(
            "{path}?ex={expires:08x}&is={issued:08x}&hm={}",
            sign(
                key,
                expires,
                issued,
                UrlKind::Ordinary,
                &OTHER_SIGNATURE_SECRET
            )
        )
    }

    fn malformed_uri(path: &str, key: &str) -> String {
        let now = now_secs();
        let (issued, expires) = issue_window(now, now);
        format!(
            "{path}?ex={expires:08x}&is={issued:08x}&hm={0}&hm={0}",
            sign(key, expires, issued, UrlKind::Ordinary, &SIGNATURE_SECRET)
        )
    }

    async fn dispatch_uri(
        app: &Arc<AppState>,
        method: Method,
        uri: &str,
        origins: &[&[u8]],
        range: Option<&str>,
    ) -> Response {
        let request = read_request(method, uri, origins, range);
        let params = params_from(request.uri().query());
        catch_all(State(Arc::clone(app)), Query(params), request).await
    }

    fn counter(app: &Arc<AppState>, name: &str) -> u64 {
        app.metrics
            .render()
            .lines()
            .find_map(|line| line.strip_prefix(name)?.trim().parse().ok())
            .expect("the counter is rendered")
    }

    fn write_signed_representations(root: &Path) {
        write_stored_representations(root);
        write_object(
            root,
            "cdn",
            IMAGE_KEY,
            &crate::test_fixtures::synthetic_png(128, 128),
        );
    }

    struct SignedReadCase {
        method: Method,
        path: &'static str,
        key: &'static str,
        range: Option<&'static str>,
        extra: &'static str,
        status: StatusCode,
    }

    fn signed_read_case(
        method: Method,
        path: &'static str,
        key: &'static str,
        range: Option<&'static str>,
        extra: &'static str,
        status: StatusCode,
    ) -> SignedReadCase {
        SignedReadCase {
            method,
            path,
            key,
            range,
            extra,
            status,
        }
    }

    fn signed_read_cases() -> [SignedReadCase; 7] {
        [
            signed_read_case(
                Method::GET,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                None,
                "",
                StatusCode::OK,
            ),
            signed_read_case(
                Method::GET,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                Some("bytes=0-3"),
                "",
                StatusCode::PARTIAL_CONTENT,
            ),
            signed_read_case(
                Method::GET,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                Some("bytes=100-200"),
                "",
                StatusCode::RANGE_NOT_SATISFIABLE,
            ),
            signed_read_case(
                Method::HEAD,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                None,
                "",
                StatusCode::OK,
            ),
            signed_read_case(
                Method::HEAD,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                Some("bytes=0-3"),
                "",
                StatusCode::PARTIAL_CONTENT,
            ),
            signed_read_case(
                Method::GET,
                ATTACHMENT_PATH,
                ATTACHMENT_KEY,
                None,
                "download=1",
                StatusCode::OK,
            ),
            signed_read_case(
                Method::GET,
                IMAGE_PATH,
                IMAGE_KEY,
                None,
                "width=64",
                StatusCode::OK,
            ),
        ]
    }

    fn without_cache_policy(headers: &HeaderMap) -> HeaderMap {
        let mut headers = headers.clone();
        headers.remove(header::CACHE_CONTROL);
        headers.remove("cdn-cache-control");
        headers
    }

    #[tokio::test]
    async fn enforce_serves_a_signed_attachment_on_every_read_shape() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let enforce = dispatch_app("mp", root, SIGNATURE_ENFORCE);

        for case in signed_read_cases() {
            let SignedReadCase {
                method,
                path,
                key,
                range,
                extra,
                status,
            } = case;
            let unsigned = if extra.is_empty() {
                path.to_owned()
            } else {
                format!("{path}?{extra}")
            };
            for signed in [
                signed_uri(path, key, extra),
                data_package_uri(path, key, extra),
            ] {
                let label = format!("{method} {signed} range={range:?}");
                let expected = dispatch_uri(&off, method.clone(), &unsigned, &[], range).await;
                let actual = dispatch_uri(&enforce, method.clone(), &signed, &[], range).await;
                assert_eq!(status, expected.status(), "{label}");
                assert_eq!(expected.status(), actual.status(), "{label}");
                assert_eq!(
                    without_cache_policy(expected.headers()),
                    without_cache_policy(actual.headers()),
                    "{label}"
                );
                match (
                    header_value(&expected, header::CACHE_CONTROL),
                    header_value(&actual, header::CACHE_CONTROL),
                ) {
                    (None, None) => {}
                    (Some(unsigned_policy), Some(signed_policy)) => {
                        assert!(unsigned_policy.contains("max-age=31536000"), "{label}");
                        assert!(signed_policy.starts_with("public, max-age="), "{label}");
                        assert!(!signed_policy.contains("max-age=31536000"), "{label}");
                        assert!(
                            !header_value(&actual, "cdn-cache-control")
                                .expect("a signed read pairs the cdn cache policy")
                                .contains("max-age=31536000"),
                            "{label}"
                        );
                    }
                    mismatch => panic!("{label} {mismatch:?}"),
                }
                assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
            }
        }
    }

    #[tokio::test]
    async fn enforce_answers_every_refusal_with_the_same_bytes() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let app = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        let missing_object = signed_uri(
            "/attachments/1/2/absent.bin",
            "attachments/1/2/absent.bin",
            "",
        );
        let package = data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, "");
        let refusals: [(&str, String); 13] = [
            (
                "data package from another secret",
                foreign_secret_data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            ),
            (
                "data package with uc removed",
                package.replace("&uc=dp", ""),
            ),
            (
                "data package with a tampered uc",
                package.replace("uc=dp", "uc=dq"),
            ),
            (
                "data package with a padded expiry",
                package.replace("ex=0&", "ex=00000000&"),
            ),
            (
                "data package signature for another key",
                data_package_uri(
                    "/attachments/9/9/other.bin",
                    "attachments/9/9/other.bin",
                    "",
                )
                .replace("/attachments/9/9/other.bin", ATTACHMENT_PATH),
            ),
            ("no signature", ATTACHMENT_PATH.to_owned()),
            (
                "other parameters only",
                format!("{ATTACHMENT_PATH}?width=64"),
            ),
            (
                "partial set",
                format!("{ATTACHMENT_PATH}?ex=6a9a7231&is=6a9920b1"),
            ),
            (
                "repeated parameter",
                malformed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            ),
            ("expired", expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY)),
            (
                "signature for another key",
                foreign_key_uri(ATTACHMENT_PATH),
            ),
            (
                "signature from another secret",
                foreign_secret_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            ),
            ("valid signature over a missing object", missing_object),
        ];
        let mut seen: Vec<(StatusCode, HeaderMap, Bytes)> = Vec::new();
        for (label, uri) in refusals {
            for method in [Method::GET, Method::HEAD] {
                for range in [None, Some("bytes=0-3"), Some("bytes=100-200")] {
                    for origins in [&[][..], &[WEB.as_bytes()][..]] {
                        let response =
                            dispatch_uri(&app, method.clone(), &uri, origins, range).await;
                        let status = response.status();
                        let headers = response.headers().clone();
                        let body = body_of(response).await;
                        assert_eq!(StatusCode::NOT_FOUND, status, "{label} {method}");
                        assert_eq!(UNAVAILABLE, body.as_ref(), "{label} {method}");
                        assert_eq!(36, body.len(), "{label} {method}");
                        assert_eq!(
                            Some("no-store"),
                            headers
                                .get(header::CACHE_CONTROL)
                                .map(|value| value.to_str().expect("ascii")),
                            "{label} {method}"
                        );
                        assert!(
                            !headers.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN),
                            "{label} {method}"
                        );
                        seen.push((status, headers, body));
                    }
                }
            }
        }
        let first = seen.first().expect("at least one refusal").clone();
        for (status, headers, body) in seen {
            assert_eq!(first.0, status);
            assert_eq!(first.1, headers);
            assert_eq!(first.2, body);
        }
    }

    #[tokio::test]
    async fn enforce_keeps_a_bad_storage_key_a_bad_request() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let app = dispatch_app("mp", root.as_path(), SIGNATURE_ENFORCE);
        let response = dispatch_uri(&app, Method::GET, "/attachments/%2e%2e/x", &[], None).await;
        assert_eq!(StatusCode::BAD_REQUEST, response.status());
        assert_eq!(b"Bad Request".as_slice(), body_of(response).await);
    }

    #[tokio::test]
    async fn enforce_refuses_before_any_object_read() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let app = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        for uri in [
            ATTACHMENT_PATH.to_owned(),
            expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            foreign_key_uri(ATTACHMENT_PATH),
            format!("{IMAGE_PATH}?width=64"),
        ] {
            let response = dispatch_uri(&app, Method::GET, &uri, &[], None).await;
            assert_eq!(StatusCode::NOT_FOUND, response.status(), "{uri}");
        }
        assert_eq!(0, counter(&app, "fluxer_media_proxy_storage_hits_total"));
        assert_eq!(
            0,
            counter(&app, "fluxer_media_proxy_transform_cache_misses_total")
        );
    }

    #[tokio::test]
    async fn a_refusal_never_reaches_a_warmed_transform_cache() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let app = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        for _ in 0..2 {
            let uri = signed_uri(IMAGE_PATH, IMAGE_KEY, "width=64");
            let response = dispatch_uri(&app, Method::GET, &uri, &[], None).await;
            assert_eq!(StatusCode::OK, response.status());
            assert!(!body_of(response).await.is_empty());
        }
        assert_eq!(
            1,
            counter(&app, "fluxer_media_proxy_transform_cache_hits_total")
        );
        let hits = counter(&app, "fluxer_media_proxy_storage_hits_total");
        assert!(hits > 0);

        let expired = format!("{}&width=64", expired_uri(IMAGE_PATH, IMAGE_KEY));
        let response = dispatch_uri(&app, Method::GET, &expired, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, response.status());
        assert_eq!(UNAVAILABLE, body_of(response).await.as_ref());
        assert_eq!(
            1,
            counter(&app, "fluxer_media_proxy_transform_cache_hits_total"),
            "an expired url never reaches the transform cache"
        );
        assert_eq!(
            hits,
            counter(&app, "fluxer_media_proxy_storage_hits_total"),
            "an expired url never reaches storage"
        );
    }

    #[tokio::test]
    async fn enforce_accepts_every_spelling_of_the_signed_key() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "cdn", "attachments/1/2/café.gif", STORED_BYTES);
        write_object(root, "cdn", "attachments/1/2/a/b.gif", STORED_BYTES);
        let app = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        for (path, key) in [
            ("/attachments/1/2/caf%C3%A9.gif", "attachments/1/2/café.gif"),
            ("/attachments/1/2/café.gif", "attachments/1/2/café.gif"),
            ("/attachments/1/2/a%2Fb.gif", "attachments/1/2/a/b.gif"),
            ("/attachments/1/2/a/b.gif", "attachments/1/2/a/b.gif"),
        ] {
            let now = now_secs();
            let (issued, expires) = issue_window(now, now);
            let uri = format!(
                "{path}?ex={expires:08x}&is={issued:08x}&hm={}",
                sign(key, expires, issued, UrlKind::Ordinary, &SIGNATURE_SECRET)
            );
            for spelling in [uri.clone(), format!("{uri}&")] {
                let response = dispatch_uri(&app, Method::GET, &spelling, &[], None).await;
                assert_eq!(StatusCode::OK, response.status(), "{spelling}");
                assert_eq!(STORED_BYTES, body_of(response).await, "{spelling}");
            }
        }
    }

    #[tokio::test]
    async fn enforce_leaves_every_other_read_route_unsigned() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        write_object(
            root,
            "cdn",
            "entrance-sounds/42/abc123def456.wav",
            STORED_BYTES,
        );
        let asset = parse_standard_asset_path(ASSET_PATH).expect("asset path");
        write_object(
            root,
            "cdn",
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(64, 64),
        );
        let off = dispatch_app("mp", root, &[]);
        let enforce = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        for path in [THEME_PATH, ENTRANCE_SOUND_PATH, ASSET_PATH, "/nothing/here"] {
            for method in [Method::GET, Method::HEAD] {
                let label = format!("{method} {path}");
                let expected = dispatch_uri(&off, method.clone(), path, &[], None).await;
                let actual = dispatch_uri(&enforce, method, path, &[], None).await;
                assert_eq!(expected.status(), actual.status(), "{label}");
                assert_eq!(expected.headers(), actual.headers(), "{label}");
                assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
            }
        }
    }

    #[tokio::test]
    async fn static_and_relay_modes_are_unchanged_with_the_signature_keys_set() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_object(root, "static", "avatars/0.png", STORED_BYTES);
        write_object(root, "cdn", ATTACHMENT_KEY, STORED_BYTES);

        let plain_static = dispatch_app("static", root, &[]);
        let signed_static = dispatch_app("static", root, SIGNATURE_ENFORCE);
        let expected = dispatch_uri(&plain_static, Method::GET, "/avatars/0.png", &[], None).await;
        let actual = dispatch_uri(&signed_static, Method::GET, "/avatars/0.png", &[], None).await;
        assert_eq!(StatusCode::OK, expected.status());
        assert_eq!(expected.status(), actual.status());
        assert_eq!(expected.headers(), actual.headers());
        assert_eq!(body_of(expected).await, body_of(actual).await);

        let relay = dispatch_app("relay", root, SIGNATURE_ENFORCE);
        let response = dispatch_uri(&relay, Method::GET, ATTACHMENT_PATH, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, response.status());
        assert_eq!(b"Not Found".as_slice(), body_of(response).await);

        let upload = dispatch_app("upload", root, SIGNATURE_ENFORCE);
        let refused = dispatch_uri(&upload, Method::GET, ATTACHMENT_PATH, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, refused.status());
        assert_eq!(UNAVAILABLE, body_of(refused).await.as_ref());
        let allowed = dispatch_uri(
            &upload,
            Method::GET,
            &signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            &[],
            None,
        )
        .await;
        assert_eq!(StatusCode::OK, allowed.status());
        assert_eq!(STORED_BYTES, body_of(allowed).await);
    }

    #[tokio::test]
    async fn a_cors_refusal_precedes_the_signature_check() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let mut env: Vec<(&str, &str)> = ENFORCE.to_vec();
        env.extend_from_slice(SIGNATURE_ENFORCE);
        let app = dispatch_app("mp", root, &env);
        for uri in [
            ATTACHMENT_PATH.to_owned(),
            signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
        ] {
            let response = dispatch_uri(&app, Method::GET, &uri, &[EVIL.as_bytes()], None).await;
            assert_eq!(StatusCode::FORBIDDEN, response.status(), "{uri}");
            assert_eq!(b"Forbidden".as_slice(), body_of(response).await, "{uri}");
        }
        assert_eq!(0, counter(&app, "fluxer_media_proxy_storage_hits_total"));
    }

    #[tokio::test]
    async fn report_mode_changes_no_response_byte_for_a_stored_object() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        for uri in [
            ATTACHMENT_PATH.to_owned(),
            format!("{ATTACHMENT_PATH}?download=1"),
            expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            foreign_key_uri(ATTACHMENT_PATH),
            format!("{IMAGE_PATH}?width=64"),
        ] {
            for method in [Method::GET, Method::HEAD] {
                for range in [None, Some("bytes=0-3"), Some("bytes=100-200")] {
                    let label = format!("{method} {uri} range={range:?}");
                    let expected = dispatch_uri(&off, method.clone(), &uri, &[], range).await;
                    let actual = dispatch_uri(&report, method.clone(), &uri, &[], range).await;
                    assert_eq!(expected.status(), actual.status(), "{label}");
                    assert_eq!(expected.headers(), actual.headers(), "{label}");
                    assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
                }
            }
        }
    }

    #[tokio::test]
    async fn report_mode_changes_only_the_cache_lifetime_of_a_valid_signature() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        for uri in [
            signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            signed_uri(IMAGE_PATH, IMAGE_KEY, "width=64"),
        ] {
            for method in [Method::GET, Method::HEAD] {
                for range in [None, Some("bytes=0-3")] {
                    let label = format!("{method} {uri} range={range:?}");
                    let expected = dispatch_uri(&off, method.clone(), &uri, &[], range).await;
                    let actual = dispatch_uri(&report, method.clone(), &uri, &[], range).await;
                    assert_eq!(expected.status(), actual.status(), "{label}");
                    assert_eq!(
                        without_cache_policy(expected.headers()),
                        without_cache_policy(actual.headers()),
                        "{label}"
                    );
                    assert_eq!(
                        Some(STATIC_CACHE_CONTROL.to_owned()),
                        header_value(&expected, header::CACHE_CONTROL),
                        "{label}"
                    );
                    let capped = header_value(&actual, header::CACHE_CONTROL)
                        .expect("a report read of a valid signature states its own max age");
                    let max_age: u64 = capped
                        .strip_prefix("public, max-age=")
                        .expect("the capped policy is a plain max age")
                        .parse()
                        .expect("a max age is a number");
                    assert!(max_age <= ATTACHMENT_URL_TTL_SECS, "{label} {capped}");
                    assert_eq!(
                        Some(capped),
                        header_value(&actual, "cdn-cache-control"),
                        "{label}"
                    );
                    assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
                }
            }
        }
    }

    #[tokio::test]
    async fn report_leaves_a_refusable_read_caching_forever() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        for uri in [
            ATTACHMENT_PATH.to_owned(),
            expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            foreign_key_uri(ATTACHMENT_PATH),
        ] {
            let response = dispatch_uri(&report, Method::GET, &uri, &[], None).await;
            assert_eq!(StatusCode::OK, response.status(), "{uri}");
            assert_eq!(
                Some(STATIC_CACHE_CONTROL.to_owned()),
                header_value(&response, header::CACHE_CONTROL),
                "{uri}"
            );
        }
    }

    #[tokio::test]
    async fn only_enforce_masks_a_missing_object_so_it_matches_a_refusal() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let off = dispatch_app("mp", root, &[]);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        let enforce = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        let uri = "/attachments/1/2/absent.bin";
        let plain = dispatch_uri(&off, Method::GET, uri, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, plain.status());
        assert_eq!(b"Not Found".as_slice(), body_of(plain).await);

        let dry_run = dispatch_uri(&report, Method::GET, uri, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, dry_run.status());
        assert_eq!(b"Not Found".as_slice(), body_of(dry_run).await);

        let signed = signed_uri(uri, "attachments/1/2/absent.bin", "");
        let masked = dispatch_uri(&enforce, Method::GET, &signed, &[], None).await;
        assert_eq!(StatusCode::NOT_FOUND, masked.status());
        assert_eq!(UNAVAILABLE, body_of(masked).await.as_ref());
    }

    #[tokio::test]
    async fn an_enforced_attachment_is_never_cached_past_its_signature() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let app = dispatch_app("mp", root, SIGNATURE_ENFORCE);

        let response = dispatch_uri(
            &app,
            Method::GET,
            &signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            &[],
            None,
        )
        .await;
        assert_eq!(StatusCode::OK, response.status());
        let policy = header_value(&response, header::CACHE_CONTROL).expect("cache control");
        let max_age: u64 = policy
            .strip_prefix("public, max-age=")
            .expect("a signed attachment states its own max age")
            .parse()
            .expect("a max age is a number");
        assert!(max_age <= ATTACHMENT_URL_TTL_SECS, "{policy}");
        assert!(max_age + 60 >= ATTACHMENT_URL_TTL_SECS, "{policy}");
        assert_eq!(Some(policy), header_value(&response, "cdn-cache-control"));

        let package = dispatch_uri(
            &app,
            Method::GET,
            &data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
            &[],
            None,
        )
        .await;
        assert_eq!(StatusCode::OK, package.status());
        assert_eq!(
            Some(format!("public, max-age={ATTACHMENT_URL_TTL_SECS}")),
            header_value(&package, header::CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn an_unenforced_read_and_every_other_route_cache_forever() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        for env in [&[][..], SIGNATURE_OFF_WITH_KEYS] {
            let app = dispatch_app("mp", root, env);
            let response = dispatch_uri(
                &app,
                Method::GET,
                &signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, ""),
                &[],
                None,
            )
            .await;
            assert_eq!(StatusCode::OK, response.status());
            assert_eq!(
                Some(STATIC_CACHE_CONTROL.to_owned()),
                header_value(&response, header::CACHE_CONTROL)
            );
        }
        let enforcing = dispatch_app("mp", root, SIGNATURE_ENFORCE);
        let theme = dispatch_uri(&enforcing, Method::GET, THEME_PATH, &[], None).await;
        assert_eq!(StatusCode::OK, theme.status());
        assert_eq!(
            Some(STATIC_CACHE_CONTROL.to_owned()),
            header_value(&theme, header::CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn report_mode_logs_one_line_for_each_refused_verdict_only() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        let cases: [(&str, String); 8] = [
            ("missing", ATTACHMENT_PATH.to_owned()),
            ("malformed", malformed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY)),
            ("mismatch", foreign_key_uri(ATTACHMENT_PATH)),
            ("expired", expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY)),
            ("", signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, "")),
            ("", data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, "")),
            (
                "malformed",
                data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, "").replace("&uc=dp", ""),
            ),
            (
                "mismatch",
                foreign_secret_data_package_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            ),
        ];
        for (verdict, uri) in cases {
            let captured = CapturedLog::default();
            let subscriber = tracing_subscriber::fmt()
                .with_writer(captured.clone())
                .with_ansi(false)
                .with_max_level(tracing::Level::TRACE)
                .finish();
            let response = {
                let _guard = tracing::subscriber::set_default(subscriber);
                dispatch_uri(&report, Method::GET, &uri, &[], None).await
            };
            assert_eq!(StatusCode::OK, response.status(), "{verdict} {uri}");
            let log = captured.text();
            let lines: Vec<&str> = log
                .lines()
                .filter(|line| line.contains("reason=\"attachment_signature_would_deny\""))
                .collect();
            if verdict.is_empty() {
                assert!(lines.is_empty(), "{uri}\n{log}");
                continue;
            }
            assert_eq!(1, lines.len(), "{verdict} {uri}\n{log}");
            assert!(lines[0].contains(" WARN "), "{verdict}\n{log}");
            assert!(
                lines[0].contains(&format!("verdict=\"{verdict}\"")),
                "{verdict}\n{log}"
            );
        }
    }

    #[tokio::test]
    async fn off_mode_serves_a_signature_carrying_request_unchanged() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let plain = dispatch_app("mp", root, &[]);
        let staged = dispatch_app("mp", root, SIGNATURE_OFF_WITH_KEYS);
        for uri in [
            ATTACHMENT_PATH.to_owned(),
            expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY),
            foreign_key_uri(ATTACHMENT_PATH),
            format!("{ATTACHMENT_PATH}?ex=zzz&is=&hm="),
            "/attachments/1/2/absent.bin".to_owned(),
        ] {
            let expected = dispatch_uri(&plain, Method::GET, &uri, &[], None).await;
            let actual = dispatch_uri(&staged, Method::GET, &uri, &[], None).await;
            assert_eq!(expected.status(), actual.status(), "{uri}");
            assert_eq!(expected.headers(), actual.headers(), "{uri}");
            assert_eq!(body_of(expected).await, body_of(actual).await, "{uri}");
        }
    }

    #[tokio::test]
    async fn off_mode_serves_a_signed_read_byte_identical_to_an_unsigned_read() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        for env in [&[][..], SIGNATURE_OFF_WITH_KEYS] {
            let unsigned_app = dispatch_app("mp", root, &[]);
            let signed_app = dispatch_app("mp", root, env);
            for case in signed_read_cases() {
                let SignedReadCase {
                    method,
                    path,
                    key,
                    range,
                    extra,
                    status,
                } = case;
                let separator = if extra.is_empty() { "" } else { "&" };
                let unsigned = if extra.is_empty() {
                    path.to_owned()
                } else {
                    format!("{path}?{extra}")
                };
                for signed in [
                    signed_uri(path, key, extra),
                    data_package_uri(path, key, extra),
                    with_extra(expired_uri(path, key), extra),
                    format!("{path}?ex=zzz&is=&hm=&uc=xx{separator}{extra}"),
                    format!("{path}?uc=dp{separator}{extra}"),
                ] {
                    let label = format!("{method} {signed} range={range:?} env={env:?}");
                    let expected =
                        dispatch_uri(&unsigned_app, method.clone(), &unsigned, &[], range).await;
                    let actual =
                        dispatch_uri(&signed_app, method.clone(), &signed, &[], range).await;
                    assert_eq!(status, expected.status(), "{label}");
                    assert_eq!(expected.status(), actual.status(), "{label}");
                    assert_eq!(expected.headers(), actual.headers(), "{label}");
                    assert_eq!(body_of(expected).await, body_of(actual).await, "{label}");
                }
            }
        }
    }

    #[test]
    fn signature_parameters_never_reach_the_attachment_reader() {
        let params: HashMap<String, String> = [
            ("ex", "0"),
            ("is", "6a9920b1"),
            ("hm", "abc"),
            ("uc", "dp"),
            ("width", "64"),
            ("download", "1"),
            ("EX", "1"),
            ("%75c", "dp"),
            ("ucx", "1"),
        ]
        .into_iter()
        .map(|(name, value)| (name.to_owned(), value.to_owned()))
        .collect();
        let filtered = without_signature_parameters(&params);
        let mut names: Vec<&str> = filtered.keys().map(String::as_str).collect();
        names.sort_unstable();
        assert_eq!(vec!["%75c", "EX", "download", "ucx", "width"], names);
        assert_eq!(Some("64"), filtered.get("width").map(String::as_str));
        assert_eq!(Some("1"), filtered.get("download").map(String::as_str));
    }

    const SIGNATURE_VERDICTS: &str = "fluxer_media_proxy_attachment_signature_verdicts_total";

    fn verdict_counter(app: &Arc<AppState>, verdict: &str) -> u64 {
        counter(
            app,
            &format!("{SIGNATURE_VERDICTS}{{verdict=\"{verdict}\"}}"),
        )
    }

    fn verdict_cases() -> [(&'static str, String); 5] {
        [
            ("missing", ATTACHMENT_PATH.to_owned()),
            ("malformed", malformed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY)),
            ("mismatch", foreign_key_uri(ATTACHMENT_PATH)),
            ("expired", expired_uri(ATTACHMENT_PATH, ATTACHMENT_KEY)),
            ("valid", signed_uri(ATTACHMENT_PATH, ATTACHMENT_KEY, "")),
        ]
    }

    #[tokio::test]
    async fn report_and_enforce_count_the_verdict_of_every_attachment_read() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        for env in [SIGNATURE_REPORT, SIGNATURE_ENFORCE] {
            let app = dispatch_app("mp", root, env);
            for (verdict, uri) in verdict_cases() {
                dispatch_uri(&app, Method::GET, &uri, &[], None).await;
                assert_eq!(1, verdict_counter(&app, verdict), "{verdict} {uri} {env:?}");
            }
            for (verdict, _) in verdict_cases() {
                assert_eq!(1, verdict_counter(&app, verdict), "{verdict} {env:?}");
            }
        }
    }

    #[tokio::test]
    async fn off_mode_counts_no_verdict_at_all() {
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        for env in [&[][..], SIGNATURE_OFF_WITH_KEYS] {
            let app = dispatch_app("mp", root, env);
            for (_, uri) in verdict_cases() {
                dispatch_uri(&app, Method::GET, &uri, &[], None).await;
            }
            for (verdict, _) in verdict_cases() {
                assert_eq!(0, verdict_counter(&app, verdict), "{verdict} {env:?}");
            }
        }
    }

    #[tokio::test]
    async fn a_report_burst_bounds_its_log_lines_and_still_counts_every_read() {
        const BURST: usize = 50;
        let tmp = tempfile::tempdir().expect("storage root");
        let root = tmp.path().canonicalize().expect("canonical storage root");
        let root = root.as_path();
        write_signed_representations(root);
        let report = dispatch_app("mp", root, SIGNATURE_REPORT);
        let captured = CapturedLog::default();
        let subscriber = tracing_subscriber::fmt()
            .with_writer(captured.clone())
            .with_ansi(false)
            .with_max_level(tracing::Level::TRACE)
            .finish();
        {
            let _guard = tracing::subscriber::set_default(subscriber);
            for _ in 0..BURST {
                let response = dispatch_uri(&report, Method::GET, ATTACHMENT_PATH, &[], None).await;
                assert_eq!(StatusCode::OK, response.status());
            }
            tokio::time::sleep(std::time::Duration::from_millis(1_100)).await;
            dispatch_uri(&report, Method::GET, ATTACHMENT_PATH, &[], None).await;
        }
        let log = captured.text();
        let lines: Vec<&str> = log
            .lines()
            .filter(|line| line.contains("reason=\"attachment_signature_would_deny\""))
            .collect();
        assert!(
            lines.len() < BURST,
            "{} lines for {BURST} refusable reads\n{log}",
            lines.len()
        );
        assert_eq!(
            (BURST + 1) as u64,
            verdict_counter(&report, "missing"),
            "every refusable read is counted"
        );
        let suppressed: u64 = lines
            .last()
            .expect("the burst logs at least one line")
            .split("suppressed=")
            .nth(1)
            .expect("an emitted line reports the lines it stands in for")
            .split_whitespace()
            .next()
            .expect("a suppressed count")
            .parse()
            .expect("a suppressed count is a number");
        assert!(suppressed > 0, "{log}");
        assert!(
            lines.len() as u64 + suppressed <= (BURST + 1) as u64,
            "{log}"
        );
    }
}
