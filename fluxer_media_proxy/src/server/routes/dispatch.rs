// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::{DeploymentMode, PolicyMode},
    constants::AssetKind,
    request_log::RequestId,
    server::{
        asset_path::{
            StorageKeyDecodeError, decode_storage_key, parse_entrance_sound_path,
            parse_guild_member_asset_path, parse_operator_wall_avatar_path,
            parse_simple_asset_path, parse_standard_asset_path,
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
    http::{HeaderMap, HeaderValue, Method, Request, StatusCode, header},
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
    if app.cfg.mode != DeploymentMode::Relay && cors::is_preflight(&method, request.headers()) {
        return preflight(&app, request.uri().path(), request.headers());
    }
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

fn preflight(app: &AppState, path: &str, headers: &HeaderMap) -> Response {
    if app.cfg.cors.mode == PolicyMode::Off {
        return cors::preflight_response(&OriginCheck::Absent);
    }
    let check = cors::check_origin(&app.cfg.cors.allowed_origins, headers);
    if check != OriginCheck::Refused {
        return cors::preflight_response(&check);
    }
    if app.cfg.cors.mode == PolicyMode::Enforce {
        return cors::refused_response(headers);
    }
    cors::log_would_refuse(&Method::OPTIONS, path, headers);
    cors::preflight_response(&check)
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
    if let Some(key) = parse_operator_wall_avatar_path(path) {
        return serve_operator_wall_avatar(app, method, &key, headers).await;
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

const OPERATOR_WALL_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";

async fn serve_operator_wall_avatar(
    app: &Arc<AppState>,
    method: Method,
    key: &str,
    headers: &HeaderMap,
) -> Response {
    let mut response =
        stored::serve_stored_raw(app, method, &app.cfg.storage.bucket_cdn, key, headers).await;
    if matches!(
        response.status(),
        StatusCode::OK | StatusCode::PARTIAL_CONTENT
    ) {
        let response_headers = response.headers_mut();
        response_headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("image/webp"));
        response_headers.insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(OPERATOR_WALL_CACHE_CONTROL),
        );
        response_headers.insert(
            "CDN-Cache-Control",
            HeaderValue::from_static(OPERATOR_WALL_CACHE_CONTROL),
        );
    }
    response
}

fn storage_key_decode_response(err: StorageKeyDecodeError) -> Response {
    text_with_source(
        StatusCode::BAD_REQUEST,
        "Bad Request",
        "invalid_storage_key",
        err,
    )
}

pub(in crate::server) fn without_signature_parameters(
    params: &HashMap<String, String>,
) -> HashMap<String, String> {
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
        time::{SystemTime, UNIX_EPOCH},
    };

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

    fn header_value(response: &Response, name: impl header::AsHeaderName) -> Option<String> {
        response
            .headers()
            .get(name)
            .map(|value| value.to_str().expect("header is ascii").to_owned())
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

    const SIGNATURE_SECRET: [u8; 32] = [11u8; 32];
    const OTHER_SIGNATURE_SECRET: [u8; 32] = [12u8; 32];
    const SIGNATURE_ENFORCE: &[(&str, &str)] = &[
        ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "enforce"),
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
}
