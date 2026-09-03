// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::DeploymentMode,
    constants::AssetKind,
    server::{
        asset_path::{
            StorageKeyDecodeError, decode_storage_key, parse_entrance_sound_path,
            parse_guild_member_asset_path, parse_simple_asset_path, parse_standard_asset_path,
        },
        external::serve_external,
        response::error::{text, text_with_source},
        state::AppState,
        stored,
    },
};
use axum::{
    body::Body,
    extract::{Query, State},
    http::{Method, Request, StatusCode},
    response::Response,
};
use std::{collections::HashMap, sync::Arc};

pub(in crate::server) async fn catch_all(
    State(app): State<Arc<AppState>>,
    Query(params): Query<HashMap<String, String>>,
    request: Request<Body>,
) -> Response {
    let method = request.method().clone();
    if method != Method::GET && method != Method::HEAD {
        return text(StatusCode::METHOD_NOT_ALLOWED, "Method Not Allowed");
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
    if let Some(rest) = path.strip_prefix("/external/") {
        return serve_external(&app, method, rest, &params, request.headers()).await;
    }
    if path.starts_with("/attachments/") {
        let key = match decode_storage_key(&path) {
            Ok(key) => key,
            Err(err) => return storage_key_decode_response(err),
        };
        return stored::serve_attachment(&app, method, &key, &params, request.headers()).await;
    }
    if path.starts_with("/themes/") && path.ends_with(".css") {
        let key = match decode_storage_key(&path) {
            Ok(key) => key,
            Err(err) => return storage_key_decode_response(err),
        };
        return stored::serve_stored_with_override(
            &app,
            method,
            &app.cfg.storage.bucket_cdn,
            &key,
            "text/css; charset=utf-8",
            request.headers(),
        )
        .await;
    }
    if let Some(key) = parse_entrance_sound_path(&path) {
        return stored::serve_stored_raw(
            &app,
            method,
            &app.cfg.storage.bucket_cdn,
            &key,
            request.headers(),
        )
        .await;
    }
    if let Some(asset) = parse_guild_member_asset_path(&path) {
        return stored::serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_simple_asset_path(&path, AssetKind::Emoji) {
        return stored::serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_simple_asset_path(&path, AssetKind::Sticker) {
        return stored::serve_asset_image(&app, method, asset, &params, request.headers()).await;
    }
    if let Some(asset) = parse_standard_asset_path(&path) {
        return stored::serve_asset_image(&app, method, asset, &params, request.headers()).await;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use axum::{body::to_bytes, http::header};
    use bytes::Bytes;
    use std::path::Path;

    const STORED_BYTES: &[u8] = b"raw-stored-bytes";

    fn dispatch_config(mode: &str, storage_root: &Path) -> Config {
        Config::load_from_iter([
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
        ])
        .expect("dispatch test config")
    }

    fn write_object(storage_root: &Path, bucket: &str, key: &str, body: &[u8]) {
        let path = storage_root.join(bucket).join(key);
        std::fs::create_dir_all(path.parent().expect("object parent")).expect("create bucket dir");
        std::fs::write(path, body).expect("write object");
    }

    async fn dispatch(app: &Arc<AppState>, method: Method, path: &str) -> Response {
        let request = Request::builder()
            .method(method)
            .uri(path)
            .body(Body::empty())
            .expect("dispatch request");
        catch_all(State(Arc::clone(app)), Query(HashMap::new()), request).await
    }

    async fn dispatch_body(app: &Arc<AppState>, path: &str) -> Bytes {
        let response = dispatch(app, Method::GET, path).await;
        assert_eq!(StatusCode::OK, response.status(), "{path} must be served");
        to_bytes(response.into_body(), usize::MAX)
            .await
            .expect("dispatch body")
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
        let app = Arc::new(AppState::for_tests(dispatch_config("static", root)));

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

    fn static_mode_app(root: &Path) -> Arc<AppState> {
        for key in STATIC_KEYS {
            write_object(root, "static", key, STORED_BYTES);
        }
        Arc::new(AppState::for_tests(dispatch_config("static", root)))
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
        let app = static_mode_app(root.as_path());
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
        let app = static_mode_app(root.as_path());
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
        write_object(root, "cdn", "themes/dark.css", b"body{color:#fff}");
        write_object(
            root,
            "cdn",
            "entrance-sounds/42/abc123def456.wav",
            STORED_BYTES,
        );
        write_object(root, "static", "themes/dark.css", STORED_BYTES);
        let app = Arc::new(AppState::for_tests(dispatch_config("mp", root)));

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
            b"body{color:#fff}".as_slice(),
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
}
