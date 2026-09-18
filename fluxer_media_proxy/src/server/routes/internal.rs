// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::BudgetedBytes,
    constants,
    image_transform::ResizeMode,
    media_process, mime,
    output_format::OutputFormat,
    server::{
        format_policy::is_svg_content_type,
        media_operations::{
            MediaFailure, MediaInput, MediaInputLimit, MetadataOutput, load_media_input,
            resolve_metadata,
        },
        response::{
            MediaResponse,
            error::{
                canonical_reason_str, json_response, storage_error_response, text, text_with_source,
            },
            media_response,
        },
        state::AppState,
        transform::execution::{run_transform, transform_error_is_timeout},
    },
};
use axum::{
    body::{Body, to_bytes},
    extract::State,
    http::{HeaderMap, Method, Request, StatusCode, header},
    response::Response,
};
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;
use futures_util::StreamExt as _;
use serde::Deserialize;
use std::sync::Arc;

const UPLOAD_SNIFF_PREFIX_BYTES: usize = 8192;

#[derive(Debug, Deserialize)]
struct MetadataRequest {
    version: Option<i64>,
    #[serde(rename = "type")]
    typ: String,
    nsfw: String,
    base64: Option<String>,
    upload_filename: Option<String>,
    filename: Option<String>,
    bucket: Option<String>,
    key: Option<String>,
    url: Option<String>,
    with_base64: Option<bool>,
}

impl MetadataRequest {
    fn into_media_input(self) -> Result<MediaInput, MediaFailure> {
        match self.typ.as_str() {
            "base64" => Ok(MediaInput::Base64 {
                data: self.base64.ok_or(MediaFailure::MediaInputMissingField)?,
                filename: self.filename,
            }),
            "upload" => Ok(MediaInput::Upload {
                upload_filename: self
                    .upload_filename
                    .ok_or(MediaFailure::MediaInputMissingField)?,
                filename: self.filename,
            }),
            "s3" => Ok(MediaInput::Storage {
                bucket: self.bucket.ok_or(MediaFailure::MediaInputMissingField)?,
                key: self.key.ok_or(MediaFailure::MediaInputMissingField)?,
                filename: self.filename,
            }),
            "external" => Ok(MediaInput::External {
                url: self.url.ok_or(MediaFailure::MediaInputMissingField)?,
                filename: self.filename,
            }),
            _ => Err(MediaFailure::MediaInputUnsupportedType),
        }
    }
}

#[derive(Debug, Deserialize)]
struct FramesRequest {
    version: Option<i64>,
    #[serde(rename = "type")]
    typ: String,
    base64: Option<String>,
    upload_filename: Option<String>,
    filename: Option<String>,
    bucket: Option<String>,
    key: Option<String>,
    url: Option<String>,
}

impl FramesRequest {
    fn into_metadata_request(self) -> MetadataRequest {
        MetadataRequest {
            version: self.version,
            typ: self.typ,
            nsfw: "allow".to_owned(),
            base64: self.base64,
            upload_filename: self.upload_filename,
            filename: self.filename,
            bucket: self.bucket,
            key: self.key,
            url: self.url,
            with_base64: None,
        }
    }
}

#[derive(Debug, Deserialize)]
struct UploadFileRequest {
    upload_filename: String,
}

pub(in crate::server) async fn metadata_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, app.cfg.secret_key.expose()) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, canonical_reason_str(status)),
    };
    let req: MetadataRequest = match serde_json::from_slice::<MetadataRequest>(&body) {
        Ok(req) if req.version == Some(2) => req,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let scan_nsfw = match req.nsfw.as_str() {
        "block" | "flag" => true,
        "allow" => false,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let include_data = req.with_base64.unwrap_or(false);
    let input = match req.into_media_input() {
        Ok(input) => input,
        Err(failure) => return failure.into_response(),
    };
    let MetadataOutput { mut metadata, data } =
        match resolve_metadata(&app, input, scan_nsfw, include_data).await {
            Ok(output) => output,
            Err(failure) => return failure.into_response(),
        };
    if let Some(data) = data {
        metadata["base64"] = serde_json::Value::String(general_purpose::STANDARD.encode(&data));
    }
    json_response(StatusCode::OK, metadata.to_string())
}

pub(in crate::server) async fn thumbnail_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, app.cfg.secret_key.expose()) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, canonical_reason_str(status)),
    };
    let req: UploadFileRequest = match serde_json::from_slice(&body) {
        Ok(req) => req,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let object = match app
        .store
        .read_object(&app.cfg.storage.bucket_uploads, &req.upload_filename)
        .await
    {
        Ok(object) => object,
        Err(err) => return storage_error_response(&req.upload_filename, err),
    };
    let media = if mime::category(&object.content_type) == Some(mime::Category::Video) {
        match media_process::extract_video_thumbnail(
            &object.data,
            OutputFormat::WebP,
            &app.media.limits(),
        ) {
            Ok(media) => media,
            Err(err) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "video_thumbnail_failed",
                    err,
                );
            }
        }
    } else {
        let options = media_process::ImageOptions {
            width: Some(512),
            height: Some(512),
            format: OutputFormat::WebP,
            resize_mode: ResizeMode::Fit,
            deadline_ms: app.media.transforms().transform_deadline_ms(),
            ..Default::default()
        };
        match run_transform(app.media.transforms(), object.data.clone(), options).await {
            Ok(media) => media,
            Err(err) if transform_error_is_timeout(&err) => {
                return text_with_source(
                    StatusCode::GATEWAY_TIMEOUT,
                    "Gateway Timeout",
                    "image_thumbnail_timeout",
                    err,
                );
            }
            Err(err) => {
                return text_with_source(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "image_thumbnail_failed",
                    err,
                );
            }
        }
    };
    media_response(MediaResponse {
        method: Method::GET,
        data: BudgetedBytes::from(Bytes::from(media.bytes)),
        content_type: media.content_type,
        range_header: None,
        disposition: None,
    })
}

pub(in crate::server) async fn frames_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, app.cfg.secret_key.expose()) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, canonical_reason_str(status)),
    };
    let req: FramesRequest = match serde_json::from_slice::<FramesRequest>(&body) {
        Ok(req) if req.version.is_none_or(|version| version == 2) => req,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let input = match req.into_metadata_request().into_media_input() {
        Ok(input) => input,
        Err(failure) => return failure.into_response(),
    };
    let input = match load_media_input(&app, input, MediaInputLimit::INTERNAL_REQUEST).await {
        Ok(input) => input,
        Err(failure) => return failure.into_response(),
    };
    match media_process::extract_video_thumbnail(
        &input.data,
        OutputFormat::JPEG,
        &app.media.limits(),
    ) {
        Ok(frame) => {
            let encoded = general_purpose::STANDARD.encode(frame.bytes);
            json_response(
                StatusCode::OK,
                format!(
                    "{{\"frames\":[{{\"timestamp\":0,\"mime_type\":\"image/jpeg\",\"base64\":\"{encoded}\"}}]}}"
                ),
            )
        }
        Err(_) => json_response(StatusCode::OK, "{\"frames\":[]}".to_owned()),
    }
}

pub(in crate::server) async fn sniff_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if !check_internal_auth(&headers, app.cfg.secret_key.expose()) {
        return text(StatusCode::UNAUTHORIZED, "Unauthorized");
    }
    let body = match read_limited_body(request).await {
        Ok(body) => body,
        Err(status) => return text(status, canonical_reason_str(status)),
    };
    let req: UploadFileRequest = match serde_json::from_slice(&body) {
        Ok(req) => req,
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let range = format!("bytes=0-{}", UPLOAD_SNIFF_PREFIX_BYTES - 1);
    let object = match app
        .store
        .stream_object(
            &app.cfg.storage.bucket_uploads,
            &req.upload_filename,
            Some(&range),
        )
        .await
    {
        Ok(object) => object,
        Err(err) => return storage_error_response(&req.upload_filename, err),
    };
    let prefix = match read_sniff_prefix(object.body).await {
        Ok(prefix) => prefix,
        Err(err) => {
            return text_with_source(
                StatusCode::BAD_GATEWAY,
                "Bad Gateway",
                "sniff_read_failed",
                err,
            );
        }
    };
    let sniffed = mime::sniff(&prefix).mime;
    let content_type = (mime::is_supported_media_mime(sniffed) && !is_svg_content_type(sniffed))
        .then_some(sniffed);
    json_response(
        StatusCode::OK,
        serde_json::json!({ "content_type": content_type }).to_string(),
    )
}

async fn read_sniff_prefix(body: Body) -> Result<Vec<u8>, axum::Error> {
    let mut prefix = Vec::with_capacity(UPLOAD_SNIFF_PREFIX_BYTES);
    let mut chunks = body.into_data_stream();
    while prefix.len() < UPLOAD_SNIFF_PREFIX_BYTES {
        let Some(chunk) = chunks.next().await else {
            break;
        };
        let chunk = chunk?;
        let wanted = chunk.len().min(UPLOAD_SNIFF_PREFIX_BYTES - prefix.len());
        prefix.extend_from_slice(&chunk[..wanted]);
    }
    Ok(prefix)
}

fn check_internal_auth(headers: &HeaderMap, secret: &str) -> bool {
    let Some(auth) = headers
        .get(header::AUTHORIZATION)
        .and_then(|value| value.to_str().ok())
    else {
        return false;
    };
    let expected = format!("Bearer {secret}");
    if auth.len() != expected.len() {
        return false;
    }
    auth.bytes()
        .zip(expected.bytes())
        .fold(0u8, |diff, (a, b)| diff | (a ^ b))
        == 0
}

async fn read_limited_body(request: Request<Body>) -> Result<Bytes, StatusCode> {
    to_bytes(
        request.into_body(),
        constants::MAX_INTERNAL_REQUEST_BODY_BYTES + 1,
    )
    .await
    .map_err(|_| StatusCode::BAD_REQUEST)
    .and_then(|body| {
        if body.len() > constants::MAX_INTERNAL_REQUEST_BODY_BYTES {
            Err(StatusCode::PAYLOAD_TOO_LARGE)
        } else {
            Ok(body)
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        config::Config,
        storage::tests::{FakeObject, fake_s3},
        test_fixtures::synthetic_png,
    };
    use axum::http::HeaderValue;
    use http_body_util::BodyExt as _;
    use std::path::Path;
    use tokio::sync::mpsc;

    fn test_app_state() -> Arc<AppState> {
        let cfg = Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .expect("test config");
        Arc::new(AppState::for_tests(cfg))
    }

    fn local_storage_config(storage_root: &Path) -> Config {
        Config::load_from_iter([
            (
                "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                "secret".to_owned(),
            ),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
                storage_root.display().to_string(),
            ),
        ])
        .expect("test config")
    }

    fn local_storage_app() -> (tempfile::TempDir, Arc<AppState>) {
        let tmp = tempfile::tempdir().expect("temp storage root");
        let storage_root = tmp.path().canonicalize().expect("canonical storage root");
        let app = Arc::new(AppState::for_tests(local_storage_config(&storage_root)));
        (tmp, app)
    }

    async fn store_upload(app: &AppState, key: &str, data: &[u8], content_type: &str) {
        app.store
            .write_object(&app.cfg.storage.bucket_uploads, key, data, content_type)
            .await
            .expect("stored upload");
    }

    async fn sniff_upload(app: &Arc<AppState>, key: &str) -> Response {
        let body = serde_json::json!({ "type": "upload", "upload_filename": key }).to_string();
        sniff_handler(
            State(Arc::clone(app)),
            authorized_headers(),
            json_request(body),
        )
        .await
    }

    fn mpeg_ts_segment() -> Vec<u8> {
        let mut segment = vec![0u8; 564];
        for offset in [0, 188, 376] {
            segment[offset] = 0x47;
        }
        segment
    }

    fn png_followed_by_zeros() -> Vec<u8> {
        let mut upload = synthetic_png(8, 8);
        upload.extend(std::iter::repeat_n(0u8, 64 * 1024));
        upload
    }

    fn authorized_headers() -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::AUTHORIZATION,
            HeaderValue::from_static("Bearer secret"),
        );
        headers
    }

    fn json_request(body: String) -> Request<Body> {
        Request::builder()
            .method(Method::POST)
            .body(Body::from(body))
            .expect("json request")
    }

    fn oversized_request() -> Request<Body> {
        Request::builder()
            .method(Method::POST)
            .body(Body::from(Bytes::from(vec![
                0u8;
                constants::MAX_INTERNAL_REQUEST_BODY_BYTES
                    + 1
            ])))
            .expect("oversized request")
    }

    fn saturated_transform_app(storage_root: &Path) -> Arc<AppState> {
        let mut cfg = local_storage_config(storage_root);
        cfg.media.max_native_transforms = 1;
        cfg.media.worker_queue_capacity = 0;
        Arc::new(AppState::for_tests(cfg))
    }

    async fn response_body(response: Response) -> String {
        let bytes = response
            .into_body()
            .collect()
            .await
            .expect("collected body")
            .to_bytes();
        String::from_utf8(bytes.to_vec()).expect("utf8 body")
    }

    #[test]
    fn internal_auth_uses_bearer_secret() {
        let headers = authorized_headers();
        assert!(check_internal_auth(&headers, "secret"));
        assert!(!check_internal_auth(&headers, "other"));
    }

    #[tokio::test]
    async fn frames_returns_empty_frames_when_extraction_fails() {
        let body = format!(
            r#"{{"version":2,"type":"base64","base64":"{}"}}"#,
            general_purpose::STANDARD.encode(b"definitely not a video")
        );
        let response = frames_handler(
            State(test_app_state()),
            authorized_headers(),
            json_request(body),
        )
        .await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            "application/json",
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .expect("content type")
                .to_str()
                .expect("ascii content type")
        );
        assert_eq!("{\"frames\":[]}", response_body(response).await);
    }

    #[tokio::test]
    async fn metadata_reads_an_own_attachment_url_whatever_its_signature() {
        use fluxer_common::attachment_url_signature::{
            ATTACHMENT_URL_TTL_SECS, UrlKind, sign, with_signature,
        };
        use std::time::{SystemTime, UNIX_EPOCH};

        const SECRET: [u8; 32] = [5u8; 32];
        const ENDPOINT: &str = "https://media.test";
        let tmp = tempfile::tempdir().expect("temp storage root");
        let storage_root = tmp.path().canonicalize().expect("canonical storage root");
        let app = Arc::new(AppState::for_tests(
            Config::load_from_iter([
                (
                    "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                    "secret".to_owned(),
                ),
                (
                    "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
                    storage_root.display().to_string(),
                ),
                (
                    "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT".to_owned(),
                    ENDPOINT.to_owned(),
                ),
                (
                    "FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE".to_owned(),
                    "enforce".to_owned(),
                ),
                (
                    "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64".to_owned(),
                    general_purpose::STANDARD.encode(SECRET),
                ),
            ])
            .expect("metadata signature config"),
        ));
        let key = "attachments/1/2/cat.png";
        app.store
            .write_object(
                &app.cfg.storage.bucket_cdn,
                key,
                &synthetic_png(4, 4),
                "image/png",
            )
            .await
            .expect("stored attachment");
        let unsigned = format!("{ENDPOINT}/{key}");
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("the clock is after the unix epoch")
            .as_secs();
        let expires = now - 1;
        let issued = expires - ATTACHMENT_URL_TTL_SECS;
        let expired = format!(
            "{unsigned}?ex={expires:08x}&is={issued:08x}&hm={}",
            sign(key, expires, issued, UrlKind::Ordinary, &SECRET)
        );

        for spelling in [
            unsigned.clone(),
            expired,
            with_signature(&unsigned, key, now, now, &SECRET),
        ] {
            let body = serde_json::json!({
                "version": 2,
                "type": "external",
                "nsfw": "allow",
                "with_base64": true,
                "url": spelling,
            })
            .to_string();
            let response = metadata_handler(
                State(Arc::clone(&app)),
                authorized_headers(),
                json_request(body),
            )
            .await;
            assert_eq!(StatusCode::OK, response.status(), "{spelling}");
            assert!(response_body(response).await.contains("\"width\":4"));
        }
    }

    #[tokio::test]
    async fn metadata_requires_version_two_exactly() {
        let encoded = general_purpose::STANDARD.encode(synthetic_png(4, 4));
        let app = test_app_state();

        for version in ["1", "3", "null"] {
            let body = format!(
                r#"{{"version":{version},"type":"base64","nsfw":"allow","base64":"{encoded}"}}"#
            );
            let response = metadata_handler(
                State(Arc::clone(&app)),
                authorized_headers(),
                json_request(body),
            )
            .await;
            assert_eq!(
                StatusCode::BAD_REQUEST,
                response.status(),
                "version {version} must be rejected"
            );
        }

        let missing_version = format!(r#"{{"type":"base64","nsfw":"allow","base64":"{encoded}"}}"#);
        let response = metadata_handler(
            State(Arc::clone(&app)),
            authorized_headers(),
            json_request(missing_version),
        )
        .await;
        assert_eq!(StatusCode::BAD_REQUEST, response.status());

        let body =
            format!(r#"{{"version":2,"type":"base64","nsfw":"allow","base64":"{encoded}"}}"#);
        let response = metadata_handler(State(app), authorized_headers(), json_request(body)).await;
        assert_eq!(StatusCode::OK, response.status());
        assert!(response_body(response).await.contains("\"width\":4"));
    }

    #[tokio::test]
    async fn thumbnail_answers_an_oversized_body_with_413_payload_too_large() {
        let response = thumbnail_handler(
            State(test_app_state()),
            authorized_headers(),
            oversized_request(),
        )
        .await;

        assert_eq!(StatusCode::PAYLOAD_TOO_LARGE, response.status());
        assert_eq!("Payload Too Large", response_body(response).await);
    }

    #[tokio::test]
    async fn frames_answers_an_oversized_body_with_413_payload_too_large() {
        let response = frames_handler(
            State(test_app_state()),
            authorized_headers(),
            oversized_request(),
        )
        .await;

        assert_eq!(StatusCode::PAYLOAD_TOO_LARGE, response.status());
        assert_eq!("Payload Too Large", response_body(response).await);
    }

    #[tokio::test]
    async fn thumbnail_answers_a_full_admission_pool_with_504_gateway_timeout() {
        let tmp = tempfile::tempdir().expect("temp storage root");
        let storage_root = tmp.path().canonicalize().expect("canonical storage root");
        let app = saturated_transform_app(&storage_root);
        app.store
            .write_object(
                &app.cfg.storage.bucket_uploads,
                "thumb.png",
                &synthetic_png(8, 8),
                "image/png",
            )
            .await
            .expect("stored upload");
        let (release, mut released) = mpsc::channel::<()>(1);
        let (started, mut has_started) = mpsc::channel::<()>(1);
        let holder = Arc::clone(&app);
        let held = tokio::spawn(async move {
            holder
                .media
                .transforms()
                .tasks()
                .run_native(None, move || {
                    let _ = started.blocking_send(());
                    let _ = released.blocking_recv();
                    Ok(())
                })
                .await
        });
        has_started
            .recv()
            .await
            .expect("the held transform started");

        let response = thumbnail_handler(
            State(Arc::clone(&app)),
            authorized_headers(),
            json_request(r#"{"upload_filename":"thumb.png"}"#.to_owned()),
        )
        .await;

        assert_eq!(StatusCode::GATEWAY_TIMEOUT, response.status());
        assert_eq!("Gateway Timeout", response_body(response).await);
        drop(release);
        held.await.expect("held task").expect("held work");
    }

    #[tokio::test]
    async fn sniff_requires_the_internal_bearer_secret() {
        let (_tmp, app) = local_storage_app();
        store_upload(&app, "x.js", &mpeg_ts_segment(), "text/javascript").await;
        let body = r#"{"type":"upload","upload_filename":"x.js"}"#;

        let response = sniff_handler(
            State(Arc::clone(&app)),
            HeaderMap::new(),
            json_request(body.to_owned()),
        )
        .await;
        assert_eq!(StatusCode::UNAUTHORIZED, response.status());
        assert_eq!("Unauthorized", response_body(response).await);

        let mut wrong_secret = HeaderMap::new();
        wrong_secret.insert(
            header::AUTHORIZATION,
            HeaderValue::from_static("Bearer other"),
        );
        let response = sniff_handler(State(app), wrong_secret, json_request(body.to_owned())).await;
        assert_eq!(StatusCode::UNAUTHORIZED, response.status());
    }

    #[tokio::test]
    async fn sniff_reports_mpeg_ts_packets_stored_under_a_javascript_name_as_video_mp2t() {
        let (_tmp, app) = local_storage_app();
        store_upload(&app, "x.js", &mpeg_ts_segment(), "text/javascript").await;

        let response = sniff_upload(&app, "x.js").await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            Some("application/json"),
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok())
        );
        assert_eq!(
            r#"{"content_type":"video/mp2t"}"#,
            response_body(response).await
        );
    }

    #[tokio::test]
    async fn sniff_reports_null_for_javascript_svg_markup_and_pdf() {
        let (_tmp, app) = local_storage_app();
        let svg = br#"<svg xmlns="http://www.w3.org/2000/svg"></svg>"#.as_slice();
        assert_eq!("image/svg+xml", mime::sniff(svg).mime);

        for (key, data) in [
            ("a.js", b"export const a = 1;\n".as_slice()),
            ("b.tsx", svg),
            ("c.pdf", b"%PDF-1.7".as_slice()),
        ] {
            store_upload(&app, key, data, "application/octet-stream").await;
            let response = sniff_upload(&app, key).await;
            assert_eq!(StatusCode::OK, response.status(), "{key}");
            assert_eq!(
                r#"{"content_type":null}"#,
                response_body(response).await,
                "{key}"
            );
        }
    }

    #[tokio::test]
    async fn sniff_reports_null_for_an_empty_upload() {
        let (_tmp, app) = local_storage_app();
        store_upload(&app, "empty.js", b"", "text/javascript").await;

        let response = sniff_upload(&app, "empty.js").await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(r#"{"content_type":null}"#, response_body(response).await);
    }

    #[tokio::test]
    async fn sniff_reports_null_when_object_storage_answers_the_prefix_range_with_416() {
        let fake = fake_s3().await;
        fake.put_object(
            "uploads/empty.js",
            FakeObject {
                content_type: Some("text/javascript".to_owned()),
                read_status: Some(416),
                ..FakeObject::default()
            },
        );
        let tmp = tempfile::tempdir().expect("temp storage root");
        let app = Arc::new(AppState::for_tests(fake.config(tmp.path())));

        let response = sniff_upload(&app, "empty.js").await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(r#"{"content_type":null}"#, response_body(response).await);
    }

    #[tokio::test]
    async fn sniff_reports_the_prefix_verdict_for_an_upload_larger_than_the_prefix() {
        let (_tmp, app) = local_storage_app();
        store_upload(&app, "large.bin", &png_followed_by_zeros(), "text/plain").await;

        let response = sniff_upload(&app, "large.bin").await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            r#"{"content_type":"image/png"}"#,
            response_body(response).await
        );
    }

    #[tokio::test]
    async fn sniff_asks_object_storage_for_only_the_prefix_range() {
        let fake = fake_s3().await;
        fake.put_object(
            "uploads/large.bin",
            FakeObject {
                body: png_followed_by_zeros(),
                content_type: Some("text/plain".to_owned()),
                ..FakeObject::default()
            },
        );
        let tmp = tempfile::tempdir().expect("temp storage root");
        let app = Arc::new(AppState::for_tests(fake.config(tmp.path())));

        let response = sniff_upload(&app, "large.bin").await;

        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            r#"{"content_type":"image/png"}"#,
            response_body(response).await
        );
        let reads: Vec<_> = fake
            .requests()
            .into_iter()
            .filter(|(method, _, _, _)| *method == Method::GET)
            .collect();
        assert_eq!(1, reads.len());
        let (_, uri, headers, _) = &reads[0];
        assert_eq!("/uploads/large.bin", uri.path());
        assert_eq!(
            Some("bytes=0-8191"),
            headers
                .get(header::RANGE)
                .and_then(|value| value.to_str().ok())
        );
    }

    #[tokio::test]
    async fn sniff_prefix_stops_at_the_prefix_when_storage_ignores_the_range() {
        let upload = png_followed_by_zeros();
        let mut yielded = 0;
        let stream_source = upload.clone();
        let body = Body::from_stream(futures_util::stream::poll_fn(move |_| {
            assert!(
                yielded < UPLOAD_SNIFF_PREFIX_BYTES,
                "the body was polled again after {yielded} bytes were yielded"
            );
            let end = (yielded + 1000).min(stream_source.len());
            let chunk = Bytes::copy_from_slice(&stream_source[yielded..end]);
            yielded = end;
            std::task::Poll::Ready(Some(Ok::<_, std::io::Error>(chunk)))
        }));

        let prefix = read_sniff_prefix(body).await.expect("prefix read");

        assert_eq!(&upload[..UPLOAD_SNIFF_PREFIX_BYTES], prefix.as_slice());
    }

    #[tokio::test]
    async fn sniff_answers_a_missing_upload_with_404() {
        let (_tmp, app) = local_storage_app();

        let response = sniff_upload(&app, "missing.js").await;

        assert_eq!(StatusCode::NOT_FOUND, response.status());
        assert_eq!("Not Found", response_body(response).await);
    }

    #[tokio::test]
    async fn sniff_answers_a_body_without_an_upload_filename_with_400() {
        let response = sniff_handler(
            State(test_app_state()),
            authorized_headers(),
            json_request(r#"{"type":"upload"}"#.to_owned()),
        )
        .await;

        assert_eq!(StatusCode::BAD_REQUEST, response.status());
        assert_eq!("Bad Request", response_body(response).await);
    }

    #[tokio::test]
    async fn sniff_answers_an_oversized_body_with_413_payload_too_large() {
        let response = sniff_handler(
            State(test_app_state()),
            authorized_headers(),
            oversized_request(),
        )
        .await;

        assert_eq!(StatusCode::PAYLOAD_TOO_LARGE, response.status());
        assert_eq!("Payload Too Large", response_body(response).await);
    }
}
