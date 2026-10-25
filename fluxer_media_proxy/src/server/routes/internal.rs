// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::BudgetedBytes,
    constants,
    image_quality::ImageQuality,
    image_transform::ResizeMode,
    media_process, mime,
    output_format::OutputFormat,
    server::{
        format_policy::is_svg_content_type,
        media_operations::{
            MediaFailure, MediaInput, MediaInputLimit, MetadataOutput, STORED_SNIFF_PREFIX_BYTES,
            StrippedMedia, load_media_input, probe_audio_duration, read_sniff_prefix,
            resolve_metadata, strip_media,
        },
        response::{
            GeneratedMediaResponse,
            error::{
                canonical_reason_str, json_response, storage_error_response, text, text_with_source,
            },
            generated_media_response,
        },
        state::AppState,
        transform::execution::{
            VideoTransformOptions, run_transform, run_video_transform, transform_error_is_timeout,
        },
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
use serde::Deserialize;
use std::sync::Arc;

const MEDIA_WIDTH_HEADER: &str = "x-fluxer-media-width";
const MEDIA_HEIGHT_HEADER: &str = "x-fluxer-media-height";

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
struct MediaSourceRequest {
    #[serde(rename = "type")]
    typ: String,
    base64: Option<String>,
    upload_filename: Option<String>,
    filename: Option<String>,
    bucket: Option<String>,
    key: Option<String>,
    content_type: Option<String>,
}

impl MediaSourceRequest {
    fn into_parts(self) -> (MetadataRequest, Option<String>) {
        let request = MetadataRequest {
            version: None,
            typ: self.typ,
            nsfw: "allow".to_owned(),
            base64: self.base64,
            upload_filename: self.upload_filename,
            filename: self.filename,
            bucket: self.bucket,
            key: self.key,
            url: None,
            with_base64: None,
        };
        (request, self.content_type)
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
        let options = VideoTransformOptions {
            format: OutputFormat::WebP,
            width: None,
            height: None,
            quality: ImageQuality::Auto,
            deadline_ms: app.media.transforms().transform_deadline_ms(),
        };
        match run_video_transform(app.media.transforms(), object.data.clone(), options).await {
            Ok(media) => media,
            Err(err) if transform_error_is_timeout(&err) => {
                return text_with_source(
                    StatusCode::GATEWAY_TIMEOUT,
                    "Gateway Timeout",
                    "video_thumbnail_timeout",
                    err,
                );
            }
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
    generated_media_response(GeneratedMediaResponse {
        method: Method::GET,
        data: BudgetedBytes::from(Bytes::from(media.bytes)),
        content_type: media.content_type,
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
    let options = VideoTransformOptions {
        format: OutputFormat::JPEG,
        width: None,
        height: None,
        quality: ImageQuality::Auto,
        deadline_ms: app.media.transforms().transform_deadline_ms(),
    };
    match run_video_transform(app.media.transforms(), input.data, options).await {
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
    let range = format!("bytes=0-{}", STORED_SNIFF_PREFIX_BYTES - 1);
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

async fn read_media_source(
    app: &AppState,
    headers: &HeaderMap,
    request: Request<Body>,
) -> Result<(MediaInput, Option<String>), Box<Response>> {
    if !check_internal_auth(headers, app.cfg.secret_key.expose()) {
        return Err(Box::new(text(StatusCode::UNAUTHORIZED, "Unauthorized")));
    }
    let body = read_limited_body(request)
        .await
        .map_err(|status| Box::new(text(status, canonical_reason_str(status))))?;
    let req: MediaSourceRequest = serde_json::from_slice(&body)
        .map_err(|_| Box::new(text(StatusCode::BAD_REQUEST, "Bad Request")))?;
    let (request, content_type) = req.into_parts();
    let input = request
        .into_media_input()
        .map_err(|failure| Box::new(failure.into_response()))?;
    Ok((input, content_type))
}

pub(in crate::server) async fn strip_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    let (input, content_type) = match read_media_source(&app, &headers, request).await {
        Ok(source) => source,
        Err(response) => return *response,
    };
    let StrippedMedia {
        data,
        content_type,
        dimensions,
    } = match strip_media(&app, input, content_type).await {
        Ok(stripped) => stripped,
        Err(failure) => return failure.into_response(),
    };
    let mut builder = Response::builder()
        .status(StatusCode::OK)
        .header(header::CONTENT_TYPE, content_type)
        .header(header::CONTENT_LENGTH, data.len());
    if let Some((width, height)) = dimensions {
        builder = builder
            .header(MEDIA_WIDTH_HEADER, width)
            .header(MEDIA_HEIGHT_HEADER, height);
    }
    builder
        .body(Body::from(data))
        .unwrap_or_else(|_| text(StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error"))
}

pub(in crate::server) async fn audio_duration_handler(
    State(app): State<Arc<AppState>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    let (input, _) = match read_media_source(&app, &headers, request).await {
        Ok(source) => source,
        Err(response) => return *response,
    };
    match probe_audio_duration(&app, input).await {
        Ok(duration) => json_response(
            StatusCode::OK,
            serde_json::json!({ "duration_seconds": duration }).to_string(),
        ),
        Err(failure) => failure.into_response(),
    }
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
    use crate::config::Config;
    use axum::http::HeaderValue;
    use http_body_util::BodyExt as _;
    use std::path::Path;

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

    fn mpeg_ts_segment() -> Vec<u8> {
        let mut segment = vec![0u8; 564];
        for offset in [0, 188, 376] {
            segment[offset] = 0x47;
        }
        segment
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

    async fn response_body(response: Response) -> String {
        let bytes = response
            .into_body()
            .collect()
            .await
            .expect("collected body")
            .to_bytes();
        String::from_utf8(bytes.to_vec()).expect("utf8 body")
    }

    #[tokio::test]
    async fn own_url_metadata_reports_the_served_dimensions() {
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
                    "https://chat.example.test/media".to_owned(),
                ),
            ])
            .expect("test config"),
        ));
        let cdn = &app.cfg.storage.bucket_cdn;
        let emoji = crate::test_fixtures::synthetic_image(128, 128, ".webp[strip]");
        for (key, data, content_type) in [
            ("emojis/1", emoji.clone(), "image/webp"),
            (
                "emojis/2",
                crate::test_fixtures::synthetic_image(128, 256, ".webp[strip,page-height=128]"),
                "image/webp",
            ),
            (
                "attachments/1/2/cat.png",
                crate::test_fixtures::synthetic_png(128, 96),
                "image/png",
            ),
        ] {
            app.store
                .write_object(cdn, key, &data, content_type)
                .await
                .expect("stored object");
        }
        let metadata = async |path: &str, with_base64: bool| {
            let body = serde_json::json!({
                "type": "external",
                "url": format!("https://chat.example.test/media/{path}"),
                "version": 2,
                "nsfw": "allow",
                "with_base64": with_base64,
            });
            let response = metadata_handler(
                State(Arc::clone(&app)),
                authorized_headers(),
                json_request(body.to_string()),
            )
            .await;
            assert_eq!(StatusCode::OK, response.status(), "{path}");
            serde_json::from_str::<serde_json::Value>(&response_body(response).await)
                .expect("metadata json")
        };
        for (path, width, height, content_type, animated) in [
            ("emojis/1.webp?size=48", 48, 48, "image/webp", false),
            ("emojis/1.webp?size=50", 56, 56, "image/webp", false),
            ("emojis/1.webp?size=4096", 128, 128, "image/webp", false),
            ("emojis/1.png?size=48", 48, 48, "image/png", false),
            ("emojis/2.webp?size=48", 48, 48, "image/webp", true),
            ("attachments/1/2/cat.png", 128, 96, "image/png", false),
            (
                "attachments/1/2/cat.png?width=64",
                64,
                48,
                "image/png",
                false,
            ),
            (
                "attachments/1/2/cat.png?width=64&ex=0&is=0&hm=00&uc=dp",
                64,
                48,
                "image/png",
                false,
            ),
            (
                "attachments/1/2/cat.png?width=nope",
                128,
                96,
                "image/png",
                false,
            ),
        ] {
            let metadata = metadata(path, false).await;
            assert_eq!(
                (
                    Some(width),
                    Some(height),
                    Some(content_type),
                    Some(animated)
                ),
                (
                    metadata["width"].as_u64(),
                    metadata["height"].as_u64(),
                    metadata["content_type"].as_str(),
                    metadata["animated"].as_bool(),
                ),
                "{path}"
            );
        }
        let metadata = metadata("emojis/1.webp?size=48", true).await;
        assert_eq!(
            (Some(128), Some(128)),
            (metadata["width"].as_u64(), metadata["height"].as_u64())
        );
        assert_eq!(
            Some(general_purpose::STANDARD.encode(&emoji).as_str()),
            metadata["base64"].as_str()
        );
    }

    #[test]
    fn internal_auth_uses_bearer_secret() {
        let headers = authorized_headers();
        assert!(check_internal_auth(&headers, "secret"));
        assert!(!check_internal_auth(&headers, "other"));
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
}
