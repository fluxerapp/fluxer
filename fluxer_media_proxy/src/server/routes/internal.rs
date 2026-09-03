// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::BudgetedBytes,
    constants,
    image_transform::ResizeMode,
    media_process, mime,
    output_format::OutputFormat,
    server::{
        media_operations::{
            MediaFailure, MediaInput, MediaInputLimit, MetadataOutput, load_media_input,
            resolve_metadata,
        },
        response::{
            MediaResponse,
            error::{
                canonical_reason_str, json_response, storage_error_response, text,
                text_with_reason, text_with_source,
            },
            media_response,
        },
        state::AppState,
        transform::execution::run_transform,
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
        Err(status) => return text(status, "Bad Request"),
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
            ..Default::default()
        };
        match run_transform(app.media.transforms(), object.data.clone(), options).await {
            Ok(media) => media,
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
        Err(_) => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let req: FramesRequest = match serde_json::from_slice::<FramesRequest>(&body) {
        Ok(req) if req.version.is_none_or(|version| version == 2) => req,
        _ => return text(StatusCode::BAD_REQUEST, "Bad Request"),
    };
    let input = match req.into_metadata_request().into_media_input() {
        Ok(input) => input,
        Err(failure) => return frames_input_failure(&failure),
    };
    let input = match load_media_input(&app, input, MediaInputLimit::INTERNAL_REQUEST).await {
        Ok(input) => input,
        Err(failure) => return frames_input_failure(&failure),
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

fn frames_input_failure(failure: &MediaFailure) -> Response {
    text_with_reason(StatusCode::BAD_REQUEST, "Bad Request", failure.code())
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
    use crate::{config::Config, test_fixtures::synthetic_png};
    use axum::http::HeaderValue;
    use http_body_util::BodyExt as _;

    fn test_app_state() -> Arc<AppState> {
        let cfg = Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .expect("test config");
        Arc::new(AppState::for_tests(cfg))
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
}
