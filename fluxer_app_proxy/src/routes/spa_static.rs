// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::branding_images::BrandingSnapshot;
use crate::config::HttpEndpoint;
use crate::local_files::{LocalFileResponseOptions, serve_text_file};
use crate::state::{AppState, MAX_STATIC_TEXT_FILE_BYTES};
use crate::static_asset_policy::apply_static_resource_security_policy;
use axum::{
    extract::State,
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};

const STATIC_TEXT_CACHE_CONTROL: &str = "no-cache";

pub async fn version_json(State(state): State<AppState>, headers: HeaderMap) -> Response {
    let mut result =
        serve_static_text_file(&state, &headers, "version.json", "application/json").await;

    if result.status() == StatusCode::NOT_FOUND && !state.config.build_version.is_empty() {
        let body = serde_json::json!({ "version": state.config.build_version });
        result = axum::Json(body).into_response();
        result
            .headers_mut()
            .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
    }

    result
}

pub async fn manifest_json(State(state): State<AppState>, headers: HeaderMap) -> Response {
    let branding = state.config.self_hosted.then(|| state.branding_snapshot());
    serve_static_text_file_with_substitutions(
        &state,
        &headers,
        "manifest.json",
        "application/manifest+json",
        state.config.static_cdn_endpoint.as_ref(),
        |text| {
            finish_manifest(
                text,
                &state.config.manifest_scope_extensions,
                branding.as_deref(),
            )
        },
    )
    .await
}

fn finish_manifest(
    text: String,
    scope_extension_origins: &[String],
    branding: Option<&BrandingSnapshot>,
) -> String {
    if scope_extension_origins.is_empty() && branding.is_none() {
        return text;
    }
    let Ok(serde_json::Value::Object(mut manifest)) = serde_json::from_str(&text) else {
        return text;
    };
    if !scope_extension_origins.is_empty() {
        let scope_extensions = scope_extension_origins
            .iter()
            .map(|origin| serde_json::json!({ "type": "origin", "origin": origin }))
            .collect();
        manifest.insert(
            "scope_extensions".to_owned(),
            serde_json::Value::Array(scope_extensions),
        );
    }
    if let Some(branding) = branding {
        apply_manifest_branding(&mut manifest, branding);
    }
    serde_json::to_string_pretty(&manifest).unwrap_or(text)
}

fn apply_manifest_branding(
    manifest: &mut serde_json::Map<String, serde_json::Value>,
    branding: &BrandingSnapshot,
) {
    manifest.remove("description");
    if let Some(product_name) = &branding.product_name {
        for key in ["name", "short_name"] {
            manifest.insert(key.to_owned(), serde_json::json!(&**product_name));
        }
    }
    if let Some(theme_color) = &branding.theme_color {
        manifest.insert("theme_color".to_owned(), serde_json::json!(&**theme_color));
    }
    if let Some(icon_url) = &branding.icon_url {
        manifest.insert(
            "icons".to_owned(),
            serde_json::json!([{ "src": &**icon_url, "sizes": "any", "purpose": "any" }]),
        );
    }
}

pub async fn browserconfig_xml(State(state): State<AppState>, headers: HeaderMap) -> Response {
    if state.config.self_hosted {
        return StatusCode::NOT_FOUND.into_response();
    }
    serve_static_text_file_with_cdn(
        &state,
        &headers,
        "browserconfig.xml",
        "application/xml; charset=utf-8",
        state.config.static_cdn_endpoint.as_ref(),
    )
    .await
}

pub async fn favicon_ico(State(state): State<AppState>) -> Response {
    let location = match state.branding_snapshot().favicon_url.as_deref() {
        Some(favicon_url) => favicon_url.to_owned(),
        None => format!(
            "{}/web/favicon.ico",
            state
                .config
                .static_cdn_endpoint
                .as_ref()
                .map_or("", HttpEndpoint::as_str)
                .trim_end_matches('/')
        ),
    };
    match HeaderValue::from_str(&location) {
        Ok(location) => (StatusCode::FOUND, [(header::LOCATION, location)]).into_response(),
        Err(_) => StatusCode::NOT_FOUND.into_response(),
    }
}

pub async fn service_worker(State(state): State<AppState>, headers: HeaderMap) -> Response {
    serve_static_text_file(
        &state,
        &headers,
        "sw.js",
        "application/javascript; charset=utf-8",
    )
    .await
}

pub async fn service_worker_map(State(state): State<AppState>, headers: HeaderMap) -> Response {
    serve_static_text_file(&state, &headers, "sw.js.map", "application/json").await
}

async fn serve_static_text_file(
    state: &AppState,
    headers: &HeaderMap,
    filename: &str,
    content_type: &'static str,
) -> Response {
    serve_static_text_file_with_cdn(state, headers, filename, content_type, None).await
}

async fn serve_static_text_file_with_cdn(
    state: &AppState,
    headers: &HeaderMap,
    filename: &str,
    content_type: &'static str,
    static_cdn_endpoint: Option<&HttpEndpoint>,
) -> Response {
    serve_static_text_file_with_substitutions(
        state,
        headers,
        filename,
        content_type,
        static_cdn_endpoint,
        |text| text,
    )
    .await
}

async fn serve_static_text_file_with_substitutions(
    state: &AppState,
    headers: &HeaderMap,
    filename: &str,
    content_type: &'static str,
    static_cdn_endpoint: Option<&HttpEndpoint>,
    finish: impl FnOnce(String) -> String,
) -> Response {
    let replacement = static_cdn_endpoint.map_or("", HttpEndpoint::as_str);
    let mut response = serve_text_file(
        &state.local_files,
        filename,
        headers,
        LocalFileResponseOptions {
            content_type,
            cache_control: STATIC_TEXT_CACHE_CONTROL,
            allow_cross_origin: false,
            max_bytes: MAX_STATIC_TEXT_FILE_BYTES,
        },
        &[("{{STATIC_CDN_ENDPOINT}}", replacement)],
        finish,
    )
    .await;
    apply_static_resource_security_policy(response.headers_mut(), state.csp.asset_header());
    response
}
