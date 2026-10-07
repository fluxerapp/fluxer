// SPDX-License-Identifier: AGPL-3.0-or-later

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
    serve_static_text_file_with_substitutions(
        &state,
        &headers,
        "manifest.json",
        "application/manifest+json",
        state.config.static_cdn_endpoint.as_ref(),
        Some(&state.config.manifest_scope_extensions),
    )
    .await
}

fn with_scope_extensions(text: String, origins: &[String]) -> String {
    if origins.is_empty() {
        return text;
    }
    let Ok(serde_json::Value::Object(mut manifest)) = serde_json::from_str(&text) else {
        return text;
    };
    let scope_extensions = origins
        .iter()
        .map(|origin| serde_json::json!({ "type": "origin", "origin": origin }))
        .collect();
    manifest.insert(
        "scope_extensions".to_owned(),
        serde_json::Value::Array(scope_extensions),
    );
    serde_json::to_string_pretty(&manifest).unwrap_or(text)
}

pub async fn browserconfig_xml(State(state): State<AppState>, headers: HeaderMap) -> Response {
    serve_static_text_file_with_cdn(
        &state,
        &headers,
        "browserconfig.xml",
        "application/xml; charset=utf-8",
        state.config.static_cdn_endpoint.as_ref(),
    )
    .await
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
        None,
    )
    .await
}

async fn serve_static_text_file_with_substitutions(
    state: &AppState,
    headers: &HeaderMap,
    filename: &str,
    content_type: &'static str,
    static_cdn_endpoint: Option<&HttpEndpoint>,
    scope_extensions: Option<&[String]>,
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
        |text| match scope_extensions {
            Some(origins) => with_scope_extensions(text, origins),
            None => text,
        },
    )
    .await;
    apply_static_resource_security_policy(response.headers_mut(), state.csp.asset_header());
    response
}

#[cfg(test)]
mod tests {
    use super::*;

    fn substitute_placeholders(
        text: &str,
        static_cdn_endpoint: &str,
        scope_extensions: Option<&[String]>,
    ) -> String {
        let text = text.replace("{{STATIC_CDN_ENDPOINT}}", static_cdn_endpoint);
        match scope_extensions {
            Some(origins) => with_scope_extensions(text, origins),
            None => text,
        }
    }

    const BUILT_MANIFEST: &str = r#"{
  "id": "/",
  "start_url": "/app",
  "scope": "/",
  "scope_extensions": [],
  "icons": [
    {
      "src": "{{STATIC_CDN_ENDPOINT}}/web/android-chrome-192x192.png"
    }
  ]
}"#;

    fn substituted_manifest(origins: &[&str]) -> serde_json::Value {
        let origins: Vec<String> = origins.iter().map(|origin| (*origin).to_owned()).collect();
        let text =
            substitute_placeholders(BUILT_MANIFEST, "https://fluxerstatic.com", Some(&origins));
        serde_json::from_str(&text).expect("substituted manifest must stay valid JSON")
    }

    #[test]
    fn manifest_scope_extensions_default_to_the_built_empty_list() {
        let manifest = substituted_manifest(&[]);
        assert_eq!(manifest["scope_extensions"], serde_json::json!([]));
        assert_eq!(
            manifest["icons"][0]["src"],
            "https://fluxerstatic.com/web/android-chrome-192x192.png"
        );
    }

    #[test]
    fn manifest_scope_extensions_list_each_configured_origin() {
        let manifest = substituted_manifest(&["https://fluxer.com", "https://canary.fluxer.com"]);
        assert_eq!(
            manifest["scope_extensions"],
            serde_json::json!([
                { "type": "origin", "origin": "https://fluxer.com" },
                { "type": "origin", "origin": "https://canary.fluxer.com" }
            ])
        );
        assert_eq!(manifest["id"], "/");
        assert_eq!(manifest["start_url"], "/app");
    }

    #[test]
    fn manifest_scope_extensions_are_added_when_the_build_has_none() {
        let text = substitute_placeholders(
            r#"{"id":"/"}"#,
            "",
            Some(&["https://fluxer.com".to_owned()]),
        );
        let manifest: serde_json::Value = serde_json::from_str(&text).expect("valid JSON");
        assert_eq!(
            manifest["scope_extensions"],
            serde_json::json!([{ "type": "origin", "origin": "https://fluxer.com" }])
        );
    }

    #[test]
    fn other_text_files_are_left_alone() {
        let origins = ["https://fluxer.com".to_owned()];
        assert_eq!(
            substitute_placeholders(
                "not json {{STATIC_CDN_ENDPOINT}}",
                "https://cdn",
                Some(&origins)
            ),
            "not json https://cdn"
        );
        assert_eq!(
            substitute_placeholders(BUILT_MANIFEST, "", None),
            BUILT_MANIFEST.replace("{{STATIC_CDN_ENDPOINT}}", "")
        );
    }
}
