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
    let branding = state.config.self_hosted.then(|| state.branding.snapshot());
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
    let location = match state.branding.snapshot().favicon_url.as_deref() {
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
            Some(origins) => finish_manifest(text, origins, None),
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

    const GENERATED_MANIFEST: &str = r##"{
  "name": "Fluxer",
  "short_name": "Fluxer",
  "description": "Fluxer is a free and open source instant messaging and VoIP platform built for friends, groups, and communities.",
  "id": "/",
  "start_url": "/app",
  "display": "standalone",
  "orientation": "portrait-primary",
  "theme_color": "#4641D9",
  "background_color": "#2b2d31",
  "categories": ["social", "communication"],
  "lang": "en",
  "scope": "/",
  "scope_extensions": [],
  "icons": [
    {
      "src": "{{STATIC_CDN_ENDPOINT}}/web/android-chrome-192x192.png",
      "sizes": "192x192",
      "type": "image/png",
      "purpose": "maskable any"
    },
    {
      "src": "{{STATIC_CDN_ENDPOINT}}/web/android-chrome-512x512.png",
      "sizes": "512x512",
      "type": "image/png",
      "purpose": "maskable any"
    }
  ]
}"##;

    fn branded_manifest(branding: &BrandingSnapshot) -> serde_json::Value {
        let text = finish_manifest(
            GENERATED_MANIFEST.replace("{{STATIC_CDN_ENDPOINT}}", ""),
            &[],
            Some(branding),
        );
        serde_json::from_str(&text).expect("a branded manifest must stay valid JSON")
    }

    fn assert_installable_shape(manifest: &serde_json::Value) {
        for key in ["name", "short_name", "id", "start_url", "scope", "display"] {
            assert!(
                manifest[key]
                    .as_str()
                    .is_some_and(|value| !value.is_empty()),
                "the manifest lost its {key}"
            );
        }
        let icons = manifest["icons"]
            .as_array()
            .expect("the manifest lost its icons");
        assert!(icons.iter().any(|icon| {
            icon["src"].as_str().is_some_and(|src| !src.is_empty())
                && icon["purpose"]
                    .as_str()
                    .is_some_and(|purpose| purpose.split(' ').any(|purpose| purpose == "any"))
        }));
    }

    #[test]
    fn a_branded_product_name_stays_a_json_string() {
        let product_name = "A \"quoted\" \\ name</script>\n\u{2028}";
        let manifest = branded_manifest(&BrandingSnapshot {
            product_name: Some(product_name.into()),
            ..BrandingSnapshot::default()
        });

        assert_eq!(manifest["name"], product_name);
        assert_eq!(manifest["short_name"], product_name);
    }

    #[test]
    fn the_official_manifest_is_left_byte_identical() {
        let text =
            GENERATED_MANIFEST.replace("{{STATIC_CDN_ENDPOINT}}", "https://fluxerstatic.com");
        assert_eq!(finish_manifest(text.clone(), &[], None), text);
    }

    fn static_dir_with_generated_files() -> std::path::PathBuf {
        static NEXT_DIR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let unique = NEXT_DIR.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root =
            std::env::temp_dir().join(format!("fluxer-spa-static-{}-{unique}", std::process::id()));
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("manifest.json"), GENERATED_MANIFEST).unwrap();
        std::fs::write(root.join("browserconfig.xml"), GENERATED_BROWSERCONFIG).unwrap();
        root
    }

    fn state_serving(root: &std::path::Path, self_hosted: bool) -> AppState {
        let mut config = crate::config::AppProxyConfig::from_env();
        config.static_dir = root.to_str().unwrap().to_owned();
        config.static_cdn_endpoint = None;
        config.manifest_scope_extensions = Vec::new();
        config.self_hosted = self_hosted;
        config.branding = BrandingSnapshot {
            product_name: Some("Example".into()),
            icon_url: Some("https://example.test/icon.png".into()),
            favicon_url: Some("https://example.test/favicon.png".into()),
            theme_color: Some("#7b2ce4".into()),
            ..BrandingSnapshot::default()
        };
        let csp = std::sync::Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        let budgets = crate::state::AppProxyBudgets::default();
        let local_files = crate::local_files::LocalFileStore::load_blocking(root, &budgets)
            .expect("the test static directory must load");
        AppState {
            branding: crate::branding_images::InstanceBranding::from_config(&config),
            config: std::sync::Arc::new(config),
            csp,
            http_client: reqwest::Client::new(),
            spa_index_source: crate::state::SpaIndexSource::bundled(""),
            local_asset_prefixes: None,
            budgets,
            local_files,
        }
    }

    async fn fetch(state: AppState, path: &str) -> Response {
        use tower::ServiceExt as _;
        crate::routes::build_router(state)
            .oneshot(
                axum::http::Request::builder()
                    .uri(path)
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap()
    }

    async fn body_of(response: Response) -> String {
        let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        String::from_utf8(bytes.to_vec()).unwrap()
    }

    #[tokio::test]
    async fn the_served_manifest_is_branded_only_on_a_self_hosted_instance() {
        let root = static_dir_with_generated_files();

        let official = fetch(state_serving(&root, false), "/manifest.json").await;
        assert_eq!(official.status(), StatusCode::OK);
        assert_eq!(
            body_of(official).await,
            GENERATED_MANIFEST.replace("{{STATIC_CDN_ENDPOINT}}", "")
        );

        let self_hosted = fetch(state_serving(&root, true), "/manifest.json").await;
        assert_eq!(self_hosted.status(), StatusCode::OK);
        let manifest: serde_json::Value =
            serde_json::from_str(&body_of(self_hosted).await).unwrap();
        assert_eq!(manifest["name"], "Example");
        assert_eq!(manifest["icons"][0]["src"], "https://example.test/icon.png");
        assert_installable_shape(&manifest);

        std::fs::remove_dir_all(root).unwrap();
    }

    #[tokio::test]
    async fn every_response_asks_search_engines_not_to_index_it() {
        let root = static_dir_with_generated_files();
        std::fs::write(root.join("robots.txt"), "User-agent: *\nAllow: /\n").unwrap();

        for path in [
            "/",
            "/login",
            "/channels/@me",
            "/invite/abc",
            "/gift/abc",
            "/robots.txt",
            "/manifest.json",
            "/version.json",
            "/_health",
            "/web/missing.png",
        ] {
            let response = fetch(state_serving(&root, false), path).await;
            assert_eq!(
                response
                    .headers()
                    .get("x-robots-tag")
                    .and_then(|value| value.to_str().ok()),
                Some("noindex, nofollow"),
                "{path} answered {} without a noindex directive",
                response.status()
            );
        }

        std::fs::remove_dir_all(root).unwrap();
    }

    const GENERATED_BROWSERCONFIG: &str = r#"<?xml version="1.0" encoding="utf-8"?>
<browserconfig>
  <msapplication>
    <tile>
      <square150x150logo src="{{STATIC_CDN_ENDPOINT}}/web/mstile-150x150.png"/>
      <TileColor>#4641D9</TileColor>
    </tile>
  </msapplication>
</browserconfig>"#;

    #[tokio::test]
    async fn the_fluxer_tile_is_served_only_on_the_official_instance() {
        let root = static_dir_with_generated_files();

        let official = fetch(state_serving(&root, false), "/browserconfig.xml").await;
        assert_eq!(official.status(), StatusCode::OK);
        assert_eq!(
            body_of(official).await,
            GENERATED_BROWSERCONFIG.replace("{{STATIC_CDN_ENDPOINT}}", "")
        );

        let self_hosted = fetch(state_serving(&root, true), "/browserconfig.xml").await;
        assert_eq!(self_hosted.status(), StatusCode::NOT_FOUND);

        std::fs::remove_dir_all(root).unwrap();
    }
}
