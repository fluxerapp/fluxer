// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::CspConfig;
use rand::RngExt;
use reqwest::Url;

#[derive(Clone, Debug, Default)]
pub struct RuntimeCspSources {
    pub static_cdn_endpoint: Option<String>,
    pub media_endpoint: Option<String>,
    pub s3_public_endpoint: Option<String>,
    pub s3_uploads_bucket: Option<String>,
    pub branding_image_origins: Vec<String>,
}

const FRAME_SOURCES: &[&str] = &[
    "https://www.youtube.com/embed/",
    "https://www.youtube.com/s/player/",
    "https://hcaptcha.com",
    "https://*.hcaptcha.com",
    "https://challenges.cloudflare.com",
];

const IMAGE_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://i.ytimg.com",
    "https://*.youtube.com",
    "https://*.fluxer.media",
    "https://fluxer.media",
];

const MEDIA_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://*.youtube.com",
    "https://*.fluxer.media",
    "https://fluxer.media",
];

const SCRIPT_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://hcaptcha.com",
    "https://*.hcaptcha.com",
    "https://challenges.cloudflare.com",
];

const STYLE_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://hcaptcha.com",
    "https://*.hcaptcha.com",
    "https://fonts.googleapis.com",
    "https://api.fonts.coollabs.io",
];

const FONT_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://fonts.gstatic.com",
    "https://api.fonts.coollabs.io",
];

const CONNECT_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "wss://*.fluxer.app",
    "https://*.fluxer.media",
    "wss://*.fluxer.media",
    "https://fluxer-uploads.ewr1.vultrobjects.com",
    "https://hcaptcha.com",
    "https://*.hcaptcha.com",
    "https://challenges.cloudflare.com",
    "https://fluxerstatus.com",
    "https://fluxer.media",
    "http://127.0.0.1:21863",
    "http://127.0.0.1:21864",
];

const WORKER_SOURCES: &[&str] = &["https://*.fluxer.app", "blob:"];

const MANIFEST_SOURCES: &[&str] = &["https://*.fluxer.app"];

pub fn generate_nonce() -> String {
    let bytes: [u8; 16] = rand::rng().random();
    hex::encode(bytes)
}

pub fn build_csp(config: &CspConfig, nonce: &str, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(config, Some(nonce), runtime_sources).join("; ")
}

pub fn build_asset_csp(config: &CspConfig, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(config, None, runtime_sources).join("; ")
}

fn build_csp_directives(
    config: &CspConfig,
    nonce: Option<&str>,
    runtime_sources: &RuntimeCspSources,
) -> Vec<String> {
    let mut directives = Vec::with_capacity(14);

    let mut default = vec!["'self'".to_owned()];
    extend_from(&mut default, config.extra_default_src.as_deref(), &[]);
    directives.push(format!("default-src {}", default.join(" ")));

    let mut script = vec![
        "'self'".to_owned(),
        "'wasm-unsafe-eval'".to_owned(),
        "blob:".to_owned(),
    ];
    if let Some(n) = nonce {
        script.insert(1, format!("'nonce-{n}'"));
    }
    extend_from(
        &mut script,
        config.extra_script_src.as_deref(),
        SCRIPT_SOURCES,
    );
    extend_runtime_sources(&mut script, runtime_sources, true, false);
    directives.push(format!("script-src {}", script.join(" ")));

    let mut style = vec!["'self'".to_owned(), "'unsafe-inline'".to_owned()];
    extend_from(&mut style, config.extra_style_src.as_deref(), STYLE_SOURCES);
    extend_runtime_sources(&mut style, runtime_sources, true, true);
    directives.push(format!("style-src {}", style.join(" ")));

    let mut img = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
    extend_from(&mut img, config.extra_img_src.as_deref(), IMAGE_SOURCES);
    extend_runtime_sources(&mut img, runtime_sources, true, true);
    for origin in &runtime_sources.branding_image_origins {
        push_endpoint_source(&mut img, Some(origin));
    }
    directives.push(format!("img-src {}", img.join(" ")));

    let mut media = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut media, config.extra_media_src.as_deref(), MEDIA_SOURCES);
    extend_runtime_sources(&mut media, runtime_sources, true, true);
    directives.push(format!("media-src {}", media.join(" ")));

    let mut font = vec!["'self'".to_owned(), "data:".to_owned()];
    extend_from(&mut font, config.extra_font_src.as_deref(), FONT_SOURCES);
    extend_runtime_sources(&mut font, runtime_sources, true, true);
    directives.push(format!("font-src {}", font.join(" ")));

    let mut connect = vec!["'self'".to_owned(), "data:".to_owned()];
    extend_from(
        &mut connect,
        config.extra_connect_src.as_deref(),
        CONNECT_SOURCES,
    );
    extend_runtime_sources(&mut connect, runtime_sources, true, true);
    extend_runtime_s3_sources(&mut connect, runtime_sources);
    directives.push(format!("connect-src {}", connect.join(" ")));

    let mut frame = vec!["'self'".to_owned()];
    extend_from(&mut frame, config.extra_frame_src.as_deref(), FRAME_SOURCES);
    directives.push(format!("frame-src {}", frame.join(" ")));

    let mut worker = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(
        &mut worker,
        config.extra_worker_src.as_deref(),
        WORKER_SOURCES,
    );
    extend_runtime_sources(&mut worker, runtime_sources, true, false);
    directives.push(format!("worker-src {}", worker.join(" ")));

    let mut manifest = vec!["'self'".to_owned()];
    extend_from(
        &mut manifest,
        config.extra_manifest_src.as_deref(),
        MANIFEST_SOURCES,
    );
    extend_runtime_sources(&mut manifest, runtime_sources, true, false);
    directives.push(format!("manifest-src {}", manifest.join(" ")));

    directives.push("object-src 'none'".to_owned());
    directives.push("base-uri 'self'".to_owned());
    directives.push("frame-ancestors 'none'".to_owned());

    if let Some(report_uri) = &config.report_uri {
        directives.push(format!("report-uri {report_uri}"));
    }

    directives
}

fn extend_runtime_sources(
    target: &mut Vec<String>,
    runtime_sources: &RuntimeCspSources,
    include_static: bool,
    include_media: bool,
) {
    if include_static {
        push_endpoint_source(target, runtime_sources.static_cdn_endpoint.as_deref());
    }
    if include_media {
        push_endpoint_source(target, runtime_sources.media_endpoint.as_deref());
    }
}

pub fn http_origin(raw: &str) -> Option<String> {
    let url = Url::parse(raw.trim()).ok()?;
    let scheme = url.scheme();
    if scheme != "http" && scheme != "https" {
        return None;
    }
    let host = url.host_str()?;
    let port = url
        .port()
        .map(|port| format!(":{port}"))
        .unwrap_or_default();
    Some(format!("{scheme}://{host}{port}"))
}

fn push_endpoint_source(target: &mut Vec<String>, endpoint: Option<&str>) {
    let Some(endpoint) = endpoint else {
        return;
    };
    let source = endpoint.trim().trim_end_matches('/');
    if source.is_empty() || target.iter().any(|existing| existing == source) {
        return;
    }
    target.push(source.to_owned());
}

fn extend_runtime_s3_sources(target: &mut Vec<String>, runtime_sources: &RuntimeCspSources) {
    push_endpoint_source(target, runtime_sources.s3_public_endpoint.as_deref());

    let Some(source) = s3_uploads_bucket_origin(runtime_sources) else {
        return;
    };
    push_endpoint_source(target, Some(&source));
}

fn s3_uploads_bucket_origin(runtime_sources: &RuntimeCspSources) -> Option<String> {
    let bucket = runtime_sources
        .s3_uploads_bucket
        .as_deref()
        .map(str::trim)
        .filter(|value| !value.is_empty())?;
    let endpoint = runtime_sources.s3_public_endpoint.as_deref()?.trim();
    let url = Url::parse(endpoint).ok()?;
    let scheme = url.scheme();
    if scheme != "http" && scheme != "https" {
        return None;
    }
    let host = url.host_str()?;
    let host = if host.starts_with(&format!("{bucket}.")) {
        host.to_owned()
    } else {
        format!("{bucket}.{host}")
    };
    let port = url
        .port()
        .map(|port| format!(":{port}"))
        .unwrap_or_default();
    Some(format!("{scheme}://{host}{port}"))
}

fn extend_from(target: &mut Vec<String>, extra: Option<&[String]>, defaults: &[&str]) {
    target.extend(defaults.iter().map(|s| (*s).to_owned()));

    for source in extra.into_iter().flatten() {
        let source = source.trim();
        if source.is_empty() || target.iter().any(|existing| existing == source) {
            continue;
        }
        target.push(source.to_owned());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_nonce_produces_32_char_hex() {
        let nonce = generate_nonce();
        assert_eq!(nonce.len(), 32);
        assert!(nonce.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn generate_nonce_is_random() {
        let a = generate_nonce();
        let b = generate_nonce();
        assert_ne!(a, b);
    }

    fn default_csp_config() -> CspConfig {
        CspConfig::default()
    }

    fn runtime_sources() -> RuntimeCspSources {
        RuntimeCspSources::default()
    }

    #[test]
    fn build_csp_includes_required_directives() {
        let config = default_csp_config();
        let csp = build_csp(&config, "testnonce", &runtime_sources());
        assert!(csp.contains("default-src"));
        assert!(csp.contains("script-src"));
        assert!(csp.contains("style-src"));
        assert!(csp.contains("img-src"));
        assert!(csp.contains("media-src"));
        assert!(csp.contains("font-src"));
        assert!(csp.contains("connect-src"));
        assert!(csp.contains("frame-src"));
        assert!(csp.contains("worker-src"));
        assert!(csp.contains("manifest-src"));
        assert!(csp.contains("object-src 'none'"));
        assert!(csp.contains("base-uri 'self'"));
        assert!(csp.contains("frame-ancestors 'none'"));
    }

    #[test]
    fn build_csp_includes_nonce_in_script_src() {
        let config = default_csp_config();
        let csp = build_csp(&config, "abc123def456", &runtime_sources());
        assert!(csp.contains("'nonce-abc123def456'"));
    }

    #[test]
    fn build_asset_csp_excludes_nonce() {
        let config = default_csp_config();
        let csp = build_asset_csp(&config, &runtime_sources());
        assert!(!csp.contains("nonce-"));
    }

    #[test]
    fn csp_no_double_spaces_or_trailing_semicolons() {
        let config = default_csp_config();
        let csp = build_csp(&config, "nonce1", &runtime_sources());
        assert!(!csp.contains("  "), "CSP contains double spaces");
        assert!(!csp.ends_with(';'), "CSP ends with semicolon");
        assert!(!csp.ends_with("; "), "CSP ends with semicolon+space");
    }

    #[test]
    fn build_csp_includes_report_uri_when_configured() {
        let config = CspConfig {
            report_uri: Some("https://example.com/csp-report".to_owned()),
            ..Default::default()
        };
        let csp = build_csp(&config, "nonce1", &runtime_sources());
        assert!(csp.contains("report-uri https://example.com/csp-report"));
    }

    #[test]
    fn build_csp_excludes_report_uri_when_none() {
        let config = default_csp_config();
        let csp = build_csp(&config, "nonce1", &runtime_sources());
        assert!(!csp.contains("report-uri"));
    }

    #[test]
    fn build_csp_includes_configured_runtime_endpoints() {
        let config = default_csp_config();
        let runtime_sources = RuntimeCspSources {
            static_cdn_endpoint: Some("https://static.example.test/".to_owned()),
            media_endpoint: Some("https://media.example.test".to_owned()),
            ..Default::default()
        };
        let csp = build_csp(&config, "nonce1", &runtime_sources);
        assert!(csp.contains("style-src 'self' 'unsafe-inline'"));
        assert!(csp.contains("https://static.example.test"));
        assert!(csp.contains("https://media.example.test"));
        assert!(!csp.contains("https://static.example.test/ "));
    }

    #[test]
    fn build_csp_includes_s3_public_and_virtual_hosted_upload_origins() {
        let config = default_csp_config();
        let runtime_sources = RuntimeCspSources {
            s3_public_endpoint: Some("http://localhost:3900/".to_owned()),
            s3_uploads_bucket: Some("fluxer-uploads".to_owned()),
            ..Default::default()
        };

        let csp = build_csp(&config, "nonce1", &runtime_sources);

        assert!(csp.contains("http://localhost:3900"));
        assert!(csp.contains("http://fluxer-uploads.localhost:3900"));
        assert!(!csp.contains("http://localhost:3900/ "));
    }
}
