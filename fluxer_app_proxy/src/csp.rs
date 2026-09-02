// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, CspConfig, CspSource, HttpEndpoint};
use axum::http::HeaderValue;
use axum::http::header::InvalidHeaderValue;
use rand::RngExt;

const CSP_NONCE_HEX_DIGITS: usize = 32;
const CSP_VALIDATION_NONCE: &str = "00000000000000000000000000000000";

const _: () = assert!(
    CSP_VALIDATION_NONCE.len() == CSP_NONCE_HEX_DIGITS,
    "the nonce a policy is validated with must be shaped like the nonce a request carries"
);

#[derive(Clone, Debug, Default)]
pub struct RuntimeCspSources {
    pub static_cdn_endpoint: Option<HttpEndpoint>,
    pub media_endpoint: Option<HttpEndpoint>,
    pub s3_public_endpoint: Option<HttpEndpoint>,
    pub s3_uploads_endpoint: Option<HttpEndpoint>,
    pub branding_image_origins: Vec<HttpEndpoint>,
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
];

const WORKER_SOURCES: &[&str] = &["https://*.fluxer.app", "blob:"];

const MANIFEST_SOURCES: &[&str] = &["https://*.fluxer.app"];

#[derive(Debug)]
pub enum CspCompileError {
    InvalidAssetPolicy(InvalidHeaderValue),
    InvalidSpaPolicy(InvalidHeaderValue),
}

impl std::fmt::Display for CspCompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidAssetPolicy(_) => {
                formatter.write_str("the asset content security policy is not a valid header value")
            }
            Self::InvalidSpaPolicy(_) => {
                formatter.write_str("the SPA content security policy is not a valid header value")
            }
        }
    }
}

impl std::error::Error for CspCompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidAssetPolicy(source) | Self::InvalidSpaPolicy(source) => Some(source),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompiledCspPolicy {
    config: CspConfig,
    asset: HeaderValue,
}

impl CompiledCspPolicy {
    pub fn from_config(config: &AppProxyConfig) -> Result<Self, CspCompileError> {
        Self::compile(
            config.csp.clone(),
            &RuntimeCspSources {
                static_cdn_endpoint: config.static_cdn_endpoint.clone(),
                media_endpoint: None,
                s3_public_endpoint: config.s3_public_endpoint.clone(),
                s3_uploads_endpoint: config.s3_uploads_endpoint.clone(),
                branding_image_origins: Vec::new(),
            },
        )
    }

    pub fn compile(
        config: CspConfig,
        configured_sources: &RuntimeCspSources,
    ) -> Result<Self, CspCompileError> {
        let asset_sources = RuntimeCspSources {
            static_cdn_endpoint: configured_sources.static_cdn_endpoint.clone(),
            ..RuntimeCspSources::default()
        };
        let asset = HeaderValue::from_str(&build_asset_csp(&config, &asset_sources))
            .map_err(CspCompileError::InvalidAssetPolicy)?;
        HeaderValue::from_str(&build_csp(
            &config,
            CSP_VALIDATION_NONCE,
            configured_sources,
        ))
        .map_err(CspCompileError::InvalidSpaPolicy)?;
        Ok(Self { config, asset })
    }

    pub fn asset_header(&self) -> HeaderValue {
        self.asset.clone()
    }

    pub fn spa_header(&self, nonce: &str, runtime_sources: &RuntimeCspSources) -> HeaderValue {
        assert!(
            nonce.len() == CSP_NONCE_HEX_DIGITS
                && nonce.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "a CSP nonce must be a 128-bit hexadecimal value"
        );
        HeaderValue::from_str(&build_csp(&self.config, nonce, runtime_sources)).expect(
            "every CSP source is a validated keyword, scheme, or ASCII origin, so a policy built \
             from them is always a valid header value",
        )
    }
}

pub fn generate_nonce() -> String {
    let bytes: [u8; 16] = rand::rng().random();
    hex::encode(bytes)
}

fn build_csp(config: &CspConfig, nonce: &str, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(config, Some(nonce), runtime_sources).join("; ")
}

fn build_asset_csp(config: &CspConfig, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(config, None, runtime_sources).join("; ")
}

fn build_csp_directives(
    config: &CspConfig,
    nonce: Option<&str>,
    runtime_sources: &RuntimeCspSources,
) -> Vec<String> {
    let mut directives = Vec::with_capacity(14);

    let mut default = vec!["'self'".to_owned()];
    extend_from(&mut default, &config.extra_default_src, &[]);
    directives.push(format!("default-src {}", default.join(" ")));

    let mut script = vec![
        "'self'".to_owned(),
        "'wasm-unsafe-eval'".to_owned(),
        "blob:".to_owned(),
    ];
    if let Some(n) = nonce {
        script.insert(1, format!("'nonce-{n}'"));
    }
    extend_from(&mut script, &config.extra_script_src, SCRIPT_SOURCES);
    extend_runtime_sources(&mut script, runtime_sources, true, false);
    directives.push(format!("script-src {}", script.join(" ")));

    let mut style = vec!["'self'".to_owned(), "'unsafe-inline'".to_owned()];
    extend_from(&mut style, &config.extra_style_src, STYLE_SOURCES);
    extend_runtime_sources(&mut style, runtime_sources, true, true);
    directives.push(format!("style-src {}", style.join(" ")));

    let mut img = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
    extend_from(&mut img, &config.extra_img_src, IMAGE_SOURCES);
    extend_runtime_sources(&mut img, runtime_sources, true, true);
    for origin in &runtime_sources.branding_image_origins {
        push_endpoint_source(&mut img, Some(origin));
    }
    directives.push(format!("img-src {}", img.join(" ")));

    let mut media = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut media, &config.extra_media_src, MEDIA_SOURCES);
    extend_runtime_sources(&mut media, runtime_sources, true, true);
    directives.push(format!("media-src {}", media.join(" ")));

    let mut font = vec!["'self'".to_owned(), "data:".to_owned()];
    extend_from(&mut font, &config.extra_font_src, FONT_SOURCES);
    extend_runtime_sources(&mut font, runtime_sources, true, true);
    directives.push(format!("font-src {}", font.join(" ")));

    let mut connect = vec!["'self'".to_owned(), "data:".to_owned()];
    extend_from(&mut connect, &config.extra_connect_src, CONNECT_SOURCES);
    extend_runtime_sources(&mut connect, runtime_sources, true, true);
    extend_runtime_s3_sources(&mut connect, runtime_sources);
    directives.push(format!("connect-src {}", connect.join(" ")));

    let mut frame = vec!["'self'".to_owned()];
    extend_from(&mut frame, &config.extra_frame_src, FRAME_SOURCES);
    directives.push(format!("frame-src {}", frame.join(" ")));

    let mut worker = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut worker, &config.extra_worker_src, WORKER_SOURCES);
    extend_runtime_sources(&mut worker, runtime_sources, true, false);
    directives.push(format!("worker-src {}", worker.join(" ")));

    let mut manifest = vec!["'self'".to_owned()];
    extend_from(&mut manifest, &config.extra_manifest_src, MANIFEST_SOURCES);
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
        push_endpoint_source(target, runtime_sources.static_cdn_endpoint.as_ref());
    }
    if include_media {
        push_endpoint_source(target, runtime_sources.media_endpoint.as_ref());
    }
}

fn push_endpoint_source(target: &mut Vec<String>, endpoint: Option<&HttpEndpoint>) {
    let Some(endpoint) = endpoint else {
        return;
    };
    let source = endpoint.csp_origin();
    if target.iter().any(|existing| existing == source) {
        return;
    }
    target.push(source.to_owned());
}

fn extend_runtime_s3_sources(target: &mut Vec<String>, runtime_sources: &RuntimeCspSources) {
    push_endpoint_source(target, runtime_sources.s3_public_endpoint.as_ref());
    push_endpoint_source(target, runtime_sources.s3_uploads_endpoint.as_ref());
}

fn extend_from(target: &mut Vec<String>, extra: &[CspSource], defaults: &[&str]) {
    for source in defaults {
        if target.iter().any(|existing| existing == source) {
            continue;
        }
        target.push((*source).to_owned());
    }

    for source in extra {
        let source = source.as_str();
        if target.iter().any(|existing| existing == source) {
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
        assert_eq!(nonce.len(), CSP_NONCE_HEX_DIGITS);
        assert!(nonce.chars().all(|c| c.is_ascii_hexdigit()));
        CompiledCspPolicy::compile(default_csp_config(), &runtime_sources())
            .unwrap()
            .spa_header(&nonce, &runtime_sources());
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

    fn endpoint(value: &str) -> HttpEndpoint {
        HttpEndpoint::parse("TEST_ENDPOINT", value).unwrap()
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
            report_uri: Some(
                crate::config::CspReportUri::parse(
                    "TEST_CSP_REPORT_URI",
                    "https://example.com/csp-report",
                )
                .unwrap(),
            ),
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
            static_cdn_endpoint: Some(endpoint("https://static.example.test/")),
            media_endpoint: Some(endpoint("https://media.example.test")),
            ..Default::default()
        };
        let csp = build_csp(&config, "nonce1", &runtime_sources);
        assert!(csp.contains("style-src 'self' 'unsafe-inline'"));
        assert!(csp.contains("https://static.example.test"));
        assert!(csp.contains("https://media.example.test"));
        assert!(!csp.contains("https://static.example.test/ "));
    }

    #[test]
    fn a_csp_source_cannot_smuggle_a_second_directive() {
        for injected in [
            "https://evil.test; script-src *",
            "https://evil.test,https://other.test",
            "https://evil.test https://other.test",
            "https://evil.test\nscript-src *",
        ] {
            assert!(
                CspSource::parse("TEST_CSP_SOURCE", injected).is_err(),
                "{injected:?} must not parse as a single CSP source"
            );
        }
    }

    #[test]
    fn a_report_uri_cannot_smuggle_a_second_directive() {
        assert!(
            crate::config::CspReportUri::parse(
                "TEST_CSP_REPORT_URI",
                "https://evil.test/r; script-src *"
            )
            .is_err()
        );
    }

    #[test]
    fn build_csp_includes_s3_public_and_virtual_hosted_upload_origins() {
        let config = default_csp_config();
        let runtime_sources = RuntimeCspSources {
            s3_public_endpoint: Some(endpoint("http://localhost:3900/")),
            s3_uploads_endpoint: Some(endpoint("http://fluxer-uploads.localhost:3900/")),
            ..Default::default()
        };

        let csp = build_csp(&config, "nonce1", &runtime_sources);

        assert!(csp.contains("http://localhost:3900"));
        assert!(csp.contains("http://fluxer-uploads.localhost:3900"));
        assert!(!csp.contains("http://localhost:3900/ "));
    }

    #[test]
    fn a_compiled_asset_header_is_the_policy_every_asset_response_reuses() {
        let sources = RuntimeCspSources {
            static_cdn_endpoint: Some(endpoint("https://static.example.test/")),
            media_endpoint: Some(endpoint("https://media.example.test")),
            s3_public_endpoint: Some(endpoint("http://localhost:3900/")),
            ..Default::default()
        };
        let policy = CompiledCspPolicy::compile(default_csp_config(), &sources).unwrap();

        assert_eq!(policy.asset_header(), policy.asset_header());
        let asset = policy.asset_header();
        let asset = asset.to_str().unwrap();
        assert!(!asset.contains("nonce-"));
        assert!(asset.contains("https://static.example.test"));
        assert!(
            !asset.contains("https://media.example.test"),
            "an asset response must not widen the policy with the endpoints only the document needs"
        );
        assert!(!asset.contains("http://localhost:3900"));
    }

    #[test]
    fn a_compiled_policy_stamps_the_requests_own_nonce_and_discovery_endpoints() {
        let policy = CompiledCspPolicy::compile(default_csp_config(), &runtime_sources()).unwrap();
        let discovered = RuntimeCspSources {
            static_cdn_endpoint: Some(endpoint("https://cdn.discovered.test")),
            branding_image_origins: vec![endpoint("https://branding.discovered.test")],
            ..Default::default()
        };

        let header = policy.spa_header("0123456789abcdef0123456789abcdef", &discovered);
        let header = header.to_str().unwrap();

        assert!(header.contains("'nonce-0123456789abcdef0123456789abcdef'"));
        assert!(header.contains("https://cdn.discovered.test"));
        assert!(header.contains("https://branding.discovered.test"));
    }

    #[test]
    fn a_compiled_policy_matches_the_directives_it_was_compiled_from() {
        let config = default_csp_config();
        let sources = RuntimeCspSources {
            static_cdn_endpoint: Some(endpoint("https://static.example.test/")),
            ..Default::default()
        };
        let policy = CompiledCspPolicy::compile(config.clone(), &sources).unwrap();

        assert_eq!(
            policy.asset_header().to_str().unwrap(),
            build_asset_csp(&config, &sources)
        );
        assert_eq!(
            policy
                .spa_header(CSP_VALIDATION_NONCE, &sources)
                .to_str()
                .unwrap(),
            build_csp(&config, CSP_VALIDATION_NONCE, &sources)
        );
    }

    #[test]
    #[should_panic(expected = "a CSP nonce must be a 128-bit hexadecimal value")]
    fn a_compiled_policy_refuses_a_nonce_it_did_not_generate() {
        let policy = CompiledCspPolicy::compile(default_csp_config(), &runtime_sources()).unwrap();
        policy.spa_header("not-a-nonce", &runtime_sources());
    }
}
