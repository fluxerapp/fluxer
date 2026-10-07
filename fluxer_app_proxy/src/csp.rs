// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, CspConfig, CspSource, HttpEndpoint};
use axum::http::HeaderValue;
use axum::http::header::InvalidHeaderValue;
use base64::{Engine as _, engine::general_purpose::STANDARD};
use sha2::{Digest, Sha256};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InlineScriptHash(String);

impl InlineScriptHash {
    pub fn of(script_text: &str) -> Self {
        Self(format!(
            "'sha256-{}'",
            STANDARD.encode(Sha256::digest(script_text.as_bytes()))
        ))
    }

    pub fn as_source(&self) -> &str {
        &self.0
    }
}

pub fn inline_script_hashes(document: &str) -> Vec<InlineScriptHash> {
    let mut hashes: Vec<InlineScriptHash> = Vec::new();
    let mut rest = document;
    while let Some((attributes, text, after)) = next_script_element(rest) {
        rest = after;
        if has_src_attribute(attributes) {
            continue;
        }
        let hash = InlineScriptHash::of(text);
        if !hashes.contains(&hash) {
            hashes.push(hash);
        }
    }
    hashes
}

fn next_script_element(html: &str) -> Option<(&str, &str, &str)> {
    let mut offset = 0;
    loop {
        let start = offset + find_ignoring_ascii_case(&html[offset..], "<script")?;
        let name_end = start + "<script".len();
        let boundary = *html.as_bytes().get(name_end)?;
        if boundary != b'>' && boundary != b'/' && !boundary.is_ascii_whitespace() {
            offset = name_end;
            continue;
        }
        let tag_end = name_end + html[name_end..].find('>')?;
        let text_start = tag_end + 1;
        let text_end = text_start + find_ignoring_ascii_case(&html[text_start..], "</script")?;
        let close_end = html[text_end..]
            .find('>')
            .map_or(html.len(), |index| text_end + index + 1);
        return Some((
            &html[name_end..tag_end],
            &html[text_start..text_end],
            &html[close_end..],
        ));
    }
}

fn find_ignoring_ascii_case(haystack: &str, needle: &str) -> Option<usize> {
    haystack
        .as_bytes()
        .windows(needle.len())
        .position(|window| window.eq_ignore_ascii_case(needle.as_bytes()))
}

fn has_src_attribute(attributes: &str) -> bool {
    attributes
        .split(|c: char| c.is_ascii_whitespace() || c == '/')
        .any(|token| {
            token
                .split('=')
                .next()
                .is_some_and(|name| name.eq_ignore_ascii_case("src"))
        })
}

#[derive(Clone, Debug, Default)]
pub struct RuntimeCspSources {
    pub static_cdn_endpoint: Option<HttpEndpoint>,
    pub media_endpoint: Option<HttpEndpoint>,
    pub s3_public_endpoint: Option<HttpEndpoint>,
    pub s3_uploads_endpoint: Option<HttpEndpoint>,
}

const FRAME_SOURCES: &[&str] = &[
    "https://www.youtube.com/embed/",
    "https://www.youtube.com/s/player/",
];

const IMAGE_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://fluxer.app",
    "https://i.ytimg.com",
    "https://*.youtube.com",
    "https://*.fluxer.media",
    "https://fluxer.media",
];

const MEDIA_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
    "https://fluxer.app",
    "https://*.youtube.com",
    "https://*.fluxer.media",
    "https://fluxer.media",
];

const SCRIPT_SOURCES: &[&str] = &["https://*.fluxer.app"];

const STYLE_SOURCES: &[&str] = &[
    "https://*.fluxer.app",
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
    runtime_sources: RuntimeCspSources,
    asset: HeaderValue,
}

#[derive(Clone, Debug)]
pub struct CspHeaderValues {
    pub enforced: HeaderValue,
    pub report_only: Option<HeaderValue>,
}

impl CompiledCspPolicy {
    pub fn from_config(config: &AppProxyConfig) -> Result<Self, CspCompileError> {
        Self::compile(
            config.csp.clone(),
            &RuntimeCspSources {
                static_cdn_endpoint: config.static_cdn_endpoint.clone(),
                media_endpoint: config.media_endpoint.clone(),
                s3_public_endpoint: config.s3_public_endpoint.clone(),
                s3_uploads_endpoint: config.s3_uploads_endpoint.clone(),
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
            &[InlineScriptHash::of("")],
            configured_sources,
        ))
        .map_err(CspCompileError::InvalidSpaPolicy)?;
        Ok(Self {
            config,
            runtime_sources: configured_sources.clone(),
            asset,
        })
    }

    pub fn asset_header(&self) -> HeaderValue {
        self.asset.clone()
    }

    pub fn spa_headers(&self, script_hashes: &[InlineScriptHash]) -> CspHeaderValues {
        let enforced = HeaderValue::from_str(&build_csp(
            &self.config,
            script_hashes,
            &self.runtime_sources,
        ))
        .expect(
            "every CSP source is a validated keyword, scheme, ASCII origin, or base64 hash, so a \
             policy built from them is always a valid header value",
        );
        CspHeaderValues {
            enforced,
            report_only: None,
        }
    }
}

fn build_csp(
    config: &CspConfig,
    script_hashes: &[InlineScriptHash],
    runtime_sources: &RuntimeCspSources,
) -> String {
    build_csp_directives(config, Some(script_hashes), runtime_sources).join("; ")
}

fn build_asset_csp(config: &CspConfig, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(config, None, runtime_sources).join("; ")
}

fn build_csp_directives(
    config: &CspConfig,
    script_hashes: Option<&[InlineScriptHash]>,
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
    if let Some(hashes) = script_hashes {
        script.splice(1..1, hashes.iter().map(|hash| hash.as_source().to_owned()));
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
    directives.push(format!("img-src {}", img.join(" ")));

    let mut media = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut media, &config.extra_media_src, MEDIA_SOURCES);
    extend_runtime_sources(&mut media, runtime_sources, true, true);
    directives.push(format!("media-src {}", media.join(" ")));

    let mut font = vec!["'self'".to_owned(), "data:".to_owned()];
    extend_from(&mut font, &config.extra_font_src, FONT_SOURCES);
    extend_runtime_sources(&mut font, runtime_sources, true, true);
    directives.push(format!("font-src {}", font.join(" ")));

    let mut connect = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
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
    for source in defaults
        .iter()
        .copied()
        .chain(extra.iter().map(CspSource::as_str))
    {
        if target.iter().any(|existing| existing == source) {
            continue;
        }
        target.push(source.to_owned());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hash_of(text: &str) -> InlineScriptHash {
        InlineScriptHash::of(text)
    }

    #[test]
    fn an_inline_script_hash_is_the_base64_sha256_of_the_exact_script_text() {
        assert_eq!(
            hash_of("alert('Hello, world.');").as_source(),
            "'sha256-qznLcsROx4GACP2dm0UCKCzCG+HiZ1guq6ZZDob/Tng='"
        );
        assert_eq!(
            hash_of("").as_source(),
            "'sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU='"
        );
    }

    #[test]
    fn every_inline_script_is_hashed_and_external_scripts_are_not() {
        let document = concat!(
            "<head><script>first()</script>",
            "<SCRIPT type=\"text/javascript\">second()</SCRIPT >",
            "<script type=\"module\" src=\"/assets/app.js\"></script>",
            "<script defer src='/assets/vendor.js'></script>",
            "<scripts>not a script</scripts>",
            "<script data-src=\"x\">\nthird()\n</script></head>",
        );

        assert_eq!(
            inline_script_hashes(document),
            vec![
                hash_of("first()"),
                hash_of("second()"),
                hash_of("\nthird()\n")
            ]
        );
    }

    #[test]
    fn an_inline_script_repeated_verbatim_is_granted_once() {
        let document = "<script>same()</script><script>same()</script>";
        assert_eq!(inline_script_hashes(document), vec![hash_of("same()")]);
    }

    #[test]
    fn an_unterminated_script_is_never_granted() {
        assert!(inline_script_hashes("<script>never_closed()").is_empty());
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
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
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
    fn build_csp_allows_the_apex_domain_for_images_and_media() {
        let config = default_csp_config();
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
        for name in ["img-src ", "media-src "] {
            let directive = csp
                .split("; ")
                .find(|directive| directive.starts_with(name))
                .expect("directive");
            assert!(
                directive
                    .split(' ')
                    .any(|source| source == "https://fluxer.app"),
                "{name}must allow the apex domain: {directive}"
            );
        }
    }

    #[test]
    fn build_csp_allows_blob_connections_for_camera_background_media() {
        let config = default_csp_config();
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
        let connect = csp
            .split("; ")
            .find(|directive| directive.starts_with("connect-src "))
            .expect("connect-src directive");
        assert!(
            connect.split(' ').any(|source| source == "blob:"),
            "connect-src must allow blob: object URLs: {connect}"
        );
    }

    #[test]
    fn an_asset_header_allows_blob_connections_for_the_camera_effect_worker() {
        let policy = CompiledCspPolicy::compile(default_csp_config(), &runtime_sources()).unwrap();
        let asset = policy.asset_header();
        let asset = asset.to_str().unwrap();
        let connect = asset
            .split("; ")
            .find(|directive| directive.starts_with("connect-src "))
            .expect("connect-src directive");
        assert!(
            connect.split(' ').any(|source| source == "blob:"),
            "the camera-effect worker is served as /assets/*.worker.js and runs under the asset \
             policy, so that policy must allow blob: object URLs: {connect}"
        );
    }

    #[test]
    fn build_csp_grants_each_script_hash_in_script_src_and_no_nonce() {
        let config = default_csp_config();
        let csp = build_csp(
            &config,
            &[hash_of("first()"), hash_of("second()")],
            &runtime_sources(),
        );
        let script = csp
            .split("; ")
            .find(|directive| directive.starts_with("script-src "))
            .expect("script-src directive");
        assert!(script.starts_with(&format!(
            "script-src 'self' {} {} 'wasm-unsafe-eval' blob:",
            hash_of("first()").as_source(),
            hash_of("second()").as_source()
        )));
        assert!(!csp.contains("nonce-"));
    }

    #[test]
    fn build_asset_csp_grants_no_inline_script() {
        let config = default_csp_config();
        let csp = build_asset_csp(&config, &runtime_sources());
        assert!(!csp.contains("nonce-"));
        assert!(!csp.contains("sha256-"));
    }

    #[test]
    fn build_csp_allows_no_third_party_captcha_hosts() {
        let csp = build_csp(
            &default_csp_config(),
            &[hash_of("boot()")],
            &runtime_sources(),
        );
        assert!(!csp.contains("hcaptcha"));
        assert!(!csp.contains("challenges.cloudflare.com"));
    }

    #[test]
    fn csp_no_double_spaces_or_trailing_semicolons() {
        let config = default_csp_config();
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
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
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
        assert!(csp.contains("report-uri https://example.com/csp-report"));
    }

    #[test]
    fn build_csp_excludes_report_uri_when_none() {
        let config = default_csp_config();
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources());
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
        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources);
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

        let csp = build_csp(&config, &[hash_of("boot()")], &runtime_sources);

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
        assert!(!asset.contains("sha256-"));
        assert!(asset.contains("https://static.example.test"));
        assert!(
            !asset.contains("https://media.example.test"),
            "an asset response must not widen the policy with the endpoints only the document needs"
        );
        assert!(!asset.contains("http://localhost:3900"));
    }

    #[test]
    fn a_compiled_policy_stamps_the_documents_script_hashes_and_configured_endpoints() {
        let configured = RuntimeCspSources {
            static_cdn_endpoint: Some(endpoint("https://cdn.configured.test")),
            ..Default::default()
        };
        let policy = CompiledCspPolicy::compile(default_csp_config(), &configured).unwrap();

        let headers = policy.spa_headers(&[hash_of("boot()")]);
        let header = headers.enforced.to_str().unwrap();

        assert!(header.contains(hash_of("boot()").as_source()));
        assert!(header.contains("https://cdn.configured.test"));
        assert!(!header.contains("nonce-"));
        assert!(headers.report_only.is_none());
    }

    #[test]
    fn the_configured_media_endpoint_reaches_every_directive_that_loads_media() {
        let mut config = AppProxyConfig::from_env();
        config.csp = default_csp_config();
        config.media_endpoint = Some(endpoint("https://media.configured.test/"));
        let policy = CompiledCspPolicy::from_config(&config).unwrap();

        let headers = policy.spa_headers(&[hash_of("boot()")]);
        let header = headers.enforced.to_str().unwrap();
        for directive in [
            "img-src",
            "media-src",
            "font-src",
            "style-src",
            "connect-src",
        ] {
            let sources = header
                .split("; ")
                .find(|candidate| candidate.starts_with(directive))
                .unwrap_or_else(|| panic!("the policy has no {directive}"));
            assert!(
                sources
                    .split(' ')
                    .any(|source| source == "https://media.configured.test"),
                "{directive} does not grant the configured media endpoint"
            );
        }
        for directive in ["script-src", "worker-src", "manifest-src"] {
            let sources = header
                .split("; ")
                .find(|candidate| candidate.starts_with(directive))
                .unwrap_or_else(|| panic!("the policy has no {directive}"));
            assert!(!sources.contains("https://media.configured.test"));
        }
        assert!(
            !policy
                .asset_header()
                .to_str()
                .unwrap()
                .contains("https://media.configured.test")
        );
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
        let hashes = [hash_of("boot()")];
        assert_eq!(
            policy.spa_headers(&hashes).enforced.to_str().unwrap(),
            build_csp(&config, &hashes, &sources)
        );
    }
}
