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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InstanceCspSources {
    pub static_cdn_endpoint: Option<HttpEndpoint>,
    pub media_endpoint: Option<HttpEndpoint>,
    pub connect_origins: Vec<String>,
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
    "https://cdn.fonts.coollabs.io",
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

#[derive(Debug, thiserror::Error)]
pub enum CspCompileError {
    #[error("the asset content security policy is not a valid header value")]
    InvalidAssetPolicy(#[source] InvalidHeaderValue),
    #[error("the SPA content security policy is not a valid header value")]
    InvalidSpaPolicy(#[source] InvalidHeaderValue),
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
            &[],
            &InstanceCspSources::default(),
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

    pub fn spa_headers(
        &self,
        script_hashes: &[InlineScriptHash],
        branding_image_origins: &[HttpEndpoint],
        instance_sources: &InstanceCspSources,
    ) -> CspHeaderValues {
        let enforced = HeaderValue::from_str(&build_csp(
            &self.config,
            script_hashes,
            &self.runtime_sources,
            branding_image_origins,
            instance_sources,
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
    branding_image_origins: &[HttpEndpoint],
    instance_sources: &InstanceCspSources,
) -> String {
    build_csp_directives(
        config,
        Some(script_hashes),
        runtime_sources,
        branding_image_origins,
        instance_sources,
    )
    .join("; ")
}

fn build_asset_csp(config: &CspConfig, runtime_sources: &RuntimeCspSources) -> String {
    build_csp_directives(
        config,
        None,
        runtime_sources,
        &[],
        &InstanceCspSources::default(),
    )
    .join("; ")
}

fn build_csp_directives(
    config: &CspConfig,
    script_hashes: Option<&[InlineScriptHash]>,
    runtime_sources: &RuntimeCspSources,
    branding_image_origins: &[HttpEndpoint],
    instance_sources: &InstanceCspSources,
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
    extend_runtime_sources(&mut script, runtime_sources, instance_sources, true, false);
    directives.push(format!("script-src {}", script.join(" ")));

    let mut style = vec!["'self'".to_owned(), "'unsafe-inline'".to_owned()];
    extend_from(&mut style, &config.extra_style_src, STYLE_SOURCES);
    extend_runtime_sources(&mut style, runtime_sources, instance_sources, true, false);
    directives.push(format!("style-src {}", style.join(" ")));

    let mut img = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
    extend_from(&mut img, &config.extra_img_src, IMAGE_SOURCES);
    extend_runtime_sources(&mut img, runtime_sources, instance_sources, true, true);
    for origin in branding_image_origins {
        push_endpoint_source(&mut img, Some(origin));
    }
    directives.push(format!("img-src {}", img.join(" ")));

    let mut media = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut media, &config.extra_media_src, MEDIA_SOURCES);
    extend_runtime_sources(&mut media, runtime_sources, instance_sources, true, true);
    directives.push(format!("media-src {}", media.join(" ")));

    let mut font = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
    extend_from(&mut font, &config.extra_font_src, FONT_SOURCES);
    extend_runtime_sources(&mut font, runtime_sources, instance_sources, true, true);
    directives.push(format!("font-src {}", font.join(" ")));

    let mut connect = vec!["'self'".to_owned(), "blob:".to_owned(), "data:".to_owned()];
    extend_from(&mut connect, &config.extra_connect_src, CONNECT_SOURCES);
    extend_from(&mut connect, &config.voice_connect_src, &[]);
    for source in &config.extra_connect_src {
        if let Some(twin) = http_twin_of_websocket_source(source.as_str())
            && !connect.contains(&twin)
        {
            connect.push(twin);
        }
    }
    extend_runtime_sources(&mut connect, runtime_sources, instance_sources, true, true);
    extend_runtime_s3_sources(&mut connect, runtime_sources);
    for origin in &instance_sources.connect_origins {
        if !connect.contains(origin) {
            connect.push(origin.clone());
        }
    }
    directives.push(format!("connect-src {}", connect.join(" ")));

    let mut frame = vec!["'self'".to_owned()];
    extend_from(&mut frame, &config.extra_frame_src, FRAME_SOURCES);
    directives.push(format!("frame-src {}", frame.join(" ")));

    let mut worker = vec!["'self'".to_owned(), "blob:".to_owned()];
    extend_from(&mut worker, &config.extra_worker_src, WORKER_SOURCES);
    extend_runtime_sources(&mut worker, runtime_sources, instance_sources, true, false);
    directives.push(format!("worker-src {}", worker.join(" ")));

    let mut manifest = vec!["'self'".to_owned()];
    extend_from(&mut manifest, &config.extra_manifest_src, MANIFEST_SOURCES);
    extend_runtime_sources(
        &mut manifest,
        runtime_sources,
        instance_sources,
        true,
        false,
    );
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
    instance_sources: &InstanceCspSources,
    include_static: bool,
    include_media: bool,
) {
    if include_static {
        push_endpoint_source(target, runtime_sources.static_cdn_endpoint.as_ref());
        push_endpoint_source(target, instance_sources.static_cdn_endpoint.as_ref());
    }
    if include_media {
        push_endpoint_source(target, runtime_sources.media_endpoint.as_ref());
        push_endpoint_source(target, instance_sources.media_endpoint.as_ref());
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

fn http_twin_of_websocket_source(source: &str) -> Option<String> {
    if let Some(rest) = source.strip_prefix("wss://") {
        return Some(format!("https://{rest}"));
    }
    source
        .strip_prefix("ws://")
        .map(|rest| format!("http://{rest}"))
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
    fn build_asset_csp_grants_no_inline_script() {
        let config = default_csp_config();
        let csp = build_asset_csp(&config, &runtime_sources());
        assert!(!csp.contains("nonce-"));
        assert!(!csp.contains("sha256-"));
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
    fn branding_image_origins_are_granted_to_images_and_nothing_else() {
        let policy = CompiledCspPolicy::compile(default_csp_config(), &runtime_sources()).unwrap();
        let branding = [
            endpoint("https://example.test"),
            endpoint("http://assets.example.test:8443"),
        ];

        let headers = policy.spa_headers(
            &[hash_of("boot()")],
            &branding,
            &InstanceCspSources::default(),
        );
        let header = headers.enforced.to_str().unwrap();

        for directive in header.split("; ") {
            let grants_branding = directive
                .split(' ')
                .any(|source| branding.iter().any(|origin| origin.csp_origin() == source));
            assert_eq!(
                grants_branding,
                directive.starts_with("img-src "),
                "unexpected branding grant in: {directive}"
            );
        }
        let img = header
            .split("; ")
            .find(|directive| directive.starts_with("img-src "))
            .unwrap();
        assert!(img.ends_with(" https://example.test http://assets.example.test:8443"));
        assert!(
            !policy
                .asset_header()
                .to_str()
                .unwrap()
                .contains("example.test")
        );
    }
}
