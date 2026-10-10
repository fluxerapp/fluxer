// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config as cfg;
use reqwest::Url;
use std::env;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InvalidAppProxyEnvironmentError {
    InvalidValue {
        name: &'static str,
        value: String,
        expected: &'static str,
    },
}

impl InvalidAppProxyEnvironmentError {
    fn new(name: &'static str, value: &str, expected: &'static str) -> Self {
        Self::InvalidValue {
            name,
            value: value.to_owned(),
            expected,
        }
    }
}

impl fmt::Display for InvalidAppProxyEnvironmentError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidValue {
                name,
                value,
                expected,
            } => write!(formatter, "{name} must be {expected}, got {value:?}"),
        }
    }
}

impl std::error::Error for InvalidAppProxyEnvironmentError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HttpUrl(Url);

impl HttpUrl {
    pub fn parse(name: &'static str, value: &str) -> Result<Self, InvalidAppProxyEnvironmentError> {
        let url = Url::parse(value.trim()).map_err(|_| {
            InvalidAppProxyEnvironmentError::new(name, value, "a valid HTTP or HTTPS URL")
        })?;
        if !matches!(url.scheme(), "http" | "https")
            || url.host_str().is_none()
            || !url.username().is_empty()
            || url.password().is_some()
            || url.fragment().is_some()
        {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                value,
                "an HTTP or HTTPS URL with a host and no credentials or fragment",
            ));
        }
        Ok(Self(url))
    }

    pub fn as_url(&self) -> &Url {
        &self.0
    }
}

impl fmt::Display for HttpUrl {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HttpEndpoint {
    url: Url,
    csp_origin: String,
}

impl HttpEndpoint {
    pub fn parse(name: &'static str, value: &str) -> Result<Self, InvalidAppProxyEnvironmentError> {
        let mut url = HttpUrl::parse(name, value)?.0;
        if url.query().is_some() {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                value,
                "an HTTP or HTTPS endpoint without a query or fragment",
            ));
        }
        if !url.path().ends_with('/') {
            let mut path = url.path().to_owned();
            path.push('/');
            url.set_path(&path);
        }
        let csp_origin = url.origin().ascii_serialization();
        if csp_origin == "null" {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                value,
                "an HTTP or HTTPS endpoint with a tuple origin",
            ));
        }
        Ok(Self { url, csp_origin })
    }

    pub fn with_host_prefix(
        &self,
        name: &'static str,
        prefix: &str,
    ) -> Result<Self, InvalidAppProxyEnvironmentError> {
        if !is_dns_bucket_name(prefix) {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                prefix,
                "a DNS-compatible bucket name",
            ));
        }
        let host = self
            .url
            .host_str()
            .expect("validated HTTP endpoint must have a host");
        let prefixed_host = if host.starts_with(&format!("{prefix}.")) {
            host.to_owned()
        } else {
            format!("{prefix}.{host}")
        };
        let mut url = self.url.clone();
        url.set_host(Some(&prefixed_host)).map_err(|_| {
            InvalidAppProxyEnvironmentError::new(name, prefix, "a DNS-compatible bucket name")
        })?;
        let csp_origin = url.origin().ascii_serialization();
        Ok(Self { url, csp_origin })
    }

    pub fn as_url(&self) -> &Url {
        &self.url
    }

    pub fn as_str(&self) -> &str {
        self.url.as_str().trim_end_matches('/')
    }

    pub fn csp_origin(&self) -> &str {
        &self.csp_origin
    }
}

fn is_dns_bucket_name(value: &str) -> bool {
    if value.is_empty() || value.len() > 253 {
        return false;
    }
    value.split('.').all(|label| {
        if label.is_empty() || label.len() > 63 {
            return false;
        }
        let bytes = label.as_bytes();
        if !bytes[0].is_ascii_lowercase() && !bytes[0].is_ascii_digit() {
            return false;
        }
        if !bytes[bytes.len() - 1].is_ascii_lowercase() && !bytes[bytes.len() - 1].is_ascii_digit()
        {
            return false;
        }
        bytes
            .iter()
            .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || *byte == b'-')
    })
}

impl fmt::Display for HttpEndpoint {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CspSource(String);

impl CspSource {
    pub fn parse(name: &'static str, value: &str) -> Result<Self, InvalidAppProxyEnvironmentError> {
        if value
            .bytes()
            .any(|byte| byte.is_ascii_whitespace() || matches!(byte, b';' | b','))
        {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                value,
                "one CSP source without whitespace or policy delimiters",
            ));
        }
        if value == "*" {
            return Ok(Self(value.to_owned()));
        }
        if is_csp_keyword_source(value) || is_csp_nonce_or_hash_source(value) {
            return Ok(Self(value.to_owned()));
        }
        if matches!(
            value,
            "http:" | "https:" | "ws:" | "wss:" | "data:" | "blob:"
        ) {
            return Ok(Self(value.to_owned()));
        }
        if let Some(source) = parse_csp_network_source(value) {
            return Ok(Self(source));
        }
        Err(InvalidAppProxyEnvironmentError::new(
            name,
            value,
            "a supported CSP keyword, scheme, wildcard, nonce, hash, or HTTP(S)/WS(S) source",
        ))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

fn is_csp_keyword_source(value: &str) -> bool {
    matches!(
        value,
        "'self'"
            | "'unsafe-inline'"
            | "'unsafe-eval'"
            | "'wasm-unsafe-eval'"
            | "'strict-dynamic'"
            | "'report-sample'"
    )
}

fn is_csp_nonce_or_hash_source(value: &str) -> bool {
    let Some(inner) = value
        .strip_prefix('\'')
        .and_then(|value| value.strip_suffix('\''))
    else {
        return false;
    };
    let Some((algorithm, encoded)) = inner.split_once('-') else {
        return false;
    };
    if !matches!(algorithm, "nonce" | "sha256" | "sha384" | "sha512") || encoded.is_empty() {
        return false;
    }
    encoded.bytes().all(|byte| {
        byte.is_ascii_alphanumeric() || matches!(byte, b'+' | b'/' | b'_' | b'-' | b'=')
    })
}

fn parse_csp_network_source(value: &str) -> Option<String> {
    let (scheme, authority_and_path) = value.split_once("://")?;
    if !matches!(scheme, "http" | "https" | "ws" | "wss") {
        return None;
    }
    let wildcard = authority_and_path.starts_with("*.");
    let parse_value = if wildcard {
        format!(
            "{scheme}://csp-wildcard.invalid.{}",
            &authority_and_path[2..]
        )
    } else {
        value.to_owned()
    };
    let url = Url::parse(&parse_value).ok()?;
    if url.host_str().is_none()
        || !url.username().is_empty()
        || url.password().is_some()
        || url.query().is_some()
        || url.fragment().is_some()
    {
        return None;
    }
    let mut source = url.origin().ascii_serialization();
    if source == "null" {
        return None;
    }
    if wildcard {
        source = source.replacen("csp-wildcard.invalid.", "*.", 1);
    }
    if url.path() != "/" {
        source.push_str(url.path());
    }
    Some(source)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CspReportUri(HttpUrl);

impl CspReportUri {
    pub fn parse(name: &'static str, value: &str) -> Result<Self, InvalidAppProxyEnvironmentError> {
        if value
            .bytes()
            .any(|byte| byte.is_ascii_whitespace() || matches!(byte, b';' | b','))
        {
            return Err(InvalidAppProxyEnvironmentError::new(
                name,
                value,
                "one HTTP or HTTPS report URI without whitespace or policy delimiters",
            ));
        }
        Ok(Self(HttpUrl::parse(name, value)?))
    }
}

impl fmt::Display for CspReportUri {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

fn warn_invalid(error: &InvalidAppProxyEnvironmentError) {
    tracing::warn!(%error, "ignoring invalid app proxy environment value");
}

fn parse_optional_http_url(name: &'static str, value: Option<String>) -> Option<HttpUrl> {
    let value = value?;
    HttpUrl::parse(name, &value).inspect_err(warn_invalid).ok()
}

fn parse_optional_http_endpoint(name: &'static str, value: Option<String>) -> Option<HttpEndpoint> {
    let value = value?;
    HttpEndpoint::parse(name, &value)
        .inspect_err(warn_invalid)
        .ok()
}

fn parse_env_or_warn<T: std::str::FromStr>(name: &str, raw: &str, default: T) -> T {
    raw.parse::<T>().unwrap_or_else(|_| {
        tracing::warn!(
            env = name,
            value = raw,
            "invalid value; falling back to default"
        );
        default
    })
}

#[derive(Clone, Debug)]
pub struct AppProxyConfig {
    pub host: String,
    pub port: u16,
    pub static_dir: String,
    pub index_upstream_url: Option<HttpUrl>,
    pub static_cdn_endpoint: Option<HttpEndpoint>,
    pub media_endpoint: Option<HttpEndpoint>,
    pub s3_public_endpoint: Option<HttpEndpoint>,
    pub s3_uploads_endpoint: Option<HttpEndpoint>,
    pub release_channel: ReleaseChannel,
    pub build_version: String,
    pub csp: CspConfig,
    pub same_origin_hosts: Vec<String>,
    pub manifest_scope_extensions: Vec<String>,
    pub self_hosted: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReleaseChannel {
    Stable,
    Canary,
}

impl ReleaseChannel {
    fn from_env_value(value: &str) -> Self {
        if value.eq_ignore_ascii_case("canary") {
            Self::Canary
        } else {
            Self::Stable
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Stable => "stable",
            Self::Canary => "canary",
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CspConfig {
    pub extra_default_src: Vec<CspSource>,
    pub extra_connect_src: Vec<CspSource>,
    pub extra_img_src: Vec<CspSource>,
    pub extra_media_src: Vec<CspSource>,
    pub extra_font_src: Vec<CspSource>,
    pub extra_script_src: Vec<CspSource>,
    pub extra_style_src: Vec<CspSource>,
    pub extra_frame_src: Vec<CspSource>,
    pub extra_worker_src: Vec<CspSource>,
    pub extra_manifest_src: Vec<CspSource>,
    pub report_uri: Option<CspReportUri>,
}

impl CspConfig {
    pub fn from_env() -> Self {
        Self {
            extra_default_src: read_csp_sources("FLUXER_CSP_EXTRA_DEFAULT_SRC"),
            extra_connect_src: read_csp_sources("FLUXER_CSP_EXTRA_CONNECT_SRC"),
            extra_img_src: read_csp_sources("FLUXER_CSP_EXTRA_IMG_SRC"),
            extra_media_src: read_csp_sources("FLUXER_CSP_EXTRA_MEDIA_SRC"),
            extra_font_src: read_csp_sources("FLUXER_CSP_EXTRA_FONT_SRC"),
            extra_script_src: read_csp_sources("FLUXER_CSP_EXTRA_SCRIPT_SRC"),
            extra_style_src: read_csp_sources("FLUXER_CSP_EXTRA_STYLE_SRC"),
            extra_frame_src: read_csp_sources("FLUXER_CSP_EXTRA_FRAME_SRC"),
            extra_worker_src: read_csp_sources("FLUXER_CSP_EXTRA_WORKER_SRC"),
            extra_manifest_src: read_csp_sources("FLUXER_CSP_EXTRA_MANIFEST_SRC"),
            report_uri: read_csp_report_uri("FLUXER_CSP_REPORT_URI"),
        }
    }
}

fn read_csp_sources(name: &'static str) -> Vec<CspSource> {
    cfg::read_env(name, "")
        .split([',', ' ', '\t', '\n'])
        .map(str::trim)
        .filter(|source| !source.is_empty())
        .filter_map(|source| {
            CspSource::parse(name, source)
                .inspect_err(warn_invalid)
                .ok()
        })
        .collect()
}

fn read_csp_report_uri(name: &'static str) -> Option<CspReportUri> {
    let value = cfg::env_value(name)?;
    CspReportUri::parse(name, value.trim())
        .inspect_err(warn_invalid)
        .ok()
}

impl AppProxyConfig {
    pub fn from_env() -> Self {
        let release_channel =
            ReleaseChannel::from_env_value(&cfg::read_env("RELEASE_CHANNEL", "stable"));

        let s3_public_endpoint = parse_optional_http_endpoint(
            "FLUXER_S3_PUBLIC_ENDPOINT",
            cfg::env_value("FLUXER_S3_PUBLIC_ENDPOINT"),
        );
        let s3_uploads_bucket = cfg::read_env("FLUXER_S3_BUCKET_UPLOADS", "fluxer-uploads");
        let s3_uploads_endpoint = s3_public_endpoint.as_ref().and_then(|endpoint| {
            endpoint
                .with_host_prefix("FLUXER_S3_BUCKET_UPLOADS", s3_uploads_bucket.trim())
                .inspect_err(warn_invalid)
                .ok()
        });

        Self {
            host: cfg::read_env("FLUXER_APP_PROXY_HOST", "0.0.0.0"),
            port: parse_env_or_warn(
                "FLUXER_APP_PROXY_PORT",
                &cfg::read_env("FLUXER_APP_PROXY_PORT", "8080"),
                8080u16,
            ),
            static_dir: cfg::read_env("FLUXER_STATIC_DIR", "./static"),
            index_upstream_url: parse_optional_http_url(
                "FLUXER_APP_PROXY_INDEX_UPSTREAM_URL",
                cfg::env_value("FLUXER_APP_PROXY_INDEX_UPSTREAM_URL"),
            ),
            static_cdn_endpoint: parse_optional_http_endpoint(
                "FLUXER_STATIC_CDN_ENDPOINT",
                cfg::env_value("FLUXER_STATIC_CDN_ENDPOINT"),
            ),
            media_endpoint: parse_optional_http_endpoint(
                "FLUXER_MEDIA_ENDPOINT",
                cfg::env_value("FLUXER_MEDIA_ENDPOINT"),
            ),
            s3_public_endpoint,
            s3_uploads_endpoint,
            release_channel,
            build_version: cfg::read_first_env(
                &["BUILD_VERSION", "FLUXER_BUILD_VERSION"],
                env!("CARGO_PKG_VERSION"),
            ),
            csp: CspConfig::from_env(),
            same_origin_hosts: parse_same_origin_hosts(
                "FLUXER_APP_PROXY_SAME_ORIGIN_HOSTS",
                &cfg::read_env("FLUXER_APP_PROXY_SAME_ORIGIN_HOSTS", ""),
            ),
            manifest_scope_extensions: parse_manifest_scope_extensions(
                "FLUXER_APP_PROXY_MANIFEST_SCOPE_EXTENSIONS",
                &cfg::read_env("FLUXER_APP_PROXY_MANIFEST_SCOPE_EXTENSIONS", ""),
            ),
            self_hosted: cfg::read_bool_env("FLUXER_SELF_HOSTED", false),
        }
    }
}

fn parse_manifest_scope_extensions(name: &'static str, raw: &str) -> Vec<String> {
    let mut origins: Vec<String> = Vec::new();
    for value in raw
        .split([',', ' ', '\t', '\n'])
        .map(str::trim)
        .filter(|value| !value.is_empty())
    {
        match parse_manifest_scope_extension_origin(name, value) {
            Ok(origin) => {
                if !origins.contains(&origin) {
                    origins.push(origin);
                }
            }
            Err(error) => warn_invalid(&error),
        }
    }
    origins
}

fn parse_manifest_scope_extension_origin(
    name: &'static str,
    value: &str,
) -> Result<String, InvalidAppProxyEnvironmentError> {
    let origin = Url::parse(value).ok().filter(|url| {
        url.scheme() == "https"
            && url.host_str().is_some()
            && url.username().is_empty()
            && url.password().is_none()
            && url.path() == "/"
            && url.query().is_none()
            && url.fragment().is_none()
    });
    match origin {
        Some(url) => Ok(url.origin().ascii_serialization()),
        None => Err(InvalidAppProxyEnvironmentError::new(
            name,
            value,
            "an HTTPS origin without a path, query or credentials",
        )),
    }
}

fn parse_same_origin_hosts(name: &'static str, raw: &str) -> Vec<String> {
    let mut hosts: Vec<String> = Vec::new();
    for value in raw
        .split([',', ' ', '\t', '\n'])
        .map(str::trim)
        .filter(|value| !value.is_empty())
    {
        match parse_same_origin_host(name, value) {
            Ok(host) => {
                if !hosts.contains(&host) {
                    hosts.push(host);
                }
            }
            Err(error) => warn_invalid(&error),
        }
    }
    hosts
}

fn parse_same_origin_host(
    name: &'static str,
    value: &str,
) -> Result<String, InvalidAppProxyEnvironmentError> {
    let host = value.trim_end_matches('.').to_ascii_lowercase();
    let is_hostname = !host.is_empty()
        && Url::parse(&format!("https://{host}/")).is_ok_and(|url| {
            url.host_str() == Some(host.as_str())
                && url.port().is_none()
                && url.path() == "/"
                && url.username().is_empty()
        });
    if !is_hostname {
        return Err(InvalidAppProxyEnvironmentError::new(
            name,
            value,
            "a bare hostname without a scheme, port or path",
        ));
    }
    Ok(host)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_origin_hosts_are_normalised_and_deduplicated() {
        assert_eq!(
            parse_same_origin_hosts(
                "TEST_SAME_ORIGIN_HOSTS",
                " web.fluxer.app, Fluxer.COM,fluxer.com. ,fluxer.com"
            ),
            vec!["web.fluxer.app".to_owned(), "fluxer.com".to_owned()]
        );
    }

    #[test]
    fn same_origin_hosts_drop_anything_but_a_bare_hostname() {
        assert_eq!(
            parse_same_origin_hosts(
                "TEST_SAME_ORIGIN_HOSTS",
                "https://fluxer.com,fluxer.com:443,fluxer.com/api,user@fluxer.com,canary.fluxer.com"
            ),
            vec!["canary.fluxer.com".to_owned()]
        );
    }

    #[test]
    fn manifest_scope_extensions_are_normalised_and_deduplicated() {
        assert_eq!(
            parse_manifest_scope_extensions(
                "TEST_SCOPE_EXTENSIONS",
                " https://Fluxer.COM, https://fluxer.com/ ,https://canary.fluxer.com:443"
            ),
            vec![
                "https://fluxer.com".to_owned(),
                "https://canary.fluxer.com".to_owned()
            ]
        );
    }

    #[test]
    fn manifest_scope_extensions_drop_anything_but_an_https_origin() {
        assert_eq!(
            parse_manifest_scope_extensions(
                "TEST_SCOPE_EXTENSIONS",
                "fluxer.com,http://fluxer.com,https://fluxer.com/app,https://fluxer.com/?a=1,https://user@fluxer.com,https://canary.fluxer.com"
            ),
            vec!["https://canary.fluxer.com".to_owned()]
        );
    }
}
