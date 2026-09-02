// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::{self as cfg, GeoipS3Config, GeoipSourceConfig};
use fluxer_svc::config::{DatabaseBackend, normalize_host, parse_hosts};
use reqwest::Url;
use std::env;
use std::fmt;

const DEFAULT_DISCOVERY_UPSTREAM_URL: &str = "http://localhost:8088/api/.well-known/fluxer";

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

fn warn_invalid(error: InvalidAppProxyEnvironmentError) {
    tracing::warn!(%error, "ignoring invalid app proxy environment value");
}

fn parse_optional_http_url(name: &'static str, value: Option<String>) -> Option<HttpUrl> {
    let value = value?;
    match HttpUrl::parse(name, &value) {
        Ok(url) => Some(url),
        Err(error) => {
            warn_invalid(error);
            None
        }
    }
}

fn parse_optional_http_endpoint(name: &'static str, value: Option<String>) -> Option<HttpEndpoint> {
    let value = value?;
    match HttpEndpoint::parse(name, &value) {
        Ok(endpoint) => Some(endpoint),
        Err(error) => {
            warn_invalid(error);
            None
        }
    }
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
    pub s3_public_endpoint: Option<HttpEndpoint>,
    pub s3_uploads_endpoint: Option<HttpEndpoint>,
    pub discovery_upstream_url: String,
    pub discovery_refresh_interval_ms: u64,
    pub release_channel: ReleaseChannel,
    pub time_freeze_enabled: bool,
    pub build_version: String,
    pub bootstrap_api_endpoint: String,
    pub bootstrap_api_public_endpoint: Option<String>,
    pub csp: CspConfig,
    pub geoip_source: GeoipSourceConfig,
    pub geoip_s3_config: Option<GeoipS3Config>,
    pub trust_client_ip_header: bool,
    pub client_ip_header_name: String,
    pub invite_meta_enabled: bool,
    pub invite_meta_cache_max_entries: u64,
    pub invite_meta_cache_ttl_ms: u64,
    pub database_backend: DatabaseBackend,
    pub scylla_hosts: Vec<String>,
    pub scylla_keyspace: String,
    pub scylla_username: Option<String>,
    pub scylla_password: Option<String>,
    pub postgres_url: Option<String>,
    pub postgres_host: String,
    pub postgres_port: u16,
    pub postgres_database: String,
    pub postgres_username: String,
    pub postgres_password: Option<String>,
    pub postgres_ssl: bool,
    pub postgres_ssl_ca: Option<String>,
    pub postgres_max_connections: usize,
    pub postgres_kv_table: String,
    pub postgres_prepared_statements: bool,
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

    pub const fn is_canary(self) -> bool {
        matches!(self, Self::Canary)
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
        .filter_map(|source| match CspSource::parse(name, source) {
            Ok(source) => Some(source),
            Err(error) => {
                warn_invalid(error);
                None
            }
        })
        .collect()
}

fn read_csp_report_uri(name: &'static str) -> Option<CspReportUri> {
    let value = cfg::non_empty_env(name)?;
    match CspReportUri::parse(name, &value) {
        Ok(report_uri) => Some(report_uri),
        Err(error) => {
            warn_invalid(error);
            None
        }
    }
}

impl AppProxyConfig {
    pub fn from_env() -> Self {
        let release_channel = ReleaseChannel::from_env_value(&cfg::read_env_preferred(
            &["RELEASE_CHANNEL"],
            "stable",
        ));
        let time_freeze_enabled = resolve_time_freeze_enabled_from_env();
        let geoip_source = cfg::parse_geoip_source_config(
            &cfg::read_first_env(&["FLUXER_GEOIP_DB_PATH", "MAXMIND_DB_PATH"], ""),
            "app_proxy",
        );
        let geoip_s3_config = cfg::read_geoip_s3_config_from_env(&geoip_source);

        let cassandra_port = parse_env_or_warn(
            "FLUXER_CASSANDRA_PORT",
            &cfg::read_env("FLUXER_CASSANDRA_PORT", "9042"),
            9042u16,
        );
        let scylla_hosts = cfg::non_empty_env("FLUXER_CASSANDRA_HOSTS")
            .map(|hosts| {
                parse_hosts(&hosts)
                    .into_iter()
                    .map(|host| normalize_host(&host, cassandra_port))
                    .collect::<Vec<_>>()
            })
            .filter(|hosts| !hosts.is_empty())
            .unwrap_or_else(|| vec![normalize_host("127.0.0.1", cassandra_port)]);
        let database_backend =
            parse_database_backend(&cfg::read_env("FLUXER_DATABASE_BACKEND", "postgres"));
        let postgres_port = parse_env_or_warn(
            "FLUXER_POSTGRES_PORT",
            &cfg::read_env("FLUXER_POSTGRES_PORT", "5432"),
            5432u16,
        );
        let postgres_max_connections = parse_env_or_warn(
            "FLUXER_POSTGRES_MAX_CONNECTIONS",
            &cfg::read_env("FLUXER_POSTGRES_MAX_CONNECTIONS", "20"),
            20usize,
        )
        .max(1);

        let s3_public_endpoint = parse_optional_http_endpoint(
            "FLUXER_S3_PUBLIC_ENDPOINT",
            cfg::non_empty_env("FLUXER_S3_PUBLIC_ENDPOINT"),
        );
        let s3_uploads_bucket = cfg::read_env("FLUXER_S3_BUCKET_UPLOADS", "fluxer-uploads");
        let s3_uploads_endpoint = s3_public_endpoint.as_ref().and_then(|endpoint| {
            match endpoint.with_host_prefix("FLUXER_S3_BUCKET_UPLOADS", s3_uploads_bucket.trim()) {
                Ok(endpoint) => Some(endpoint),
                Err(error) => {
                    warn_invalid(error);
                    None
                }
            }
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
                cfg::non_empty_env("FLUXER_APP_PROXY_INDEX_UPSTREAM_URL"),
            ),
            static_cdn_endpoint: parse_optional_http_endpoint(
                "FLUXER_STATIC_CDN_ENDPOINT",
                cfg::non_empty_env("FLUXER_STATIC_CDN_ENDPOINT"),
            ),
            s3_public_endpoint: s3_public_endpoint.clone(),
            s3_uploads_endpoint,
            discovery_upstream_url: resolve_discovery_upstream_url_from_env(),
            discovery_refresh_interval_ms: parse_env_or_warn(
                "DISCOVERY_REFRESH_INTERVAL_MS",
                &cfg::read_env("DISCOVERY_REFRESH_INTERVAL_MS", "60000"),
                60_000u64,
            ),
            release_channel,
            time_freeze_enabled,
            build_version: cfg::read_env_preferred(
                &["BUILD_VERSION", "FLUXER_BUILD_VERSION"],
                env!("CARGO_PKG_VERSION"),
            ),
            bootstrap_api_endpoint: cfg::read_env("PUBLIC_BOOTSTRAP_API_ENDPOINT", "/api"),
            bootstrap_api_public_endpoint: resolve_bootstrap_api_public_endpoint_from_env(),
            csp: CspConfig::from_env(),
            geoip_source,
            geoip_s3_config,
            trust_client_ip_header: cfg::read_bool_env(
                &["FLUXER_TRUST_CLIENT_IP_HEADER", "TRUST_CLIENT_IP_HEADER"],
                false,
            ),
            client_ip_header_name: cfg::read_first_env(
                &[
                    "FLUXER_CLIENT_IP_HEADER_NAME",
                    "FLUXER_CLIENT_IP_HEADER",
                    "CLIENT_IP_HEADER_NAME",
                    "CLIENT_IP_HEADER",
                ],
                "x-forwarded-for",
            )
            .trim()
            .to_ascii_lowercase(),
            invite_meta_enabled: cfg::read_bool_env(
                &["FLUXER_APP_PROXY_INVITE_META_ENABLED"],
                true,
            ),
            invite_meta_cache_max_entries: parse_env_or_warn(
                "FLUXER_APP_PROXY_INVITE_META_CACHE_MAX_ENTRIES",
                &cfg::read_env("FLUXER_APP_PROXY_INVITE_META_CACHE_MAX_ENTRIES", "10000"),
                10_000u64,
            ),
            invite_meta_cache_ttl_ms: parse_env_or_warn(
                "FLUXER_APP_PROXY_INVITE_META_CACHE_TTL_MS",
                &cfg::read_env("FLUXER_APP_PROXY_INVITE_META_CACHE_TTL_MS", "30000"),
                30_000u64,
            ),
            database_backend,
            scylla_hosts,
            scylla_keyspace: cfg::read_env("FLUXER_CASSANDRA_KEYSPACE", "fluxer"),
            scylla_username: cfg::non_empty_env("FLUXER_CASSANDRA_USERNAME"),
            scylla_password: cfg::non_empty_env("FLUXER_CASSANDRA_PASSWORD"),
            postgres_url: cfg::non_empty_env("FLUXER_POSTGRES_URL"),
            postgres_host: cfg::read_env("FLUXER_POSTGRES_HOST", "127.0.0.1"),
            postgres_port,
            postgres_database: cfg::read_env("FLUXER_POSTGRES_DATABASE", "fluxer"),
            postgres_username: cfg::read_env("FLUXER_POSTGRES_USERNAME", "fluxer"),
            postgres_password: cfg::non_empty_env("FLUXER_POSTGRES_PASSWORD")
                .or_else(|| Some("fluxer".to_owned())),
            postgres_ssl: cfg::read_bool_env(&["FLUXER_POSTGRES_SSL"], false),
            postgres_ssl_ca: cfg::non_empty_env("FLUXER_POSTGRES_SSL_CA"),
            postgres_max_connections,
            postgres_kv_table: cfg::read_env("FLUXER_POSTGRES_KV_TABLE", "fluxer_kv"),
            postgres_prepared_statements: resolve_postgres_prepared_statements_from_env(),
        }
    }
}

fn parse_database_backend(value: &str) -> DatabaseBackend {
    match value.trim().to_ascii_lowercase().as_str() {
        "cassandra" | "scylla" | "scylladb" => DatabaseBackend::Cassandra,
        _ => DatabaseBackend::Postgres,
    }
}

fn resolve_discovery_upstream_url_from_env() -> String {
    resolve_discovery_upstream_url(|name| env::var(name).ok())
}

fn resolve_time_freeze_enabled_from_env() -> bool {
    resolve_time_freeze_enabled(|name| env::var(name).ok())
}

fn resolve_postgres_prepared_statements_from_env() -> bool {
    resolve_postgres_prepared_statements(|name| env::var(name).ok())
}

fn resolve_bootstrap_api_public_endpoint_from_env() -> Option<String> {
    resolve_bootstrap_api_public_endpoint(|name| env::var(name).ok())
}

fn resolve_bootstrap_api_public_endpoint<F>(mut read_var: F) -> Option<String>
where
    F: FnMut(&str) -> Option<String>,
{
    let endpoint = read_var("PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT")
        .map(|value| value.trim().to_owned())
        .filter(|value| !value.is_empty())?;
    let base_domain = read_var("FLUXER_BASE_DOMAIN").unwrap_or_default();
    let public_port = read_var("FLUXER_PUBLIC_PORT").and_then(|port| port.trim().parse().ok());

    Some(cfg::normalize_public_endpoint(
        &endpoint,
        &base_domain,
        public_port,
    ))
}

fn resolve_time_freeze_enabled<F>(mut read_var: F) -> bool
where
    F: FnMut(&str) -> Option<String>,
{
    if let Some(value) = read_var("FLUXER_APP_PROXY_TIME_FREEZE_ENABLED") {
        return parse_boolish(&value);
    }

    !read_var("FLUXER_SELF_HOSTED").is_some_and(|value| parse_boolish(&value))
}

fn parse_boolish(value: &str) -> bool {
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn resolve_postgres_prepared_statements<F>(mut read_var: F) -> bool
where
    F: FnMut(&str) -> Option<String>,
{
    let Some(value) = read_var("FLUXER_POSTGRES_PREPARED_STATEMENTS")
        .map(|value| value.trim().to_ascii_lowercase())
        .filter(|value| !value.is_empty())
    else {
        return true;
    };

    match value.as_str() {
        "1" | "true" | "yes" | "y" | "on" => true,
        "0" | "false" | "no" | "n" | "off" => false,
        other => {
            tracing::warn!(
                env = "FLUXER_POSTGRES_PREPARED_STATEMENTS",
                value = other,
                "invalid value; falling back to default"
            );
            true
        }
    }
}

fn resolve_discovery_upstream_url<F>(mut read_var: F) -> String
where
    F: FnMut(&str) -> Option<String>,
{
    if let Some(value) = read_var("DISCOVERY_UPSTREAM_URL")
        .map(|value| value.trim().to_owned())
        .filter(|value| !value.is_empty())
    {
        return value;
    }

    [
        "FLUXER_API_ENDPOINT",
        "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
        "PUBLIC_BOOTSTRAP_API_ENDPOINT",
        "FLUXER_INTERNAL_API_ENDPOINT",
    ]
    .into_iter()
    .find_map(|name| {
        read_var(name)
            .as_deref()
            .and_then(discovery_url_from_api_endpoint)
    })
    .unwrap_or_else(|| DEFAULT_DISCOVERY_UPSTREAM_URL.to_owned())
}

fn discovery_url_from_api_endpoint(value: &str) -> Option<String> {
    let endpoint = value.trim().trim_end_matches('/');
    if endpoint.starts_with("http://") || endpoint.starts_with("https://") {
        Some(format!("{endpoint}/.well-known/fluxer"))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn resolve_discovery_from_pairs(pairs: &[(&str, &str)]) -> String {
        let env: HashMap<&str, &str> = pairs.iter().copied().collect();
        resolve_discovery_upstream_url(|name| env.get(name).map(|value| value.to_string()))
    }

    fn resolve_time_freeze_from_pairs(pairs: &[(&str, &str)]) -> bool {
        let env: HashMap<&str, &str> = pairs.iter().copied().collect();
        resolve_time_freeze_enabled(|name| env.get(name).map(|value| value.to_string()))
    }

    fn resolve_prepared_statements_from_pairs(pairs: &[(&str, &str)]) -> bool {
        let env: HashMap<&str, &str> = pairs.iter().copied().collect();
        resolve_postgres_prepared_statements(|name| env.get(name).map(|value| value.to_string()))
    }

    fn resolve_bootstrap_endpoint_from_pairs(pairs: &[(&str, &str)]) -> Option<String> {
        let env: HashMap<&str, &str> = pairs.iter().copied().collect();
        resolve_bootstrap_api_public_endpoint(|name| env.get(name).map(|value| value.to_string()))
    }

    #[test]
    fn a_non_default_public_port_reaches_the_boot_html_api_endpoint() {
        assert_eq!(
            resolve_bootstrap_endpoint_from_pairs(&[
                (
                    "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
                    "http://fluxer.example/api",
                ),
                ("FLUXER_BASE_DOMAIN", "fluxer.example"),
                ("FLUXER_PUBLIC_PORT", "19080"),
            ]),
            Some("http://fluxer.example:19080/api".to_owned())
        );
    }

    #[test]
    fn a_default_public_port_leaves_the_boot_html_api_endpoint_alone() {
        assert_eq!(
            resolve_bootstrap_endpoint_from_pairs(&[
                (
                    "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
                    "https://fluxer.example/api",
                ),
                ("FLUXER_BASE_DOMAIN", "fluxer.example"),
                ("FLUXER_PUBLIC_PORT", "443"),
            ]),
            Some("https://fluxer.example/api".to_owned())
        );
    }

    #[test]
    fn the_boot_html_api_endpoint_keeps_a_port_it_already_carries() {
        assert_eq!(
            resolve_bootstrap_endpoint_from_pairs(&[
                (
                    "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
                    "http://fluxer.example:19080/api",
                ),
                ("FLUXER_BASE_DOMAIN", "fluxer.example"),
                ("FLUXER_PUBLIC_PORT", "19080"),
            ]),
            Some("http://fluxer.example:19080/api".to_owned())
        );
    }

    #[test]
    fn the_boot_html_api_endpoint_is_untouched_without_a_base_domain_and_port() {
        assert_eq!(
            resolve_bootstrap_endpoint_from_pairs(&[(
                "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
                "http://fluxer.example/api",
            )]),
            Some("http://fluxer.example/api".to_owned())
        );
        assert_eq!(resolve_bootstrap_endpoint_from_pairs(&[]), None);
    }

    #[test]
    fn csp_config_default_has_no_extra_sources() {
        let c = CspConfig::default();
        assert!(
            c.extra_default_src.is_empty()
                && c.extra_script_src.is_empty()
                && c.report_uri.is_none()
        );
    }

    #[test]
    fn release_channel_stable_is_default() {
        assert_eq!(
            ReleaseChannel::from_env_value("stable"),
            ReleaseChannel::Stable
        );
        assert_eq!(
            ReleaseChannel::from_env_value("unknown"),
            ReleaseChannel::Stable
        );
    }

    #[test]
    fn release_channel_canary_case_insensitive() {
        assert_eq!(
            ReleaseChannel::from_env_value("canary"),
            ReleaseChannel::Canary
        );
        assert_eq!(
            ReleaseChannel::from_env_value("CANARY"),
            ReleaseChannel::Canary
        );
    }

    #[test]
    fn release_channel_as_str_and_is_canary() {
        assert_eq!(ReleaseChannel::Stable.as_str(), "stable");
        assert_eq!(ReleaseChannel::Canary.as_str(), "canary");
        assert!(!ReleaseChannel::Stable.is_canary());
        assert!(ReleaseChannel::Canary.is_canary());
    }

    #[test]
    fn explicit_discovery_upstream_url_wins() {
        assert_eq!(
            resolve_discovery_from_pairs(&[
                (
                    "DISCOVERY_UPSTREAM_URL",
                    "https://web.canary.fluxer.app/api/.well-known/fluxer",
                ),
                ("FLUXER_API_ENDPOINT", "https://api.canary.fluxer.app"),
            ]),
            "https://web.canary.fluxer.app/api/.well-known/fluxer"
        );
    }

    #[test]
    fn discovery_upstream_url_derives_from_existing_api_endpoint() {
        assert_eq!(
            resolve_discovery_from_pairs(&[(
                "FLUXER_API_ENDPOINT",
                "https://api.canary.fluxer.app/"
            )]),
            "https://api.canary.fluxer.app/.well-known/fluxer"
        );
    }

    #[test]
    fn discovery_upstream_url_skips_relative_bootstrap_endpoint() {
        assert_eq!(
            resolve_discovery_from_pairs(&[
                ("PUBLIC_BOOTSTRAP_API_ENDPOINT", "/api"),
                (
                    "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
                    "https://api.canary.fluxer.app",
                ),
            ]),
            "https://api.canary.fluxer.app/.well-known/fluxer"
        );
    }

    #[test]
    fn discovery_upstream_url_falls_back_to_local_default() {
        assert_eq!(
            resolve_discovery_from_pairs(&[("PUBLIC_BOOTSTRAP_API_ENDPOINT", "/api")]),
            DEFAULT_DISCOVERY_UPSTREAM_URL
        );
    }

    #[test]
    fn a_set_but_empty_prepared_statements_value_keeps_the_shared_default() {
        assert!(resolve_prepared_statements_from_pairs(&[]));
        assert!(
            resolve_prepared_statements_from_pairs(&[("FLUXER_POSTGRES_PREPARED_STATEMENTS", "")]),
            "an empty value disabled named statements here while every other service kept them"
        );
        assert!(resolve_prepared_statements_from_pairs(&[(
            "FLUXER_POSTGRES_PREPARED_STATEMENTS",
            "   ",
        )]));
    }

    #[test]
    fn an_explicit_prepared_statements_value_is_honoured() {
        assert!(!resolve_prepared_statements_from_pairs(&[(
            "FLUXER_POSTGRES_PREPARED_STATEMENTS",
            "false",
        )]));
        assert!(!resolve_prepared_statements_from_pairs(&[(
            "FLUXER_POSTGRES_PREPARED_STATEMENTS",
            "OFF",
        )]));
        assert!(resolve_prepared_statements_from_pairs(&[(
            "FLUXER_POSTGRES_PREPARED_STATEMENTS",
            "yes",
        )]));
    }

    #[test]
    fn a_non_boolean_prepared_statements_value_keeps_the_shared_default() {
        assert!(resolve_prepared_statements_from_pairs(&[(
            "FLUXER_POSTGRES_PREPARED_STATEMENTS",
            "maybe",
        )]));
    }

    #[test]
    fn time_freeze_enabled_by_default_for_hosted_runtime() {
        assert!(resolve_time_freeze_from_pairs(&[]));
    }

    #[test]
    fn time_freeze_disabled_by_default_for_self_hosted_runtime() {
        assert!(!resolve_time_freeze_from_pairs(&[(
            "FLUXER_SELF_HOSTED",
            "true"
        )]));
    }

    #[test]
    fn explicit_time_freeze_setting_overrides_self_hosted_default() {
        assert!(resolve_time_freeze_from_pairs(&[
            ("FLUXER_SELF_HOSTED", "true"),
            ("FLUXER_APP_PROXY_TIME_FREEZE_ENABLED", "true"),
        ]));
        assert!(!resolve_time_freeze_from_pairs(&[
            ("FLUXER_SELF_HOSTED", "false"),
            ("FLUXER_APP_PROXY_TIME_FREEZE_ENABLED", "false"),
        ]));
    }
}
