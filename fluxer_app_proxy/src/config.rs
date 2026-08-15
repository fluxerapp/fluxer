// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::{self as cfg, GeoipS3Config, GeoipSourceConfig};
use fluxer_svc::config::{DatabaseBackend, normalize_host, parse_hosts};
use std::env;

const DEFAULT_DISCOVERY_UPSTREAM_URL: &str = "http://localhost:8088/api/.well-known/fluxer";

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
    pub index_upstream_url: Option<String>,
    pub static_cdn_endpoint: Option<String>,
    pub s3_public_endpoint: Option<String>,
    pub s3_uploads_bucket: String,
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
    pub extra_default_src: Option<Vec<String>>,
    pub extra_connect_src: Option<Vec<String>>,
    pub extra_img_src: Option<Vec<String>>,
    pub extra_media_src: Option<Vec<String>>,
    pub extra_font_src: Option<Vec<String>>,
    pub extra_script_src: Option<Vec<String>>,
    pub extra_style_src: Option<Vec<String>>,
    pub extra_frame_src: Option<Vec<String>>,
    pub extra_worker_src: Option<Vec<String>>,
    pub extra_manifest_src: Option<Vec<String>>,
    pub report_uri: Option<String>,
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
            report_uri: cfg::non_empty_env("FLUXER_CSP_REPORT_URI"),
        }
    }
}

fn read_csp_sources(name: &str) -> Option<Vec<String>> {
    let sources: Vec<String> = cfg::read_env(name, "")
        .split([',', ' ', '\t', '\n'])
        .map(str::trim)
        .filter(|source| !source.is_empty())
        .map(str::to_owned)
        .collect();

    if sources.is_empty() {
        None
    } else {
        Some(sources)
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

        Self {
            host: cfg::read_env("FLUXER_APP_PROXY_HOST", "0.0.0.0"),
            port: parse_env_or_warn(
                "FLUXER_APP_PROXY_PORT",
                &cfg::read_env("FLUXER_APP_PROXY_PORT", "8080"),
                8080u16,
            ),
            static_dir: cfg::read_env("FLUXER_STATIC_DIR", "./static"),
            index_upstream_url: cfg::non_empty_env("FLUXER_APP_PROXY_INDEX_UPSTREAM_URL"),
            static_cdn_endpoint: cfg::non_empty_env("FLUXER_STATIC_CDN_ENDPOINT"),
            s3_public_endpoint: cfg::non_empty_env("FLUXER_S3_PUBLIC_ENDPOINT"),
            s3_uploads_bucket: cfg::read_env("FLUXER_S3_BUCKET_UPLOADS", "fluxer-uploads"),
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
            bootstrap_api_public_endpoint: cfg::non_empty_env(
                "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT",
            ),
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

    #[test]
    fn csp_config_default_has_no_extra_sources() {
        let c = CspConfig::default();
        assert!(
            c.extra_default_src.is_none() && c.extra_script_src.is_none() && c.report_uri.is_none()
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
