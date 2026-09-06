// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::normalize_public_endpoint_from_env;
use std::env;

const DEFAULT_ADMIN_OAUTH_CLIENT_ID: &str = "1234567890123456789";

#[derive(Clone, Debug)]
pub struct AdminConfig {
    pub env: RuntimeEnv,
    pub host: String,
    pub port: u16,
    pub secret_key_base: String,
    pub base_path: String,
    pub api_endpoint: String,
    pub media_endpoint: String,
    pub static_cdn_endpoint: String,
    pub admin_endpoint: String,
    pub web_app_endpoint: String,
    pub kv_url: String,
    pub oauth_client_id: String,
    pub oauth_client_secret: String,
    pub oauth_redirect_uri: String,
    pub build_version: String,
    pub release_channel: String,
    pub self_hosted: bool,
    pub proxy: ProxyConfig,
}

#[derive(Clone, Debug)]
pub struct ProxyConfig {
    pub trust_client_ip_header: bool,
    pub client_ip_header_name: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeEnv {
    Development,
    Production,
    Test,
}

impl AdminConfig {
    pub fn from_env() -> anyhow::Result<Self> {
        let base_path = normalize_base_path(&read_env("FLUXER_ADMIN_BASE_PATH", ""));
        let admin_endpoint = normalize_public_endpoint_from_env(&trim_trailing_slash(&read_env(
            "FLUXER_ADMIN_ENDPOINT",
            "https://admin.fluxer.app",
        )));
        let oauth_redirect_uri = normalize_public_endpoint_from_env(&read_env_preferred(
            &["FLUXER_ADMIN_OAUTH_REDIRECT_URI"],
            &format!("{admin_endpoint}/oauth2_callback"),
        ));
        let secret_key_base = read_env("FLUXER_ADMIN_SECRET_KEY_BASE", "");
        anyhow::ensure!(
            !secret_key_base.trim().is_empty(),
            "FLUXER_ADMIN_SECRET_KEY_BASE is required"
        );

        Ok(Self {
            env: RuntimeEnv::from_env_value(&read_env("FLUXER_ENV", "development")),
            host: read_env("FLUXER_ADMIN_HOST", "0.0.0.0"),
            port: read_env("FLUXER_ADMIN_PORT", "3020")
                .parse()
                .unwrap_or(3020),
            secret_key_base,
            base_path,
            api_endpoint: trim_trailing_slash(&read_env(
                "FLUXER_API_ENDPOINT",
                "https://api.fluxer.app",
            )),
            media_endpoint: normalize_public_endpoint_from_env(&trim_trailing_slash(&read_env(
                "FLUXER_MEDIA_ENDPOINT",
                "https://media.fluxer.app",
            ))),
            static_cdn_endpoint: normalize_public_endpoint_from_env(&trim_trailing_slash(
                &read_env("FLUXER_STATIC_CDN_ENDPOINT", ""),
            )),

            admin_endpoint,
            web_app_endpoint: normalize_public_endpoint_from_env(&trim_trailing_slash(&read_env(
                "FLUXER_APP_ENDPOINT",
                "https://app.fluxer.app",
            ))),
            kv_url: read_env("FLUXER_KV_URL", ""),
            oauth_client_id: read_env(
                "FLUXER_ADMIN_OAUTH_CLIENT_ID",
                DEFAULT_ADMIN_OAUTH_CLIENT_ID,
            ),
            oauth_client_secret: read_env("FLUXER_ADMIN_OAUTH_CLIENT_SECRET", ""),
            oauth_redirect_uri,
            build_version: read_env_preferred(
                &["BUILD_VERSION", "FLUXER_BUILD_VERSION"],
                env!("CARGO_PKG_VERSION"),
            ),
            release_channel: read_env_preferred(
                &["RELEASE_CHANNEL", "FLUXER_RELEASE_CHANNEL"],
                "stable",
            ),
            self_hosted: read_bool_env(&["FLUXER_SELF_HOSTED"], false),
            proxy: ProxyConfig {
                trust_client_ip_header: read_bool_env(
                    &["FLUXER_TRUST_CLIENT_IP_HEADER", "TRUST_CLIENT_IP_HEADER"],
                    false,
                ),
                client_ip_header_name: read_env_preferred(
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
            },
        })
    }

    pub fn is_dev(&self) -> bool {
        self.env == RuntimeEnv::Development
    }

    pub fn is_production(&self) -> bool {
        self.env == RuntimeEnv::Production
    }

    pub fn secure_cookies(&self) -> bool {
        self.admin_endpoint.starts_with("https://")
    }
}

impl RuntimeEnv {
    pub(crate) fn from_env_value(value: &str) -> Self {
        match value {
            "production" => Self::Production,
            "test" => Self::Test,
            _ => Self::Development,
        }
    }
}

pub fn normalize_base_path(value: &str) -> String {
    let trimmed = value.trim().trim_matches('/');
    if trimmed.is_empty() {
        String::new()
    } else {
        format!("/{trimmed}")
    }
}

pub fn trim_trailing_slash(value: &str) -> String {
    value.trim_end_matches('/').to_owned()
}

pub(crate) fn read_env(name: &str, fallback: &str) -> String {
    env::var(name).unwrap_or_else(|_| fallback.to_owned())
}

pub(crate) fn read_env_preferred(names: &[&str], fallback: &str) -> String {
    names
        .iter()
        .find_map(|name| env::var(name).ok().filter(|value| !value.trim().is_empty()))
        .unwrap_or_else(|| fallback.to_owned())
}

pub(crate) fn read_bool_env(names: &[&str], fallback: bool) -> bool {
    let Some(value) = names.iter().find_map(|name| env::var(name).ok()) else {
        return fallback;
    };
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    static ENV_LOCK: Mutex<()> = Mutex::new(());

    const MANAGED_ENV: [&str; 11] = [
        "FLUXER_ENV",
        "FLUXER_ADMIN_HOST",
        "FLUXER_ADMIN_PORT",
        "FLUXER_ADMIN_ENDPOINT",
        "FLUXER_ADMIN_OAUTH_CLIENT_ID",
        "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
        "FLUXER_MASTER_CONFIG",
        "FLUXER_APP_ENDPOINT",
        "FLUXER_MEDIA_ENDPOINT",
        "FLUXER_STATIC_CDN_ENDPOINT",
        "FLUXER_BASE_DOMAIN",
    ];

    fn config_from_env(vars: &[(&str, &str)]) -> AdminConfig {
        let _guard = ENV_LOCK.lock().unwrap();
        for name in MANAGED_ENV {
            unsafe { env::remove_var(name) };
        }
        unsafe { env::remove_var("FLUXER_PUBLIC_PORT") };
        unsafe { env::set_var("FLUXER_ADMIN_SECRET_KEY_BASE", "test-secret") };
        for (name, value) in vars {
            unsafe { env::set_var(name, value) };
        }
        let config = AdminConfig::from_env().expect("config loads with a secret");
        for (name, _) in vars {
            unsafe { env::remove_var(name) };
        }
        config
    }

    #[test]
    fn normalize_base_path_strips_trailing_slashes() {
        assert_eq!(normalize_base_path("admin/"), "/admin");
        assert_eq!(normalize_base_path("admin///"), "/admin");
    }

    #[test]
    fn normalize_base_path_adds_leading_slash() {
        assert_eq!(normalize_base_path("admin"), "/admin");
    }

    #[test]
    fn normalize_base_path_empty_stays_empty() {
        assert_eq!(normalize_base_path(""), "");
        assert_eq!(normalize_base_path("   "), "");
        assert_eq!(normalize_base_path("/"), "");
    }

    #[test]
    fn normalize_base_path_preserves_inner() {
        assert_eq!(normalize_base_path("/foo/bar/"), "/foo/bar");
    }

    #[test]
    fn trim_trailing_slash_removes_trailing() {
        assert_eq!(
            trim_trailing_slash("https://example.com/"),
            "https://example.com"
        );
        assert_eq!(
            trim_trailing_slash("https://example.com"),
            "https://example.com"
        );
    }

    #[test]
    fn trim_trailing_slash_empty_string() {
        assert_eq!(trim_trailing_slash(""), "");
        assert_eq!(trim_trailing_slash("/"), "");
    }

    #[test]
    fn runtime_env_from_env_value() {
        assert_eq!(
            RuntimeEnv::from_env_value("production"),
            RuntimeEnv::Production
        );
        assert_eq!(RuntimeEnv::from_env_value("test"), RuntimeEnv::Test);
        assert_eq!(
            RuntimeEnv::from_env_value("development"),
            RuntimeEnv::Development
        );
        assert_eq!(
            RuntimeEnv::from_env_value("anything"),
            RuntimeEnv::Development
        );
    }

    #[test]
    fn is_production_returns_true_for_production() {
        let config = AdminConfig {
            env: RuntimeEnv::Production,
            host: String::new(),
            port: 3020,
            secret_key_base: String::new(),
            base_path: String::new(),
            api_endpoint: String::new(),
            media_endpoint: String::new(),
            static_cdn_endpoint: String::new(),

            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            kv_url: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: String::new(),
            release_channel: String::new(),
            self_hosted: false,
            proxy: ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: String::new(),
            },
        };
        assert!(config.is_production());
        assert!(!config.is_dev());
    }

    #[test]
    fn is_dev_returns_true_for_development() {
        let config = AdminConfig {
            env: RuntimeEnv::Development,
            host: String::new(),
            port: 3020,
            secret_key_base: String::new(),
            base_path: String::new(),
            api_endpoint: String::new(),
            media_endpoint: String::new(),
            static_cdn_endpoint: String::new(),

            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            kv_url: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: String::new(),
            release_channel: String::new(),
            self_hosted: false,
            proxy: ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: String::new(),
            },
        };
        assert!(config.is_dev());
        assert!(!config.is_production());
    }

    #[test]
    fn from_env_uses_defaults() {
        let config = config_from_env(&[]);
        assert_eq!(config.env, RuntimeEnv::Development);
        assert_eq!(config.host, "0.0.0.0");
        assert_eq!(config.port, 3020);
        assert_eq!(config.oauth_client_id, DEFAULT_ADMIN_OAUTH_CLIENT_ID);
        assert_eq!(
            config.oauth_redirect_uri,
            "https://admin.fluxer.app/oauth2_callback"
        );
    }

    #[test]
    fn a_non_default_public_port_reaches_the_public_endpoints() {
        let config = config_from_env(&[
            ("FLUXER_BASE_DOMAIN", "fluxer.example"),
            ("FLUXER_PUBLIC_PORT", "19080"),
            ("FLUXER_ADMIN_ENDPOINT", "http://fluxer.example/admin"),
            ("FLUXER_APP_ENDPOINT", "http://fluxer.example:19080"),
            ("FLUXER_MEDIA_ENDPOINT", "http://fluxer.example/media"),
            ("FLUXER_STATIC_CDN_ENDPOINT", "https://cdn.example.net"),
            (
                "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
                "http://fluxer.example/admin/oauth2_callback",
            ),
        ]);

        assert_eq!(config.admin_endpoint, "http://fluxer.example:19080/admin");
        assert_eq!(config.media_endpoint, "http://fluxer.example:19080/media");
        assert_eq!(config.web_app_endpoint, "http://fluxer.example:19080");
        assert_eq!(config.static_cdn_endpoint, "https://cdn.example.net");
        assert_eq!(
            config.oauth_redirect_uri,
            format!("{}/oauth2_callback", config.admin_endpoint)
        );
    }

    #[test]
    fn a_default_public_port_leaves_the_public_endpoints_alone() {
        let config = config_from_env(&[
            ("FLUXER_BASE_DOMAIN", "fluxer.example"),
            ("FLUXER_PUBLIC_PORT", "443"),
            ("FLUXER_ADMIN_ENDPOINT", "https://fluxer.example/admin"),
            ("FLUXER_APP_ENDPOINT", "https://fluxer.example"),
            ("FLUXER_MEDIA_ENDPOINT", "https://fluxer.example/media"),
            ("FLUXER_STATIC_CDN_ENDPOINT", "https://fluxer.example"),
            (
                "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
                "https://fluxer.example/admin/oauth2_callback",
            ),
        ]);

        assert_eq!(config.admin_endpoint, "https://fluxer.example/admin");
        assert_eq!(config.media_endpoint, "https://fluxer.example/media");
        assert_eq!(config.web_app_endpoint, "https://fluxer.example");
        assert_eq!(config.static_cdn_endpoint, "https://fluxer.example");
        assert_eq!(
            config.oauth_redirect_uri,
            "https://fluxer.example/admin/oauth2_callback"
        );
    }

    #[test]
    fn the_oauth_redirect_uri_matches_the_api_derived_admin_endpoint() {
        let config = config_from_env(&[
            ("FLUXER_BASE_DOMAIN", "fluxer.example"),
            ("FLUXER_PUBLIC_PORT", "19080"),
            ("FLUXER_ADMIN_ENDPOINT", "http://fluxer.example/admin"),
            (
                "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
                "http://fluxer.example/admin/oauth2_callback",
            ),
        ]);

        let api_admin_endpoint = fluxer_common::config::normalize_public_endpoint(
            "http://fluxer.example/admin",
            "fluxer.example",
            Some(19080),
        );
        assert_eq!(
            config.oauth_redirect_uri,
            format!("{api_admin_endpoint}/oauth2_callback")
        );
    }
}
