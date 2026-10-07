// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::{
    env_value, normalize_base_path, normalize_public_endpoint_from_env, read_bool_env, read_env,
    read_first_env, trim_trailing_slash,
};

const DEFAULT_ADMIN_OAUTH_CLIENT_ID: &str = "1234567890123456789";
const DEFAULT_REPORTS_BUCKET_ORIGIN: &str = "https://fluxer-reports.ewr1.vultrobjects.com";

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
    pub reports_bucket_origin: String,
    pub admin_endpoint: String,
    pub web_app_endpoint: String,
    pub oauth_client_id: String,
    pub oauth_client_secret: String,
    pub oauth_redirect_uri: String,
    pub build_version: String,
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
        let oauth_redirect_uri = normalize_public_endpoint_from_env(&read_env(
            "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
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
            reports_bucket_origin: reports_bucket_origin_from_env(),

            admin_endpoint,
            web_app_endpoint: normalize_public_endpoint_from_env(&trim_trailing_slash(&read_env(
                "FLUXER_APP_ENDPOINT",
                "https://app.fluxer.app",
            ))),
            oauth_client_id: read_env(
                "FLUXER_ADMIN_OAUTH_CLIENT_ID",
                DEFAULT_ADMIN_OAUTH_CLIENT_ID,
            ),
            oauth_client_secret: read_env("FLUXER_ADMIN_OAUTH_CLIENT_SECRET", ""),
            oauth_redirect_uri,
            build_version: read_first_env(
                &["BUILD_VERSION", "FLUXER_BUILD_VERSION"],
                env!("CARGO_PKG_VERSION"),
            ),
            self_hosted: read_bool_env("FLUXER_SELF_HOSTED", false),
            proxy: ProxyConfig {
                trust_client_ip_header: read_bool_env("FLUXER_TRUST_CLIENT_IP_HEADER", false),
                client_ip_header_name: read_env("FLUXER_CLIENT_IP_HEADER_NAME", "x-forwarded-for")
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

    pub fn admin_origin(&self) -> Option<String> {
        let origin = url::Url::parse(&self.admin_endpoint).ok()?.origin();
        origin.is_tuple().then(|| origin.ascii_serialization())
    }
}

fn reports_bucket_origin_from_env() -> String {
    let public_endpoint = env_value("FLUXER_S3_PUBLIC_ENDPOINT")
        .map(|value| normalize_public_endpoint_from_env(value.trim()));
    let endpoint = env_value("FLUXER_S3_ENDPOINT");
    presign_endpoint(
        public_endpoint.as_deref(),
        endpoint.as_deref(),
        &read_env("FLUXER_S3_BUCKET_UPLOADS", "fluxer-uploads"),
    )
    .and_then(|endpoint| {
        bucket_origin(
            &endpoint,
            read_bool_env("FLUXER_S3_FORCE_PATH_STYLE", false),
            &read_env("FLUXER_S3_BUCKET_REPORTS", "fluxer-reports"),
        )
    })
    .unwrap_or_else(|| DEFAULT_REPORTS_BUCKET_ORIGIN.to_owned())
}

fn presign_endpoint(
    public_endpoint: Option<&str>,
    endpoint: Option<&str>,
    uploads_bucket: &str,
) -> Option<url::Url> {
    let Some(public_endpoint) = public_endpoint else {
        return url::Url::parse(endpoint?.trim()).ok();
    };
    let mut parsed = url::Url::parse(public_endpoint).ok()?;
    let host = parsed.host_str()?.to_owned();
    if let Some(shared_host) = host.strip_prefix(&format!("{uploads_bucket}.")) {
        parsed.set_host(Some(shared_host)).ok()?;
    }
    Some(parsed)
}

fn bucket_origin(endpoint: &url::Url, force_path_style: bool, bucket: &str) -> Option<String> {
    if !matches!(endpoint.scheme(), "http" | "https") {
        return None;
    }
    let path_style = force_path_style
        || !matches!(endpoint.host(), Some(url::Host::Domain(_)))
        || !is_virtual_hostable_bucket(bucket, endpoint.scheme() == "http");
    if path_style {
        return Some(endpoint.origin().ascii_serialization());
    }
    let mut virtual_host = endpoint.clone();
    virtual_host
        .set_host(Some(&format!("{bucket}.{}", endpoint.host_str()?)))
        .ok()?;
    Some(virtual_host.origin().ascii_serialization())
}

fn is_virtual_hostable_bucket(bucket: &str, allow_dots: bool) -> bool {
    if allow_dots && bucket.contains('.') {
        return bucket
            .split('.')
            .all(|label| is_virtual_hostable_bucket(label, false));
    }
    (3..=63).contains(&bucket.len())
        && bucket
            .bytes()
            .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-')
        && !bucket.starts_with('-')
        && !bucket.ends_with('-')
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::sync::Mutex;

    static ENV_LOCK: Mutex<()> = Mutex::new(());

    const MANAGED_ENV: [&str; 15] = [
        "FLUXER_ENV",
        "FLUXER_ADMIN_HOST",
        "FLUXER_ADMIN_PORT",
        "FLUXER_ADMIN_ENDPOINT",
        "FLUXER_ADMIN_OAUTH_CLIENT_ID",
        "FLUXER_ADMIN_OAUTH_REDIRECT_URI",
        "FLUXER_APP_ENDPOINT",
        "FLUXER_MEDIA_ENDPOINT",
        "FLUXER_STATIC_CDN_ENDPOINT",
        "FLUXER_BASE_DOMAIN",
        "FLUXER_S3_ENDPOINT",
        "FLUXER_S3_PUBLIC_ENDPOINT",
        "FLUXER_S3_FORCE_PATH_STYLE",
        "FLUXER_S3_BUCKET_UPLOADS",
        "FLUXER_S3_BUCKET_REPORTS",
    ];

    fn config_from_env(vars: &[(&str, &str)]) -> AdminConfig {
        let _guard = ENV_LOCK.lock().unwrap();
        for name in MANAGED_ENV {
            unsafe { env::remove_var(name) };
        }
        unsafe { env::remove_var("FLUXER_PUBLIC_PORT") };
        unsafe { env::remove_var("FLUXER_PUBLIC_ORIGIN") };
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
            reports_bucket_origin: String::new(),

            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: String::new(),
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
            reports_bucket_origin: String::new(),

            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: String::new(),
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
            ("FLUXER_APP_ENDPOINT", "http://fluxer.example"),
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

    fn origin_for(endpoint: &str, force_path_style: bool, bucket: &str) -> Option<String> {
        bucket_origin(
            &url::Url::parse(endpoint).expect("valid endpoint"),
            force_path_style,
            bucket,
        )
    }

    #[test]
    fn bucket_origin_matches_the_addressing_of_presigned_urls() {
        let cases = [
            (
                "https://ewr1.vultrobjects.com",
                false,
                "fluxer-reports",
                "https://fluxer-reports.ewr1.vultrobjects.com",
            ),
            (
                "https://ewr1.vultrobjects.com/",
                true,
                "fluxer-reports",
                "https://ewr1.vultrobjects.com",
            ),
            (
                "http://seaweedfs:8333",
                true,
                "fluxer-reports",
                "http://seaweedfs:8333",
            ),
            (
                "http://seaweedfs:8333",
                false,
                "fluxer-reports",
                "http://fluxer-reports.seaweedfs:8333",
            ),
            (
                "http://127.0.0.1:8333",
                false,
                "fluxer-reports",
                "http://127.0.0.1:8333",
            ),
            (
                "http://[::1]:8333",
                false,
                "fluxer-reports",
                "http://[::1]:8333",
            ),
            (
                "https://s3.example.com:9000",
                false,
                "fluxer-reports",
                "https://fluxer-reports.s3.example.com:9000",
            ),
            (
                "https://s3.example.com:443/base/path",
                false,
                "fluxer-reports",
                "https://fluxer-reports.s3.example.com",
            ),
            (
                "https://S3.Example.com",
                false,
                "fluxer-reports",
                "https://fluxer-reports.s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "reports.example",
                "https://s3.example.com",
            ),
            (
                "http://s3.example.com",
                false,
                "reports.example",
                "http://reports.example.s3.example.com",
            ),
            (
                "http://s3.example.com",
                false,
                "a.example",
                "http://s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "Reports",
                "https://s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "ab",
                "https://s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "reports_bucket",
                "https://s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "-reports",
                "https://s3.example.com",
            ),
            (
                "https://s3.example.com",
                false,
                "192.168.1.1",
                "https://s3.example.com",
            ),
        ];
        for (endpoint, force_path_style, bucket, expected) in cases {
            assert_eq!(
                origin_for(endpoint, force_path_style, bucket).as_deref(),
                Some(expected),
                "{endpoint} {force_path_style} {bucket}"
            );
        }
        assert_eq!(origin_for("ftp://s3.example.com", true, "reports"), None);
    }

    #[test]
    fn presign_endpoint_prefers_the_public_endpoint_and_drops_the_uploads_bucket_host() {
        let host = |public: Option<&str>, endpoint: Option<&str>| {
            presign_endpoint(public, endpoint, "fluxer-uploads").map(|url| url.to_string())
        };
        assert_eq!(
            host(
                Some("https://fluxer-uploads.ewr1.vultrobjects.com"),
                Some("https://internal.example")
            )
            .as_deref(),
            Some("https://ewr1.vultrobjects.com/")
        );
        assert_eq!(
            host(
                Some("https://cdn.example.com"),
                Some("http://seaweedfs:8333")
            )
            .as_deref(),
            Some("https://cdn.example.com/")
        );
        assert_eq!(
            host(None, Some("http://seaweedfs:8333")).as_deref(),
            Some("http://seaweedfs:8333/")
        );
        assert_eq!(host(Some("not a url"), Some("http://seaweedfs:8333")), None);
        assert_eq!(host(None, None), None);
    }

    #[test]
    fn the_reports_bucket_origin_defaults_to_the_hosted_bucket() {
        let config = config_from_env(&[]);
        assert_eq!(config.reports_bucket_origin, DEFAULT_REPORTS_BUCKET_ORIGIN);

        let config = config_from_env(&[("FLUXER_S3_ENDPOINT", "not a url")]);
        assert_eq!(config.reports_bucket_origin, DEFAULT_REPORTS_BUCKET_ORIGIN);
    }

    #[test]
    fn the_reports_bucket_origin_follows_the_object_store_settings() {
        let hosted = config_from_env(&[
            ("FLUXER_S3_ENDPOINT", "https://ewr1.vultrobjects.com"),
            (
                "FLUXER_S3_PUBLIC_ENDPOINT",
                "https://fluxer-uploads.ewr1.vultrobjects.com",
            ),
        ]);
        assert_eq!(hosted.reports_bucket_origin, DEFAULT_REPORTS_BUCKET_ORIGIN);

        let bundled = config_from_env(&[
            ("FLUXER_S3_ENDPOINT", "http://seaweedfs:8333"),
            (
                "FLUXER_S3_PUBLIC_ENDPOINT",
                "https://objects.fluxer.example",
            ),
            ("FLUXER_S3_FORCE_PATH_STYLE", "true"),
        ]);
        assert_eq!(
            bundled.reports_bucket_origin,
            "https://objects.fluxer.example"
        );

        let internal_only = config_from_env(&[
            ("FLUXER_S3_ENDPOINT", "http://seaweedfs:8333"),
            ("FLUXER_S3_FORCE_PATH_STYLE", "true"),
        ]);
        assert_eq!(internal_only.reports_bucket_origin, "http://seaweedfs:8333");

        let outside = config_from_env(&[
            (
                "FLUXER_S3_ENDPOINT",
                "https://s3.eu-central-1.amazonaws.com",
            ),
            ("FLUXER_S3_FORCE_PATH_STYLE", "false"),
            ("FLUXER_S3_BUCKET_UPLOADS", "example-uploads"),
            ("FLUXER_S3_BUCKET_REPORTS", "example-reports"),
        ]);
        assert_eq!(
            outside.reports_bucket_origin,
            "https://example-reports.s3.eu-central-1.amazonaws.com"
        );

        let renamed_uploads = config_from_env(&[
            ("FLUXER_S3_ENDPOINT", "https://s3.example.com"),
            (
                "FLUXER_S3_PUBLIC_ENDPOINT",
                "https://example-uploads.s3.example.com",
            ),
            ("FLUXER_S3_BUCKET_UPLOADS", "example-uploads"),
            ("FLUXER_S3_BUCKET_REPORTS", "example-reports"),
        ]);
        assert_eq!(
            renamed_uploads.reports_bucket_origin,
            "https://example-reports.s3.example.com"
        );
    }

    #[test]
    fn a_non_default_public_port_reaches_the_reports_bucket_origin() {
        let config = config_from_env(&[
            ("FLUXER_BASE_DOMAIN", "fluxer.example"),
            ("FLUXER_PUBLIC_PORT", "19080"),
            ("FLUXER_S3_ENDPOINT", "http://seaweedfs:8333"),
            ("FLUXER_S3_PUBLIC_ENDPOINT", "http://fluxer.example"),
            ("FLUXER_S3_FORCE_PATH_STYLE", "true"),
        ]);
        assert_eq!(config.reports_bucket_origin, "http://fluxer.example:19080");
    }
}
