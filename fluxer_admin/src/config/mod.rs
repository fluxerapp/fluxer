// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::{
    env_value, normalize_base_path, normalize_public_endpoint_from_env, read_bool_env, read_env,
    read_first_env, trim_trailing_slash,
};

const DEFAULT_ADMIN_OAUTH_CLIENT_ID: &str = "1234567890123456789";
const DEFAULT_REPORTS_BUCKET_ORIGIN: &str = "https://fluxer-reports.ewr1.vultrobjects.com";

#[derive(Clone, Debug)]
pub struct AdminConfig {
    pub host: String,
    pub port: u16,
    pub secret_key_base: String,
    pub base_path: String,
    pub api_endpoint: String,
    pub api_public_endpoint: String,
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
            api_public_endpoint: normalize_public_endpoint_from_env(&trim_trailing_slash(
                &read_env("FLUXER_API_PUBLIC_ENDPOINT", ""),
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

    pub fn secure_cookies(&self) -> bool {
        self.admin_endpoint.starts_with("https://")
    }

    pub fn admin_origin(&self) -> Option<String> {
        endpoint_origin(&self.admin_endpoint)
    }

    pub fn image_origins(&self) -> Vec<String> {
        let api = if self.api_public_endpoint.is_empty() {
            &self.api_endpoint
        } else {
            &self.api_public_endpoint
        };
        let mut origins = Vec::new();
        for origin in [
            &self.static_cdn_endpoint,
            &self.media_endpoint,
            &self.web_app_endpoint,
            &self.reports_bucket_origin,
            api,
        ]
        .into_iter()
        .filter_map(|endpoint| endpoint_origin(endpoint))
        {
            if !origins.contains(&origin) {
                origins.push(origin);
            }
        }
        origins
    }

    pub fn shows_image(&self, image_url: &str) -> bool {
        let Ok(url) = url::Url::parse(image_url) else {
            return image_url.starts_with('/') && !image_url.starts_with("//");
        };
        if matches!(url.scheme(), "data" | "blob") {
            return true;
        }
        let origin = url.origin();
        if !origin.is_tuple() {
            return false;
        }
        let origin = origin.ascii_serialization();
        self.admin_origin().as_ref() == Some(&origin) || self.image_origins().contains(&origin)
    }
}

fn endpoint_origin(endpoint: &str) -> Option<String> {
    let origin = url::Url::parse(endpoint).ok()?.origin();
    origin.is_tuple().then(|| origin.ascii_serialization())
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
