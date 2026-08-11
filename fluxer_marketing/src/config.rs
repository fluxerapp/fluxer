// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::{self as cfg, GeoipS3Config, GeoipSourceConfig};
use std::env;

const DEFAULT_SECRET_KEY_BASE: &str = "development-marketing-secret";

#[derive(Clone, Debug)]
pub struct MarketingConfig {
    pub env: RuntimeEnv,
    pub host: String,
    pub port: u16,
    pub secret_key_base: String,
    pub base_path: String,
    pub api_endpoint: String,
    pub static_cdn_endpoint: String,
    pub marketing_endpoint: String,
    pub geoip_db_path: String,
    pub geoip_source: GeoipSourceConfig,
    pub geoip_s3_config: Option<GeoipS3Config>,
    pub trust_client_ip_header: bool,
    pub client_ip_header_name: String,
    pub release_channel: ReleaseChannel,
    pub build_version: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeEnv {
    Development,
    Production,
    Test,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReleaseChannel {
    Stable,
    Canary,
}

pub const DOWNLOAD_RELEASE_CHANNEL: ReleaseChannel = ReleaseChannel::Canary;

impl MarketingConfig {
    pub fn from_env() -> Self {
        let geoip_source = cfg::parse_geoip_source_config(
            &cfg::read_first_env(&["FLUXER_GEOIP_DB_PATH", "MAXMIND_DB_PATH"], ""),
            "marketing",
        );
        let geoip_s3_config = cfg::read_geoip_s3_config_from_env(&geoip_source);
        let geoip_db_path = geoip_source.maxmind_db_path().unwrap_or_default();
        let env = RuntimeEnv::from_env_value(&cfg::read_env("FLUXER_ENV", "development"));
        let secret_key_base =
            cfg::read_env("FLUXER_MARKETING_SECRET_KEY_BASE", DEFAULT_SECRET_KEY_BASE);
        if env == RuntimeEnv::Production && secret_key_base == DEFAULT_SECRET_KEY_BASE {
            panic!(
                "FLUXER_MARKETING_SECRET_KEY_BASE must be set to a non-default value in production"
            );
        }
        Self {
            env,
            host: cfg::read_env("FLUXER_MARKETING_HOST", "0.0.0.0"),
            port: cfg::read_env("FLUXER_MARKETING_PORT", "3010")
                .parse()
                .unwrap_or(3010),
            secret_key_base,
            base_path: cfg::normalize_base_path(&cfg::read_env("FLUXER_MARKETING_BASE_PATH", "")),
            api_endpoint: cfg::trim_trailing_slash(&cfg::read_env(
                "FLUXER_API_ENDPOINT",
                "https://api.fluxer.app",
            )),
            static_cdn_endpoint: cfg::trim_trailing_slash(&cfg::read_env(
                "FLUXER_STATIC_CDN_ENDPOINT",
                "",
            )),
            marketing_endpoint: cfg::trim_trailing_slash(&cfg::read_env(
                "FLUXER_MARKETING_ENDPOINT",
                "https://fluxer.app",
            )),
            geoip_db_path,
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
            release_channel: ReleaseChannel::from_env_value(&cfg::read_env_preferred(
                &["RELEASE_CHANNEL", "FLUXER_RELEASE_CHANNEL"],
                "stable",
            )),
            build_version: cfg::read_env_preferred(
                &["BUILD_VERSION", "FLUXER_BUILD_VERSION"],
                env!("CARGO_PKG_VERSION"),
            ),
        }
    }

    pub fn base_url(&self) -> String {
        if self.base_path.is_empty() {
            return self.marketing_endpoint.clone();
        }
        if self.marketing_endpoint.ends_with(&self.base_path) {
            return self.marketing_endpoint.clone();
        }
        format!("{}{}", self.marketing_endpoint, self.base_path)
    }

    pub fn is_dev(&self) -> bool {
        self.env == RuntimeEnv::Development
    }

    pub fn is_canary(&self) -> bool {
        self.release_channel == ReleaseChannel::Canary
    }
}

impl RuntimeEnv {
    fn from_env_value(value: &str) -> Self {
        match value {
            "production" => Self::Production,
            "test" => Self::Test,
            _ => Self::Development,
        }
    }
}

impl ReleaseChannel {
    fn from_env_value(value: &str) -> Self {
        if value.eq_ignore_ascii_case("canary") {
            Self::Canary
        } else {
            Self::Stable
        }
    }

    pub const fn is_canary(self) -> bool {
        matches!(self, Self::Canary)
    }

    pub const fn segment(self) -> &'static str {
        match self {
            Self::Stable => "stable",
            Self::Canary => "canary",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_geoip_s3_source_like_typescript_startup() {
        let source = cfg::parse_geoip_source_config(
            "s3://geoip/GeoLite2-City.mmdb?download_path=/tmp/city.mmdb&asn_key=GeoLite2-ASN.mmdb",
            "marketing",
        );
        assert_eq!(
            source,
            GeoipSourceConfig::S3 {
                maxmind_db_path: "/tmp/fluxer/geoip/marketing/city.mmdb".to_owned(),
                maxmind_asn_db_path: Some(
                    "/tmp/fluxer/geoip/marketing/GeoLite2-ASN.mmdb".to_owned()
                ),
                s3_bucket: "geoip".to_owned(),
                s3_key: "GeoLite2-City.mmdb".to_owned(),
                s3_asn_key: Some("GeoLite2-ASN.mmdb".to_owned()),
            }
        );
    }
}
