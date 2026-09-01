// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{env, path::Path};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GeoipSourceConfig {
    Filesystem {
        maxmind_db_path: Option<String>,
    },
    S3 {
        maxmind_db_path: String,
        maxmind_asn_db_path: Option<String>,
        s3_bucket: String,
        s3_key: String,
        s3_asn_key: Option<String>,
    },
}

impl GeoipSourceConfig {
    pub fn maxmind_db_path(&self) -> Option<String> {
        match self {
            Self::Filesystem { maxmind_db_path } => maxmind_db_path.clone(),
            Self::S3 {
                maxmind_db_path, ..
            } => Some(maxmind_db_path.clone()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeoipS3Config {
    pub endpoint: String,
    pub region: String,
    pub access_key_id: String,
    pub secret_access_key: String,
}

pub fn read_geoip_s3_config_from_env(source: &GeoipSourceConfig) -> Option<GeoipS3Config> {
    read_geoip_s3_config(source, |name| env::var(name).ok())
}

fn read_geoip_s3_config<F>(source: &GeoipSourceConfig, mut read_var: F) -> Option<GeoipS3Config>
where
    F: FnMut(&str) -> Option<String>,
{
    match source {
        GeoipSourceConfig::S3 { .. } => Some(GeoipS3Config {
            endpoint: read_var("FLUXER_S3_ENDPOINT").unwrap_or_default(),
            region: read_var("FLUXER_S3_REGION").unwrap_or_default(),
            access_key_id: read_var("FLUXER_S3_ACCESS_KEY_ID").unwrap_or_default(),
            secret_access_key: read_var("FLUXER_S3_SECRET_ACCESS_KEY").unwrap_or_default(),
        }),
        GeoipSourceConfig::Filesystem { .. } => None,
    }
}

pub fn parse_geoip_source_config(raw_value: &str, service_name: &str) -> GeoipSourceConfig {
    let trimmed = raw_value.trim();
    if trimmed.is_empty() {
        return GeoipSourceConfig::Filesystem {
            maxmind_db_path: None,
        };
    }
    if !trimmed.starts_with("s3://") {
        return GeoipSourceConfig::Filesystem {
            maxmind_db_path: Some(trimmed.to_owned()),
        };
    }
    parse_geoip_s3_source_config(trimmed, service_name)
}

fn parse_geoip_s3_source_config(raw_value: &str, service_name: &str) -> GeoipSourceConfig {
    let url = reqwest::Url::parse(raw_value)
        .unwrap_or_else(|err| panic!("invalid GeoIP S3 URL {}: {}", raw_value, err));
    let s3_bucket = url.host_str().unwrap_or("").to_owned();
    if s3_bucket.is_empty() {
        panic!("invalid GeoIP S3 URL (missing bucket): {raw_value}");
    }
    let s3_key = percent_decode(url.path().trim_start_matches('/'));
    if s3_key.is_empty() {
        panic!("invalid GeoIP S3 URL (missing object key): {raw_value}");
    }
    let maxmind_db_path =
        geoip_runtime_path(&resolve_geoip_download_path(&url, raw_value), service_name);
    let s3_asn_key = url
        .query_pairs()
        .find(|(key, _)| key == "asn_key")
        .map(|(_, value)| value.into_owned());
    let maxmind_asn_db_path = s3_asn_key.as_ref().map(|asn_key| {
        let configured_path = url
            .query_pairs()
            .find(|(key, _)| key == "asn_download_path")
            .map(|(_, value)| require_absolute_path(value.as_ref(), "asn_download_path", raw_value))
            .unwrap_or_else(|| {
                let directory = Path::new(&maxmind_db_path)
                    .parent()
                    .map(Path::to_path_buf)
                    .unwrap_or_default();
                directory
                    .join(Path::new(asn_key).file_name().unwrap_or_default())
                    .to_string_lossy()
                    .into_owned()
            });
        geoip_runtime_path(&configured_path, service_name)
    });
    GeoipSourceConfig::S3 {
        maxmind_db_path,
        maxmind_asn_db_path,
        s3_bucket,
        s3_key,
        s3_asn_key,
    }
}

fn resolve_geoip_download_path(url: &reqwest::Url, raw_value: &str) -> String {
    let Some(download_path) = url
        .query_pairs()
        .find(|(key, _)| key == "download_path")
        .map(|(_, value)| value.into_owned())
    else {
        panic!("invalid GeoIP S3 URL (missing query parameter \"download_path\"): {raw_value}");
    };
    require_absolute_path(&download_path, "download_path", raw_value)
}

fn require_absolute_path(value: &str, param: &str, raw_value: &str) -> String {
    if !Path::new(value).is_absolute() {
        panic!("GeoIP S3 URL query parameter \"{param}\" must be an absolute path: {raw_value}");
    }
    value.to_owned()
}

fn geoip_runtime_path(configured_path: &str, service_name: &str) -> String {
    let basename = Path::new(configured_path)
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or("GeoLite2-City.mmdb");
    Path::new("/tmp/fluxer/geoip")
        .join(service_name)
        .join(basename)
        .to_string_lossy()
        .into_owned()
}

fn percent_decode(value: &str) -> String {
    urlencoding::decode(value)
        .map(|value| value.into_owned())
        .unwrap_or_else(|_| value.to_owned())
}

pub fn read_env(name: &str, fallback: &str) -> String {
    env::var(name).unwrap_or_else(|_| fallback.to_owned())
}

pub fn read_env_preferred(names: &[&str], fallback: &str) -> String {
    names
        .iter()
        .find_map(|name| env::var(name).ok().filter(|value| !value.trim().is_empty()))
        .unwrap_or_else(|| fallback.to_owned())
}

pub fn read_first_env(names: &[&str], fallback: &str) -> String {
    names
        .iter()
        .find_map(|name| env::var(name).ok())
        .unwrap_or_else(|| fallback.to_owned())
}

pub fn read_bool_env(names: &[&str], fallback: bool) -> bool {
    let Some(value) = names.iter().find_map(|name| env::var(name).ok()) else {
        return fallback;
    };
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

pub fn non_empty_env(name: &str) -> Option<String> {
    env::var(name)
        .ok()
        .map(|v| v.trim().to_owned())
        .filter(|v| !v.is_empty())
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

fn is_default_port(scheme: &str, port: u16) -> bool {
    matches!(
        (scheme, port),
        ("http", 80) | ("https", 443) | ("ws", 80) | ("wss", 443)
    )
}

fn strip_trailing_dot(host: &str) -> &str {
    host.strip_suffix('.').unwrap_or(host)
}

pub fn normalize_public_endpoint(url: &str, base_domain: &str, public_port: Option<u16>) -> String {
    let domain = base_domain.trim().to_lowercase();
    let domain = strip_trailing_dot(&domain);
    let Some(port) = public_port.filter(|port| *port != 0) else {
        return url.to_owned();
    };
    if domain.is_empty() {
        return url.to_owned();
    }
    let Ok(parsed) = reqwest::Url::parse(url) else {
        return url.to_owned();
    };
    let host = parsed.host_str().unwrap_or_default().to_lowercase();
    if strip_trailing_dot(&host) != domain {
        return url.to_owned();
    }
    if is_default_port(parsed.scheme(), port) {
        return url.to_owned();
    }
    let Some(scheme_end) = url.find("://") else {
        return url.to_owned();
    };
    let authority_start = scheme_end + 3;
    if url[authority_start..].starts_with('/') {
        return url.to_owned();
    }
    let authority_end = url[authority_start..]
        .find(['/', '\\', '?', '#'])
        .map_or(url.len(), |index| authority_start + index);
    let authority = &url[authority_start..authority_end];
    let host = authority.rsplit('@').next().unwrap_or_default();
    if host[host.rfind(']').map_or(0, |index| index + 1)..].contains(':') {
        return url.to_owned();
    }
    format!("{}:{port}{}", &url[..authority_end], &url[authority_end..])
}

pub fn normalize_public_endpoint_from_env(url: &str) -> String {
    normalize_public_endpoint(
        url,
        &read_env("FLUXER_BASE_DOMAIN", ""),
        non_empty_env("FLUXER_PUBLIC_PORT").and_then(|port| port.parse().ok()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_filesystem_geoip_source() {
        let source = parse_geoip_source_config("/data/GeoLite2-City.mmdb", "test");
        assert_eq!(
            source,
            GeoipSourceConfig::Filesystem {
                maxmind_db_path: Some("/data/GeoLite2-City.mmdb".to_owned()),
            }
        );
    }

    #[test]
    fn parses_empty_geoip_source() {
        let source = parse_geoip_source_config("", "test");
        assert_eq!(
            source,
            GeoipSourceConfig::Filesystem {
                maxmind_db_path: None,
            }
        );
    }

    #[test]
    fn parses_s3_geoip_source() {
        let source = parse_geoip_source_config(
            "s3://geoip/GeoLite2-City.mmdb?download_path=/tmp/city.mmdb&asn_key=GeoLite2-ASN.mmdb",
            "test_svc",
        );
        assert_eq!(
            source,
            GeoipSourceConfig::S3 {
                maxmind_db_path: "/tmp/fluxer/geoip/test_svc/city.mmdb".to_owned(),
                maxmind_asn_db_path: Some(
                    "/tmp/fluxer/geoip/test_svc/GeoLite2-ASN.mmdb".to_owned()
                ),
                s3_bucket: "geoip".to_owned(),
                s3_key: "GeoLite2-City.mmdb".to_owned(),
                s3_asn_key: Some("GeoLite2-ASN.mmdb".to_owned()),
            }
        );
    }

    #[test]
    fn reads_geoip_s3_config_only_for_s3_source() {
        let source = parse_geoip_source_config(
            "s3://geoip/GeoLite2-City.mmdb?download_path=/tmp/city.mmdb",
            "test_svc",
        );
        let config = read_geoip_s3_config(&source, |name| {
            Some(
                match name {
                    "FLUXER_S3_ENDPOINT" => "https://s3.example.test",
                    "FLUXER_S3_REGION" => "ewr1",
                    "FLUXER_S3_ACCESS_KEY_ID" => "access",
                    "FLUXER_S3_SECRET_ACCESS_KEY" => "secret",
                    _ => "",
                }
                .to_owned(),
            )
        })
        .expect("s3 source should read s3 config");

        assert_eq!(
            config,
            GeoipS3Config {
                endpoint: "https://s3.example.test".to_owned(),
                region: "ewr1".to_owned(),
                access_key_id: "access".to_owned(),
                secret_access_key: "secret".to_owned(),
            }
        );

        let filesystem_source = parse_geoip_source_config("/tmp/city.mmdb", "test_svc");
        assert!(read_geoip_s3_config(&filesystem_source, |_| None).is_none());
    }

    #[test]
    fn normalize_base_path_strips_slashes() {
        assert_eq!(normalize_base_path("/foo/bar/"), "/foo/bar");
        assert_eq!(normalize_base_path("foo"), "/foo");
        assert_eq!(normalize_base_path(""), "");
        assert_eq!(normalize_base_path("/"), "");
    }

    #[test]
    fn default_https_install_is_untouched() {
        for url in [
            "https://fluxer.example",
            "https://fluxer.example/media",
            "https://fluxer.example/admin/oauth2_callback",
            "wss://fluxer.example/gateway",
        ] {
            assert_eq!(
                url,
                normalize_public_endpoint(url, "fluxer.example", Some(443))
            );
        }
    }

    #[test]
    fn default_http_install_is_untouched() {
        for url in [
            "http://fluxer.example",
            "http://fluxer.example/media",
            "ws://fluxer.example/gateway",
        ] {
            assert_eq!(
                url,
                normalize_public_endpoint(url, "fluxer.example", Some(80))
            );
        }
    }

    #[test]
    fn inserts_a_non_default_port_for_the_base_domain() {
        assert_eq!(
            "http://fluxer.example:19080/media",
            normalize_public_endpoint("http://fluxer.example/media", "fluxer.example", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example:19080",
            normalize_public_endpoint("http://fluxer.example", "fluxer.example", Some(19080))
        );
        assert_eq!(
            "https://fluxer.example:8443/admin/oauth2_callback",
            normalize_public_endpoint(
                "https://fluxer.example/admin/oauth2_callback",
                "fluxer.example",
                Some(8443)
            )
        );
    }

    #[test]
    fn a_default_port_for_the_urls_own_scheme_is_never_inserted() {
        assert_eq!(
            "ws://fluxer.example/gateway",
            normalize_public_endpoint("ws://fluxer.example/gateway", "fluxer.example", Some(80))
        );
        assert_eq!(
            "wss://fluxer.example/gateway",
            normalize_public_endpoint("wss://fluxer.example/gateway", "fluxer.example", Some(443))
        );
        assert_eq!(
            "http://fluxer.example:443/media",
            normalize_public_endpoint("http://fluxer.example/media", "fluxer.example", Some(443))
        );
        assert_eq!(
            "https://fluxer.example:80/media",
            normalize_public_endpoint("https://fluxer.example/media", "fluxer.example", Some(80))
        );
    }

    #[test]
    fn another_host_is_never_touched() {
        for url in [
            "https://cdn.example.net/assets",
            "https://media.example.net",
            "http://api:8080",
            "http://media-proxy:8080",
        ] {
            assert_eq!(
                url,
                normalize_public_endpoint(url, "fluxer.example", Some(19080))
            );
        }
        assert_eq!(
            "https://sub.fluxer.example/media",
            normalize_public_endpoint(
                "https://sub.fluxer.example/media",
                "fluxer.example",
                Some(19080)
            )
        );
    }

    #[test]
    fn an_explicit_port_is_never_touched() {
        for url in [
            "http://fluxer.example:19080/media",
            "http://fluxer.example:8080/media",
            "https://fluxer.example:443/media",
            "http://fluxer.example:80/media",
            "http://user:pass@fluxer.example:19080/media",
        ] {
            assert_eq!(
                url,
                normalize_public_endpoint(url, "fluxer.example", Some(19080))
            );
        }
    }

    #[test]
    fn is_idempotent() {
        let once =
            normalize_public_endpoint("http://fluxer.example/media", "fluxer.example", Some(19080));
        let twice = normalize_public_endpoint(&once, "fluxer.example", Some(19080));
        assert_eq!("http://fluxer.example:19080/media", once);
        assert_eq!(once, twice);
    }

    #[test]
    fn an_unset_port_leaves_every_url_alone() {
        assert_eq!(
            "http://fluxer.example/media",
            normalize_public_endpoint("http://fluxer.example/media", "fluxer.example", None)
        );
        assert_eq!(
            "http://fluxer.example/media",
            normalize_public_endpoint("http://fluxer.example/media", "fluxer.example", Some(0))
        );
    }

    #[test]
    fn an_empty_base_domain_leaves_every_url_alone() {
        assert_eq!(
            "http://fluxer.example/media",
            normalize_public_endpoint("http://fluxer.example/media", "", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example/media",
            normalize_public_endpoint("http://fluxer.example/media", "   ", Some(19080))
        );
    }

    #[test]
    fn a_url_that_does_not_parse_is_returned_unchanged() {
        for url in ["", "/api", "fluxer.example/media", "not a url", "://x"] {
            assert_eq!(
                url,
                normalize_public_endpoint(url, "fluxer.example", Some(19080))
            );
        }
    }

    #[test]
    fn matches_the_host_case_insensitively_and_ignores_a_trailing_dot() {
        assert_eq!(
            "http://FLUXER.example:19080/Media",
            normalize_public_endpoint("http://FLUXER.example/Media", "Fluxer.Example", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example.:19080/media",
            normalize_public_endpoint(
                "http://fluxer.example./media",
                "fluxer.example",
                Some(19080)
            )
        );
        assert_eq!(
            "http://fluxer.example:19080/media",
            normalize_public_endpoint(
                "http://fluxer.example/media",
                "fluxer.example.",
                Some(19080)
            )
        );
    }

    #[test]
    fn preserves_path_query_fragment_and_trailing_slash() {
        assert_eq!(
            "http://fluxer.example:19080/",
            normalize_public_endpoint("http://fluxer.example/", "fluxer.example", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example:19080?a=1",
            normalize_public_endpoint("http://fluxer.example?a=1", "fluxer.example", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example:19080#top",
            normalize_public_endpoint("http://fluxer.example#top", "fluxer.example", Some(19080))
        );
        assert_eq!(
            "http://fluxer.example:19080/media/x.png?v=1#frag",
            normalize_public_endpoint(
                "http://fluxer.example/media/x.png?v=1#frag",
                "fluxer.example",
                Some(19080)
            )
        );
    }

    #[test]
    fn keeps_credentials_and_ipv6_literals_intact() {
        assert_eq!(
            "http://user:pass@fluxer.example:19080/media",
            normalize_public_endpoint(
                "http://user:pass@fluxer.example/media",
                "fluxer.example",
                Some(19080)
            )
        );
        assert_eq!(
            "http://[::1]:19080/media",
            normalize_public_endpoint("http://[::1]/media", "[::1]", Some(19080))
        );
        assert_eq!(
            "http://[::1]:8080/media",
            normalize_public_endpoint("http://[::1]:8080/media", "[::1]", Some(19080))
        );
    }

    #[test]
    fn matches_the_typescript_normalizer_on_the_shared_vectors() {
        let raw = include_str!("testdata/public_endpoint_vectors.json");
        let vectors: serde_json::Value = serde_json::from_str(raw).expect("vectors parse as json");
        let vectors = vectors.as_array().expect("vectors are an array");
        assert!(!vectors.is_empty());
        for vector in vectors {
            let url = vector["url"].as_str().expect("vector carries a url");
            let base_domain = vector["base_domain"]
                .as_str()
                .expect("vector carries a base domain");
            let public_port = vector["public_port"].as_u64().map(|port| port as u16);
            let expected = vector["normalized"]
                .as_str()
                .expect("vector carries a normalized url");
            assert_eq!(
                expected,
                normalize_public_endpoint(url, base_domain, public_port),
                "vector {url} @ {base_domain} port {public_port:?}"
            );
        }
    }

    #[test]
    fn trim_trailing_slash_works() {
        assert_eq!(
            trim_trailing_slash("https://example.com/"),
            "https://example.com"
        );
        assert_eq!(trim_trailing_slash(""), "");
    }
}
