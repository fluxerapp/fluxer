// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{GeoipS3Config, GeoipSourceConfig};
use aws_credential_types::Credentials;
use aws_sigv4::{
    http_request::{
        PayloadChecksumKind, PercentEncodingMode, SignableBody, SignableRequest, SigningSettings,
        UriPathNormalizationMode, sign,
    },
    sign::v4,
};
use axum::http::HeaderMap;
use maxminddb::{Reader, geoip2};
use moka::sync::Cache;
use std::{
    fs::{self, File},
    io,
    net::IpAddr,
    path::Path,
    time::{Duration, SystemTime},
};

const DEFAULT_COUNTRY_CODE: &str = "US";
const GEOIP_CACHE_TTL: Duration = Duration::from_secs(10 * 60);
const GEOIP_CACHE_MAX_ENTRIES: u64 = 4096;

pub struct GeoipConfig {
    pub geoip_source: GeoipSourceConfig,
    pub geoip_s3_config: Option<GeoipS3Config>,
    pub trust_client_ip_header: bool,
    pub client_ip_header_name: String,
}

#[derive(Clone, Debug, Default)]
pub struct GeoipLookup {
    pub country_code: Option<String>,
    pub region_code: Option<String>,
    pub latitude: Option<f64>,
    pub longitude: Option<f64>,
}

#[derive(Debug)]
pub struct GeoipResolver {
    reader: Option<Reader<Vec<u8>>>,
    cache: Cache<IpAddr, GeoipLookup>,
    trust_client_ip_header: bool,
    client_ip_header_name: String,
}

impl GeoipResolver {
    pub fn from_config(config: &GeoipConfig) -> Self {
        let db_path = prepare_geoip_database(&config.geoip_source, config.geoip_s3_config.as_ref())
            .unwrap_or_else(|err| panic!("failed to prepare GeoIP database: {err:#}"));
        let reader = if let Some(path) = db_path.as_deref() {
            open_reader(path)
        } else {
            None
        };
        Self {
            reader,
            cache: Cache::builder()
                .max_capacity(GEOIP_CACHE_MAX_ENTRIES)
                .time_to_live(GEOIP_CACHE_TTL)
                .build(),
            trust_client_ip_header: config.trust_client_ip_header,
            client_ip_header_name: config.client_ip_header_name.clone(),
        }
    }

    pub fn lookup(&self, headers: &HeaderMap) -> GeoipLookup {
        let Some(reader) = self.reader.as_ref() else {
            return GeoipLookup::default();
        };
        let Some(ip) = extract_client_ip(
            headers,
            self.trust_client_ip_header,
            &self.client_ip_header_name,
        ) else {
            return GeoipLookup::default();
        };
        if let Some(cached) = self.cache.get(&ip) {
            return cached;
        }
        let result = lookup_geoip(reader, ip);
        self.cache.insert(ip, result.clone());
        result
    }

    pub fn country_code(&self, headers: &HeaderMap) -> String {
        self.lookup(headers)
            .country_code
            .unwrap_or_else(|| DEFAULT_COUNTRY_CODE.to_owned())
    }
}

fn open_reader(path: &str) -> Option<Reader<Vec<u8>>> {
    match Reader::open_readfile(path) {
        Ok(reader) => Some(reader),
        Err(error) => {
            tracing::warn!(
                geoip_db_path = path,
                ?error,
                "failed to open GeoIP database; geoip will be empty"
            );
            None
        }
    }
}

fn lookup_geoip(reader: &Reader<Vec<u8>>, ip: IpAddr) -> GeoipLookup {
    let Ok(lookup) = reader.lookup(ip) else {
        return GeoipLookup::default();
    };
    let Ok(Some(city)) = lookup.decode::<geoip2::City<'_>>() else {
        return GeoipLookup::default();
    };
    let country_code = city.country.iso_code.and_then(normalize_country_code);
    let region_code = city
        .subdivisions
        .first()
        .and_then(|s| s.iso_code)
        .map(|s| s.to_uppercase());
    let latitude = city.location.latitude;
    let longitude = city.location.longitude;
    GeoipLookup {
        country_code,
        region_code,
        latitude,
        longitude,
    }
}

fn extract_client_ip(
    headers: &HeaderMap,
    trust_client_ip_header: bool,
    header_name: &str,
) -> Option<IpAddr> {
    if !trust_client_ip_header {
        return None;
    }
    let header_value = headers.get(header_name)?.to_str().ok()?;
    let first_hop = header_value.split(',').next()?.trim();
    parse_ip(first_hop)
}

fn parse_ip(value: &str) -> Option<IpAddr> {
    if value.is_empty() {
        return None;
    }
    if let Ok(ip) = value.parse() {
        return Some(ip);
    }
    if let Some(stripped) = value
        .strip_prefix('[')
        .and_then(|value| value.split_once(']').map(|(host, _)| host))
    {
        return stripped.parse().ok();
    }
    if value.matches(':').count() == 1
        && let Some((host, _port)) = value.rsplit_once(':')
    {
        return host.parse().ok();
    }
    None
}

fn normalize_country_code(value: &str) -> Option<String> {
    let country_code = value.trim().to_ascii_uppercase();
    if country_code.len() == 2 && country_code.chars().all(|ch| ch.is_ascii_alphabetic()) {
        Some(country_code)
    } else {
        None
    }
}

fn prepare_geoip_database(
    source: &GeoipSourceConfig,
    s3_config: Option<&GeoipS3Config>,
) -> anyhow::Result<Option<String>> {
    match source {
        GeoipSourceConfig::Filesystem { maxmind_db_path } => Ok(maxmind_db_path.clone()),
        GeoipSourceConfig::S3 {
            maxmind_db_path,
            maxmind_asn_db_path,
            s3_bucket,
            s3_key,
            s3_asn_key,
        } => {
            let s3 = s3_config.ok_or_else(|| {
                anyhow::anyhow!("GeoIP is configured for S3 mode, but S3 configuration is missing")
            })?;
            require_s3_config(s3)?;
            download_s3_object(s3, s3_bucket, s3_key, Path::new(maxmind_db_path))?;
            if let (Some(asn_key), Some(asn_path)) = (s3_asn_key, maxmind_asn_db_path) {
                download_s3_object(s3, s3_bucket, asn_key, Path::new(asn_path))?;
            }
            tracing::info!(
                maxmind_db_path,
                s3_bucket,
                s3_key,
                "GeoIP database downloaded from S3"
            );
            Ok(Some(maxmind_db_path.clone()))
        }
    }
}

fn require_s3_config(config: &GeoipS3Config) -> anyhow::Result<()> {
    require_s3_value(
        &config.endpoint,
        "GeoIP S3 startup mode requires s3.endpoint",
    )?;
    require_s3_value(&config.region, "GeoIP S3 startup mode requires s3.region")?;
    require_s3_value(
        &config.access_key_id,
        "GeoIP S3 startup mode requires s3.access_key_id",
    )?;
    require_s3_value(
        &config.secret_access_key,
        "GeoIP S3 startup mode requires s3.secret_access_key",
    )
}

fn require_s3_value(value: &str, message: &str) -> anyhow::Result<()> {
    if value.trim().is_empty() {
        anyhow::bail!("{}", message);
    }
    Ok(())
}

fn download_s3_object(
    config: &GeoipS3Config,
    bucket: &str,
    key: &str,
    destination: &Path,
) -> anyhow::Result<()> {
    let Some(parent) = destination.parent() else {
        anyhow::bail!(
            "GeoIP S3 destination must include a parent directory: {}",
            destination.display()
        );
    };
    fs::create_dir_all(parent)?;
    let request = signed_s3_get_request(config, bucket, key)?;
    let mut response = reqwest::blocking::Client::new()
        .get(request.url)
        .headers(request.headers)
        .send()
        .map_err(|err| {
            anyhow::anyhow!("failed to download GeoIP database from s3://{bucket}/{key}: {err}")
        })?;
    if !response.status().is_success() {
        let status = response.status();
        anyhow::bail!("failed to download GeoIP database from s3://{bucket}/{key}: HTTP {status}");
    }
    let mut file = tempfile::Builder::new().make_in(parent, |path| File::create_new(path))?;
    io::copy(&mut response, &mut file)?;
    file.persist(destination).map_err(|err| err.error)?;
    Ok(())
}

struct SignedS3Request {
    url: reqwest::Url,
    headers: reqwest::header::HeaderMap,
}

fn signed_s3_get_request(
    config: &GeoipS3Config,
    bucket: &str,
    key: &str,
) -> anyhow::Result<SignedS3Request> {
    let mut url = reqwest::Url::parse(config.endpoint.trim_end_matches('/'))?;
    {
        let mut segments = url.path_segments_mut().map_err(|_| {
            anyhow::anyhow!("S3 endpoint cannot be a base URL: {}", config.endpoint)
        })?;
        segments.pop_if_empty();
        segments.push(bucket);
        for segment in key.split('/') {
            segments.push(segment);
        }
    }

    let identity = Credentials::new(
        config.access_key_id.clone(),
        config.secret_access_key.clone(),
        None,
        None,
        "fluxer-geoip",
    )
    .into();
    let mut signing_settings = SigningSettings::default();
    signing_settings.percent_encoding_mode = PercentEncodingMode::Single;
    signing_settings.payload_checksum_kind = PayloadChecksumKind::XAmzSha256;
    signing_settings.uri_path_normalization_mode = UriPathNormalizationMode::Disabled;
    let signing_params = v4::SigningParams::builder()
        .identity(&identity)
        .region(&config.region)
        .name("s3")
        .time(SystemTime::now())
        .settings(signing_settings)
        .build()?
        .into();
    let signable_request = SignableRequest::new(
        "GET",
        url.as_str(),
        std::iter::empty::<(&str, &str)>(),
        SignableBody::Bytes(&[]),
    )?;
    let (signing_instructions, _signature) = sign(signable_request, &signing_params)?.into_parts();
    let (signed_headers, signed_params) = signing_instructions.into_parts();
    if !signed_params.is_empty() {
        anyhow::bail!("S3 GeoIP signing unexpectedly produced query parameters for header signing");
    }
    let mut headers = reqwest::header::HeaderMap::new();
    for header in signed_headers {
        let name = reqwest::header::HeaderName::from_bytes(header.name().as_bytes())?;
        let mut value = reqwest::header::HeaderValue::from_str(header.value())?;
        value.set_sensitive(header.sensitive());
        headers.insert(name, value);
    }
    Ok(SignedS3Request { url, headers })
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::HeaderValue;

    #[test]
    fn extracts_first_configured_client_ip_only_when_trusted() {
        let mut headers = HeaderMap::new();
        headers.insert(
            "x-forwarded-for",
            HeaderValue::from_static("203.0.113.10, 10.0.0.4"),
        );
        assert_eq!(
            extract_client_ip(&headers, true, "x-forwarded-for"),
            Some("203.0.113.10".parse().unwrap())
        );
        assert_eq!(extract_client_ip(&headers, false, "x-forwarded-for"), None);
    }

    #[test]
    fn parse_ip_handles_ipv4() {
        assert_eq!(
            parse_ip("203.0.113.10"),
            Some("203.0.113.10".parse().unwrap())
        );
    }

    #[test]
    fn parse_ip_handles_ipv4_with_port() {
        assert_eq!(
            parse_ip("203.0.113.10:8080"),
            Some("203.0.113.10".parse().unwrap())
        );
    }

    #[test]
    fn parse_ip_handles_ipv6_bracketed() {
        assert_eq!(parse_ip("[::1]:8080"), Some("::1".parse().unwrap()));
    }

    #[test]
    fn parse_ip_handles_empty() {
        assert_eq!(parse_ip(""), None);
    }

    #[test]
    fn normalizes_valid_country_codes() {
        assert_eq!(normalize_country_code("se"), Some("SE".to_owned()));
        assert_eq!(normalize_country_code("US"), Some("US".to_owned()));
        assert_eq!(normalize_country_code("abc"), None);
        assert_eq!(normalize_country_code(""), None);
    }
}
