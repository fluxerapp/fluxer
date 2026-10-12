// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::public_ip::{is_public_ip, is_valid_public_hostname};
use reqwest::dns::{Addrs, Name, Resolve, Resolving};
use std::fmt;
use std::net::{IpAddr, SocketAddr};
use tokio::net::lookup_host;
use url::Url;

const MAX_URL_LEN: usize = 8192;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    InvalidUrl,
    BlockedUrl,
    DnsLookupFailed,
    HostResolvedToNoAddress,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidUrl => f.write_str("invalid URL"),
            Self::BlockedUrl => f.write_str("blocked URL"),
            Self::DnsLookupFailed => f.write_str("DNS lookup failed"),
            Self::HostResolvedToNoAddress => f.write_str("host resolved to no address"),
        }
    }
}

impl std::error::Error for Error {}

pub async fn validate_url(url: &Url) -> Result<(), Error> {
    if url.as_str().is_empty() || url.as_str().len() > MAX_URL_LEN || contains_ctl(url.as_str()) {
        return Err(Error::InvalidUrl);
    }
    if !(url.scheme().eq_ignore_ascii_case("http") || url.scheme().eq_ignore_ascii_case("https")) {
        return Err(Error::BlockedUrl);
    }
    if !url.username().is_empty() || url.password().is_some() {
        return Err(Error::BlockedUrl);
    }
    if url.port() == Some(0) {
        return Err(Error::InvalidUrl);
    }
    let host = normalize_host(url.host_str().ok_or(Error::InvalidUrl)?)?;
    if let Ok(ip) = host.parse::<IpAddr>() {
        return validate_ip(ip);
    }
    if !is_valid_public_hostname(&host) {
        return Err(Error::BlockedUrl);
    }
    let port = url.port_or_known_default().ok_or(Error::BlockedUrl)?;
    let mut seen = false;
    let addrs = lookup_host((host.as_str(), port))
        .await
        .map_err(|_| Error::DnsLookupFailed)?;
    for addr in addrs {
        seen = true;
        validate_ip(addr.ip())?;
    }
    if seen {
        Ok(())
    } else {
        Err(Error::HostResolvedToNoAddress)
    }
}

pub fn parse_url(raw: &str) -> Result<Url, Error> {
    if raw.is_empty() || raw.len() > MAX_URL_LEN || contains_ctl(raw) {
        return Err(Error::InvalidUrl);
    }
    Url::parse(raw).map_err(|_| Error::InvalidUrl)
}

pub fn resolve_redirect(base_url: &Url, location: &str) -> Result<Url, Error> {
    if location.is_empty() || location.len() > MAX_URL_LEN || contains_ctl(location) {
        return Err(Error::InvalidUrl);
    }
    let fragment = location.find('#').unwrap_or(location.len());
    let trimmed = location[..fragment].trim_matches([' ', '\t', '\r', '\n']);
    if trimmed.is_empty() {
        return Err(Error::InvalidUrl);
    }
    base_url.join(trimmed).map_err(|_| Error::InvalidUrl)
}

fn contains_ctl(value: &str) -> bool {
    value.bytes().any(|ch| ch < 0x20 || ch == 0x7f)
}

fn normalize_host(raw: &str) -> Result<String, Error> {
    let trimmed = raw.trim_matches([' ', '\t', '\r', '\n']);
    let without_brackets = trimmed
        .strip_prefix('[')
        .and_then(|host| host.strip_suffix(']'))
        .unwrap_or(trimmed);
    let without_dot = without_brackets
        .strip_suffix('.')
        .unwrap_or(without_brackets);
    if without_dot.is_empty() {
        return Err(Error::InvalidUrl);
    }
    Ok(without_dot.to_ascii_lowercase())
}

fn validate_ip(ip: IpAddr) -> Result<(), Error> {
    if is_public_ip(ip) {
        Ok(())
    } else {
        Err(Error::BlockedUrl)
    }
}

pub struct PinnedDnsResolver;

impl Resolve for PinnedDnsResolver {
    fn resolve(&self, name: Name) -> Resolving {
        Box::pin(async move {
            let host = name.as_str().to_owned();
            let resolved: Vec<SocketAddr> = lookup_host((host.as_str(), 0)).await?.collect();
            if resolved.is_empty() {
                return Err(Box::new(Error::HostResolvedToNoAddress)
                    as Box<dyn std::error::Error + Send + Sync>);
            }
            for addr in &resolved {
                validate_ip(addr.ip())?;
            }
            Ok(Box::new(resolved.into_iter()) as Addrs)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(raw: &str) -> Url {
        Url::parse(raw).unwrap()
    }

    #[tokio::test]
    async fn blocks_private_and_special_ip_literals() {
        for raw in [
            "http://0.0.0.0/",
            "http://10.0.0.1/",
            "http://100.64.0.1/",
            "http://127.0.0.1/",
            "http://169.254.169.254/",
            "http://172.16.5.5/",
            "http://192.168.1.1/",
            "http://[::1]/",
            "http://[fc00::1]/",
            "http://[fe80::1]/",
            "http://[64:ff9b::a9fe:a9fe]/",
            "http://[::7f00:1]/",
            "http://[2002:a9fe:a9fe::]/",
        ] {
            assert_eq!(
                validate_url(&url(raw)).await,
                Err(Error::BlockedUrl),
                "{raw}"
            );
        }
    }

    #[tokio::test]
    async fn checks_the_canonical_ipv4_host() {
        for raw in [
            "http://0x7f.0x0.0x0.0x1/",
            "http://0xa9.0xfe.0xa9.0xfe/latest/meta-data/",
            "http://169.254.169.0xfe/",
            "http://127.0.0.0x1/",
            "http://0x7f000001/",
            "http://2130706433/",
            "http://0177.0.0.1/",
            "http://127.1/",
        ] {
            assert_eq!(
                validate_url(&url(raw)).await,
                Err(Error::BlockedUrl),
                "{raw}"
            );
        }
        let base = url("https://example.com/start.gif");
        let next = resolve_redirect(&base, "http://0xa9.0xfe.0xa9.0xfe/latest/meta-data/").unwrap();
        assert_eq!(validate_url(&next).await, Err(Error::BlockedUrl));
    }

    #[tokio::test]
    async fn allows_public_ip_literals_without_dns() {
        assert_eq!(validate_url(&url("https://8.8.8.8/")).await, Ok(()));
        assert_eq!(
            validate_url(&url("https://[2606:4700:4700::1111]/")).await,
            Ok(())
        );
    }

    #[tokio::test]
    async fn rejects_userinfo_and_non_http_s_schemes() {
        assert_eq!(
            validate_url(&url("https://user:pass@example.com/")).await,
            Err(Error::BlockedUrl)
        );
        assert_eq!(
            validate_url(&url("file:///etc/passwd")).await,
            Err(Error::BlockedUrl)
        );
    }

    #[test]
    fn validates_public_host_syntax() {
        assert!(is_valid_public_hostname("example.com"));
        assert!(is_valid_public_hostname("xn--bcher-kva.example"));
        assert!(!is_valid_public_hostname("localhost"));
        assert!(!is_valid_public_hostname("example"));
        assert!(!is_valid_public_hostname("bad_name.example"));
        assert!(!is_valid_public_hostname("example.123"));
    }

    #[test]
    fn resolves_redirects_without_fragments() {
        let base = url("https://example.com/a/b/c?x=1");
        assert_eq!(
            resolve_redirect(&base, "../d?y=2#ignored")
                .unwrap()
                .as_str(),
            "https://example.com/a/d?y=2"
        );
        assert_eq!(
            resolve_redirect(&base, "//cdn.example.com/p")
                .unwrap()
                .as_str(),
            "https://cdn.example.com/p"
        );
    }
}
