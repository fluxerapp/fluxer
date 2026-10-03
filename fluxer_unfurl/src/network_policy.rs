// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::dns::{Addrs, Name, Resolve, Resolving};
use std::fmt;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
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
    match ip {
        IpAddr::V4(ip) if blocked_ipv4(ip) => Err(Error::BlockedUrl),
        IpAddr::V6(ip) if blocked_ipv6(ip) => Err(Error::BlockedUrl),
        _ => Ok(()),
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

fn ipv4_to_u32(ip: Ipv4Addr) -> u32 {
    u32::from_be_bytes(ip.octets())
}

fn ipv4_in(ip: Ipv4Addr, prefix: Ipv4Addr, bits: u32) -> bool {
    let mask = if bits == 0 {
        0
    } else {
        u32::MAX << (32 - bits)
    };
    (ipv4_to_u32(ip) & mask) == (ipv4_to_u32(prefix) & mask)
}

fn ipv6_to_u128(ip: Ipv6Addr) -> u128 {
    u128::from_be_bytes(ip.octets())
}

fn ipv6_in(ip: Ipv6Addr, prefix: Ipv6Addr, bits: u32) -> bool {
    let mask = if bits == 0 {
        0
    } else {
        u128::MAX << (128 - bits)
    };
    (ipv6_to_u128(ip) & mask) == (ipv6_to_u128(prefix) & mask)
}

fn blocked_ipv4(ip: Ipv4Addr) -> bool {
    ipv4_in(ip, Ipv4Addr::new(0, 0, 0, 0), 8)
        || ipv4_in(ip, Ipv4Addr::new(10, 0, 0, 0), 8)
        || ipv4_in(ip, Ipv4Addr::new(100, 64, 0, 0), 10)
        || ipv4_in(ip, Ipv4Addr::new(127, 0, 0, 0), 8)
        || ipv4_in(ip, Ipv4Addr::new(169, 254, 0, 0), 16)
        || ipv4_in(ip, Ipv4Addr::new(172, 16, 0, 0), 12)
        || ipv4_in(ip, Ipv4Addr::new(192, 0, 0, 0), 24)
        || ipv4_in(ip, Ipv4Addr::new(192, 0, 2, 0), 24)
        || ipv4_in(ip, Ipv4Addr::new(192, 88, 99, 0), 24)
        || ipv4_in(ip, Ipv4Addr::new(192, 168, 0, 0), 16)
        || ipv4_in(ip, Ipv4Addr::new(198, 18, 0, 0), 15)
        || ipv4_in(ip, Ipv4Addr::new(198, 51, 100, 0), 24)
        || ipv4_in(ip, Ipv4Addr::new(203, 0, 113, 0), 24)
        || ipv4_in(ip, Ipv4Addr::new(224, 0, 0, 0), 4)
        || ipv4_in(ip, Ipv4Addr::new(240, 0, 0, 0), 4)
        || ip == Ipv4Addr::new(255, 255, 255, 255)
}

fn embedded_ipv4(ip: Ipv6Addr) -> Option<Ipv4Addr> {
    let octets = ip.octets();
    if ipv6_in(ip, Ipv6Addr::new(0x0064, 0xff9b, 0, 0, 0, 0, 0, 0), 96)
        || ipv6_in(ip, Ipv6Addr::UNSPECIFIED, 96)
    {
        return Some(Ipv4Addr::new(
            octets[12], octets[13], octets[14], octets[15],
        ));
    }
    if ipv6_in(ip, Ipv6Addr::new(0x2002, 0, 0, 0, 0, 0, 0, 0), 16) {
        return Some(Ipv4Addr::new(octets[2], octets[3], octets[4], octets[5]));
    }
    None
}

fn blocked_ipv6(ip: Ipv6Addr) -> bool {
    if let Some(mapped) = ip.to_ipv4_mapped() {
        return blocked_ipv4(mapped);
    }
    if let Some(embedded) = embedded_ipv4(ip) {
        return blocked_ipv4(embedded);
    }
    ip.is_unspecified()
        || ip.is_loopback()
        || ipv6_in(ip, Ipv6Addr::new(0x2001, 0x0db8, 0, 0, 0, 0, 0, 0), 32)
        || ipv6_in(ip, Ipv6Addr::new(0xfc00, 0, 0, 0, 0, 0, 0, 0), 7)
        || ipv6_in(ip, Ipv6Addr::new(0xfe80, 0, 0, 0, 0, 0, 0, 0), 10)
        || ipv6_in(ip, Ipv6Addr::new(0xff00, 0, 0, 0, 0, 0, 0, 0), 8)
}

fn is_valid_public_hostname(host: &str) -> bool {
    if host.is_empty() || host.len() > 253 || !host.contains('.') {
        return false;
    }
    let mut last = "";
    for label in host.split('.') {
        if label.is_empty() || label.len() > 63 {
            return false;
        }
        let bytes = label.as_bytes();
        if !bytes[0].is_ascii_alphanumeric() || !bytes[bytes.len() - 1].is_ascii_alphanumeric() {
            return false;
        }
        if !bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'-')
        {
            return false;
        }
        last = label;
    }
    !last.bytes().all(|b| b.is_ascii_digit())
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
