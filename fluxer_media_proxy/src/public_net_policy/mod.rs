// SPDX-License-Identifier: AGPL-3.0-or-later

mod ip_tables;
mod resolver;

#[cfg(test)]
mod tests;

pub use ip_tables::is_blocked_ip_literal;
pub use resolver::{PinnedDnsResolver, is_pinned_dns_failure};

use std::net::IpAddr;
use thiserror::Error;
use url::Url;

const MAX_URL_LEN: usize = 8192;
const MAX_PUBLIC_HOSTNAME_BYTES: usize = 253;
const MAX_PUBLIC_HOSTNAME_LABEL_BYTES: usize = 63;
const ALLOWED_PUBLIC_URL_PORTS: [u16; 2] = [80, 443];

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum Error {
    #[error("invalid URL")]
    InvalidUrl,
    #[error("blocked URL")]
    BlockedUrl,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ParsedUrl<'a> {
    pub scheme: &'a str,
    pub authority: &'a str,
    pub host: &'a str,
    pub port: Option<u16>,
    pub path_query: &'a str,
    pub fragment: Option<&'a str>,
}

fn contains_ctl(value: &str) -> bool {
    value.bytes().any(|ch| ch < 0x20 || ch == 0x7f)
}

pub fn parse_url(url: &str) -> Result<ParsedUrl<'_>, Error> {
    if url.is_empty() || url.len() > MAX_URL_LEN || contains_ctl(url) {
        return Err(Error::InvalidUrl);
    }
    let scheme_end = url.find("://").ok_or(Error::InvalidUrl)?;
    let scheme = &url[..scheme_end];
    if !(scheme.eq_ignore_ascii_case("http") || scheme.eq_ignore_ascii_case("https")) {
        return Err(Error::BlockedUrl);
    }
    let mut rest = &url[scheme_end + 3..];
    let authority_end = rest.find(['/', '?', '#']).unwrap_or(rest.len());
    if authority_end == 0 {
        return Err(Error::InvalidUrl);
    }
    let authority = &rest[..authority_end];
    if authority.contains('@') {
        return Err(Error::BlockedUrl);
    }
    rest = &rest[authority_end..];
    let mut port = None;
    let host = if let Some(after_bracket) = authority.strip_prefix('[') {
        let close = after_bracket.find(']').ok_or(Error::InvalidUrl)?;
        let host = &after_bracket[..close];
        let suffix = &after_bracket[close + 1..];
        if !suffix.is_empty() {
            let raw = suffix.strip_prefix(':').ok_or(Error::InvalidUrl)?;
            port = Some(parse_port(raw)?);
        }
        host
    } else if let Some(colon) = authority.find(':') {
        if authority[colon + 1..].contains(':') {
            return Err(Error::InvalidUrl);
        }
        port = Some(parse_port(&authority[colon + 1..])?);
        &authority[..colon]
    } else {
        authority
    };
    if host.is_empty() {
        return Err(Error::InvalidUrl);
    }
    let (path_query, fragment) = if rest.is_empty() {
        ("/", None)
    } else {
        match rest.find('#') {
            Some(hash) => (&rest[..hash], Some(&rest[hash + 1..])),
            None => (rest, None),
        }
    };
    Ok(ParsedUrl {
        scheme,
        authority,
        host,
        port,
        path_query,
        fragment,
    })
}

fn parse_port(raw: &str) -> Result<u16, Error> {
    if raw.is_empty() {
        return Err(Error::InvalidUrl);
    }
    let port = raw.parse::<u16>().map_err(|_| Error::InvalidUrl)?;
    if port == 0 {
        return Err(Error::InvalidUrl);
    }
    Ok(port)
}

fn default_port_for_scheme(scheme: &str) -> u16 {
    if scheme.eq_ignore_ascii_case("https") {
        443
    } else {
        80
    }
}

fn normalize_host(raw: &str) -> Result<String, Error> {
    let trimmed = raw.trim_matches([' ', '\t', '\r', '\n']);
    let without_dot = trimmed.strip_suffix('.').unwrap_or(trimmed);
    if without_dot.is_empty() {
        return Err(Error::InvalidUrl);
    }
    Ok(without_dot.to_ascii_lowercase())
}

pub fn is_valid_public_hostname(host: &str) -> bool {
    if host.is_empty() || host.len() > MAX_PUBLIC_HOSTNAME_BYTES || !host.contains('.') {
        return false;
    }
    let mut last = "";
    for label in host.split('.') {
        if label.is_empty() || label.len() > MAX_PUBLIC_HOSTNAME_LABEL_BYTES {
            return false;
        }
        let bytes = label.as_bytes();
        if !bytes[0].is_ascii_alphanumeric() || !bytes[bytes.len() - 1].is_ascii_alphanumeric() {
            return false;
        }
        if !bytes
            .iter()
            .all(|byte| byte.is_ascii_alphanumeric() || *byte == b'-')
        {
            return false;
        }
        last = label;
    }
    !last.bytes().all(|byte| byte.is_ascii_digit())
}

pub fn validate_url(url: &str) -> Result<(), Error> {
    let parsed = parse_url(url)?;
    if parsed.fragment.is_some() {
        return Err(Error::BlockedUrl);
    }
    let port = parsed
        .port
        .unwrap_or_else(|| default_port_for_scheme(parsed.scheme));
    if !ALLOWED_PUBLIC_URL_PORTS.contains(&port) {
        return Err(Error::BlockedUrl);
    }
    let host = normalize_host(parsed.host)?;
    if let Ok(address) = host.parse::<IpAddr>() {
        return if ip_tables::is_public_ip(address) {
            Ok(())
        } else {
            Err(Error::BlockedUrl)
        };
    }
    if !is_valid_public_hostname(&host) {
        return Err(Error::BlockedUrl);
    }
    Ok(())
}

pub fn resolve_redirect(base_url: &str, location: &str) -> Result<String, Error> {
    if location.is_empty() || location.len() > MAX_URL_LEN || contains_ctl(location) {
        return Err(Error::InvalidUrl);
    }
    let fragment = location.find('#').unwrap_or(location.len());
    let loc = location[..fragment].trim_matches([' ', '\t', '\r', '\n']);
    if loc
        .get(..7)
        .is_some_and(|s| s.eq_ignore_ascii_case("http://"))
        || loc
            .get(..8)
            .is_some_and(|s| s.eq_ignore_ascii_case("https://"))
    {
        return Ok(loc.to_owned());
    }
    let base = parse_url(base_url)?;
    if loc.starts_with("//") {
        return Ok(format!("{}:{loc}", base.scheme));
    }
    if loc.starts_with('/') {
        return Ok(format!("{}://{}{}", base.scheme, base.authority, loc));
    }
    let q = base.path_query.find('?').unwrap_or(base.path_query.len());
    let base_path = &base.path_query[..q];
    if loc.starts_with('?') {
        return Ok(format!(
            "{}://{}{}{}",
            base.scheme, base.authority, base_path, loc
        ));
    }
    let slash = base_path.rfind('/').unwrap_or(0);
    let prefix = if slash == 0 {
        "/"
    } else {
        &base_path[..slash + 1]
    };
    let joined = format!("{prefix}{loc}");
    Ok(format!(
        "{}://{}{}",
        base.scheme,
        base.authority,
        remove_dot_segments(&joined)
    ))
}

fn remove_dot_segments(path_query: &str) -> String {
    let q = path_query.find('?').unwrap_or(path_query.len());
    let path = &path_query[..q];
    let query = &path_query[q..];
    let mut segments = Vec::new();
    for segment in path.split('/') {
        if segment.is_empty() || segment == "." {
            continue;
        }
        if segment == ".." {
            segments.pop();
        } else {
            segments.push(segment);
        }
    }
    let mut out = String::from("/");
    out.push_str(&segments.join("/"));
    if path.len() > 1 && path.ends_with('/') && !out.ends_with('/') {
        out.push('/');
    }
    out.push_str(query);
    out
}

pub fn external_url_for_log(value: &str) -> String {
    let Ok(mut parsed) = Url::parse(value) else {
        return "[invalid-url]".to_owned();
    };
    let _ = parsed.set_username("");
    let _ = parsed.set_password(None);
    parsed.set_path("/[redacted]");
    parsed.set_query(None);
    parsed.set_fragment(None);
    parsed.to_string()
}
