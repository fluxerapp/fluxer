// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{env, fs};

pub fn env_value(name: &str) -> Option<String> {
    resolve_env_value(name, |key| env::var(key).ok()).unwrap_or_else(|error| panic!("{error}"))
}

pub fn resolve_env_value<F>(name: &str, get: F) -> Result<Option<String>, String>
where
    F: Fn(&str) -> Option<String>,
{
    let non_blank = |value: Option<String>| value.filter(|value| !value.trim().is_empty());
    let value = non_blank(get(name));
    let Some(path) = non_blank(get(&format!("{name}_FILE"))) else {
        return Ok(value);
    };
    if value.is_some() {
        return Err(format!("{name} and {name}_FILE are both set, set only one"));
    }
    let contents = fs::read_to_string(&path)
        .map_err(|error| format!("{name}_FILE could not read {path} ({error})"))?;
    let contents = contents
        .strip_suffix('\n')
        .map_or(contents.as_str(), |rest| {
            rest.strip_suffix('\r').unwrap_or(rest)
        });
    Ok(non_blank(Some(contents.to_owned())))
}

pub fn resolve_env_files<I>(vars: I) -> Result<Vec<(String, String)>, String>
where
    I: IntoIterator<Item = (String, String)>,
{
    let vars: Vec<(String, String)> = vars.into_iter().collect();
    let get = |key: &str| {
        vars.iter()
            .find_map(|(name, value)| (name == key).then(|| value.clone()))
    };
    let mut resolved = Vec::new();
    for (key, _) in &vars {
        let Some(name) = key
            .strip_suffix("_FILE")
            .filter(|name| name.starts_with("FLUXER_"))
        else {
            continue;
        };
        if let Some(value) = resolve_env_value(name, get)? {
            resolved.push((name.to_owned(), value));
        }
    }
    let rest: Vec<(String, String)> = vars
        .iter()
        .filter(|(key, _)| !resolved.iter().any(|(name, _)| name == key))
        .cloned()
        .collect();
    resolved.extend(rest);
    Ok(resolved)
}

pub fn read_env(name: &str, fallback: &str) -> String {
    env_value(name).unwrap_or_else(|| fallback.to_owned())
}

pub fn read_first_env(names: &[&str], fallback: &str) -> String {
    names
        .iter()
        .find_map(|name| env_value(name))
        .unwrap_or_else(|| fallback.to_owned())
}

pub fn read_bool_env(name: &str, fallback: bool) -> bool {
    let Some(value) = env_value(name) else {
        return fallback;
    };
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

pub fn env_filter(default: &str) -> tracing_subscriber::EnvFilter {
    env_value("RUST_LOG")
        .and_then(|filter| tracing_subscriber::EnvFilter::try_new(filter).ok())
        .unwrap_or_else(|| tracing_subscriber::EnvFilter::new(default))
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

fn canonicalize_domain(value: &str) -> String {
    let mut domain = value.trim().to_lowercase();
    if domain.ends_with('.') {
        domain.pop();
    }
    domain
}

fn default_port(scheme: &str) -> u16 {
    if scheme == "https" { 443 } else { 80 }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PublicOrigin {
    pub scheme: String,
    pub domain: String,
    pub port: u16,
}

pub fn parse_public_origin(origin: &str) -> Option<PublicOrigin> {
    let trimmed = origin.trim();
    if trimmed.is_empty() {
        return None;
    }
    let parsed = url::Url::parse(trimmed).ok()?;
    let scheme = parsed.scheme();
    if scheme != "http" && scheme != "https" {
        return None;
    }
    if parsed.path() != "/" || parsed.query().is_some() || parsed.fragment().is_some() {
        return None;
    }
    if !parsed.username().is_empty() || parsed.password().is_some() {
        return None;
    }
    let domain = canonicalize_domain(parsed.host_str()?);
    if domain.is_empty() {
        return None;
    }
    Some(PublicOrigin {
        scheme: scheme.to_owned(),
        domain,
        port: parsed.port().unwrap_or_else(|| default_port(scheme)),
    })
}

pub fn resolve_public_domain_and_port<F>(mut read_var: F) -> anyhow::Result<(String, Option<u16>)>
where
    F: FnMut(&str) -> Option<String>,
{
    let mut non_empty_var = |name: &str| {
        read_var(name)
            .map(|value| value.trim().to_owned())
            .filter(|value| !value.is_empty())
    };
    let configured_port = match non_empty_var("FLUXER_PUBLIC_PORT") {
        None => None,
        Some(raw) => Some(
            raw.parse::<u16>()
                .ok()
                .filter(|port| *port != 0)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "FLUXER_PUBLIC_PORT must be a port between 1 and 65535, got {raw}"
                    )
                })?,
        ),
    };
    let configured_domain =
        non_empty_var("FLUXER_BASE_DOMAIN").map(|value| canonicalize_domain(&value));
    let Some(origin) = non_empty_var("FLUXER_PUBLIC_ORIGIN") else {
        return Ok((configured_domain.unwrap_or_default(), configured_port));
    };
    let parsed = parse_public_origin(&origin).ok_or_else(|| {
        anyhow::anyhow!(
            "FLUXER_PUBLIC_ORIGIN must be a scheme, host and optional port such as https://chat.example.com:8443, got {origin}"
        )
    })?;
    Ok((parsed.domain, Some(parsed.port)))
}

pub fn normalize_public_endpoint(url: &str, base_domain: &str, public_port: Option<u16>) -> String {
    let Some(port) = public_port.filter(|port| *port != 0) else {
        return url.to_owned();
    };
    let domain = canonicalize_domain(base_domain);
    if domain.is_empty() {
        return url.to_owned();
    }
    let Ok(parsed) = url::Url::parse(url) else {
        return url.to_owned();
    };
    if canonicalize_domain(parsed.host_str().unwrap_or_default()) != domain {
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

pub fn try_normalize_public_endpoint_from_env(url: &str) -> anyhow::Result<String> {
    let (base_domain, public_port) = resolve_public_domain_and_port(env_value)?;
    Ok(normalize_public_endpoint(url, &base_domain, public_port))
}

pub fn normalize_public_endpoint_from_env(url: &str) -> String {
    try_normalize_public_endpoint_from_env(url).unwrap_or_else(|error| panic!("{error}"))
}

#[derive(Debug, Default)]
pub struct EnvMap(Vec<(String, String)>);

impl EnvMap {
    pub fn from_pairs<I, K, V>(vars: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        Self(
            vars.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.0
            .iter()
            .find_map(|(k, v)| (k == key).then_some(v.as_str()))
            .filter(|v| !v.trim().is_empty())
    }
}

pub fn parse_bool(var_name: &str, raw: Option<&str>) -> anyhow::Result<Option<bool>> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    match raw.to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" => Ok(Some(true)),
        "false" | "0" | "no" => Ok(Some(false)),
        _ => Err(anyhow::anyhow!(
            "{var_name} must be a boolean: true, false, 1, 0, yes, or no"
        )),
    }
}

pub fn parse_number<T>(
    var_name: &str,
    raw: Option<&str>,
    default_value: T,
    min_value: T,
    max_value: T,
) -> anyhow::Result<T>
where
    T: std::str::FromStr + PartialOrd + std::fmt::Display + Copy,
{
    let Some(raw) = raw else {
        return Ok(default_value);
    };
    let parsed = raw
        .trim()
        .parse::<T>()
        .map_err(|_| anyhow::anyhow!("{var_name} must be a number"))?;
    anyhow::ensure!(
        parsed >= min_value && parsed <= max_value,
        "{var_name} must be between {min_value} and {max_value}"
    );
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_the_typescript_normalizer_on_the_shared_vectors() {
        let raw = include_str!("testdata/public_endpoint_vectors.json");
        let vectors: serde_json::Value = serde_json::from_str(raw).expect("vectors parse as json");
        let vectors = vectors.as_array().expect("vectors are an array");
        assert!(!vectors.is_empty());
        for vector in vectors {
            let url = vector["url"].as_str().expect("vector has a url");
            let base_domain = vector["base_domain"]
                .as_str()
                .expect("vector has a base domain");
            let public_port = vector["public_port"].as_u64().map(|port| port as u16);
            let expected = vector["normalized"]
                .as_str()
                .expect("vector has a normalized url");
            assert_eq!(
                expected,
                normalize_public_endpoint(url, base_domain, public_port),
                "vector {url} @ {base_domain} port {public_port:?}"
            );
        }
    }
}
