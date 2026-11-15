// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{dns_txt, http_fetch, network_policy};
use reqwest::header::{ACCEPT_ENCODING, CONTENT_ENCODING, HeaderValue, LOCATION};
use std::time::Duration;
use tokio::time::timeout;
use url::Url;

const WELL_KNOWN_TIMEOUT: Duration = Duration::from_millis(3000);
const WELL_KNOWN_MAX_BYTES: usize = 16 * 1024;
const WELL_KNOWN_PATH: &str = "/.well-known/fluxer-verification";
const WELL_KNOWN_MAX_REDIRECTS: usize = 3;
const MAX_TOKEN_BYTES: usize = 256;

pub async fn verify_domain(
    client: &reqwest::Client,
    domain: &str,
    token: &str,
) -> anyhow::Result<bool> {
    dns_txt::validate_hostname(domain)?;
    if token.is_empty() || token.len() > MAX_TOKEN_BYTES {
        anyhow::bail!("invalid verification token");
    }
    if dns_txt_matches(domain, token).await {
        return Ok(true);
    }
    match timeout(
        WELL_KNOWN_TIMEOUT,
        well_known_matches(client, domain, token),
    )
    .await
    {
        Ok(Ok(matched)) => Ok(matched),
        Ok(Err(err)) => {
            tracing::debug!(error = %err, domain, "well-known verification lookup failed");
            Ok(false)
        }
        Err(_) => {
            tracing::debug!(domain, "well-known verification lookup timed out");
            Ok(false)
        }
    }
}

async fn dns_txt_matches(domain: &str, token: &str) -> bool {
    let expected = format!("fluxer-verification={token}");
    dns_txt::first_answer(&format!("_fluxer.{domain}"), |records| {
        records.iter().any(|record| record == &expected)
    })
    .await
    .is_some()
}

async fn well_known_matches(
    client: &reqwest::Client,
    domain: &str,
    token: &str,
) -> anyhow::Result<bool> {
    let mut url = Url::parse(&format!("https://{domain}{WELL_KNOWN_PATH}"))?;
    for _ in 0..=WELL_KNOWN_MAX_REDIRECTS {
        if !is_same_site_well_known(&url, domain) {
            return Ok(false);
        }
        network_policy::validate_url(&url).await?;
        let response = client
            .get(url.clone())
            .header(ACCEPT_ENCODING, HeaderValue::from_static("identity"))
            .send()
            .await?;
        if response.status().is_redirection() {
            let Some(location) = response
                .headers()
                .get(LOCATION)
                .and_then(|value| value.to_str().ok())
            else {
                return Ok(false);
            };
            url = network_policy::resolve_redirect(&url, location)?;
            continue;
        }
        if !response.status().is_success() {
            return Ok(false);
        }
        if let Some(encoding) = response.headers().get(CONTENT_ENCODING)
            && !encoding
                .to_str()
                .is_ok_and(|value| value.trim().eq_ignore_ascii_case("identity"))
        {
            return Ok(false);
        }
        if response
            .content_length()
            .is_some_and(|len| len > WELL_KNOWN_MAX_BYTES as u64)
        {
            return Ok(false);
        }
        let body = http_fetch::read_body(response, WELL_KNOWN_MAX_BYTES).await?;
        return Ok(body_matches_token(&body, token));
    }
    Ok(false)
}

fn is_same_site_well_known(url: &Url, domain: &str) -> bool {
    let Some(host) = url.host_str() else {
        return false;
    };
    let same_site = host == domain
        || host.strip_prefix("www.") == Some(domain)
        || domain.strip_prefix("www.") == Some(host);
    url.scheme() == "https"
        && same_site
        && url.port().is_none()
        && url.path() == WELL_KNOWN_PATH
        && url.query().is_none()
        && url.username().is_empty()
        && url.password().is_none()
}

fn body_matches_token(body: &[u8], token: &str) -> bool {
    let text = String::from_utf8_lossy(body);
    text.strip_prefix('\u{feff}').unwrap_or(&text).trim() == token
}
