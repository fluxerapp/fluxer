// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{http_fetch, network_policy};
use reqwest::Method;
use reqwest::header::{
    ACCEPT, ACCEPT_ENCODING, AUTHORIZATION, CONTENT_ENCODING, CONTENT_TYPE, HeaderMap, HeaderName,
    HeaderValue,
};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::time::timeout;
use url::Url;

const FETCH_TIMEOUT: Duration = Duration::from_secs(4);
const MAX_RESPONSE_BYTES: usize = 256 * 1024;
const MAX_REQUEST_BODY_BYTES: usize = 64 * 1024;
const DPOP: HeaderName = HeaderName::from_static("dpop");

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FetchedResponse {
    pub status: u16,
    pub headers: Vec<(String, String)>,
    pub body: String,
}

pub async fn fetch(
    client: &reqwest::Client,
    method: &str,
    url: &str,
    headers: &[(String, String)],
    body: Option<String>,
) -> anyhow::Result<FetchedResponse> {
    let method = match method {
        "GET" => Method::GET,
        "POST" => Method::POST,
        _ => anyhow::bail!("unsupported method"),
    };
    let url = network_policy::parse_url(url)?;
    validate_target(&url)?;
    if body
        .as_ref()
        .is_some_and(|b| b.len() > MAX_REQUEST_BODY_BYTES)
    {
        anyhow::bail!("request body too large");
    }
    let headers = forwarded_headers(headers)?;
    timeout(FETCH_TIMEOUT, async move {
        network_policy::validate_url(&url).await?;
        let mut request = client.request(method, url).headers(headers);
        if let Some(body) = body {
            request = request.body(body);
        }
        let response = request.send().await?;
        let status = response.status().as_u16();
        if let Some(encoding) = response.headers().get(CONTENT_ENCODING)
            && !encoding
                .to_str()
                .is_ok_and(|value| value.trim().eq_ignore_ascii_case("identity"))
        {
            anyhow::bail!("encoded response bodies are not accepted");
        }
        if response
            .content_length()
            .is_some_and(|len| len > MAX_RESPONSE_BYTES as u64)
        {
            anyhow::bail!("response body too large");
        }
        let headers = response
            .headers()
            .iter()
            .filter_map(|(name, value)| {
                value
                    .to_str()
                    .ok()
                    .map(|value| (name.as_str().to_owned(), value.to_owned()))
            })
            .collect();
        let bytes = http_fetch::read_body(response, MAX_RESPONSE_BYTES).await?;
        Ok(FetchedResponse {
            status,
            headers,
            body: String::from_utf8(bytes)
                .map_err(|_| anyhow::anyhow!("response body is not UTF-8"))?,
        })
    })
    .await
    .map_err(|_| anyhow::anyhow!("request timed out"))?
}

fn validate_target(url: &Url) -> anyhow::Result<()> {
    if url.scheme() != "https" || url.port().is_some() || url.fragment().is_some() {
        anyhow::bail!("blocked URL");
    }
    if !matches!(url.host(), Some(url::Host::Domain(_))) {
        anyhow::bail!("blocked URL");
    }
    Ok(())
}

fn forwarded_headers(headers: &[(String, String)]) -> anyhow::Result<HeaderMap> {
    let mut forwarded = HeaderMap::new();
    for (name, value) in headers {
        let name = HeaderName::from_bytes(name.as_bytes())?;
        if [ACCEPT, AUTHORIZATION, CONTENT_TYPE, DPOP].contains(&name) {
            forwarded.append(name, HeaderValue::from_str(value)?);
        }
    }
    forwarded.insert(ACCEPT_ENCODING, HeaderValue::from_static("identity"));
    Ok(forwarded)
}
