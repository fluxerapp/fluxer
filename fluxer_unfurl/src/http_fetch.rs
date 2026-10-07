// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::network_policy;
use reqwest::header::{HeaderMap, LOCATION};
use std::time::Duration;
use tokio::time::timeout;

pub const DEFAULT_MAX_BYTES: usize = 8 * 1024 * 1024;

pub const DEFAULT_HTML_MAX_BYTES: usize = DEFAULT_MAX_BYTES;

pub const DEFAULT_TIMEOUT: Duration = Duration::from_secs(10);
pub const DEFAULT_MAX_REDIRECTS: usize = 5;

pub struct FetchResult {
    pub bytes: Vec<u8>,
    pub headers: HeaderMap,
    pub status: u16,
}

pub struct FetchHead {
    pub content_type: Option<String>,
    pub final_url: String,
    pub status: u16,
}

pub struct FetchMaybeBodyResult {
    pub bytes: Option<Vec<u8>>,
    pub content_type: Option<String>,
    pub headers: HeaderMap,
    pub final_url: String,
    pub status: u16,
}

pub async fn fetch_url(
    client: &reqwest::Client,
    url: &str,
    max_bytes: usize,
    request_timeout: Duration,
) -> anyhow::Result<FetchResult> {
    fetch_url_with_headers(client, url, HeaderMap::new(), max_bytes, request_timeout).await
}

pub async fn fetch_url_with_headers(
    client: &reqwest::Client,
    url: &str,
    request_headers: HeaderMap,
    max_bytes: usize,
    request_timeout: Duration,
) -> anyhow::Result<FetchResult> {
    let result = fetch_url_with_headers_maybe_body(
        client,
        url,
        request_headers,
        max_bytes,
        request_timeout,
        |_| true,
    )
    .await?;

    Ok(FetchResult {
        bytes: result.bytes.unwrap_or_default(),
        headers: result.headers,
        status: result.status,
    })
}

pub async fn fetch_url_maybe_body<F>(
    client: &reqwest::Client,
    url: &str,
    max_bytes: usize,
    request_timeout: Duration,
    should_read_body: F,
) -> anyhow::Result<FetchMaybeBodyResult>
where
    F: Fn(&FetchHead) -> bool,
{
    fetch_url_with_headers_maybe_body(
        client,
        url,
        HeaderMap::new(),
        max_bytes,
        request_timeout,
        should_read_body,
    )
    .await
}

pub async fn fetch_url_with_headers_maybe_body<F>(
    client: &reqwest::Client,
    url: &str,
    request_headers: HeaderMap,
    max_bytes: usize,
    request_timeout: Duration,
    should_read_body: F,
) -> anyhow::Result<FetchMaybeBodyResult>
where
    F: Fn(&FetchHead) -> bool,
{
    let mut current_url = network_policy::parse_url(url)?;
    let mut visited: Vec<String> = Vec::new();
    for redirects in 0..=DEFAULT_MAX_REDIRECTS {
        let current_str = current_url.as_str().to_owned();
        if visited.contains(&current_str) {
            return Err(anyhow::anyhow!("redirect loop detected"));
        }
        visited.push(current_str);
        network_policy::validate_url(&current_url).await?;
        let resp = timeout(
            request_timeout,
            client
                .get(current_url.clone())
                .headers(request_headers.clone())
                .send(),
        )
        .await
        .map_err(|_| anyhow::anyhow!("request timed out"))??;

        let status = resp.status();
        let headers = resp.headers().clone();
        if status.is_redirection() {
            if redirects == DEFAULT_MAX_REDIRECTS {
                return Err(anyhow::anyhow!("too many redirects"));
            }
            let location = headers
                .get(LOCATION)
                .ok_or_else(|| anyhow::anyhow!("redirect missing Location header"))?
                .to_str()
                .map_err(|_| anyhow::anyhow!("redirect Location header is not valid ASCII"))?;
            current_url = network_policy::resolve_redirect(&current_url, location)?;
            continue;
        }

        let final_url = resp.url().to_string();
        let content_type = headers
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .map(|ct| ct.split(';').next().unwrap_or(ct).trim().to_owned());

        let head = FetchHead {
            content_type: content_type.clone(),
            final_url: final_url.clone(),
            status: status.as_u16(),
        };
        if !should_read_body(&head) {
            return Ok(FetchMaybeBodyResult {
                bytes: None,
                content_type,
                headers,
                final_url,
                status: status.as_u16(),
            });
        }

        if let Some(len) = resp.content_length()
            && len > max_bytes as u64
        {
            return Err(anyhow::anyhow!(
                "response body too large: {len} > {max_bytes}"
            ));
        }

        let bytes_result = timeout(request_timeout, read_body(resp, max_bytes))
            .await
            .map_err(|_| anyhow::anyhow!("body read timed out"))??;

        return Ok(FetchMaybeBodyResult {
            bytes: Some(bytes_result),
            content_type,
            headers,
            final_url,
            status: status.as_u16(),
        });
    }
    Err(anyhow::anyhow!("too many redirects"))
}

async fn read_body(resp: reqwest::Response, max_bytes: usize) -> anyhow::Result<Vec<u8>> {
    let mut buf = Vec::with_capacity(max_bytes.min(64 * 1024));
    let mut stream = resp;

    while let Some(chunk) = stream.chunk().await? {
        let next_len = buf
            .len()
            .checked_add(chunk.len())
            .ok_or_else(|| anyhow::anyhow!("response body exceeds size limit"))?;
        if next_len > max_bytes {
            return Err(anyhow::anyhow!(
                "response body exceeds size limit of {max_bytes} bytes"
            ));
        }
        buf.extend_from_slice(&chunk);
    }

    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_max_bytes_is_8mib() {
        assert_eq!(DEFAULT_MAX_BYTES, 8 * 1024 * 1024);
    }

    #[test]
    fn default_html_max_bytes_matches_old_unfurl_document_limit() {
        assert_eq!(DEFAULT_HTML_MAX_BYTES, 8 * 1024 * 1024);
    }

    #[test]
    fn default_timeout_is_10s() {
        assert_eq!(DEFAULT_TIMEOUT, Duration::from_secs(10));
    }

    #[tokio::test]
    async fn rejects_blocked_ip_literal_before_fetch() {
        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .unwrap();
        let result = fetch_url(
            &client,
            "http://127.0.0.1/",
            DEFAULT_MAX_BYTES,
            Duration::from_secs(1),
        )
        .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn rejects_private_ip_ranges() {
        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .unwrap();
        for url in [
            "http://10.0.0.1/",
            "http://192.168.1.1/",
            "http://172.16.0.1/",
            "http://[::1]/",
        ] {
            assert!(
                fetch_url(&client, url, DEFAULT_MAX_BYTES, Duration::from_secs(1))
                    .await
                    .is_err(),
                "should reject {url}"
            );
        }
    }

    #[tokio::test]
    async fn fetch_url_maybe_body_skips_body_when_policy_returns_false() {
        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .unwrap();
        let result = fetch_url_maybe_body(
            &client,
            "http://127.0.0.1/",
            DEFAULT_MAX_BYTES,
            Duration::from_secs(1),
            |_| false,
        )
        .await;
        assert!(result.is_err());
    }

    #[test]
    #[allow(clippy::assertions_on_constants)]
    fn default_max_redirects_is_reasonable() {
        assert!(DEFAULT_MAX_REDIRECTS >= 3 && DEFAULT_MAX_REDIRECTS <= 10);
    }
}
