// SPDX-License-Identifier: AGPL-3.0-or-later

use super::html_normalizer;
use reqwest::{Client, redirect::Policy};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NormalizedResponse {
    pub status: u16,
    pub content_type: Option<String>,
    pub location: Option<String>,
    pub body: String,
}

pub fn capture_client() -> Result<Client, String> {
    Client::builder()
        .redirect(Policy::none())
        .build()
        .map_err(|error| format!("failed to build capture client: {error}"))
}

pub async fn fetch_route(
    client: &Client,
    base_url: &str,
    route: &str,
    cookie: Option<&str>,
) -> Result<NormalizedResponse, String> {
    let url = format!("{}{}", base_url.trim_end_matches('/'), route);
    let mut request = client.get(&url);
    if let Some(cookie) = cookie {
        request = request.header(reqwest::header::COOKIE, cookie);
    }
    let response = request
        .send()
        .await
        .map_err(|error| format!("failed to fetch {url}: {error}"))?;
    let status = response.status().as_u16();
    let headers = response.headers().clone();
    let content_type = headers
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .map(|value| html_normalizer::normalize_header_value("content-type", value));
    let location = headers
        .get(reqwest::header::LOCATION)
        .and_then(|value| value.to_str().ok())
        .map(|value| html_normalizer::normalize_header_value("location", value));
    let raw_body = response
        .text()
        .await
        .map_err(|error| format!("failed to read body for {url}: {error}"))?;
    let body = html_normalizer::normalize_body(content_type.as_deref(), &raw_body);
    Ok(NormalizedResponse {
        status,
        content_type,
        location,
        body,
    })
}
