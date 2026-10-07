// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{api::client::with_proxy_client_ip_header, config::AdminConfig};
use base64::{Engine as _, engine::general_purpose::STANDARD};
use serde::Deserialize;

pub const ADMIN_OAUTH_SCOPE: &str = "identify email";

#[derive(Debug, Deserialize)]
pub struct TokenResponse {
    pub access_token: String,
}

#[derive(Debug, Deserialize)]
pub struct UserInfo {
    pub id: String,
}

pub fn authorize_url(config: &AdminConfig, state: &str) -> String {
    let params = [
        ("client_id", config.oauth_client_id.as_str()),
        ("redirect_uri", config.oauth_redirect_uri.as_str()),
        ("response_type", "code"),
        ("scope", ADMIN_OAUTH_SCOPE),
        ("state", state),
    ];
    let query = params
        .iter()
        .map(|(k, v)| format!("{}={}", k, urlencoding::encode(v)))
        .collect::<Vec<_>>()
        .join("&");
    format!("{}/oauth2/authorize?{}", config.web_app_endpoint, query)
}

pub async fn exchange_code(
    http_client: &reqwest::Client,
    config: &AdminConfig,
    code: &str,
) -> anyhow::Result<TokenResponse> {
    let response = with_proxy_client_ip_header(
        http_client.post(format!("{}/oauth2/token", config.api_endpoint)),
        config,
    )
    .header("Content-Type", "application/x-www-form-urlencoded")
    .body(
        [
            ("grant_type", "authorization_code"),
            ("code", code),
            ("redirect_uri", config.oauth_redirect_uri.as_str()),
            ("client_id", config.oauth_client_id.as_str()),
            ("client_secret", config.oauth_client_secret.as_str()),
        ]
        .iter()
        .map(|(k, v)| format!("{}={}", k, urlencoding::encode(v)))
        .collect::<Vec<_>>()
        .join("&"),
    )
    .send()
    .await?;
    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.map_err(|error| {
            anyhow::anyhow!("failed to read token exchange error body: {error}")
        })?;
        tracing::error!(
            %status,
            url = format!("{}/oauth2/token", config.api_endpoint),
            redirect_uri = %config.oauth_redirect_uri,
            client_id = %config.oauth_client_id,
            %body,
            "token exchange failed"
        );
        anyhow::bail!("token exchange failed with status {status}: {body}");
    }
    let token: TokenResponse = response.json().await?;
    Ok(token)
}

pub async fn fetch_user_info(
    http_client: &reqwest::Client,
    config: &AdminConfig,
    access_token: &str,
) -> anyhow::Result<UserInfo> {
    let response = with_proxy_client_ip_header(
        http_client.get(format!("{}/users/@me", config.api_endpoint)),
        config,
    )
    .header("Authorization", format!("Bearer {}", access_token))
    .send()
    .await?;
    if !response.status().is_success() {
        anyhow::bail!("user info fetch failed with status {}", response.status());
    }
    let user: UserInfo = response.json().await?;
    Ok(user)
}

pub async fn revoke_token(
    http_client: &reqwest::Client,
    config: &AdminConfig,
    token: &str,
) -> anyhow::Result<()> {
    let credentials = format!("{}:{}", config.oauth_client_id, config.oauth_client_secret);
    let basic = format!("Basic {}", STANDARD.encode(credentials.as_bytes()));
    let response = with_proxy_client_ip_header(
        http_client.post(format!("{}/oauth2/token/revoke", config.api_endpoint)),
        config,
    )
    .header("Content-Type", "application/x-www-form-urlencoded")
    .header("Authorization", basic)
    .body(format!(
        "token={}&token_type_hint=access_token",
        urlencoding::encode(token)
    ))
    .send()
    .await?;
    if !response.status().is_success() {
        anyhow::bail!("token revoke failed with status {}", response.status());
    }
    Ok(())
}
