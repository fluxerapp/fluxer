// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{ApiError, with_proxy_client_ip_header},
    config::{AdminConfig, SsoConfig},
    oauth2::admin_client_basic_auth,
};
use base64::{
    Engine as _,
    engine::general_purpose::{STANDARD, URL_SAFE_NO_PAD},
};
use hmac::{Hmac, KeyInit, Mac};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

pub const SSO_SCOPE: &str = "openid profile";

#[derive(Clone, Debug, Deserialize)]
pub struct ProviderMetadata {
    pub issuer: String,
    pub authorization_endpoint: String,
    pub token_endpoint: String,
}

#[derive(Deserialize)]
struct TokenResponse {
    id_token: String,
}

#[derive(Deserialize)]
struct TokenErrorResponse {
    error: Option<String>,
}

#[derive(Serialize)]
struct SignInRequest<'a> {
    id_token: &'a str,
    nonce: &'a str,
}

#[derive(Debug, Deserialize)]
pub struct SsoSignInResponse {
    pub user_id: String,
    pub access_token: String,
}

pub async fn discover(http: &reqwest::Client, sso: &SsoConfig) -> anyhow::Result<ProviderMetadata> {
    let issuer = sso.issuer.trim_end_matches('/');
    let response = http
        .get(format!("{issuer}/.well-known/openid-configuration"))
        .header("Accept", "application/json")
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        anyhow::bail!("provider discovery failed with status {status}");
    }
    let metadata: ProviderMetadata = response.json().await?;
    if metadata.issuer.trim_end_matches('/') != issuer {
        anyhow::bail!("provider discovery returned a different issuer");
    }
    if issuer.starts_with("https://")
        && !(metadata.authorization_endpoint.starts_with("https://")
            && metadata.token_endpoint.starts_with("https://"))
    {
        anyhow::bail!("provider discovery returned an endpoint that is not https");
    }
    Ok(metadata)
}

pub fn authorize_url(
    metadata: &ProviderMetadata,
    sso: &SsoConfig,
    state: &str,
    nonce: &str,
    code_challenge: &str,
) -> anyhow::Result<String> {
    let mut url = url::Url::parse(&metadata.authorization_endpoint)?;
    url.query_pairs_mut()
        .append_pair("response_type", "code")
        .append_pair("client_id", &sso.client_id)
        .append_pair("redirect_uri", &sso.redirect_uri)
        .append_pair("scope", SSO_SCOPE)
        .append_pair("state", state)
        .append_pair("nonce", nonce)
        .append_pair("code_challenge", code_challenge)
        .append_pair("code_challenge_method", "S256");
    Ok(url.into())
}

pub fn code_challenge(verifier: &str) -> String {
    URL_SAFE_NO_PAD.encode(Sha256::digest(verifier.as_bytes()))
}

pub fn nonce(secret_key_base: &str, verifier: &str) -> String {
    let mut mac = Hmac::<Sha256>::new_from_slice(secret_key_base.as_bytes())
        .expect("HMAC can take key of any size");
    mac.update(b"fluxer-admin-sso-nonce:");
    mac.update(verifier.as_bytes());
    URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

fn form_encode(pairs: &[(&str, &str)]) -> String {
    url::form_urlencoded::Serializer::new(String::new())
        .extend_pairs(pairs)
        .finish()
}

fn client_secret_basic(sso: &SsoConfig) -> String {
    let id = urlencoding::encode(&sso.client_id);
    let secret = urlencoding::encode(&sso.client_secret);
    format!("Basic {}", STANDARD.encode(format!("{id}:{secret}")))
}

pub async fn exchange_code(
    http: &reqwest::Client,
    metadata: &ProviderMetadata,
    sso: &SsoConfig,
    code: &str,
    verifier: &str,
) -> anyhow::Result<String> {
    let response = http
        .post(&metadata.token_endpoint)
        .header("Content-Type", "application/x-www-form-urlencoded")
        .header("Accept", "application/json")
        .header("Authorization", client_secret_basic(sso))
        .body(form_encode(&[
            ("grant_type", "authorization_code"),
            ("code", code),
            ("redirect_uri", sso.redirect_uri.as_str()),
            ("code_verifier", verifier),
        ]))
        .send()
        .await?;
    let status = response.status();
    if !status.is_success() {
        let error = response
            .json::<TokenErrorResponse>()
            .await
            .ok()
            .and_then(|body| body.error)
            .unwrap_or_default();
        anyhow::bail!("provider token exchange failed with status {status} and error {error:?}");
    }
    let token: TokenResponse = response
        .json()
        .await
        .map_err(|_| anyhow::anyhow!("provider token response has no id_token"))?;
    Ok(token.id_token)
}

pub async fn sign_in(
    http: &reqwest::Client,
    config: &AdminConfig,
    id_token: &str,
    nonce: &str,
) -> Result<SsoSignInResponse, ApiError> {
    let response = with_proxy_client_ip_header(
        http.post(format!("{}/admin/sso/sign-in", config.api_endpoint)),
        config,
    )
    .header("Authorization", admin_client_basic_auth(config))
    .json(&SignInRequest { id_token, nonce })
    .send()
    .await
    .map_err(|error| ApiError::Network(error.to_string()))?;
    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(ApiError::http(status.as_u16(), body));
    }
    response
        .json()
        .await
        .map_err(|error| ApiError::Parse(error.to_string()))
}
