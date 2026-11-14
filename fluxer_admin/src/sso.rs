// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{ApiError, with_proxy_client_ip_header},
    config::{AdminConfig, SsoConfig},
    oauth2::admin_client_basic_auth,
};
use anyhow::Context as _;
use base64::{
    Engine as _,
    engine::general_purpose::{STANDARD, URL_SAFE_NO_PAD},
};
use hmac::{Hmac, KeyInit, Mac};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::Duration;

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

pub(crate) fn http_client(build_version: &str) -> anyhow::Result<reqwest::Client> {
    reqwest::Client::builder()
        .user_agent(format!("FluxerAdmin/{build_version} (Rust)"))
        .https_only(true)
        .redirect(reqwest::redirect::Policy::none())
        .timeout(Duration::from_secs(15))
        .no_gzip()
        .no_brotli()
        .no_deflate()
        .http1_only()
        .build()
        .context("failed to create the SSO HTTP client")
}

fn parse_https_url(value: &str) -> anyhow::Result<url::Url> {
    let url = url::Url::parse(value)?;
    anyhow::ensure!(
        url.scheme() == "https"
            && url.has_host()
            && url.username().is_empty()
            && url.password().is_none()
            && url.fragment().is_none(),
        "SSO URLs must use https and contain no credentials or fragment"
    );
    Ok(url)
}

pub(crate) fn validate_issuer_url(issuer: &str) -> anyhow::Result<()> {
    let url = parse_https_url(issuer)?;
    anyhow::ensure!(url.query().is_none(), "SSO issuer must contain no query");
    Ok(())
}

fn validate_metadata(metadata: &ProviderMetadata, issuer: &str) -> anyhow::Result<()> {
    if metadata.issuer.trim_end_matches('/') != issuer.trim_end_matches('/') {
        anyhow::bail!("provider discovery returned a different issuer");
    }
    parse_https_url(&metadata.authorization_endpoint)?;
    parse_https_url(&metadata.token_endpoint)?;
    Ok(())
}

pub async fn discover(http: &reqwest::Client, sso: &SsoConfig) -> anyhow::Result<ProviderMetadata> {
    validate_issuer_url(&sso.issuer)?;
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
    validate_metadata(&metadata, issuer)?;
    Ok(metadata)
}

pub fn authorize_url(
    metadata: &ProviderMetadata,
    sso: &SsoConfig,
    state: &str,
    nonce: &str,
    code_challenge: &str,
) -> anyhow::Result<String> {
    let mut url = parse_https_url(&metadata.authorization_endpoint)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn provider_client_refuses_http_before_connecting() {
        let error = http_client("test")
            .unwrap()
            .get("http://127.0.0.1/")
            .send()
            .await
            .expect_err("plaintext provider requests must fail");
        assert!(error.is_builder());
    }

    #[test]
    fn issuer_requires_https_without_credentials_query_or_fragment() {
        for issuer in [
            "http://id.example.com",
            "ftp://id.example.com",
            "id.example.com",
            "https://user:secret@id.example.com",
            "https://id.example.com?tenant=staff",
            "https://id.example.com#tenant",
        ] {
            assert!(validate_issuer_url(issuer).is_err(), "{issuer}");
        }
        for issuer in [
            "https://id.example.com",
            "https://id.example.com/",
            "https://id.example.com/realms/staff",
        ] {
            assert!(validate_issuer_url(issuer).is_ok(), "{issuer}");
        }
    }

    #[test]
    fn discovery_rejects_insecure_or_ambiguous_endpoints() {
        let issuer = "https://id.example.com";
        let valid = ProviderMetadata {
            issuer: issuer.to_owned(),
            authorization_endpoint: format!("{issuer}/authorize?tenant=staff"),
            token_endpoint: format!("{issuer}/token"),
        };
        assert!(validate_metadata(&valid, issuer).is_ok());
        for endpoint in [
            "http://id.example.com/endpoint",
            "https://",
            "https://user:secret@id.example.com/endpoint",
            "https://id.example.com/endpoint#fragment",
        ] {
            let authorization = ProviderMetadata {
                authorization_endpoint: endpoint.to_owned(),
                ..valid.clone()
            };
            let token = ProviderMetadata {
                token_endpoint: endpoint.to_owned(),
                ..valid.clone()
            };
            assert!(
                validate_metadata(&authorization, issuer).is_err(),
                "{endpoint}"
            );
            assert!(validate_metadata(&token, issuer).is_err(), "{endpoint}");
        }
    }

    #[test]
    fn discovery_rejects_a_different_issuer() {
        let metadata = ProviderMetadata {
            issuer: "https://other.example.com".to_owned(),
            authorization_endpoint: "https://id.example.com/authorize".to_owned(),
            token_endpoint: "https://id.example.com/token".to_owned(),
        };
        assert!(validate_metadata(&metadata, "https://id.example.com").is_err());
    }
}
