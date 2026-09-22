// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::GeneratedClient;
use crate::{config::AdminConfig, session::Session};
use progenitor_client::ClientInfo;
use reqwest::header::{AUTHORIZATION, HeaderMap, HeaderName, HeaderValue};
use reqwest::{Method, RequestBuilder};
use serde::Serialize;
use serde::de::DeserializeOwned;

#[derive(Debug)]
pub enum ApiError {
    Network(String),
    Http { status: u16, message: String },
    Parse(String),
}

pub type ApiResult<T> = Result<T, ApiError>;

const INTERNAL_ADMIN_PROXY_CLIENT_IP: &str = "127.0.0.1";

pub trait ApiResultExt<T> {
    fn log_error(self, context: &str) -> Option<T>;
}

impl<T> ApiResultExt<T> for ApiResult<T> {
    fn log_error(self, context: &str) -> Option<T> {
        match self {
            Ok(value) => Some(value),
            Err(error) => {
                tracing::warn!(%context, %error, "admin API request failed");
                None
            }
        }
    }
}

pub struct AdminApiClient {
    generated: GeneratedClient,
}

impl AdminApiClient {
    pub fn new(http_client: &reqwest::Client, config: &AdminConfig, session: &Session) -> Self {
        let generated = GeneratedClient::new_with_client(
            &config.api_endpoint,
            http_client.clone(),
            build_session_headers(config, session),
        );
        Self { generated }
    }

    fn build_url(&self, path: &str, query_params: Option<&[(&str, &str)]>) -> String {
        let mut url = format!("{}{}", self.generated.baseurl(), path);
        let query = query_params
            .unwrap_or_default()
            .iter()
            .filter(|(_, value)| !value.is_empty())
            .map(|(key, value)| {
                format!(
                    "{}={}",
                    urlencoding::encode(key),
                    urlencoding::encode(value)
                )
            })
            .collect::<Vec<_>>()
            .join("&");
        if !query.is_empty() {
            url.push('?');
            url.push_str(&query);
        }
        url
    }

    fn request(
        &self,
        method: Method,
        path: &str,
        query_params: Option<&[(&str, &str)]>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<RequestBuilder> {
        let url = self.build_url(path, query_params);
        Ok(self
            .generated
            .client()
            .request(method, &url)
            .header("Content-Type", "application/json")
            .headers(self.headers_with_reason(audit_log_reason)?))
    }

    fn headers_with_reason(&self, audit_log_reason: Option<&str>) -> ApiResult<HeaderMap> {
        let mut headers = self.generated.inner().clone();
        if let Some(reason) = audit_log_reason {
            let mut value = HeaderValue::from_str(reason)
                .map_err(|_| ApiError::Parse("invalid audit log reason header".to_owned()))?;
            value.set_sensitive(true);
            headers.insert("x-audit-log-reason", value);
        }
        Ok(headers)
    }

    async fn send_request(builder: RequestBuilder) -> ApiResult<reqwest::Response> {
        builder
            .send()
            .await
            .map_err(|e| ApiError::Network(e.to_string()))
    }

    async fn send_json<B: Serialize + ?Sized>(
        &self,
        method: Method,
        path: &str,
        body: Option<&B>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<reqwest::Response> {
        let builder = self.request(method, path, None, audit_log_reason)?;
        let builder = match body {
            Some(body) => builder.json(body),
            None => builder,
        };
        Self::send_request(builder).await
    }

    pub async fn get<T: DeserializeOwned>(
        &self,
        path: &str,
        query_params: Option<&[(&str, &str)]>,
    ) -> ApiResult<T> {
        let response =
            Self::send_request(self.request(Method::GET, path, query_params, None)?).await?;
        Self::parse_response(response).await
    }

    pub async fn post<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
    ) -> ApiResult<T> {
        self.post_with_reason(path, body, None).await
    }

    pub async fn post_typed<T, B>(&self, path: &str, body: &B) -> ApiResult<T>
    where
        T: DeserializeOwned,
        B: Serialize + ?Sized,
    {
        self.post_typed_with_reason(path, body, None).await
    }

    pub async fn post_typed_with_reason<T, B>(
        &self,
        path: &str,
        body: &B,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T>
    where
        T: DeserializeOwned,
        B: Serialize + ?Sized,
    {
        let response = self
            .send_json(Method::POST, path, Some(body), audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn post_with_reason<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T> {
        let response = self
            .send_json(Method::POST, path, body, audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn post_void(&self, path: &str, body: Option<&serde_json::Value>) -> ApiResult<()> {
        self.post_void_with_reason(path, body, None).await
    }

    pub async fn post_void_with_reason(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let response = self
            .send_json(Method::POST, path, body, audit_log_reason)
            .await?;
        Self::parse_void_response(response).await
    }

    pub async fn patch<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
    ) -> ApiResult<T> {
        self.patch_with_reason(path, body, None).await
    }

    pub async fn patch_with_reason<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T> {
        let response = self
            .send_json(Method::PATCH, path, body, audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn patch_typed_with_reason<T, B>(
        &self,
        path: &str,
        body: &B,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T>
    where
        T: DeserializeOwned,
        B: Serialize + ?Sized,
    {
        let response = self
            .send_json(Method::PATCH, path, Some(body), audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn put_with_reason<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T> {
        let response = self
            .send_json(Method::PUT, path, body, audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn put_typed_with_reason<T, B>(
        &self,
        path: &str,
        body: &B,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T>
    where
        T: DeserializeOwned,
        B: Serialize + ?Sized,
    {
        let response = self
            .send_json(Method::PUT, path, Some(body), audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    pub async fn put_void_with_reason(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let response = self
            .send_json(Method::PUT, path, body, audit_log_reason)
            .await?;
        Self::parse_void_response(response).await
    }

    pub async fn delete_void(&self, path: &str, body: Option<&serde_json::Value>) -> ApiResult<()> {
        self.delete_void_with_reason(path, body, None).await
    }

    pub async fn delete_void_with_reason(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let response = self
            .send_json(Method::DELETE, path, body, audit_log_reason)
            .await?;
        Self::parse_void_response(response).await
    }

    pub async fn delete_with_reason<T: DeserializeOwned>(
        &self,
        path: &str,
        body: Option<&serde_json::Value>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T> {
        let response = self
            .send_json(Method::DELETE, path, body, audit_log_reason)
            .await?;
        Self::parse_response(response).await
    }

    async fn parse_void_response(response: reqwest::Response) -> ApiResult<()> {
        Self::check_response_status(response).await.map(drop)
    }

    async fn check_response_status(response: reqwest::Response) -> ApiResult<reqwest::Response> {
        if response.status().is_success() {
            return Ok(response);
        }
        let status = response.status().as_u16();
        let message = response.text().await.map_err(|error| {
            ApiError::Network(format!("failed to read error response body: {error}"))
        })?;
        Err(ApiError::Http { status, message })
    }

    pub(crate) fn generated(&self) -> &GeneratedClient {
        &self.generated
    }

    pub(crate) fn generated_with_reason(
        &self,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<GeneratedClient> {
        Ok(GeneratedClient::new_with_client(
            self.generated.baseurl(),
            self.generated.client().clone(),
            self.headers_with_reason(audit_log_reason)?,
        ))
    }

    pub(crate) fn generated_value<T, U>(&self, value: U) -> ApiResult<T>
    where
        T: DeserializeOwned,
        U: Serialize,
    {
        let json = serde_json::to_value(value).map_err(|e| ApiError::Parse(e.to_string()))?;
        serde_json::from_value(json).map_err(|e| ApiError::Parse(e.to_string()))
    }

    pub(crate) fn generated_error<E: std::fmt::Debug>(
        &self,
        error: progenitor_client::Error<E>,
    ) -> ApiError {
        match error.status() {
            Some(status) => ApiError::Http {
                status: status.as_u16(),
                message: format!("{error:?}"),
            },
            None => ApiError::Network(format!("{error:?}")),
        }
    }

    async fn parse_response<T: DeserializeOwned>(response: reqwest::Response) -> ApiResult<T> {
        let response = Self::check_response_status(response).await?;
        let text = if response.status() == reqwest::StatusCode::NO_CONTENT {
            String::new()
        } else {
            response
                .text()
                .await
                .map_err(|e| ApiError::Network(e.to_string()))?
        };
        if text.is_empty() {
            return serde_json::from_value(serde_json::Value::Null)
                .map_err(|e| ApiError::Parse(e.to_string()));
        }
        serde_json::from_str(&text).map_err(|e| ApiError::Parse(e.to_string()))
    }
}

fn build_session_headers(config: &AdminConfig, session: &Session) -> HeaderMap {
    let mut headers = HeaderMap::new();
    let mut auth_value = HeaderValue::from_str(&format!("Bearer {}", session.access_token))
        .expect("failed to build generated API Authorization header");
    auth_value.set_sensitive(true);
    headers.insert(AUTHORIZATION, auth_value);
    headers.extend(build_proxy_client_ip_headers(config));
    headers
}

pub(crate) fn with_proxy_client_ip_header(
    builder: RequestBuilder,
    config: &AdminConfig,
) -> RequestBuilder {
    match proxy_client_ip_header_name(config) {
        Some(name) => builder.header(name, INTERNAL_ADMIN_PROXY_CLIENT_IP),
        None => builder,
    }
}

fn build_proxy_client_ip_headers(config: &AdminConfig) -> HeaderMap {
    let mut headers = HeaderMap::new();
    if let Some(name) = proxy_client_ip_header_name(config) {
        headers.insert(
            name,
            HeaderValue::from_static(INTERNAL_ADMIN_PROXY_CLIENT_IP),
        );
    }
    headers
}

fn proxy_client_ip_header_name(config: &AdminConfig) -> Option<HeaderName> {
    if !config.proxy.trust_client_ip_header {
        return None;
    }
    match HeaderName::from_bytes(config.proxy.client_ip_header_name.as_bytes()) {
        Ok(name) => Some(name),
        Err(error) => {
            tracing::warn!(
                header = %config.proxy.client_ip_header_name,
                %error,
                "invalid admin client IP header name; internal API calls may be rejected"
            );
            None
        }
    }
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Network(msg) => write!(f, "network error: {msg}"),
            Self::Http { status, message } => write!(f, "HTTP {status}: {message}"),
            Self::Parse(msg) => write!(f, "parse error: {msg}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{Value, json};

    fn response(status: u16, body: &'static str) -> reqwest::Response {
        axum::http::Response::builder()
            .status(status)
            .body(body)
            .expect("valid response")
            .into()
    }

    #[tokio::test]
    async fn parses_successful_json_and_empty_responses() {
        for (status, body, expected) in [
            (200, r#"{"value":1}"#, json!({"value": 1})),
            (201, "[1,2]", json!([1, 2])),
            (202, "null", Value::Null),
            (200, "", Value::Null),
            (204, "ignored body", Value::Null),
        ] {
            let actual: Value = AdminApiClient::parse_response(response(status, body))
                .await
                .expect("valid response body");
            assert_eq!(actual, expected, "HTTP {status}: {body}");
        }
    }

    #[tokio::test]
    async fn empty_responses_preserve_null_deserialization_errors() {
        let expected = serde_json::from_value::<Vec<String>>(Value::Null)
            .expect_err("null is not a list")
            .to_string();
        for (status, body) in [(200, ""), (204, "ignored body")] {
            let error = AdminApiClient::parse_response::<Vec<String>>(response(status, body))
                .await
                .expect_err("missing list");
            assert_eq!(error.to_string(), format!("parse error: {expected}"));
        }
    }

    #[tokio::test]
    async fn malformed_json_preserves_deserialization_errors() {
        for body in [" ", "{", "not JSON"] {
            let expected = serde_json::from_str::<Value>(body)
                .expect_err("malformed JSON")
                .to_string();
            let error = AdminApiClient::parse_response::<Value>(response(200, body))
                .await
                .expect_err("malformed response");
            assert_eq!(error.to_string(), format!("parse error: {expected}"));
        }
    }

    #[tokio::test]
    async fn void_responses_do_not_parse_successful_bodies() {
        for status in [200, 201, 202, 204] {
            AdminApiClient::parse_void_response(response(status, "not JSON"))
                .await
                .expect("successful void response");
        }
    }

    #[tokio::test]
    async fn typed_and_void_responses_preserve_http_errors() {
        for status in [302, 400, 403, 404, 500] {
            for body in ["", "plain error", r#"{"code":"FORBIDDEN"}"#] {
                let typed = AdminApiClient::parse_response::<Value>(response(status, body))
                    .await
                    .map(drop);
                let empty = AdminApiClient::parse_void_response(response(status, body)).await;
                for result in [typed, empty] {
                    let error = result.expect_err("unsuccessful response");
                    assert_eq!(error.to_string(), format!("HTTP {status}: {body}"));
                }
            }
        }
    }
}
