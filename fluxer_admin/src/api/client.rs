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
    Http {
        status: u16,
        message: String,
        details: Option<Box<ApiErrorDetails>>,
    },
    Parse(String),
}

#[derive(Debug)]
pub struct ApiErrorDetails {
    message: Option<String>,
    errors: Vec<ApiFieldError>,
}

#[derive(Debug)]
struct ApiFieldError {
    path: Option<String>,
    message: Option<String>,
}

impl ApiErrorDetails {
    fn parse(message: &str) -> Option<Self> {
        let body: serde_json::Value = serde_json::from_str(message).ok()?;
        let errors = body["errors"]
            .as_array()
            .into_iter()
            .flatten()
            .map(|error| ApiFieldError {
                path: error["path"].as_str().map(str::to_owned),
                message: error["message"].as_str().map(str::to_owned),
            })
            .collect();
        Some(Self {
            message: body["message"].as_str().map(str::to_owned),
            errors,
        })
    }
}

impl ApiError {
    pub fn http(status: u16, message: impl Into<String>) -> Self {
        let message = message.into();
        let details = ApiErrorDetails::parse(&message).map(Box::new);
        Self::Http {
            status,
            message,
            details,
        }
    }

    pub fn refusal_message(&self, fallback: &str) -> String {
        let Self::Http {
            details: Some(details),
            ..
        } = self
        else {
            return fallback.to_owned();
        };
        details.message.as_ref().map_or_else(
            || fallback.to_owned(),
            |detail| format!("{fallback}: {detail}"),
        )
    }

    pub fn validation_message(&self) -> Option<String> {
        let Self::Http {
            status: 400,
            details: Some(details),
            ..
        } = self
        else {
            return None;
        };
        let detail = details.errors.first().and_then(|error| {
            let message = error.message.as_ref()?;
            Some(
                match error.path.as_deref().filter(|path| !path.is_empty()) {
                    Some(path) => format!("{path}: {message}"),
                    None => message.to_owned(),
                },
            )
        });
        detail.or_else(|| details.message.clone())
    }

    pub fn detail_message(&self) -> Option<String> {
        let Self::Http {
            status: 400 | 403 | 404 | 409,
            details: Some(details),
            ..
        } = self
        else {
            return None;
        };
        details
            .errors
            .first()
            .and_then(|error| error.message.as_ref())
            .or(details.message.as_ref())
            .cloned()
    }

    pub fn field_errors(&self) -> Vec<(String, String)> {
        let Self::Http {
            status: 400,
            details: Some(details),
            ..
        } = self
        else {
            return Vec::new();
        };
        details
            .errors
            .iter()
            .filter_map(|error| Some((error.path.clone()?, error.message.clone()?)))
            .collect()
    }

    pub fn has_field_error(&self, path: &str) -> bool {
        let Self::Http {
            details: Some(details),
            ..
        } = self
        else {
            return false;
        };
        details
            .errors
            .iter()
            .any(|error| error.path.as_deref() == Some(path))
    }
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

    pub fn anonymous(http_client: &reqwest::Client, config: &AdminConfig) -> Self {
        let generated = GeneratedClient::new_with_client(
            &config.api_endpoint,
            http_client.clone(),
            build_proxy_client_ip_headers(config),
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
            headers.insert("x-audit-log-reason", audit_log_reason_header(reason)?);
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

    pub async fn get_bytes(&self, path: &str) -> ApiResult<(String, Vec<u8>)> {
        let response = Self::send_request(self.request(Method::GET, path, None, None)?).await?;
        let response = Self::check_response_status(response).await?;
        let content_type = response
            .headers()
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_owned();
        let body = response
            .bytes()
            .await
            .map_err(|e| ApiError::Network(e.to_string()))?;
        Ok((content_type, body.to_vec()))
    }

    pub async fn send<T, B>(
        &self,
        method: Method,
        path: &str,
        body: Option<&B>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<T>
    where
        T: DeserializeOwned,
        B: Serialize + ?Sized,
    {
        let response = self.send_json(method, path, body, audit_log_reason).await?;
        Self::parse_response(response).await
    }

    pub async fn send_void<B: Serialize + ?Sized>(
        &self,
        method: Method,
        path: &str,
        body: Option<&B>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<()> {
        let response = self.send_json(method, path, body, audit_log_reason).await?;
        Self::parse_void_response(response).await
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
        Err(ApiError::http(status, message))
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

    pub(crate) fn generated_error<E: std::fmt::Debug + Serialize>(
        &self,
        error: progenitor_client::Error<E>,
    ) -> ApiError {
        if let progenitor_client::Error::InvalidResponsePayload(_, err) = &error {
            return ApiError::Parse(format!("invalid response payload: {err}"));
        }
        let message = match &error {
            progenitor_client::Error::ErrorResponse(response) => {
                serde_json::to_string(response.as_ref()).unwrap_or_else(|_| format!("{error:?}"))
            }
            _ => format!("{error:?}"),
        };
        match error.status() {
            Some(status) => ApiError::http(status.as_u16(), message),
            None => ApiError::Network(message),
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
            Self::Http {
                status, message, ..
            } => write!(f, "HTTP {status}: {message}"),
            Self::Parse(msg) => write!(f, "parse error: {msg}"),
        }
    }
}

fn audit_log_reason_header(reason: &str) -> ApiResult<HeaderValue> {
    let mut value = HeaderValue::from_bytes(reason.as_bytes())
        .map_err(|_| ApiError::Parse("invalid audit log reason header".to_owned()))?;
    value.set_sensitive(true);
    Ok(value)
}
