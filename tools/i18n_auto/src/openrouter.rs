// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::Duration;

use anyhow::{Context, Result, bail};
use serde_json::{Map, Value, json};

use crate::llm::{LocalizationClient, LocalizationResult, clean_translation_response};

pub struct OpenRouterClient {
    base_url: String,
    model: String,
    fallback_models: Vec<String>,
    provider_sort: String,
    http_referer: String,
    app_title: String,
    api_key: String,
    request_timeout: Duration,
    client: reqwest::blocking::Client,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpenRouterClientConfig {
    pub base_url: String,
    pub model: String,
    pub fallback_models: Vec<String>,
    pub provider_sort: String,
    pub http_referer: String,
    pub app_title: String,
    pub api_key: String,
    pub request_timeout_seconds: f64,
}

impl OpenRouterClient {
    pub fn new(config: OpenRouterClientConfig) -> Result<Self> {
        if config.api_key.trim().is_empty() {
            bail!("OPENROUTER_API_KEY is required for i18n:auto");
        }
        let request_timeout = Duration::from_secs_f64(config.request_timeout_seconds);
        let client = reqwest::blocking::Client::builder()
            .timeout(request_timeout)
            .build()
            .context("failed to build OpenRouter HTTP client")?;
        Ok(Self {
            base_url: config.base_url.trim_end_matches('/').to_string(),
            model: config.model,
            fallback_models: config.fallback_models,
            provider_sort: config.provider_sort,
            http_referer: config.http_referer,
            app_title: config.app_title,
            api_key: config.api_key,
            request_timeout,
            client,
        })
    }
}

impl LocalizationClient for OpenRouterClient {
    fn base_url(&self) -> &str {
        &self.base_url
    }

    fn model(&self) -> &str {
        &self.model
    }

    fn request_timeout_seconds(&self) -> f64 {
        self.request_timeout.as_secs_f64()
    }

    fn localize(
        &self,
        options: &Map<String, Value>,
        system_prompt: &str,
        user_prompt: &str,
    ) -> Result<LocalizationResult> {
        let max_tokens = options
            .get("num_predict")
            .and_then(Value::as_u64)
            .unwrap_or(800);
        let expects_json = system_prompt.contains("Return only valid compact JSON");
        let mut provider = Map::new();
        if !self.provider_sort.is_empty() {
            provider.insert("sort".to_string(), json!(self.provider_sort));
        }
        provider.insert("allow_fallbacks".to_string(), json!(true));
        if expects_json {
            provider.insert("require_parameters".to_string(), json!(true));
        }

        let mut payload = Map::new();
        payload.insert("model".to_string(), json!(self.model));
        let fallback_models = self
            .fallback_models
            .iter()
            .map(|model| model.trim())
            .filter(|model| !model.is_empty() && *model != self.model)
            .map(|model| json!(model))
            .take(3)
            .collect::<Vec<_>>();
        if !fallback_models.is_empty() {
            payload.insert("models".to_string(), Value::Array(fallback_models));
        }
        payload.insert(
            "messages".to_string(),
            json!([
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ]),
        );
        payload.insert(
            "temperature".to_string(),
            json!(
                options
                    .get("temperature")
                    .and_then(Value::as_f64)
                    .unwrap_or(0.1)
            ),
        );
        payload.insert(
            "top_p".to_string(),
            json!(options.get("top_p").and_then(Value::as_f64).unwrap_or(0.9)),
        );
        payload.insert("max_tokens".to_string(), json!(max_tokens));
        payload.insert("stream".to_string(), json!(false));
        payload.insert("provider".to_string(), Value::Object(provider));
        if expects_json {
            payload.insert(
                "response_format".to_string(),
                json!({"type": "json_object"}),
            );
        }

        let response = self
            .client
            .post(format!("{}/chat/completions", self.base_url))
            .bearer_auth(&self.api_key)
            .header("HTTP-Referer", &self.http_referer)
            .header("X-OpenRouter-Title", &self.app_title)
            .json(&Value::Object(payload))
            .send()
            .with_context(|| {
                format!(
                    "OpenRouter request failed after {}s",
                    self.request_timeout.as_secs_f64()
                )
            })?;
        let status = response.status();
        let body = response
            .text()
            .context("failed to read OpenRouter response")?;
        if !status.is_success() {
            bail!(
                "OpenRouter API error: {} - {}",
                status.as_u16(),
                truncate_error_body(&body)
            );
        }
        let data = serde_json::from_str::<Value>(&body)
            .context("failed to parse OpenRouter response JSON")?;
        let Some(content) =
            extract_chat_message_content(&data).filter(|content| !content.is_empty())
        else {
            bail!("Empty response from OpenRouter");
        };
        Ok(LocalizationResult::localized(clean_translation_response(
            &content,
        )))
    }
}

pub fn parse_openrouter_fallback_models(value: &str) -> Vec<String> {
    value
        .split(',')
        .map(str::trim)
        .filter(|model| !model.is_empty())
        .map(ToString::to_string)
        .collect()
}

pub fn is_openrouter_available(base_url: &str, api_key: &str) -> bool {
    if api_key.trim().is_empty() {
        return false;
    }
    let Ok(client) = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(4))
        .build()
    else {
        return false;
    };
    client
        .get(format!("{}/models", base_url.trim_end_matches('/')))
        .bearer_auth(api_key)
        .send()
        .map(|response| response.status().is_success())
        .unwrap_or(false)
}

fn extract_chat_message_content(data: &Value) -> Option<String> {
    let content = data
        .get("choices")
        .and_then(Value::as_array)
        .and_then(|choices| choices.first())
        .and_then(|choice| choice.get("message"))
        .and_then(|message| message.get("content"))?;
    match content {
        Value::String(value) => Some(value.to_string()),
        Value::Array(parts) => {
            let text = parts
                .iter()
                .filter_map(|part| match part {
                    Value::String(value) => Some(value.as_str()),
                    Value::Object(object) => object
                        .get("text")
                        .or_else(|| object.get("content"))
                        .and_then(Value::as_str),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join("");
            if text.is_empty() { None } else { Some(text) }
        }
        _ => None,
    }
}

fn truncate_error_body(body: &str) -> String {
    const MAX_ERROR_BODY_CHARS: usize = 2000;
    let mut truncated = body.chars().take(MAX_ERROR_BODY_CHARS).collect::<String>();
    if body.chars().count() > MAX_ERROR_BODY_CHARS {
        truncated.push_str("...");
    }
    truncated
}
