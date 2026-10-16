// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Result;
use regex::Regex;
use serde_json::{Map, Value, json};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalizationResult {
    pub localized: String,
    pub notes: String,
}

impl LocalizationResult {
    pub fn localized(localized: impl Into<String>) -> Self {
        Self {
            localized: localized.into(),
            notes: String::new(),
        }
    }
}

pub trait LocalizationClient: Sync {
    fn base_url(&self) -> &str;
    fn model(&self) -> &str;
    fn request_timeout_seconds(&self) -> f64;
    fn localize(
        &self,
        options: &Map<String, Value>,
        system_prompt: &str,
        user_prompt: &str,
    ) -> Result<LocalizationResult>;
}

pub fn base_options() -> Map<String, Value> {
    let mut options = Map::new();
    options.insert("temperature".to_string(), json!(0.1));
    options.insert("top_p".to_string(), json!(0.9));
    options.insert("num_ctx".to_string(), json!(4096));
    options.insert("num_predict".to_string(), json!(800));
    options
}

pub fn clean_translation_response(content: &str) -> String {
    let trimmed = trim_special_response_tokens(content.trim());
    let fenced_re = Regex::new(r"(?is)^```(?:[a-z]+)?\s*([\s\S]*?)\s*```$")
        .expect("valid fenced response regex");
    let mut raw = fenced_re
        .captures(trimmed)
        .and_then(|captures| captures.get(1))
        .map(|matched| matched.as_str().trim().to_string())
        .unwrap_or_else(|| trimmed.to_string());
    let label_re = Regex::new(r"(?is)^(?:localized(?: string)?|translation|answer):\s*([\s\S]+)$")
        .expect("valid label response regex");
    if let Some(captures) = label_re.captures(&raw)
        && let Some(value) = captures.get(1)
    {
        raw = value.as_str().trim().to_string();
    }
    if raw.starts_with('{')
        && raw.ends_with('}')
        && let Ok(Value::Object(object)) = serde_json::from_str::<Value>(&raw)
        && let Some(localized) = object.get("localized").and_then(Value::as_str)
    {
        return localized.to_string();
    }
    if raw.len() >= 2
        && raw.starts_with('"')
        && raw.ends_with('"')
        && let Ok(Value::String(value)) = serde_json::from_str::<Value>(&raw)
    {
        return value;
    }
    raw
}

fn trim_special_response_tokens(content: &str) -> &str {
    let mut trimmed = content.trim();
    loop {
        let next = trimmed
            .strip_suffix("<|im_end|>")
            .or_else(|| trimmed.strip_suffix("<end_of_turn>"))
            .or_else(|| trimmed.strip_suffix("</s>"));
        let Some(next) = next else {
            return trimmed.trim();
        };
        trimmed = next.trim();
    }
}
