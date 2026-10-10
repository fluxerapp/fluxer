// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub const SOURCE_LOCALE: &str = "en-US";
pub const DEFAULT_OPENROUTER_BASE_URL: &str = "https://openrouter.ai/api/v1";
pub const DEFAULT_OPENROUTER_MODEL: &str = "google/gemini-2.5-flash-lite";
pub const DEFAULT_OPENROUTER_FALLBACK_MODELS: &str =
    "openai/gpt-4.1-nano,mistralai/mistral-nemo,mistralai/mistral-small-3.2-24b-instruct";
pub const DEFAULT_OPENROUTER_PROVIDER_SORT: &str = "throughput";
pub const DEFAULT_OPENROUTER_HTTP_REFERER: &str = "https://fluxer.chat";
pub const DEFAULT_OPENROUTER_APP_TITLE: &str = "Fluxer i18n auto";
pub const DEFAULT_STRING_CONCURRENCY: usize = 2;
pub const DEFAULT_LOCALE_CONCURRENCY: usize = 1;
pub const DEFAULT_REQUEST_TIMEOUT_SECONDS: f64 = 300.0;
pub const DEFAULT_PROGRESS_INTERVAL_SECONDS: f64 = 20.0;
pub const GUIDANCE_EXCERPT_CHAR_LIMIT: usize = 1600;

pub const AUTO_I18N_REVIEWED_UNCHANGED_FILE: &str = "auto-i18n-reviewed-unchanged.json";

pub type EnvOverlay = HashMap<String, String>;

pub fn default_app_dir() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .and_then(Path::parent)
        .map(|repo_root| repo_root.join("fluxer_app"))
        .unwrap_or_else(|| PathBuf::from("fluxer_app"))
}

pub fn i18n_dir(app_dir: &Path) -> PathBuf {
    app_dir.join("src").join("features").join("i18n")
}

pub fn locales_dir(app_dir: &Path) -> PathBuf {
    i18n_dir(app_dir).join("locales")
}

pub fn env_value(key: &str, env_overrides: &EnvOverlay, fallback: &str) -> String {
    std::env::var(key)
        .ok()
        .filter(|value| !value.is_empty())
        .or_else(|| env_overrides.get(key).cloned())
        .unwrap_or_else(|| fallback.to_string())
}

pub fn positive_float_env(key: &str, fallback: f64, env_overrides: &EnvOverlay) -> f64 {
    let value = env_value(key, env_overrides, "");
    if value.is_empty() {
        return fallback;
    }
    value
        .parse::<f64>()
        .ok()
        .filter(|parsed| *parsed > 0.0)
        .unwrap_or(fallback)
}

pub fn positive_int_env(key: &str, fallback: usize, env_overrides: &EnvOverlay) -> usize {
    positive_float_env(key, fallback as f64, env_overrides).max(1.0) as usize
}

pub fn trim_trailing_slash(value: &str) -> String {
    value.trim_end_matches('/').to_string()
}
