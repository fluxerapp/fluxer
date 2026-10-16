// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fs;
use std::path::PathBuf;

use anyhow::Result;

use crate::config::{
    DEFAULT_OPENROUTER_BASE_URL, DEFAULT_OPENROUTER_MODEL, EmptyFallback, EnvOverlay, env_value,
    trim_trailing_slash,
};
use crate::openrouter::is_openrouter_available;
use crate::runner::translate_main;

const ENV_KEYS: &[&str] = &[
    "FLUXER_AUTO_I18N",
    "FLUXER_AUTO_I18N_LOCALE_CONCURRENCY",
    "FLUXER_AUTO_I18N_PROGRESS_INTERVAL",
    "FLUXER_AUTO_I18N_REQUEST_TIMEOUT",
    "FLUXER_AUTO_I18N_STRING_CONCURRENCY",
    "I18N_LLM_MODEL",
    "OPENROUTER_API_KEY",
    "OPENROUTER_APP_TITLE",
    "OPENROUTER_BASE_URL",
    "OPENROUTER_FALLBACK_MODELS",
    "OPENROUTER_HTTP_REFERER",
    "OPENROUTER_MODEL",
    "OPENROUTER_PROVIDER_SORT",
];

pub fn auto_main(args: &[String]) -> Result<u8> {
    let env_overrides = load_env_from_files(ENV_KEYS);
    let fluxer_auto_i18n = env_value("FLUXER_AUTO_I18N", &env_overrides, "");
    let openrouter_base_url = trim_trailing_slash(&env_value(
        "OPENROUTER_BASE_URL",
        &env_overrides,
        DEFAULT_OPENROUTER_BASE_URL,
    ));
    let openrouter_api_key = env_value("OPENROUTER_API_KEY", &env_overrides, "");
    let i18n_llm_model = env_value("I18N_LLM_MODEL", &env_overrides, "")
        .if_empty(|| env_value("OPENROUTER_MODEL", &env_overrides, DEFAULT_OPENROUTER_MODEL));
    let bypass_run_gate = args.iter().any(|arg| {
        matches!(arg.as_str(), "--help" | "-h" | "--dry-run") || arg.starts_with("--dry-run=")
    });
    let explicitly_disabled = matches!(
        fluxer_auto_i18n.to_lowercase().as_str(),
        "0" | "false" | "no" | "off"
    );
    if explicitly_disabled && !bypass_run_gate {
        eprintln!("i18n:auto skipped: FLUXER_AUTO_I18N=0 disables automatic translations.");
        return Ok(0);
    }
    let openrouter_is_available =
        is_openrouter_available(&openrouter_base_url, &openrouter_api_key);
    let should_run = fluxer_auto_i18n == "1" || (!explicitly_disabled && openrouter_is_available);
    if !should_run && !bypass_run_gate {
        eprintln!(
            "i18n:auto skipped: OpenRouter is unavailable. Set OPENROUTER_API_KEY, or set FLUXER_AUTO_I18N=1 to attempt translations with {} at {}.",
            if i18n_llm_model.is_empty() {
                DEFAULT_OPENROUTER_MODEL
            } else {
                &i18n_llm_model
            },
            openrouter_base_url
        );
        return Ok(0);
    }
    if should_run && fluxer_auto_i18n != "1" && openrouter_is_available {
        println!(
            "i18n:auto detected OpenRouter availability at {openrouter_base_url}; running without FLUXER_AUTO_I18N=1."
        );
    }

    translate_main(args, &env_overrides)
}

pub fn load_env_from_files(keys: &[&str]) -> EnvOverlay {
    let target_keys = keys
        .iter()
        .copied()
        .collect::<std::collections::HashSet<_>>();
    let mut env = EnvOverlay::new();
    let Some(home) = std::env::var_os("HOME").map(PathBuf::from) else {
        return env;
    };
    for candidate in [
        ".bash_profile",
        ".bashrc",
        ".profile",
        ".zprofile",
        ".zshrc",
    ] {
        let file_path = home.join(candidate);
        let Ok(content) = fs::read_to_string(file_path) else {
            continue;
        };
        for line in content.lines() {
            let Some((key, value)) = parse_export_line(line) else {
                continue;
            };
            if target_keys.contains(key.as_str()) {
                env.entry(key).or_insert(value);
            }
        }
    }
    env
}

pub fn parse_export_line(line: &str) -> Option<(String, String)> {
    let trimmed = line.trim();
    let key_value = trimmed.strip_prefix("export ")?;
    let (key, value) = key_value.split_once('=')?;
    let mut chars = key.chars();
    let first = chars.next()?;
    if !(first.is_alphabetic() || first == '_') {
        return None;
    }
    if !chars.all(|character| character.is_alphanumeric() || character == '_') {
        return None;
    }
    Some((key.to_string(), strip_quotes(value)))
}

pub fn strip_quotes(value: &str) -> String {
    let trimmed = value.trim();
    let first = trimmed.chars().next();
    let last = trimmed.chars().next_back();
    if trimmed.len() >= 2 && first == last && matches!(first, Some('\'' | '"')) {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}
