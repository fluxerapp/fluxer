// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::Path;

use anyhow::Result;

use crate::config::{GUIDANCE_EXCERPT_CHAR_LIMIT, locales_dir};
use crate::locales::display_name;

const GLOBAL_SYSTEM_PROMPT: &str = r#"You localize product UI copy for Fluxer, a modern consumer chat app.

- Preserve the source intent, product function, tone, and authorial wording.
- Translate only the source string. Context, references, and comments are metadata.
- Preserve placeholders exactly: {name}, {{count}}, %s, %@, $1, <b>...</b>, Markdown, ICU syntax, emoji, URLs, and line breaks.
- Preserve source sentence-fragment grammar, including leading ellipses and text that continues prior UI copy.
- Keep copy concise and natural for product UI, without adding information that is not present in the source.
- Keep Fluxer's product vocabulary and naming stable unless persisted locale guidance explicitly says otherwise.
- Avoid embellishing, simplifying, softening, formalising, idiomatic rewrites, or culture-specific substitutions that change the source wording.
- If the source is ambiguous, choose the most likely chat-app interpretation and keep the result close to the source.
- Output only the localized string. Do not output JSON, notes, labels, quotes, or Markdown fences."#;

const NON_ENGLISH_SYSTEM_PROMPT: &str = r#"For non-English locales:

- Use target-language grammar, vocabulary, punctuation, and regional conventions.
- Preserve the capitalisation intention of the source, adapted to target-language conventions.
- Do not use English-style Title Case unless the target language naturally uses it.
- Use sentence-style UI capitalisation where appropriate: usually only the first word is capitalised, and parenthesised status labels normally stay lowercase after "(" unless the word is a proper noun.
- Prefer familiar, friendly, lightweight wording used in popular messaging apps in the target region."#;

const EN_GB_SYSTEM_PROMPT: &str = r#"English (United Kingdom) localisation is a minimal-edit pass over the English (United States) source.

- Keep Fluxer's exact wording, sentence structure, tone, and product vocabulary unless a spelling, punctuation, date, number, measurement, or grammatical locale difference requires a change.
- Do not replace words with more British-sounding alternatives when the US wording is understandable.
- Do not translate "US" to "UK", "United States" to "United Kingdom", or change country, region, currency, organisation, or market names unless the source text explicitly asks for that meaning change.
- Do not add, remove, soften, formalise, idiomatically rewrite, or make the copy more akin to what a Brit would say.
- Keep our words."#;

pub fn build_system_prompt(locale: &str) -> String {
    [
        Some(format!(
            "You are a professional English (United States) (en-US) to {} ({locale}) translator.",
            display_name(locale)
        )),
        Some(format!(
            "Produce exactly one {} localization for Fluxer.",
            display_name(locale)
        )),
        Some(GLOBAL_SYSTEM_PROMPT.to_string()),
        build_locale_system_prompt(locale),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n\n")
}

pub fn build_locale_system_prompt(locale: &str) -> Option<String> {
    if locale == "en-GB" {
        Some(EN_GB_SYSTEM_PROMPT.to_string())
    } else if !locale.starts_with("en") {
        Some(NON_ENGLISH_SYSTEM_PROMPT.to_string())
    } else {
        None
    }
}

pub fn build_target_locale_prompt(locale: &str) -> String {
    format!(
        "Target locale: {}.\nUse the persisted locale guidance below as the source of truth for voice, tone, terminology, punctuation, and regional conventions.",
        display_name(locale)
    )
}

pub fn load_prompt_guidance(app_dir: &Path, locale: &str) -> Result<Vec<String>> {
    Ok([load_guidance_file(
        &format!("Locale guidance for {}", display_name(locale)),
        &locales_dir(app_dir)
            .join(locale)
            .join("LOCALIZATION_PROMPT.md"),
    )?]
    .into_iter()
    .flatten()
    .collect())
}

fn load_guidance_file(label: &str, file_path: &Path) -> Result<Option<String>> {
    if !file_path.exists() {
        return Ok(None);
    }
    let excerpt = compact_guidance_excerpt(
        &std::fs::read_to_string(file_path)?,
        GUIDANCE_EXCERPT_CHAR_LIMIT,
    );
    Ok((!excerpt.is_empty()).then(|| format!("{label}:\n{excerpt}")))
}

pub fn compact_guidance_excerpt(content: &str, limit: usize) -> String {
    let mut compact = content
        .replace("\r\n", "\n")
        .split('\n')
        .map(str::trim)
        .collect::<Vec<_>>()
        .join("\n");
    while compact.contains("\n\n\n") {
        compact = compact.replace("\n\n\n", "\n\n");
    }
    let compact = compact.trim().to_string();
    if compact.len() <= limit {
        return compact;
    }
    let limit = floor_char_boundary(&compact, limit);
    let truncated = &compact[..limit];
    let last_break = [
        truncated.rfind("\n\n"),
        truncated.rfind('\n'),
        truncated.rfind(". "),
    ]
    .into_iter()
    .flatten()
    .max()
    .unwrap_or(0);
    let boundary = if last_break >= limit * 3 / 4 {
        last_break
    } else {
        limit
    };
    format!("{}\n[excerpt truncated]", compact[..boundary].trim_end())
}

fn floor_char_boundary(value: &str, limit: usize) -> usize {
    let mut boundary = limit.min(value.len());
    while !value.is_char_boundary(boundary) {
        boundary -= 1;
    }
    boundary
}
