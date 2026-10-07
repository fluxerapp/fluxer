// SPDX-License-Identifier: AGPL-3.0-or-later

use std::cmp::Reverse;

use anyhow::{Result, bail};
use regex::Regex;

use crate::po::Entry;

const EN_GB_PROTECTED_REGION_TERMS: &[&str] = &[
    "United States of America",
    "United States",
    "U.S.",
    "USA",
    "US",
];

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IcuControl {
    pub argument: String,
    pub kind: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenAlias {
    pub token: String,
    pub alias: String,
}

pub fn should_keep_unchanged(source: &str, locale: &str) -> bool {
    if locale == "en-US" {
        return false;
    }
    if source.trim().is_empty() {
        return true;
    }
    if !has_any_letter(source) {
        return true;
    }
    if is_preserved_token_composition(source) {
        return true;
    }
    is_preserved_artifact(source)
}

pub fn has_any_letter(source: &str) -> bool {
    source.chars().any(char::is_alphabetic)
}

pub fn is_preserved_artifact(source: &str) -> bool {
    let trimmed = source.trim();
    if [
        r"(?i)^https?://\S+$",
        r"(?i)^(?:mailto:|tel:|file:|app://|fluxer://)\S+$",
        r"^[@#][\w-]+$",
        r"^\{[^{}]+\}$",
        r"^<[^>]+>$",
        r"^%[@sd]$",
        r"^\$\d+$",
        r#"^[\s{}#,._:/+()\[\]'"-]+$"#,
        r"^[A-Z][A-Z0-9_./:+-]+$",
    ]
    .iter()
    .any(|pattern| {
        Regex::new(pattern)
            .expect("valid artifact regex")
            .is_match(trimmed)
    }) {
        return true;
    }
    Regex::new(r"^[\w./:+-]+$")
        .expect("valid word artifact regex")
        .is_match(trimmed)
        && Regex::new(r"[\d_./:+-]")
            .expect("valid word artifact marker regex")
            .is_match(trimmed)
}

pub fn is_preserved_token_composition(source: &str) -> bool {
    let entry = Entry {
        msgid: source.to_string(),
        ..Entry::default()
    };
    let mut tokens = extract_preserved_tokens(&entry);
    if tokens.is_empty() {
        return false;
    }
    tokens.sort_by_key(|token| Reverse(token.len()));
    let mut remainder = source.to_string();
    for token in tokens {
        remainder = Regex::new(&regex::escape(&token))
            .expect("valid preserved token cleanup regex")
            .replace_all(&remainder, "")
            .into_owned();
    }
    remainder
        .chars()
        .all(|character| character.is_whitespace() || is_preserved_token_separator(character))
}

fn is_preserved_token_separator(character: char) -> bool {
    matches!(
        character,
        ':' | ','
            | '.'
            | '/'
            | '\\'
            | '-'
            | '–'
            | '—'
            | '('
            | ')'
            | '['
            | ']'
            | '{'
            | '}'
            | '|'
            | '·'
            | '•'
            | '#'
            | '+'
            | '\''
            | '"'
    )
}

pub fn validate_localization(entry: &Entry, localized: &str) -> Result<()> {
    if !entry.msgid.trim().is_empty() && localized.trim().is_empty() {
        bail!("Localized string is empty");
    }
    let source_tokens = extract_preserved_tokens(entry);
    let localized_entry = Entry {
        msgid: localized.to_string(),
        comments: entry.comments.clone(),
        ..Entry::default()
    };
    let localized_tokens = extract_preserved_tokens(&localized_entry);
    for token in source_tokens {
        if !localized_tokens.contains(&token) {
            bail!("Localized string did not preserve token {token}");
        }
    }
    for control in extract_icu_controls(&entry.msgid) {
        let pattern = Regex::new(&format!(
            r"\{{\s*{}\s*,\s*{}\s*,",
            regex::escape(&control.argument),
            regex::escape(&control.kind)
        ))
        .expect("valid ICU validation regex");
        if !pattern.is_match(localized) {
            bail!(
                "Localized string did not preserve ICU {} argument {}",
                control.kind,
                control.argument
            );
        }
    }
    Ok(())
}

pub fn validate_locale_specific_localization(
    entry: &Entry,
    locale: &str,
    localized: &str,
) -> Result<()> {
    if locale != "en-GB" {
        return Ok(());
    }
    for term in extract_en_gb_protected_terms(&entry.msgid) {
        if !contains_exact_term(localized, term) {
            bail!("en-GB localization changed protected source term {term}");
        }
    }
    Ok(())
}

pub fn extract_en_gb_protected_terms(source: &str) -> Vec<&'static str> {
    EN_GB_PROTECTED_REGION_TERMS
        .iter()
        .copied()
        .filter(|term| contains_exact_term(source, term))
        .collect()
}

pub fn contains_exact_term(source: &str, term: &str) -> bool {
    source.match_indices(term).any(|(start, _match)| {
        let before_ok = source[..start]
            .chars()
            .next_back()
            .is_none_or(|before| !is_word_char(before));
        let end = start + term.len();
        let after_ok = source[end..]
            .chars()
            .next()
            .is_none_or(|after| !is_word_char(after));
        before_ok && after_ok
    })
}

fn is_word_char(value: char) -> bool {
    value == '_' || value.is_alphanumeric()
}

pub fn normalize_localized_capitalization(entry: &Entry, locale: &str, localized: &str) -> String {
    if locale.starts_with("en") || !entry.msgid.starts_with('(') || !localized.starts_with('(') {
        return localized.to_string();
    }
    let mut chars = localized.chars();
    let Some(first) = chars.next() else {
        return localized.to_string();
    };
    let Some(second) = chars.next() else {
        return localized.to_string();
    };
    let second_string = second.to_string();
    if second_string != second.to_uppercase().to_string() {
        return localized.to_string();
    }
    let rest = chars.as_str();
    format!("{first}{}{rest}", second.to_lowercase())
}

pub fn extract_preserved_tokens(entry: &Entry) -> Vec<String> {
    let mut tokens = Vec::new();
    for token in extract_top_level_brace_tokens(&entry.msgid) {
        push_unique(&mut tokens, token);
    }
    let token_re = Regex::new(r"%[@sd]|\$\d+|<[^>]+>|https?://\S+|:[a-zA-Z0-9_+-]+:")
        .expect("valid preserved token regex");
    for matched in token_re.find_iter(&entry.msgid) {
        push_unique(&mut tokens, matched.as_str().to_string());
    }
    tokens
}

pub fn extract_top_level_brace_tokens(source: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut depth = 0usize;
    let mut start: Option<usize> = None;
    let double_brace_re = Regex::new(r"^\{\{[^{}]+\}\}$").expect("valid double brace regex");
    let brace_re = Regex::new(r"^\{[^{},]+\}$").expect("valid brace regex");
    for (index, character) in source.char_indices() {
        if character == '{' {
            if depth == 0 {
                start = Some(index);
            }
            depth += 1;
        } else if character == '}' && depth > 0 {
            depth -= 1;
            if depth == 0 {
                if let Some(start_index) = start {
                    let token = &source[start_index..index + character.len_utf8()];
                    if double_brace_re.is_match(token) || brace_re.is_match(token) {
                        tokens.push(token.to_string());
                    }
                }
                start = None;
            }
        }
    }
    tokens
}

pub fn has_icu_control(source: &str) -> bool {
    Regex::new(r"\{[^{}]+,\s*(?:plural|select|selectordinal)\s*,")
        .expect("valid ICU detection regex")
        .is_match(source)
}

pub fn extract_icu_controls(source: &str) -> Vec<IcuControl> {
    Regex::new(r"\{\s*([^{}\s,]+)\s*,\s*(plural|select|selectordinal)\s*,")
        .expect("valid ICU control regex")
        .captures_iter(source)
        .map(|captures| IcuControl {
            argument: captures[1].to_string(),
            kind: captures[2].to_string(),
        })
        .collect()
}

pub fn build_masked_source(entry: &Entry) -> (String, Vec<TokenAlias>) {
    let token_aliases = extract_preserved_tokens(entry)
        .into_iter()
        .enumerate()
        .map(|(index, token)| TokenAlias {
            token,
            alias: format!("{{FLUXER_TOKEN_{index}}}"),
        })
        .collect::<Vec<_>>();
    (
        mask_text_with_aliases(&entry.msgid, &token_aliases),
        token_aliases,
    )
}

pub fn mask_text_with_aliases(text: &str, token_aliases: &[TokenAlias]) -> String {
    let mut masked = text.to_string();
    let mut aliases = token_aliases.iter().collect::<Vec<_>>();
    aliases.sort_by_key(|alias| std::cmp::Reverse(alias.token.len()));
    for item in aliases {
        masked = Regex::new(&regex::escape(&item.token))
            .expect("valid token mask regex")
            .replace_all(&masked, item.alias.as_str())
            .into_owned();
    }
    masked
}

pub fn restore_masked_tokens(localized: &str, token_aliases: &[TokenAlias]) -> String {
    let mut restored = localized.to_string();
    for item in token_aliases {
        restored = restored.replace(&item.alias, &item.token);
    }
    restored
}

pub fn build_token_alias_context(token_aliases: &[TokenAlias]) -> String {
    let mut lines = vec!["Literal protected aliases used in the source string:".to_string()];
    lines.extend(
        token_aliases
            .iter()
            .map(|item| format!("{} = {}", item.alias, item.token)),
    );
    lines.extend([
		"Aliases are required substrings, not words or labels to localize.".to_string(),
		"Copy every alias exactly in the localized output, with the same spelling and braces.".to_string(),
		"Do not replace an alias with translated words, even if the original token name looks meaningful."
			.to_string(),
		"Translate only the human-readable words around aliases.".to_string(),
	]);
    lines.join("\n")
}

fn push_unique(tokens: &mut Vec<String>, token: String) {
    if !tokens.contains(&token) {
        tokens.push(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_sources_that_should_stay_unchanged() {
        assert!(should_keep_unchanged("", "de"));
        assert!(should_keep_unchanged("https://fluxer.app/docs", "de"));
        assert!(should_keep_unchanged("{productName}", "de"));
        assert!(should_keep_unchanged("{authorName} {description}", "de"));
        assert!(should_keep_unchanged(
            "{emojiName}: {reactionCountText}, {actionText}",
            "de"
        ));
        assert!(should_keep_unchanged("{start} – {end}", "de"));
        assert!(should_keep_unchanged("C++", "de"));
        assert!(!should_keep_unchanged("DSA {reportId}", "de"));
        assert!(!should_keep_unchanged("Hello", "de"));
        assert!(!should_keep_unchanged("{productName}", "en-US"));
    }

    #[test]
    fn extracts_masks_and_restores_preserved_tokens() {
        let entry = Entry::with_msgid("{productName} needs <0>{permission}</0> %@ $1 :wave:");
        assert_eq!(
            extract_preserved_tokens(&entry),
            vec![
                "{productName}",
                "{permission}",
                "<0>",
                "</0>",
                "%@",
                "$1",
                ":wave:"
            ]
        );
        let (masked, aliases) = build_masked_source(&entry);
        assert!(masked.contains("{FLUXER_TOKEN_0}"));
        assert_eq!(
            restore_masked_tokens(&masked, &aliases),
            "{productName} needs <0>{permission}</0> %@ $1 :wave:"
        );
    }

    #[test]
    fn validates_tokens_and_icu_controls() {
        let entry = Entry {
            msgid: "{0} {1, plural, one {member} other {members}}".to_string(),
            comments: vec!["#. placeholder {0}: count".to_string()],
            ..Entry::default()
        };
        validate_localization(&entry, "{0} {1, plural, one {miembro} other {miembros}}").unwrap();
        let error =
            validate_localization(&entry, "{0} {2, plural, one {x} other {x}}").unwrap_err();
        assert!(
            error
                .to_string()
                .contains("did not preserve ICU plural argument 1")
        );
        let missing_token =
            validate_localization(&Entry::with_msgid("Open {productName}"), "Open").unwrap_err();
        assert!(missing_token.to_string().contains("{productName}"));
    }

    #[test]
    fn validates_en_gb_protected_region_terms() {
        let entry = Entry::with_msgid("US color settings for the United States");
        validate_locale_specific_localization(
            &entry,
            "en-GB",
            "US colour settings for the United States",
        )
        .unwrap();
        let error = validate_locale_specific_localization(
            &entry,
            "en-GB",
            "UK colour settings for the United Kingdom",
        )
        .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("protected source term United States")
        );
    }

    #[test]
    fn normalizes_non_english_parenthesized_capitalization() {
        assert_eq!(
            normalize_localized_capitalization(
                &Entry::with_msgid("(No content)"),
                "es-ES",
                "(Sin contenido)",
            ),
            "(sin contenido)"
        );
        assert_eq!(
            normalize_localized_capitalization(
                &Entry::with_msgid("(No content)"),
                "en-GB",
                "(No content)",
            ),
            "(No content)"
        );
    }
}
