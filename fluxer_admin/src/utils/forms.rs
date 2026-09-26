// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{body, extract::Request};
use serde::Deserialize;
use std::collections::BTreeMap;
use url::form_urlencoded;

const MAX_FORM_BYTES: usize = 1024 * 1024;

pub fn clean_string(value: &str) -> Option<String> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_owned())
    }
}

pub fn parse_comma_separated(value: &str) -> Vec<String> {
    comma_separated_values(value).map(str::to_owned).collect()
}

fn comma_separated_values(value: &str) -> impl Iterator<Item = &str> {
    value
        .split([',', '\n', '\r'])
        .map(str::trim)
        .filter(|value| !value.is_empty())
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct MultiValueForm {
    fields: BTreeMap<String, Vec<String>>,
}

impl MultiValueForm {
    pub async fn from_request(request: Request) -> Option<Self> {
        let bytes = body::to_bytes(request.into_body(), MAX_FORM_BYTES)
            .await
            .ok()?;
        Some(Self::parse(bytes.as_ref()))
    }

    pub fn parse(bytes: &[u8]) -> Self {
        let mut fields: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (key, value) in form_urlencoded::parse(bytes) {
            fields
                .entry(key.into_owned())
                .or_default()
                .push(value.into_owned());
        }
        Self { fields }
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.fields.contains_key(key)
    }

    pub fn first(&self, key: &str) -> Option<&str> {
        self.fields
            .get(key)
            .and_then(|values| values.first())
            .map(String::as_str)
    }

    pub fn clean(&self, key: &str) -> Option<String> {
        self.first(key).and_then(clean_string)
    }

    pub fn parse_value<T: std::str::FromStr>(&self, key: &str) -> Result<Option<T>, T::Err> {
        self.parse_value_any(&[key])
    }

    pub fn parse_value_any<T: std::str::FromStr>(
        &self,
        keys: &[&str],
    ) -> Result<Option<T>, T::Err> {
        keys.iter()
            .find_map(|key| self.first(key))
            .map(str::parse)
            .transpose()
    }

    pub fn parse_u32(&self, key: &str) -> Option<u32> {
        self.first(key).and_then(|value| value.parse().ok())
    }

    pub fn parse_u64(&self, key: &str) -> Option<u64> {
        self.first(key).and_then(|value| value.parse().ok())
    }

    pub fn bool_value(&self, key: &str) -> bool {
        self.fields.get(key).is_some_and(|values| {
            values
                .iter()
                .any(|value| matches!(value.as_str(), "1" | "true" | "on"))
        })
    }

    pub fn list_values(&self, key: &str) -> Vec<String> {
        self.fields
            .get(key)
            .into_iter()
            .flat_map(|values| values.iter())
            .flat_map(|value| comma_separated_values(value))
            .map(str::to_owned)
            .collect()
    }

    pub fn list_values_any(&self, keys: &[&str]) -> Vec<String> {
        keys.iter().flat_map(|key| self.list_values(key)).collect()
    }

    pub fn parse_list_values<T: std::str::FromStr>(&self, keys: &[&str]) -> Result<Vec<T>, T::Err> {
        self.list_values_any(keys)
            .iter()
            .map(|value| value.parse())
            .collect()
    }
}

pub fn parse_page(value: Option<&str>) -> u32 {
    value
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1)
        .max(1)
}

pub fn parse_per_page(value: Option<&str>, default: u32) -> u32 {
    value
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(default)
        .clamp(1, 100)
}

pub fn sanitize_redirect(value: &str) -> String {
    let trimmed = value.trim();
    if trimmed.is_empty() || !trimmed.starts_with('/') || trimmed.starts_with("//") {
        return "/".to_owned();
    }
    trimmed.to_owned()
}

#[derive(Deserialize)]
pub struct HtmxFormData {
    #[serde(default)]
    pub _csrf: String,
    #[serde(flatten)]
    pub fields: std::collections::HashMap<String, String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clean_string_trims_whitespace() {
        assert_eq!(clean_string("  hello  "), Some("hello".to_owned()));
    }

    #[test]
    fn clean_string_returns_none_for_empty() {
        assert_eq!(clean_string(""), None);
        assert_eq!(clean_string("   "), None);
        assert_eq!(clean_string("\t\n"), None);
    }

    #[test]
    fn clean_string_preserves_inner_spaces() {
        assert_eq!(
            clean_string("  hello world  "),
            Some("hello world".to_owned())
        );
    }

    #[test]
    fn parse_comma_separated_splits() {
        assert_eq!(parse_comma_separated("a, b, c"), vec!["a", "b", "c"]);
        assert_eq!(parse_comma_separated("a\nb\r\nc"), vec!["a", "b", "c"]);
    }

    #[test]
    fn parse_comma_separated_filters_empty() {
        assert_eq!(parse_comma_separated("a,,b, ,c"), vec!["a", "b", "c"]);
        assert!(parse_comma_separated("").is_empty());
        assert!(parse_comma_separated(", , ,").is_empty());
    }

    #[test]
    fn multi_value_form_preserves_repeated_fields() {
        let form = MultiValueForm::parse(b"fields%5B%5D=avatar&fields%5B%5D=banner&name=Test");

        assert_eq!(
            form.list_values("fields[]"),
            vec!["avatar".to_owned(), "banner".to_owned()]
        );
        assert_eq!(form.clean("name"), Some("Test".to_owned()));
    }

    #[test]
    fn multi_value_form_list_values_accepts_comma_and_repeated_values() {
        let form = MultiValueForm::parse(b"acls=user.read,user.write&acls=guild.read");

        assert_eq!(
            form.list_values("acls"),
            vec![
                "user.read".to_owned(),
                "user.write".to_owned(),
                "guild.read".to_owned()
            ]
        );
    }

    #[test]
    fn multi_value_form_bool_value_accepts_html_checkbox_values() {
        assert!(MultiValueForm::parse(b"enabled=on").bool_value("enabled"));
        assert!(MultiValueForm::parse(b"enabled=true").bool_value("enabled"));
        assert!(!MultiValueForm::parse(b"enabled=false").bool_value("enabled"));
    }

    #[test]
    fn parse_page_defaults_to_one() {
        assert_eq!(parse_page(None), 1);
        assert_eq!(parse_page(Some("")), 1);
        assert_eq!(parse_page(Some("abc")), 1);
    }

    #[test]
    fn parse_page_clamps_to_minimum_one() {
        assert_eq!(parse_page(Some("0")), 1);
    }

    #[test]
    fn parse_page_valid_input() {
        assert_eq!(parse_page(Some("5")), 5);
        assert_eq!(parse_page(Some("100")), 100);
    }

    #[test]
    fn sanitize_redirect_prevents_open_redirect() {
        assert_eq!(sanitize_redirect(""), "/");
        assert_eq!(sanitize_redirect("https://evil.com"), "/");
        assert_eq!(sanitize_redirect("//evil.com"), "/");
        assert_eq!(sanitize_redirect("javascript:alert(1)"), "/");
    }

    #[test]
    fn sanitize_redirect_allows_valid_paths() {
        assert_eq!(sanitize_redirect("/users"), "/users");
        assert_eq!(sanitize_redirect("/guilds/123"), "/guilds/123");
        assert_eq!(sanitize_redirect("  /trimmed  "), "/trimmed");
    }

    #[test]
    fn parse_per_page_clamps_range() {
        assert_eq!(parse_per_page(None, 25), 25);
        assert_eq!(parse_per_page(Some("0"), 25), 1);
        assert_eq!(parse_per_page(Some("200"), 25), 100);
        assert_eq!(parse_per_page(Some("50"), 25), 50);
    }
}
