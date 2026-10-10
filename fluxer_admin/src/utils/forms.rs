// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{body, extract::Request};
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

    pub fn has_key_starting_with(&self, prefix: &str) -> bool {
        self.fields.keys().any(|key| key.starts_with(prefix))
    }

    pub fn values(&self, key: &str) -> &[String] {
        self.fields.get(key).map(Vec::as_slice).unwrap_or_default()
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

    pub fn opt_out_value(&self, key: &str) -> bool {
        !self.contains_key(&format!("{key}_present")) || self.bool_value(key)
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
