// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result, bail};
use std::collections::BTreeMap;
use std::path::Path;

pub fn read_env_file(path: &Path) -> Result<BTreeMap<String, String>> {
    let mut values = BTreeMap::new();
    if !path.exists() {
        return Ok(values);
    }
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read {}", path.display()))?;
    for (index, raw_line) in text.lines().enumerate() {
        let line_number = index + 1;
        let mut line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some(rest) = line.strip_prefix("export ") {
            line = rest.trim();
        }
        let Some((key, value)) = line.split_once('=') else {
            bail!(
                "Invalid env line in {}:{}: {}",
                path.display(),
                line_number,
                raw_line
            );
        };
        let key = key.trim();
        if key.is_empty() {
            bail!("Missing env key in {}:{line_number}", path.display());
        }
        values.insert(key.to_owned(), parse_env_value(value.trim()));
    }
    Ok(values)
}

pub fn merge_env_layers(layers: &[BTreeMap<String, String>]) -> BTreeMap<String, String> {
    let mut merged = BTreeMap::new();
    for layer in layers {
        merged.extend(layer.clone());
    }
    merged
}

pub fn merge_default_env_with_current(
    development_path: &Path,
    local_path: &Path,
    root_local_path: &Path,
    current: BTreeMap<String, String>,
) -> Result<BTreeMap<String, String>> {
    merge_default_env_with_current_and_baseline(
        development_path,
        local_path,
        root_local_path,
        current,
        read_container_initial_env().unwrap_or_default(),
    )
}

fn merge_default_env_with_current_and_baseline(
    development_path: &Path,
    local_path: &Path,
    root_local_path: &Path,
    current: BTreeMap<String, String>,
    baseline: BTreeMap<String, String>,
) -> Result<BTreeMap<String, String>> {
    let development = read_env_file(development_path)?;
    let local = read_env_file(local_path)?;
    let root_local = read_env_file(root_local_path)?;
    let mut effective_current = current;
    for (key, value) in &development {
        let has_file_override = local.contains_key(key) || root_local.contains_key(key);
        let current_is_development_default = effective_current.get(key) == Some(value);
        let current_is_container_default = baseline
            .get(key)
            .is_some_and(|baseline_value| effective_current.get(key) == Some(baseline_value));
        if has_file_override && (current_is_development_default || current_is_container_default) {
            effective_current.remove(key);
        }
    }
    Ok(merge_env_layers(&[
        development,
        local,
        root_local,
        effective_current,
    ]))
}

fn read_container_initial_env() -> Option<BTreeMap<String, String>> {
    let bytes = std::fs::read("/proc/1/environ").ok()?;
    let mut values = BTreeMap::new();
    for entry in bytes.split(|byte| *byte == 0) {
        if entry.is_empty() {
            continue;
        }
        let text = String::from_utf8_lossy(entry);
        let Some((key, value)) = text.split_once('=') else {
            continue;
        };
        values.insert(key.to_owned(), value.to_owned());
    }
    Some(values)
}

fn parse_env_value(mut value: &str) -> String {
    if value.len() >= 2 {
        let bytes = value.as_bytes();
        if (bytes[0] == b'\'' || bytes[0] == b'"') && bytes[0] == bytes[value.len() - 1] {
            return value[1..value.len() - 1].to_owned();
        }
    }
    if let Some((before, _)) = value.split_once(" #") {
        value = before.trim_end();
    }
    value.to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_env_values_with_shell_like_comments() {
        assert_eq!(parse_env_value("'quoted'"), "quoted");
        assert_eq!(parse_env_value("\"quoted\""), "quoted");
        assert_eq!(parse_env_value("value # comment"), "value");
        assert_eq!(parse_env_value("value#not-comment"), "value#not-comment");
    }

    #[test]
    fn reads_exports_and_reports_invalid_lines() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("env");
        std::fs::write(
            &file,
            "\n# comment\nexport A=1\nB='two words'\nC=value # trailing\n",
        )
        .unwrap();
        let values = read_env_file(&file).unwrap();
        assert_eq!(values.get("A").unwrap(), "1");
        assert_eq!(values.get("B").unwrap(), "two words");
        assert_eq!(values.get("C").unwrap(), "value");

        std::fs::write(&file, "nope\n").unwrap();
        assert!(
            read_env_file(&file)
                .unwrap_err()
                .to_string()
                .contains("Invalid env line")
        );
    }

    #[test]
    fn local_env_overrides_injected_development_defaults() {
        let dir = tempfile::tempdir().unwrap();
        let development = dir.path().join("development.env");
        let local = dir.path().join("local.env");
        let root_local = dir.path().join(".env.local");
        std::fs::write(&development, "A=default\nB=default\nC=default\n").unwrap();
        std::fs::write(&local, "A=local\n").unwrap();
        std::fs::write(&root_local, "B=root\n").unwrap();
        let current = BTreeMap::from([
            ("A".to_owned(), "default".to_owned()),
            ("B".to_owned(), "custom".to_owned()),
            ("C".to_owned(), "default".to_owned()),
        ]);
        let merged = merge_default_env_with_current_and_baseline(
            &development,
            &local,
            &root_local,
            current,
            BTreeMap::new(),
        )
        .unwrap();
        assert_eq!(merged.get("A").map(String::as_str), Some("local"));
        assert_eq!(merged.get("B").map(String::as_str), Some("custom"));
        assert_eq!(merged.get("C").map(String::as_str), Some("default"));
    }

    #[test]
    fn stale_container_defaults_yield_to_file_overrides() {
        let dir = tempfile::tempdir().unwrap();
        let development = dir.path().join("development.env");
        let local = dir.path().join("local.env");
        let root_local = dir.path().join(".env.local");
        std::fs::write(
            &development,
            "A=new-default\nB=new-default\nC=new-default\n",
        )
        .unwrap();
        std::fs::write(&local, "A=local\nC=local\n").unwrap();
        std::fs::write(&root_local, "").unwrap();
        let baseline = BTreeMap::from([
            ("A".to_owned(), "old-default".to_owned()),
            ("B".to_owned(), "old-default".to_owned()),
            ("C".to_owned(), "old-default".to_owned()),
        ]);
        let current = BTreeMap::from([
            ("A".to_owned(), "old-default".to_owned()),
            ("B".to_owned(), "custom".to_owned()),
            ("C".to_owned(), "old-default".to_owned()),
        ]);
        let merged = merge_default_env_with_current_and_baseline(
            &development,
            &local,
            &root_local,
            current,
            baseline,
        )
        .unwrap();
        assert_eq!(merged.get("A").map(String::as_str), Some("local"));
        assert_eq!(merged.get("B").map(String::as_str), Some("custom"));
        assert_eq!(merged.get("C").map(String::as_str), Some("local"));
    }
}
