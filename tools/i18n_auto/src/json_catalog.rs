// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context, Result, bail};
use serde_json::{Map, Value};

use crate::po::{Entry, Translation};
use crate::ts_catalog::StaticTsCatalogKind;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StaticJsonCatalogConfig {
    pub kind: StaticTsCatalogKind,
    pub source_path: PathBuf,
}

fn parse_object(content: &str, what: &str) -> Result<Map<String, Value>> {
    let value: Value =
        serde_json::from_str(content).with_context(|| format!("failed to parse {what}"))?;
    match value {
        Value::Object(map) => Ok(map),
        _ => bail!("{what} must be a JSON object"),
    }
}

fn flatten(
    kind: &StaticTsCatalogKind,
    map: &Map<String, Value>,
) -> Result<BTreeMap<String, String>> {
    let mut out = BTreeMap::new();
    for (key, value) in map {
        match kind {
            StaticTsCatalogKind::SimpleMessages => match value {
                Value::String(text) => {
                    out.insert(key.clone(), text.clone());
                }
                _ => bail!("{key} must be a string"),
            },
            StaticTsCatalogKind::EmailTemplates => match value {
                Value::Object(fields) => {
                    for (field, text) in fields {
                        match text {
                            Value::String(text) => {
                                out.insert(format!("{key}.{field}"), text.clone());
                            }
                            _ => bail!("{key}.{field} must be a string"),
                        }
                    }
                }
                _ => bail!("{key} must be an object of template fields"),
            },
        }
    }
    Ok(out)
}

fn render(kind: &StaticTsCatalogKind, values: &BTreeMap<String, String>) -> Result<String> {
    let mut root = Map::new();
    match kind {
        StaticTsCatalogKind::SimpleMessages => {
            for (key, text) in values {
                root.insert(key.clone(), Value::String(text.clone()));
            }
        }
        StaticTsCatalogKind::EmailTemplates => {
            for (context, text) in values {
                let Some((template, field)) = context.split_once('.') else {
                    bail!("{context} must name a template field as <template>.<field>");
                };
                let entry = root
                    .entry(template.to_string())
                    .or_insert_with(|| Value::Object(Map::new()));
                let Value::Object(fields) = entry else {
                    bail!("{template} must be an object of template fields");
                };
                fields.insert(field.to_string(), Value::String(text.clone()));
            }
        }
    }
    let mut buffer = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"\t");
    let mut serializer = serde_json::Serializer::with_formatter(&mut buffer, formatter);
    serde::Serialize::serialize(&Value::Object(root), &mut serializer)
        .context("failed to serialize catalog")?;
    let mut rendered = String::from_utf8(buffer).context("catalog is not valid UTF-8")?;
    rendered.push('\n');
    Ok(rendered)
}

fn static_comments(kind: &StaticTsCatalogKind, context: &str) -> Vec<String> {
    match kind {
        StaticTsCatalogKind::SimpleMessages => vec![format!("#. Static catalog key: {context}")],
        StaticTsCatalogKind::EmailTemplates => vec![format!("#. Email template field: {context}")],
    }
}

pub fn read_static_json_entries(
    config: &StaticJsonCatalogConfig,
    source_content: &str,
    target_content: &str,
    reset: bool,
) -> Result<Vec<Entry>> {
    let source = flatten(
        &config.kind,
        &parse_object(source_content, "the source catalog")?,
    )?;
    let target = flatten(
        &config.kind,
        &parse_object(target_content, "the locale catalog")?,
    )?;
    Ok(source
        .into_iter()
        .enumerate()
        .map(|(index, (context, msgid))| {
            let msgstr = if reset {
                String::new()
            } else {
                target.get(&context).cloned().unwrap_or_default()
            };
            Entry {
                comments: static_comments(&config.kind, &context),
                references: vec![format!("#: {}", config.source_path.display())],
                msgctxt: Some(context),
                msgid,
                msgstr,
                line_number: index + 1,
            }
        })
        .collect())
}

pub fn rebuild_static_json_allow_replacing(
    config: &StaticJsonCatalogConfig,
    source_content: &str,
    target_content: &str,
    translations: &[Translation],
) -> Result<String> {
    let source = flatten(
        &config.kind,
        &parse_object(source_content, "the source catalog")?,
    )?;
    let mut target = flatten(
        &config.kind,
        &parse_object(target_content, "the locale catalog")?,
    )?;
    for translation in translations {
        let Some(context) = translation.msgctxt.as_ref() else {
            continue;
        };
        if !source.contains_key(context) {
            continue;
        }
        target.insert(context.clone(), translation.msgstr.clone());
    }
    let merged = source
        .keys()
        .map(|context| {
            let text = target
                .get(context)
                .cloned()
                .unwrap_or_else(|| source[context].clone());
            (context.clone(), text)
        })
        .collect::<BTreeMap<_, _>>();
    render(&config.kind, &merged)
}

pub fn reset_static_json_translations(
    config: &StaticJsonCatalogConfig,
    target_content: &str,
) -> Result<String> {
    let target = flatten(
        &config.kind,
        &parse_object(target_content, "the locale catalog")?,
    )?;
    let cleared = target
        .into_keys()
        .map(|context| (context, String::new()))
        .collect::<BTreeMap<_, _>>();
    render(&config.kind, &cleared)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn simple() -> StaticJsonCatalogConfig {
        StaticJsonCatalogConfig {
            kind: StaticTsCatalogKind::SimpleMessages,
            source_path: PathBuf::from("weblate/messages.json"),
        }
    }

    fn email() -> StaticJsonCatalogConfig {
        StaticJsonCatalogConfig {
            kind: StaticTsCatalogKind::EmailTemplates,
            source_path: PathBuf::from("weblate/messages.json"),
        }
    }

    #[test]
    fn reads_simple_entries_pairing_source_with_locale() {
        let entries = read_static_json_entries(
            &simple(),
            "{\"a.b\":\"Hello\",\"c.d\":\"Bye\"}",
            "{\"a.b\":\"Hallo\"}",
            false,
        )
        .unwrap();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].msgctxt.as_deref(), Some("a.b"));
        assert_eq!(entries[0].msgid, "Hello");
        assert_eq!(entries[0].msgstr, "Hallo");
        assert_eq!(entries[1].msgstr, "");
    }

    #[test]
    fn reset_blanks_every_value() {
        let entries =
            read_static_json_entries(&simple(), "{\"a\":\"Hello\"}", "{\"a\":\"Hallo\"}", true)
                .unwrap();
        assert_eq!(entries[0].msgstr, "");
    }

    #[test]
    fn writes_tab_indented_sorted_json_with_a_trailing_newline() {
        let rendered = rebuild_static_json_allow_replacing(
            &simple(),
            "{\"b\":\"Two\",\"a\":\"One\"}",
            "{\"a\":\"One\",\"b\":\"Two\"}",
            &[Translation {
                msgctxt: Some("a".to_string()),
                msgid: "One".to_string(),
                msgstr: "Eins".to_string(),
                reviewed_unchanged: false,
                notes: String::new(),
            }],
        )
        .unwrap();
        assert_eq!(rendered, "{\n\t\"a\": \"Eins\",\n\t\"b\": \"Two\"\n}\n");
    }

    #[test]
    fn keeps_email_template_fields_nested() {
        let entries = read_static_json_entries(
            &email(),
            "{\"welcome\":{\"subject\":\"Hi\",\"body\":\"Text\"}}",
            "{\"welcome\":{\"subject\":\"Hallo\",\"body\":\"\"}}",
            false,
        )
        .unwrap();
        assert_eq!(entries[0].msgctxt.as_deref(), Some("welcome.body"));
        assert_eq!(entries[1].msgctxt.as_deref(), Some("welcome.subject"));
        let rendered = rebuild_static_json_allow_replacing(
            &email(),
            "{\"welcome\":{\"subject\":\"Hi\",\"body\":\"Text\"}}",
            "{\"welcome\":{\"subject\":\"Hallo\",\"body\":\"\"}}",
            &[Translation {
                msgctxt: Some("welcome.body".to_string()),
                msgid: "Text".to_string(),
                msgstr: "Inhalt".to_string(),
                reviewed_unchanged: false,
                notes: String::new(),
            }],
        )
        .unwrap();
        assert_eq!(
            rendered,
            "{\n\t\"welcome\": {\n\t\t\"body\": \"Inhalt\",\n\t\t\"subject\": \"Hallo\"\n\t}\n}\n"
        );
    }

    #[test]
    fn ignores_translations_the_source_no_longer_defines() {
        let rendered = rebuild_static_json_allow_replacing(
            &simple(),
            "{\"a\":\"One\"}",
            "{\"a\":\"Eins\"}",
            &[Translation {
                msgctxt: Some("gone".to_string()),
                msgid: "Gone".to_string(),
                msgstr: "Weg".to_string(),
                reviewed_unchanged: false,
                notes: String::new(),
            }],
        )
        .unwrap();
        assert_eq!(rendered, "{\n\t\"a\": \"Eins\"\n}\n");
    }
}
