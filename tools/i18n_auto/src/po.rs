// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result, bail};
use regex::Regex;

use crate::config::{
    AUTO_I18N_COMMENT_PREFIX, AUTO_I18N_UNCHANGED_COMMENT, is_auto_i18n_unchanged_comment,
};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Entry {
    pub comments: Vec<String>,
    pub references: Vec<String>,
    pub msgctxt: Option<String>,
    pub msgid: String,
    pub msgstr: String,
    pub line_number: usize,
}

impl Entry {
    pub fn with_msgid(msgid: impl Into<String>) -> Self {
        Self {
            msgid: msgid.into(),
            ..Self::default()
        }
    }

    fn with_line_number(line_number: usize) -> Self {
        Self {
            line_number,
            ..Self::default()
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Translation {
    pub msgid: String,
    pub msgstr: String,
    pub msgctxt: Option<String>,
    pub reviewed_unchanged: bool,
    pub notes: String,
}

impl Translation {
    pub fn new(
        msgctxt: Option<String>,
        msgid: impl Into<String>,
        msgstr: impl Into<String>,
    ) -> Self {
        Self {
            msgid: msgid.into(),
            msgstr: msgstr.into(),
            msgctxt,
            reviewed_unchanged: false,
            notes: String::new(),
        }
    }
}

pub fn parse_po(content: &str) -> Result<Vec<Entry>> {
    let mut entries = Vec::new();
    let normalized = content.replace("\r\n", "\n");
    let mut current: Option<Entry> = None;
    let mut current_field: Option<Field> = None;
    let mut is_header = true;

    for (index, line) in normalized.split('\n').enumerate() {
        if line.starts_with("#. ") || line.starts_with("# ") {
            current
                .get_or_insert_with(|| Entry::with_line_number(index))
                .comments
                .push(line.to_string());
        } else if line.starts_with("#: ") {
            current
                .get_or_insert_with(|| Entry::with_line_number(index))
                .references
                .push(line.to_string());
        } else if let Some(token) = line.strip_prefix("msgctxt ") {
            let entry = current.get_or_insert_with(|| Entry::with_line_number(index));
            entry.msgctxt = Some(parse_po_token(token, index)?);
            current_field = Some(Field::Msgctxt);
        } else if let Some(token) = line.strip_prefix("msgid ") {
            let entry = current.get_or_insert_with(|| Entry::with_line_number(index));
            entry.msgid = parse_po_token(token, index)?;
            current_field = Some(Field::Msgid);
        } else if let Some(token) = line.strip_prefix("msgstr ") {
            let entry = current.get_or_insert_with(|| Entry::with_line_number(index));
            entry.msgstr = parse_po_token(token, index)?;
            current_field = Some(Field::Msgstr);
        } else if line.starts_with('"') && line.ends_with('"') {
            if let (Some(entry), Some(field)) = (current.as_mut(), current_field) {
                let value = parse_po_token(line, index)?;
                match field {
                    Field::Msgctxt => {
                        if let Some(msgctxt) = entry.msgctxt.as_mut() {
                            msgctxt.push_str(&value);
                        }
                    }
                    Field::Msgid => entry.msgid.push_str(&value),
                    Field::Msgstr => entry.msgstr.push_str(&value),
                }
            }
        } else if line.is_empty() && current.is_some() {
            let entry = current.take().expect("checked current exists");
            if is_header && entry.msgid.is_empty() {
                is_header = false;
            } else if !entry.msgid.is_empty() {
                entries.push(entry);
            }
            current_field = None;
        }
    }

    if let Some(entry) = current
        && !entry.msgid.is_empty()
    {
        entries.push(entry);
    }

    Ok(entries)
}

#[derive(Clone, Copy)]
enum Field {
    Msgctxt,
    Msgid,
    Msgstr,
}

pub fn parse_po_token(token: &str, line_number: usize) -> Result<String> {
    let value = serde_json::from_str::<serde_json::Value>(token)
        .with_context(|| format!("Failed to parse PO string on line {}", line_number + 1))?;
    match value {
        serde_json::Value::String(value) => Ok(value),
        _ => bail!("PO string on line {} is not a string", line_number + 1),
    }
}

pub fn rebuild_po_allow_replacing(content: &str, translations: &[Translation]) -> Result<String> {
    let translation_map = translations
        .iter()
        .map(|translation| {
            Ok((
                entry_key(translation.msgctxt.as_deref(), &translation.msgid)?,
                translation,
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    let normalized = content.replace("\r\n", "\n");
    let trimmed = normalized.trim_end();
    let split_re = Regex::new(r"\n{2,}").expect("valid split regex");
    let blocks = split_re.split(trimmed);
    let mut rebuilt = Vec::new();
    for block in blocks {
        rebuilt.push(rebuild_block_allow_replacing(block, &translation_map)?);
    }
    Ok(format!("{}\n", rebuilt.join("\n\n")))
}

fn rebuild_block_allow_replacing(
    block: &str,
    translation_map: &[(String, &Translation)],
) -> Result<String> {
    let lines = block
        .split('\n')
        .filter(|line| !is_auto_i18n_unchanged_comment(line))
        .map(str::to_string)
        .collect::<Vec<_>>();
    let Some(msgid_range) = field_range(&lines, "msgid") else {
        return Ok(block.to_string());
    };
    let Some(msgstr_range) = field_range(&lines, "msgstr") else {
        return Ok(block.to_string());
    };
    let msgid = read_field_value(&lines, msgid_range)?;
    let msgctxt = field_range(&lines, "msgctxt")
        .map(|range| read_field_value(&lines, range))
        .transpose()?;
    let key = entry_key(msgctxt.as_deref(), &msgid)?;
    let Some((_, translation)) = translation_map
        .iter()
        .find(|(candidate, _)| *candidate == key)
    else {
        return Ok(block.to_string());
    };

    let mut next_lines = Vec::new();
    if translation.reviewed_unchanged {
        next_lines.push(AUTO_I18N_UNCHANGED_COMMENT.to_string());
    }
    next_lines.extend_from_slice(&lines[..msgstr_range.0]);
    next_lines.push(format!("msgstr \"{}\"", escape_po(&translation.msgstr)));
    next_lines.extend_from_slice(&lines[msgstr_range.1..]);
    Ok(next_lines.join("\n"))
}

pub fn reset_po_translations(content: &str) -> Result<String> {
    let translations = parse_po(content)?
        .into_iter()
        .map(|entry| Translation::new(entry.msgctxt, entry.msgid, ""))
        .collect::<Vec<_>>();
    rebuild_po_allow_replacing(content, &translations)
}

pub fn entry_key(msgctxt: Option<&str>, msgid: &str) -> Result<String> {
    serde_json::to_string(&(msgctxt, msgid)).context("failed to serialize PO entry key")
}

#[derive(Clone, Copy)]
struct Range(usize, usize);

fn field_range(lines: &[String], field_name: &str) -> Option<Range> {
    let start = lines
        .iter()
        .position(|line| line.starts_with(&format!("{field_name} ")))?;
    let mut end = start + 1;
    while end < lines.len() && lines[end].starts_with('"') && lines[end].ends_with('"') {
        end += 1;
    }
    Some(Range(start, end))
}

fn read_field_value(lines: &[String], range: Range) -> Result<String> {
    let Range(start, end) = range;
    let mut value = parse_po_token(
        lines[start]
            .split_once(' ')
            .map(|(_field, token)| token)
            .unwrap_or_default(),
        start,
    )?;
    for (index, line) in lines.iter().enumerate().take(end).skip(start + 1) {
        value.push_str(&parse_po_token(line, index)?);
    }
    Ok(value)
}

pub fn escape_po(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}

pub fn extract_translator_comments(entry: &Entry) -> Vec<String> {
    entry
        .comments
        .iter()
        .filter_map(|line| line.strip_prefix("#. "))
        .map(str::trim)
        .filter(|comment| {
            !comment.is_empty()
                && !is_auto_i18n_comment(comment)
                && !is_placeholder_comment(comment)
        })
        .map(str::to_string)
        .collect()
}

pub fn extract_placeholder_hints(entry: &Entry) -> Vec<String> {
    entry
        .comments
        .iter()
        .filter_map(|line| line.strip_prefix("#. "))
        .map(str::trim)
        .filter(|comment| !comment.is_empty() && is_placeholder_comment(comment))
        .map(str::to_string)
        .collect()
}

pub fn is_auto_i18n_comment(comment: &str) -> bool {
    comment.to_lowercase().starts_with(AUTO_I18N_COMMENT_PREFIX)
}

pub fn is_placeholder_comment(comment: &str) -> bool {
    comment.to_lowercase().starts_with("placeholder {") && comment.contains("}:")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_po() -> &'static str {
        "msgid \"\"\nmsgstr \"\"\n\"Content-Type: text/plain\\n\"\n\n\
		 #. Greeting shown on the welcome screen.\n\
		 #. placeholder {0}: user.name\n\
		 #: src/example.tsx:1\n\
		 msgctxt \"welcome title\"\n\
		 msgid \"Hello \\\"world\\\"\"\n\
		 msgstr \"\"\n\n\
		 #. auto-i18n: reviewed unchanged\n\
		 #: src/example.tsx:2\n\
		 msgctxt \"verb\"\n\
		 msgid \"Delete\"\n\
		 msgstr \"Delete\"\n\n\
		 #: src/example.tsx:3\n\
		 msgctxt \"keyboard key\"\n\
		 msgid \"Delete\"\n\
		 msgstr \"\"\n\n\
		 #: src/example.tsx:4\n\
		 msgid \"Line one\\n\"\n\
		 \"Line two\"\n\
		 msgstr \"\"\n"
    }

    #[test]
    fn parses_context_comments_duplicates_and_multiline() {
        let entries = parse_po(sample_po()).unwrap();
        assert_eq!(entries.len(), 4);
        let welcome = entries
            .iter()
            .find(|entry| entry.msgctxt.as_deref() == Some("welcome title"))
            .unwrap();
        assert_eq!(welcome.msgid, "Hello \"world\"");
        assert_eq!(
            extract_translator_comments(welcome),
            vec!["Greeting shown on the welcome screen."]
        );
        assert_eq!(
            extract_placeholder_hints(welcome),
            vec!["placeholder {0}: user.name"]
        );
        let multiline = entries
            .iter()
            .find(|entry| entry.msgid.starts_with("Line"))
            .unwrap();
        assert_eq!(multiline.msgid, "Line one\nLine two");
    }

    #[test]
    fn rebuilds_by_context_and_manages_unchanged_markers() {
        let rebuilt = rebuild_po_allow_replacing(
            sample_po(),
            &[
                Translation::new(
                    Some("welcome title".to_string()),
                    "Hello \"world\"",
                    "Bonjour \"monde\"",
                ),
                Translation::new(Some("verb".to_string()), "Delete", "Supprimer"),
                Translation {
                    msgctxt: Some("keyboard key".to_string()),
                    msgid: "Delete".to_string(),
                    msgstr: "Delete".to_string(),
                    reviewed_unchanged: true,
                    notes: String::new(),
                },
            ],
        )
        .unwrap();
        let entries = parse_po(&rebuilt).unwrap();
        assert_eq!(
            entries
                .iter()
                .find(|entry| entry.msgctxt.as_deref() == Some("welcome title"))
                .unwrap()
                .msgstr,
            "Bonjour \"monde\""
        );
        assert_eq!(
            entries
                .iter()
                .find(|entry| entry.msgctxt.as_deref() == Some("verb"))
                .unwrap()
                .msgstr,
            "Supprimer"
        );
        let keyboard = entries
            .iter()
            .find(|entry| entry.msgctxt.as_deref() == Some("keyboard key"))
            .unwrap();
        assert_eq!(keyboard.msgstr, "Delete");
        assert!(
            keyboard
                .comments
                .contains(&AUTO_I18N_UNCHANGED_COMMENT.to_string())
        );
        assert!(
            !keyboard
                .comments
                .contains(&crate::config::AUTO_I18N_LEGACY_UNCHANGED_COMMENT.to_string())
        );
        assert!(
            !entries
                .iter()
                .find(|entry| entry.msgctxt.as_deref() == Some("verb"))
                .unwrap()
                .comments
                .contains(&AUTO_I18N_UNCHANGED_COMMENT.to_string())
        );
    }

    #[test]
    fn untouched_blocks_keep_reviewed_unchanged_markers() {
        let content = format!(
            "{}\n{AUTO_I18N_UNCHANGED_COMMENT}\n#: src/example.tsx:5\nmsgid \"Audio\"\nmsgstr \"Audio\"\n",
            sample_po()
        );
        let rebuilt = rebuild_po_allow_replacing(
            &content,
            &[Translation::new(
                Some("keyboard key".to_string()),
                "Delete",
                "Supprimer",
            )],
        )
        .unwrap();
        let entries = parse_po(&rebuilt).unwrap();
        let verb = entries
            .iter()
            .find(|entry| entry.msgctxt.as_deref() == Some("verb"))
            .unwrap();
        assert_eq!(
            verb.comments,
            vec![crate::config::AUTO_I18N_LEGACY_UNCHANGED_COMMENT.to_string()]
        );
        let audio = entries.iter().find(|entry| entry.msgid == "Audio").unwrap();
        assert_eq!(
            audio.comments,
            vec![AUTO_I18N_UNCHANGED_COMMENT.to_string()]
        );
        assert_eq!(
            entries
                .iter()
                .find(|entry| entry.msgctxt.as_deref() == Some("keyboard key"))
                .unwrap()
                .msgstr,
            "Supprimer"
        );
        assert_eq!(
            rebuild_po_allow_replacing(&content, &[]).unwrap(),
            format!("{}\n", content.trim_end())
        );
    }

    #[test]
    fn reset_clears_msgstr_and_removes_reviewed_markers() {
        let reset = reset_po_translations(sample_po()).unwrap();
        let entries = parse_po(&reset).unwrap();
        assert!(entries.iter().all(|entry| entry.msgstr.is_empty()));
        assert!(!reset.contains(AUTO_I18N_UNCHANGED_COMMENT));
        assert!(!reset.contains(crate::config::AUTO_I18N_LEGACY_UNCHANGED_COMMENT));
    }
}
