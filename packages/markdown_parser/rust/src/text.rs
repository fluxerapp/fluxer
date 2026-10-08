// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::Node;
use crate::constants::{MAX_LINE_LENGTH, MAX_LINK_URL_LENGTH};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Line {
    pub text: String,
    pub offset: usize,
}

pub fn split_lines(input: &str) -> Vec<Line> {
    if input.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut start = 0;
    for segment in input.split_inclusive('\n') {
        if out.len() >= crate::constants::MAX_LINES {
            break;
        }
        let text = segment.strip_suffix('\n').unwrap_or(segment);
        out.push(Line {
            text: bounded_line_text(text).to_owned(),
            offset: start,
        });
        start += segment.len();
    }
    if start < input.len() && out.len() < crate::constants::MAX_LINES {
        out.push(Line {
            text: bounded_line_text(&input[start..]).to_owned(),
            offset: start,
        });
    }
    if out.len() == 1 && out[0].text.is_empty() {
        out.clear();
    }
    out
}

pub fn trim_start(value: &str) -> &str {
    value.trim_start_matches([' ', '\t', '\r', '\n'])
}

pub fn trim_right(value: &str) -> &str {
    value.trim_end_matches([' ', '\t', '\r', '\n'])
}

pub fn trim(value: &str) -> &str {
    value.trim_matches([' ', '\t', '\r', '\n'])
}

pub fn bounded_utf8_prefix_length(value: &str, max_len: usize) -> usize {
    let mut end = value.len().min(max_len);
    while end > 0 && end < value.len() && !value.is_char_boundary(end) {
        end -= 1;
    }
    end
}

pub fn bounded_line_text(value: &str) -> &str {
    &value[..bounded_utf8_prefix_length(value, MAX_LINE_LENGTH)]
}

pub fn concat(left: &str, right: &str) -> String {
    if left.is_empty() {
        return right.to_owned();
    }
    if right.is_empty() {
        return left.to_owned();
    }
    let mut out = String::with_capacity(left.len() + right.len());
    out.push_str(left);
    out.push_str(right);
    out
}

pub fn concat3(a: &str, b: &str, c: &str) -> String {
    let mut out = String::with_capacity(a.len() + b.len() + c.len());
    out.push_str(a);
    out.push_str(b);
    out.push_str(c);
    out
}

pub fn append_text(nodes: &mut Vec<Node>, content: impl Into<String>) {
    let content = content.into();
    if content.is_empty() {
        return;
    }
    if let Some(Node::Text { content: previous }) = nodes.last_mut() {
        previous.push_str(&content);
        return;
    }
    nodes.push(Node::Text { content });
}

pub fn line_as_text(lines: &[Line], current: usize) -> String {
    if current + 1 == lines.len() {
        lines[current].text.clone()
    } else {
        concat(&lines[current].text, "\n")
    }
}

pub fn lines_to_text(lines: &[Line]) -> String {
    let mut out = String::new();
    for (index, line) in lines.iter().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        out.push_str(&line.text);
    }
    out
}

pub fn trim_line_window(lines: &mut [Line]) -> Vec<Line> {
    let mut start = 0;
    let mut end = lines.len();
    while start < end && trim(&lines[start].text).is_empty() {
        start += 1;
    }
    while end > start && trim(&lines[end - 1].text).is_empty() {
        end -= 1;
    }
    if start >= end {
        return Vec::new();
    }
    let mut out = lines[start..end].to_vec();
    let first_trimmed_len = trim_start(&out[0].text).len();
    let leading = out[0].text.len() - first_trimmed_len;
    out[0].offset += leading;
    out[0].text = trim_start(&out[0].text).to_owned();
    let last = out.len() - 1;
    out[last].text = trim_right(&out[last].text).to_owned();
    out
}

#[inline]
pub fn starts_with(value: &str, prefix: &str) -> bool {
    value.as_bytes().starts_with(prefix.as_bytes())
}

#[inline]
pub fn ends_with(value: &[u8], suffix: &[u8]) -> bool {
    value.ends_with(suffix)
}

#[inline]
pub fn is_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

#[inline]
pub fn is_alpha_numeric(byte: u8) -> bool {
    byte.is_ascii_alphanumeric()
}

pub fn is_digit_only(value: &str) -> bool {
    !value.is_empty() && value.bytes().all(is_digit)
}

pub fn is_whitespace(byte: u8) -> bool {
    matches!(byte, 0 | b'\t' | b'\n' | b'\r' | b' ')
}

pub fn has_visible_content(value: &str) -> bool {
    value.chars().any(|char| {
        !char.is_whitespace()
            && !matches!(
                char,
                '\0'
                    | '\u{00ad}'
                    | '\u{034f}'
                    | '\u{061c}'
                    | '\u{115f}'
                    | '\u{1160}'
                    | '\u{17b4}'
                    | '\u{17b5}'
                    | '\u{180e}'
                    | '\u{200b}'..='\u{200f}'
                    | '\u{202a}'..='\u{202e}'
                    | '\u{2060}'..='\u{2069}'
                    | '\u{2800}'
                    | '\u{3164}'
                    | '\u{fe00}'..='\u{fe0f}'
                    | '\u{feff}'
                    | '\u{ffa0}'
                    | '\u{e0100}'..='\u{e01ef}'
            )
    })
}

pub fn is_punctuation(byte: u8) -> bool {
    (33..=47).contains(&byte)
        || (58..=64).contains(&byte)
        || (91..=96).contains(&byte)
        || (123..=126).contains(&byte)
}

pub fn is_escapable_character(byte: u8) -> bool {
    matches!(
        byte,
        b'[' | b']'
            | b'('
            | b')'
            | b'\\'
            | b'*'
            | b'_'
            | b'~'
            | b'`'
            | b'@'
            | b'#'
            | b'-'
            | b'|'
            | b':'
            | b'<'
            | b'>'
    )
}

pub fn byte_at(value: &str, index: usize) -> u8 {
    value.as_bytes().get(index).copied().unwrap_or(0)
}

pub fn advance_one(value: &str, index: usize) -> usize {
    if index >= value.len() {
        return 0;
    }
    if value.is_char_boundary(index) {
        value[index..].chars().next().map_or(1, char::len_utf8)
    } else {
        1
    }
}

pub fn find_from(value: &str, start: usize, needle: u8) -> Option<usize> {
    value.as_bytes()[start..]
        .iter()
        .position(|byte| *byte == needle)
        .map(|pos| start + pos)
}

pub fn bounded_url_end(value: &str, end: usize, prefix_len: usize) -> usize {
    bounded_utf8_prefix_length(&value[..end], prefix_len + MAX_LINK_URL_LENGTH)
}

pub fn remove_text_presentation(text: &str) -> String {
    if !text.contains('\u{fe0f}') {
        return text.to_owned();
    }
    text.replace("™️", "™")
        .replace("©️", "©")
        .replace("®️", "®")
}

pub fn trim_start_newline_whitespace(value: &str) -> &str {
    value.trim_start_matches([' ', '\t', '\n', '\r'])
}

pub fn string_from_lossy(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}
