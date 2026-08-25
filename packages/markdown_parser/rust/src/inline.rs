// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{EmojiKind, Node, ParserFlags, ParserResult};
use crate::constants::{MAX_INLINE_DEPTH, MAX_LINE_LENGTH};
use crate::links;
use crate::normalize::{
    combine_adjacent_text, compact_empty_text_nodes, flatten_top_level_formatting,
    merge_adjacent_text_simple, normalize_nodes,
};
use crate::parser::{MarkdownParser, ParseError};
use crate::text::{
    advance_one, append_text, byte_at, ends_with, has_visible_content, is_alpha_numeric,
    is_escapable_character, string_from_lossy,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct FormattingEntry {
    delimiter: u8,
    is_double: bool,
}

#[derive(Clone, Debug, Default)]
struct FormattingContext {
    entries: Vec<FormattingEntry>,
}

impl FormattingContext {
    fn can_enter(&self, delimiter: u8, is_double: bool) -> bool {
        self.entries.len() < MAX_INLINE_DEPTH && !self.is_active(delimiter, is_double)
    }

    fn is_active(&self, delimiter: u8, is_double: bool) -> bool {
        self.entries
            .iter()
            .any(|entry| entry.delimiter == delimiter && entry.is_double == is_double)
    }

    fn push(&mut self, delimiter: u8, is_double: bool) {
        if self.entries.len() < MAX_INLINE_DEPTH {
            self.entries.push(FormattingEntry {
                delimiter,
                is_double,
            });
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FormattingNodeType {
    Strong,
    Emphasis,
    Underline,
    Strikethrough,
    Spoiler,
    InlineCode,
}

#[derive(Clone, Copy, Debug)]
struct FormattingMarkerInfo<'a> {
    marker: &'a str,
    node_type: FormattingNodeType,
    marker_length: usize,
}

#[derive(Clone, Copy, Debug)]
struct FormattingEnd {
    end_position: usize,
}

pub fn parse_inline(
    parser: &mut MarkdownParser,
    text: &str,
    base_offset: usize,
) -> Result<Vec<Node>, ParseError> {
    if text.is_empty() {
        return Ok(Vec::new());
    }
    let mut context = FormattingContext::default();
    let mut nodes = parse_inline_with_context(parser, text, base_offset, &mut context)?;
    if nodes.len() > 1 {
        normalize_nodes(&mut nodes, false);
        flatten_top_level_formatting(&mut nodes);
        combine_adjacent_text(&mut nodes, false);
        merge_adjacent_text_simple(&mut nodes);
        compact_empty_text_nodes(&mut nodes);
    }
    Ok(nodes)
}

fn parse_inline_with_context(
    parser: &mut MarkdownParser,
    text: &str,
    base_offset: usize,
    context: &mut FormattingContext,
) -> Result<Vec<Node>, ParseError> {
    let mut nodes = Vec::new();
    let mut accumulated = Vec::<u8>::new();
    let mut position = 0usize;
    while position < text.len() {
        let current = byte_at(text, position);
        if current == b'\\' && position + 1 < text.len() {
            let next = byte_at(text, position + 1);
            if next == b'_' && position > 0 && byte_at(text, position - 1) == 0xaf {
                accumulated.push(current);
                accumulated.push(next);
                position += 2;
                continue;
            }
            if next == b':'
                && let Some(result) = links::parse_emoji_shortcode(parser, &text[position + 1..])
                && let Node::Emoji {
                    kind: EmojiKind::Standard { raw, .. },
                } = &result.node
            {
                accumulated.extend_from_slice(raw.as_bytes());
                position += 1 + result.advance;
                continue;
            }
            if let Some(emoji) = parser
                .emoji_context()
                .standard_at(base_offset + position + 1)
                && text[position + 1..].starts_with(&emoji.raw)
            {
                accumulated.extend_from_slice(emoji.raw.as_bytes());
                position += 1 + emoji.len;
                continue;
            }
            if is_escapable_character(next)
                || is_ordered_list_marker_dot_escape(text, position)
                || is_word_dot_escape(text, position)
            {
                accumulated.push(next);
                position += 2;
                continue;
            }
        }

        let remaining = &text[position..];
        if !ends_with(&accumulated, b"<\"")
            && !ends_with(&accumulated, b"<'")
            && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
            && links::starts_with_url(remaining)
            && let Some(result) = links::parse_url_segment(remaining, parser.flags())
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if current == b'_'
            && position + 1 < text.len()
            && byte_at(text, position + 1) != b'_'
            && links::is_word_underscore(text, position)
        {
            accumulated.push(b'_');
            position += 1;
            continue;
        }

        if let Some(result) = parse_regional_indicator_flag(remaining) {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if let Some(emoji) = parser.emoji_context().standard_at(base_offset + position)
            && remaining.starts_with(&emoji.raw)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(Node::Emoji {
                kind: EmojiKind::Standard {
                    raw: emoji.raw.clone(),
                    codepoints: emoji.codepoints.clone(),
                    name: emoji.name.clone(),
                },
            });
            position += emoji.len;
            continue;
        }

        if current == b'<'
            && position + 2 < text.len()
            && let Some(result) = links::parse_custom_emoji(remaining)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if current == b'<'
            && remaining.starts_with("<t:")
            && let Some(result) = links::parse_timestamp(remaining)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if current == b':'
            && let Some(result) = links::parse_emoji_shortcode(parser, remaining)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if current == b'<' && position + 1 < text.len() {
            let next = byte_at(text, position + 1);
            if next == b'+'
                && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
                && let Some(result) = links::parse_phone_link(parser, remaining)
            {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(result.node);
                position += result.advance;
                continue;
            }
            if remaining.starts_with("<sms:")
                && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
                && let Some(result) = links::parse_sms_link(parser, remaining)
            {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(result.node);
                position += result.advance;
                continue;
            }
            if matches!(next, b'@' | b'#' | b'/' | b'i')
                && let Some(result) = links::parse_mention(remaining, parser.flags())
            {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(result.node);
                position += result.advance;
                continue;
            }
            if ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS) {
                if let Some(result) = links::parse_autolink(remaining, parser.flags()) {
                    flush_accumulated_text(&mut nodes, &mut accumulated);
                    nodes.push(result.node);
                    position += result.advance;
                    continue;
                }
                if let Some(result) = links::parse_email_link(parser, remaining) {
                    flush_accumulated_text(&mut nodes, &mut accumulated);
                    nodes.push(result.node);
                    position += result.advance;
                    continue;
                }
            }
        }

        if current == b'@' && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_EVERYONE_MENTIONS)
        {
            let is_escaped = position > 0 && byte_at(text, position - 1) == b'\\';
            if !is_escaped && remaining.starts_with("@everyone") {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(Node::Mention {
                    kind: crate::ast::MentionKind::Everyone,
                });
                position += 9;
                continue;
            }
            if !is_escaped && remaining.starts_with("@here") {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(Node::Mention {
                    kind: crate::ast::MentionKind::Here,
                });
                position += 5;
                continue;
            }
        }

        if current >= 0x80 {
            let char_len = remaining.chars().next().map_or(1, char::len_utf8);
            accumulated.extend_from_slice(&text.as_bytes()[position..position + char_len]);
            position += char_len;
            continue;
        }

        let is_double_underscore =
            current == b'_' && position + 1 < text.len() && byte_at(text, position + 1) == b'_';
        if (is_formatting_char(current) || current == b'[')
            && (is_double_underscore
                || !(current == b'_'
                    && !accumulated.is_empty()
                    && is_alpha_numeric(*accumulated.last().unwrap_or(&0))))
        {
            let previous = if position > 0 {
                byte_at(text, position - 1)
            } else {
                0
            };
            if let Some(result) = parse_special_sequence(
                parser,
                remaining,
                base_offset + position,
                context,
                previous,
            )? {
                flush_accumulated_text(&mut nodes, &mut accumulated);
                nodes.push(result.node);
                position += result.advance;
                continue;
            }
        }

        accumulated.push(current);
        position += 1;
        if accumulated.len() > MAX_LINE_LENGTH {
            flush_bounded_accumulated_text(&mut nodes, &mut accumulated, MAX_LINE_LENGTH);
            break;
        }
    }
    flush_accumulated_text(&mut nodes, &mut accumulated);
    if nodes.len() > 1 {
        normalize_nodes(&mut nodes, false);
    }
    Ok(nodes)
}

fn parse_regional_indicator_flag(text: &str) -> Option<ParserResult> {
    let mut chars = text.chars();
    let first = chars.next()?;
    let second = chars.next()?;
    let first_letter = regional_indicator_letter(first)?;
    let second_letter = regional_indicator_letter(second)?;
    let raw = format!("{first}{second}");
    Some(ParserResult {
        node: Node::Emoji {
            kind: EmojiKind::Standard {
                raw: raw.clone(),
                codepoints: format!("{:x}-{:x}", first as u32, second as u32),
                name: format!("flag_{first_letter}{second_letter}"),
            },
        },
        advance: raw.len(),
    })
}

fn regional_indicator_letter(ch: char) -> Option<char> {
    let codepoint = ch as u32;
    if !(0x1f1e6..=0x1f1ff).contains(&codepoint) {
        return None;
    }
    char::from_u32(u32::from(b'a') + codepoint - 0x1f1e6)
}

fn is_ordered_list_marker_dot_escape(text: &str, backslash_position: usize) -> bool {
    if backslash_position + 2 >= text.len()
        || byte_at(text, backslash_position + 1) != b'.'
        || byte_at(text, backslash_position + 2) != b' '
    {
        return false;
    }
    let mut digit_start = backslash_position;
    while digit_start > 0 && byte_at(text, digit_start - 1).is_ascii_digit() {
        digit_start -= 1;
    }
    if digit_start == backslash_position {
        return false;
    }
    let line_start = text[..digit_start]
        .rfind('\n')
        .map_or(0, |position| position + 1);
    if text.as_bytes()[line_start..digit_start]
        .iter()
        .any(|byte| *byte != b' ')
    {
        return false;
    }
    let indent = digit_start - line_start;
    indent == 0 || indent >= 2
}

fn is_word_dot_escape(text: &str, backslash_position: usize) -> bool {
    if backslash_position == 0 || byte_at(text, backslash_position + 1) != b'.' {
        return false;
    }
    let previous = byte_at(text, backslash_position - 1);
    previous.is_ascii_alphanumeric() || previous == b'.'
}

fn parse_special_sequence(
    parser: &mut MarkdownParser,
    text: &str,
    base_offset: usize,
    context: &mut FormattingContext,
    previous: u8,
) -> Result<Option<ParserResult>, ParseError> {
    if text.is_empty() {
        return Ok(None);
    }
    match byte_at(text, 0) {
        b'*' | b'_' | b'~' | b'|' | b'`' => {
            if let Some(result) = parse_formatting(parser, text, base_offset, context, previous)? {
                return Ok(Some(result));
            }
        }
        b'@' => {
            if ParserFlags::has(parser.flags(), ParserFlags::ALLOW_EVERYONE_MENTIONS) {
                if text.starts_with("@everyone") {
                    return Ok(Some(ParserResult {
                        node: Node::Mention {
                            kind: crate::ast::MentionKind::Everyone,
                        },
                        advance: 9,
                    }));
                }
                if text.starts_with("@here") {
                    return Ok(Some(ParserResult {
                        node: Node::Mention {
                            kind: crate::ast::MentionKind::Here,
                        },
                        advance: 5,
                    }));
                }
            }
        }
        b'[' => {
            if let Some(result) = links::parse_timestamp(text) {
                return Ok(Some(result));
            }
            if ParserFlags::has(parser.flags(), ParserFlags::ALLOW_MASKED_LINKS)
                && let Some(result) = links::parse_masked_link(parser, text, base_offset)?
            {
                return Ok(Some(result));
            }
        }
        _ => {}
    }
    if byte_at(text, 0) != b'['
        && let Some(result) = links::parse_timestamp(text)
    {
        return Ok(Some(result));
    }
    if byte_at(text, 0) != b'<'
        && byte_at(text, 0) != b'['
        && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_MASKED_LINKS)
        && let Some(result) = links::parse_masked_link(parser, text, base_offset)?
    {
        return Ok(Some(result));
    }
    Ok(None)
}

fn parse_formatting(
    parser: &mut MarkdownParser,
    text: &str,
    base_offset: usize,
    context: &mut FormattingContext,
    previous: u8,
) -> Result<Option<ParserResult>, ParseError> {
    if text.len() < 2 {
        return Ok(None);
    }
    let Some(marker) = get_formatting_marker_info(text) else {
        return Ok(None);
    };
    if marker.node_type == FormattingNodeType::Spoiler
        && !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_SPOILERS)
    {
        return Ok(None);
    }
    if !context.can_enter(byte_at(marker.marker, 0), marker.marker.len() > 1) {
        return Ok(None);
    }
    if marker.marker_length == 1 && marker.node_type == FormattingNodeType::Emphasis {
        let next = byte_at(text, marker.marker_length);
        if !links::can_open_single_emphasis(byte_at(marker.marker, 0), previous, next) {
            return Ok(None);
        }
    }
    let Some(end) = find_formatting_end(text, marker) else {
        return Ok(None);
    };
    let inner = &text[marker.marker_length..end.end_position];
    if !formatting_inner_has_visible_content(marker, inner) {
        return Ok(None);
    }
    let is_block = context.is_active(byte_at(marker.marker, 0), marker.marker.len() > 1);
    let node = create_formatting_node(
        parser,
        marker,
        inner,
        base_offset + marker.marker_length,
        is_block,
    )?;
    Ok(Some(ParserResult {
        node,
        advance: end.end_position + marker.marker_length,
    }))
}

fn formatting_inner_has_visible_content(marker: FormattingMarkerInfo<'_>, inner: &str) -> bool {
    if marker.node_type == FormattingNodeType::InlineCode {
        return has_visible_content(&unescape_inline_code(inner));
    }
    has_visible_content(inner)
}

fn get_formatting_marker_info(text: &str) -> Option<FormattingMarkerInfo<'_>> {
    if text.is_empty() || !is_formatting_char(byte_at(text, 0)) {
        return None;
    }
    let first = byte_at(text, 0);
    let second = byte_at(text, 1);
    let third = byte_at(text, 2);
    if first == b'*' && second == b'*' && third == b'*' {
        return Some(marker("***", FormattingNodeType::Emphasis, 3));
    }
    if first == b'_' && second == b'_' && third == b'_' {
        return Some(marker("___", FormattingNodeType::Emphasis, 3));
    }
    if first == b'|' && second == b'|' {
        return Some(marker("||", FormattingNodeType::Spoiler, 2));
    }
    if first == b'~' && second == b'~' {
        return Some(marker("~~", FormattingNodeType::Strikethrough, 2));
    }
    if first == b'*' && second == b'*' {
        return Some(marker("**", FormattingNodeType::Strong, 2));
    }
    if first == b'_' && second == b'_' {
        return Some(marker("__", FormattingNodeType::Underline, 2));
    }
    if first == b'`' {
        let mut count = 1;
        while count < text.len() && byte_at(text, count) == b'`' {
            count += 1;
        }
        return Some(FormattingMarkerInfo {
            marker: &text[..count],
            node_type: FormattingNodeType::InlineCode,
            marker_length: count,
        });
    }
    if first == b'*' {
        return Some(marker("*", FormattingNodeType::Emphasis, 1));
    }
    if first == b'_' {
        return Some(marker("_", FormattingNodeType::Emphasis, 1));
    }
    None
}

fn marker(
    marker: &'static str,
    node_type: FormattingNodeType,
    marker_length: usize,
) -> FormattingMarkerInfo<'static> {
    FormattingMarkerInfo {
        marker,
        node_type,
        marker_length,
    }
}

fn find_formatting_end(text: &str, marker: FormattingMarkerInfo<'_>) -> Option<FormattingEnd> {
    let mut position = marker.marker_length;
    let mut nested_level = 0usize;
    if text.len() < marker.marker_length * 2 {
        return None;
    }
    if marker.node_type == FormattingNodeType::InlineCode && marker.marker_length > 1 {
        while position < text.len() {
            let current = byte_at(text, position);
            if is_inline_code_line_break(current) {
                return None;
            }
            if current == b'`' {
                let mut count = 0usize;
                let mut check = position;
                while check < text.len() && byte_at(text, check) == b'`' {
                    count += 1;
                    check += 1;
                }
                if count >= marker.marker_length {
                    return Some(FormattingEnd {
                        end_position: position + count - marker.marker_length,
                    });
                }
                position = check;
                continue;
            }
            position += advance_one(text, position);
            if position > MAX_LINE_LENGTH {
                break;
            }
        }
        return None;
    }
    if marker.marker_length == 1 && marker.node_type == FormattingNodeType::InlineCode {
        let marker_char = byte_at(marker.marker, 0);
        while position < text.len() {
            let current = byte_at(text, position);
            if is_inline_code_line_break(current) {
                return None;
            }
            if current == b'\\' && position + 1 < text.len() {
                position += 2;
                continue;
            }
            if current == marker_char {
                if marker_char == b'`'
                    && position + 1 < text.len()
                    && byte_at(text, position + 1) == b'`'
                {
                    while position < text.len() && byte_at(text, position) == b'`' {
                        position += 1;
                    }
                    continue;
                }
                return Some(FormattingEnd {
                    end_position: position,
                });
            }
            position += advance_one(text, position);
            if position > MAX_LINE_LENGTH {
                break;
            }
        }
        return None;
    }
    if marker.marker_length == 1 && marker.node_type == FormattingNodeType::Emphasis {
        let marker_char = byte_at(marker.marker, 0);
        while position < text.len() {
            let current = byte_at(text, position);
            if current == b'\\' && position + 1 < text.len() {
                position += 2;
                continue;
            }
            if current == marker_char {
                if marker_char == b'*'
                    && let Some(nested_end) = find_nested_asterisk_run_end(text, position)
                {
                    position = nested_end;
                    continue;
                }
                if marker_char == b'_'
                    && position + 1 < text.len()
                    && byte_at(text, position + 1) == b'_'
                {
                    position += 2;
                    continue;
                }
                let prev = if position > 0 {
                    byte_at(text, position - 1)
                } else {
                    0
                };
                let next = byte_at(text, position + 1);
                if links::can_close_single_emphasis(marker_char, prev, next) {
                    return Some(FormattingEnd {
                        end_position: position,
                    });
                }
            }
            position += advance_one(text, position);
            if position > MAX_LINE_LENGTH {
                break;
            }
        }
        return None;
    }
    if marker.node_type == FormattingNodeType::InlineCode {
        while position < text.len() {
            let current = byte_at(text, position);
            if is_inline_code_line_break(current) {
                return None;
            }
            if current == b'`' {
                return Some(FormattingEnd {
                    end_position: position,
                });
            }
            position += advance_one(text, position);
            if position > MAX_LINE_LENGTH {
                break;
            }
        }
        return None;
    }
    let first_marker_char = byte_at(marker.marker, 0);
    let is_double = marker.marker.len() > 1;
    while position < text.len() {
        if byte_at(text, position) == b'\\' && position + 1 < text.len() {
            position += 2;
            continue;
        }
        if text.as_bytes()[position..].starts_with(marker.marker.as_bytes()) {
            if nested_level == 0 {
                if marker.node_type == FormattingNodeType::Spoiler
                    && position == marker.marker_length
                    && position + marker.marker.len() < text.len()
                {
                    position += 1;
                    continue;
                }
                return Some(FormattingEnd {
                    end_position: position,
                });
            }
            nested_level -= 1;
            position += marker.marker.len();
            continue;
        }
        if is_double
            && position + 1 < text.len()
            && byte_at(text, position) == first_marker_char
            && byte_at(text, position + 1) == first_marker_char
        {
            nested_level += 1;
        }
        position += advance_one(text, position);
        if position > MAX_LINE_LENGTH {
            break;
        }
    }
    None
}

fn is_inline_code_line_break(byte: u8) -> bool {
    matches!(byte, b'\n' | b'\r')
}

fn find_nested_asterisk_run_end(text: &str, position: usize) -> Option<usize> {
    let run_length = links::count_repeated_byte(text, position, b'*');
    if run_length < 2 {
        return None;
    }
    let previous = if position > 0 {
        byte_at(text, position - 1)
    } else {
        0
    };
    let next = byte_at(text, position + run_length);
    if !links::is_left_flanking_delimiter(previous, next) {
        return None;
    }
    let nested_marker = get_formatting_marker_info(&text[position..])?;
    if byte_at(nested_marker.marker, 0) != b'*' || nested_marker.marker_length < 2 {
        return None;
    }
    let nested_end = find_formatting_end(&text[position..], nested_marker)?;
    let end_position = position + nested_end.end_position + nested_marker.marker_length;
    if end_position <= position || end_position > text.len() {
        return None;
    }
    Some(end_position)
}

fn create_formatting_node(
    parser: &mut MarkdownParser,
    marker: FormattingMarkerInfo<'_>,
    inner: &str,
    inner_offset: usize,
    is_block: bool,
) -> Result<Node, ParseError> {
    if marker.node_type == FormattingNodeType::InlineCode {
        return Ok(Node::InlineCode {
            content: unescape_inline_code(inner),
        });
    }
    if marker.marker == "***" || marker.marker == "___" {
        let mut strong_context = FormattingContext::default();
        strong_context.push(b'*', true);
        let inner_nodes =
            parse_inline_with_context(parser, inner, inner_offset, &mut strong_context)?;
        return Ok(Node::Emphasis {
            children: vec![Node::Strong {
                children: inner_nodes,
            }],
        });
    }
    let mut new_context = FormattingContext::default();
    new_context.push(byte_at(marker.marker, 0), marker.marker.len() > 1);
    let children = parse_inline_with_context(parser, inner, inner_offset, &mut new_context)?;
    Ok(match marker.node_type {
        FormattingNodeType::Strong => Node::Strong { children },
        FormattingNodeType::Emphasis => Node::Emphasis { children },
        FormattingNodeType::Underline => Node::Underline { children },
        FormattingNodeType::Strikethrough => Node::Strikethrough { children },
        FormattingNodeType::Spoiler => Node::Spoiler {
            children,
            is_block: Some(is_block),
        },
        FormattingNodeType::InlineCode => unreachable!(),
    })
}

fn unescape_inline_code(content: &str) -> String {
    if !content.contains("\\`") {
        return content.to_owned();
    }
    let mut out = Vec::<u8>::new();
    let mut position = 0usize;
    while position < content.len() {
        if byte_at(content, position) != b'\\' {
            out.push(byte_at(content, position));
            position += 1;
            continue;
        }
        let mut count = 0usize;
        while position + count < content.len() && byte_at(content, position + count) == b'\\' {
            count += 1;
        }
        let next = position + count;
        if next < content.len() && byte_at(content, next) == b'`' {
            out.extend(std::iter::repeat_n(b'\\', count / 2));
            out.push(b'`');
            position = next + 1;
            continue;
        }
        out.extend_from_slice(&content.as_bytes()[position..next]);
        position = next;
    }
    string_from_lossy(&out)
}

fn flush_accumulated_text(nodes: &mut Vec<Node>, accumulated: &mut Vec<u8>) {
    if accumulated.is_empty() {
        return;
    }
    let content = string_from_lossy(accumulated);
    append_text(nodes, content);
    accumulated.clear();
}

fn flush_bounded_accumulated_text(
    nodes: &mut Vec<Node>,
    accumulated: &mut Vec<u8>,
    max_len: usize,
) {
    if accumulated.is_empty() {
        return;
    }
    let mut prefix_len = accumulated.len().min(max_len);
    while prefix_len > 0 && std::str::from_utf8(&accumulated[..prefix_len]).is_err() {
        prefix_len -= 1;
    }
    if prefix_len > 0 {
        append_text(nodes, string_from_lossy(&accumulated[..prefix_len]));
    }
    accumulated.clear();
}

fn is_formatting_char(char: u8) -> bool {
    matches!(char, b'*' | b'_' | b'~' | b'|' | b'`')
}
