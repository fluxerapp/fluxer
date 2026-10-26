// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{EmojiKind, Node, ParserFlags, ParserResult};
use crate::constants::{CODE_FENCE_LENGTH, MAX_INLINE_DEPTH, MAX_LINE_LENGTH};
use crate::emoji::{StandardCursor, StandardEmoji};
use crate::links;
use crate::normalize::{
    combine_adjacent_text, compact_empty_text_nodes, flatten_top_level_formatting,
    merge_adjacent_text_simple, normalize_nodes,
};
use crate::parser::{MarkdownParser, ParseError};
use crate::scan::{Closing, Lookahead, Pattern, Token, step};
use crate::text::{
    append_text, byte_at, has_visible_content, is_alpha_numeric, is_escapable_character,
    string_from_lossy,
};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct FormattingEntry {
    delimiter: u8,
    is_double: bool,
}

#[derive(Clone, Debug, Default)]
struct FormattingContext {
    entries: [FormattingEntry; MAX_INLINE_DEPTH],
    len: usize,
}

impl FormattingContext {
    fn can_enter(&self, delimiter: u8, is_double: bool) -> bool {
        self.len < MAX_INLINE_DEPTH && !self.is_active(delimiter, is_double)
    }

    fn is_active(&self, delimiter: u8, is_double: bool) -> bool {
        self.entries[..self.len]
            .iter()
            .any(|entry| entry.delimiter == delimiter && entry.is_double == is_double)
    }

    fn push(&mut self, delimiter: u8, is_double: bool) {
        if self.len < MAX_INLINE_DEPTH {
            self.entries[self.len] = FormattingEntry {
                delimiter,
                is_double,
            };
            self.len += 1;
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
struct FormattingMarkerInfo {
    delimiter: u8,
    node_type: FormattingNodeType,
    marker_length: usize,
}

impl FormattingMarkerInfo {
    fn is_double(self) -> bool {
        self.marker_length > 1
    }

    fn is_triple_emphasis(self) -> bool {
        self.marker_length == 3 && self.node_type == FormattingNodeType::Emphasis
    }

    fn scan_index(self) -> usize {
        match (self.delimiter, self.marker_length) {
            (b'*', 2) => 0,
            (b'_', 2) => 1,
            (b'~', _) => 2,
            (b'|', _) => 3,
            (b'*', 3) => 4,
            (b'_', 3) => 5,
            (b'*', _) => 6,
            (b'_', _) => 7,
            _ => 8,
        }
    }
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
    let mut accumulated = String::new();
    let mut scan = Lookahead::new(text);
    let emoji_context = parser.shared_emoji_context();
    let mut emojis = emoji_context.standard_cursor();
    let mut position = 0usize;
    while position < text.len() {
        let current = byte_at(text, position);
        if is_plain_text_byte(current) {
            let run_limit = emojis
                .next_offset(base_offset + position)
                .map_or(text.len(), |offset| (offset - base_offset).min(text.len()));
            if run_limit > position {
                let run_end = text.as_bytes()[position..run_limit]
                    .iter()
                    .position(|byte| !is_plain_text_byte(*byte))
                    .map_or(run_limit, |offset| position + offset);
                let room = (MAX_LINE_LENGTH + 1)
                    .saturating_sub(accumulated.len())
                    .max(1);
                let end = run_end.min(position + room);
                accumulated.push_str(&text[position..end]);
                position = end;
                if accumulated.len() > MAX_LINE_LENGTH {
                    flush_bounded_accumulated_text(&mut nodes, &mut accumulated, MAX_LINE_LENGTH);
                    break;
                }
                continue;
            }
        }
        if current == b'\\' && position + 1 < text.len() {
            let next = byte_at(text, position + 1);
            if next == b'_' && position > 0 && byte_at(text, position - 1) == 0xaf {
                accumulated.push_str("\\_");
                position += 2;
                continue;
            }
            if next == b':'
                && let Some(result) = links::parse_emoji_shortcode(parser, &text[position + 1..])
                && let Node::Emoji {
                    kind: EmojiKind::Standard { raw, .. },
                } = &result.node
            {
                accumulated.push_str(raw);
                position += 1 + result.advance;
                continue;
            }
            if let Some(emoji) = emojis.at(base_offset + position + 1)
                && text[position + 1..].starts_with(&emoji.raw)
            {
                accumulated.push_str(&emoji.raw);
                position += 1 + emoji.len;
                continue;
            }
            if next == b'`' {
                let run = backtick_run_length(text, position + 1);
                if run >= CODE_FENCE_LENGTH {
                    accumulated.extend(std::iter::repeat_n('`', run));
                    position += 1 + run;
                    continue;
                }
            }
            if is_escapable_character(next)
                || is_ordered_list_marker_dot_escape(text, position)
                || is_word_dot_escape(text, position)
            {
                accumulated.push(char::from(next));
                position += 2;
                continue;
            }
        }

        let remaining = &text[position..];
        if !accumulated.ends_with("<\"")
            && !accumulated.ends_with("<'")
            && ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
            && links::starts_with_url(remaining)
            && let Some(result) = links::parse_url_segment(&mut scan, position, parser.flags())
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
            accumulated.push('_');
            position += 1;
            continue;
        }

        if let Some(result) = parse_regional_indicator_flag(remaining) {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            push_regional_indicator_pair(
                &mut nodes,
                &mut emojis,
                base_offset + position,
                &remaining[..result.advance],
                result.node,
            );
            position += result.advance;
            continue;
        }

        if let Some(emoji) = emojis.at(base_offset + position)
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

        if current == b':'
            && let Some(result) = links::parse_emoji_shortcode(parser, remaining)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
        }

        if current == b'<'
            && let Some(close) = scan.next(Pattern::AngleClose, position, text.len())
            && let Some(result) = parse_angle_bracket(parser, &mut scan, position, close)
        {
            flush_accumulated_text(&mut nodes, &mut accumulated);
            nodes.push(result.node);
            position += result.advance;
            continue;
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
            accumulated.push_str(&text[position..position + char_len]);
            position += char_len;
            continue;
        }

        let is_double_underscore =
            current == b'_' && position + 1 < text.len() && byte_at(text, position + 1) == b'_';
        if (is_formatting_char(current) || current == b'[')
            && (is_double_underscore
                || !(current == b'_'
                    && !accumulated.is_empty()
                    && is_alpha_numeric(*accumulated.as_bytes().last().unwrap_or(&0))))
        {
            let previous = if position > 0 {
                byte_at(text, position - 1)
            } else {
                0
            };
            if let Some(result) = parse_special_sequence(
                parser,
                &mut scan,
                position,
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

        accumulated.push(char::from(current));
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

fn push_regional_indicator_pair(
    nodes: &mut Vec<Node>,
    emojis: &mut StandardCursor<'_>,
    offset: usize,
    pair: &str,
    flag: Node,
) {
    let (first, second) = pair.split_at(pair.len() / 2);
    match emojis.at(offset) {
        Some(emoji) if emoji.raw == pair => nodes.push(standard_emoji_node(emoji)),
        Some(emoji) if emoji.raw == first => {
            nodes.push(standard_emoji_node(emoji));
            match emojis.at(offset + first.len()) {
                Some(emoji) if emoji.raw == second => nodes.push(standard_emoji_node(emoji)),
                _ => nodes.extend(regional_indicator_emoji(second)),
            }
        }
        _ => nodes.push(flag),
    }
}

fn standard_emoji_node(emoji: &StandardEmoji) -> Node {
    Node::Emoji {
        kind: EmojiKind::Standard {
            raw: emoji.raw.clone(),
            codepoints: emoji.codepoints.clone(),
            name: emoji.name.clone(),
        },
    }
}

fn regional_indicator_emoji(text: &str) -> Option<Node> {
    let ch = text.chars().next()?;
    let letter = regional_indicator_letter(ch)?;
    Some(Node::Emoji {
        kind: EmojiKind::Standard {
            raw: ch.to_string(),
            codepoints: format!("{:x}", ch as u32),
            name: format!("regional_indicator_{letter}"),
        },
    })
}

fn regional_indicator_letter(ch: char) -> Option<char> {
    let codepoint = ch as u32;
    if !(0x1f1e6..=0x1f1ff).contains(&codepoint) {
        return None;
    }
    char::from_u32(u32::from(b'a') + codepoint - 0x1f1e6)
}

fn backtick_run_length(text: &str, start: usize) -> usize {
    let mut run = 0usize;
    while start + run < text.len() && byte_at(text, start + run) == b'`' {
        run += 1;
    }
    run
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
    let mut line_start = digit_start;
    while line_start > 0 && byte_at(text, line_start - 1) == b' ' {
        line_start -= 1;
    }
    if line_start > 0 && byte_at(text, line_start - 1) != b'\n' {
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

fn parse_angle_bracket(
    parser: &MarkdownParser,
    scan: &mut Lookahead<'_>,
    position: usize,
    close: usize,
) -> Option<ParserResult> {
    let tag = &scan.text[position..=close];
    links::parse_custom_emoji(tag)
        .or_else(|| links::parse_timestamp(tag))
        .or_else(|| links::parse_phone_link(parser, tag))
        .or_else(|| links::parse_sms_link(parser, tag))
        .or_else(|| links::parse_mention(tag, parser.flags()))
        .or_else(|| links::parse_autolink(scan, position, close, parser.flags()))
        .or_else(|| links::parse_email_link(parser, tag))
}

fn parse_special_sequence(
    parser: &mut MarkdownParser,
    scan: &mut Lookahead<'_>,
    position: usize,
    base_offset: usize,
    context: &mut FormattingContext,
    previous: u8,
) -> Result<Option<ParserResult>, ParseError> {
    if byte_at(scan.text, position) != b'[' {
        return parse_formatting(parser, scan, position, base_offset, context, previous);
    }
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_MASKED_LINKS) {
        return Ok(None);
    }
    links::parse_masked_link(parser, scan, position, base_offset)
}

fn parse_formatting(
    parser: &mut MarkdownParser,
    scan: &mut Lookahead<'_>,
    position: usize,
    base_offset: usize,
    context: &mut FormattingContext,
    previous: u8,
) -> Result<Option<ParserResult>, ParseError> {
    let text = &scan.text[position..];
    if text.len() < 2 {
        return Ok(None);
    }
    let Some(marker) = get_formatting_marker_info(scan, position) else {
        return Ok(None);
    };
    if marker.node_type == FormattingNodeType::Spoiler
        && !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_SPOILERS)
    {
        return Ok(None);
    }
    if !context.can_enter(marker.delimiter, marker.is_double()) {
        return Ok(None);
    }
    if marker.marker_length == 1 && marker.node_type == FormattingNodeType::Emphasis {
        let next = byte_at(text, marker.marker_length);
        if !links::can_open_single_emphasis(marker.delimiter, previous, next) {
            return Ok(None);
        }
    }
    let Some(end_position) = find_formatting_end(scan, position, marker) else {
        return Ok(None);
    };
    let inner = &text[marker.marker_length..end_position];
    if !has_visible_content(inner) {
        return Ok(None);
    }
    let is_block = context.is_active(marker.delimiter, marker.is_double());
    let node = create_formatting_node(
        parser,
        marker,
        inner,
        base_offset + marker.marker_length,
        is_block,
    )?;
    Ok(Some(ParserResult {
        node,
        advance: end_position + marker.marker_length,
    }))
}

fn get_formatting_marker_info(
    scan: &mut Lookahead<'_>,
    position: usize,
) -> Option<FormattingMarkerInfo> {
    let text = scan.text;
    let first = byte_at(text, position);
    if !is_formatting_char(first) {
        return None;
    }
    let second = byte_at(text, position + 1);
    let third = byte_at(text, position + 2);
    let (node_type, marker_length) = match (first, second, third) {
        (b'*', b'*', b'*') | (b'_', b'_', b'_') => (FormattingNodeType::Emphasis, 3),
        (b'|', b'|', _) => (FormattingNodeType::Spoiler, 2),
        (b'~', b'~', _) => (FormattingNodeType::Strikethrough, 2),
        (b'*', b'*', _) => (FormattingNodeType::Strong, 2),
        (b'_', b'_', _) => (FormattingNodeType::Underline, 2),
        (b'`', _, _) => (
            FormattingNodeType::InlineCode,
            scan.run_end(position, b'`') - position,
        ),
        (b'*' | b'_', _, _) => (FormattingNodeType::Emphasis, 1),
        _ => return None,
    };
    Some(FormattingMarkerInfo {
        delimiter: first,
        node_type,
        marker_length,
    })
}

fn find_formatting_end(
    scan: &mut Lookahead<'_>,
    opener: usize,
    marker: FormattingMarkerInfo,
) -> Option<usize> {
    let len = scan.text.len();
    if len - opener < marker.marker_length * 2 {
        return None;
    }
    let mut start = opener + marker.marker_length;
    if marker.node_type == FormattingNodeType::InlineCode && marker.marker_length > 1 {
        return scan.code_span_end(opener, marker.marker_length, MAX_LINE_LENGTH);
    }
    if marker.node_type == FormattingNodeType::Spoiler
        && scan.text.as_bytes()[start..].starts_with(b"||")
        && start + 2 < len
    {
        start += 1;
    }
    let close_len = if marker.is_triple_emphasis() { 3 } else { 1 };
    let closer = scan.closer(
        Closing::Formatting(marker.scan_index()),
        start,
        close_len,
        |scan, position| classify_formatting(scan, marker, position),
    );
    closer?
        .within(opener, MAX_LINE_LENGTH)
        .map(|end| end - opener)
}

fn classify_formatting(
    scan: &mut Lookahead<'_>,
    marker: FormattingMarkerInfo,
    position: usize,
) -> Token {
    let text = scan.text;
    let bytes = text.as_bytes();
    let current = bytes[position];
    let delimiter = marker.delimiter;
    if marker.node_type == FormattingNodeType::InlineCode && is_inline_code_line_break(current) {
        return Token::Fail;
    }
    if current == b'\\' && position + 1 < bytes.len() {
        return Token::Skip(2);
    }
    let repeats = |count: usize| {
        bytes.len() - position >= count
            && bytes[position..position + count]
                .iter()
                .all(|byte| *byte == delimiter)
    };
    match (marker.node_type, marker.marker_length) {
        (FormattingNodeType::InlineCode, _) if current == b'`' => {
            if repeats(2) {
                Token::Skip(scan.run_end(position, b'`') - position)
            } else {
                Token::Close
            }
        }
        (FormattingNodeType::Emphasis, 1) if current == delimiter => {
            if delimiter == b'*'
                && let Some(nested_end) = find_nested_asterisk_run_end(scan, position)
            {
                return Token::Skip(nested_end - position);
            }
            if delimiter == b'_' && repeats(2) {
                return Token::Skip(2);
            }
            let next = byte_at(text, position + 1);
            if links::can_close_single_emphasis(delimiter, bytes[position - 1], next) {
                return Token::Close;
            }
            step(text, position)
        }
        (FormattingNodeType::Emphasis, 3) if repeats(3) => Token::Close,
        (FormattingNodeType::Emphasis, 3) if repeats(2) => Token::Open(1),
        (FormattingNodeType::InlineCode | FormattingNodeType::Emphasis, _) => step(text, position),
        _ if repeats(2) => Token::Close,
        _ => step(text, position),
    }
}

fn is_inline_code_line_break(byte: u8) -> bool {
    matches!(byte, b'\n' | b'\r')
}

fn find_nested_asterisk_run_end(scan: &mut Lookahead<'_>, position: usize) -> Option<usize> {
    let text = scan.text;
    let run_length = scan.run_end(position, b'*') - position;
    if run_length < 2 {
        return None;
    }
    let previous = byte_at(text, position - 1);
    let next = byte_at(text, position + run_length);
    if !links::is_left_flanking_delimiter(previous, next) {
        return None;
    }
    let nested_marker = get_formatting_marker_info(scan, position)?;
    let nested_end = find_formatting_end(scan, position, nested_marker)?;
    let end_position = position + nested_end + nested_marker.marker_length;
    if end_position <= position || end_position > text.len() {
        return None;
    }
    Some(end_position)
}

fn create_formatting_node(
    parser: &mut MarkdownParser,
    marker: FormattingMarkerInfo,
    inner: &str,
    inner_offset: usize,
    is_block: bool,
) -> Result<Node, ParseError> {
    if marker.node_type == FormattingNodeType::InlineCode {
        return Ok(Node::InlineCode {
            content: unescape_inline_code(inner),
        });
    }
    if marker.is_triple_emphasis() {
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
    new_context.push(marker.delimiter, marker.is_double());
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

fn flush_accumulated_text(nodes: &mut Vec<Node>, accumulated: &mut String) {
    append_text(nodes, accumulated);
    accumulated.clear();
}

fn flush_bounded_accumulated_text(nodes: &mut Vec<Node>, accumulated: &mut String, max_len: usize) {
    let mut prefix_len = accumulated.len().min(max_len);
    while !accumulated.is_char_boundary(prefix_len) {
        prefix_len -= 1;
    }
    accumulated.truncate(prefix_len);
    flush_accumulated_text(nodes, accumulated);
}

fn is_plain_text_byte(byte: u8) -> bool {
    byte.is_ascii()
        && !matches!(
            byte,
            b'\\' | b'h' | b'f' | b'_' | b'<' | b':' | b'@' | b'*' | b'~' | b'|' | b'`' | b'['
        )
}

fn is_formatting_char(char: u8) -> bool {
    matches!(char, b'*' | b'_' | b'~' | b'|' | b'`')
}
