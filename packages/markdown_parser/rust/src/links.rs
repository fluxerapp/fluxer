// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{
    EmojiKind, GuildNavigationType, MentionKind, Node, ParserFlags, ParserResult, TimestampStyle,
};
use crate::constants::{MAX_LINE_LENGTH, MAX_LINK_URL_LENGTH};
use crate::parser::{MarkdownParser, ParseError};
use crate::text::{
    advance_one, bounded_url_end, byte_at, find_from, has_visible_content, is_alpha_numeric,
    is_digit, is_digit_only, is_whitespace, starts_with, trim,
};

const APP_PROTOCOL_SCHEME: &str = "fluxer:";

#[derive(Clone, Debug)]
struct BracketResult<'a> {
    bracket_position: usize,
    link_text: &'a str,
}

#[derive(Clone, Debug)]
struct UrlInfo<'a> {
    url: &'a str,
    escaped: bool,
    advance_by: usize,
}

#[derive(Clone, Debug)]
struct ComparableUrl<'a> {
    origin: &'a str,
    path: &'a str,
    search: &'a str,
    hash: &'a str,
}

pub fn parse_custom_emoji(text: &str) -> Option<ParserResult> {
    if !(starts_with(text, "<:") || starts_with(text, "<a:")) {
        return None;
    }
    let end = find_from(text, 0, b'>')?;
    if end < 4 {
        return None;
    }
    let mut pos = 1;
    let mut animated = false;
    if byte_at(text, pos) == b'a' {
        animated = true;
        pos += 1;
    }
    if byte_at(text, pos) != b':' {
        return None;
    }
    pos += 1;
    let name_start = pos;
    while pos < end && byte_at(text, pos) != b':' {
        if !is_custom_emoji_name_char(byte_at(text, pos)) {
            return None;
        }
        pos += 1;
    }
    if pos == name_start || pos >= end || byte_at(text, pos) != b':' {
        return None;
    }
    let name = &text[name_start..pos];
    pos += 1;
    let id_start = pos;
    while pos < end {
        if !is_digit(byte_at(text, pos)) {
            return None;
        }
        pos += 1;
    }
    if pos == id_start {
        return None;
    }
    Some(ParserResult {
        node: Node::Emoji {
            kind: EmojiKind::Custom {
                name: name.to_owned(),
                id: text[id_start..pos].to_owned(),
                animated,
            },
        },
        advance: end + 1,
    })
}

pub fn parse_emoji_shortcode(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !starts_with(text, ":") || text.len() < 3 {
        return None;
    }
    let end = find_from(text, 1, b':')?;
    if end == 1 {
        return None;
    }
    let name = &text[1..end];
    if let Some(content) = special_shortcode_text(name) {
        return Some(ParserResult {
            node: Node::Text {
                content: content.to_owned(),
            },
            advance: end + 1,
        });
    }
    if !is_valid_emoji_name(name) {
        return None;
    }
    let base = parser.emoji_context().shortcode(name)?;
    let mut raw = base.raw.clone();
    let mut codepoints = base.codepoints.clone();
    let mut advance = end + 1;
    let after = &text[advance..];
    if starts_with(after, ":skin-tone-")
        && after.len() >= 13
        && byte_at(after, 11).is_ascii_digit()
        && byte_at(after, 12) == b':'
    {
        let tone = byte_at(after, 11) - b'0';
        if (1..=5).contains(&tone)
            && let Some(skin) = parser.emoji_context().skin(name, tone)
        {
            raw = skin.raw.clone();
            codepoints = skin.codepoints.clone();
            advance += 13;
        }
    }
    Some(ParserResult {
        node: Node::Emoji {
            kind: EmojiKind::Standard {
                raw,
                codepoints,
                name: name.to_owned(),
            },
        },
        advance,
    })
}

pub fn parse_timestamp(text: &str) -> Option<ParserResult> {
    if !starts_with(text, "<t:") {
        return None;
    }
    let end = find_from(text, 0, b'>')?;
    let inner = &text[3..end];
    if inner.is_empty() {
        return None;
    }
    let mut parts = inner.split(':');
    let timestamp_part = parts.next()?;
    let style_part = parts.next();
    if parts.next().is_some() || !is_digit_only(timestamp_part) {
        return None;
    }
    let timestamp = timestamp_part.parse::<u64>().ok()?;
    if timestamp == 0 || timestamp > 8_640_000_000_000 {
        return None;
    }
    let mut style = TimestampStyle::ShortDateTime;
    if let Some(value) = style_part {
        if value.is_empty() {
            return None;
        }
        style = match byte_at(value, 0) {
            b't' => TimestampStyle::ShortTime,
            b'T' => TimestampStyle::LongTime,
            b'd' => TimestampStyle::ShortDate,
            b'D' => TimestampStyle::LongDate,
            b'f' => TimestampStyle::ShortDateTime,
            b'F' => TimestampStyle::LongDateTime,
            b's' => TimestampStyle::ShortDateShortTime,
            b'S' => TimestampStyle::ShortDateMediumTime,
            b'R' => TimestampStyle::RelativeTime,
            _ => return None,
        };
    }
    Some(ParserResult {
        node: Node::Timestamp { timestamp, style },
        advance: end + 1,
    })
}

pub fn parse_mention(text: &str, flags: u32) -> Option<ParserResult> {
    if text.len() < 2 || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = find_from(text, 0, b'>')?;
    if end < 2 {
        return None;
    }
    let inner = &text[1..end];
    if inner.is_empty() {
        return None;
    }
    match byte_at(inner, 0) {
        b'@' => {
            if inner.len() > 2 && byte_at(inner, 1) == b'&' {
                let id = &inner[2..];
                if ParserFlags::has(flags, ParserFlags::ALLOW_ROLE_MENTIONS) && is_digit_only(id) {
                    return mention(MentionKind::Role { id: id.to_owned() }, end);
                }
            } else {
                let id = if starts_with(inner, "@!") {
                    &inner[2..]
                } else {
                    &inner[1..]
                };
                if ParserFlags::has(flags, ParserFlags::ALLOW_USER_MENTIONS) && is_digit_only(id) {
                    return mention(MentionKind::User { id: id.to_owned() }, end);
                }
            }
        }
        b'#' => {
            let id = &inner[1..];
            if ParserFlags::has(flags, ParserFlags::ALLOW_CHANNEL_MENTIONS) && is_digit_only(id) {
                return mention(MentionKind::Channel { id: id.to_owned() }, end);
            }
        }
        b'/' => {
            if !ParserFlags::has(flags, ParserFlags::ALLOW_COMMAND_MENTIONS) {
                return None;
            }
            let colon = find_from(inner, 0, b':')?;
            let id = &inner[colon + 1..];
            if !is_digit_only(id) {
                return None;
            }
            let command = &inner[1..colon];
            let mut segments = Vec::new();
            for segment in command.split(' ') {
                if segment.is_empty() || !is_valid_command_name(segment) || segments.len() == 3 {
                    return None;
                }
                segments.push(segment);
            }
            if segments.is_empty() {
                return None;
            }
            return mention(
                MentionKind::Command {
                    name: segments[0].to_owned(),
                    subcommand_group: (segments.len() == 3).then(|| segments[1].to_owned()),
                    subcommand: (segments.len() >= 2)
                        .then(|| segments[segments.len() - 1].to_owned()),
                    id: id.to_owned(),
                },
                end,
            );
        }
        b'i' if starts_with(inner, "id:") => {
            if !ParserFlags::has(flags, ParserFlags::ALLOW_GUILD_NAVIGATIONS) {
                return None;
            }
            let mut parts = inner.split(':');
            let id_label = parts.next()?;
            let nav_type = parts.next()?;
            let nav_id = parts.next();
            if parts.next().is_some() || id_label != "id" {
                return None;
            }
            let navigation = guild_navigation_type(nav_type)?;
            if navigation != GuildNavigationType::LinkedRoles && nav_id.is_some() {
                return None;
            }
            return mention(
                MentionKind::GuildNavigation {
                    navigation_type: navigation,
                    id: nav_id.map(ToOwned::to_owned),
                },
                end,
            );
        }
        _ => {}
    }
    None
}

fn mention(kind: MentionKind, end: usize) -> Option<ParserResult> {
    Some(ParserResult {
        node: Node::Mention { kind },
        advance: end + 1,
    })
}

pub fn parse_masked_link(
    parser: &mut MarkdownParser,
    text: &str,
    base_offset: usize,
) -> Result<Option<ParserResult>, ParseError> {
    if text.is_empty() || byte_at(text, 0) != b'[' {
        return Ok(None);
    }
    let Some(closing) = find_closing_bracket(text) else {
        return Ok(None);
    };
    let link_text = closing.link_text;
    let trimmed_link_text = trim(link_text);
    if contains_link_syntax(link_text) {
        if closing.bracket_position + 1 < text.len()
            && byte_at(text, closing.bracket_position + 1) == b'('
            && let Some(url_info) = extract_url(text, closing.bracket_position + 2)
        {
            return Ok(Some(ParserResult {
                node: Node::Text {
                    content: text[..url_info.advance_by].to_owned(),
                },
                advance: url_info.advance_by,
            }));
        }
        return Ok(Some(ParserResult {
            node: Node::Text {
                content: text[..closing.bracket_position + 1].to_owned(),
            },
            advance: closing.bracket_position + 1,
        }));
    }
    if closing.bracket_position + 1 >= text.len()
        || byte_at(text, closing.bracket_position + 1) != b'('
    {
        return Ok(None);
    }
    let Some(url_info) = extract_url(text, closing.bracket_position + 2) else {
        if let Some(mention) = parse_mention(trimmed_link_text, parser.flags())
            && mention.advance == trimmed_link_text.len()
        {
            return Ok(None);
        }
        return Ok(Some(ParserResult {
            node: Node::Text {
                content: text[..closing.bracket_position + 1].to_owned(),
            },
            advance: closing.bracket_position + 1,
        }));
    };
    if !has_visible_content(link_text)
        || is_slash_command_masked_link_text(trim(link_text))
        || is_email_like(trim(link_text))
        || url_info.url.len() > MAX_LINK_URL_LENGTH
        || !is_valid_masked_link_url(url_info.url)
        || (starts_with_url(trimmed_link_text)
            && should_treat_as_masked_link(trimmed_link_text, url_info.url))
    {
        return Ok(Some(ParserResult {
            node: Node::Text {
                content: text[..url_info.advance_by].to_owned(),
            },
            advance: url_info.advance_by,
        }));
    }
    let inline_nodes = crate::inline::parse_inline(parser, link_text, base_offset + 1)?;
    let text_node = if inline_nodes.len() == 1 {
        Box::new(inline_nodes.into_iter().next().unwrap())
    } else {
        Box::new(Node::Sequence {
            children: inline_nodes,
        })
    };
    let source = &text[..url_info.advance_by];
    Ok(Some(ParserResult {
        node: Node::Link {
            text: Some(text_node),
            url: url_info.url.to_owned(),
            raw_url: url_info.url.to_owned(),
            source: source.to_owned(),
            escaped: url_info.escaped,
        },
        advance: url_info.advance_by,
    }))
}

fn find_closing_bracket(text: &str) -> Option<BracketResult<'_>> {
    let mut position = 1;
    let mut nested = 0usize;
    while position < text.len() {
        match byte_at(text, position) {
            b'[' => {
                nested += 1;
                position += 1;
            }
            b']' if nested > 0 => {
                nested -= 1;
                position += 1;
            }
            b']' => {
                return Some(BracketResult {
                    bracket_position: position,
                    link_text: &text[1..position],
                });
            }
            b'\\' => position += if position + 1 < text.len() { 2 } else { 1 },
            _ => position += advance_one(text, position),
        }
        if position > MAX_LINK_URL_LENGTH {
            break;
        }
    }
    None
}

fn extract_url(text: &str, start: usize) -> Option<UrlInfo<'_>> {
    if start >= text.len() {
        return None;
    }
    if byte_at(text, start) == b'<' {
        return extract_escaped_url(text, start + 1);
    }
    extract_unescaped_url(text, start)
}

fn extract_escaped_url(text: &str, start: usize) -> Option<UrlInfo<'_>> {
    let mut pos = start;
    while pos < text.len() {
        if byte_at(text, pos) == b'>' {
            let url = &text[start..pos];
            pos += 1;
            if pos >= text.len() || byte_at(text, pos) != b')' {
                return None;
            }
            return Some(UrlInfo {
                url,
                escaped: true,
                advance_by: pos + 1,
            });
        }
        pos += advance_one(text, pos);
    }
    None
}

fn extract_unescaped_url(text: &str, start: usize) -> Option<UrlInfo<'_>> {
    let mut pos = start;
    let mut nested = 0usize;
    while pos < text.len() {
        match byte_at(text, pos) {
            b'(' => {
                nested += 1;
                pos += 1;
            }
            b')' if nested > 0 => {
                nested -= 1;
                pos += 1;
            }
            b')' => {
                return Some(UrlInfo {
                    url: &text[start..pos],
                    escaped: false,
                    advance_by: pos + 1,
                });
            }
            _ => pos += advance_one(text, pos),
        }
    }
    None
}

pub fn parse_url_segment(text: &str, flags: u32) -> Option<ParserResult> {
    if !ParserFlags::has(flags, ParserFlags::ALLOW_AUTOLINKS) {
        return None;
    }
    let prefix_len = if starts_with(text, "https://") {
        8
    } else if starts_with(text, "http://") {
        7
    } else if starts_with_app_protocol_url(text) {
        APP_PROTOCOL_SCHEME.len()
    } else {
        return None;
    };
    let mut end = prefix_len;
    let mut paren_depth = 0usize;
    while end < text.len() {
        let c = byte_at(text, end);
        if c == b'(' {
            paren_depth += 1;
            end += 1;
        } else if c == b')' {
            if paren_depth > 0 {
                paren_depth -= 1;
                end += 1;
            } else {
                break;
            }
        } else if is_url_termination_char(c) {
            break;
        } else {
            end += advance_one(text, end);
        }
        if end - prefix_len > MAX_LINK_URL_LENGTH {
            end = bounded_url_end(text, end, prefix_len);
            break;
        }
    }
    while end > 0
        && is_trimmed_autolink_punctuation(byte_at(text, end - 1))
        && !has_terminal_tld(&text[..end])
    {
        end -= 1;
    }
    if end <= prefix_len {
        return None;
    }
    let url = &text[..end];
    if (starts_with_app_protocol_url(url) && !looks_like_valid_app_protocol_url(url))
        || contains_nested_scheme_before_path(url, prefix_len)
        || contains_formatting_marker_before_path(url, prefix_len)
        || contains_unparenthesized_nested_http_url(url)
        || contains_angle_bracket_syntax(url)
        || contains_angle_email_like(url)
    {
        return None;
    }
    let escaped = matches!(byte_at(text, 0), b'"' | b'\'')
        || (end < text.len() && matches!(byte_at(text, end), b'"' | b'\''));
    Some(ParserResult {
        node: Node::Link {
            text: None,
            url: url.to_owned(),
            raw_url: url.to_owned(),
            source: url.to_owned(),
            escaped,
        },
        advance: url.len(),
    })
}

pub fn parse_autolink(text: &str, flags: u32) -> Option<ParserResult> {
    if !ParserFlags::has(flags, ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    if matches!(byte_at(text, 1), b'"' | b'\'') || !starts_with_url(&text[1..]) {
        return None;
    }
    let end = find_from(text, 1, b'>')?;
    let url = &text[1..end];
    if url.len() > MAX_LINK_URL_LENGTH {
        return None;
    }
    let prefix_len = if starts_with(url, "https://") {
        8
    } else if starts_with(url, "http://") {
        7
    } else if starts_with_app_protocol_url(url) {
        APP_PROTOCOL_SCHEME.len()
    } else {
        return None;
    };
    if starts_with_app_protocol_url(url) {
        if !looks_like_valid_app_protocol_url(url) {
            return None;
        }
    } else if !looks_like_valid_http_url_prefix(url, prefix_len) {
        return None;
    }
    Some(ParserResult {
        node: Node::Link {
            text: None,
            url: url.to_owned(),
            raw_url: url.to_owned(),
            source: text[..end + 1].to_owned(),
            escaped: true,
        },
        advance: end + 1,
    })
}

pub fn parse_email_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = find_from(text, 1, b'>')?;
    let content = &text[1..end];
    if starts_with(content, "http://")
        || starts_with(content, "https://")
        || content.is_empty()
        || byte_at(content, 0) == b'+'
    {
        return None;
    }
    if !is_valid_email(content) {
        return None;
    }
    let url = format!("mailto:{content}");
    Some(ParserResult {
        node: Node::Link {
            text: Some(Box::new(Node::Text {
                content: content.to_owned(),
            })),
            url: url.clone(),
            raw_url: url,
            source: text[..end + 1].to_owned(),
            escaped: true,
        },
        advance: end + 1,
    })
}

pub fn parse_phone_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = find_from(text, 1, b'>')?;
    let content = &text[1..end];
    if !is_valid_phone_number(content) {
        return None;
    }
    let normalized = normalize_phone_number(content);
    let url = format!("tel:{normalized}");
    Some(ParserResult {
        node: Node::Link {
            text: Some(Box::new(Node::Text {
                content: content.to_owned(),
            })),
            url: url.clone(),
            raw_url: url,
            source: text[..end + 1].to_owned(),
            escaped: true,
        },
        advance: end + 1,
    })
}

pub fn parse_sms_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
        || !starts_with(text, "<sms:")
    {
        return None;
    }
    let end = find_from(text, 1, b'>')?;
    let content = &text[1..end];
    let phone = &content[4..];
    if !is_valid_phone_number(phone) {
        return None;
    }
    let normalized = normalize_phone_number(phone);
    let url = format!("sms:{normalized}");
    Some(ParserResult {
        node: Node::Link {
            text: Some(Box::new(Node::Text {
                content: phone.to_owned(),
            })),
            url: url.clone(),
            raw_url: url,
            source: text[..end + 1].to_owned(),
            escaped: true,
        },
        advance: end + 1,
    })
}

pub fn starts_with_url(text: &str) -> bool {
    starts_with(text, "http://")
        || starts_with(text, "https://")
        || starts_with_app_protocol_url(text)
}

fn starts_with_app_protocol_url(text: &str) -> bool {
    if !starts_with(text, APP_PROTOCOL_SCHEME) || text.len() <= APP_PROTOCOL_SCHEME.len() {
        return false;
    }
    matches!(
        byte_at(text, APP_PROTOCOL_SCHEME.len()),
        b'/' | b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' | b'-'
    )
}

pub fn looks_like_valid_http_url_prefix(url: &str, prefix_len: usize) -> bool {
    looks_like_valid_http_url_prefix_with_userinfo(url, prefix_len, true)
}

fn looks_like_valid_http_url_prefix_with_userinfo(
    url: &str,
    prefix_len: usize,
    allow_userinfo: bool,
) -> bool {
    if url.len() <= prefix_len
        || contains_unparenthesized_nested_http_url(url)
        || contains_angle_bracket_syntax(url)
        || contains_angle_email_like(url)
    {
        return false;
    }
    let mut authority_end = prefix_len;
    while authority_end < url.len() {
        let char = byte_at(url, authority_end);
        if matches!(char, b'/' | b'?' | b'#') {
            break;
        }
        authority_end += 1;
    }
    let colon_before_path_slash = authority_end < url.len()
        && byte_at(url, authority_end) == b'/'
        && authority_end > prefix_len
        && byte_at(url, authority_end - 1) == b':';
    is_valid_http_authority(
        &url[prefix_len..authority_end],
        allow_userinfo,
        colon_before_path_slash,
    )
}

fn is_valid_http_authority(
    authority: &str,
    allow_userinfo: bool,
    colon_before_path_slash: bool,
) -> bool {
    if authority.is_empty() {
        return false;
    }
    let mut host_authority = authority;
    if let Some(userinfo_end) = authority.rfind('@') {
        if !allow_userinfo || userinfo_end + 1 >= authority.len() {
            return false;
        }
        host_authority = &authority[userinfo_end + 1..];
    }
    if byte_at(host_authority, 0) == b'[' {
        let Some(close) = find_from(host_authority, 0, b']') else {
            return false;
        };
        if !is_valid_bracketed_ip_literal(&host_authority[1..close]) {
            return false;
        }
        if close + 1 == host_authority.len() {
            return true;
        }
        if byte_at(host_authority, close + 1) != b':' || close + 2 >= host_authority.len() {
            return false;
        }
        return is_digit_only(&host_authority[close + 2..]);
    }
    let mut host = host_authority;
    if let Some(port_separator) = host_authority.rfind(':') {
        if port_separator + 1 >= host_authority.len() {
            if !colon_before_path_slash {
                return false;
            }
        } else if !is_digit_only(&host_authority[port_separator + 1..]) {
            return false;
        }
        host = &host_authority[..port_separator];
    }
    if host.is_empty() {
        return false;
    }
    for char in host.bytes() {
        match char {
            b'[' | b']' | b'"' | b'\'' | b'<' | b'>' | b'(' | b')' | b'\\' | b'|' => return false,
            b':' | b' ' | b'\t' | b'\r' | b'\n' => return false,
            _ => {}
        }
    }
    true
}

fn contains_angle_bracket_syntax(value: &str) -> bool {
    ["<:", "<a:", "<id:", "<@", "<#", "</", "<t:", "<+", "<sms:"]
        .iter()
        .any(|needle| value.contains(needle))
}

fn contains_unparenthesized_nested_http_url(url: &str) -> bool {
    for needle in ["<http://", "<https://"] {
        if let Some(pos) = url.find(needle)
            && !url[..pos].contains('(')
        {
            return true;
        }
    }
    false
}

fn contains_nested_scheme_before_path(url: &str, prefix_len: usize) -> bool {
    let rest = &url[prefix_len..];
    let first_path_delimiter = rest
        .bytes()
        .position(|char| matches!(char, b'/' | b'?' | b'#'))
        .unwrap_or(rest.len());
    for needle in ["http://", "https://"] {
        if let Some(pos) = rest.find(needle)
            && pos < first_path_delimiter
            && pos > 0
            && byte_at(rest, pos - 1) == b'('
        {
            return true;
        }
    }
    false
}

fn contains_formatting_marker_before_path(url: &str, prefix_len: usize) -> bool {
    let rest = &url[prefix_len..];
    let authority_end = rest
        .bytes()
        .position(|char| matches!(char, b'/' | b'?' | b'#'))
        .unwrap_or(rest.len());
    let authority = &rest[..authority_end];
    authority.contains("__")
        || authority.contains("**")
        || authority.contains("~~")
        || authority.contains("||")
}

fn is_valid_bracketed_ip_literal(value: &str) -> bool {
    !value.is_empty()
        && value.contains(':')
        && value
            .bytes()
            .all(|char| is_digit(char) || char.is_ascii_hexdigit() || matches!(char, b':' | b'.'))
}

pub fn is_valid_masked_link_url(url: &str) -> bool {
    if url.is_empty() || url.contains('"') {
        return false;
    }
    if starts_with(url, "https://") {
        return looks_like_valid_http_url_prefix_with_userinfo(url, 8, false);
    }
    if starts_with(url, "http://") {
        return looks_like_valid_http_url_prefix_with_userinfo(url, 7, false);
    }
    if starts_with(url, "mailto:") {
        return is_valid_email(&url["mailto:".len()..]);
    }
    if starts_with(url, "tel:") {
        return is_valid_phone_number(&url["tel:".len()..]);
    }
    if starts_with(url, "sms:") {
        return is_valid_phone_number(&url["sms:".len()..]);
    }
    if starts_with_app_protocol_url(url) {
        return looks_like_valid_app_protocol_url(url);
    }
    false
}

fn contains_angle_email_like(value: &str) -> bool {
    let mut offset = 0;
    while offset < value.len() {
        let Some(open_relative) = value[offset..].find('<') else {
            return false;
        };
        let open = offset + open_relative;
        let Some(close_relative) = value[open + 1..].find('>') else {
            return false;
        };
        let close = open + 1 + close_relative;
        let inner = &value[open + 1..close];
        if !inner.is_empty() && byte_at(inner, 0) != b'@' && inner.contains('@') {
            return true;
        }
        offset = close + 1;
    }
    false
}

pub fn should_treat_as_masked_link(link_text: &str, url: &str) -> bool {
    if starts_with_app_protocol_url(link_text) || starts_with_app_protocol_url(url) {
        return link_text != url;
    }
    let Some(left) = comparable_http_url(link_text) else {
        return true;
    };
    let Some(right) = comparable_http_url(url) else {
        return true;
    };
    !left.origin.eq_ignore_ascii_case(right.origin)
        || !equivalent_url_path(left.path, right.path)
        || left.search != right.search
        || left.hash != right.hash
}

fn comparable_http_url(value: &str) -> Option<ComparableUrl<'_>> {
    let prefix_len = if starts_with(value, "https://") {
        8
    } else if starts_with(value, "http://") {
        7
    } else {
        return None;
    };
    let mut cursor = prefix_len;
    while cursor < value.len() {
        let char = byte_at(value, cursor);
        if matches!(char, b'/' | b'?' | b'#') {
            break;
        }
        cursor += 1;
    }
    if cursor == prefix_len {
        return None;
    }
    let origin = &value[..cursor];
    let path_start = cursor;
    let mut search_start = value.len();
    let mut hash_start = value.len();
    while cursor < value.len() {
        let char = byte_at(value, cursor);
        if char == b'?' && search_start == value.len() && hash_start == value.len() {
            search_start = cursor;
        } else if char == b'#' {
            hash_start = cursor;
            break;
        }
        cursor += 1;
    }
    let path_end = search_start.min(hash_start);
    let search_end = hash_start;
    Some(ComparableUrl {
        origin,
        path: &value[path_start..path_end],
        search: if search_start == value.len() {
            ""
        } else {
            &value[search_start..search_end]
        },
        hash: if hash_start == value.len() {
            ""
        } else {
            &value[hash_start..]
        },
    })
}

fn equivalent_url_path(left: &str, right: &str) -> bool {
    let normalized_left = if left.is_empty() { "/" } else { left };
    let normalized_right = if right.is_empty() { "/" } else { right };
    normalized_left == normalized_right
}

fn looks_like_valid_app_protocol_url(url: &str) -> bool {
    if !starts_with_app_protocol_url(url) {
        return false;
    }
    url[APP_PROTOCOL_SCHEME.len()..].bytes().all(|char| {
        !matches!(
            char,
            b'"' | b'\'' | b'<' | b'>' | b'\\' | b'|' | b' ' | b'\t' | b'\r' | b'\n'
        )
    })
}

pub fn is_url_termination_char(char: u8) -> bool {
    matches!(char, b' ' | b'\t' | b'\n' | b'\r' | b')' | b'"')
}

pub fn is_word_underscore(text: &str, pos: usize) -> bool {
    if byte_at(text, pos) != b'_' {
        return false;
    }
    let prev = if pos > 0 { byte_at(text, pos - 1) } else { 0 };
    let next = byte_at(text, pos + 1);
    (is_alpha_numeric(prev) || prev == b'_') && (is_alpha_numeric(next) || next == b'_')
}

pub fn is_custom_emoji_name_char(char: u8) -> bool {
    is_alpha_numeric(char) || matches!(char, b'_' | b'-')
}

pub fn is_valid_emoji_name(name: &str) -> bool {
    !name.is_empty()
        && name
            .bytes()
            .all(|char| char >= 0x80 || is_alpha_numeric(char) || matches!(char, b'_' | b'-'))
}

fn is_valid_command_name(name: &str) -> bool {
    !name.is_empty()
        && name.len() <= 32
        && name.bytes().all(|char| {
            !char.is_ascii_uppercase()
                && (char >= 0x80 || is_alpha_numeric(char) || matches!(char, b'-' | b'_' | b'\''))
        })
}

fn contains_link_syntax(text: &str) -> bool {
    let mut search_offset = 0;
    while let Some(bracket) = text[search_offset..].find('[') {
        let bracket = search_offset + bracket;
        let after = &text[bracket + 1..];
        let Some(close) = after.find(']') else {
            return false;
        };
        let close_abs = bracket + 1 + close;
        if close_abs + 1 < text.len() && byte_at(text, close_abs + 1) == b'(' {
            return true;
        }
        search_offset = close_abs + 1;
    }
    false
}

fn is_slash_command_masked_link_text(text: &str) -> bool {
    starts_with(text, "</") && text.ends_with('>') && text.contains(':')
}

fn is_email_like(text: &str) -> bool {
    let Some(at) = text.find('@') else {
        return false;
    };
    if at == 0 || at + 1 >= text.len() || text[at + 1..].contains('@') {
        return false;
    }
    if text
        .bytes()
        .any(|char| matches!(char, b' ' | b'\t' | b'\r' | b'\n'))
    {
        return false;
    }
    text[at + 1..].contains('.')
}

pub fn is_valid_email(text: &str) -> bool {
    if !text.contains('@') || text.contains(' ') {
        return false;
    }
    let Some(at) = text.find('@') else {
        return false;
    };
    if at == 0 || at + 1 >= text.len() {
        return false;
    }
    for char in text[..at].bytes() {
        if !(is_alpha_numeric(char) || matches!(char, b'.' | b'_' | b'%' | b'+' | b'-')) {
            return false;
        }
    }
    let domain = &text[at + 1..];
    let Some(dot) = domain.rfind('.') else {
        return false;
    };
    dot > 0 && dot + 2 <= domain.len() && all_ascii_domain(domain)
}

fn all_ascii_domain(domain: &str) -> bool {
    domain
        .bytes()
        .all(|char| is_alpha_numeric(char) || matches!(char, b'-' | b'.' | b'[' | b']'))
}

pub fn is_valid_phone_number(text: &str) -> bool {
    if text.len() < 2 || byte_at(text, 0) != b'+' {
        return false;
    }
    let mut digit_count = 0usize;
    for char in text[1..].bytes() {
        if is_digit(char) {
            digit_count += 1;
        } else if !matches!(char, b' ' | b'-' | b'(' | b')') {
            return false;
        }
    }
    digit_count >= 6 && (b'1'..=b'9').contains(&byte_at(text, 1))
}

pub fn normalize_phone_number(text: &str) -> String {
    text.bytes()
        .filter(|char| !matches!(char, b' ' | b'-' | b'(' | b')'))
        .map(char::from)
        .collect()
}

fn is_trimmed_autolink_punctuation(char: u8) -> bool {
    matches!(char, b'.' | b',' | b';' | b':' | b'!' | b'?')
}

fn has_terminal_tld(text: &str) -> bool {
    if text.len() < 4 {
        return false;
    }
    let mut i = text.len();
    let mut letter_count = 0usize;
    while i > 0 {
        let char = byte_at(text, i - 1);
        if !char.is_ascii_alphabetic() {
            break;
        }
        letter_count += 1;
        i -= 1;
    }
    letter_count >= 2 && i > 0 && byte_at(text, i - 1) == b'.'
}

pub fn has_open_inline_code(text: &str) -> bool {
    if !text.contains('`') {
        return false;
    }
    let mut open_len: Option<usize> = None;
    let mut index = 0;
    while index < text.len() {
        if byte_at(text, index) != b'`' {
            index += 1;
            continue;
        }
        let mut run = 0usize;
        while index + run < text.len() && byte_at(text, index + run) == b'`' {
            run += 1;
        }
        if open_len.is_none() {
            open_len = Some(run);
        } else if open_len == Some(run) {
            open_len = None;
        }
        index += run;
    }
    open_len.is_some()
}

pub fn has_valid_code_fence_language(language: &str) -> bool {
    if is_whitespace(byte_at(language, 0)) {
        return false;
    }
    let trimmed_value = trim(language);
    if trimmed_value.is_empty() {
        return false;
    }
    let primary = trimmed_value.split([' ', '\t']).next().unwrap_or_default();
    if primary.is_empty() {
        return false;
    }
    if !primary.bytes().all(|char| {
        is_alpha_numeric(char) || matches!(char, b'_' | b'+' | b'.' | b'#' | b'/' | b'-')
    }) {
        return false;
    }
    primary.len() == trimmed_value.len() || is_known_multi_token_code_fence_language(primary)
}

fn special_shortcode_text(name: &str) -> Option<&'static str> {
    match name {
        "tm" => Some("™"),
        "copyright" => Some("©"),
        "registered" => Some("®"),
        _ => None,
    }
}

fn guild_navigation_type(value: &str) -> Option<GuildNavigationType> {
    match value {
        "customize" => Some(GuildNavigationType::Customize),
        "browse" => Some(GuildNavigationType::Browse),
        "guide" => Some(GuildNavigationType::Guide),
        "linked-roles" => Some(GuildNavigationType::LinkedRoles),
        _ => None,
    }
}

fn is_known_multi_token_code_fence_language(primary: &str) -> bool {
    KNOWN_MULTI_TOKEN_CODE_FENCE_LANGUAGES
        .iter()
        .any(|value| value.eq_ignore_ascii_case(primary))
}

const KNOWN_MULTI_TOKEN_CODE_FENCE_LANGUAGES: &[&str] = &[
    "ada",
    "agda",
    "ansi",
    "adoc",
    "ansible",
    "asciidoc",
    "asm",
    "awk",
    "bash",
    "bat",
    "batch",
    "c",
    "c#",
    "c++",
    "caddy",
    "capnp",
    "cedar",
    "cedarschema",
    "cfg",
    "cl",
    "clj",
    "cljc",
    "cljs",
    "clojure",
    "cmake",
    "cmd",
    "cobol",
    "commonlisp",
    "conf",
    "config",
    "cpp",
    "cs",
    "csharp",
    "css",
    "cts",
    "cxx",
    "d",
    "dart",
    "devicetree",
    "diff",
    "docker",
    "dockerfile",
    "dot",
    "dts",
    "dtsi",
    "ecmascript",
    "elisp",
    "elixir",
    "elm",
    "erlang",
    "erl",
    "ex",
    "exs",
    "f#",
    "fish",
    "fs",
    "fsharp",
    "fsi",
    "fsx",
    "gleam",
    "glsl",
    "go",
    "gql",
    "graphql",
    "graphqls",
    "groovy",
    "h",
    "haskell",
    "hh",
    "hcl",
    "hrl",
    "hs",
    "html",
    "htm",
    "hpp",
    "hxx",
    "idris",
    "ini",
    "j2",
    "java",
    "javascript",
    "jinja",
    "jinja2",
    "jq",
    "js",
    "json",
    "json5",
    "jsonc",
    "julia",
    "jl",
    "jsx",
    "katex",
    "kt",
    "kotlin",
    "kts",
    "ksh",
    "latex",
    "lean",
    "lisp",
    "log",
    "lua",
    "markdown",
    "matlab",
    "meson",
    "md",
    "mdown",
    "mjs",
    "mkdn",
    "ml",
    "mli",
    "mm",
    "mts",
    "nasm",
    "nginx",
    "ninja",
    "nix",
    "node",
    "obj-c",
    "objective-c",
    "objc",
    "ocaml",
    "patch",
    "pbtxt",
    "perl",
    "pgsq",
    "pgsql",
    "php",
    "plain",
    "plaintext",
    "pl",
    "plist",
    "pm",
    "postgres",
    "postgresql",
    "postscript",
    "proto",
    "protobuf",
    "ps",
    "ps1",
    "psd1",
    "psm1",
    "psql",
    "pwsh",
    "powershell",
    "prolog",
    "py",
    "py3",
    "python",
    "python3",
    "query",
    "r",
    "rb",
    "rego",
    "res",
    "rescript",
    "resi",
    "ron",
    "rq",
    "rs",
    "ruby",
    "rust",
    "scala",
    "scheme",
    "scss",
    "sh",
    "shell",
    "shellscript",
    "solidity",
    "sparql",
    "sql",
    "ss",
    "svg",
    "sv",
    "svh",
    "svelte",
    "swift",
    "terraform",
    "tex",
    "text",
    "textproto",
    "tf",
    "tfvars",
    "thrift",
    "tla",
    "tlaplus",
    "toml",
    "ts",
    "tsx",
    "txt",
    "typescript",
    "typst",
    "vb",
    "vbnet",
    "verilog",
    "vhdl",
    "vhd",
    "vim",
    "viml",
    "vimscript",
    "vue",
    "xml",
    "yaml",
    "yml",
    "zig",
    "zshell",
    "zsh",
];

pub fn can_open_single_emphasis(marker: u8, previous: u8, next: u8) -> bool {
    if !is_left_flanking_delimiter(previous, next) {
        return false;
    }
    marker != b'_'
        || !is_right_flanking_delimiter(previous, next)
        || crate::text::is_punctuation(previous)
}

pub fn can_close_single_emphasis(marker: u8, previous: u8, next: u8) -> bool {
    if !is_right_flanking_delimiter(previous, next) {
        return false;
    }
    marker != b'_'
        || !is_left_flanking_delimiter(previous, next)
        || crate::text::is_punctuation(next)
}

pub fn is_left_flanking_delimiter(previous: u8, next: u8) -> bool {
    if crate::text::is_whitespace(next) {
        return false;
    }
    !crate::text::is_punctuation(next)
        || crate::text::is_whitespace(previous)
        || crate::text::is_punctuation(previous)
}

pub fn is_right_flanking_delimiter(previous: u8, next: u8) -> bool {
    if crate::text::is_whitespace(previous) {
        return false;
    }
    !crate::text::is_punctuation(previous)
        || crate::text::is_whitespace(next)
        || crate::text::is_punctuation(next)
}

pub fn count_repeated_byte(text: &str, position: usize, byte: u8) -> usize {
    let mut count = 0;
    while position + count < text.len() && byte_at(text, position + count) == byte {
        count += 1;
    }
    count
}

pub fn max_inline_scan() -> usize {
    MAX_LINE_LENGTH
}

#[cfg(test)]
mod tests {
    use super::{extract_escaped_url, has_valid_code_fence_language};

    #[test]
    fn escaped_url_accepts_paren_immediately_after_destination() {
        let info = extract_escaped_url("https://example.com>) trailing", 0).expect("valid");
        assert_eq!(info.url, "https://example.com");
        assert!(info.escaped);
        assert_eq!(info.advance_by, "https://example.com>)".len());
    }

    #[test]
    fn escaped_url_rejects_content_between_destination_and_paren() {
        assert!(extract_escaped_url("https://example.com> and more)", 0).is_none());
        assert!(extract_escaped_url("https://example.com> )", 0).is_none());
        assert!(extract_escaped_url("https://example.com>text)", 0).is_none());
    }

    #[test]
    fn escaped_url_rejects_missing_paren() {
        assert!(extract_escaped_url("https://example.com>", 0).is_none());
        assert!(extract_escaped_url("https://example.com", 0).is_none());
    }

    #[test]
    fn escaped_url_keeps_paren_inside_destination() {
        let info = extract_escaped_url("https://example.com/a.yaml)>)", 0).expect("valid");
        assert_eq!(info.url, "https://example.com/a.yaml)");
    }

    #[test]
    fn rejects_leading_whitespace() {
        assert!(!has_valid_code_fence_language(" hello"));
        assert!(!has_valid_code_fence_language("\thello"));
        assert!(!has_valid_code_fence_language("   rust"));
        assert!(!has_valid_code_fence_language(" js code"));
    }

    #[test]
    fn rejects_empty_and_whitespace_only() {
        assert!(!has_valid_code_fence_language(""));
        assert!(!has_valid_code_fence_language(" "));
        assert!(!has_valid_code_fence_language("   "));
        assert!(!has_valid_code_fence_language("\t"));
    }

    #[test]
    fn accepts_single_token_languages() {
        assert!(has_valid_code_fence_language("rust"));
        assert!(has_valid_code_fence_language("js"));
        assert!(has_valid_code_fence_language("c#"));
        assert!(has_valid_code_fence_language("c++"));
        assert!(has_valid_code_fence_language("objective-c"));
        assert!(has_valid_code_fence_language("rust  "));
    }

    #[test]
    fn rejects_unknown_multi_token_info_strings() {
        assert!(!has_valid_code_fence_language("hello world"));
    }

    #[test]
    fn accepts_known_multi_token_languages() {
        assert!(has_valid_code_fence_language("js code"));
    }

    #[test]
    fn rejects_invalid_characters_in_primary_token() {
        assert!(!has_valid_code_fence_language("a!b"));
        assert!(!has_valid_code_fence_language("rust!"));
    }
}
