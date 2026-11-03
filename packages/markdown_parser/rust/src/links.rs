// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{
    EmojiKind, GuildNavigationType, MentionKind, Node, ParserFlags, ParserResult, TimestampStyle,
};
use crate::constants::MAX_LINK_URL_LENGTH;
use crate::parser::{MarkdownParser, ParseError};
use crate::scan::{ANGLE_SYNTAX_PREFIXES, Lookahead, Pattern, http_scheme_len};
use crate::text::{
    bounded_utf8_prefix_length, byte_at, find_from, has_visible_content, is_alpha_numeric,
    is_digit, is_digit_only, is_whitespace, starts_with, trim,
};

const APP_PROTOCOL_SCHEME: &str = "fluxer:";
const MAX_COMMAND_NAME_LENGTH: usize = 32;
const MAX_COMMAND_SEGMENTS: usize = 3;
const MAX_COMMAND_COLON_INDEX: usize = MAX_COMMAND_SEGMENTS * (MAX_COMMAND_NAME_LENGTH + 1);

#[derive(Clone, Debug)]
struct UrlInfo<'a> {
    url: &'a str,
    start: usize,
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

pub(crate) fn parse_custom_emoji(text: &str) -> Option<ParserResult> {
    if !(starts_with(text, "<:") || starts_with(text, "<a:")) {
        return None;
    }
    let end = text.len() - 1;
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

pub(crate) fn parse_timestamp(text: &str) -> Option<ParserResult> {
    if !starts_with(text, "<t:") {
        return None;
    }
    let end = text.len() - 1;
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

pub(crate) fn parse_mention(text: &str, flags: u32) -> Option<ParserResult> {
    if text.len() < 2 || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = text.len() - 1;
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
            let colon = inner
                .bytes()
                .take(MAX_COMMAND_COLON_INDEX + 1)
                .position(|byte| byte == b':')?;
            let id = &inner[colon + 1..];
            if !is_digit_only(id) {
                return None;
            }
            let command = &inner[1..colon];
            let mut segments = Vec::new();
            for segment in command.split(' ') {
                if segment.is_empty()
                    || !is_valid_command_name(segment)
                    || segments.len() == MAX_COMMAND_SEGMENTS
                {
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
                    subcommand_group: (segments.len() == MAX_COMMAND_SEGMENTS)
                        .then(|| segments[1].to_owned()),
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

pub(crate) fn parse_masked_link(
    parser: &mut MarkdownParser,
    scan: &mut Lookahead<'_>,
    position: usize,
    base_offset: usize,
) -> Result<Option<ParserResult>, ParseError> {
    let text = &scan.text[position..];
    if text.is_empty() || byte_at(text, 0) != b'[' {
        return Ok(None);
    }
    let Some(bracket_position) = scan
        .closing_bracket(position)
        .map(|closing| closing - position)
        .filter(|distance| *distance <= MAX_LINK_URL_LENGTH)
    else {
        return Ok(None);
    };
    let link_text = &text[1..bracket_position];
    if scan.has_link_syntax(position + 1, position + bracket_position) {
        if bracket_position + 1 < text.len()
            && byte_at(text, bracket_position + 1) == b'('
            && let Some(url_info) = extract_url(scan, position, bracket_position + 2)
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
                content: text[..bracket_position + 1].to_owned(),
            },
            advance: bracket_position + 1,
        }));
    }
    if bracket_position + 1 >= text.len() || byte_at(text, bracket_position + 1) != b'(' {
        return Ok(None);
    }
    let trimmed_link_text = trim(link_text);
    let Some(url_info) = extract_url(scan, position, bracket_position + 2) else {
        if trimmed_link_text.find('>') == Some(trimmed_link_text.len().wrapping_sub(1))
            && parse_mention(trimmed_link_text, parser.flags()).is_some()
        {
            return Ok(None);
        }
        return Ok(Some(ParserResult {
            node: Node::Text {
                content: text[..bracket_position + 1].to_owned(),
            },
            advance: bracket_position + 1,
        }));
    };
    if !has_visible_content(link_text)
        || is_slash_command_masked_link_text(trimmed_link_text)
        || is_email_like(trimmed_link_text)
        || url_info.url.len() > MAX_LINK_URL_LENGTH
        || !is_valid_masked_link_url(scan, position + url_info.start, url_info.url.len())
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

fn extract_url<'a>(scan: &mut Lookahead<'a>, position: usize, start: usize) -> Option<UrlInfo<'a>> {
    let text = &scan.text[position..];
    if start >= text.len() {
        return None;
    }
    if byte_at(text, start) == b'<' {
        let close =
            scan.next(Pattern::AngleClose, position + start + 1, scan.text.len())? - position;
        if close + 1 >= text.len() || byte_at(text, close + 1) != b')' {
            return None;
        }
        return Some(UrlInfo {
            url: &text[start + 1..close],
            start: start + 1,
            escaped: true,
            advance_by: close + 2,
        });
    }
    let close = scan.closing_paren(position + start)? - position;
    Some(UrlInfo {
        url: &text[start..close],
        start,
        escaped: false,
        advance_by: close + 1,
    })
}

pub(crate) fn parse_url_segment(
    scan: &mut Lookahead<'_>,
    position: usize,
    flags: u32,
) -> Option<ParserResult> {
    if !ParserFlags::has(flags, ParserFlags::ALLOW_AUTOLINKS) {
        return None;
    }
    let text = &scan.text[position..];
    let prefix_len = if starts_with(text, "https://") {
        8
    } else if starts_with(text, "http://") {
        7
    } else if starts_with_app_protocol_url(text) {
        APP_PROTOCOL_SCHEME.len()
    } else {
        return None;
    };
    let stop = scan.url_stop(position + prefix_len) - position;
    let mut end = if stop - prefix_len <= MAX_LINK_URL_LENGTH {
        stop
    } else {
        bounded_utf8_prefix_length(text, prefix_len + MAX_LINK_URL_LENGTH)
    };
    while end > 0
        && is_trimmed_autolink_punctuation(byte_at(text, end - 1))
        && !has_terminal_tld(&text[..end])
    {
        end -= 1;
    }
    if end <= prefix_len {
        return None;
    }
    let url_end = position + end;
    if (starts_with_app_protocol_url(&text[..end])
        && !is_valid_app_protocol_url(scan, position, url_end))
        || contains_nested_scheme_before_path(scan, position + prefix_len, url_end)
        || contains_formatting_marker_before_path(scan, position + prefix_len, url_end)
        || contains_angle_bracket_markup(scan, position, url_end)
    {
        return None;
    }
    let text = &scan.text[position..];
    let url = &text[..end];
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

pub(crate) fn parse_autolink(
    scan: &mut Lookahead<'_>,
    position: usize,
    close: usize,
    flags: u32,
) -> Option<ParserResult> {
    let text = &scan.text[position..=close];
    if !ParserFlags::has(flags, ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    if matches!(byte_at(text, 1), b'"' | b'\'') || !starts_with_url(&text[1..]) {
        return None;
    }
    let end = text.len() - 1;
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
        if !is_valid_app_protocol_url(scan, position + 1, close) {
            return None;
        }
    } else if !looks_like_valid_http_url_prefix(scan, position + 1, close, prefix_len, true) {
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

pub(crate) fn parse_email_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = text.len() - 1;
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

pub(crate) fn parse_phone_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS) || byte_at(text, 0) != b'<' {
        return None;
    }
    let end = text.len() - 1;
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

pub(crate) fn parse_sms_link(parser: &MarkdownParser, text: &str) -> Option<ParserResult> {
    if !ParserFlags::has(parser.flags(), ParserFlags::ALLOW_AUTOLINKS)
        || !starts_with(text, "<sms:")
    {
        return None;
    }
    let end = text.len() - 1;
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

fn looks_like_valid_http_url_prefix(
    scan: &mut Lookahead<'_>,
    start: usize,
    end: usize,
    prefix_len: usize,
    allow_userinfo: bool,
) -> bool {
    if end - start <= prefix_len || contains_angle_bracket_markup(scan, start, end) {
        return false;
    }
    let url = &scan.text[start..end];
    let authority_end = path_start(scan, start + prefix_len, end) - start;
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

fn path_start(scan: &mut Lookahead<'_>, from: usize, end: usize) -> usize {
    scan.next(Pattern::PathDelimiter, from, end).unwrap_or(end)
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

fn contains_angle_bracket_markup(scan: &mut Lookahead<'_>, start: usize, end: usize) -> bool {
    scan.next(Pattern::AngleOpen, start, end).is_some()
        && (contains_unparenthesized_nested_http_url(scan, start, end)
            || contains_angle_bracket_syntax(scan, start, end)
            || scan.has_angle_email(start, end))
}

fn contains_angle_bracket_syntax(scan: &mut Lookahead<'_>, start: usize, end: usize) -> bool {
    let mut from = start;
    while let Some(position) = scan.next(Pattern::AngleSyntax, from, end) {
        let window = &scan.text.as_bytes()[position..end];
        if ANGLE_SYNTAX_PREFIXES
            .iter()
            .any(|prefix| window.starts_with(prefix.as_bytes()))
        {
            return true;
        }
        from = position + 1;
    }
    false
}

fn contains_unparenthesized_nested_http_url(
    scan: &mut Lookahead<'_>,
    start: usize,
    end: usize,
) -> bool {
    let bytes = scan.text.as_bytes();
    let Some(position) = scan
        .next(Pattern::NestedHttpUrl, start, end)
        .filter(|position| {
            http_scheme_len(&bytes[position + 1..]).is_some_and(|len| position + 1 + len <= end)
        })
    else {
        return false;
    };
    scan.next(Pattern::OpenParen, start, position).is_none()
}

fn contains_nested_scheme_before_path(
    scan: &mut Lookahead<'_>,
    rest_start: usize,
    end: usize,
) -> bool {
    let bytes = scan.text.as_bytes();
    let path = path_start(scan, rest_start, end);
    let mut checked = [false; 2];
    let mut from = rest_start;
    while !(checked[0] && checked[1])
        && let Some(position) = scan.next(Pattern::HttpScheme, from, path)
    {
        let len = http_scheme_len(&bytes[position..]).unwrap_or_default();
        let kind = usize::from(len == 8);
        if !checked[kind] {
            checked[kind] = true;
            if position + len <= end && position > rest_start && bytes[position - 1] == b'(' {
                return true;
            }
        }
        from = position + 1;
    }
    false
}

fn contains_formatting_marker_before_path(
    scan: &mut Lookahead<'_>,
    rest_start: usize,
    end: usize,
) -> bool {
    let path = path_start(scan, rest_start, end);
    scan.next(Pattern::DoubledMarker, rest_start, path)
        .is_some_and(|position| position + 2 <= path)
}

fn is_valid_bracketed_ip_literal(value: &str) -> bool {
    !value.is_empty()
        && value.contains(':')
        && value
            .bytes()
            .all(|char| is_digit(char) || char.is_ascii_hexdigit() || matches!(char, b':' | b'.'))
}

fn is_valid_masked_link_url(scan: &mut Lookahead<'_>, start: usize, len: usize) -> bool {
    let url = &scan.text[start..start + len];
    if url.is_empty() || url.contains('"') {
        return false;
    }
    if starts_with(url, "https://") {
        return looks_like_valid_http_url_prefix(scan, start, start + len, 8, false);
    }
    if starts_with(url, "http://") {
        return looks_like_valid_http_url_prefix(scan, start, start + len, 7, false);
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
    starts_with_app_protocol_url(url) && is_valid_app_protocol_url(scan, start, start + len)
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

fn is_valid_app_protocol_url(scan: &mut Lookahead<'_>, start: usize, end: usize) -> bool {
    scan.next(
        Pattern::AppProtocolInvalid,
        start + APP_PROTOCOL_SCHEME.len(),
        end,
    )
    .is_none()
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
        && name.len() <= MAX_COMMAND_NAME_LENGTH
        && name.bytes().all(|char| {
            !char.is_ascii_uppercase()
                && (char >= 0x80 || is_alpha_numeric(char) || matches!(char, b'-' | b'_' | b'\''))
        })
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
    let Some(at) = text
        .bytes()
        .position(|char| {
            !(is_alpha_numeric(char) || matches!(char, b'.' | b'_' | b'%' | b'+' | b'-'))
        })
        .filter(|at| byte_at(text, *at) == b'@')
    else {
        return false;
    };
    if at == 0 || at + 1 >= text.len() {
        return false;
    }
    let domain = &text[at + 1..];
    all_ascii_domain(domain)
        && domain
            .rfind('.')
            .is_some_and(|dot| dot > 0 && dot + 2 <= domain.len())
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
