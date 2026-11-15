// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt::Write;

use crate::ast::{EmojiKind, ListItem, MentionKind, Node};

macro_rules! key {
    ($name:literal) => {
        concat!(",\"", $name, "\":")
    };
}

pub fn write_ast_json(nodes: &[Node], capacity: usize) -> String {
    let mut out = String::with_capacity(capacity);
    out.push_str("{\"nodes\":");
    write_nodes(&mut out, nodes);
    out.push('}');
    out
}

fn write_nodes(out: &mut String, nodes: &[Node]) {
    out.push('[');
    for (index, node) in nodes.iter().enumerate() {
        if index > 0 {
            out.push(',');
        }
        write_node(out, node);
    }
    out.push(']');
}

fn write_node(out: &mut String, node: &Node) {
    out.push_str("{\"type\":\"");
    out.push_str(node.tag_name());
    out.push('"');
    match node {
        Node::Text { content } | Node::InlineCode { content } => {
            out.push_str(key!("content"));
            write_str(out, content);
        }
        Node::Blockquote {
            children,
            blank_lines,
        } => {
            write_children(out, children);
            if let Some(blank_lines) = blank_lines {
                out.push_str(key!("blankLines"));
                write_number(out, *blank_lines as u64);
            }
        }
        Node::Strong { children }
        | Node::Emphasis { children }
        | Node::Underline { children }
        | Node::Strikethrough { children }
        | Node::Subtext { children }
        | Node::Sequence { children } => write_children(out, children),
        Node::Spoiler { children, is_block } => {
            write_children(out, children);
            if let Some(is_block) = is_block {
                out.push_str(key!("isBlock"));
                write_bool(out, *is_block);
            }
        }
        Node::Heading { level, children } => {
            out.push_str(key!("level"));
            write_number(out, u64::from(*level));
            write_children(out, children);
        }
        Node::List { ordered, items } => {
            out.push_str(key!("ordered"));
            write_bool(out, *ordered);
            out.push_str(key!("items"));
            out.push('[');
            for (index, ListItem { children, ordinal }) in items.iter().enumerate() {
                if index > 0 {
                    out.push(',');
                }
                out.push_str("{\"children\":");
                write_nodes(out, children);
                if let Some(ordinal) = ordinal {
                    out.push_str(key!("ordinal"));
                    write_number(out, *ordinal as u64);
                }
                out.push('}');
            }
            out.push(']');
        }
        Node::CodeBlock { language, content } => {
            if let Some(language) = language {
                out.push_str(key!("language"));
                write_str(out, language);
            }
            out.push_str(key!("content"));
            write_str(out, content);
        }
        Node::Link {
            text,
            url,
            escaped,
            raw_url,
            source,
        } => {
            if let Some(text) = text {
                out.push_str(key!("text"));
                write_node(out, text);
            }
            out.push_str(key!("url"));
            write_str(out, url);
            out.push_str(key!("escaped"));
            write_bool(out, *escaped);
            out.push_str(key!("rawUrl"));
            write_str(out, raw_url);
            out.push_str(key!("source"));
            write_str(out, source);
        }
        Node::Mention { kind } => {
            out.push_str(key!("kind"));
            write_mention_kind(out, kind);
        }
        Node::Timestamp { timestamp, style } => {
            out.push_str(key!("timestamp"));
            write_number(out, *timestamp);
            out.push_str(key!("style"));
            let _ = write!(out, "\"{style:?}\"");
        }
        Node::Emoji { kind } => {
            out.push_str(key!("kind"));
            write_emoji_kind(out, kind);
        }
        Node::Table {
            header,
            alignments,
            rows,
        } => {
            out.push_str(key!("header"));
            write_node(out, header);
            out.push_str(key!("alignments"));
            out.push('[');
            for (index, alignment) in alignments.iter().enumerate() {
                if index > 0 {
                    out.push(',');
                }
                let _ = write!(out, "\"{alignment:?}\"");
            }
            out.push(']');
            out.push_str(key!("rows"));
            write_nodes(out, rows);
        }
        Node::TableRow { cells } => {
            out.push_str(key!("cells"));
            write_nodes(out, cells);
        }
        Node::TableCell { children } => write_children(out, children),
        Node::Alert {
            alert_type,
            children,
        } => {
            out.push_str(key!("alertType"));
            let _ = write!(out, "\"{alert_type:?}\"");
            write_children(out, children);
        }
    }
    out.push('}');
}

fn write_mention_kind(out: &mut String, kind: &MentionKind) {
    match kind {
        MentionKind::User { id } => write_tagged_id(out, "User", id),
        MentionKind::Channel { id } => write_tagged_id(out, "Channel", id),
        MentionKind::Role { id } => write_tagged_id(out, "Role", id),
        MentionKind::Command {
            name,
            subcommand_group,
            subcommand,
            id,
        } => {
            out.push_str("{\"kind\":\"Command\"");
            out.push_str(key!("name"));
            write_str(out, name);
            if let Some(subcommand_group) = subcommand_group {
                out.push_str(key!("subcommandGroup"));
                write_str(out, subcommand_group);
            }
            if let Some(subcommand) = subcommand {
                out.push_str(key!("subcommand"));
                write_str(out, subcommand);
            }
            out.push_str(key!("id"));
            write_str(out, id);
            out.push('}');
        }
        MentionKind::GuildNavigation {
            navigation_type,
            id,
        } => {
            out.push_str("{\"kind\":\"GuildNavigation\"");
            out.push_str(key!("navigationType"));
            let _ = write!(out, "\"{navigation_type:?}\"");
            if let Some(id) = id {
                out.push_str(key!("id"));
                write_str(out, id);
            }
            out.push('}');
        }
        MentionKind::Everyone => out.push_str("{\"kind\":\"Everyone\"}"),
        MentionKind::Here => out.push_str("{\"kind\":\"Here\"}"),
    }
}

fn write_tagged_id(out: &mut String, kind: &str, id: &str) {
    out.push_str("{\"kind\":\"");
    out.push_str(kind);
    out.push('"');
    out.push_str(key!("id"));
    write_str(out, id);
    out.push('}');
}

fn write_emoji_kind(out: &mut String, kind: &EmojiKind) {
    match kind {
        EmojiKind::Standard {
            raw,
            codepoints,
            name,
        } => {
            out.push_str("{\"kind\":\"Standard\"");
            out.push_str(key!("raw"));
            write_str(out, raw);
            out.push_str(key!("codepoints"));
            write_str(out, codepoints);
            out.push_str(key!("name"));
            write_str(out, name);
        }
        EmojiKind::Custom { name, id, animated } => {
            out.push_str("{\"kind\":\"Custom\"");
            out.push_str(key!("name"));
            write_str(out, name);
            out.push_str(key!("id"));
            write_str(out, id);
            out.push_str(key!("animated"));
            write_bool(out, *animated);
        }
    }
    out.push('}');
}

fn write_children(out: &mut String, children: &[Node]) {
    out.push_str(key!("children"));
    write_nodes(out, children);
}

fn write_bool(out: &mut String, value: bool) {
    out.push_str(if value { "true" } else { "false" });
}

fn write_number(out: &mut String, value: u64) {
    let _ = write!(out, "{value}");
}

fn write_str(out: &mut String, value: &str) {
    out.push('"');
    let bytes = value.as_bytes();
    let mut start = 0;
    let mut index = next_escape(bytes, 0);
    while index < bytes.len() {
        out.push_str(&value[start..index]);
        let byte = bytes[index];
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            0x08 => out.push_str("\\b"),
            0x0c => out.push_str("\\f"),
            _ => {
                out.push_str("\\u00");
                out.push(char::from(HEX_DIGITS[usize::from(byte >> 4)]));
                out.push(char::from(HEX_DIGITS[usize::from(byte & 0xf)]));
            }
        }
        start = index + 1;
        index = next_escape(bytes, start);
    }
    out.push_str(&value[start..]);
    out.push('"');
}

fn next_escape(bytes: &[u8], from: usize) -> usize {
    const LOW_BITS: u64 = u64::from_ne_bytes([0x01; 8]);
    const HIGH_BITS: u64 = u64::from_ne_bytes([0x80; 8]);
    let has_zero = |word: u64| word.wrapping_sub(LOW_BITS) & !word & HIGH_BITS != 0;
    let mut index = from;
    while let Some(chunk) = bytes.get(index..index + 8) {
        let word = u64::from_ne_bytes(chunk.try_into().unwrap_or_default());
        if word.wrapping_sub(LOW_BITS * 0x20) & !word & HIGH_BITS != 0
            || has_zero(word ^ (LOW_BITS * u64::from(b'"')))
            || has_zero(word ^ (LOW_BITS * u64::from(b'\\')))
        {
            break;
        }
        index += 8;
    }
    bytes[index..]
        .iter()
        .position(|byte| *byte < 0x20 || matches!(byte, b'"' | b'\\'))
        .map_or(bytes.len(), |offset| index + offset)
}

const HEX_DIGITS: &[u8; 16] = b"0123456789abcdef";
