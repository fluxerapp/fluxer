// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{
    AlertType, EmojiKind, GuildNavigationType, ListItem, MentionKind, Node, TableAlignment,
    TimestampStyle,
};

pub const FORMAT_VERSION: u8 = 1;

pub fn write_ast_binary(nodes: &[Node]) -> Vec<u8> {
    let mut out = Vec::with_capacity(256);
    out.push(FORMAT_VERSION);
    write_nodes(&mut out, nodes);
    out
}

fn write_nodes(out: &mut Vec<u8>, nodes: &[Node]) {
    write_varint(out, nodes.len() as u64);
    for node in nodes {
        write_node(out, node);
    }
}

fn write_node(out: &mut Vec<u8>, node: &Node) {
    match node {
        Node::Text { content } => {
            out.push(0);
            write_str(out, content);
        }
        Node::Blockquote {
            children,
            blank_lines,
        } => {
            out.push(1);
            write_optional_varint(out, blank_lines.map(|value| value as u64));
            write_nodes(out, children);
        }
        Node::Strong { children } => {
            out.push(2);
            write_nodes(out, children);
        }
        Node::Emphasis { children } => {
            out.push(3);
            write_nodes(out, children);
        }
        Node::Underline { children } => {
            out.push(4);
            write_nodes(out, children);
        }
        Node::Strikethrough { children } => {
            out.push(5);
            write_nodes(out, children);
        }
        Node::Spoiler { children, is_block } => {
            out.push(6);
            out.push(match is_block {
                None => 0,
                Some(false) => 1,
                Some(true) => 2,
            });
            write_nodes(out, children);
        }
        Node::Heading { level, children } => {
            out.push(7);
            out.push(*level);
            write_nodes(out, children);
        }
        Node::Subtext { children } => {
            out.push(8);
            write_nodes(out, children);
        }
        Node::List { ordered, items } => {
            out.push(9);
            out.push(u8::from(*ordered));
            write_varint(out, items.len() as u64);
            for ListItem { children, ordinal } in items {
                write_optional_varint(out, ordinal.map(|value| value as u64));
                write_nodes(out, children);
            }
        }
        Node::CodeBlock { language, content } => {
            out.push(10);
            write_optional_str(out, language.as_deref());
            write_str(out, content);
        }
        Node::InlineCode { content } => {
            out.push(11);
            write_str(out, content);
        }
        Node::Sequence { children } => {
            out.push(12);
            write_nodes(out, children);
        }
        Node::Link {
            text,
            url,
            escaped,
            raw_url,
            source,
        } => {
            out.push(13);
            out.push(u8::from(*escaped) | (u8::from(text.is_some()) << 1));
            write_str(out, url);
            write_str(out, raw_url);
            write_str(out, source);
            if let Some(text) = text {
                write_node(out, text);
            }
        }
        Node::Mention { kind } => {
            out.push(14);
            write_mention(out, kind);
        }
        Node::Timestamp { timestamp, style } => {
            out.push(15);
            write_varint(out, *timestamp);
            out.push(timestamp_style_code(*style));
        }
        Node::Emoji { kind } => {
            out.push(16);
            write_emoji(out, kind);
        }
        Node::Table {
            header,
            alignments,
            rows,
        } => {
            out.push(17);
            write_node(out, header);
            write_varint(out, alignments.len() as u64);
            for alignment in alignments {
                out.push(alignment_code(*alignment));
            }
            write_nodes(out, rows);
        }
        Node::TableRow { cells } => {
            out.push(18);
            write_nodes(out, cells);
        }
        Node::TableCell { children } => {
            out.push(19);
            write_nodes(out, children);
        }
        Node::Alert {
            alert_type,
            children,
        } => {
            out.push(20);
            out.push(alert_code(*alert_type));
            write_nodes(out, children);
        }
    }
}

fn write_mention(out: &mut Vec<u8>, kind: &MentionKind) {
    match kind {
        MentionKind::User { id } => {
            out.push(0);
            write_str(out, id);
        }
        MentionKind::Channel { id } => {
            out.push(1);
            write_str(out, id);
        }
        MentionKind::Role { id } => {
            out.push(2);
            write_str(out, id);
        }
        MentionKind::Command {
            name,
            subcommand_group,
            subcommand,
            id,
        } => {
            out.push(3);
            write_str(out, name);
            write_optional_str(out, subcommand_group.as_deref());
            write_optional_str(out, subcommand.as_deref());
            write_str(out, id);
        }
        MentionKind::GuildNavigation {
            navigation_type,
            id,
        } => {
            out.push(4);
            out.push(match navigation_type {
                GuildNavigationType::Customize => 0,
                GuildNavigationType::Browse => 1,
                GuildNavigationType::Guide => 2,
                GuildNavigationType::LinkedRoles => 3,
            });
            write_optional_str(out, id.as_deref());
        }
        MentionKind::Everyone => out.push(5),
        MentionKind::Here => out.push(6),
    }
}

fn write_emoji(out: &mut Vec<u8>, kind: &EmojiKind) {
    match kind {
        EmojiKind::Standard {
            raw,
            codepoints,
            name,
        } => {
            out.push(0);
            write_str(out, raw);
            write_str(out, codepoints);
            write_str(out, name);
        }
        EmojiKind::Custom { name, id, animated } => {
            out.push(1);
            out.push(u8::from(*animated));
            write_str(out, name);
            write_str(out, id);
        }
    }
}

fn timestamp_style_code(style: TimestampStyle) -> u8 {
    match style {
        TimestampStyle::ShortTime => 0,
        TimestampStyle::LongTime => 1,
        TimestampStyle::ShortDate => 2,
        TimestampStyle::LongDate => 3,
        TimestampStyle::ShortDateTime => 4,
        TimestampStyle::LongDateTime => 5,
        TimestampStyle::ShortDateShortTime => 6,
        TimestampStyle::ShortDateMediumTime => 7,
        TimestampStyle::RelativeTime => 8,
    }
}

fn alignment_code(alignment: TableAlignment) -> u8 {
    match alignment {
        TableAlignment::Left => 0,
        TableAlignment::Center => 1,
        TableAlignment::Right => 2,
        TableAlignment::None => 3,
    }
}

fn alert_code(alert_type: AlertType) -> u8 {
    match alert_type {
        AlertType::Note => 0,
        AlertType::Tip => 1,
        AlertType::Important => 2,
        AlertType::Warning => 3,
        AlertType::Caution => 4,
    }
}

fn write_varint(out: &mut Vec<u8>, mut value: u64) {
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

fn write_optional_varint(out: &mut Vec<u8>, value: Option<u64>) {
    match value {
        None => out.push(0),
        Some(value) => {
            out.push(1);
            write_varint(out, value);
        }
    }
}

fn write_str(out: &mut Vec<u8>, value: &str) {
    write_varint(out, value.len() as u64);
    out.extend_from_slice(value.as_bytes());
}

fn write_optional_str(out: &mut Vec<u8>, value: Option<&str>) {
    match value {
        None => out.push(0),
        Some(value) => {
            out.push(1);
            write_str(out, value);
        }
    }
}
