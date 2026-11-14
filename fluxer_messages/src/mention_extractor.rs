// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::ast::{ListItem, MentionKind};
use fluxer_markdown_parser::{EmojiContext, MarkdownParser, Node, ParserFlags};
use linkify::{LinkFinder, LinkKind};
use std::borrow::Cow;
use std::collections::HashSet;
use std::sync::OnceLock;

#[derive(Clone, Debug, Default, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct MessageMentions {
    pub users: HashSet<i64>,
    pub roles: HashSet<i64>,
    pub channels: HashSet<i64>,
    #[serde(default)]
    pub everyone: bool,
    #[serde(default)]
    pub here: bool,
}

impl MessageMentions {
    pub fn is_empty(&self) -> bool {
        self.users.is_empty()
            && self.roles.is_empty()
            && self.channels.is_empty()
            && !self.everyone
            && !self.here
    }
}

pub fn extract_mentions_from_markdown(input: Option<&str>) -> MessageMentions {
    let Some(input) = input else {
        return MessageMentions::default();
    };
    if input.is_empty() || !may_contain_mention(input) {
        return MessageMentions::default();
    }
    parse_mentions(input)
}

fn may_contain_mention(input: &str) -> bool {
    let bytes = input.as_bytes();
    bytes.iter().enumerate().any(|(index, byte)| match byte {
        b'<' => matches!(bytes.get(index + 1), Some(b'@' | b'#')),
        b'@' => {
            let rest = &bytes[index..];
            rest.starts_with(b"@everyone") || rest.starts_with(b"@here")
        }
        _ => false,
    })
}

fn parse_mentions(input: &str) -> MessageMentions {
    let cleaned = blank_raw_urls(input);
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
    let Ok(nodes) = parser.parse(cleaned.as_ref()) else {
        return MessageMentions::default();
    };
    let mut mentions = MessageMentions::default();
    collect_mentions_from_nodes(&nodes, &mut mentions);
    mentions
}

pub fn extend_mentions_from_markdown(input: Option<&str>, mentions: &mut MessageMentions) {
    let extracted = extract_mentions_from_markdown(input);
    mentions.users.extend(extracted.users);
    mentions.roles.extend(extracted.roles);
    mentions.channels.extend(extracted.channels);
    mentions.everyone = mentions.everyone || extracted.everyone;
    mentions.here = mentions.here || extracted.here;
}

fn collect_mentions_from_nodes(nodes: &[Node], mentions: &mut MessageMentions) {
    for node in nodes {
        collect_mentions_from_node(node, mentions);
    }
}

fn collect_mentions_from_list_items(items: &[ListItem], mentions: &mut MessageMentions) {
    for item in items {
        collect_mentions_from_nodes(&item.children, mentions);
    }
}

fn collect_mentions_from_node(node: &Node, mentions: &mut MessageMentions) {
    match node {
        Node::Mention { kind } => collect_mention_kind(kind, mentions),
        Node::Blockquote { children, .. }
        | Node::Strong { children }
        | Node::Emphasis { children }
        | Node::Underline { children }
        | Node::Strikethrough { children }
        | Node::Heading { children, .. }
        | Node::Subtext { children }
        | Node::Sequence { children }
        | Node::TableCell { children }
        | Node::Alert { children, .. } => collect_mentions_from_nodes(children, mentions),
        Node::Spoiler { children, .. } => collect_mentions_from_nodes(children, mentions),
        Node::List { items, .. } => collect_mentions_from_list_items(items, mentions),
        Node::Link { text, .. } => {
            if let Some(text) = text {
                collect_mentions_from_node(text, mentions);
            }
        }
        Node::Table { header, rows, .. } => {
            collect_mentions_from_node(header, mentions);
            collect_mentions_from_nodes(rows, mentions);
        }
        Node::TableRow { cells } => collect_mentions_from_nodes(cells, mentions),
        Node::Text { .. }
        | Node::CodeBlock { .. }
        | Node::InlineCode { .. }
        | Node::Timestamp { .. }
        | Node::Emoji { .. } => {}
    }
}

fn collect_mention_kind(kind: &MentionKind, mentions: &mut MessageMentions) {
    match kind {
        MentionKind::User { id } => insert_id(id, &mut mentions.users),
        MentionKind::Role { id } => insert_id(id, &mut mentions.roles),
        MentionKind::Channel { id } => insert_id(id, &mut mentions.channels),
        MentionKind::Everyone => mentions.everyone = true,
        MentionKind::Here => mentions.here = true,
        MentionKind::Command { .. } | MentionKind::GuildNavigation { .. } => {}
    }
}

fn insert_id(id: &str, target: &mut HashSet<i64>) {
    if let Ok(id) = id.parse::<i64>()
        && id != 0
    {
        target.insert(id);
    }
}

fn blank_raw_urls(input: &str) -> Cow<'_, str> {
    let mut output = None::<String>;
    for link in url_finder().links(input) {
        if link.kind() != &LinkKind::Url {
            continue;
        }
        let end = raw_url_token_end(input, link.end());
        let output = output.get_or_insert_with(|| input.to_owned());
        output.replace_range(link.start()..end, &" ".repeat(end - link.start()));
    }
    output.map(Cow::Owned).unwrap_or(Cow::Borrowed(input))
}

fn raw_url_token_end(input: &str, start: usize) -> usize {
    input[start..]
        .char_indices()
        .find_map(|(offset, ch)| ch.is_whitespace().then_some(start + offset))
        .unwrap_or(input.len())
}

fn url_finder() -> &'static LinkFinder {
    static FINDER: OnceLock<LinkFinder> = OnceLock::new();
    FINDER.get_or_init(|| {
        let mut finder = LinkFinder::new();
        finder.kinds(&[LinkKind::Url]);
        finder
    })
}
