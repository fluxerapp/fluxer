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

#[cfg(test)]
mod tests {
    use super::{extract_mentions_from_markdown, may_contain_mention, parse_mentions};

    const MENTION_CORPUS: &[&str] = &[
        "hey, are we still on for tonight?",
        "lol",
        "that build is green now, shipping it",
        "no idea, ask in the other channel",
        "brb",
        "https://example.com/some/long/path?query=1&other=2",
        "check out https://github.com/fluxerapp/fluxer/pull/1234 when you get a sec",
        "mail me at someone@example.com",
        "the price is 12 <> 15 depending on the region",
        "a < b && c > d",
        "<https://example.com/autolink>",
        "<sms:+15550001111>",
        "<+15550001111>",
        "</settings:123>",
        "<id:customize>",
        "<:custom_emoji:1234567890>",
        "<t:1700000000:R>",
        "```rust\nfn main() { println!(\"hi\"); }\n```",
        "`inline code with a # and an < in it`",
        "**bold** *italic* __underline__ ~~strike~~ ||spoiler||",
        "> quoted line\n> another quoted line",
        "# heading\n## smaller heading\n-# subtext",
        "- item one\n- item two\n1. numbered",
        "|a|b|\n|-|-|\n|1|2|",
        "[masked link](https://example.com)",
        "emoji party 🎉🎉🎉 and a flag 🇸🇪",
        "escaped \\<@123> should stay text",
        "escaped \\@everyone should stay text",
        "channel #general is over there",
        "email me @ work tomorrow",
        "@ everyone with a space",
        "@ here with a space",
        "@everyones and @heresy are longer words",
        "hi <@123> <@!456> <@&789> <#321>",
        "<@0> <@&0> <#0>",
        "@everyone hello @here",
        "`@everyone` @here\n```\n@everyone\n```",
        "`<@111>` <@222>\n```txt\n<@333> <#444>\n```\n<#555>",
        "https://example.com/<@123> <@456>",
        "[click here](https://example.com) and <#888> <@999>",
        "**bold <@100>** *italic <@&200>* ~~strike <#300>~~ __underline <@400>__",
        "> quoted <@111>\n<@222>",
        "||spoiler <@333>||",
        "[<@101>](https://example.com/<@202>) <#303>",
        "<@123456789012345678> <@&999> <#888>",
        "<#not_an_id> <@not_an_id> <@&not_an_id>",
    ];

    #[test]
    fn prefilter_never_changes_extraction_over_the_corpus() {
        for input in MENTION_CORPUS {
            assert_eq!(
                extract_mentions_from_markdown(Some(input)),
                parse_mentions(input),
                "prefilter changed the result for {input:?}"
            );
        }
    }

    #[test]
    fn corpus_exercises_both_prefilter_outcomes() {
        assert!(
            MENTION_CORPUS
                .iter()
                .any(|input| may_contain_mention(input))
        );
        assert!(
            MENTION_CORPUS
                .iter()
                .any(|input| !may_contain_mention(input))
        );
    }

    #[test]
    fn prefilter_accepts_every_mention_marker() {
        assert!(may_contain_mention("<@1>"));
        assert!(may_contain_mention("<@!1>"));
        assert!(may_contain_mention("<@&1>"));
        assert!(may_contain_mention("<#1>"));
        assert!(may_contain_mention("@everyone"));
        assert!(may_contain_mention("@here"));
        assert!(!may_contain_mention("< @1>"));
        assert!(!may_contain_mention("@ everyone"));
        assert!(!may_contain_mention("plain text"));
    }

    #[test]
    fn extracts_real_user_role_and_channel_mentions() {
        let mentions = extract_mentions_from_markdown(Some("hi <@123> <@!456> <@&789> <#321>"));
        assert!(mentions.users.contains(&123));
        assert!(mentions.users.contains(&456));
        assert!(mentions.roles.contains(&789));
        assert!(mentions.channels.contains(&321));
    }

    #[test]
    fn ignores_mentions_inside_inline_and_block_code() {
        let mentions = extract_mentions_from_markdown(Some(
            "`<@111>` <@222>\n```txt\n<@333> <#444>\n```\n<#555>",
        ));
        assert!(!mentions.users.contains(&111));
        assert!(mentions.users.contains(&222));
        assert!(!mentions.users.contains(&333));
        assert!(!mentions.channels.contains(&444));
        assert!(mentions.channels.contains(&555));
    }

    #[test]
    fn ignores_mentions_inside_urls() {
        let mentions = extract_mentions_from_markdown(Some("https://example.com/<@123> <@456>"));
        assert!(!mentions.users.contains(&123));
        assert!(mentions.users.contains(&456));
    }

    #[test]
    fn detects_everyone_and_here() {
        let mentions = extract_mentions_from_markdown(Some("@everyone hello @here"));
        assert!(mentions.everyone);
        assert!(mentions.here);
    }

    #[test]
    fn ignores_everyone_and_here_in_code() {
        let mentions =
            extract_mentions_from_markdown(Some("`@everyone` @here\n```\n@everyone\n```"));
        assert!(!mentions.everyone);
        assert!(mentions.here);
    }

    #[test]
    fn extracts_from_nested_formatting() {
        let mentions = extract_mentions_from_markdown(Some(
            "**bold <@100>** *italic <@&200>* ~~strike <#300>~~ __underline <@400>__",
        ));
        assert!(mentions.users.contains(&100));
        assert!(mentions.roles.contains(&200));
        assert!(mentions.channels.contains(&300));
        assert!(mentions.users.contains(&400));
    }

    #[test]
    fn extracts_from_blockquotes() {
        let mentions = extract_mentions_from_markdown(Some("> quoted <@111>\n<@222>"));
        assert!(mentions.users.contains(&111));
        assert!(mentions.users.contains(&222));
    }

    #[test]
    fn extracts_from_spoilers() {
        let mentions = extract_mentions_from_markdown(Some("||spoiler <@333>||"));
        assert!(mentions.users.contains(&333));
    }

    #[test]
    fn returns_empty_for_none_and_empty() {
        assert!(extract_mentions_from_markdown(None).is_empty());
        assert!(extract_mentions_from_markdown(Some("")).is_empty());
    }

    #[test]
    fn ignores_zero_id() {
        let mentions = extract_mentions_from_markdown(Some("<@0> <@&0> <#0>"));
        assert!(mentions.users.is_empty());
        assert!(mentions.roles.is_empty());
        assert!(mentions.channels.is_empty());
    }

    #[test]
    fn extracts_mention_after_markdown_link() {
        let mentions = extract_mentions_from_markdown(Some(
            "[click here](https://example.com) and <#888> <@999>",
        ));
        assert!(mentions.channels.contains(&888));
        assert!(mentions.users.contains(&999));
    }

    #[test]
    fn deduplicates_repeated_mentions() {
        let mentions = extract_mentions_from_markdown(Some("<@100> <@100> <@100> <@&200> <@&200>"));
        assert_eq!(mentions.users.len(), 1);
        assert_eq!(mentions.roles.len(), 1);
    }
}
