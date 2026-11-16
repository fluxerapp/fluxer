// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::Value;

fn parse(input: &str) -> Value {
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(""));
    let nodes = parser.parse(input).expect("parse succeeds");
    serde_json::to_value(&nodes).expect("serialize succeeds")
}

fn only_children(input: &str, node_type: &str) -> Value {
    let nodes = parse(input);
    assert_eq!(
        nodes.as_array().map(Vec::len),
        Some(1),
        "{input:?} gave {nodes}"
    );
    assert_eq!(nodes[0]["type"], node_type, "{input:?} gave {nodes}");
    nodes[0]["children"].clone()
}

fn quoted(body: &[&str]) -> String {
    body.iter()
        .map(|line| format!("> {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn alert_body_after_list_matches_blockquote_body() {
    let bodies: &[&[&str]] = &[
        &["- Bullet point", "String with link https://example.com"],
        &["- Bullet point", "String with [link](https://example.com)"],
        &["- a", "- b", "after"],
        &["1. one", "2. two", "after"],
        &["Intro", "- Bullet point", "after"],
        &["- a", "  - nested", "after"],
        &["- a", "after", "more"],
        &["- a", "after", "- b", "end"],
        &["- a", "**bold** after"],
        &["- a", "`code` after"],
        &["- a", "<@123> after"],
        &["- a", "# heading"],
        &["- a", "-# subtext"],
    ];
    for body in bodies {
        let blockquote = quoted(body);
        let alert = format!("> [!NOTE]\n{blockquote}");
        assert_eq!(
            only_children(&alert, "Alert"),
            only_children(&blockquote, "Blockquote"),
            "{body:?}"
        );
    }
}
