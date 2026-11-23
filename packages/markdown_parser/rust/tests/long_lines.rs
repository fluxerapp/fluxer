// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, Node, ParserFlags};

fn rendered_text(input: &str) -> String {
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(""));
    let nodes = parser.parse(input).expect("parse succeeds");
    nodes
        .iter()
        .map(|node| match node {
            Node::Text { content } => content.as_str(),
            other => panic!("unexpected node {other:?}"),
        })
        .collect()
}

#[test]
fn single_line_longer_than_line_cap_keeps_all_text() {
    let input = "word ".repeat(1200);
    assert_eq!(rendered_text(&input), input);
}

#[test]
fn paragraph_longer_than_line_cap_keeps_all_text() {
    let input = format!("{}\n", "a".repeat(79)).repeat(80);
    assert_eq!(rendered_text(input.trim_end()), input.trim_end());
}
