// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::{Value, json};

fn parse(input: &str) -> Value {
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(""));
    json!(parser.parse(input).expect("parse succeeds"))
}

#[test]
fn email_text_masks_only_its_own_mailto_address() {
    assert_eq!(
        parse("[example@example.com](mailto:Example@Example.com)")[0]["type"],
        "Link"
    );
    assert_eq!(
        parse("[example@example.com](mailto:other@example.com)")[0]["type"],
        "Text"
    );
    assert_eq!(
        parse("[example@example.com](https://example.com)")[0]["type"],
        "Text"
    );
}
