// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::json;

fn parse(input: &str) -> serde_json::Value {
    let mut parser = MarkdownParser::new(ParserFlags::ALLOW_CODE_BLOCKS, EmojiContext::default());
    let nodes = parser.parse(input).expect("parse succeeds");
    serde_json::to_value(nodes).expect("serialize succeeds")
}

#[test]
fn space_after_fence_keeps_following_lines() {
    assert_eq!(
        parse("``` hello\nworld\n```"),
        json!([{"type":"CodeBlock","content":" hello\nworld\n"}])
    );
}

#[test]
fn bare_fence_has_no_language() {
    assert_eq!(
        parse("```\nfoo\n```"),
        json!([{"type":"CodeBlock","content":"foo\n"}])
    );
}

#[test]
fn single_line_fence_with_space_is_content() {
    assert_eq!(
        parse("``` hello```"),
        json!([{"type":"CodeBlock","content":" hello"}])
    );
}

#[test]
fn single_line_fence_without_space_is_content() {
    assert_eq!(
        parse("```hello```"),
        json!([{"type":"CodeBlock","content":"hello"}])
    );
}

#[test]
fn fence_after_a_preceding_line_opens_code_block() {
    assert_eq!(
        parse("intro line\nlabel```rust\nfn main() {}\n```"),
        json!([
            {"type": "Text", "content": "intro line\nlabel"},
            {"type": "CodeBlock", "language": "rust", "content": "fn main() {}\n"}
        ])
    );
}

#[test]
fn fenced_body_with_pipes_after_a_line_is_not_a_table() {
    assert_eq!(
        parse("heading text\nrow```md\na | b\n---\nc | d\n```"),
        json!([
            {"type": "Text", "content": "heading text\nrow"},
            {"type": "CodeBlock", "language": "md", "content": "a | b\n---\nc | d\n"}
        ])
    );
}

#[test]
fn unterminated_midline_fence_after_a_line_stays_text() {
    assert_eq!(
        parse("hello\nfoo```bar with no closing fence"),
        json!([{"type": "Text", "content": "hello\nfoo```bar with no closing fence"}])
    );
}

#[test]
fn escaped_fence_stays_text_on_one_line() {
    assert_eq!(
        parse("\\```hello```"),
        json!([{"type": "Text", "content": "```hello```"}])
    );
}

#[test]
fn escaped_fence_stays_text_across_lines() {
    assert_eq!(
        parse("\\```rust\nfn main() {}\n```"),
        json!([{"type": "Text", "content": "```rust\nfn main() {}\n```"}])
    );
}

#[test]
fn escaped_midline_fence_stays_text() {
    assert_eq!(
        parse("label\\```rust\nfn main() {}\n```"),
        json!([{"type": "Text", "content": "label```rust\nfn main() {}\n```"}])
    );
}

#[test]
fn escaped_backslash_before_fence_still_opens_a_code_block() {
    assert_eq!(
        parse("\\\\```hello```"),
        json!([
            {"type": "Text", "content": "\\"},
            {"type": "CodeBlock", "content": "hello"}
        ])
    );
}

#[test]
fn longer_escaped_fence_stays_text() {
    assert_eq!(
        parse("\\````hello````"),
        json!([{"type": "Text", "content": "````hello````"}])
    );
}
