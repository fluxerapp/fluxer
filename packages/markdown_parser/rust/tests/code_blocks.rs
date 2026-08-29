// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::json;

fn parse(input: &str) -> serde_json::Value {
    let mut parser = MarkdownParser::new(ParserFlags::ALLOW_CODE_BLOCKS, EmojiContext::default());
    let nodes = parser.parse(input).expect("parse succeeds");
    serde_json::to_value(nodes).expect("serialize succeeds")
}

#[test]
fn space_after_fence_is_content_not_language() {
    assert_eq!(
        parse("``` hello\n```"),
        json!([{"type":"CodeBlock","content":" hello\n"}])
    );
}

#[test]
fn space_after_fence_keeps_following_lines() {
    assert_eq!(
        parse("``` hello\nworld\n```"),
        json!([{"type":"CodeBlock","content":" hello\nworld\n"}])
    );
}

#[test]
fn tab_after_fence_is_content_not_language() {
    assert_eq!(
        parse("```\thello\n```"),
        json!([{"type":"CodeBlock","content":"\thello\n"}])
    );
}

#[test]
fn multiple_spaces_after_fence_are_preserved_as_content() {
    assert_eq!(
        parse("```   rust\nfn main() {}\n```"),
        json!([{"type":"CodeBlock","content":"   rust\nfn main() {}\n"}])
    );
}

#[test]
fn space_before_otherwise_valid_language_chars_is_content() {
    assert_eq!(
        parse("``` c++\nx\n```"),
        json!([{"type":"CodeBlock","content":" c++\nx\n"}])
    );
}

#[test]
fn space_before_known_multi_token_language_is_content() {
    assert_eq!(
        parse("``` js code\n```"),
        json!([{"type":"CodeBlock","content":" js code\n"}])
    );
}

#[test]
fn space_after_fence_disables_ansi_language() {
    assert_eq!(
        parse("``` ansi\n\u{1b}[31mhi\u{1b}[0m\n```"),
        json!([{"type":"CodeBlock","content":" ansi\n\u{1b}[31mhi\u{1b}[0m\n"}])
    );
}

#[test]
fn space_after_longer_fence_is_content() {
    assert_eq!(
        parse("````  rust\ncode\n````"),
        json!([{"type":"CodeBlock","content":"  rust\ncode\n"}])
    );
}

#[test]
fn language_immediately_after_fence_is_detected() {
    assert_eq!(
        parse("```hello\n```"),
        json!([{"type":"Text","content":"```hello\n```"}])
    );
}

#[test]
fn common_language_with_body_is_detected() {
    assert_eq!(
        parse("```js\ncode\n```"),
        json!([{"type":"CodeBlock","language":"js","content":"code\n"}])
    );
    assert_eq!(
        parse("```rust\nfn main() {}\n```"),
        json!([{"type":"CodeBlock","language":"rust","content":"fn main() {}\n"}])
    );
}

#[test]
fn languages_with_special_characters_are_detected() {
    assert_eq!(
        parse("```c#\nx\n```"),
        json!([{"type":"CodeBlock","language":"c#","content":"x\n"}])
    );
    assert_eq!(
        parse("```c++\nx\n```"),
        json!([{"type":"CodeBlock","language":"c++","content":"x\n"}])
    );
}

#[test]
fn ansi_language_immediately_after_fence_is_detected() {
    assert_eq!(
        parse("```ansi\n\u{1b}[31mhi\u{1b}[0m\n```"),
        json!([{"type":"CodeBlock","language":"ansi","content":"\u{1b}[31mhi\u{1b}[0m\n"}])
    );
}

#[test]
fn known_multi_token_language_without_leading_space_is_detected() {
    assert_eq!(
        parse("```js code\n```"),
        json!([{"type":"Text","content":"```js code\n```"}])
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
fn empty_fenced_block_has_no_language_and_empty_body() {
    assert_eq!(
        parse("```\n```"),
        json!([{"type":"Text","content":"```\n```"}])
    );
}

#[test]
fn unknown_multi_token_info_string_is_content() {
    assert_eq!(
        parse("```hello world\n```"),
        json!([{"type":"CodeBlock","content":"hello world\n"}])
    );
}

#[test]
fn whitespace_only_info_string_is_content() {
    assert_eq!(
        parse("```  \nfoo\n```"),
        json!([{"type":"CodeBlock","content":"  \nfoo\n"}])
    );
    assert_eq!(
        parse("``` \n```"),
        json!([{"type":"Text","content":"``` \n```"}])
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
