// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::{Value, json};

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

fn alert_children(body: &[&str]) -> Value {
    only_children(&format!("> [!NOTE]\n{}", quoted(body)), "Alert")
}

fn text(content: &str) -> Value {
    json!({"type": "Text", "content": content})
}

fn list(items: &[&str]) -> Value {
    json!({
        "type": "List",
        "ordered": false,
        "items": items
            .iter()
            .map(|item| json!({"children": [text(item)]}))
            .collect::<Vec<_>>()
    })
}

#[test]
fn alert_body_matches_blockquote_body() {
    let bodies: &[&[&str]] = &[
        &["para", "", "- a"],
        &["para", "", "", "- a"],
        &["- a", "", "", "- b"],
        &["- a", "", "", "", "", "- a"],
        &["a", "", "", "", "b"],
        &["<@123>", "", "- a"],
        &["-# small", "", "- a"],
        &["https://example.com", "", "- a"],
        &["```", "  x", "```"],
        &["```py", "def f():", "    return 1", "```"],
        &["- a", "  b"],
        &["- a", "  - b", "    c"],
        &["a", "", "  b"],
        &["  a"],
        &["a  ", "b"],
    ];
    for body in bodies {
        assert_eq!(
            alert_children(body),
            only_children(&quoted(body), "Blockquote"),
            "{body:?}"
        );
    }
}

#[test]
fn alert_keeps_blank_line_before_list() {
    assert_eq!(
        alert_children(&["para", "", "- a"]),
        json!([text("para\n\n"), list(&["a"])])
    );
    assert_eq!(
        alert_children(&["- a", "", "", "- b"]),
        json!([list(&["a"]), text("\n\n"), list(&["b"])])
    );
}

#[test]
fn alert_keeps_every_blank_line() {
    assert_eq!(
        alert_children(&["a", "", "", "", "b"]),
        json!([text("a\n\n\n\nb")])
    );
    assert_eq!(
        alert_children(&["- a", "", "", "", "", "- a"]),
        json!([list(&["a"]), text("\n\n\n\n"), list(&["a"])])
    );
}

#[test]
fn alert_keeps_code_block_indentation() {
    assert_eq!(
        alert_children(&["```", "  x", "```"]),
        json!([{"type": "CodeBlock", "content": "  x\n"}])
    );
    assert_eq!(
        alert_children(&["```py", "def f():", "    return 1", "```"]),
        json!([{"type": "CodeBlock", "language": "py", "content": "def f():\n    return 1\n"}])
    );
}

#[test]
fn alert_keeps_indented_line_inside_list_item() {
    assert_eq!(alert_children(&["- a", "  b"]), json!([list(&["a\nb"])]));
    assert_eq!(alert_children(&["a", "", "  b"]), json!([text("a\n\n  b")]));
}

#[test]
fn list_continuation_keeps_line_break() {
    assert_eq!(
        parse("- first line\n  second line"),
        json!([list(&["first line\nsecond line"])])
    );
    assert_eq!(
        only_children("> - a\n>   b", "Blockquote"),
        json!([list(&["a\nb"])])
    );
    assert_eq!(
        parse("- a\n  - b\n    cont b"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), list(&["b\ncont b"])]}
        ]}])
    );
    assert_eq!(
        parse("1. a\n   b"),
        json!([{"type": "List", "ordered": true, "items": [
            {"children": [text("a\nb")], "ordinal": 1}
        ]}])
    );
}

#[test]
fn list_continuation_after_nested_list_adds_no_blank_line() {
    assert_eq!(
        parse("- a\n  - b\n  c"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), list(&["b"]), text("c")]}
        ]}])
    );
}

#[test]
fn closing_fence_with_trailing_whitespace_adds_no_blank_line() {
    let spoiler = json!({"type": "Spoiler", "children": [text("x")], "isBlock": true});
    for input in ["||\nx\n||\ny", "||\r\nx\r\n||\r\ny", "||\nx\n||  \ny"] {
        assert_eq!(parse(input), json!([spoiler, text("y")]), "{input:?}");
    }
    for input in ["> ||\n> x\n> ||\n> y", "> ||\r\n> x\r\n> ||\r\n> y"] {
        assert_eq!(
            only_children(input, "Blockquote"),
            json!([spoiler, text("y")]),
            "{input:?}"
        );
    }
    let code = json!({"type": "CodeBlock", "content": "x\n"});
    for input in ["```\nx\n```\ny", "```\nx\n```  \ny", "```\nx\n```\t\ny"] {
        assert_eq!(parse(input), json!([code, text("y")]), "{input:?}");
    }
}

#[test]
fn whitespace_only_line_after_list_is_a_blank_line() {
    let expected = parse("- a\n\nafter");
    assert_eq!(expected, json!([list(&["a"]), text("\nafter")]));
    for input in ["- a\n  \nafter", "- a\n \nafter", "- a\n\t\nafter"] {
        assert_eq!(parse(input), expected, "{input:?}");
    }
    let body = json!([list(&["a"]), text("\nafter")]);
    for (input, node_type) in [
        ("> - a\n>   \n> after", "Blockquote"),
        (">>> - a\n  \nafter", "Blockquote"),
        ("> [!NOTE]\n> - a\n>   \n> after", "Alert"),
        ("||- a\n  \nafter||", "Spoiler"),
    ] {
        assert_eq!(only_children(input, node_type), body, "{input:?}");
    }
    assert_eq!(parse("- a\n  \n- b"), parse("- a\n\n- b"));
}

#[test]
fn whitespace_only_line_inside_list_item_matches_empty_line() {
    for (spaced, empty) in [
        ("- a\n  \n  b", "- a\n\n  b"),
        ("- a\n  \n  \n  b", "- a\n\n\n  b"),
        ("> - a\n>   \n>   b", "> - a\n> \n>   b"),
        (
            "> [!NOTE]\n> - a\n>   \n>   b",
            "> [!NOTE]\n> - a\n> \n>   b",
        ),
    ] {
        assert_eq!(parse(spaced), parse(empty), "{spaced:?}");
    }
    assert_eq!(parse("- a\n  \n  b"), json!([list(&["a"]), text("\n  b")]));
}

#[test]
fn code_fence_belongs_to_the_item_its_indent_reaches() {
    let code = json!({"type": "CodeBlock", "content": "code"});
    assert_eq!(
        parse("- a\n  - b\n  ```code```"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), list(&["b"]), code]}
        ]}])
    );
    assert_eq!(
        parse("- a\n  - b\n    ```code```"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), {"type": "List", "ordered": false, "items": [
                {"children": [text("b"), code]}
            ]}]}
        ]}])
    );
    assert_eq!(
        parse("- a\n```\ncode\n```"),
        json!([list(&["a"]), {"type": "CodeBlock", "content": "code\n"}])
    );
    assert_eq!(
        parse("- a\n  - b\n```code```"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), list(&["b"])]}
        ]}, code])
    );
    assert_eq!(
        parse("- a\n  - b\n  ```code``` tail"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), list(&["b"]), code, text(" tail")]}
        ]}])
    );
    assert_eq!(
        parse("- a\n  ```\n  x\n  ``` tail"),
        json!([{"type": "List", "ordered": false, "items": [
            {"children": [text("a"), {"type": "CodeBlock", "content": "x\n"}, text(" tail")]}
        ]}])
    );
    for body in [
        &["- a", "  - b", "  ```code```"][..],
        &["- a", "```", "code", "```"][..],
    ] {
        assert_eq!(
            alert_children(body),
            only_children(&quoted(body), "Blockquote"),
            "{body:?}"
        );
    }
}

#[test]
fn crlf_input_parses_like_lf() {
    for input in [
        "```\nx\n```\ny",
        "```js\nx\n```",
        "> ```\n> x\n> ```\n> y",
        "> a\n> b\nc",
        "> [!NOTE]\n> a\n> - b\n> c",
        "- a\n- b\nc",
        "# h\nx",
        "||\nx\n||\ny",
        "- a\n\nafter",
        "a\nb",
    ] {
        assert_eq!(
            parse(&input.replace('\n', "\r\n")),
            parse(input),
            "{input:?}"
        );
    }
}

#[test]
fn crlf_input_keeps_emoji_offsets() {
    let context = "S\t3\t4\t\u{1f600}\tgrinning\t1f600";
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(context));
    let crlf = serde_json::to_value(parser.parse("a\r\n\u{1f600}").expect("parse succeeds"))
        .expect("serialize succeeds");
    let context = "S\t2\t4\t\u{1f600}\tgrinning\t1f600";
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(context));
    let lf = serde_json::to_value(parser.parse("a\n\u{1f600}").expect("parse succeeds"))
        .expect("serialize succeeds");
    assert_eq!(crlf, lf);
    assert!(lf.to_string().contains("Emoji"), "{lf}");
}
