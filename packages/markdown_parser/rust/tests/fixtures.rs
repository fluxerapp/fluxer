// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use serde_json::json;

fn parse(input: &str, flags: u32, emoji_context: &str) -> serde_json::Value {
    let mut parser = MarkdownParser::new(flags, EmojiContext::parse(emoji_context));
    let nodes = parser.parse(input).expect("parse succeeds");
    let value = serde_json::to_value(json!({"nodes": nodes})).expect("serialize succeeds");
    let written = fluxer_markdown_parser::parse_markdown_json(input, flags, emoji_context)
        .expect("parse succeeds");
    assert_eq!(
        serde_json::from_str::<serde_json::Value>(&written).expect("written json parses"),
        value
    );
    value
}

#[test]
fn native_parser_fixtures_cover_typescript_suite_surface() {
    let inline_flags = ParserFlags::ALLOW_SPOILERS
        | ParserFlags::ALLOW_MASKED_LINKS
        | ParserFlags::ALLOW_AUTOLINKS
        | ParserFlags::ALLOW_USER_MENTIONS
        | ParserFlags::ALLOW_CHANNEL_MENTIONS
        | ParserFlags::ALLOW_EVERYONE_MENTIONS;
    assert_eq!(parse("", 0, ""), json!({"nodes": []}));
    assert_eq!(
        parse(
            "# Heading\n||secret||",
            ParserFlags::ALLOW_HEADINGS | ParserFlags::ALLOW_SPOILERS,
            ""
        ),
        json!({"nodes":[
            {"type":"Heading","level":1,"children":[{"type":"Text","content":"Heading"}]},
            {"type":"Spoiler","children":[{"type":"Text","content":"secret"}],"isBlock":false}
        ]})
    );
    assert_eq!(
        parse("**bold** *em* __under__ ~~strike~~ `code`", 0, ""),
        json!({"nodes":[
            {"type":"Strong","children":[{"type":"Text","content":"bold"}]},
            {"type":"Text","content":" "},
            {"type":"Emphasis","children":[{"type":"Text","content":"em"}]},
            {"type":"Text","content":" "},
            {"type":"Underline","children":[{"type":"Text","content":"under"}]},
            {"type":"Text","content":" "},
            {"type":"Strikethrough","children":[{"type":"Text","content":"strike"}]},
            {"type":"Text","content":" "},
            {"type":"InlineCode","content":"code"}
        ]})
    );
    assert_eq!(
        parse(
            "Nevermind ! It is on C:\\, i thought i moved it",
            ParserFlags::ALL,
            ""
        ),
        json!({"nodes":[
            {"type":"Text","content":"Nevermind ! It is on C:\\, i thought i moved it"}
        ]})
    );
    assert_eq!(
        parse(
            "C:\\. C:\\! C:\\? C:\\; C:\\/ C:\\' C:\\\" C:\\{ C:\\}",
            ParserFlags::ALL,
            ""
        ),
        json!({"nodes":[
            {"type":"Text","content":"C:\\. C:\\! C:\\? C:\\; C:\\/ C:\\' C:\\\" C:\\{ C:\\}"}
        ]})
    );
    assert_eq!(
        parse("1\\. not a list", ParserFlags::ALLOW_LISTS, ""),
        json!({"nodes":[
            {"type":"Text","content":"1. not a list"}
        ]})
    );
    assert_eq!(
        parse("@everyone <@123> <#456> <t:1234567890:R>", inline_flags, ""),
        json!({"nodes":[
            {"type":"Mention","kind":{"kind":"Everyone"}},
            {"type":"Text","content":" "},
            {"type":"Mention","kind":{"kind":"User","id":"123"}},
            {"type":"Text","content":" "},
            {"type":"Mention","kind":{"kind":"Channel","id":"456"}},
            {"type":"Text","content":" "},
            {"type":"Timestamp","timestamp":1234567890,"style":"RelativeTime"}
        ]})
    );
    assert_eq!(
        parse(
            "hello 😄 :smile:",
            0,
            "S\t6\t4\t😄\tsmile\t1f604\nC\tsmile\t😄\t1f604\n"
        ),
        json!({"nodes":[
            {"type":"Text","content":"hello "},
            {"type":"Emoji","kind":{"kind":"Standard","raw":"😄","codepoints":"1f604","name":"smile"}},
            {"type":"Text","content":" "},
            {"type":"Emoji","kind":{"kind":"Standard","raw":"😄","codepoints":"1f604","name":"smile"}}
        ]})
    );
    assert_eq!(
        parse("Sweden 🇸🇪", 0, ""),
        json!({"nodes":[
            {"type":"Text","content":"Sweden "},
            {"type":"Emoji","kind":{"kind":"Standard","raw":"🇸🇪","codepoints":"1f1f8-1f1ea","name":"flag_se"}}
        ]})
    );
    assert_eq!(
        parse("# \u{200e} \n# \u{200e}", ParserFlags::ALLOW_HEADINGS, ""),
        json!({"nodes":[
            {"type":"Heading","level":1,"children":[{"type":"Text","content":"\u{200e} "}]},
            {"type":"Heading","level":1,"children":[{"type":"Text","content":"\u{200e}"}]}
        ]})
    );
    assert_eq!(
        parse("> Sweden 🇸🇪", ParserFlags::ALLOW_BLOCKQUOTES, ""),
        json!({"nodes":[{
            "type":"Blockquote",
            "children":[
                {"type":"Text","content":"Sweden "},
                {"type":"Emoji","kind":{"kind":"Standard","raw":"🇸🇪","codepoints":"1f1f8-1f1ea","name":"flag_se"}}
            ]
        }]})
    );
    assert_eq!(
        parse("> ", ParserFlags::ALLOW_BLOCKQUOTES, ""),
        json!({"nodes":[{
            "type":"Blockquote",
            "children":[],
            "blankLines":1
        }]})
    );
    assert_eq!(
        parse("> \n>  \nsome text", ParserFlags::ALLOW_BLOCKQUOTES, ""),
        json!({"nodes":[
            {
                "type":"Blockquote",
                "children":[],
                "blankLines":2
            },
            {"type":"Text","content":"some text"}
        ]})
    );
    assert_eq!(
        parse("> \n>", ParserFlags::ALLOW_BLOCKQUOTES, ""),
        json!({"nodes":[
            {
                "type":"Blockquote",
                "children":[],
                "blankLines":1
            },
            {"type":"Text","content":">"}
        ]})
    );
}

fn count_nodes(value: &serde_json::Value) -> usize {
    match value {
        serde_json::Value::Object(map) => {
            usize::from(map.contains_key("type")) + map.values().map(count_nodes).sum::<usize>()
        }
        serde_json::Value::Array(items) => items.iter().map(count_nodes).sum(),
        _ => 0,
    }
}

#[test]
fn wide_table_with_short_rows_stays_within_node_budget() {
    let header = vec!["a"; 500].join("|");
    let align = vec!["-"; 500].join("|");
    let mut input = format!("{header}\n{align}\n");
    while input.len() + 2 <= 4000 {
        input.push_str("|\n");
    }
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
    let nodes = parser.parse(&input).expect("parse succeeds");
    let value = serde_json::to_value(&nodes).expect("serialize succeeds");
    assert!(count_nodes(&value) <= 12_000);

    let quoted_header = vec!["a"; 71].join("|");
    let quoted_align = vec!["-"; 71].join("|");
    let block = format!(
        "> {quoted_header}\n> {quoted_align}\n{}x\n",
        "> |\n".repeat(160)
    );
    let quoted = block.repeat(4000 / block.len());
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
    let nodes = parser.parse(&quoted).expect("parse succeeds");
    let value = serde_json::to_value(&nodes).expect("serialize succeeds");
    assert!(count_nodes(&value) <= 12_000);

    let alert = format!(
        "> [!NOTE]\n> {quoted_header}\n> {quoted_align}\n{}x\n",
        "> |\n".repeat(160)
    );
    let spoiler = format!(
        "||\n{quoted_header}\n{quoted_align}\n{}||\n",
        "|\n".repeat(170)
    );
    for unit in [alert, spoiler] {
        let input = unit.repeat(4000 / unit.len());
        let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
        let nodes = parser.parse(&input).expect("parse succeeds");
        let value = serde_json::to_value(&nodes).expect("serialize succeeds");
        assert!(count_nodes(&value) <= 12_000);
    }

    let filled = parse("a|b\n-|-\n1|2\n|", ParserFlags::ALL, "");
    assert_eq!(filled["nodes"][0]["rows"].as_array().map(Vec::len), Some(2));
}

#[test]
fn skin_tone_shortcode_names_resolve_from_emoji_context() {
    let context = concat!(
        "C\tadult_tone1\t\u{1F9D1}\u{1F3FB}\t1f9d1-1f3fb\n",
        "C\tcouple_mm_tone1_tone3\t\u{1F468}\u{1F3FB}\u{200D}\u{2764}\u{FE0F}\u{200D}\u{1F468}\u{1F3FD}\t1f468-1f3fb-200d-2764-fe0f-200d-1f468-1f3fd\n",
        "C\tadult\t\u{1F9D1}\t1f9d1\n",
    );
    assert_eq!(
        parse(
            ":adult_tone1: :couple_mm_tone1_tone3: :adult: <:blob123:1234567890> :adult_tone6:",
            ParserFlags::ALL,
            context
        ),
        json!({"nodes":[
            {"type":"Emoji","kind":{"kind":"Standard","raw":"\u{1F9D1}\u{1F3FB}","codepoints":"1f9d1-1f3fb","name":"adult_tone1"}},
            {"type":"Text","content":" "},
            {"type":"Emoji","kind":{"kind":"Standard","raw":"\u{1F468}\u{1F3FB}\u{200D}\u{2764}\u{FE0F}\u{200D}\u{1F468}\u{1F3FD}","codepoints":"1f468-1f3fb-200d-2764-fe0f-200d-1f468-1f3fd","name":"couple_mm_tone1_tone3"}},
            {"type":"Text","content":" "},
            {"type":"Emoji","kind":{"kind":"Standard","raw":"\u{1F9D1}","codepoints":"1f9d1","name":"adult"}},
            {"type":"Text","content":" "},
            {"type":"Emoji","kind":{"kind":"Custom","name":"blob123","id":"1234567890","animated":false}},
            {"type":"Text","content":" :adult_tone6:"}
        ]})
    );
}
