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
