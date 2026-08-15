// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};
use proptest::prelude::*;
use serde_json::json;

fn parse(input: &str, flags: u32, emoji_context: &str) -> serde_json::Value {
    let mut parser = MarkdownParser::new(flags, EmojiContext::parse(emoji_context));
    let nodes = parser.parse(input).expect("parse succeeds");
    serde_json::to_value(json!({"nodes": nodes})).expect("serialize succeeds")
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

#[test]
fn native_parser_allows_apostrophe_in_masked_link_destination() {
    let url = "https://docs.example.test/help/faq/#why-can't-this-link-parse%3F";
    let source = format!("[Example resource]({url})");
    let input = r#"A masked link should parse when a URL contains an apostrophe `'` in the fragment: [Example resource](https://docs.example.test/help/faq/#why-can't-this-link-parse%3F)"#;
    assert_eq!(
        parse(input, ParserFlags::ALLOW_MASKED_LINKS, ""),
        json!({"nodes":[
            {"type":"Text","content":"A masked link should parse when a URL contains an apostrophe "},
            {"type":"InlineCode","content":"'"},
            {"type":"Text","content":" in the fragment: "},
            {
                "type":"Link",
                "text":{"type":"Text","content":"Example resource"},
                "url":url,
                "escaped":false,
                "rawUrl":url,
                "source":source
            }
        ]})
    );
}

#[test]
fn native_parser_unescapes_numeric_dots_after_masked_link() {
    let input =
        "[`34d2f5e`](https://-/a/commit/-) 1\\.2\\.3\\.4\\.5\\.6\\.7\\.8\\.9\\.10\n \\- Amy";
    assert_eq!(
        parse(input, ParserFlags::ALLOW_MASKED_LINKS, ""),
        json!({"nodes":[
            {
                "type":"Link",
                "text":{"type":"InlineCode","content":"34d2f5e"},
                "url":"https://-/a/commit/-",
                "escaped":false,
                "rawUrl":"https://-/a/commit/-",
                "source":"[`34d2f5e`](https://-/a/commit/-)"
            },
            {"type":"Text","content":" 1.2.3.4.5.6.7.8.9.10\n - Amy"}
        ]})
    );
}

#[test]
fn native_parser_unescapes_word_and_sentence_dots() {
    assert_eq!(
        parse("Four more years\\. religion\\.\\.\\.", ParserFlags::ALL, ""),
        json!({"nodes":[
            {"type":"Text","content":"Four more years. religion..."}
        ]})
    );
    assert_eq!(
        parse("C:\\.", ParserFlags::ALL, ""),
        json!({"nodes":[
            {"type":"Text","content":"C:\\."}
        ]})
    );
}

#[test]
fn native_parser_allows_app_protocol_links() {
    let flags = ParserFlags::ALLOW_AUTOLINKS | ParserFlags::ALLOW_MASKED_LINKS;
    assert_eq!(
        parse(
            "fluxer://invite/abc fluxer:/channels/123/456 [Open gift](fluxer:gift/xyz)",
            flags,
            ""
        ),
        json!({"nodes":[
            {
                "type":"Link",
                "url":"fluxer://invite/abc",
                "escaped":false,
                "rawUrl":"fluxer://invite/abc",
                "source":"fluxer://invite/abc"
            },
            {"type":"Text","content":" "},
            {
                "type":"Link",
                "url":"fluxer:/channels/123/456",
                "escaped":false,
                "rawUrl":"fluxer:/channels/123/456",
                "source":"fluxer:/channels/123/456"
            },
            {"type":"Text","content":" "},
            {
                "type":"Link",
                "text":{"type":"Text","content":"Open gift"},
                "url":"fluxer:gift/xyz",
                "escaped":false,
                "rawUrl":"fluxer:gift/xyz",
                "source":"[Open gift](fluxer:gift/xyz)"
            }
        ]})
    );
}

#[test]
fn native_parser_rejects_apostrophe_in_masked_link_authority() {
    let input = "[Bad](https://ex'ample.com/path)";
    assert_eq!(
        parse(input, ParserFlags::ALLOW_MASKED_LINKS, ""),
        json!({"nodes":[
            {"type":"Text","content":input}
        ]})
    );
}

#[test]
fn native_parser_keeps_content_after_unterminated_escaped_destination() {
    let flags = ParserFlags::ALLOW_MASKED_LINKS | ParserFlags::ALLOW_AUTOLINKS;
    assert_eq!(
        parse(
            "a [x](<https://example.com/y)> b [e](<https://example.com/f>)",
            flags,
            ""
        ),
        json!({"nodes":[
            {"type":"Text","content":"a [x]("},
            {"type":"Link","url":"https://example.com/y)","escaped":true,"rawUrl":"https://example.com/y)","source":"<https://example.com/y)>"},
            {"type":"Text","content":" b "},
            {"type":"Link","text":{"type":"Text","content":"e"},"url":"https://example.com/f","escaped":true,"rawUrl":"https://example.com/f","source":"[e](<https://example.com/f>)"}
        ]})
    );
    assert_eq!(
        parse("[bad](<https://example.com/a> )", flags, ""),
        json!({"nodes":[
            {"type":"Text","content":"[bad]("},
            {"type":"Link","url":"https://example.com/a","escaped":true,"rawUrl":"https://example.com/a","source":"<https://example.com/a>"},
            {"type":"Text","content":" )"}
        ]})
    );
}

#[test]
fn native_parser_still_accepts_well_formed_escaped_destinations() {
    let flags = ParserFlags::ALLOW_MASKED_LINKS | ParserFlags::ALLOW_AUTOLINKS;
    assert_eq!(
        parse("[ok](<https://example.com/a>)", flags, ""),
        json!({"nodes":[
            {"type":"Link","text":{"type":"Text","content":"ok"},"url":"https://example.com/a","escaped":true,"rawUrl":"https://example.com/a","source":"[ok](<https://example.com/a>)"}
        ]})
    );
}

#[test]
fn native_parser_rejects_masked_links_without_visible_text() {
    for input in [
        "[](https://duckduckgo.com)",
        "[ ](https://duckduckgo.com)",
        "[\u{200e} ](<https://duckduckgo.com>)",
    ] {
        assert_eq!(
            parse(input, ParserFlags::ALLOW_MASKED_LINKS, ""),
            json!({"nodes":[
                {"type":"Text","content":input}
            ]})
        );
    }
}

#[test]
fn native_parser_requires_visible_subtext_content() {
    assert_eq!(
        parse("-# hello", ParserFlags::ALLOW_SUBTEXT, ""),
        json!({"nodes":[
            {"type":"Subtext","children":[{"type":"Text","content":"hello"}]}
        ]})
    );
    for input in ["-#", "-# ", "-# \t"] {
        assert_eq!(
            parse(input, ParserFlags::ALLOW_SUBTEXT, ""),
            json!({"nodes":[
                {"type":"Text","content":input}
            ]})
        );
    }
    assert_eq!(
        parse(
            "> -#",
            ParserFlags::ALLOW_BLOCKQUOTES | ParserFlags::ALLOW_SUBTEXT,
            ""
        ),
        json!({"nodes":[{
            "type":"Blockquote",
            "children":[{"type":"Text","content":"-#"}]
        }]})
    );
}

#[test]
fn native_parser_rejects_formatting_without_visible_content() {
    for input in [
        "` `",
        "`` ``",
        "** **",
        "__ __",
        "~~ ~~",
        "|| ||",
        "`\u{200e}`",
        "~~\u{200e} ~~",
        "# ",
        "||\n||",
        "|| \n||",
    ] {
        assert_eq!(
            parse(input, ParserFlags::ALL, ""),
            json!({"nodes":[
                {"type":"Text","content":input}
            ]})
        );
    }
}

#[test]
fn native_parser_rejects_inline_code_across_lines() {
    assert_eq!(
        parse("`\nHello world\n`", ParserFlags::ALL, ""),
        json!({"nodes":[
            {"type":"Text","content":"`\nHello world\n`"}
        ]})
    );
    assert_eq!(
        parse(
            "It seems the new fixes introduced a new bug <:trolley:1435375026174558456> :\n\n`\n.\n`",
            ParserFlags::ALL,
            ""
        ),
        json!({"nodes":[
            {"type":"Text","content":"It seems the new fixes introduced a new bug "},
            {"type":"Emoji","kind":{"kind":"Custom","name":"trolley","id":"1435375026174558456","animated":false}},
            {"type":"Text","content":" :\n\n`\n.\n`"}
        ]})
    );
    assert_eq!(
        parse("one `line` only", ParserFlags::ALL, ""),
        json!({"nodes":[
            {"type":"Text","content":"one "},
            {"type":"InlineCode","content":"line"},
            {"type":"Text","content":" only"}
        ]})
    );
}

proptest::proptest! {
    #[test]
    fn arbitrary_text_does_not_panic(input in ".{0,4096}") {
        let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::default());
        let nodes = parser.parse(&input).expect("parse succeeds");
        let serialized = serde_json::to_string(&nodes).expect("serialize succeeds");
        prop_assert!(serialized.starts_with('['));
    }
}
