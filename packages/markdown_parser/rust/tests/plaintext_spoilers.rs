// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::plaintext::{PlaintextOptions, parse_and_render_plaintext};

fn render(content: &str, hide_spoilers: bool) -> String {
    let options = PlaintextOptions {
        preserve_markdown: true,
        hide_spoilers,
        ..PlaintextOptions::default()
    };
    parse_and_render_plaintext(content, ParserFlags::ALL, "", &options).expect("render succeeds")
}

#[test]
fn hidden_spoilers_render_as_placeholder() {
    assert_eq!(render("a ||secret|| b", true), "a █████ b");
    assert!(!render("||\nsecret\n||", true).contains("secret"));
}

#[test]
fn visible_spoilers_keep_their_markers() {
    assert_eq!(render("a ||secret|| b", false), "a ||secret|| b");
}
