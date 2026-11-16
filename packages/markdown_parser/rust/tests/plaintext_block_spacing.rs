// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::plaintext::{PlaintextOptions, parse_and_render_plaintext};

fn render(content: &str, preserve_markdown: bool) -> String {
    let options = PlaintextOptions {
        preserve_markdown,
        ..PlaintextOptions::default()
    };
    parse_and_render_plaintext(content, ParserFlags::ALL, "", &options).expect("render succeeds")
}

#[test]
fn blank_line_after_block_is_kept() {
    assert_eq!(render("- a\n\nafter", false), "\u{2022} a\n\nafter");
    assert_eq!(render("- a\n\nafter", true), "- a\n\nafter");
    assert_eq!(render("-# s\n\nafter", false), "s\n\nafter");
    assert_eq!(render("```\nx\n```\n\nafter", true), "```\nx\n```\n\nafter");
    assert_eq!(render("```\nx\n```\n\nafter", false), "x\n\nafter");
}

#[test]
fn blank_line_after_block_is_kept_in_containers() {
    assert_eq!(render("> - a\n> \n> after", false), "\u{2022} a\n\nafter");
    assert_eq!(render("> - a\n> \n> after", true), "> - a\n> \n> after");
    assert_eq!(
        render("> [!NOTE]\n> - a\n> \n> after", false),
        "Note\n\u{2022} a\n\nafter"
    );
    assert_eq!(render("||\n- a\n\nafter\n||", false), "\u{2022} a\n\nafter");
    assert_eq!(render("> -# s\n> \n> after", false), "s\n\nafter");
}

#[test]
fn block_without_blank_line_stays_on_next_line() {
    assert_eq!(render("- a\nafter", false), "\u{2022} a\nafter");
    assert_eq!(render("before\n\n- a", false), "before\n\n\u{2022} a");
    assert_eq!(render("-# s\nafter", false), "s\nafter");
    assert_eq!(render("```\nx\n```\nafter", true), "```\nx\n```\nafter");
}

#[test]
fn bullet_continuation_lines_indent_by_bullet_width() {
    assert_eq!(render("- a\n  b", false), "\u{2022} a\n  b");
    assert_eq!(render("- a\n  b", true), "- a\n  b");
}
