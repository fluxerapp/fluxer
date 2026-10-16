// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::binary::{FORMAT_VERSION, write_ast_binary};
use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};

fn encode(input: &str) -> Vec<u8> {
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(""));
    let nodes = parser.parse(input).expect("parse should succeed");
    write_ast_binary(&nodes)
}

#[test]
fn version_byte_is_stable() {
    assert_eq!(FORMAT_VERSION, 1);
}

#[test]
fn empty_input_is_version_and_zero_count() {
    assert_eq!(encode(""), [1, 0]);
}

#[test]
fn strong_text_golden_bytes() {
    assert_eq!(encode("**b**"), [1, 1, 2, 1, 0, 1, b'b']);
}

#[test]
fn timestamp_golden_bytes() {
    assert_eq!(
        encode("<t:1234567890:R>"),
        [1, 1, 15, 0xD2, 0x85, 0xD8, 0xCC, 0x04, 8]
    );
}

#[test]
fn ordered_list_golden_bytes() {
    assert_eq!(encode("4. a"), [1, 1, 9, 1, 1, 1, 4, 1, 0, 1, b'a']);
}

#[test]
fn link_golden_bytes() {
    let url = b"https://e.com/a";
    let mut expected = vec![1, 1, 13, 2];
    for _ in 0..2 {
        expected.push(url.len() as u8);
        expected.extend_from_slice(url);
    }
    let source = b"[t](https://e.com/a)";
    expected.push(source.len() as u8);
    expected.extend_from_slice(source);
    expected.extend_from_slice(&[0, 1, b't']);
    assert_eq!(encode("[t](https://e.com/a)"), expected);
}

#[test]
fn spoiler_and_heading_golden_bytes() {
    assert_eq!(
        encode("# h\n||s||"),
        [1, 2, 7, 1, 1, 0, 1, b'h', 6, 1, 1, 0, 1, b's']
    );
}
