use fluxer_markdown_parser::binary::write_ast_binary;
use fluxer_markdown_parser::{EmojiContext, MarkdownParser, ParserFlags};

fn encode(input: &str) -> Vec<u8> {
    let mut parser = MarkdownParser::new(ParserFlags::ALL, EmojiContext::parse(""));
    let nodes = parser.parse(input).expect("parse should succeed");
    write_ast_binary(&nodes)
}

#[test]
fn ordered_list_no_checkbox() {
    assert_eq!(encode("4. a"), [1, 1, 9, 1, 1, 1, 4, 0, 1, 0, 1, b'a']);
}

#[test]
fn ordered_list_checkbox() {
    assert_eq!(encode("4. [x] a"), [1, 1, 9, 1, 1, 1, 4, 2, 1, 0, 1, b'a']);
    assert_eq!(encode("4. [ ] a"), [1, 1, 9, 1, 1, 1, 4, 1, 1, 0, 1, b'a']);
}

#[test]
fn unordered_list_no_checkbox() {
    assert_eq!(encode("- a"), [1, 1, 9, 0, 1, 0, 0, 1, 0, 1, b'a']);
}

#[test]
fn unordered_list_checkbox() {
    assert_eq!(encode("- [x] a"), [1, 1, 9, 0, 1, 0, 2, 1, 0, 1, b'a']);
    assert_eq!(encode("- [ ] a"), [1, 1, 9, 0, 1, 0, 1, 1, 0, 1, b'a']);
}

#[test]
fn no_checkbox() {
    assert_eq!(encode("- [p] a"), [1, 1, 9, 0, 1, 0, 0, 1, 0, 5, b'[', b'p', b']', b' ', b'a']);
}
