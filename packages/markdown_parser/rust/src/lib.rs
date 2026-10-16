// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod ast;
pub mod binary;
pub mod block;
pub mod constants;
pub mod emoji;
pub mod inline;
pub mod json;
pub mod links;
pub mod native;
pub mod normalize;
pub mod parser;
pub mod plaintext;
pub mod text;
pub mod wasm;

pub use ast::{Node, ParserFlags};
pub use emoji::EmojiContext;
pub use parser::{MarkdownParser, ParseError};

pub fn parse_markdown_json(
    input: &str,
    flags: u32,
    emoji_context: &str,
) -> Result<String, ParseError> {
    let context = EmojiContext::parse(emoji_context);
    let mut parser = MarkdownParser::new(flags, context);
    let nodes = parser.parse(input)?;
    json::write_ast_json(&nodes)
}
