// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;

use crate::ast::{Node, ParserFlags};
use crate::constants::{MAX_AST_NODES, MAX_LINE_LENGTH};
use crate::emoji::EmojiContext;
use crate::normalize::{
    apply_text_presentation_node, combine_adjacent_text, compact_empty_text_nodes,
    flatten_top_level_formatting, normalize_nodes,
};
use crate::text::{self, Line, trim, trim_start};

#[derive(Debug)]
pub enum ParseError {
    Serialize(serde_json::Error),
}

impl fmt::Display for ParseError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Serialize(error) => write!(formatter, "{error}"),
        }
    }
}

impl std::error::Error for ParseError {}

impl From<serde_json::Error> for ParseError {
    fn from(value: serde_json::Error) -> Self {
        Self::Serialize(value)
    }
}

#[derive(Clone, Debug)]
pub struct MarkdownParser {
    flags: u32,
    emoji_context: EmojiContext,
    pub(crate) node_count: usize,
}

impl MarkdownParser {
    pub fn new(flags: u32, emoji_context: EmojiContext) -> Self {
        Self {
            flags,
            emoji_context,
            node_count: 0,
        }
    }

    pub fn flags(&self) -> u32 {
        self.flags
    }

    pub fn emoji_context(&self) -> &EmojiContext {
        &self.emoji_context
    }

    pub fn parse(&mut self, input: &str) -> Result<Vec<Node>, ParseError> {
        let lines = text::split_lines(input);
        self.parse_lines(lines)
    }

    pub(crate) fn parse_lines(&mut self, lines: Vec<Line>) -> Result<Vec<Node>, ParseError> {
        RuntimeState {
            parser: self,
            lines,
            current_line: 0,
        }
        .parse()
    }

    pub(crate) fn child_with_flags(&self, flags: u32) -> Self {
        Self::new(flags, self.emoji_context.clone())
    }
}

pub(crate) struct RuntimeState<'a> {
    pub(crate) parser: &'a mut MarkdownParser,
    pub(crate) lines: Vec<Line>,
    pub(crate) current_line: usize,
}

impl RuntimeState<'_> {
    fn parse(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut ast = Vec::new();
        if self.lines.is_empty() {
            return Ok(ast);
        }

        while self.current_line < self.lines.len() && self.parser.node_count <= MAX_AST_NODES {
            if self.lines[self.current_line].text.len() > MAX_LINE_LENGTH {
                self.lines[self.current_line].text =
                    text::bounded_line_text(&self.lines[self.current_line].text).to_owned();
            }
            let line = self.lines[self.current_line].text.clone();
            let trimmed = trim_start(&line);
            if trim(trimmed).is_empty() {
                let blank_count = self.count_blank_lines();
                if !ast.is_empty() && self.current_line + blank_count < self.lines.len() {
                    let next_line = self.lines[self.current_line + blank_count].clone();
                    let next_trimmed = trim_start(&next_line.text);
                    let next_offset =
                        next_line.offset + (next_line.text.len() - next_trimmed.len());
                    let is_next_heading =
                        crate::block::parse_heading_node(self.parser, next_trimmed, next_offset)?
                            .is_some();
                    let is_prev_heading = matches!(ast.last(), Some(Node::Heading { .. }));
                    if !is_next_heading && !is_prev_heading {
                        ast.push(Node::Text {
                            content: "\n".repeat(blank_count),
                        });
                        self.parser.node_count += 1;
                    }
                }
                self.current_line += blank_count;
                continue;
            }
            let block = crate::block::parse_block(self)?;
            if let Some(node) = block.node {
                ast.push(node);
                if let Some(extra_nodes) = block.extra_nodes {
                    ast.extend(extra_nodes);
                }
                self.current_line = block.new_line_index;
                self.parser.node_count = block.new_node_count;
                continue;
            }
            self.parse_inline_line(&mut ast)?;
            self.current_line += 1;
        }
        for node in &mut ast {
            apply_text_presentation_node(node);
        }
        if ast.len() > 1 {
            normalize_nodes(&mut ast, false);
            flatten_top_level_formatting(&mut ast);
            combine_adjacent_text(&mut ast, false);
        }
        compact_empty_text_nodes(&mut ast);
        Ok(ast)
    }

    fn count_blank_lines(&self) -> usize {
        let mut count = 0usize;
        let mut index = self.current_line;
        while index < self.lines.len() && trim(&self.lines[index].text).is_empty() {
            count += 1;
            index += 1;
        }
        count
    }

    fn parse_inline_line(&mut self, ast: &mut Vec<Node>) -> Result<(), ParseError> {
        let mut text = self.lines[self.current_line].text.clone();
        let base_offset = self.lines[self.current_line].offset;
        let mut consumed = 1usize;
        while self.current_line + consumed < self.lines.len() {
            let next_line = &self.lines[self.current_line + consumed].text;
            let trimmed_next = trim_start(next_line);
            if crate::block::is_block_start(trimmed_next, self.parser.flags())
                || crate::block::is_table_start(
                    &self.lines,
                    self.current_line + consumed,
                    self.parser.flags(),
                )
                || trimmed_next.is_empty()
            {
                break;
            }
            text.push('\n');
            text.push_str(next_line);
            consumed += 1;
        }
        if self.current_line + consumed < self.lines.len() {
            let next_line = &self.lines[self.current_line + consumed].text;
            let trimmed_next = trim_start(next_line);
            let next_is_heading = crate::block::is_heading_start(trimmed_next, self.parser.flags());
            let next_is_blockquote =
                crate::block::is_blockquote_start(trimmed_next, self.parser.flags());
            if trimmed_next.is_empty() || (!next_is_heading && !next_is_blockquote) {
                text.push('\n');
            }
        }
        let inline_nodes = crate::inline::parse_inline(self.parser, &text, base_offset)?;
        for node in inline_nodes {
            ast.push(node);
            self.parser.node_count += 1;
            if self.parser.node_count > MAX_AST_NODES {
                break;
            }
        }
        self.current_line += consumed - 1;
        Ok(())
    }
}

#[allow(dead_code)]
pub(crate) fn all_flags() -> u32 {
    ParserFlags::ALL
}
