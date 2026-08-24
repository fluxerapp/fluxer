// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::{AlertType, ListItem, Node, ParserFlags, TableAlignment};
use crate::constants::{MAX_AST_NODES, MAX_LINE_LENGTH};
use crate::links::{has_open_inline_code, has_valid_code_fence_language};
use crate::normalize::{normalize_nodes, replace_trailing_whitespace_with_newline};
use crate::parser::{MarkdownParser, ParseError, RuntimeState};
use crate::text::{
    Line, bounded_line_text, byte_at, concat, concat3, has_visible_content, line_as_text,
    lines_to_text, starts_with, trim, trim_line_window, trim_right, trim_start,
    trim_start_newline_whitespace,
};

#[derive(Clone, Debug)]
pub(crate) struct BlockParseResult {
    pub node: Option<Node>,
    pub new_line_index: usize,
    pub new_node_count: usize,
    pub extra_nodes: Option<Vec<Node>>,
}

#[derive(Clone, Debug)]
struct CodeBlockResult {
    node: Node,
    new_line_index: usize,
    extra_content: Option<ExtraContent>,
}

#[derive(Clone, Debug)]
struct ExtraContent {
    content: String,
    offset: usize,
}

#[derive(Clone, Debug)]
struct BlockSpoilerResult {
    node: Node,
    new_line_index: usize,
    extra_content: Option<ExtraContent>,
}

#[derive(Clone, Debug)]
struct ClosingFence<'a> {
    fence_index: usize,
    backtick_count: usize,
    trailing_text: &'a str,
}

#[derive(Clone, Debug)]
pub(crate) struct ListMatch<'a> {
    ordered: bool,
    indent_level: usize,
    content: &'a str,
    ordinal: Option<usize>,
}

#[derive(Clone, Debug)]
struct ListResult {
    node: Node,
    new_line_index: usize,
    new_node_count: usize,
}

#[derive(Clone, Debug)]
struct TableResult {
    node: Node,
    new_line_index: usize,
}

#[derive(Clone, Debug)]
struct TableCellText {
    text: String,
    offset: usize,
}

pub(crate) fn parse_block(state: &mut RuntimeState<'_>) -> Result<BlockParseResult, ParseError> {
    let current = state.current_line;
    let line = state.lines[current].text.clone();
    let line_offset = state.lines[current].offset;
    let trimmed = trim_start(&line);
    let trimmed_offset = line_offset + (line.len() - trimmed.len());

    if starts_with(trimmed, ">>> ") {
        if !ParserFlags::has(
            state.parser.flags(),
            ParserFlags::ALLOW_MULTILINE_BLOCKQUOTES,
        ) {
            return Ok(BlockParseResult {
                node: Some(Node::Text {
                    content: line_as_text(&state.lines, current),
                }),
                new_line_index: current + 1,
                new_node_count: state.parser.node_count + 1,
                extra_nodes: None,
            });
        }
        return parse_multiline_blockquote(
            state.parser,
            &state.lines,
            current,
            state.parser.node_count,
        );
    }
    if starts_with(trimmed, "> ") {
        if !ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_BLOCKQUOTES) {
            return Ok(no_block(current, state.parser.node_count));
        }
        return parse_blockquote(state.parser, &state.lines, current, state.parser.node_count);
    }

    if let Some(list_match) = match_list_item(&line) {
        if ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_LISTS) {
            let list = parse_list(
                state.parser,
                &state.lines,
                current,
                list_match.ordered,
                list_match.indent_level,
                1,
                state.parser.node_count,
            )?;
            return Ok(BlockParseResult {
                node: Some(list.node),
                new_line_index: list.new_line_index,
                new_node_count: list.new_node_count,
                extra_nodes: None,
            });
        }
        return Ok(BlockParseResult {
            node: Some(Node::Text { content: line }),
            new_line_index: current + 1,
            new_node_count: state.parser.node_count + 1,
            extra_nodes: None,
        });
    }

    if starts_with(trimmed, "||") && !trimmed[2..].contains("||") {
        if ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_SPOILERS) {
            let result = parse_block_spoiler(state.parser, &state.lines, current)?;
            if let Some(extra) = &result.extra_content {
                state.lines[result.new_line_index].text = extra.content.clone();
                state.lines[result.new_line_index].offset = extra.offset;
            }
            return Ok(BlockParseResult {
                node: Some(result.node),
                new_line_index: result.new_line_index,
                new_node_count: state.parser.node_count + 1,
                extra_nodes: None,
            });
        }
        return Ok(BlockParseResult {
            node: Some(Node::Text { content: line }),
            new_line_index: current + 1,
            new_node_count: state.parser.node_count + 1,
            extra_nodes: None,
        });
    }

    if ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_CODE_BLOCKS) {
        if let Some(fence_pos) = line.find("```") {
            let starts_with_fence =
                starts_with(trimmed, "```") && fence_pos == line.len() - trimmed.len();
            if starts_with_fence {
                if let Some(result) = parse_code_block(state.parser, &state.lines, current)? {
                    if let Some(extra) = &result.extra_content {
                        state.lines[result.new_line_index].text = extra.content.clone();
                        state.lines[result.new_line_index].offset = extra.offset;
                    }
                    return Ok(BlockParseResult {
                        node: Some(result.node),
                        new_line_index: result.new_line_index,
                        new_node_count: state.parser.node_count + 1,
                        extra_nodes: None,
                    });
                }
                return Ok(no_block(current, state.parser.node_count));
            }
            let prefix = &line[..fence_pos];
            if has_open_inline_code(prefix) {
                return Ok(no_block(current, state.parser.node_count));
            }
            let inline_nodes = crate::inline::parse_inline(state.parser, prefix, line_offset)?;
            let code_lines = slice_lines_from_fence(&state.lines, current, fence_pos);
            if let Some(code_result) = parse_code_block(state.parser, &code_lines, 0)? {
                let new_line_index = current + code_result.new_line_index;
                let mut extra_nodes = Vec::new();
                if inline_nodes.len() > 1 {
                    extra_nodes.extend(inline_nodes[1..].iter().cloned());
                }
                extra_nodes.push(code_result.node.clone());
                if let Some(extra) = &code_result.extra_content {
                    state.lines[new_line_index].text = extra.content.clone();
                    state.lines[new_line_index].offset = extra.offset;
                }
                let first_node = inline_nodes.first().cloned().unwrap_or(code_result.node);
                return Ok(BlockParseResult {
                    node: Some(first_node),
                    extra_nodes: Some(extra_nodes),
                    new_line_index,
                    new_node_count: state.parser.node_count + inline_nodes.len() + 1,
                });
            }
        }
    } else if starts_with(trimmed, "```") {
        let mut content = line;
        let mut end_index = current + 1;
        while end_index < state.lines.len() {
            content = concat3(&content, "\n", &state.lines[end_index].text);
            if trim(&state.lines[end_index].text) == "```" {
                end_index += 1;
                break;
            }
            end_index += 1;
        }
        return Ok(BlockParseResult {
            node: Some(Node::Text { content }),
            new_line_index: end_index,
            new_node_count: state.parser.node_count + 1,
            extra_nodes: None,
        });
    }

    if starts_with(trimmed, "-#") {
        if ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_SUBTEXT)
            && let Some(node) = parse_subtext_node(state.parser, trimmed, trimmed_offset)?
        {
            return Ok(BlockParseResult {
                node: Some(node),
                new_line_index: current + 1,
                new_node_count: state.parser.node_count + 1,
                extra_nodes: None,
            });
        }
        return Ok(BlockParseResult {
            node: Some(Node::Text {
                content: line_as_text(&state.lines, current),
            }),
            new_line_index: current + 1,
            new_node_count: state.parser.node_count + 1,
            extra_nodes: None,
        });
    }

    if starts_with(trimmed, "#") {
        if ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_HEADINGS)
            && let Some(node) = parse_heading_node(state.parser, trimmed, trimmed_offset)?
        {
            return Ok(BlockParseResult {
                node: Some(node),
                new_line_index: current + 1,
                new_node_count: state.parser.node_count + 1,
                extra_nodes: None,
            });
        }
        return Ok(no_block(current, state.parser.node_count));
    }

    if trimmed.contains('|')
        && ParserFlags::has(state.parser.flags(), ParserFlags::ALLOW_TABLES)
        && let Some(table) = parse_table(state.parser, &state.lines, current)?
    {
        return Ok(BlockParseResult {
            node: Some(table.node),
            new_line_index: table.new_line_index,
            new_node_count: state.parser.node_count + 1,
            extra_nodes: None,
        });
    }

    Ok(no_block(current, state.parser.node_count))
}

fn no_block(current: usize, node_count: usize) -> BlockParseResult {
    BlockParseResult {
        node: None,
        new_line_index: current,
        new_node_count: node_count,
        extra_nodes: None,
    }
}

pub(crate) fn parse_heading_node(
    parser: &mut MarkdownParser,
    trimmed_line: &str,
    offset: usize,
) -> Result<Option<Node>, ParseError> {
    let mut level = 0usize;
    while level < trimmed_line.len() && level < 4 && byte_at(trimmed_line, level) == b'#' {
        level += 1;
    }
    if (1..=4).contains(&level)
        && level < trimmed_line.len()
        && byte_at(trimmed_line, level) == b' '
    {
        let content = &trimmed_line[level + 1..];
        if !has_visible_content(content) && trim(content).is_empty() {
            return Ok(None);
        }
        let children = crate::inline::parse_inline(parser, content, offset + level + 1)?;
        return Ok(Some(Node::Heading {
            level: level as u8,
            children,
        }));
    }
    Ok(None)
}

fn parse_subtext_node(
    parser: &mut MarkdownParser,
    trimmed_line: &str,
    offset: usize,
) -> Result<Option<Node>, ParseError> {
    if !starts_with(trimmed_line, "-#") {
        return Ok(None);
    }
    if (trimmed_line.len() > 2 && byte_at(trimmed_line, 2) != b' ')
        || (trimmed_line.len() > 3 && byte_at(trimmed_line, 3) == b' ')
    {
        return Ok(None);
    }
    if trimmed_line.len() <= 3 {
        return Ok(None);
    }
    let content = &trimmed_line[3..];
    if !has_visible_content(content) {
        return Ok(None);
    }
    let children = crate::inline::parse_inline(parser, content, offset + 3)?;
    Ok(Some(Node::Subtext { children }))
}

fn parse_blockquote(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
    node_count: usize,
) -> Result<BlockParseResult, ParseError> {
    let mut child_lines = Vec::new();
    let mut content_len = 0usize;
    let mut index = current;
    while index < lines.len() {
        let trimmed_line = trim_start(&lines[index].text);
        let trimmed_offset = lines[index].text.len() - trimmed_line.len();
        let mut child_text = "";
        let mut child_offset = lines[index].offset + trimmed_offset;
        if trimmed_line == "> " || trimmed_line == ">  " {
            child_offset += 2;
        } else if starts_with(trimmed_line, "> ") {
            child_text = &trimmed_line[2..];
            child_offset += 2;
        } else {
            break;
        }
        child_lines.push(Line {
            text: bounded_line_text(child_text).to_owned(),
            offset: child_offset,
        });
        content_len += child_text.len() + 1;
        if content_len > MAX_LINE_LENGTH * 100 {
            break;
        }
        index += 1;
    }
    if child_lines.is_empty() {
        return Ok(no_block(index, node_count));
    }
    if ParserFlags::has(parser.flags(), ParserFlags::ALLOW_ALERTS)
        && let Some(alert) = parse_alert_lines(parser, child_lines.clone())?
    {
        return Ok(BlockParseResult {
            node: Some(alert),
            new_line_index: index,
            new_node_count: node_count + 1,
            extra_nodes: None,
        });
    }
    let mut child_parser =
        parser.child_with_flags(parser.flags() & !ParserFlags::ALLOW_BLOCKQUOTES);
    let mut child_nodes = child_parser.parse_lines(child_lines)?;
    normalize_nodes(&mut child_nodes, true);
    let blank_lines = if child_nodes.is_empty() {
        Some(index.saturating_sub(current))
    } else {
        None
    };
    Ok(BlockParseResult {
        node: Some(Node::Blockquote {
            children: child_nodes,
            blank_lines,
        }),
        new_line_index: index,
        new_node_count: node_count + 1,
        extra_nodes: None,
    })
}

fn parse_multiline_blockquote(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
    node_count: usize,
) -> Result<BlockParseResult, ParseError> {
    let trimmed_line = trim_start(&lines[current].text);
    if !starts_with(trimmed_line, ">>> ") {
        return Ok(BlockParseResult {
            node: Some(Node::Text {
                content: String::new(),
            }),
            new_line_index: current,
            new_node_count: node_count,
            extra_nodes: None,
        });
    }
    let trimmed_offset = lines[current].text.len() - trimmed_line.len();
    let mut child_lines = vec![Line {
        text: bounded_line_text(&trimmed_line[4..]).to_owned(),
        offset: lines[current].offset + trimmed_offset + 4,
    }];
    let mut content_len = trimmed_line[4..].len();
    let mut index = current + 1;
    while index < lines.len() {
        child_lines.push(lines[index].clone());
        content_len += lines[index].text.len() + 1;
        if content_len > MAX_LINE_LENGTH * 100 {
            break;
        }
        index += 1;
    }
    let mut child_parser = parser.child_with_flags(
        (parser.flags() & !ParserFlags::ALLOW_MULTILINE_BLOCKQUOTES)
            | ParserFlags::ALLOW_BLOCKQUOTES,
    );
    let child_nodes = child_parser.parse_lines(child_lines)?;
    let blank_lines = if child_nodes.is_empty() {
        Some(index.saturating_sub(current))
    } else {
        None
    };
    Ok(BlockParseResult {
        node: Some(Node::Blockquote {
            children: child_nodes,
            blank_lines,
        }),
        new_line_index: index,
        new_node_count: node_count + 1,
        extra_nodes: None,
    })
}

fn parse_code_block(
    _parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
) -> Result<Option<CodeBlockResult>, ParseError> {
    if current >= lines.len() {
        return Ok(None);
    }
    let line = &lines[current].text;
    let trimmed_line = trim_start(line);
    let indent_spaces = line.len() - trimmed_line.len();
    let list_indent = &line[..indent_spaces];
    let mut fence_length = 0usize;
    while fence_length < trimmed_line.len() && byte_at(trimmed_line, fence_length) == b'`' {
        fence_length += 1;
    }
    if fence_length < 3 {
        return Ok(None);
    }
    let language_part = &trimmed_line[fence_length..];
    let closing_fence = "`".repeat(fence_length);
    if let Some(closing_index) = language_part.find(&closing_fence) {
        let inline_content = &language_part[..closing_index];
        if !has_visible_content(inline_content) {
            return Ok(None);
        }
        let trailing = &language_part[closing_index + fence_length..];
        return Ok(Some(CodeBlockResult {
            node: Node::CodeBlock {
                language: None,
                content: inline_content.to_owned(),
            },
            new_line_index: if trailing.is_empty() {
                current + 1
            } else {
                current
            },
            extra_content: (!trailing.is_empty()).then(|| ExtraContent {
                content: trailing.to_owned(),
                offset: lines[current].offset + (line.len() - trailing.len()),
            }),
        }));
    }
    let treat_opening_as_language = has_valid_code_fence_language(language_part);
    let language = treat_opening_as_language.then(|| trim(language_part).to_owned());
    let mut temp = current + 1;
    let mut line_count = 0usize;
    let mut has_closing = false;
    while temp < lines.len() {
        if find_closing_fence(trim_start(&lines[temp].text), &closing_fence, fence_length).is_some()
        {
            has_closing = true;
            break;
        }
        line_count += 1;
        if line_count > 1000 {
            break;
        }
        temp += 1;
    }
    if !has_closing {
        return Ok(None);
    }
    let mut content = String::new();
    if !treat_opening_as_language && !language_part.is_empty() {
        content.push_str(language_part);
        content.push('\n');
    }
    let mut index = current + 1;
    while index < lines.len() {
        let current_line = &lines[index].text;
        let trimmed = trim_start(current_line);
        if let Some(closing) = find_closing_fence(trimmed, &closing_fence, fence_length) {
            let absolute_fence = current_line.find(&closing_fence).unwrap_or(0);
            let prefix = &current_line[..absolute_fence];
            let content_line = if indent_spaces > 0 && starts_with(prefix, list_indent) {
                &prefix[indent_spaces..]
            } else {
                prefix
            };
            if !content_line.is_empty() {
                content.push_str(content_line);
                content.push('\n');
            }
            let extra_text = if !closing.trailing_text.is_empty() {
                closing.trailing_text
            } else if closing.backtick_count > fence_length {
                &trimmed[closing.fence_index + fence_length..]
            } else {
                ""
            };
            if !extra_text.is_empty() {
                if !has_visible_content(&content) {
                    return Ok(None);
                }
                return Ok(Some(CodeBlockResult {
                    node: Node::CodeBlock { language, content },
                    new_line_index: index,
                    extra_content: Some(ExtraContent {
                        content: extra_text.to_owned(),
                        offset: lines[index].offset + (current_line.len() - extra_text.len()),
                    }),
                }));
            }
            index += 1;
            break;
        }
        let content_line = if indent_spaces > 0 && starts_with(current_line, list_indent) {
            &current_line[indent_spaces..]
        } else {
            current_line
        };
        content.push_str(content_line);
        content.push('\n');
        if content.len() > MAX_LINE_LENGTH * 100 {
            break;
        }
        index += 1;
    }
    if !has_visible_content(&content) {
        return Ok(None);
    }
    Ok(Some(CodeBlockResult {
        node: Node::CodeBlock { language, content },
        new_line_index: index,
        extra_content: None,
    }))
}

fn find_closing_fence<'a>(
    trimmed_line: &'a str,
    closing_fence: &str,
    fence_length: usize,
) -> Option<ClosingFence<'a>> {
    let fence_index = trimmed_line.find(closing_fence)?;
    let mut count = 0usize;
    let mut idx = fence_index;
    while idx < trimmed_line.len() && byte_at(trimmed_line, idx) == b'`' {
        count += 1;
        idx += 1;
    }
    let after = byte_at(trimmed_line, idx);
    let trailing = &trimmed_line[idx..];
    let only_whitespace_after = after == 0 || matches!(after, b' ' | b'\t' | b'`');
    if count >= fence_length && (only_whitespace_after || trailing.contains(closing_fence)) {
        return Some(ClosingFence {
            fence_index,
            backtick_count: count,
            trailing_text: trailing,
        });
    }
    None
}

fn parse_block_spoiler(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
) -> Result<BlockSpoilerResult, ParseError> {
    let mut found_end = false;
    let mut trailing_content: Option<ExtraContent> = None;
    let mut child_lines = Vec::new();
    let mut content_len = 0usize;
    let mut index = current;
    while index < lines.len() {
        let line = &lines[index].text;
        if index == current {
            if let Some(start) = line.find("||") {
                let child_text = &line[start + 2..];
                child_lines.push(Line {
                    text: bounded_line_text(child_text).to_owned(),
                    offset: lines[index].offset + start + 2,
                });
                content_len += child_text.len() + 1;
            }
        } else if let Some(end) = line.find("||") {
            let child_text = &line[..end];
            child_lines.push(Line {
                text: bounded_line_text(child_text).to_owned(),
                offset: lines[index].offset,
            });
            found_end = true;
            let trailing = &line[end + 2..];
            if !trailing.is_empty() {
                trailing_content = Some(ExtraContent {
                    content: trailing.to_owned(),
                    offset: lines[index].offset + (line.len() - trailing.len()),
                });
            }
            index += 1;
            break;
        } else {
            child_lines.push(lines[index].clone());
            content_len += line.len() + 1;
        }
        if content_len > MAX_LINE_LENGTH * 10 {
            break;
        }
        index += 1;
    }
    if !found_end {
        let content = lines_to_text(&child_lines);
        return Ok(BlockSpoilerResult {
            node: Node::Text {
                content: concat("||", trim_right(&content)),
            },
            new_line_index: index,
            extra_content: None,
        });
    }
    let mut child_parser = parser.child_with_flags(parser.flags());
    let mut window = child_lines;
    if !has_visible_content(&lines_to_text(&window)) {
        return Ok(BlockSpoilerResult {
            node: Node::Text {
                content: lines_to_text(&lines[current..index]),
            },
            new_line_index: index,
            extra_content: None,
        });
    }
    let inner = child_parser.parse_lines(trim_line_window(&mut window))?;
    Ok(BlockSpoilerResult {
        node: Node::Spoiler {
            children: inner,
            is_block: Some(true),
        },
        new_line_index: if trailing_content.is_some() {
            index - 1
        } else {
            index
        },
        extra_content: trailing_content,
    })
}

fn parse_alert_lines(
    parser: &mut MarkdownParser,
    lines: Vec<Line>,
) -> Result<Option<Node>, ParseError> {
    if lines.is_empty() || !starts_with(&lines[0].text, "[!") {
        return Ok(None);
    }
    let Some(close) = lines[0].text.find(']') else {
        return Ok(None);
    };
    let label = &lines[0].text[2..close];
    let Some(alert_type) = alert_type(label) else {
        return Ok(None);
    };

    let mut content_lines = Vec::new();
    let after_label = &lines[0].text[close + 1..];
    let after_trimmed = trim_start_newline_whitespace(after_label);
    if !after_trimmed.is_empty() {
        content_lines.push(Line {
            text: bounded_line_text(after_trimmed).to_owned(),
            offset: lines[0].offset + close + 1 + (after_label.len() - after_trimmed.len()),
        });
    }
    if lines.len() > 1 {
        content_lines.extend(lines[1..].iter().cloned());
    }
    let mut child_parser = parser.child_with_flags(
        (parser.flags() & !ParserFlags::ALLOW_BLOCKQUOTES)
            | ParserFlags::ALLOW_LISTS
            | ParserFlags::ALLOW_HEADINGS,
    );
    let prepared = prepare_alert_content_lines(content_lines);
    let mut children = child_parser.parse_lines(prepared)?;
    children = post_process_alert_nodes(children);
    Ok(Some(Node::Alert {
        alert_type,
        children,
    }))
}

fn prepare_alert_content_lines(mut lines: Vec<Line>) -> Vec<Line> {
    let mut out = Vec::new();
    let mut blank_run = 0usize;
    for line in lines.drain(..) {
        let is_list_like = is_alert_list_like_line(trim(&line.text));
        let mut prepared = line;
        if is_list_like {
            prepared.text = bounded_line_text(&prepared.text).to_owned();
        } else {
            let left_trimmed = trim_start(&prepared.text);
            let leading_bytes = prepared.text.len() - left_trimmed.len();
            prepared.offset += leading_bytes;
            prepared.text = bounded_line_text(trim_right(left_trimmed)).to_owned();
        }
        if trim(&prepared.text).is_empty() {
            blank_run += 1;
            if blank_run > 2 {
                continue;
            }
            prepared.text.clear();
        } else {
            blank_run = 0;
        }
        out.push(prepared);
    }
    trim_line_window(&mut out)
}

fn is_alert_list_like_line(trimmed_line: &str) -> bool {
    if starts_with(trimmed_line, "-") {
        return true;
    }
    let mut index = 0usize;
    while index < trimmed_line.len() && byte_at(trimmed_line, index).is_ascii_digit() {
        index += 1;
    }
    index > 0 && index < trimmed_line.len() && byte_at(trimmed_line, index) == b'.'
}

fn post_process_alert_nodes(nodes: Vec<Node>) -> Vec<Node> {
    if nodes.len() <= 1 {
        return nodes;
    }
    let mut out = Vec::new();
    let mut index = 0usize;
    while index < nodes.len() {
        let node = nodes[index].clone();
        if let Node::Text { content } = &node
            && index + 1 < nodes.len()
            && matches!(nodes[index + 1], Node::List { .. })
        {
            let content = replace_trailing_whitespace_with_newline(content);
            if !content.is_empty() {
                out.push(Node::Text { content });
            }
            index += 1;
            continue;
        }
        if matches!(node, Node::List { .. }) && index + 1 < nodes.len() {
            out.push(node);
            if let Node::Text { content } = &nodes[index + 1] {
                let content = trim(content);
                if !content.is_empty() {
                    out.push(Node::Text {
                        content: concat("\n", content),
                    });
                    index += 2;
                    continue;
                }
            }
            index += 1;
            continue;
        }
        out.push(node);
        index += 1;
    }
    out
}

fn parse_list(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
    ordered: bool,
    indent_level: usize,
    depth: usize,
    node_count: usize,
) -> Result<ListResult, ParseError> {
    let mut items = Vec::new();
    let mut index = current;
    let mut new_count = node_count;
    while index < lines.len() {
        if new_count > MAX_AST_NODES {
            break;
        }
        let current_line = &lines[index].text;
        let trimmed_line = trim_start(current_line);
        if starts_with(trimmed_line, "#") || is_blockquote_start(trimmed_line, parser.flags()) {
            break;
        }
        if let Some(item) = match_list_item(current_line) {
            let ordinal = normalise_ordinal(&items, item.ordinal, ordered);
            if item.indent_level < indent_level {
                break;
            }
            if item.indent_level == indent_level {
                if item.ordered != ordered {
                    break;
                }
                let mut children = Vec::new();
                if let Some(inline_item) = match_list_item(item.content) {
                    let mut nested_items = Vec::new();
                    let inline_nodes = crate::inline::parse_inline(
                        parser,
                        inline_item.content,
                        lines[index].offset + (current_line.len() - inline_item.content.len()),
                    )?;
                    nested_items.push(ListItem {
                        children: inline_nodes,
                        ordinal: None,
                    });
                    if let Some(nested) = try_parse_nested_list(
                        parser,
                        lines,
                        index + 1,
                        indent_level,
                        depth,
                        new_count,
                    )? {
                        if let Node::List { items, .. } = nested.node {
                            nested_items.extend(items);
                            index = nested.new_line_index;
                            new_count = nested.new_node_count;
                        } else {
                            index += 1;
                            new_count += 1;
                        }
                    } else {
                        index += 1;
                        new_count += 1;
                    }
                    children.push(Node::List {
                        ordered: inline_item.ordered,
                        items: nested_items,
                    });
                } else {
                    let content_nodes = crate::inline::parse_inline(
                        parser,
                        item.content,
                        lines[index].offset + (current_line.len() - item.content.len()),
                    )?;
                    children.extend(content_nodes.iter().cloned());
                    if let Some(nested) = try_parse_nested_list(
                        parser,
                        lines,
                        index + 1,
                        indent_level,
                        depth,
                        new_count,
                    )? {
                        children.push(nested.node);
                        index = nested.new_line_index;
                        new_count = nested.new_node_count;
                    } else {
                        index += 1;
                        new_count += content_nodes.len();
                    }
                }
                items.push(ListItem { children, ordinal });
            } else if item.indent_level == indent_level + 1 && depth < 9 {
                let nested = parse_list(
                    parser,
                    lines,
                    index,
                    item.ordered,
                    item.indent_level,
                    depth + 1,
                    new_count,
                )?;
                if let Some(last) = items.last_mut() {
                    last.children.push(nested.node);
                }
                index = nested.new_line_index;
                new_count = nested.new_node_count;
            } else {
                break;
            }
        } else if is_bullet_point_text(current_line) {
            if let Some(last) = items.last_mut() {
                last.children.push(Node::Text {
                    content: trim(current_line).to_owned(),
                });
            }
            index += 1;
            new_count += 1;
        } else if is_list_continuation(current_line, indent_level) {
            if let Some(last) = items.last_mut() {
                let trimmed = trim_start(current_line);
                let parsed = crate::inline::parse_inline(
                    parser,
                    trimmed,
                    lines[index].offset + (current_line.len() - trimmed.len()),
                )?;
                last.children.extend(parsed.iter().cloned());
                new_count += parsed.len();
            }
            index += 1;
        } else {
            break;
        }
        if items.len() > MAX_AST_NODES {
            break;
        }
    }
    Ok(ListResult {
        node: Node::List { ordered, items },
        new_line_index: index,
        new_node_count: new_count,
    })
}

fn try_parse_nested_list(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
    parent_indent: usize,
    depth: usize,
    node_count: usize,
) -> Result<Option<ListResult>, ParseError> {
    if current >= lines.len() {
        return Ok(None);
    }
    let trimmed = trim_start(&lines[current].text);
    if starts_with(trimmed, "```")
        && let Some(result) = parse_code_block(parser, lines, current)?
    {
        return Ok(Some(ListResult {
            node: result.node,
            new_line_index: result.new_line_index,
            new_node_count: node_count + 1,
        }));
    }
    if let Some(item) = match_list_item(&lines[current].text)
        && item.indent_level > parent_indent
        && depth < 9
    {
        return parse_list(
            parser,
            lines,
            current,
            item.ordered,
            item.indent_level,
            depth + 1,
            node_count,
        )
        .map(Some);
    }
    Ok(None)
}

pub(crate) fn match_list_item(line: &str) -> Option<ListMatch<'_>> {
    let mut indent = 0usize;
    while indent < line.len() && byte_at(line, indent) == b' ' {
        indent += 1;
    }
    if indent > 0 && indent < 2 {
        return None;
    }
    let indent_level = indent / 2;
    if indent >= line.len() {
        return None;
    }
    let marker = byte_at(line, indent);
    if matches!(marker, b'*' | b'-') && indent + 1 < line.len() && byte_at(line, indent + 1) == b' '
    {
        return Some(ListMatch {
            ordered: false,
            indent_level,
            content: &line[indent + 2..],
            ordinal: None,
        });
    }
    if marker.is_ascii_digit() {
        let mut pos = indent;
        while pos < line.len() && byte_at(line, pos).is_ascii_digit() {
            pos += 1;
        }
        if pos < line.len()
            && byte_at(line, pos) == b'.'
            && pos + 1 < line.len()
            && byte_at(line, pos + 1) == b' '
        {
            return Some(ListMatch {
                ordered: true,
                indent_level,
                content: &line[pos + 2..],
                ordinal: line[indent..pos].parse::<usize>().ok().or(Some(1)),
            });
        }
    }
    None
}

fn parse_table(
    parser: &mut MarkdownParser,
    lines: &[Line],
    current: usize,
) -> Result<Option<TableResult>, ParseError> {
    if current + 2 >= lines.len() {
        return Ok(None);
    }
    let header_line = trim_table_line(&lines[current]);
    let align_line = trim_table_line(&lines[current + 1]);
    if !header_line.text.contains('|') || !align_line.text.contains('|') {
        return Ok(None);
    }
    let header_cells = split_table_cells(&header_line);
    if header_cells.is_empty() || !cells_have_content(&header_cells) {
        return Ok(None);
    }
    let align_cells = split_table_cells(&align_line);
    let Some(alignments) = parse_alignments(&align_cells) else {
        return Ok(None);
    };
    if header_cells.len() != alignments.len() {
        return Ok(None);
    }
    let header_row = create_table_row(parser, &header_cells)?;
    let column_count = header_cells.len();
    let mut rows = Vec::new();
    let mut index = current + 2;
    while index < lines.len() {
        let line = trim_table_line(&lines[index]);
        if !line.text.contains('|') || is_table_block_break(&line.text) {
            break;
        }
        let cells = normalize_column_count(split_table_cells(&line), column_count);
        rows.push(create_table_row(parser, &cells)?);
        index += 1;
    }
    if rows.is_empty() {
        return Ok(None);
    }
    Ok(Some(TableResult {
        node: Node::Table {
            header: Box::new(header_row),
            alignments,
            rows,
        },
        new_line_index: index,
    }))
}

fn trim_table_line(line: &Line) -> TableCellText {
    let left = trim_start(&line.text);
    let leading = line.text.len() - left.len();
    TableCellText {
        text: trim_right(left).to_owned(),
        offset: line.offset + leading,
    }
}

fn trim_table_cell(cell: &TableCellText) -> TableCellText {
    let left = trim_start(&cell.text);
    let leading = cell.text.len() - left.len();
    TableCellText {
        text: trim_right(left).to_owned(),
        offset: cell.offset + leading,
    }
}

fn split_table_cells(source: &TableCellText) -> Vec<TableCellText> {
    let line = &source.text;
    let mut start = 0usize;
    let mut end = line.len();
    if line.starts_with('|') {
        start = 1;
    }
    if end > start && line.ends_with('|') {
        end -= 1;
    }
    let mut cells = Vec::new();
    if start >= end {
        return cells;
    }
    let mut cell = String::new();
    let mut cell_offset = source.offset + start;
    let mut i = start;
    while i < end {
        if byte_at(line, i) == b'\\' && i + 1 < end && byte_at(line, i + 1) == b'|' {
            cell.push('|');
            i += 2;
            continue;
        }
        if byte_at(line, i) == b'|' {
            cells.push(TableCellText {
                text: std::mem::take(&mut cell),
                offset: cell_offset,
            });
            i += 1;
            cell_offset = source.offset + i;
            continue;
        }
        let ch = line[i..].chars().next().unwrap_or('\0');
        cell.push(ch);
        i += ch.len_utf8();
    }
    cells.push(TableCellText {
        text: cell,
        offset: cell_offset,
    });
    cells
}

fn parse_alignments(cells: &[TableCellText]) -> Option<Vec<TableAlignment>> {
    if cells.is_empty() {
        return None;
    }
    let mut alignments = Vec::with_capacity(cells.len());
    for cell in cells {
        let value = trim(&cell.text);
        if value.is_empty() || !value.contains('-') {
            return None;
        }
        if !value
            .bytes()
            .all(|c| matches!(c, b' ' | b':' | b'-' | b'|'))
        {
            return None;
        }
        let left = value.starts_with(':');
        let right = value.ends_with(':');
        alignments.push(if left && right {
            TableAlignment::Center
        } else if left {
            TableAlignment::Left
        } else if right {
            TableAlignment::Right
        } else {
            TableAlignment::None
        });
    }
    Some(alignments)
}

fn create_table_row(
    parser: &mut MarkdownParser,
    cells: &[TableCellText],
) -> Result<Node, ParseError> {
    let mut out = Vec::new();
    for cell in cells {
        let trimmed_cell = trim_table_cell(cell);
        let value = trimmed_cell.text;
        let parsed = crate::inline::parse_inline(parser, &value, trimmed_cell.offset)?;
        let children = if parsed.is_empty() {
            vec![Node::Text { content: value }]
        } else {
            parsed
        };
        out.push(Node::TableCell { children });
    }
    Ok(Node::TableRow { cells: out })
}

pub(crate) fn is_block_start(line: &str, flags: u32) -> bool {
    starts_with(line, "#")
        || (ParserFlags::has(flags, ParserFlags::ALLOW_SUBTEXT) && starts_with(line, "-#"))
        || (ParserFlags::has(flags, ParserFlags::ALLOW_CODE_BLOCKS) && starts_with(line, "```"))
        || (ParserFlags::has(flags, ParserFlags::ALLOW_LISTS) && match_list_item(line).is_some())
        || is_blockquote_start(line, flags)
}

pub(crate) fn is_table_start(lines: &[Line], index: usize, flags: u32) -> bool {
    if !ParserFlags::has(flags, ParserFlags::ALLOW_TABLES) || index + 2 >= lines.len() {
        return false;
    }
    let header_line = trim_table_line(&lines[index]);
    let align_line = trim_table_line(&lines[index + 1]);
    if !header_line.text.contains('|') || !align_line.text.contains('|') {
        return false;
    }
    let header_cells = split_table_cells(&header_line);
    if header_cells.is_empty() || !cells_have_content(&header_cells) {
        return false;
    }
    let Some(alignments) = parse_alignments(&split_table_cells(&align_line)) else {
        return false;
    };
    if header_cells.len() != alignments.len() {
        return false;
    }
    let body_line = trim_table_line(&lines[index + 2]);
    body_line.text.contains('|') && !is_table_block_break(&body_line.text)
}

pub(crate) fn is_heading_start(line: &str, flags: u32) -> bool {
    if !ParserFlags::has(flags, ParserFlags::ALLOW_HEADINGS) || !starts_with(line, "#") {
        return false;
    }
    let mut level = 0usize;
    while level < line.len() && level < 4 && byte_at(line, level) == b'#' {
        level += 1;
    }
    (1..=4).contains(&level) && level < line.len() && byte_at(line, level) == b' '
}

pub(crate) fn is_blockquote_start(line: &str, flags: u32) -> bool {
    (ParserFlags::has(flags, ParserFlags::ALLOW_MULTILINE_BLOCKQUOTES) && starts_with(line, ">>> "))
        || (ParserFlags::has(flags, ParserFlags::ALLOW_BLOCKQUOTES) && starts_with(line, "> "))
}

fn alert_type(label: &str) -> Option<AlertType> {
    if label.eq_ignore_ascii_case("NOTE") {
        Some(AlertType::Note)
    } else if label.eq_ignore_ascii_case("TIP") {
        Some(AlertType::Tip)
    } else if label.eq_ignore_ascii_case("IMPORTANT") {
        Some(AlertType::Important)
    } else if label.eq_ignore_ascii_case("WARNING") {
        Some(AlertType::Warning)
    } else if label.eq_ignore_ascii_case("CAUTION") {
        Some(AlertType::Caution)
    } else {
        None
    }
}

fn slice_lines_from_fence(lines: &[Line], current: usize, fence_pos: usize) -> Vec<Line> {
    let mut out = Vec::with_capacity(lines.len() - current);
    out.push(Line {
        text: lines[current].text[fence_pos..].to_owned(),
        offset: lines[current].offset + fence_pos,
    });
    out.extend(lines[current + 1..].iter().cloned());
    out
}

fn normalise_ordinal(items: &[ListItem], ordinal: Option<usize>, ordered: bool) -> Option<usize> {
    if !ordered {
        return None;
    }
    if items.is_empty() {
        return ordinal.or(Some(1));
    }
    let start = items[0].ordinal.or(ordinal).unwrap_or(1);
    Some(start + items.len())
}

fn is_bullet_point_text(text: &str) -> bool {
    if match_list_item(text).is_some() {
        return false;
    }
    let trimmed_text = trim_start(text);
    starts_with(trimmed_text, "- ") && !starts_with(text, "  ")
}

fn is_list_continuation(line: &str, indent_level: usize) -> bool {
    let mut spaces = 0usize;
    while spaces < line.len() && byte_at(line, spaces) == b' ' {
        spaces += 1;
    }
    spaces > indent_level * 2
}

fn cells_have_content(cells: &[TableCellText]) -> bool {
    cells.iter().any(|cell| !trim(&cell.text).is_empty())
}

fn normalize_column_count(cells: Vec<TableCellText>, expected: usize) -> Vec<TableCellText> {
    if cells.len() == expected {
        return cells;
    }
    let mut out = Vec::with_capacity(expected);
    for index in 0..expected {
        if let Some(cell) = cells.get(index) {
            out.push(cell.clone());
        } else {
            out.push(TableCellText {
                text: String::new(),
                offset: 0,
            });
        }
    }
    if cells.len() > expected && expected > 0 {
        let mut combined = out[expected - 1].clone();
        for extra in &cells[expected..] {
            combined.text = concat3(&combined.text, "|", &extra.text);
        }
        out[expected - 1] = combined;
    }
    out
}

fn is_table_block_break(text: &str) -> bool {
    if text.is_empty() {
        return false;
    }
    let first = byte_at(text, 0);
    if matches!(first, b'#' | b'>' | b'-' | b'*') {
        return true;
    }
    if text.len() >= 2 && byte_at(text, 0) == b'-' && byte_at(text, 1) == b'#' {
        return true;
    }
    if first.is_ascii_digit() {
        for i in 1..text.len().min(4) {
            if byte_at(text, i) == b'.' {
                return true;
            }
        }
    }
    false
}
