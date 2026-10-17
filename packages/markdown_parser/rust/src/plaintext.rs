// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;

use crate::ast::{
    AlertType, EmojiKind, GuildNavigationType, ListItem, MentionKind, Node, TableAlignment,
};
use crate::emoji::EmojiContext;
use crate::parser::{MarkdownParser, ParseError};

#[derive(Clone, Debug)]
pub struct PlaintextOptions {
    pub preserve_markdown: bool,
    pub include_emoji_names: bool,
    pub include_link_urls: bool,
    pub users: HashMap<String, String>,
    pub roles: HashMap<String, String>,
    pub channels: HashMap<String, String>,
}

impl Default for PlaintextOptions {
    fn default() -> Self {
        Self {
            preserve_markdown: false,
            include_emoji_names: true,
            include_link_urls: false,
            users: HashMap::new(),
            roles: HashMap::new(),
            channels: HashMap::new(),
        }
    }
}

pub fn parse_and_render_plaintext(
    content: &str,
    parser_flags: u32,
    emoji_context: &str,
    options: &PlaintextOptions,
) -> Result<String, ParseError> {
    let context = EmojiContext::parse(emoji_context);
    let mut parser = MarkdownParser::new(parser_flags, context);
    let nodes = parser.parse(content)?;
    Ok(render_ast_to_plaintext(&nodes, options))
}

pub fn render_ast_to_plaintext(nodes: &[Node], options: &PlaintextOptions) -> String {
    render_to_plaintext(nodes, options)
}

fn is_block_node(node: &Node) -> bool {
    matches!(
        node,
        Node::Alert { .. }
            | Node::Blockquote { .. }
            | Node::CodeBlock { .. }
            | Node::Heading { .. }
            | Node::List { .. }
            | Node::Subtext { .. }
            | Node::Table { .. }
            | Node::TableRow { .. }
            | Node::Spoiler {
                is_block: Some(true),
                ..
            }
    )
}

fn join_with_visible_line_breaks(parts: Vec<String>) -> String {
    parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn indent_continuation_lines(text: &str, width: usize) -> String {
    let mut lines = text.split('\n');
    let Some(first) = lines.next() else {
        return String::new();
    };
    let indent = " ".repeat(width);
    let mut result = String::from(first);
    for line in lines {
        result.push('\n');
        result.push_str(&indent);
        result.push_str(line);
    }
    result
}

fn render_node_to_plaintext(node: &Node, options: &PlaintextOptions) -> String {
    match node {
        Node::Text { content } => content.clone(),
        Node::Strong { children } => render_formatting_node(children, "**", options),
        Node::Emphasis { children } => render_formatting_node(children, "*", options),
        Node::Underline { children } => render_formatting_node(children, "__", options),
        Node::Strikethrough { children } => render_formatting_node(children, "~~", options),
        Node::Spoiler { children, is_block } => {
            let content = render_nodes_to_plaintext(children, options);
            if options.preserve_markdown {
                if is_block.unwrap_or(false) {
                    return format!("||\n{content}\n||");
                }
                return format!("||{content}||");
            }
            content
        }
        Node::Heading { level, children } => {
            let content = render_nodes_to_plaintext(children, options);
            if options.preserve_markdown {
                format!("{} {content}", "#".repeat((*level).into()))
            } else {
                content
            }
        }
        Node::Subtext { children } => render_nodes_to_plaintext(children, options),
        Node::List { ordered, items } => render_list(*ordered, items, options),
        Node::CodeBlock { language, content } => {
            if !options.preserve_markdown {
                return content.clone();
            }
            let mut normalized = content.clone();
            if !normalized.ends_with('\n') {
                normalized.push('\n');
            }
            format!("```{}\n{normalized}```", language.as_deref().unwrap_or(""))
        }
        Node::InlineCode { content } => {
            if options.preserve_markdown {
                format!("`{content}`")
            } else {
                content.clone()
            }
        }
        Node::Link { text, url, .. } => render_link(text.as_deref(), url, options),
        Node::Mention { kind } => render_mention_to_plaintext(kind, options),
        Node::Timestamp { timestamp, .. } => timestamp.to_string(),
        Node::Emoji { kind } => render_emoji_to_plaintext(kind, options),
        Node::Blockquote {
            children,
            blank_lines,
        } => {
            let content = render_nodes_to_plaintext(children, options);
            if options.preserve_markdown {
                if children.is_empty() {
                    let line_count = blank_lines.unwrap_or(1).max(1);
                    return std::iter::repeat_n("> ".to_owned(), line_count)
                        .collect::<Vec<_>>()
                        .join("\n");
                }
                return content
                    .split('\n')
                    .map(|line| format!("> {line}"))
                    .collect::<Vec<_>>()
                    .join("\n");
            }
            content
        }
        Node::Sequence { children } => render_nodes_to_plaintext(children, options),
        Node::Table {
            header,
            alignments,
            rows,
        } => render_table_node_to_markdown(header, alignments, rows, options),
        Node::Alert {
            alert_type,
            children,
        } => {
            let content = render_nodes_to_plaintext(children, options);
            let label = render_alert_label(*alert_type);
            if options.preserve_markdown {
                return format!(
                    "> [!{}]\n{}",
                    alert_type_name(*alert_type).to_ascii_uppercase(),
                    content
                        .split('\n')
                        .map(|line| format!("> {line}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                );
            }
            if content.is_empty() {
                label.to_owned()
            } else {
                format!("{label}\n{content}")
            }
        }
        Node::TableRow { cells } => render_table_row_to_plaintext(cells, options),
        Node::TableCell { children } => render_table_cell_to_plaintext(children, options),
    }
}

fn render_formatting_node(children: &[Node], marker: &str, options: &PlaintextOptions) -> String {
    let content = render_nodes_to_plaintext(children, options);
    if options.preserve_markdown {
        format!("{marker}{content}{marker}")
    } else {
        content
    }
}

fn render_list(ordered: bool, items: &[ListItem], options: &PlaintextOptions) -> String {
    let start_ordinal = items.first().and_then(|item| item.ordinal).unwrap_or(1);
    items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            let content = render_nodes_to_plaintext(&item.children, options)
                .trim()
                .to_owned();
            let prefix = if ordered {
                format!("{}. ", start_ordinal + index)
            } else if options.preserve_markdown {
                "- ".to_owned()
            } else {
                "\u{2022} ".to_owned()
            };
            let checkbox = if let Some(checked) = item.checked {
                if checked {
                    "[x] "
                } else {
                    "[ ] "
                }
            } else {
                ""
            };
            format!(
                "{prefix}{checkbox}{}",
                indent_continuation_lines(&content, prefix.len())
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_link(text: Option<&Node>, url: &str, options: &PlaintextOptions) -> String {
    let Some(text_node) = text else {
        return url.to_owned();
    };
    let link_text = render_node_to_plaintext(text_node, options);
    if options.preserve_markdown {
        return format!("[{link_text}]({url})");
    }
    if options.include_link_urls && normalize_url(&link_text) != normalize_url(url) {
        return format!("{link_text} ({url})");
    }
    link_text
}

fn normalize_url(value: &str) -> Option<String> {
    if value.is_empty() || !value.contains("://") {
        return None;
    }
    Some(value.trim_end_matches('/').to_owned())
}

fn render_table_row_to_plaintext(cells: &[Node], options: &PlaintextOptions) -> String {
    cells
        .iter()
        .map(|cell| render_node_to_plaintext(cell, options))
        .collect::<Vec<_>>()
        .join(" | ")
}

fn render_table_cell_to_plaintext(children: &[Node], options: &PlaintextOptions) -> String {
    collapse_table_cell_whitespace(&render_nodes_to_plaintext(children, options))
}

fn collapse_table_cell_whitespace(value: &str) -> String {
    let mut result = String::new();
    let mut pending_space = false;
    for ch in value.trim().chars() {
        if ch == '\n' || ch == '\r' || ch == '\t' || ch == ' ' {
            pending_space = true;
            continue;
        }
        if pending_space && !result.is_empty() {
            result.push(' ');
        }
        pending_space = false;
        result.push(ch);
    }
    result
}

fn render_table_node_to_markdown(
    header: &Node,
    alignments: &[TableAlignment],
    rows: &[Node],
    options: &PlaintextOptions,
) -> String {
    let header_cells = match header {
        Node::TableRow { cells } => cells
            .iter()
            .map(|cell| render_node_to_plaintext(cell, options))
            .collect::<Vec<_>>(),
        _ => vec![render_node_to_plaintext(header, options)],
    };
    let body_rows = rows
        .iter()
        .map(|row| match row {
            Node::TableRow { cells } => cells
                .iter()
                .map(|cell| render_node_to_plaintext(cell, options))
                .collect::<Vec<_>>(),
            _ => vec![render_node_to_plaintext(row, options)],
        })
        .collect::<Vec<_>>();
    let body_column_count = body_rows.iter().map(Vec::len).max().unwrap_or(0);
    let column_count = header_cells
        .len()
        .max(body_column_count)
        .max(alignments.len())
        .max(1);
    let normal_header = pad_table_row_cells(header_cells, column_count);
    let separator = (0..column_count)
        .map(|index| format_markdown_table_separator(alignments.get(index).copied()))
        .collect::<Vec<_>>();
    let mut parts = vec![
        format_markdown_table_row(&normal_header),
        format_markdown_table_row(&separator),
    ];
    for row in body_rows {
        parts.push(format_markdown_table_row(&pad_table_row_cells(
            row,
            column_count,
        )));
    }
    join_with_visible_line_breaks(parts)
}

fn pad_table_row_cells(mut cells: Vec<String>, column_count: usize) -> Vec<String> {
    cells.resize(column_count, String::new());
    cells
}

fn escape_markdown_table_cell(value: &str) -> String {
    value.replace('|', "\\|")
}

fn format_markdown_table_row(cells: &[String]) -> String {
    format!(
        "| {} |",
        cells
            .iter()
            .map(|cell| escape_markdown_table_cell(cell))
            .collect::<Vec<_>>()
            .join(" | ")
    )
}

fn format_markdown_table_separator(alignment: Option<TableAlignment>) -> String {
    match alignment {
        Some(TableAlignment::Left) => ":---",
        Some(TableAlignment::Center) => ":---:",
        Some(TableAlignment::Right) => "---:",
        _ => "---",
    }
    .to_owned()
}

fn render_alert_label(alert_type: AlertType) -> &'static str {
    match alert_type {
        AlertType::Tip => "Tip",
        AlertType::Important => "Important",
        AlertType::Warning => "Warning",
        AlertType::Caution => "Caution",
        AlertType::Note => "Note",
    }
}

fn alert_type_name(alert_type: AlertType) -> &'static str {
    render_alert_label(alert_type)
}

fn render_mention_to_plaintext(kind: &MentionKind, options: &PlaintextOptions) -> String {
    match kind {
        MentionKind::User { id } => options
            .users
            .get(id)
            .map_or_else(|| format!("@{id}"), |name| format!("@{name}")),
        MentionKind::Role { id } => options
            .roles
            .get(id)
            .map_or_else(|| "@unknown-role".to_owned(), |name| format!("@{name}")),
        MentionKind::Channel { id } => options
            .channels
            .get(id)
            .map_or_else(|| "#unknown-channel".to_owned(), |name| format!("#{name}")),
        MentionKind::Everyone => "@everyone".to_owned(),
        MentionKind::Here => "@here".to_owned(),
        MentionKind::Command {
            name,
            subcommand_group,
            subcommand,
            ..
        } => {
            let mut command = format!("/{name}");
            if let Some(group) = subcommand_group {
                command.push(' ');
                command.push_str(group);
            }
            if let Some(value) = subcommand {
                command.push(' ');
                command.push_str(value);
            }
            command
        }
        MentionKind::GuildNavigation {
            navigation_type,
            id,
            ..
        } => match navigation_type {
            GuildNavigationType::Customize => "#customize".to_owned(),
            GuildNavigationType::Browse => "#browse".to_owned(),
            GuildNavigationType::Guide => "#guide".to_owned(),
            GuildNavigationType::LinkedRoles => id.as_ref().map_or_else(
                || "#linked-roles".to_owned(),
                |value| format!("#linked-roles:{value}"),
            ),
        },
    }
}

fn render_emoji_to_plaintext(kind: &EmojiKind, options: &PlaintextOptions) -> String {
    match kind {
        EmojiKind::Standard { raw, .. } => raw.clone(),
        EmojiKind::Custom { name, .. } if options.include_emoji_names => format!(":{name}:"),
        EmojiKind::Custom { .. } => String::new(),
    }
}

fn render_nodes_to_plaintext(nodes: &[Node], options: &PlaintextOptions) -> String {
    let mut result = String::new();
    let mut previous_node: Option<&Node> = None;
    for node in nodes {
        let rendered = render_node_to_plaintext(node, options);
        if rendered.is_empty() {
            previous_node = Some(node);
            continue;
        }
        let needs_block_separator = !result.is_empty()
            && previous_node.is_some()
            && (previous_node.is_some_and(is_block_node) || is_block_node(node))
            && !result.ends_with('\n')
            && !rendered.starts_with('\n');
        if needs_block_separator {
            trim_trailing_horizontal_whitespace_for_block_separator(
                &mut result,
                options.preserve_markdown,
            );
            result.push('\n');
            result.push_str(&rendered);
        } else {
            result.push_str(&rendered);
        }
        previous_node = Some(node);
    }
    result
}

fn render_to_plaintext(nodes: &[Node], options: &PlaintextOptions) -> String {
    let rendered = render_nodes_to_plaintext(nodes, options);
    let whitespace_normalized = if options.preserve_markdown {
        trim_horizontal_whitespace_before_newlines_preserving_empty_blockquotes(&rendered)
    } else {
        trim_horizontal_whitespace_before_newlines(&rendered)
    };
    trim_newlines(&collapse_excess_newlines(&whitespace_normalized))
}

fn trim_trailing_horizontal_whitespace(value: &mut String) {
    while value.ends_with(' ') || value.ends_with('\t') {
        value.pop();
    }
}

fn trim_trailing_horizontal_whitespace_for_block_separator(
    value: &mut String,
    preserve_markdown: bool,
) {
    if preserve_markdown && value.rsplit('\n').next() == Some("> ") {
        return;
    }
    trim_trailing_horizontal_whitespace(value);
}

fn trim_horizontal_whitespace_before_newlines(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    let mut pending_horizontal = String::new();
    for ch in value.chars() {
        if ch == ' ' || ch == '\t' {
            pending_horizontal.push(ch);
            continue;
        }
        if ch != '\n' {
            result.push_str(&pending_horizontal);
        }
        pending_horizontal.clear();
        result.push(ch);
    }
    result.push_str(&pending_horizontal);
    result
}

fn trim_horizontal_whitespace_before_newlines_preserving_empty_blockquotes(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    let mut lines = value.split('\n').peekable();
    while let Some(line) = lines.next() {
        if lines.peek().is_some() {
            if line == "> " {
                result.push_str(line);
            } else {
                result.push_str(line.trim_end_matches([' ', '\t']));
            }
            result.push('\n');
        } else {
            result.push_str(line);
        }
    }
    result
}

fn collapse_excess_newlines(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    let mut newline_count = 0usize;
    for ch in value.chars() {
        if ch == '\n' {
            newline_count += 1;
            if newline_count <= 2 {
                result.push(ch);
            }
        } else {
            newline_count = 0;
            result.push(ch);
        }
    }
    result
}

fn trim_newlines(value: &str) -> String {
    value.trim_matches('\n').to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ParserFlags;

    #[test]
    fn preserves_notification_markdown_and_resolves_mentions() {
        let mut users = HashMap::new();
        users.insert("1".to_owned(), "Alice".to_owned());
        let mut roles = HashMap::new();
        roles.insert("2".to_owned(), "Ops".to_owned());
        let mut channels = HashMap::new();
        channels.insert("3".to_owned(), "alerts".to_owned());
        let options = PlaintextOptions {
            preserve_markdown: true,
            users,
            roles,
            channels,
            ..PlaintextOptions::default()
        };

        let result = parse_and_render_plaintext(
            "**hi** <@1> <@&2> <#3> [site](https://fluxer.app)",
            ParserFlags::ALL,
            "",
            &options,
        )
        .unwrap();

        assert_eq!(
            result,
            "**hi** @Alice @Ops #alerts [site](https://fluxer.app)"
        );

        let empty_blockquote_result = parse_and_render_plaintext(
            "> \n>  \nsome text",
            ParserFlags::ALLOW_BLOCKQUOTES,
            "",
            &options,
        )
        .unwrap();

        assert_eq!(empty_blockquote_result, "> \n> \nsome text");
    }
}
