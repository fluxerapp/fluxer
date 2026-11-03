// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ast::Node;
use crate::text::{concat, remove_text_presentation, trim};

pub fn normalize_nodes(nodes: &mut Vec<Node>, inside_blockquote: bool) {
    for node in nodes.iter_mut() {
        normalize_node(node);
    }
    combine_adjacent_text(nodes, inside_blockquote);
}

pub fn normalize_child_nodes(nodes: &mut Vec<Node>, inside_blockquote: bool) {
    normalize_nodes(nodes, inside_blockquote);
    merge_adjacent_text_simple(nodes);
    compact_empty_text_nodes(nodes);
}

pub fn normalize_node(node: &mut Node) {
    match node {
        Node::Text { content } => remove_text_presentation(content),
        Node::Strong { children }
        | Node::Emphasis { children }
        | Node::Underline { children }
        | Node::Strikethrough { children }
        | Node::Subtext { children }
        | Node::Sequence { children }
        | Node::TableCell { children }
        | Node::Alert { children, .. } => normalize_child_nodes(children, false),
        Node::Spoiler { children, .. } => normalize_child_nodes(children, false),
        Node::Heading { children, .. } => normalize_child_nodes(children, false),
        Node::Blockquote { children, .. } => normalize_child_nodes(children, true),
        Node::List { items, .. } => {
            for item in items {
                normalize_child_nodes(&mut item.children, false);
            }
        }
        Node::Link { text, .. } => {
            if let Some(child) = text {
                normalize_node(child);
            }
        }
        Node::Table { header, rows, .. } => {
            normalize_node(header);
            for row in rows {
                normalize_node(row);
            }
        }
        Node::TableRow { cells } => {
            for cell in cells {
                normalize_node(cell);
            }
        }
        Node::CodeBlock { .. }
        | Node::InlineCode { .. }
        | Node::Mention { .. }
        | Node::Timestamp { .. }
        | Node::Emoji { .. } => {}
    }
}

pub fn apply_text_presentation_node(node: &mut Node) {
    match node {
        Node::Text { content } => remove_text_presentation(content),
        Node::Strong { children }
        | Node::Emphasis { children }
        | Node::Underline { children }
        | Node::Strikethrough { children }
        | Node::Subtext { children }
        | Node::Sequence { children }
        | Node::TableCell { children }
        | Node::Alert { children, .. } => {
            for child in children {
                apply_text_presentation_node(child);
            }
        }
        Node::Spoiler { children, .. } => {
            for child in children {
                apply_text_presentation_node(child);
            }
        }
        Node::Heading { children, .. } => {
            for child in children {
                apply_text_presentation_node(child);
            }
        }
        Node::Blockquote { children, .. } => {
            for child in children {
                apply_text_presentation_node(child);
            }
        }
        Node::List { items, .. } => {
            for item in items {
                for child in &mut item.children {
                    apply_text_presentation_node(child);
                }
            }
        }
        Node::Link { text, .. } => {
            if let Some(child) = text {
                apply_text_presentation_node(child);
            }
        }
        Node::Table { header, rows, .. } => {
            apply_text_presentation_node(header);
            for row in rows {
                apply_text_presentation_node(row);
            }
        }
        Node::TableRow { cells } => {
            for cell in cells {
                apply_text_presentation_node(cell);
            }
        }
        Node::CodeBlock { .. }
        | Node::InlineCode { .. }
        | Node::Mention { .. }
        | Node::Timestamp { .. }
        | Node::Emoji { .. } => {}
    }
}

pub fn flatten_top_level_formatting(nodes: &mut [Node]) {
    for node in nodes {
        match node {
            Node::Strong { children } => flatten_same_formatting_type(children, "Strong"),
            Node::Emphasis { children } => flatten_same_formatting_type(children, "Emphasis"),
            Node::Underline { children } => flatten_same_formatting_type(children, "Underline"),
            Node::Strikethrough { children } => {
                flatten_same_formatting_type(children, "Strikethrough")
            }
            Node::Spoiler { children, .. } => flatten_same_formatting_type(children, "Spoiler"),
            _ => continue,
        }
        if let Some(children) = formatting_children_mut(node) {
            combine_adjacent_text(children, false);
            compact_empty_text_nodes(children);
        }
    }
}

fn formatting_children_mut(node: &mut Node) -> Option<&mut Vec<Node>> {
    match node {
        Node::Strong { children }
        | Node::Emphasis { children }
        | Node::Underline { children }
        | Node::Strikethrough { children }
        | Node::Spoiler { children, .. } => Some(children),
        _ => None,
    }
}

fn flatten_same_formatting_type(children: &mut Vec<Node>, node_type: &'static str) {
    if children.len() <= 1 || !children.iter().any(|child| child.tag_name() == node_type) {
        return;
    }
    let mut out = Vec::with_capacity(children.len());
    for child in std::mem::take(children) {
        if child.tag_name() == node_type {
            match child {
                Node::Strong { children }
                | Node::Emphasis { children }
                | Node::Underline { children }
                | Node::Strikethrough { children }
                | Node::Spoiler { children, .. } => out.extend(children),
                other => out.push(other),
            }
        } else {
            out.push(child);
        }
    }
    *children = out;
}

pub fn combine_adjacent_text(nodes: &mut Vec<Node>, inside_blockquote: bool) {
    if nodes.len() <= 1 {
        return;
    }
    let mut last_was_text = false;
    let has_adjacent_text = nodes.iter().any(|node| {
        let is_text = matches!(node, Node::Text { .. });
        let adjacent = is_text && last_was_text;
        last_was_text = is_text;
        adjacent
    });
    if !has_adjacent_text && !inside_blockquote {
        return;
    }

    let mut out = Vec::with_capacity(nodes.len());
    if inside_blockquote {
        let mut current_text = String::new();
        let mut non_text_seen = false;
        for node in std::mem::take(nodes) {
            match node {
                Node::Text { content } => {
                    if non_text_seen {
                        if !current_text.is_empty() {
                            out.push(Node::Text {
                                content: std::mem::take(&mut current_text),
                            });
                        }
                        non_text_seen = false;
                    }
                    current_text.push_str(&content);
                }
                other => {
                    if !current_text.is_empty() {
                        out.push(Node::Text {
                            content: std::mem::take(&mut current_text),
                        });
                    }
                    out.push(other);
                    non_text_seen = true;
                }
            }
        }
        if !current_text.is_empty() {
            out.push(Node::Text {
                content: current_text,
            });
        }
    } else {
        let mut current_text: Option<String> = None;
        for node in std::mem::take(nodes) {
            match node {
                Node::Text { content } => {
                    if content.is_empty() {
                        continue;
                    }
                    if is_malformed_block_text(&content) {
                        if let Some(value) = current_text.take() {
                            out.push(Node::Text { content: value });
                        }
                        out.push(Node::Text { content });
                    } else if let Some(value) = current_text.as_mut() {
                        if content.contains("\n\n") {
                            let previous = std::mem::take(value);
                            out.push(Node::Text { content: previous });
                            out.push(Node::Text { content });
                            current_text = None;
                        } else {
                            value.push_str(&content);
                        }
                    } else {
                        current_text = Some(content);
                    }
                }
                other => {
                    if let Some(value) = current_text.take() {
                        out.push(Node::Text { content: value });
                    }
                    out.push(other);
                }
            }
        }
        if let Some(value) = current_text {
            out.push(Node::Text { content: value });
        }
    }
    *nodes = out;
}

pub fn merge_adjacent_text_simple(nodes: &mut Vec<Node>) {
    if nodes.len() <= 1 {
        return;
    }
    let mut last_was_text = false;
    let has_adjacent_text = nodes.iter().any(|node| {
        let is_text = matches!(node, Node::Text { .. });
        let adjacent = is_text && last_was_text;
        last_was_text = is_text;
        adjacent
    });
    if !has_adjacent_text {
        return;
    }
    let mut out = Vec::with_capacity(nodes.len());
    let mut current_text = String::new();
    for node in std::mem::take(nodes) {
        match node {
            Node::Text { content } => current_text.push_str(&content),
            other => {
                if !current_text.is_empty() {
                    out.push(Node::Text {
                        content: std::mem::take(&mut current_text),
                    });
                }
                out.push(other);
            }
        }
    }
    if !current_text.is_empty() {
        out.push(Node::Text {
            content: current_text,
        });
    }
    *nodes = out;
}

pub fn compact_empty_text_nodes(nodes: &mut Vec<Node>) {
    if nodes.len() <= 1 {
        return;
    }
    if nodes.iter().any(Node::is_empty_text) {
        nodes.retain(|node| !node.is_empty_text());
    }
}

pub fn is_malformed_block_text(content: &str) -> bool {
    if content.is_empty() || !(content.starts_with('#') || content.starts_with("-#")) {
        return false;
    }
    let trimmed_content = trim(content);
    trimmed_content.starts_with('#') || trimmed_content.starts_with("-#")
}

pub fn replace_trailing_whitespace_with_newline(content: &str) -> String {
    let trimmed = crate::text::trim_right(content);
    if trimmed.len() == content.len() {
        return content.to_owned();
    }
    concat(trimmed, "\n")
}
