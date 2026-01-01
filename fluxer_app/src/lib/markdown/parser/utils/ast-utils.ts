/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {NodeType} from '../types/enums';
import type {
	AlertNode,
	BlockquoteNode,
	FormattingNode,
	HeadingNode,
	LinkNode,
	ListNode,
	Node,
	SequenceNode,
	SubtextNode,
	TableCellNode,
	TableNode,
	TableRowNode,
	TextNode,
} from '../types/nodes';

const NT_TEXT = NodeType.Text;
const NT_STRONG = NodeType.Strong;
const NT_EMPHASIS = NodeType.Emphasis;
const NT_UNDERLINE = NodeType.Underline;
const NT_STRIKETHROUGH = NodeType.Strikethrough;
const NT_SPOILER = NodeType.Spoiler;
const NT_SEQUENCE = NodeType.Sequence;
const NT_HEADING = NodeType.Heading;
const NT_SUBTEXT = NodeType.Subtext;
const NT_BLOCKQUOTE = NodeType.Blockquote;
const NT_LIST = NodeType.List;
const NT_LINK = NodeType.Link;
const NT_TABLE = NodeType.Table;
const NT_TABLE_ROW = NodeType.TableRow;
const NT_TABLE_CELL = NodeType.TableCell;
const NT_ALERT = NodeType.Alert;

const FORMATTING_NODE_TYPES: Set<(typeof NodeType)[keyof typeof NodeType]> = new Set([
	NT_STRONG,
	NT_EMPHASIS,
	NT_UNDERLINE,
	NT_STRIKETHROUGH,
	NT_SPOILER,
	NT_SEQUENCE,
]);

export function flattenAST(nodes: Array<Node>): void {
	const nodeCount = nodes.length;
	if (nodeCount <= 1) {
		return;
	}

	for (let i = 0; i < nodeCount; i++) {
		flattenNode(nodes[i]);
	}

	flattenChildren(nodes, false);
}

function flattenNode(node: Node): void {
	const nodeType = node.type;

	if (nodeType === NT_TEXT) {
		return;
	}

	if (FORMATTING_NODE_TYPES.has(nodeType)) {
		const formattingNode = node as FormattingNode;
		const children = formattingNode.children;
		const childCount = children.length;

		if (childCount === 0) {
			return;
		}

		for (let i = 0; i < childCount; i++) {
			flattenNode(children[i]);
		}

		flattenChildren(children, false);
		return;
	}

	switch (nodeType) {
		case NT_HEADING:
		case NT_SUBTEXT: {
			const typedNode = node as HeadingNode | SubtextNode;
			const children = typedNode.children;
			const childCount = children.length;

			for (let i = 0; i < childCount; i++) {
				flattenNode(children[i]);
			}

			flattenChildren(children, false);
			break;
		}

		case NT_BLOCKQUOTE: {
			const blockquoteNode = node as BlockquoteNode;
			const children = blockquoteNode.children;
			const childCount = children.length;

			for (let i = 0; i < childCount; i++) {
				flattenNode(children[i]);
			}

			flattenChildren(children, true);
			break;
		}

		case NT_LIST: {
			const listNode = node as ListNode;
			const items = listNode.items;
			const itemCount = items.length;

			for (let i = 0; i < itemCount; i++) {
				const item = items[i];
				const itemChildren = item.children;
				const itemChildCount = itemChildren.length;

				for (let j = 0; j < itemChildCount; j++) {
					flattenNode(itemChildren[j]);
				}

				flattenChildren(itemChildren, false);
			}
			break;
		}

		case NT_LINK: {
			const linkNode = node as LinkNode;
			const text = linkNode.text;
			if (text) {
				flattenNode(text);
				if (text.type === NT_SEQUENCE) {
					const sequenceNode = text as SequenceNode;
					const seqChildren = sequenceNode.children;
					const seqChildCount = seqChildren.length;

					for (let i = 0; i < seqChildCount; i++) {
						flattenNode(seqChildren[i]);
					}

					flattenChildren(seqChildren, false);
				}
			}
			break;
		}

		case NT_TABLE: {
			const tableNode = node as TableNode;
			flattenTableRow(tableNode.header);

			const rows = tableNode.rows;
			const rowCount = rows.length;
			for (let i = 0; i < rowCount; i++) {
				flattenTableRow(rows[i]);
			}
			break;
		}

		case NT_TABLE_ROW:
			flattenTableRow(node as TableRowNode);
			break;

		case NT_TABLE_CELL: {
			const cellNode = node as TableCellNode;
			const cellChildren = cellNode.children;
			const cellChildCount = cellChildren.length;

			for (let i = 0; i < cellChildCount; i++) {
				flattenNode(cellChildren[i]);
			}

			flattenChildren(cellChildren, false);
			break;
		}

		case NT_ALERT: {
			const alertNode = node as AlertNode;
			const alertChildren = alertNode.children;
			const alertChildCount = alertChildren.length;

			for (let i = 0; i < alertChildCount; i++) {
				flattenNode(alertChildren[i]);
			}

			flattenChildren(alertChildren, false);
			break;
		}
	}
}

function flattenTableRow(row: TableRowNode): void {
	const cells = row.cells;
	const cellCount = cells.length;

	for (let i = 0; i < cellCount; i++) {
		const cell = cells[i];
		const cellChildren = cell.children;
		const childCount = cellChildren.length;

		if (childCount === 0) continue;

		for (let j = 0; j < childCount; j++) {
			flattenNode(cellChildren[j]);
		}

		flattenChildren(cellChildren, false);
	}
}

export function flattenChildren(nodes: Array<Node>, insideBlockquote = false): void {
	const nodeCount = nodes.length;
	if (nodeCount <= 1) {
		return;
	}

	flattenFormattingNodes(nodes);

	combineAdjacentTextNodes(nodes, insideBlockquote);

	removeEmptyTextNodesBetweenAlerts(nodes);
}

function flattenFormattingNodes(nodes: Array<Node>): void {
	if (nodes.length <= 1) {
		return;
	}

	let i = 0;
	while (i < nodes.length) {
		const node = nodes[i];
		if (FORMATTING_NODE_TYPES.has(node.type)) {
			const formattingNode = node as FormattingNode;
			flattenSameType(formattingNode.children, node.type);
		}
		i++;
	}
}

export function isFormattingNode(node: Node): boolean {
	return FORMATTING_NODE_TYPES.has(node.type);
}

export function flattenSameType(children: Array<Node>, nodeType: NodeType): void {
	if (children.length <= 1) {
		return;
	}

	let needsFlattening = false;
	for (let i = 0; i < children.length; i++) {
		if (children[i].type === nodeType) {
			needsFlattening = true;
			break;
		}
	}

	if (!needsFlattening) {
		return;
	}

	let i = 0;
	const result: Array<Node> = [];

	while (i < children.length) {
		const child = children[i];
		if (child.type === nodeType && 'children' in child) {
			const innerNodes = (child as FormattingNode).children;
			for (let j = 0; j < innerNodes.length; j++) {
				result.push(innerNodes[j]);
			}
		} else {
			result.push(child);
		}
		i++;
	}

	children.length = 0;
	for (let i = 0; i < result.length; i++) {
		children.push(result[i]);
	}
}

export function combineAdjacentTextNodes(nodes: Array<Node>, insideBlockquote = false): void {
	const nodeCount = nodes.length;
	if (nodeCount <= 1) {
		return;
	}

	let hasAdjacentTextNodes = false;
	let lastWasText = false;

	for (let i = 0; i < nodeCount; i++) {
		const isText = nodes[i].type === NT_TEXT;
		if (isText && lastWasText) {
			hasAdjacentTextNodes = true;
			break;
		}
		lastWasText = isText;
	}

	if (!hasAdjacentTextNodes && !insideBlockquote) {
		return;
	}

	const result: Array<Node> = [];
	let currentText = '';
	let nonTextNodeSeen = false;

	if (insideBlockquote) {
		for (let i = 0; i < nodeCount; i++) {
			const node = nodes[i];
			const isTextNode = node.type === NT_TEXT;

			if (isTextNode) {
				if (nonTextNodeSeen) {
					if (currentText) {
						result.push({type: NT_TEXT, content: currentText});
						currentText = '';
					}
					nonTextNodeSeen = false;
				}
				currentText += (node as TextNode).content;
			} else {
				if (currentText) {
					result.push({type: NT_TEXT, content: currentText});
					currentText = '';
				}
				result.push(node);
				nonTextNodeSeen = true;
			}
		}

		if (currentText) {
			result.push({type: NT_TEXT, content: currentText});
		}
	} else {
		let currentTextNode: TextNode | null = null;

		for (let i = 0; i < nodeCount; i++) {
			const node = nodes[i];
			if (node.type === NT_TEXT) {
				const textNode = node as TextNode;
				const content = textNode.content;

				let isMalformedContent = false;
				if (content && (content[0] === '#' || (content[0] === '-' && content.length > 1 && content[1] === '#'))) {
					const trimmed = content.trim();
					isMalformedContent = trimmed.startsWith('#') || trimmed.startsWith('-#');
				}

				if (isMalformedContent) {
					if (currentTextNode) {
						result.push(currentTextNode);
						currentTextNode = null;
					}
					result.push({type: NT_TEXT, content});
				} else if (currentTextNode) {
					const hasDoubleNewline = content.includes('\n\n');

					if (hasDoubleNewline) {
						result.push(currentTextNode);
						result.push({type: NT_TEXT, content});
						currentTextNode = null;
					} else {
						currentTextNode.content += content;
					}
				} else {
					currentTextNode = {type: NT_TEXT, content};
				}
			} else {
				if (currentTextNode) {
					result.push(currentTextNode);
					currentTextNode = null;
				}
				result.push(node);
			}
		}

		if (currentTextNode) {
			result.push(currentTextNode);
		}
	}

	nodes.length = 0;
	for (let i = 0; i < result.length; i++) {
		nodes.push(result[i]);
	}
}

function removeEmptyTextNodesBetweenAlerts(nodes: Array<Node>): void {
	const nodeCount = nodes.length;
	if (nodeCount < 3) {
		return;
	}

	let hasAlert = false;
	let hasTextNode = false;

	for (let i = 0; i < nodeCount; i++) {
		const type = nodes[i].type;
		hasAlert ||= type === NT_ALERT;
		hasTextNode ||= type === NT_TEXT;

		if (hasAlert && hasTextNode) break;
	}

	if (!hasAlert || !hasTextNode) {
		return;
	}

	let emptyTextBetweenAlerts = false;
	for (let i = 1; i < nodeCount - 1; i++) {
		const current = nodes[i];
		if (
			current.type === NT_TEXT &&
			nodes[i - 1].type === NT_ALERT &&
			nodes[i + 1].type === NT_ALERT &&
			(current as TextNode).content.trim() === ''
		) {
			emptyTextBetweenAlerts = true;
			break;
		}
	}

	if (!emptyTextBetweenAlerts) {
		return;
	}

	const result: Array<Node> = [];

	for (let i = 0; i < nodeCount; i++) {
		const current = nodes[i];

		if (
			i > 0 &&
			i < nodeCount - 1 &&
			current.type === NT_TEXT &&
			(current as TextNode).content.trim() === '' &&
			nodes[i - 1].type === NT_ALERT &&
			nodes[i + 1].type === NT_ALERT
		) {
			continue;
		}

		result.push(current);
	}

	nodes.length = 0;
	for (let i = 0; i < result.length; i++) {
		nodes.push(result[i]);
	}
}

export function mergeTextNodes(nodes: Array<Node>): Array<Node> {
	const nodeCount = nodes.length;
	if (nodeCount <= 1) {
		return nodes;
	}

	let hasConsecutiveTextNodes = false;
	let prevWasText = false;

	for (let i = 0; i < nodeCount; i++) {
		const isText = nodes[i].type === NT_TEXT;
		if (isText && prevWasText) {
			hasConsecutiveTextNodes = true;
			break;
		}
		prevWasText = isText;
	}

	if (!hasConsecutiveTextNodes) {
		return nodes;
	}

	const mergedNodes: Array<Node> = [];
	let currentText = '';

	for (let i = 0; i < nodeCount; i++) {
		const node = nodes[i];
		if (node.type === NT_TEXT) {
			currentText += (node as TextNode).content;
		} else {
			if (currentText) {
				mergedNodes.push({type: NT_TEXT, content: currentText});
				currentText = '';
			}
			mergedNodes.push(node);
		}
	}

	if (currentText) {
		mergedNodes.push({type: NT_TEXT, content: currentText});
	}

	return mergedNodes;
}

export function addTextNode(nodes: Array<Node>, text: string): void {
	if (text && text.length > 0) {
		nodes.push({type: NT_TEXT, content: text});
	}
}
