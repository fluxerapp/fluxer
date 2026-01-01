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

import {MAX_AST_NODES} from '../types/constants';
import {NodeType} from '../types/enums';
import type {ListItem, ListNode, Node} from '../types/nodes';
import {parseCodeBlock} from './block-parsers';

interface ListParseResult {
	node: ListNode;
	newLineIndex: number;
	newNodeCount: number;
}

export function parseList(
	lines: Array<string>,
	currentLineIndex: number,
	isOrdered: boolean,
	indentLevel: number,
	depth: number,
	parserFlags: number,
	nodeCount: number,
	parseInline: (text: string) => Array<Node>,
): ListParseResult {
	const items: Array<ListItem> = [];
	const startLine = currentLineIndex;
	const firstLineContent = lines[startLine];
	let newLineIndex = currentLineIndex;
	let newNodeCount = nodeCount;

	while (newLineIndex < lines.length) {
		if (newNodeCount > MAX_AST_NODES) break;
		const currentLine = lines[newLineIndex];
		const trimmed = currentLine.trimStart();

		if (isBlockBreak(trimmed)) break;

		const listMatch = matchListItem(currentLine);

		if (listMatch) {
			const [itemOrdered, itemIndent, content, ordinal] = listMatch;

			if (itemIndent < indentLevel) break;

			if (itemIndent === indentLevel) {
				if (itemOrdered !== isOrdered) {
					if (newLineIndex === startLine) {
						const simpleList = createSimpleList(firstLineContent);

						return {
							node: simpleList,
							newLineIndex: newLineIndex + 1,
							newNodeCount: newNodeCount + 1,
						};
					}
					break;
				}

				const result = handleSameIndentLevel(
					items,
					content,
					indentLevel,
					depth,
					parseInline,
					(parentIndent, depth) => {
						const tryResult = tryParseNestedContent(
							lines,
							newLineIndex + 1,
							parentIndent,
							depth,
							(isOrdered, indentLevel, depth) =>
								parseList(
									lines,
									newLineIndex + 1,
									isOrdered,
									indentLevel,
									depth,
									parserFlags,
									newNodeCount,
									parseInline,
								),
						);

						return tryResult;
					},
					newLineIndex,
					ordinal,
				);

				newLineIndex = result.newLineIndex;
				newNodeCount = result.newNodeCount;
			} else if (itemIndent === indentLevel + 1) {
				const result = handleNestedIndentLevel(
					items,
					currentLine,
					itemOrdered,
					itemIndent,
					depth,
					(isOrdered, indentLevel, depth) =>
						parseList(lines, newLineIndex, isOrdered, indentLevel, depth, parserFlags, newNodeCount, parseInline),
					newLineIndex,
					newNodeCount,
				);

				newLineIndex = result.newLineIndex;
				newNodeCount = result.newNodeCount;
			} else {
				break;
			}
		} else if (isBulletPointText(currentLine)) {
			const result = handleBulletPointText(items, currentLine, newLineIndex, newNodeCount);

			newLineIndex = result.newLineIndex;
			newNodeCount = result.newNodeCount;
		} else if (isListContinuation(currentLine, indentLevel)) {
			const result = handleListContinuation(items, currentLine, newLineIndex, newNodeCount, parseInline);

			newLineIndex = result.newLineIndex;
			newNodeCount = result.newNodeCount;
		} else {
			break;
		}

		if (items.length > MAX_AST_NODES) break;
	}

	if (items.length === 0 && newLineIndex === startLine) {
		const simpleList = createSimpleList(firstLineContent);

		return {
			node: simpleList,
			newLineIndex: newLineIndex + 1,
			newNodeCount: newNodeCount + 1,
		};
	}

	return {
		node: {
			type: NodeType.List,
			ordered: isOrdered,
			items,
		},
		newLineIndex,
		newNodeCount,
	};
}

function isBlockBreak(trimmed: string): boolean {
	return trimmed.startsWith('#') || trimmed.startsWith('>') || trimmed.startsWith('>>> ');
}

function createSimpleList(content: string): ListNode {
	return {
		type: NodeType.List,
		ordered: false,
		items: [{children: [{type: NodeType.Text, content}]}],
	};
}

function handleSameIndentLevel(
	items: Array<ListItem>,
	content: string,
	indentLevel: number,
	depth: number,
	parseInline: (text: string) => Array<Node>,
	tryParseNestedContent: (parentIndent: number, depth: number) => {node: Node | null; newLineIndex: number},
	currentLineIndex: number,
	ordinal?: number,
): {newItems: Array<ListItem>; newLineIndex: number; newNodeCount: number} {
	const itemNodes: Array<Node> = [];
	let newNodeCount = 0;
	let newLineIndex = currentLineIndex + 1;

	const contentListMatch = matchListItem(content);
	if (contentListMatch) {
		const nestedContent = tryParseNestedContent(indentLevel, depth);

		const [isInlineOrdered, _, inlineItemContent] = contentListMatch;
		const inlineItemNodes = parseInline(inlineItemContent);

		const nestedListItems: Array<ListItem> = [{children: inlineItemNodes}];

		if (nestedContent.node && nestedContent.node.type === NodeType.List) {
			const nestedList = nestedContent.node as ListNode;
			nestedListItems.push(...nestedList.items);
			newLineIndex = nestedContent.newLineIndex;
		}

		const nestedList: ListNode = {
			type: NodeType.List,
			ordered: isInlineOrdered,
			items: nestedListItems,
		};

		itemNodes.push(nestedList);
		newNodeCount++;
	} else {
		const parsedNodes = parseInline(content);
		itemNodes.push(...parsedNodes);
		newNodeCount = itemNodes.length;

		const nestedContent = tryParseNestedContent(indentLevel, depth);
		if (nestedContent.node) {
			itemNodes.push(nestedContent.node);
			newNodeCount++;
			newLineIndex = nestedContent.newLineIndex;
		}
	}

	items.push({
		children: itemNodes,
		...(ordinal !== undefined ? {ordinal} : {}),
	});

	return {
		newItems: items,
		newLineIndex,
		newNodeCount,
	};
}

function handleNestedIndentLevel(
	items: Array<ListItem>,
	currentLine: string,
	isOrdered: boolean,
	indentLevel: number,
	depth: number,
	parseList: (isOrdered: boolean, indentLevel: number, depth: number) => ListParseResult,
	currentLineIndex: number,
	nodeCount: number,
): {newItems: Array<ListItem>; newLineIndex: number; newNodeCount: number} {
	if (depth >= 9) {
		if (items.length > 0) {
			items[items.length - 1].children.push({
				type: NodeType.Text,
				content: currentLine.trim(),
			});
			return {
				newItems: items,
				newLineIndex: currentLineIndex + 1,
				newNodeCount: nodeCount + 1,
			};
		}
		return {
			newItems: items,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount,
		};
	}

	const nested = parseList(isOrdered, indentLevel, depth + 1);

	if (items.length > 0) {
		items[items.length - 1].children.push(nested.node);
	}

	return {
		newItems: items,
		newLineIndex: nested.newLineIndex,
		newNodeCount: nested.newNodeCount,
	};
}

function handleBulletPointText(
	items: Array<ListItem>,
	currentLine: string,
	currentLineIndex: number,
	nodeCount: number,
): {newItems: Array<ListItem>; newLineIndex: number; newNodeCount: number} {
	if (items.length > 0) {
		items[items.length - 1].children.push({
			type: NodeType.Text,
			content: currentLine.trim(),
		});
		return {
			newItems: items,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + 1,
		};
	}

	return {
		newItems: items,
		newLineIndex: currentLineIndex + 1,
		newNodeCount: nodeCount,
	};
}

function handleListContinuation(
	items: Array<ListItem>,
	currentLine: string,
	currentLineIndex: number,
	nodeCount: number,
	parseInline: (text: string) => Array<Node>,
): {newItems: Array<ListItem>; newLineIndex: number; newNodeCount: number} {
	if (items.length > 0) {
		const content = currentLine.trimStart();
		const parsedNodes = parseInline(content);
		items[items.length - 1].children.push(...parsedNodes);
		return {
			newItems: items,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + parsedNodes.length,
		};
	}

	return {
		newItems: items,
		newLineIndex: currentLineIndex + 1,
		newNodeCount: nodeCount,
	};
}

function tryParseNestedContent(
	lines: Array<string>,
	currentLineIndex: number,
	parentIndent: number,
	depth: number,
	parseListFactory: (isOrdered: boolean, indentLevel: number, depth: number) => ListParseResult,
): {node: Node | null; newLineIndex: number} {
	if (currentLineIndex >= lines.length) return {node: null, newLineIndex: currentLineIndex};

	const line = lines[currentLineIndex];
	const trimmed = line.trimStart();

	if (trimmed.startsWith('```')) {
		const result = parseCodeBlock(lines, currentLineIndex);

		return {
			node: result.node,
			newLineIndex: result.newLineIndex,
		};
	}

	const listMatch = matchListItem(line);

	if (listMatch) {
		const [isOrdered, indent, _] = listMatch;
		if (indent > parentIndent && depth < 9) {
			const result = parseListFactory(isOrdered, indent, depth + 1);
			return {
				node: result.node,
				newLineIndex: result.newLineIndex,
			};
		}
	}

	return {node: null, newLineIndex: currentLineIndex};
}

function isListContinuation(line: string, indentLevel: number): boolean {
	let spaceCount = 0;
	for (let i = 0; i < line.length; i++) {
		if (line[i] === ' ') spaceCount++;
		else break;
	}
	return spaceCount > indentLevel * 2;
}

function isBulletPointText(text: string): boolean {
	const listMatch = matchListItem(text);
	if (listMatch) return false;

	const trimmed = text.trimStart();
	return trimmed.startsWith('- ') && !text.startsWith('  ');
}

export function matchListItem(line: string): [boolean, number, string, number?] | null {
	let indent = 0;
	let pos = 0;

	while (pos < line.length && line[pos] === ' ') {
		indent++;
		pos++;
	}

	if (indent > 0 && indent < 2) return null;
	const indentLevel = Math.floor(indent / 2);

	if (pos >= line.length) return null;

	const marker = line[pos];
	if (marker === '*' || marker === '-') {
		return handleUnorderedListMarker(line, pos, indentLevel);
	}
	if (/[0-9]/.test(marker)) {
		return handleOrderedListMarker(line, pos, indentLevel);
	}

	return null;
}

function handleUnorderedListMarker(
	line: string,
	pos: number,
	indentLevel: number,
): [boolean, number, string, undefined] | null {
	if (line[pos + 1] === ' ') {
		return [false, indentLevel, line.slice(pos + 2), undefined];
	}
	return null;
}

function handleOrderedListMarker(
	line: string,
	pos: number,
	indentLevel: number,
): [boolean, number, string, number] | null {
	let currentPos = pos;
	let ordinalStr = '';

	while (currentPos < line.length && /[0-9]/.test(line[currentPos])) {
		ordinalStr += line[currentPos];
		currentPos++;
	}

	if (line[currentPos] === '.' && line[currentPos + 1] === ' ') {
		const ordinal = Number.parseInt(ordinalStr, 10);
		return [true, indentLevel, line.slice(currentPos + 2), ordinal];
	}

	return null;
}
