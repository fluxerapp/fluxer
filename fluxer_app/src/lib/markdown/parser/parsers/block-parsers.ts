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

import {Parser} from '../parser/parser';
import {MAX_AST_NODES, MAX_LINE_LENGTH} from '../types/constants';
import {AlertType, NodeType, ParserFlags} from '../types/enums';
import type {AlertNode, CodeBlockNode, HeadingNode, Node, SpoilerNode, SubtextNode, TextNode} from '../types/nodes';
import {flattenChildren} from '../utils/ast-utils';
import * as InlineParsers from './inline-parsers';
import * as ListParsers from './list-parsers';
import * as TableParsers from './table-parsers';

const ALERT_PATTERN = /^\[!([A-Z]+)\]\s*\n?/;

interface BlockParseResult {
	node: Node | null;
	newLineIndex: number;
	newNodeCount: number;
	extraNodes?: Array<Node>;
}

const stringCache = new Map<string, boolean>();

function hasOpenInlineCode(text: string): boolean {
	if (!text.includes('`')) return false;

	let openLength: number | null = null;
	let index = 0;

	while (index < text.length) {
		if (text[index] !== '`') {
			index++;
			continue;
		}

		let runLength = 0;
		while (index + runLength < text.length && text[index + runLength] === '`') {
			runLength++;
		}

		if (openLength === null) {
			openLength = runLength;
		} else if (runLength === openLength) {
			openLength = null;
		}

		index += runLength;
	}

	return openLength !== null;
}

function cachedStartsWith(str: string, search: string): boolean {
	const key = `${str}:${search}:startsWith`;
	if (!stringCache.has(key)) {
		stringCache.set(key, str.startsWith(search));
	}
	return stringCache.get(key)!;
}

export function parseBlock(
	_parser: Parser,
	lines: Array<string>,
	currentLineIndex: number,
	parserFlags: number,
	nodeCount: number,
): BlockParseResult {
	if (currentLineIndex >= lines.length) {
		return {node: null, newLineIndex: currentLineIndex, newNodeCount: nodeCount};
	}

	const line = lines[currentLineIndex];
	const trimmed = line.trimStart();

	if (cachedStartsWith(trimmed, '>>> ')) {
		if (!(parserFlags & ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES)) {
			const result = {
				node: parseBlockAsText(lines, currentLineIndex, '>>> '),
				newLineIndex: currentLineIndex + 1,
				newNodeCount: nodeCount + 1,
			};
			return result;
		}
		const result = parseMultilineBlockquote(lines, currentLineIndex, parserFlags, nodeCount);
		return result;
	}

	if (cachedStartsWith(trimmed, '>')) {
		if (!(parserFlags & ParserFlags.ALLOW_BLOCKQUOTES)) {
			return {node: null, newLineIndex: currentLineIndex, newNodeCount: nodeCount};
		}
		const result = parseBlockquote(lines, currentLineIndex, parserFlags, nodeCount);
		return result;
	}

	const listMatch = ListParsers.matchListItem(line);

	if (listMatch) {
		const [isOrdered, indentLevel, _content] = listMatch;
		if (parserFlags & ParserFlags.ALLOW_LISTS) {
			const result = ListParsers.parseList(
				lines,
				currentLineIndex,
				isOrdered,
				indentLevel,
				1,
				parserFlags,
				nodeCount,
				(text) => InlineParsers.parseInline(text, parserFlags),
			);

			const finalResult = {
				node: result.node,
				newLineIndex: result.newLineIndex,
				newNodeCount: result.newNodeCount,
			};

			return finalResult;
		}

		const result = {
			node: {type: NodeType.Text, content: line} as TextNode,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + 1,
		};

		return result;
	}

	if (trimmed.startsWith('||') && !trimmed.slice(2).includes('||')) {
		if (parserFlags & ParserFlags.ALLOW_SPOILERS) {
			const result = parseSpoiler(lines, currentLineIndex, parserFlags);

			const finalResult = {
				node: result.node,
				newLineIndex: result.newLineIndex,
				newNodeCount: nodeCount + 1,
			};

			return finalResult;
		}

		const result = {
			node: {type: NodeType.Text, content: line} as TextNode,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + 1,
		};

		return result;
	}

	if (parserFlags & ParserFlags.ALLOW_CODE_BLOCKS) {
		const fencePosition = line.indexOf('```');

		if (fencePosition !== -1) {
			const startsWithFence = cachedStartsWith(trimmed, '```') && fencePosition === line.length - trimmed.length;
			if (startsWithFence) {
				const result = parseCodeBlock(lines, currentLineIndex);

				const finalResult: BlockParseResult = {
					node: result.node,
					newLineIndex: result.newLineIndex,
					newNodeCount: nodeCount + 1,
				};

				if (result.extraContent) {
					finalResult.extraNodes = [{type: NodeType.Text, content: result.extraContent}];
					finalResult.newNodeCount = nodeCount + 2;
				}

				return finalResult;
			}

			const prefixText = line.slice(0, fencePosition);
			if (hasOpenInlineCode(prefixText)) {
				return {node: null, newLineIndex: currentLineIndex, newNodeCount: nodeCount};
			}
			const inlineNodes = InlineParsers.parseInline(prefixText, parserFlags);

			const codeLines = [line.slice(fencePosition), ...lines.slice(currentLineIndex + 1)];
			const codeResult = parseCodeBlock(codeLines, 0);
			const newLineIndex = currentLineIndex + codeResult.newLineIndex;

			const extraNodes: Array<Node> = [];
			if (inlineNodes.length > 1) {
				extraNodes.push(...inlineNodes.slice(1));
			}
			extraNodes.push(codeResult.node);
			if (codeResult.extraContent) {
				extraNodes.push({type: NodeType.Text, content: codeResult.extraContent});
			}

			const firstNode = inlineNodes[0] ?? codeResult.node;
			const newNodeCount = nodeCount + inlineNodes.length + 1 + (codeResult.extraContent ? 1 : 0);

			return {
				node: firstNode,
				extraNodes: extraNodes.length > 0 ? extraNodes : undefined,
				newLineIndex,
				newNodeCount,
			};
		}
	}

	if (!(parserFlags & ParserFlags.ALLOW_CODE_BLOCKS) && cachedStartsWith(trimmed, '```')) {
		let codeBlockText = lines[currentLineIndex];
		let endLineIndex = currentLineIndex + 1;

		while (endLineIndex < lines.length) {
			const nextLine = lines[endLineIndex];

			if (nextLine.trim() === '```') {
				codeBlockText += `\n${nextLine}`;
				endLineIndex++;
				break;
			}

			codeBlockText += `\n${nextLine}`;
			endLineIndex++;
		}

		return {
			node: {type: NodeType.Text, content: codeBlockText} as TextNode,
			newLineIndex: endLineIndex,
			newNodeCount: nodeCount + 1,
		};
	}

	if (cachedStartsWith(trimmed, '-#')) {
		if (parserFlags & ParserFlags.ALLOW_SUBTEXT) {
			const subtextNode = parseSubtext(trimmed, (text) => InlineParsers.parseInline(text, parserFlags));

			if (subtextNode) {
				const result = {
					node: subtextNode,
					newLineIndex: currentLineIndex + 1,
					newNodeCount: nodeCount + 1,
				};

				return result;
			}
		}

		const result = {
			node: {type: NodeType.Text, content: handleLineAsText(lines, currentLineIndex)} as TextNode,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + 1,
		};

		return result;
	}

	if (cachedStartsWith(trimmed, '#')) {
		if (parserFlags & ParserFlags.ALLOW_HEADINGS) {
			const headingNode = parseHeading(trimmed, (text) => InlineParsers.parseInline(text, parserFlags));

			if (headingNode) {
				const result = {
					node: headingNode,
					newLineIndex: currentLineIndex + 1,
					newNodeCount: nodeCount + 1,
				};

				return result;
			}
		}

		const result = {
			node: {type: NodeType.Text, content: handleLineAsText(lines, currentLineIndex)} as TextNode,
			newLineIndex: currentLineIndex + 1,
			newNodeCount: nodeCount + 1,
		};

		return result;
	}

	if (trimmed.includes('|') && parserFlags & ParserFlags.ALLOW_TABLES) {
		const startIndex = currentLineIndex;

		const tableResult = TableParsers.parseTable(lines, currentLineIndex, parserFlags, (text) =>
			InlineParsers.parseInline(text, parserFlags),
		);

		if (tableResult.node) {
			const result = {
				node: tableResult.node,
				newLineIndex: tableResult.newLineIndex,
				newNodeCount: nodeCount + 1,
			};

			return result;
		}

		currentLineIndex = startIndex;
	}

	return {node: null, newLineIndex: currentLineIndex, newNodeCount: nodeCount};
}

function handleLineAsText(lines: Array<string>, currentLineIndex: number): string {
	const isLastLine = currentLineIndex === lines.length - 1;
	return isLastLine ? lines[currentLineIndex] : `${lines[currentLineIndex]}\n`;
}

function parseBlockAsText(lines: Array<string>, currentLineIndex: number, marker: string): TextNode {
	const originalContent = lines[currentLineIndex];

	if (marker === '>' || marker === '>>> ') {
		return {
			type: NodeType.Text,
			content: originalContent + (currentLineIndex < lines.length - 1 ? '\n' : ''),
		};
	}

	return {
		type: NodeType.Text,
		content: originalContent,
	};
}

const MAX_HEADING_LEVEL = 4;

export function parseHeading(trimmed: string, parseInline: (text: string) => Array<Node>): HeadingNode | null {
	let level = 0;
	for (let i = 0; i < trimmed.length && i < MAX_HEADING_LEVEL; i++) {
		if (trimmed[i] === '#') level++;
		else break;
	}

	if (level >= 1 && level <= MAX_HEADING_LEVEL && trimmed[level] === ' ') {
		const content = trimmed.slice(level + 1);
		const inlineNodes = parseInline(content);

		const result: HeadingNode = {
			type: NodeType.Heading,
			level,
			children: inlineNodes,
		};

		return result;
	}

	return null;
}

function parseSubtext(trimmed: string, parseInline: (text: string) => Array<Node>): SubtextNode | null {
	if (trimmed.startsWith('-#')) {
		if ((trimmed.length > 2 && trimmed[2] !== ' ') || (trimmed.length > 3 && trimmed[3] === ' ')) {
			return null;
		}

		const content = trimmed.slice(3);
		const inlineNodes = parseInline(content);

		const result: SubtextNode = {
			type: NodeType.Subtext,
			children: inlineNodes,
		};

		return result;
	}

	return null;
}

function parseBlockquote(
	lines: Array<string>,
	currentLineIndex: number,
	parserFlags: number,
	nodeCount: number,
): BlockParseResult {
	let blockquoteContent = '';
	const startLine = currentLineIndex;
	let newLineIndex = currentLineIndex;

	while (newLineIndex < lines.length) {
		if (nodeCount > MAX_AST_NODES) break;
		const line = lines[newLineIndex];
		const trimmed = line.trimStart();

		if (trimmed === '> ' || trimmed === '>  ') {
			if (blockquoteContent.length > 0) blockquoteContent += '\n';
			newLineIndex++;
		} else if (trimmed.startsWith('> ')) {
			const content = trimmed.slice(2);
			if (blockquoteContent.length > 0) blockquoteContent += '\n';
			blockquoteContent += content;
			newLineIndex++;
		} else {
			break;
		}

		if (blockquoteContent.length > MAX_LINE_LENGTH * 100) break;
	}

	if (blockquoteContent === '' && newLineIndex === startLine) {
		return {node: null, newLineIndex, newNodeCount: nodeCount};
	}

	if (parserFlags & ParserFlags.ALLOW_ALERTS) {
		const alertNode = parseAlert(blockquoteContent, parserFlags);

		if (alertNode) {
			return {
				node: alertNode,
				newLineIndex,
				newNodeCount: nodeCount + 1,
			};
		}
	}

	const childFlags = parserFlags & ~ParserFlags.ALLOW_BLOCKQUOTES;
	const childParser = new Parser(blockquoteContent, childFlags);
	const {nodes: childNodes} = childParser.parse();

	flattenChildren(childNodes, true);

	return {
		node: {
			type: NodeType.Blockquote,
			children: childNodes,
		},
		newLineIndex,
		newNodeCount: nodeCount + 1,
	};
}

function parseMultilineBlockquote(
	lines: Array<string>,
	currentLineIndex: number,
	parserFlags: number,
	nodeCount: number,
): BlockParseResult {
	const line = lines[currentLineIndex];
	const trimmed = line.trimStart();

	if (!trimmed.startsWith('>>> ')) {
		return {
			node: {type: NodeType.Text, content: ''},
			newLineIndex: currentLineIndex,
			newNodeCount: nodeCount,
		};
	}

	let content = trimmed.slice(4);
	let newLineIndex = currentLineIndex + 1;

	while (newLineIndex < lines.length) {
		const current = lines[newLineIndex];
		content += `\n${current}`;
		newLineIndex++;
		if (content.length > MAX_LINE_LENGTH * 100) break;
	}

	const childFlags = (parserFlags & ~ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES) | ParserFlags.ALLOW_BLOCKQUOTES;
	const childParser = new Parser(content, childFlags);
	const {nodes: childNodes} = childParser.parse();

	return {
		node: {
			type: NodeType.Blockquote,
			children: childNodes,
		},
		newLineIndex,
		newNodeCount: nodeCount + 1,
	};
}

export function parseCodeBlock(
	lines: Array<string>,
	currentLineIndex: number,
): {node: CodeBlockNode; newLineIndex: number; extraContent?: string} {
	const line = lines[currentLineIndex];
	const trimmed = line.trimStart();

	const indentSpaces = line.length - trimmed.length;
	const listIndent = indentSpaces > 0 ? ' '.repeat(indentSpaces) : '';

	let fenceLength = 0;
	for (let i = 0; i < trimmed.length && trimmed[i] === '`'; i++) {
		fenceLength++;
	}

	const languagePart = trimmed.slice(fenceLength);

	const closingFence = '`'.repeat(fenceLength);
	const closingFenceIndex = languagePart.indexOf(closingFence);

	let language: string | undefined;
	if (closingFenceIndex !== -1) {
		const inlineContent = languagePart.slice(0, closingFenceIndex);
		const trailingInline = languagePart.slice(closingFenceIndex + fenceLength);

		return {
			node: {
				type: NodeType.CodeBlock,
				language: undefined,
				content: inlineContent,
			},
			newLineIndex: currentLineIndex + 1,
			extraContent: trailingInline || undefined,
		};
	}

	language = languagePart.trim() || undefined;
	let newLineIndex = currentLineIndex + 1;

	let tempIndex = newLineIndex;
	let lineCount = 0;

	while (tempIndex < lines.length) {
		const trimmedLine = lines[tempIndex].trimStart();
		if (trimmedLine.startsWith(closingFence)) {
			let backtickCount = 0;
			for (let i = 0; i < trimmedLine.length && trimmedLine[i] === '`'; i++) {
				backtickCount++;
			}

			const charAfterBackticks = trimmedLine[backtickCount];
			if (
				backtickCount >= fenceLength &&
				(!charAfterBackticks || charAfterBackticks === ' ' || charAfterBackticks === '\t' || charAfterBackticks === '`')
			) {
				break;
			}
		}
		tempIndex++;
		lineCount++;
		if (lineCount > 1000) break;
	}

	const contentParts: Array<string> = [];
	let contentLength = 0;

	while (newLineIndex < lines.length) {
		const current = lines[newLineIndex];
		const trimmedLine = current.trimStart();

		const fenceIndex = trimmedLine.indexOf(closingFence);
		if (fenceIndex !== -1) {
			let backtickCount = 0;
			let idx = fenceIndex;
			while (idx < trimmedLine.length && trimmedLine[idx] === '`') {
				backtickCount++;
				idx++;
			}

			const charAfterBackticks = trimmedLine[idx];
			const onlyWhitespaceAfter =
				!charAfterBackticks || charAfterBackticks === ' ' || charAfterBackticks === '\t' || charAfterBackticks === '`';

			if (backtickCount >= fenceLength && onlyWhitespaceAfter) {
				const contentPrefix = current.slice(0, current.indexOf(closingFence));
				let contentLine = contentPrefix;
				if (indentSpaces > 0 && contentPrefix.startsWith(listIndent)) {
					contentLine = contentPrefix.slice(indentSpaces);
				}

				if (contentLine.length > 0) {
					contentParts.push(contentLine);
					contentParts.push('\n');
				}

				let extraContent: string | undefined;
				const trailingText = trimmedLine.slice(idx);
				if (trailingText) {
					extraContent = trailingText;
				} else if (backtickCount > fenceLength) {
					extraContent = trimmedLine.slice(fenceLength);
				}

				newLineIndex++;

				if (extraContent) {
					return {
						node: {
							type: NodeType.CodeBlock,
							language,
							content: contentParts.join(''),
						},
						newLineIndex,
						extraContent,
					};
				}
				break;
			}
		}

		let contentLine = current;
		if (indentSpaces > 0 && current.startsWith(listIndent)) {
			contentLine = current.slice(indentSpaces);
		}

		contentParts.push(contentLine);
		contentParts.push('\n');
		contentLength += contentLine.length + 1;

		if (contentLength > MAX_LINE_LENGTH * 100) break;
		newLineIndex++;
	}

	return {
		node: {
			type: NodeType.CodeBlock,
			language,
			content: contentParts.join(''),
		},
		newLineIndex,
	};
}

function parseSpoiler(
	lines: Array<string>,
	currentLineIndex: number,
	parserFlags: number,
): {node: SpoilerNode | TextNode; newLineIndex: number} {
	const startLine = currentLineIndex;
	let foundEnd = false;
	let blockContent = '';
	let newLineIndex = currentLineIndex;

	while (newLineIndex < lines.length) {
		const line = lines[newLineIndex];

		if (newLineIndex === startLine) {
			const startIdx = line.indexOf('||');
			if (startIdx !== -1) {
				blockContent += line.slice(startIdx + 2);
			}
		} else {
			const endIdx = line.indexOf('||');
			if (endIdx !== -1) {
				blockContent += line.slice(0, endIdx);
				foundEnd = true;
				newLineIndex++;
				break;
			}
			blockContent += line;
		}

		blockContent += '\n';
		newLineIndex++;

		if (blockContent.length > MAX_LINE_LENGTH * 10) break;
	}

	if (!foundEnd) {
		return {
			node: {
				type: NodeType.Text,
				content: `||${blockContent.trimEnd()}`,
			},
			newLineIndex,
		};
	}

	const childParser = new Parser(blockContent.trim(), parserFlags);
	const {nodes: innerNodes} = childParser.parse();

	return {
		node: {
			type: NodeType.Spoiler,
			children: innerNodes,
			isBlock: true,
		},
		newLineIndex,
	};
}

function parseAlert(blockquoteText: string, parserFlags: number): AlertNode | null {
	const alertMatch = blockquoteText.match(ALERT_PATTERN);
	if (!alertMatch) {
		return null;
	}

	const alertTypeStr = alertMatch[1].toUpperCase();
	let alertType: AlertType;

	switch (alertTypeStr) {
		case 'NOTE':
			alertType = AlertType.Note;
			break;
		case 'TIP':
			alertType = AlertType.Tip;
			break;
		case 'IMPORTANT':
			alertType = AlertType.Important;
			break;
		case 'WARNING':
			alertType = AlertType.Warning;
			break;
		case 'CAUTION':
			alertType = AlertType.Caution;
			break;
		default:
			return null;
	}

	const content = blockquoteText.slice(alertMatch[0].length);

	const childFlags =
		(parserFlags & ~ParserFlags.ALLOW_BLOCKQUOTES) | ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_HEADINGS;

	const lines = content.split('\n');
	const processedLines = lines.map((line) => {
		const trimmed = line.trim();
		if (trimmed.startsWith('-') || /^\d+\./.test(trimmed)) {
			return line;
		}
		return trimmed;
	});

	const processedContent = processedLines
		.join('\n')
		.replace(/\n{3,}/g, '\n\n')
		.trim();

	const childParser = new Parser(processedContent, childFlags);
	const {nodes: childNodes} = childParser.parse();

	const mergedNodes: Array<Node> = [];
	let currentText = '';

	for (const node of childNodes) {
		if (node.type === NodeType.Text) {
			if (currentText) {
				currentText += node.content;
			} else {
				currentText = node.content;
			}
		} else {
			if (currentText) {
				mergedNodes.push({type: NodeType.Text, content: currentText});
				currentText = '';
			}
			mergedNodes.push(node);
		}
	}

	if (currentText) {
		mergedNodes.push({type: NodeType.Text, content: currentText});
	}

	const finalNodes = postProcessAlertNodes(mergedNodes);

	return {
		type: NodeType.Alert,
		alertType,
		children: finalNodes,
	};
}

function postProcessAlertNodes(nodes: Array<Node>): Array<Node> {
	if (nodes.length <= 1) return nodes;

	const result: Array<Node> = [];
	let i = 0;

	while (i < nodes.length) {
		const node = nodes[i];

		if (node.type === NodeType.Text && i + 1 < nodes.length) {
			if (nodes[i + 1].type === NodeType.List) {
				const trimmedContent = node.content.replace(/\s+$/, '\n');
				if (trimmedContent) {
					result.push({type: NodeType.Text, content: trimmedContent});
				}
			} else {
				result.push(node);
			}
		} else if (node.type === NodeType.List && i + 1 < nodes.length) {
			result.push(node);

			const nextNode = nodes[i + 1];
			if (nextNode.type === NodeType.Text) {
				const content = nextNode.content.trim();
				if (content) {
					result.push({type: NodeType.Text, content: `\n${content}`});
					i++;
				}
			}
		} else {
			result.push(node);
		}

		i++;
	}

	return result;
}
