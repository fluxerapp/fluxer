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

import {NodeType, TableAlignment} from '../types/enums';
import type {Node, TableCellNode, TableNode, TableRowNode} from '../types/nodes';

interface TableParseResult {
	node: TableNode | null;
	newLineIndex: number;
}

const PIPE = 124;
const SPACE = 32;
const BACKSLASH = 92;
const DASH = 45;
const COLON = 58;
const HASH = 35;
const GREATER_THAN = 62;
const ASTERISK = 42;
const DIGIT_0 = 48;
const DIGIT_9 = 57;
const PERIOD = 46;

const MAX_CACHE_SIZE = 1000;

const inlineContentCache = new Map<string, Array<Node>>();

export function parseTable(
	lines: Array<string>,
	currentLineIndex: number,
	_parserFlags: number,
	parseInline: (text: string) => Array<Node>,
): TableParseResult {
	const startIndex = currentLineIndex;

	if (startIndex + 2 >= lines.length) {
		return {node: null, newLineIndex: currentLineIndex};
	}

	const headerLine = lines[currentLineIndex];
	const alignmentLine = lines[currentLineIndex + 1];

	if (!containsPipe(headerLine) || !containsPipe(alignmentLine)) {
		return {node: null, newLineIndex: currentLineIndex};
	}

	try {
		const headerCells = fastSplitTableCells(headerLine.trim());
		if (headerCells.length === 0 || !hasContent(headerCells)) {
			return {node: null, newLineIndex: currentLineIndex};
		}

		const headerRow = createTableRow(headerCells, parseInline);

		const columnCount = headerRow.cells.length;
		currentLineIndex++;

		const alignmentCells = fastSplitTableCells(alignmentLine.trim());

		if (!validateAlignmentRow(alignmentCells)) {
			return {node: null, newLineIndex: startIndex};
		}

		const alignments = parseAlignments(alignmentCells);

		if (!alignments || headerRow.cells.length !== alignments.length) {
			return {node: null, newLineIndex: startIndex};
		}

		currentLineIndex++;

		const rows: Array<TableRowNode> = [];

		while (currentLineIndex < lines.length) {
			const line = lines[currentLineIndex];

			if (!containsPipe(line)) break;

			const trimmed = line.trim();
			if (isBlockBreakFast(trimmed)) break;

			const cellContents = fastSplitTableCells(trimmed);

			if (cellContents.length !== columnCount) {
				normalizeColumnCount(cellContents, columnCount);
			}

			const row = createTableRow(cellContents, parseInline);

			rows.push(row);
			currentLineIndex++;
		}

		if (rows.length === 0) {
			return {node: null, newLineIndex: startIndex};
		}

		let hasAnyContent = hasRowContent(headerRow);

		if (!hasAnyContent) {
			for (const row of rows) {
				if (hasRowContent(row)) {
					hasAnyContent = true;
					break;
				}
			}
		}

		if (!hasAnyContent) {
			return {node: null, newLineIndex: startIndex};
		}

		if (inlineContentCache.size > MAX_CACHE_SIZE) {
			inlineContentCache.clear();
		}

		return {
			node: {
				type: NodeType.Table,
				header: headerRow,
				alignments: alignments,
				rows,
			},
			newLineIndex: currentLineIndex,
		};
	} catch (_err) {
		return {node: null, newLineIndex: startIndex};
	}
}

function containsPipe(text: string): boolean {
	return text.indexOf('|') !== -1;
}

function hasContent(cells: Array<string>): boolean {
	for (const cell of cells) {
		if (cell.trim().length > 0) {
			return true;
		}
	}
	return false;
}

function hasRowContent(row: TableRowNode): boolean {
	for (const cell of row.cells) {
		if (
			cell.children.length > 0 &&
			!(cell.children.length === 1 && cell.children[0].type === NodeType.Text && cell.children[0].content.trim() === '')
		) {
			return true;
		}
	}
	return false;
}

function validateAlignmentRow(cells: Array<string>): boolean {
	if (cells.length === 0) return false;

	for (const cell of cells) {
		const trimmed = cell.trim();

		if (trimmed.length === 0 || trimmed.indexOf('-') === -1) {
			return false;
		}

		for (let i = 0; i < trimmed.length; i++) {
			const charCode = trimmed.charCodeAt(i);
			if (charCode !== SPACE && charCode !== COLON && charCode !== DASH && charCode !== PIPE) {
				return false;
			}
		}
	}

	return true;
}

function fastSplitTableCells(line: string): Array<string> {
	let start = 0;
	let end = line.length;

	if (line.length > 0 && line.charCodeAt(0) === PIPE) {
		start = 1;
	}

	if (line.length > 0 && end > start && line.charCodeAt(end - 1) === PIPE) {
		end--;
	}

	if (start >= end) {
		return [];
	}

	const content = line.substring(start, end);
	const cells: Array<string> = [];
	let currentCell = '';
	let i = 0;

	while (i < content.length) {
		if (content.charCodeAt(i) === BACKSLASH && i + 1 < content.length && content.charCodeAt(i + 1) === PIPE) {
			currentCell += '|';
			i += 2;
			continue;
		}

		if (content.charCodeAt(i) === PIPE) {
			cells.push(currentCell);
			currentCell = '';
			i++;
			continue;
		}

		currentCell += content[i];
		i++;
	}

	cells.push(currentCell);
	return cells;
}

function parseAlignments(cells: Array<string>): Array<TableAlignment> | null {
	if (cells.length === 0) return null;

	const alignments: Array<TableAlignment> = [];

	for (const cell of cells) {
		const trimmed = cell.trim();
		if (!trimmed || trimmed.indexOf('-') === -1) return null;

		const left = trimmed.charCodeAt(0) === COLON;
		const right = trimmed.charCodeAt(trimmed.length - 1) === COLON;

		if (left && right) {
			alignments.push(TableAlignment.Center);
		} else if (left) {
			alignments.push(TableAlignment.Left);
		} else if (right) {
			alignments.push(TableAlignment.Right);
		} else {
			alignments.push(TableAlignment.None);
		}
	}

	return alignments;
}

function createTableRow(cellContents: Array<string>, parseInline: (text: string) => Array<Node>): TableRowNode {
	const cells: Array<TableCellNode> = [];

	for (const cellContent of cellContents) {
		const trimmed = cellContent.trim();

		let inlineNodes: Array<Node>;
		if (inlineContentCache.has(trimmed)) {
			inlineNodes = inlineContentCache.get(trimmed)!;
		} else {
			inlineNodes = parseInline(trimmed);
			inlineContentCache.set(trimmed, inlineNodes);
		}

		cells.push({
			type: NodeType.TableCell,
			children: inlineNodes.length > 0 ? inlineNodes : [{type: NodeType.Text, content: trimmed}],
		});
	}

	return {type: NodeType.TableRow, cells};
}

function normalizeColumnCount(cells: Array<string>, expectedColumns: number): void {
	if (cells.length > expectedColumns) {
		const lastCellIndex = expectedColumns - 1;
		cells[lastCellIndex] = `${cells[lastCellIndex]}|${cells.slice(expectedColumns).join('|')}`;
		cells.length = expectedColumns;
	} else {
		while (cells.length < expectedColumns) {
			cells.push('');
		}
	}
}

function isBlockBreakFast(text: string): boolean {
	if (!text || text.length === 0) return false;

	const firstChar = text.charCodeAt(0);

	if (firstChar === HASH || firstChar === GREATER_THAN || firstChar === DASH || firstChar === ASTERISK) {
		return true;
	}

	if (
		text.length >= 4 &&
		text.charCodeAt(0) === GREATER_THAN &&
		text.charCodeAt(1) === GREATER_THAN &&
		text.charCodeAt(2) === GREATER_THAN &&
		text.charCodeAt(3) === SPACE
	) {
		return true;
	}

	if (text.length >= 2 && text.charCodeAt(0) === DASH && text.charCodeAt(1) === HASH) {
		return true;
	}

	if (firstChar >= DIGIT_0 && firstChar <= DIGIT_9) {
		for (let i = 1; i < Math.min(text.length, 4); i++) {
			if (text.charCodeAt(i) === PERIOD) {
				return true;
			}
		}
	}

	return false;
}
