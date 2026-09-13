// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	columnWidthPercents,
	maxContentPx,
	minContentPx,
	type Piece,
	PX,
	TABLE_BUDGET_PX,
	TABLE_FLOOR_CAP_SHARE,
	TABLE_MAX_CELL_CHARS,
	TABLE_MAX_COLUMNS,
	TABLE_MAX_IDENT_CHARS,
	TABLE_WIDE_TIER_PX,
} from '@/table/DocsTableMetrics.ts';
import {splitTableRow} from './DocsSource.ts';

export {
	columnWidthPercents,
	PX,
	TABLE_BUDGET_PX,
	TABLE_FLOOR_CAP_SHARE,
	TABLE_MAX_CELL_CHARS,
	TABLE_MAX_COLUMNS,
	TABLE_MAX_IDENT_CHARS,
	TABLE_WIDE_TIER_PX,
};

const CANONICAL_SHAPES = new Set([
	'field|type|description',
	'status|body|condition',
	'value|name|description',
	'value|description',
]);

const PARALLEL_SENTINELS = new Set(['unset', 'empty', 'none', 'required', 'n/a', '-', 'same target', 'no default']);

export interface DocsTable {
	readonly line: number;
	readonly endLine: number;
	readonly columns: number;
	readonly rows: number;
	readonly header: ReadonlyArray<string>;
	readonly canonical: boolean;
	readonly minDemandPx: number;
	readonly columnMinPx: ReadonlyArray<number>;
	readonly columnMaxPx: ReadonlyArray<number>;
	readonly worstCellChars: number;
	readonly worstCellColumn: number;
	readonly worstCellText: string;
	readonly col1IdentChars: number;
	readonly col1IdentText: string;
	readonly parallel: boolean;
	readonly nonParallelReason: string;
}

function isDelimiter(line: string, columns: number): boolean {
	const cells = splitTableRow(line);
	return cells.length === columns && cells.every((cell) => /^:?-+:?$/u.test(cell));
}

function stripInline(text: string): string {
	return text
		.replace(/!\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/\[([^\]]*)\]\([^)]*\)/gu, '$1')
		.replace(/<sup>([^<]*)<\/sup>/gu, '$1')
		.replace(/<br\s*\/?>/giu, ' ')
		.replace(/<[^>]+>/gu, '')
		.replace(/\*\*([^*]*)\*\*/gu, '$1')
		.replace(/\*([^*]*)\*/gu, '$1')
		.replace(/&nbsp;/gu, ' ')
		.replace(/&lt;/gu, '<')
		.replace(/&gt;/gu, '>')
		.replace(/&amp;/gu, '&')
		.replace(/\\([!-/:-@[-`{-~])/gu, '$1');
}

function pieces(cell: string): Array<Piece> {
	const out: Array<Piece> = [];
	const pattern = /`([^`]*)`/gu;
	let last = 0;
	let match: RegExpExecArray | null = pattern.exec(cell);
	while (match != null) {
		if (match.index > last) {
			out.push({text: stripInline(cell.slice(last, match.index)), code: false});
		}
		out.push({
			text: match[1].replace(/\\([\\|])/gu, (escaped, character: string) => (character === '|' ? character : escaped)),
			code: true,
		});
		last = pattern.lastIndex;
		match = pattern.exec(cell);
	}
	if (last < cell.length) {
		out.push({text: stripInline(cell.slice(last)), code: false});
	}
	return out.filter((piece) => piece.text.length > 0);
}

function renderedText(parts: ReadonlyArray<Piece>): string {
	return parts.map((piece) => piece.text).join('');
}

function cellKind(cell: string): string {
	const stripped = cell.replace(/<sup>[^<]*<\/sup>/gu, '');
	const parts = pieces(stripped);
	if (parts.length === 0) {
		return 'empty';
	}
	if (parts.every((piece) => piece.code || /^[\s,]+$/u.test(piece.text))) {
		return 'code';
	}
	if (parts.every((piece) => !piece.code)) {
		return /[.!?]\s|[.!?]$/u.test(stripped) ? 'sentence' : 'phrase';
	}
	return 'mixed';
}

export function extractTables(source: string): Array<DocsTable> {
	const lines = source.split('\n');
	const tables: Array<DocsTable> = [];
	let fenced = false;
	let index = 0;
	while (index < lines.length) {
		if (/^\s*(?:```|~~~)/u.test(lines[index])) {
			fenced = !fenced;
			index += 1;
			continue;
		}
		if (fenced) {
			index += 1;
			continue;
		}
		if (!lines[index].trim().startsWith('|') || index + 1 >= lines.length) {
			index += 1;
			continue;
		}
		const header = splitTableRow(lines[index]);
		if (header.length === 0 || !isDelimiter(lines[index + 1], header.length)) {
			index += 1;
			continue;
		}
		const start = index;
		index += 2;
		const body: Array<Array<string>> = [];
		while (index < lines.length && lines[index].trim().startsWith('|')) {
			body.push(splitTableRow(lines[index]));
			index += 1;
		}
		const columns = header.length;
		const columnMinPx = new Array<number>(columns).fill(0);
		const columnMaxPx = new Array<number>(columns).fill(0);
		let worstCellChars = 0;
		let worstCellColumn = 0;
		let worstCellText = '';
		let col1IdentChars = 0;
		let col1IdentText = '';
		for (const [rowIndex, row] of [header, ...body].entries()) {
			const isHeader = rowIndex === 0;
			for (let column = 0; column < columns; column += 1) {
				const parts = pieces(isHeader ? (row[column] ?? '').toUpperCase() : (row[column] ?? ''));
				const minimum = minContentPx(parts);
				const maximum = maxContentPx(parts, isHeader);
				if (maximum > columnMaxPx[column]) {
					columnMaxPx[column] = maximum;
				}
				if (minimum > columnMinPx[column]) {
					columnMinPx[column] = minimum;
				}
				const text = renderedText(parts);
				if (!isHeader && text.length > worstCellChars) {
					worstCellChars = text.length;
					worstCellColumn = column;
					worstCellText = text;
				}
				if (column === 0 && !isHeader) {
					for (const run of text.split(/(?<=\s)|(?<=-)(?!\d)/u)) {
						const token = run.trim();
						if (token.length > col1IdentChars) {
							col1IdentChars = token.length;
							col1IdentText = token;
						}
					}
				}
			}
		}
		const headerShape = header.map((cell) => cell.trim().toLowerCase()).join('|');
		let parallel = true;
		let nonParallelReason = '';
		for (let column = 0; column < columns && parallel; column += 1) {
			if (headerShape === 'status|body|condition' && column === 1) {
				continue;
			}
			const shape = body
				.map((row) => (row[column] ?? '').trim())
				.filter((cell) => cell.length > 0 && !PARALLEL_SENTINELS.has(cell.toLowerCase()))
				.map(cellKind);
			const kinds = new Set(shape);
			kinds.delete('empty');
			const prose = kinds.has('sentence') || kinds.has('phrase') || kinds.has('mixed');
			if (kinds.has('code') && prose && shape.length > 2) {
				parallel = false;
				nonParallelReason = `column ${(column + 1).toString()} mixes code-only cells with prose cells`;
			}
		}
		tables.push({
			line: start + 1,
			endLine: index,
			columns,
			rows: body.length,
			header,
			canonical: CANONICAL_SHAPES.has(headerShape),
			minDemandPx: Math.round(columnMinPx.reduce((a, b) => a + b, 0) + columns * PX.cellPad),
			columnMinPx: columnMinPx.map((value) => Math.round(value)),
			columnMaxPx: columnMaxPx.map((value) => Math.round(value)),
			worstCellChars,
			worstCellColumn,
			worstCellText,
			col1IdentChars,
			col1IdentText,
			parallel,
			nonParallelReason,
		});
	}
	return tables;
}
