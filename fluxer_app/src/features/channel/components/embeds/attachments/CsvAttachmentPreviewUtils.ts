// SPDX-License-Identifier: AGPL-3.0-or-later

import {TableAlignment} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {TableNode, TableRowNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export type CsvRows = Array<Array<string>>;

const MAX_CSV_TABLE_CELLS = 262144;

function getAttachmentExtension(attachment: MessageAttachment): string {
	return (attachment.filename ?? attachment.title ?? '').split('.').pop()?.toLowerCase() ?? '';
}

export function isCsvAttachment(attachment: MessageAttachment): boolean {
	const normalizedType = (attachment.content_type ?? '').toLowerCase().split(';')[0].trim();
	return (
		getAttachmentExtension(attachment) === 'csv' ||
		normalizedType === 'text/csv' ||
		normalizedType === 'application/csv'
	);
}

function isLineBreak(value: string): boolean {
	return value === '\n' || value === '\r';
}

export function parseCsvRows(content: string | null): CsvRows | null {
	if (content == null) {
		return null;
	}
	if (content.length === 0) {
		return [];
	}
	const rows: CsvRows = [];
	let row: Array<string> = [];
	let cell = '';
	let inQuotes = false;

	const pushCell = () => {
		row.push(cell);
		cell = '';
	};
	const pushRow = () => {
		pushCell();
		rows.push(row);
		row = [];
	};

	for (let index = 0; index < content.length; index++) {
		const character = content[index];
		if (character === '"') {
			if (inQuotes && content[index + 1] === '"') {
				cell += '"';
				index++;
				continue;
			}
			inQuotes = !inQuotes;
			continue;
		}
		if (!inQuotes && character === ',') {
			pushCell();
			continue;
		}
		if (!inQuotes && isLineBreak(character)) {
			pushRow();
			if (character === '\r' && content[index + 1] === '\n') {
				index++;
			}
			continue;
		}
		cell += character;
	}

	if (cell.length > 0 || row.length > 0) {
		pushRow();
	}
	return rows;
}

function getColumnCount(rows: CsvRows): number {
	return rows.reduce((max, row) => Math.max(max, row.length), 1);
}

function buildTableRow(row: Array<string>, columnCount: number): TableRowNode {
	return {
		type: 'TableRow',
		cells: Array.from({length: columnCount}, (_, cellIndex) => ({
			type: 'TableCell',
			children: [
				{
					type: 'Text',
					content: row[cellIndex] ?? '',
				},
			],
		})),
	};
}

export function buildCsvTableNode(rows: CsvRows): TableNode {
	const columnCount = Math.min(getColumnCount(rows), MAX_CSV_TABLE_CELLS);
	const rowLimit = Math.max(1, Math.floor(MAX_CSV_TABLE_CELLS / columnCount));
	const [header = [], ...bodyRows] = rows.slice(0, rowLimit);
	return {
		type: 'Table',
		header: buildTableRow(header, columnCount),
		alignments: Array.from({length: columnCount}, () => TableAlignment.None),
		rows: bodyRows.map((row) => buildTableRow(row, columnCount)),
	};
}
