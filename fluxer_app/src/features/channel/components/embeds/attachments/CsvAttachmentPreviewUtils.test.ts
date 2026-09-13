// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildCsvTableNode,
	isCsvAttachment,
	parseCsvRows,
} from '@app/features/channel/components/embeds/attachments/CsvAttachmentPreviewUtils';
import {TableAlignment} from '@app/features/messaging/utils/markdown/parser/Enums';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

function attachmentFixture(fields: Partial<MessageAttachment>): MessageAttachment {
	return fields as MessageAttachment;
}

describe('CSV attachment preview utils', () => {
	it('detects CSV attachments by extension or content type', () => {
		expect(
			isCsvAttachment(attachmentFixture({id: '1', filename: 'data.csv', content_type: 'application/octet-stream'})),
		).toBe(true);
		expect(
			isCsvAttachment(attachmentFixture({id: '2', filename: 'data.txt', content_type: 'text/csv; charset=utf-8'})),
		).toBe(true);
		expect(isCsvAttachment(attachmentFixture({id: '3', filename: 'data.txt', content_type: 'text/plain'}))).toBe(false);
	});

	it('parses quoted values, escaped quotes, and CRLF rows', () => {
		expect(parseCsvRows('name,note\r\n"Ada, Lovelace","said ""hello"""\r\nGrace,Compiler')).toEqual([
			['name', 'note'],
			['Ada, Lovelace', 'said "hello"'],
			['Grace', 'Compiler'],
		]);
	});

	it('preserves empty cells and ignores one trailing row break', () => {
		expect(parseCsvRows('a,,c\n1,2,\n')).toEqual([
			['a', '', 'c'],
			['1', '2', ''],
		]);
	});

	it('normalizes ragged rows into a markdown table node', () => {
		expect(buildCsvTableNode([['a'], ['1', '2']])).toEqual({
			type: 'Table',
			header: {
				type: 'TableRow',
				cells: [
					{type: 'TableCell', children: [{type: 'Text', content: 'a'}]},
					{type: 'TableCell', children: [{type: 'Text', content: ''}]},
				],
			},
			alignments: [TableAlignment.None, TableAlignment.None],
			rows: [
				{
					type: 'TableRow',
					cells: [
						{type: 'TableCell', children: [{type: 'Text', content: '1'}]},
						{type: 'TableCell', children: [{type: 'Text', content: '2'}]},
					],
				},
			],
		});
	});
});
