// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MessageNonceRequest,
	MessageRequestSchema,
	MessageUpdateRequestSchema,
	RichEmbedRequest,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {describe, expect, it} from 'vitest';

describe('MessageNonceRequest', () => {
	it.each([
		[0, '0'],
		[Number.MAX_SAFE_INTEGER, '9007199254740991'],
		['  client-nonce  ', 'client-nonce'],
		['-1', '-1'],
	])('normalizes nonce %j to its wire string', (input, expected) => {
		expect(MessageNonceRequest.parse(input)).toBe(expected);
	});

	it.each([
		-1,
		0.5,
		Number.MAX_SAFE_INTEGER + 1,
		Number.POSITIVE_INFINITY,
		'',
		'a'.repeat(33),
	])('rejects invalid nonce %j', (input) => {
		expect(MessageNonceRequest.safeParse(input).success).toBe(false);
	});
});

describe('MessageRequestSchema', () => {
	it('preserves ANSI escape characters in message content', () => {
		const content = '```ansi\nWelcome to \u001b[2;33mFluxer\u001b[0m!\n```';
		const result = MessageRequestSchema.safeParse({content});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.content).toBe(content);
	});
	it('uses platform-compatible normalization for message content', () => {
		const result = MessageRequestSchema.safeParse({content: '  hello\x00\x01\u001B\u000C\u202Eworld  '});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.content).toBe('hello\x00\x01\u001Bworld');
	});
});

describe('MessageUpdateRequestSchema', () => {
	it('preserves ANSI escape characters in edited message content', () => {
		const content = '```ansi\nEdited \u001b[2;31mred\u001b[0m\n```';
		const result = MessageUpdateRequestSchema.safeParse({content});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.content).toBe(content);
	});
	it('preserves uploaded attachment fields for newly added edit attachments', () => {
		const result = MessageUpdateRequestSchema.safeParse({
			attachments: [
				{
					id: 0,
					filename: 'upload.png',
					upload_filename: 'tmp-key',
					file_size: 2048,
					content_type: 'image/png',
					title: 'Image title',
					description: 'Image description',
				},
			],
		});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.attachments).toEqual([
			{
				id: 0,
				filename: 'upload.png',
				upload_filename: 'tmp-key',
				file_size: 2048,
				content_type: 'image/png',
				title: 'Image title',
				description: 'Image description',
				flags: 0,
			},
		]);
	});
	it('still accepts existing attachment references during edits', () => {
		const result = MessageUpdateRequestSchema.safeParse({
			attachments: [
				{
					id: '123456789012345678',
					filename: 'existing.png',
					title: 'Existing title',
					description: 'Existing description',
				},
			],
		});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.attachments).toEqual([
			{
				id: 123456789012345678n,
				filename: 'existing.png',
				title: 'Existing title',
				description: 'Existing description',
				flags: 0,
			},
		]);
	});
});

describe('RichEmbedRequest', () => {
	it('accepts an explicitly empty title', () => {
		const result = RichEmbedRequest.safeParse({
			title: '',
			description: 'Webhook embed body',
		});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.title).toBe('');
	});
});
