// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MessageRequestSchema,
	MessageUpdateRequestSchema,
	RichEmbedRequest,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {describe, expect, it} from 'vitest';

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
	it('accepts a simple poll with normalized options and default settings', () => {
		const result = MessageRequestSchema.safeParse({
			poll: {
				title: '  Dinner?  ',
				duration_seconds: '3600',
				options: [{text: '  Pizza  '}, {text: 'Sushi'}],
			},
		});
		expect(result.success).toBe(true);
		if (!result.success) {
			return;
		}
		expect(result.data.poll).toMatchObject({
			title: 'Dinner?',
			duration_seconds: 3600,
			anonymous: false,
			allow_ranked_choice: false,
			allow_custom_answers: false,
			options: [{text: 'Pizza'}, {text: 'Sushi'}],
		});
	});
	it('accepts poll options that reference uploaded message attachments', () => {
		const result = MessageRequestSchema.safeParse({
			attachments: [
				{
					id: 0,
					filename: 'pizza.png',
					upload_filename: 'tmp-pizza',
					file_size: 2048,
					content_type: 'image/png',
				},
			],
			poll: {
				title: 'Dinner?',
				duration_seconds: 3600,
				options: [{text: 'Pizza', attachment_id: 0}, {text: 'Sushi'}],
			},
		});
		expect(result.success).toBe(true);
	});
	it('rejects polls without enough options', () => {
		const result = MessageRequestSchema.safeParse({
			poll: {
				title: 'Dinner?',
				duration_seconds: 3600,
				options: [{text: 'Pizza'}],
			},
		});
		expect(result.success).toBe(false);
	});
	it('rejects poll options with duplicate text', () => {
		const result = MessageRequestSchema.safeParse({
			poll: {
				title: 'Dinner?',
				duration_seconds: 3600,
				options: [{text: 'Pizza'}, {text: ' pizza '}],
			},
		});
		expect(result.success).toBe(false);
	});
	it('rejects poll requests with ambiguous close times', () => {
		const result = MessageRequestSchema.safeParse({
			poll: {
				title: 'Dinner?',
				duration_seconds: 3600,
				expires_at: '2026-07-04T20:00:00Z',
				options: [{text: 'Pizza'}, {text: 'Sushi'}],
			},
		});
		expect(result.success).toBe(false);
	});
	it('rejects poll option attachments that are not included in the message request', () => {
		const result = MessageRequestSchema.safeParse({
			poll: {
				title: 'Dinner?',
				duration_seconds: 3600,
				options: [{text: 'Pizza', attachment_id: 0}, {text: 'Sushi'}],
			},
		});
		expect(result.success).toBe(false);
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
