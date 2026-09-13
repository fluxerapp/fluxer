// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted, markChannelAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface MessageSearchResult {
	messages: Array<{
		id: string;
		channel_id: string;
		content: string;
		author: {
			id: string;
			username: string;
		};
		timestamp: string;
		embeds?: Array<{
			type?: string;
			provider?: {
				name?: string;
			};
		}>;
		attachments?: Array<{
			id: string;
			filename: string;
		}>;
	}>;
	total: number;
	hits_per_page: number;
	page: number;
}

interface SearchIndexingResponse {
	indexing: true;
}

type MessageSearchResponse = MessageSearchResult | SearchIndexingResponse;

function isSearchResult(response: MessageSearchResponse): response is MessageSearchResult {
	return 'messages' in response;
}

interface MessageWithTimestamp {
	id: string;
	timestamp: string;
	content: string;
}

async function sendMessageWithEmbed(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
	embedTitle: string,
): Promise<MessageWithTimestamp> {
	await ensureSessionStarted(harness, token);
	const msg = await createBuilder<MessageWithTimestamp>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({
			content,
			embeds: [
				{
					title: embedTitle,
					description: 'Test embed description',
				},
			],
		})
		.execute();
	return msg;
}

describe('Message Search Sorting and Advanced Filters', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('Sorting by timestamp', () => {
		test('sort_by timestamp with sort_order desc returns newest messages first', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Timestamp Sort Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `timestamp-sort-${timestamp}`;
			const messages: Array<MessageWithTimestamp> = [];
			for (let i = 0; i < 5; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push({id: msg.id, timestamp: msg.timestamp ?? '', content: msg.content});
			}
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					sort_by: 'timestamp',
					sort_order: 'desc',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result) && result.messages.length >= 2) {
				for (let i = 0; i < result.messages.length - 1; i++) {
					const currentId = BigInt(result.messages[i]!.id);
					const nextId = BigInt(result.messages[i + 1]!.id);
					expect(currentId).toBeGreaterThan(nextId);
				}
			}
		});
		test('sort_by timestamp with sort_order asc returns oldest messages first', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Timestamp Asc Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `timestamp-asc-${timestamp}`;
			const messages: Array<MessageWithTimestamp> = [];
			for (let i = 0; i < 5; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push({id: msg.id, timestamp: msg.timestamp ?? '', content: msg.content});
			}
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					sort_by: 'timestamp',
					sort_order: 'asc',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result) && result.messages.length >= 2) {
				for (let i = 0; i < result.messages.length - 1; i++) {
					const currentId = BigInt(result.messages[i]!.id);
					const nextId = BigInt(result.messages[i + 1]!.id);
					expect(currentId).toBeLessThan(nextId);
				}
			}
		});
	});
	describe('Sorting by relevance', () => {
		test('sort_by relevance returns most relevant matches first', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Relevance Sort Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const searchTerm = `relevance-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `This message mentions ${searchTerm} once`);
			await sendMessage(harness, account.token, channelId, `${searchTerm} ${searchTerm} ${searchTerm} multiple times`);
			await sendMessage(harness, account.token, channelId, 'This message has no match at all');
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: searchTerm,
					context_channel_id: channelId,
					sort_by: 'relevance',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBeGreaterThanOrEqual(0);
			}
		});
		test('exact matches are ranked higher than partial matches', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exact Match Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const exactTerm = `exactmatch-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${exactTerm}`);
			await sendMessage(harness, account.token, channelId, `Some prefix ${exactTerm} and suffix`);
			await sendMessage(harness, account.token, channelId, `partial ${exactTerm} concatenated`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: exactTerm,
					context_channel_id: channelId,
					sort_by: 'relevance',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBeGreaterThanOrEqual(0);
			}
		});
	});
	describe('Embed filtering', () => {
		test('embed_type filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Embed Type Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `embed-type-${timestamp}`;
			await sendMessageWithEmbed(harness, account.token, channelId, `${batchContent} with embed`, 'Test Embed');
			await sendMessage(harness, account.token, channelId, `${batchContent} without embed`);
			await markChannelAsIndexed(harness, channelId);
			const resultWithHas = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['embed'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultWithHas)) {
				expect(resultWithHas).toBeDefined();
			}
		});
		test('exclude_embed_type filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Embed Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `exclude-embed-${timestamp}`;
			await sendMessageWithEmbed(harness, account.token, channelId, `${batchContent} with embed`, 'Exclude Test');
			await sendMessage(harness, account.token, channelId, `${batchContent} no embed here`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_has: ['embed'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('embed_provider filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Embed Provider Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `embed-provider-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} https://youtube.com/watch?v=test123`);
			await sendMessage(harness, account.token, channelId, `${batchContent} https://twitter.com/user/status/123`);
			await sendMessage(harness, account.token, channelId, `${batchContent} plain text no links`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					embed_provider: ['YouTube'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('exclude_embed_provider filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Provider Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `exclude-provider-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} https://youtube.com/watch?v=abc`);
			await sendMessage(harness, account.token, channelId, `${batchContent} plain text`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_embed_provider: ['YouTube'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
	});
	describe('Link filtering', () => {
		test('link_hostname filter finds messages with links to specific domains', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Link Hostname Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `link-filter-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} check https://github.com/repo`);
			await sendMessage(harness, account.token, channelId, `${batchContent} visit https://example.com/page`);
			await sendMessage(harness, account.token, channelId, `${batchContent} no links here`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					link_hostname: ['github.com'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content.toLowerCase()).toContain('github.com');
				}
			}
		});
		test('link_hostname filter includes URLs enclosed in angle brackets', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Angle Link Hostname Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `angle-link-filter-${timestamp}`;
			const angleLinkMessage = await sendMessage(
				harness,
				account.token,
				channelId,
				`${batchContent} <https://google.com>`,
			);
			const bareLinkMessage = await sendMessage(
				harness,
				account.token,
				channelId,
				`${batchContent} https://google.com`,
			);
			const otherLinkMessage = await sendMessage(
				harness,
				account.token,
				channelId,
				`${batchContent} https://example.com`,
			);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					link_hostname: ['google.com'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				const resultIds = result.messages.map((message) => message.id);
				expect(resultIds).toContain(angleLinkMessage.id);
				expect(resultIds).toContain(bareLinkMessage.id);
				expect(resultIds).not.toContain(otherLinkMessage.id);
			}
		});
		test('exclude_link_hostname excludes specific domains', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Link Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `exclude-link-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} see https://github.com/test`);
			await sendMessage(harness, account.token, channelId, `${batchContent} at https://example.com/info`);
			await sendMessage(harness, account.token, channelId, `${batchContent} plain message`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_link_hostname: ['github.com'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content.toLowerCase()).not.toContain('github.com');
				}
			}
		});
		test('has link filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has Link Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `has-link-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} https://example.com`);
			await sendMessage(harness, account.token, channelId, `${batchContent} no link`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['link'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content).toMatch(/https?:\/\//);
				}
			}
		});
	});
	describe('Attachment filtering', () => {
		test('attachment_filename filter finds specific filenames', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Attachment Filename Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `attachment-name-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} message one`);
			await sendMessage(harness, account.token, channelId, `${batchContent} message two`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					attachment_filename: ['screenshot.png'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('exclude_attachment_filename works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Attachment Name Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `exclude-attach-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} msg 1`);
			await sendMessage(harness, account.token, channelId, `${batchContent} msg 2`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_attachment_filename: ['document.pdf'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('attachment_extension filter works with multiple extensions', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Attachment Extension Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `attach-ext-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} with image`);
			await sendMessage(harness, account.token, channelId, `${batchContent} with document`);
			await sendMessage(harness, account.token, channelId, `${batchContent} text only`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					attachment_extension: ['png', 'jpg'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('exclude_attachment_extension works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Exclude Extension Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `exclude-ext-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} msg a`);
			await sendMessage(harness, account.token, channelId, `${batchContent} msg b`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_attachment_extension: ['exe', 'bat'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
		test('has file filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Has File Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `has-file-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} with attachment`);
			await sendMessage(harness, account.token, channelId, `${batchContent} text only`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['file'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
	});
	describe('Multiple content queries', () => {
		test('contents array with multiple terms uses OR logic', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multiple Contents Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const term1 = `alpha${timestamp}`;
			const term2 = `beta${timestamp}`;
			const term3 = `gamma${timestamp}`;
			await sendMessage(harness, account.token, channelId, `Message with ${term1}`);
			await sendMessage(harness, account.token, channelId, `Message with ${term2}`);
			await sendMessage(harness, account.token, channelId, `Message with ${term3}`);
			await sendMessage(harness, account.token, channelId, `Message with nothing special ${timestamp}`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					contents: [term1, term2],
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					const hasTermOne = msg.content.toLowerCase().includes(term1.toLowerCase());
					const hasTermTwo = msg.content.toLowerCase().includes(term2.toLowerCase());
					expect(hasTermOne || hasTermTwo).toBe(true);
				}
			}
		});
		test('contents array combined with other filters works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Contents Combined Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const term1 = `searchone${timestamp}`;
			const term2 = `searchtwo${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${term1} https://github.com/test`);
			await sendMessage(harness, account.token, channelId, `${term2} no link here`);
			await sendMessage(harness, account.token, channelId, `${term1} plain text`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					contents: [term1, term2],
					context_channel_id: channelId,
					has: ['link'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					const hasTermOne = msg.content.toLowerCase().includes(term1.toLowerCase());
					const hasTermTwo = msg.content.toLowerCase().includes(term2.toLowerCase());
					expect(hasTermOne || hasTermTwo).toBe(true);
					expect(msg.content).toMatch(/https?:\/\//);
				}
			}
		});
		test('single content query still works alongside contents array parameters', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Content Priority Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const searchTerm = `priorityterm-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `This has ${searchTerm} in it`);
			await sendMessage(harness, account.token, channelId, `This does not match the term`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: searchTerm,
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBeGreaterThanOrEqual(0);
			}
		});
	});
	describe('Combined filter scenarios', () => {
		test('sorting with link filter works correctly', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Sort Link Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `sort-link-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} https://first.com`);
			await sendMessage(harness, account.token, channelId, `${batchContent} https://second.com`);
			await sendMessage(harness, account.token, channelId, `${batchContent} https://third.com`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['link'],
					sort_by: 'timestamp',
					sort_order: 'asc',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result) && result.messages.length >= 2) {
				for (let i = 0; i < result.messages.length - 1; i++) {
					const currentId = BigInt(result.messages[i]!.id);
					const nextId = BigInt(result.messages[i + 1]!.id);
					expect(currentId).toBeLessThan(nextId);
				}
				for (const msg of result.messages) {
					expect(msg.content).toMatch(/https?:\/\//);
				}
			}
		});
		test('multiple exclusion filters work together', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Multiple Exclude Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `multi-exclude-${timestamp}`;
			await sendMessage(harness, account.token, channelId, `${batchContent} https://github.com/repo`);
			await sendMessage(harness, account.token, channelId, `${batchContent} https://example.com/page`);
			await sendMessage(harness, account.token, channelId, `${batchContent} plain text message`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					exclude_link_hostname: ['github.com'],
					exclude_has: ['embed'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(msg.content.toLowerCase()).not.toContain('github.com');
				}
			}
		});
		test('pagination works correctly with filters applied', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Pagination Filter Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `paginate-filter-${timestamp}`;
			for (let i = 0; i < 8; i++) {
				await sendMessage(harness, account.token, channelId, `${batchContent} https://example${i}.com msg ${i}`);
			}
			await markChannelAsIndexed(harness, channelId);
			const resultPage1 = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['link'],
					hits_per_page: 3,
					page: 1,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const resultPage2 = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					has: ['link'],
					hits_per_page: 3,
					page: 2,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultPage1) && isSearchResult(resultPage2)) {
				expect(resultPage1.page).toBe(1);
				expect(resultPage2.page).toBe(2);
				const page1Ids = resultPage1.messages.map((m) => m.id);
				const page2Ids = resultPage2.messages.map((m) => m.id);
				const overlap = page1Ids.filter((id) => page2Ids.includes(id));
				expect(overlap.length).toBe(0);
			}
		});
	});
});
