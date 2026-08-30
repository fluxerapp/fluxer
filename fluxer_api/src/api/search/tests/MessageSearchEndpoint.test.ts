// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createChannelID, createMessageID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {ChannelRepository} from '../../channel/ChannelRepository';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {
	deleteMessage,
	ensureSessionStarted,
	markChannelAsIndexed,
	sendMessage,
} from '../../message/tests/MessageTestUtils';
import {getMessageSearchService} from '../../SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_TIMEOUTS, wait} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

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

describe('Message Search Endpoint', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		Config.dev.validateResponses = true;
		await harness.shutdown();
	});
	describe('Response Shape', () => {
		test('omits referenced_message from search hits when response validation is disabled', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Reply Search Guild');
			const channelId = guild.system_channel_id!;
			const uniqueContent = `reply-search-term-${Date.now()}`;
			const target = await sendMessage(harness, account.token, channelId, `${uniqueContent} target`);
			await createBuilder(harness, account.token)
				.post(`/channels/${channelId}/messages`)
				.body({
					content: `${uniqueContent} reply`,
					message_reference: {channel_id: channelId, guild_id: guild.id, message_id: target.id},
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			await markChannelAsIndexed(harness, channelId);
			Config.dev.validateResponses = false;
			let hits: MessageSearchResult['messages'] = [];
			for (let attempt = 0; attempt < 20; attempt++) {
				const result = await createBuilder<MessageSearchResponse>(harness, account.token)
					.post('/search/messages')
					.body({
						content: uniqueContent,
						context_channel_id: channelId,
					})
					.expect(HTTP_STATUS.OK)
					.execute();
				if (isSearchResult(result) && result.messages.length > 0) {
					hits = result.messages;
					break;
				}
				await wait(TEST_TIMEOUTS.QUICK);
			}
			expect(hits.length).toBeGreaterThan(0);
			for (const message of hits) {
				expect(message).not.toHaveProperty('referenced_message');
			}
		});
	});
	describe('Authentication', () => {
		test('requires authentication (401 without token)', async () => {
			await createBuilder(harness, '')
				.post('/search/messages')
				.body({content: 'test'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('works with valid auth token (200)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Search Test Guild');
			const channelId = guild.system_channel_id!;
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'nonexistent',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
		});
	});
	describe('Basic Search', () => {
		test('returns empty results when no messages exist', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Empty Search Guild');
			const channelId = guild.system_channel_id!;
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'nonexistent query',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result.messages).toEqual([]);
				expect(result.total).toBe(0);
			}
		});
		test('search endpoint accepts content parameter and returns valid response', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Content Search Guild');
			const channelId = guild.system_channel_id!;
			const uniqueContent = `unique-search-term-${Date.now()}`;
			await sendMessage(harness, account.token, channelId, uniqueContent);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: uniqueContent,
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				expect(Array.isArray(result.messages)).toBe(true);
				expect(typeof result.total).toBe('number');
			}
		});
		test('search endpoint accepts partial content queries', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Partial Match Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const fullContent = `extraordinary-searchable-content-${timestamp}`;
			await sendMessage(harness, account.token, channelId, fullContent);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'extraordinary',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				expect(Array.isArray(result.messages)).toBe(true);
			}
		});
		test('search is case-insensitive in query handling', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Case Insensitive Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const mixedCaseContent = `CaSeMiXeD-searchterm-${timestamp}`;
			await sendMessage(harness, account.token, channelId, mixedCaseContent);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'casemixed',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				expect(Array.isArray(result.messages)).toBe(true);
			}
		});
		test('returns correct response structure', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Structure Test Guild');
			const channelId = guild.system_channel_id!;
			await sendMessage(harness, account.token, channelId, `structure-test-${Date.now()}`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'structure-test',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(result)) {
				expect(result).toHaveProperty('messages');
				expect(result).toHaveProperty('total');
				expect(result).toHaveProperty('hits_per_page');
				expect(result).toHaveProperty('page');
				expect(Array.isArray(result.messages)).toBe(true);
				expect(typeof result.total).toBe('number');
				expect(typeof result.hits_per_page).toBe('number');
				expect(typeof result.page).toBe('number');
			}
		});
		test('repairs stale search documents and reports only live messages', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Search Read Repair Guild');
			const channelId = guild.system_channel_id!;
			const baseContent = `read-repair-${Date.now()}`;
			const staleMessages: Array<{id: string}> = [];
			for (let i = 0; i < 5; i++) {
				staleMessages.push(await sendMessage(harness, account.token, channelId, `${baseContent} deleted ${i}`));
			}
			await markChannelAsIndexed(harness, channelId);
			await new ChannelRepository().messages.bulkDeleteMessages(
				createChannelID(BigInt(channelId)),
				staleMessages.map((message) => createMessageID(BigInt(message.id))),
			);
			await sendMessage(harness, account.token, channelId, `${baseContent} live 1`);
			await sendMessage(harness, account.token, channelId, `${baseContent} live 2`);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					author_id: [account.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages).toHaveLength(2);
			expect(result.total).toBe(2);
			expect(result.messages.every((message) => message.content.includes('live'))).toBe(true);
			const repairedResult = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: baseContent,
					context_channel_id: channelId,
					author_id: [account.userId],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(repairedResult)) {
				expect.fail('Expected repaired search result but got indexing response');
			}
			expect(repairedResult.total).toBe(2);
		});
		test('deleting a message removes its search document before read repair is needed', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Search Delete Cleanup Guild');
			const channelId = guild.system_channel_id!;
			const baseContent = `delete-cleanup-${Date.now()}`;
			const deleted = await sendMessage(harness, account.token, channelId, `${baseContent} deleted`);
			const kept = await sendMessage(harness, account.token, channelId, `${baseContent} kept`);
			await markChannelAsIndexed(harness, channelId);
			await deleteMessage(harness, account.token, channelId, deleted.id);
			const searchService = getMessageSearchService();
			if (!searchService) {
				expect.fail('Expected search service to be available');
			}
			const rawResult = await searchService.searchMessages(
				baseContent,
				{channelIds: [channelId], authorId: [account.userId]},
				{hitsPerPage: 25, page: 1},
			);
			expect(rawResult.total).toBe(1);
			expect(rawResult.hits.map((hit) => hit.id)).toEqual([kept.id]);
		});
	});
	describe('Pagination', () => {
		test('respects hits_per_page parameter (default 25, min 1, max 25)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Pagination Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `pagination-test-${timestamp}`;
			for (let i = 0; i < 10; i++) {
				await sendMessage(harness, account.token, channelId, `${batchContent} message ${i}`);
			}
			await markChannelAsIndexed(harness, channelId);
			const resultDefault = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultDefault)) {
				expect(resultDefault.hits_per_page).toBe(25);
			}
			const resultCustom = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					hits_per_page: 5,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultCustom)) {
				expect(resultCustom.hits_per_page).toBe(5);
				expect(resultCustom.messages.length).toBeLessThanOrEqual(5);
			}
			const resultMin = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					hits_per_page: 1,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultMin)) {
				expect(resultMin.hits_per_page).toBe(1);
				expect(resultMin.messages.length).toBeLessThanOrEqual(1);
			}
		});
		test('rejects hits_per_page values below minimum (1)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Min Pagination Guild');
			const channelId = guild.system_channel_id!;
			await markChannelAsIndexed(harness, channelId);
			await createBuilder(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'test',
					context_channel_id: channelId,
					hits_per_page: 0,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('rejects hits_per_page values above maximum (25)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Max Pagination Guild');
			const channelId = guild.system_channel_id!;
			await markChannelAsIndexed(harness, channelId);
			await createBuilder(harness, account.token)
				.post('/search/messages')
				.body({
					content: 'test',
					context_channel_id: channelId,
					hits_per_page: 50,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('respects page parameter for pagination', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Page Pagination Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `page-test-${timestamp}`;
			for (let i = 0; i < 6; i++) {
				await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
			}
			await markChannelAsIndexed(harness, channelId);
			const resultPage1 = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					hits_per_page: 3,
					page: 1,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultPage1)) {
				expect(resultPage1.page).toBe(1);
			}
			const resultPage2 = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					hits_per_page: 3,
					page: 2,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultPage2)) {
				expect(resultPage2.page).toBe(2);
			}
		});
		test('returns consistent total count across pages', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Total Count Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `total-count-${timestamp}`;
			for (let i = 0; i < 7; i++) {
				await sendMessage(harness, account.token, channelId, `${batchContent} item ${i}`);
			}
			await markChannelAsIndexed(harness, channelId);
			const resultPage1 = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
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
					hits_per_page: 3,
					page: 2,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (isSearchResult(resultPage1) && isSearchResult(resultPage2)) {
				expect(resultPage1.total).toBe(resultPage2.total);
			}
		});
	});
	describe('Message ID Filtering', () => {
		test('max_id filter is accepted and returns valid response', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'MaxId Filter Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `maxid-filter-${timestamp}`;
			const messages = [];
			for (let i = 0; i < 5; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push(msg);
			}
			await markChannelAsIndexed(harness, channelId);
			const middleMessageId = messages[2]!.id;
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					max_id: middleMessageId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(BigInt(msg.id)).toBeLessThan(BigInt(middleMessageId));
				}
			}
		});
		test('min_id filter is accepted and returns valid response', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'MinId Filter Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `minid-filter-${timestamp}`;
			const messages = [];
			for (let i = 0; i < 5; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push(msg);
			}
			await markChannelAsIndexed(harness, channelId);
			const middleMessageId = messages[2]!.id;
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					min_id: middleMessageId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					expect(BigInt(msg.id)).toBeGreaterThan(BigInt(middleMessageId));
				}
			}
		});
		test('both max_id and min_id together are accepted', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Range Filter Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `range-filter-${timestamp}`;
			const messages = [];
			for (let i = 0; i < 7; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push(msg);
			}
			await markChannelAsIndexed(harness, channelId);
			const minMessageId = messages[1]!.id;
			const maxMessageId = messages[5]!.id;
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					min_id: minMessageId,
					max_id: maxMessageId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				for (const msg of result.messages) {
					const msgId = BigInt(msg.id);
					expect(msgId).toBeGreaterThan(BigInt(minMessageId));
					expect(msgId).toBeLessThan(BigInt(maxMessageId));
				}
			}
		});
		test('max_id filter with boundary message returns empty or filtered results', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'MaxId Empty Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `maxid-empty-${timestamp}`;
			const messages = [];
			for (let i = 0; i < 3; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push(msg);
			}
			await markChannelAsIndexed(harness, channelId);
			const firstMessageId = messages[0]!.id;
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					max_id: firstMessageId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBe(0);
			}
		});
		test('min_id filter with boundary message returns empty or filtered results', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'MinId Empty Guild');
			const channelId = guild.system_channel_id!;
			const timestamp = Date.now();
			const batchContent = `minid-empty-${timestamp}`;
			const messages = [];
			for (let i = 0; i < 3; i++) {
				const msg = await sendMessage(harness, account.token, channelId, `${batchContent} msg ${i}`);
				messages.push(msg);
			}
			await markChannelAsIndexed(harness, channelId);
			const lastMessageId = messages[messages.length - 1]!.id;
			const result = await createBuilder<MessageSearchResponse>(harness, account.token)
				.post('/search/messages')
				.body({
					content: batchContent,
					context_channel_id: channelId,
					min_id: lastMessageId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toBeDefined();
			if (isSearchResult(result)) {
				expect(result.messages.length).toBe(0);
			}
		});
	});
	describe('Personal notes search indexing', () => {
		test('indexes new personal note messages after initial channel indexing', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await createBuilder<{
				type: 'session';
				data: {
					private_channels: Array<{
						id: string;
					}>;
				};
			}>(harness, '')
				.post('/test/rpc-session-init')
				.body({
					type: 'session',
					token: account.token,
					version: 1,
					ip: '127.0.0.1',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const personalNotesChannelId = account.userId;
			await sendMessage(harness, account.token, personalNotesChannelId, `personal-notes-bootstrap-${Date.now()}`);
			await markChannelAsIndexed(harness, personalNotesChannelId);
			const uniqueContent = `personal-notes-search-${Date.now()}`;
			await sendMessage(harness, account.token, personalNotesChannelId, uniqueContent);
			let found = false;
			for (let attempt = 0; attempt < 20; attempt++) {
				const result = await createBuilder<MessageSearchResponse>(harness, account.token)
					.post('/search/messages')
					.body({
						content: uniqueContent,
						context_channel_id: personalNotesChannelId,
					})
					.expect(HTTP_STATUS.OK)
					.execute();
				if (isSearchResult(result) && result.messages.some((message) => message.content === uniqueContent)) {
					found = true;
					break;
				}
				await wait(TEST_TIMEOUTS.QUICK);
			}
			expect(found).toBe(true);
		});
	});
});
