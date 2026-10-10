// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {
	deleteMessage,
	ensureSessionStarted,
	markChannelAsIndexed,
	sendMessage,
} from '@app/api/message/tests/MessageTestUtils';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_TIMEOUTS, wait} from '@app/api/test/TestConstants';
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
	describe('Authentication', () => {
		test('requires authentication (401 without token)', async () => {
			await createBuilder(harness, '')
				.post('/search/messages')
				.body({content: 'test'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('Basic Search', () => {
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
