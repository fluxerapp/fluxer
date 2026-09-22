// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {markChannelAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
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
			bot?: boolean;
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

describe('Message Search Exact Phrases', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	test('exact_phrases returns only messages containing the exact phrase', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Exact Phrase Guild');
		const channelId = guild.system_channel_id!;
		const timestamp = Date.now();
		const tag = `ep-${timestamp}`;
		await sendMessage(harness, owner.token, channelId, `${tag} hello world`);
		await sendMessage(harness, owner.token, channelId, `${tag} hello everyone in the world`);
		await sendMessage(harness, owner.token, channelId, `${tag} world hello`);
		await markChannelAsIndexed(harness, channelId);
		const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
			.post('/search/messages')
			.body({
				exact_phrases: ['hello world'],
				context_channel_id: channelId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		if (isSearchResult(result)) {
			for (const msg of result.messages) {
				expect(msg.content).toContain('hello world');
			}
			expect(result.messages.some((m) => m.content.includes(`${tag} hello world`))).toBe(true);
		}
	});
	test('exact_phrases is case-sensitive', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Case Sensitive Guild');
		const channelId = guild.system_channel_id!;
		const timestamp = Date.now();
		const tag = `ci-${timestamp}`;
		await sendMessage(harness, owner.token, channelId, `${tag} Hello World`);
		await sendMessage(harness, owner.token, channelId, `${tag} HELLO WORLD`);
		await sendMessage(harness, owner.token, channelId, `${tag} hello world`);
		await markChannelAsIndexed(harness, channelId);
		const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
			.post('/search/messages')
			.body({
				exact_phrases: ['hello world'],
				context_channel_id: channelId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		if (isSearchResult(result)) {
			for (const msg of result.messages) {
				expect(msg.content).toContain('hello world');
			}
			expect(result.messages.every((m) => !m.content.includes('HELLO WORLD'))).toBe(true);
			expect(result.messages.every((m) => !m.content.includes('Hello World'))).toBe(true);
		}
	});
	test('multiple exact_phrases requires all phrases to be present', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Multi Phrase Guild');
		const channelId = guild.system_channel_id!;
		const timestamp = Date.now();
		const tag = `mp-${timestamp}`;
		await sendMessage(harness, owner.token, channelId, `${tag} good morning and good night`);
		await sendMessage(harness, owner.token, channelId, `${tag} good morning everyone`);
		await sendMessage(harness, owner.token, channelId, `${tag} good night everyone`);
		await markChannelAsIndexed(harness, channelId);
		const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
			.post('/search/messages')
			.body({
				exact_phrases: ['good morning', 'good night'],
				context_channel_id: channelId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		if (isSearchResult(result)) {
			for (const msg of result.messages) {
				expect(msg.content).toContain('good morning');
				expect(msg.content).toContain('good night');
			}
		}
	});
	test('exact_phrases combined with regular content', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Combined Content Guild');
		const channelId = guild.system_channel_id!;
		const timestamp = Date.now();
		const tag = `cc-${timestamp}`;
		await sendMessage(harness, owner.token, channelId, `${tag} hello world foo`);
		await sendMessage(harness, owner.token, channelId, `${tag} hello world bar`);
		await sendMessage(harness, owner.token, channelId, `${tag} goodbye world foo`);
		await markChannelAsIndexed(harness, channelId);
		const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
			.post('/search/messages')
			.body({
				content: tag,
				exact_phrases: ['hello world'],
				context_channel_id: channelId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		if (isSearchResult(result)) {
			for (const msg of result.messages) {
				expect(msg.content).toContain('hello world');
				expect(msg.content).toContain(tag);
			}
		}
	});
	test('exact_phrases combined with author_id filter', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Author Filter Phrase Guild');
		const channelId = guild.system_channel_id!;
		const timestamp = Date.now();
		const tag = `af-${timestamp}`;
		await sendMessage(harness, owner.token, channelId, `${tag} hello world`);
		await markChannelAsIndexed(harness, channelId);
		const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
			.post('/search/messages')
			.body({
				exact_phrases: ['hello world'],
				author_id: [owner.userId],
				context_channel_id: channelId,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		if (isSearchResult(result)) {
			for (const msg of result.messages) {
				expect(msg.content).toContain('hello world');
				expect(msg.author.id).toBe(owner.userId);
			}
		}
	});
});
