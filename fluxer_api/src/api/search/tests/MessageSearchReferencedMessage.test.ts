// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	MessageResponse,
	MessageSearchResponse,
	MessageSearchResultsResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {markChannelAsIndexed, markGuildChannelsAsIndexed, sendMessage} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {SearchService} from '../SearchService';

function isSearchResult(response: MessageSearchResponse): response is MessageSearchResultsResponse {
	return 'messages' in response;
}

describe('Message Search Referenced Message', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await harness.shutdown();
	});

	async function sendReply(
		token: string,
		channelId: string,
		guildId: string,
		content: string,
		targetMessageId: string,
	): Promise<MessageResponse> {
		return createBuilder<MessageResponse>(harness, token)
			.post(`/channels/${channelId}/messages`)
			.body({
				content,
				message_reference: {
					message_id: targetMessageId,
					channel_id: channelId,
					guild_id: guildId,
					type: MessageReferenceTypes.DEFAULT,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function searchAndCaptureServiceResult(
		token: string,
		body: Record<string, unknown>,
	): Promise<MessageSearchResponse> {
		const spy = vi.spyOn(SearchService.prototype, 'searchMessages');
		try {
			await createBuilder(harness, token).post('/search/messages').body(body).expect(HTTP_STATUS.OK).execute();
			const [invocation] = spy.mock.results;
			if (!invocation || invocation.type !== 'return') {
				throw new Error('SearchService.searchMessages did not return a result');
			}
			return await invocation.value;
		} finally {
			spy.mockRestore();
		}
	}

	test('channel scope omits referenced_message from reply hits', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Reply Search Channel Scope');
		const channelId = guild.system_channel_id!;
		const marker = `reply-search-${Date.now()}`;
		const original = await sendMessage(harness, account.token, channelId, `${marker} original`);
		const reply = await sendReply(account.token, channelId, guild.id, `${marker} answer`, original.id);
		await markChannelAsIndexed(harness, channelId);
		const result = await searchAndCaptureServiceResult(account.token, {
			content: `${marker} answer`,
			context_channel_id: channelId,
		});
		expect(isSearchResult(result)).toBe(true);
		if (!isSearchResult(result)) return;
		const hit = result.messages.find((message) => message.id === reply.id);
		expect(hit).toBeDefined();
		expect(hit?.message_reference?.message_id).toBe(original.id);
		expect(Object.hasOwn(hit!, 'referenced_message')).toBe(false);
	});

	test('guild scope omits referenced_message from reply hits', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Reply Search Guild Scope');
		const channelId = guild.system_channel_id!;
		const marker = `reply-guild-search-${Date.now()}`;
		const original = await sendMessage(harness, account.token, channelId, `${marker} original`);
		const reply = await sendReply(account.token, channelId, guild.id, `${marker} answer`, original.id);
		await markGuildChannelsAsIndexed(harness, account.token, guild.id);
		const result = await searchAndCaptureServiceResult(account.token, {
			content: `${marker} answer`,
			context_guild_id: guild.id,
		});
		expect(isSearchResult(result)).toBe(true);
		if (!isSearchResult(result)) return;
		const hit = result.messages.find((message) => message.id === reply.id);
		expect(hit).toBeDefined();
		expect(hit?.message_reference?.message_id).toBe(original.id);
		expect(Object.hasOwn(hit!, 'referenced_message')).toBe(false);
	});
});
