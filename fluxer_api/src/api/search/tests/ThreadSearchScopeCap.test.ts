// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, markGuildChannelsAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TEST_TIMEOUTS, wait} from '@app/api/test/TestConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@fluxer/constants/src/ThreadConstants', async (importOriginal) => ({
	...(await importOriginal<typeof import('@fluxer/constants/src/ThreadConstants')>()),
	THREAD_SCOPE_MAX: 1,
}));

interface SearchResult {
	messages: Array<{channel_id: string}>;
}

describe('thread search scope cap', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	it('caps the thread scope across every guild of a global search', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const channelIds = new Set<string>();
		const threadIds = new Set<string>();
		for (const name of ['first', 'second']) {
			const guild = await createGuild(harness, owner.token, name);
			const channel = await createChannel(harness, owner.token, guild.id, 'general');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
				.post(`/channels/${channel.id}/threads`)
				.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
				.expect(201)
				.execute();
			await sendMessage(harness, owner.token, channel.id, 'needle channel');
			await threadsRequest(harness, owner.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'needle thread'})
				.execute();
			channelIds.add(channel.id);
			threadIds.add(thread.id);
		}
		let result: SearchResult = {messages: []};
		for (let attempt = 0; attempt < 20; attempt++) {
			result = await threadsRequest<SearchResult>(harness, owner.token)
				.post('/search/messages')
				.body({content: 'needle', scope: 'all_guilds'})
				.expect(200)
				.execute();
			if (result.messages.length >= 3) break;
			await wait(TEST_TIMEOUTS.QUICK);
		}
		const searched = new Set(result.messages.map((message) => message.channel_id));
		expect([...channelIds].every((id) => searched.has(id))).toBe(true);
		expect([...threadIds].filter((id) => searched.has(id))).toEqual([[...threadIds][1]]);
	});

	it('keeps the newest thread of the guild when the cap drops older ones in earlier channels', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'spread');
		const first = await createChannel(harness, owner.token, guild.id, 'first');
		const second = await createChannel(harness, owner.token, guild.id, 'second');
		await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
		const threadIds: Array<string> = [];
		for (const channel of [first, second]) {
			const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
				.post(`/channels/${channel.id}/threads`)
				.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
				.expect(201)
				.execute();
			await threadsRequest(harness, owner.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'needle thread'})
				.execute();
			threadIds.push(thread.id);
		}
		let result: SearchResult = {messages: []};
		for (let attempt = 0; attempt < 20; attempt++) {
			result = await threadsRequest<SearchResult>(harness, owner.token)
				.post('/search/messages')
				.body({content: 'needle', context_guild_id: guild.id})
				.expect(200)
				.execute();
			if (result.messages.length >= 1) break;
			await wait(TEST_TIMEOUTS.QUICK);
		}
		expect(result.messages.map((message) => message.channel_id)).toEqual([threadIds[1]]);
	});

	it.each([
		['guild', (guildId: string) => ({context_guild_id: guildId})],
		['all_guilds', () => ({scope: 'all_guilds'})],
		['all', () => ({scope: 'all'})],
	] as const)('searches an explicitly named older thread past the scope cap in %s search', async (_scope, body) => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'capped');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
		const startThread = (type: number) =>
			threadsRequest<ThreadChannelResponse>(harness, owner.token)
				.post(`/channels/${channel.id}/threads`)
				.body({name: 'topic', type})
				.expect(201)
				.execute();
		const older = await startThread(ChannelTypes.PRIVATE_THREAD);
		await startThread(ChannelTypes.PUBLIC_THREAD);
		await threadsRequest(harness, owner.token)
			.post(`/channels/${older.id}/messages`)
			.body({content: 'needle older'})
			.execute();
		let result: SearchResult = {messages: []};
		for (let attempt = 0; attempt < 20; attempt++) {
			result = await threadsRequest<SearchResult>(harness, owner.token)
				.post('/search/messages')
				.body({content: 'needle', ...body(guild.id), channel_ids: [older.id]})
				.expect(200)
				.execute();
			if (result.messages.length >= 1) break;
			await wait(TEST_TIMEOUTS.QUICK);
		}
		expect(result.messages.map((message) => message.channel_id)).toEqual([older.id]);
	});
});
