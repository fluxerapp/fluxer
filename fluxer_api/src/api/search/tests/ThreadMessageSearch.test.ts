// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, markGuildChannelsAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {TEST_TIMEOUTS, wait} from '@app/api/test/TestConstants';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SearchResult {
	messages: Array<{id: string; channel_id: string; flags: number}>;
	channels: Array<{id: string}>;
	threads?: Array<{id: string}>;
	members?: Array<{id?: string; user_id?: string}>;
}

describe('thread message search', () => {
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

	async function setup() {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'search');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
		const startThread = (type: number) =>
			threadsRequest<ThreadChannelResponse>(harness, owner.token)
				.post(`/channels/${channel.id}/threads`)
				.body({name: 'topic', type})
				.expect(201)
				.execute();
		const publicThread = await startThread(ChannelTypes.PUBLIC_THREAD);
		const privateThread = await startThread(ChannelTypes.PRIVATE_THREAD);
		const post = (token: string, channelId: string, content: string) =>
			threadsRequest<MessageResponse>(harness, token).post(`/channels/${channelId}/messages`).body({content}).execute();
		const source = await sendMessage(harness, owner.token, channel.id, 'needle source');
		await threadsRequest(harness, owner.token)
			.post(`/channels/${channel.id}/messages/${source.id}/threads`)
			.body({name: 'from source'})
			.expect(201)
			.execute();
		await post(member.token, publicThread.id, 'needle public');
		await post(owner.token, privateThread.id, 'needle private');
		return {owner, member, guildId: guild.id, channelId: channel.id, publicThread, privateThread, source};
	}

	async function search(
		account: TestAccount,
		body: Record<string, unknown>,
		options: {capable?: boolean; status?: number; code?: string; expected?: number} = {},
	): Promise<SearchResult> {
		for (let attempt = 0; ; attempt++) {
			const result = await threadsRequest<SearchResult>(harness, account.token, {capable: options.capable ?? true})
				.post('/search/messages')
				.body({content: 'needle', ...body})
				.expect(options.status ?? 200, options.code)
				.execute();
			if (options.status !== undefined || attempt >= 20) return result;
			if ('messages' in result && result.messages.length >= (options.expected ?? 1)) return result;
			await wait(TEST_TIMEOUTS.QUICK);
		}
	}

	it('searches public threads for viewers and returns threads and members', async () => {
		const s = await setup();
		const result = await search(s.member, {context_guild_id: s.guildId}, {expected: 2});
		const channelIds = new Set(result.messages.map((message) => message.channel_id));
		expect(channelIds).toEqual(new Set([s.channelId, s.publicThread.id]));
		expect(result.channels.map((channel) => channel.id)).toEqual([s.channelId]);
		expect(result.threads?.map((thread) => thread.id)).toEqual([s.publicThread.id]);
		expect(result.members).toEqual([expect.objectContaining({id: s.publicThread.id, user_id: s.member.userId})]);
		const source = result.messages.find((message) => message.id === s.source.id);
		expect((source?.flags ?? 0) & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
	});

	it('includes private threads the viewer moderates', async () => {
		const s = await setup();
		const result = await search(s.owner, {context_guild_id: s.guildId}, {expected: 3});
		expect(new Set(result.messages.map((message) => message.channel_id))).toEqual(
			new Set([s.channelId, s.publicThread.id, s.privateThread.id]),
		);
	});

	it('drops private threads from a timed out thread moderator who is not a member', async () => {
		const s = await setup();
		const role = await threadsRequest<GuildRoleResponse>(harness, s.owner.token)
			.post(`/guilds/${s.guildId}/roles`)
			.body({name: 'mods', permissions: ThreadPermissionFlags.MANAGE_THREADS.toString()})
			.execute();
		await addMemberRole(harness, s.owner.token, s.guildId, s.member.userId, role.id);
		const moderated = await search(s.member, {context_guild_id: s.guildId}, {expected: 3});
		expect(new Set(moderated.messages.map((message) => message.channel_id))).toEqual(
			new Set([s.channelId, s.publicThread.id, s.privateThread.id]),
		);
		await search(s.member, {context_guild_id: s.guildId, channel_ids: [s.privateThread.id]});
		await threadsRequest(harness, s.owner.token)
			.patch(`/guilds/${s.guildId}/members/${s.member.userId}`)
			.body({communication_disabled_until: new Date(Date.now() + 10 * 60 * 1000).toISOString()})
			.expect(200)
			.execute();
		const timedOut = await search(s.member, {context_guild_id: s.guildId}, {expected: 2});
		expect(new Set(timedOut.messages.map((message) => message.channel_id))).toEqual(
			new Set([s.channelId, s.publicThread.id]),
		);
		await search(s.member, {context_guild_id: s.guildId, channel_ids: [s.privateThread.id]}, {status: 403});
	});

	it('narrows to an explicit thread and refuses a private thread the viewer cannot see', async () => {
		const s = await setup();
		const result = await search(s.member, {context_guild_id: s.guildId, channel_ids: [s.publicThread.id]});
		expect(result.messages.map((message) => message.channel_id)).toEqual([s.publicThread.id]);
		await search(s.member, {context_guild_id: s.guildId, channel_ids: [s.privateThread.id]}, {status: 403});
	});

	it('extends cross-guild search with the same thread scope', async () => {
		const s = await setup();
		const viewer = await search(s.member, {scope: 'all_guilds'}, {expected: 2});
		expect(new Set(viewer.messages.map((message) => message.channel_id))).toEqual(
			new Set([s.channelId, s.publicThread.id]),
		);
		const control = await search(s.member, {scope: 'all_guilds'}, {capable: false});
		expect(control.messages.every((message) => message.channel_id === s.channelId)).toBe(true);
		expect(control).not.toHaveProperty('threads');
	});

	it('keeps control searches identical to a guild without threads', async () => {
		const s = await setup();
		const result = await search(s.member, {context_guild_id: s.guildId}, {capable: false});
		expect(result.messages.every((message) => message.channel_id === s.channelId)).toBe(true);
		expect(result).not.toHaveProperty('threads');
		expect(result).not.toHaveProperty('members');
		for (const message of result.messages) {
			expect(message.flags & ServerMessageFlags.HAS_THREAD).toBe(0);
		}
		await search(
			s.member,
			{context_guild_id: s.guildId, channel_ids: [s.publicThread.id]},
			{capable: false, status: 400, code: 'INVALID_FORM_BODY'},
		);
	});
	it('never hands an indexed thread created notice to a control search', async () => {
		const s = await setup();
		const [notice] = (await new ChannelRepository().messages.listMessages(createChannelID(BigInt(s.channelId)))).filter(
			(message) => message.type === MessageTypes.THREAD_CREATED,
		);
		if (!notice) expect.fail('Expected a thread created notice');
		await getMessageSearchService()!.indexMessage(notice);
		const viewer = await search(s.owner, {context_guild_id: s.guildId, content: 'topic'});
		expect(viewer.messages.map((message) => message.id)).toEqual([notice.id.toString()]);
		const control = await search(
			s.owner,
			{context_guild_id: s.guildId, content: 'topic'},
			{capable: false, expected: 0},
		);
		expect(control.messages).toEqual([]);
	});
});
