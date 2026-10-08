// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

interface RpcEnvelope<T> {
	type: string;
	data: T;
}

describe('RpcService thread collections', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let member: TestAccount;
	let guildId: string;
	let channelId: string;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function rpc<T>(body: Record<string, unknown>): Promise<RpcEnvelope<T>> {
		return createBuilder<RpcEnvelope<T>>(harness, '').post('/test/rpc-session-init').body(body).execute();
	}

	async function setup(): Promise<void> {
		owner = await createTestAccount(harness);
		member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'rpc threads');
		guildId = guild.id;
		channelId = (await createChannel(harness, owner.token, guildId, 'general')).id;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
	}

	async function startThread(type: number = ChannelTypes.PUBLIC_THREAD): Promise<ThreadChannelResponse> {
		return threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'rpc', type})
			.expect(201)
			.execute();
	}

	test('control guild collections carry no thread keys', async () => {
		await setup();
		const channels = await rpc<Record<string, unknown>>({
			type: 'guild_collection',
			guild_id: guildId,
			collection: 'channels',
		});
		for (const key of ['thread_gate', 'thread_tainted', 'threads', 'thread_members', 'thread_only_channels']) {
			expect(channels.data).not.toHaveProperty(key);
		}
	});

	test('active guild channel collections carry the gate and forums but load threads separately', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		await startThread();
		const spy = vi.spyOn(ThreadRepository.prototype, 'listActiveThreads');
		try {
			const {data} = await rpc<{
				thread_gate: {active: boolean; config_version: number};
				thread_tainted: boolean;
				thread_only_channels: Array<unknown>;
			}>({type: 'guild_collection', guild_id: guildId, collection: 'channels'});
			expect(data.thread_gate.active).toBe(true);
			expect(data.thread_gate.config_version).toBeGreaterThan(0);
			expect(data.thread_tainted).toBe(true);
			expect(data.thread_only_channels).toEqual([]);
			for (const key of ['threads', 'thread_members', 'thread_parent_settings', 'thread_load_failed']) {
				expect(data).not.toHaveProperty(key);
			}
			expect(spy).not.toHaveBeenCalled();
		} finally {
			spy.mockRestore();
		}
	});

	test('forums without a parent config row carry the default surface fields', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		const forum = await threadsRequest<{id: string}>(harness, owner.token)
			.post(`/guilds/${guildId}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
			.execute();
		const defaults = {
			available_tags: [],
			default_tag_setting: 'match_some',
			default_forum_layout: 0,
			flags: 0,
		};
		const collection = await rpc<{thread_only_channels: Array<Record<string, unknown>>}>({
			type: 'guild_collection',
			guild_id: guildId,
			collection: 'channels',
		});
		expect(collection.data.thread_only_channels).toEqual([expect.objectContaining({id: forum.id, ...defaults})]);
		const flip = await rpc<{thread_parent_settings: Array<Record<string, unknown>>}>({
			type: 'guild_thread_flip_data',
			guild_id: guildId,
			config_version: 0,
			paged_members: true,
		});
		expect(flip.data.thread_parent_settings).toContainEqual(
			expect.objectContaining({channel_id: forum.id, ...defaults}),
		);
		expect(flip.data.thread_parent_settings.map((settings) => settings.channel_id)).not.toContain(channelId);
	});

	test('a tainted guild that left the experiment keeps only the taint marker', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		await startThread();
		await setChannelThreadsConfig({enabled: true, user_basis_points: 10000, disabled_guild_ids: [guildId]});
		const {data} = await rpc<Record<string, unknown>>({
			type: 'guild_collection',
			guild_id: guildId,
			collection: 'channels',
		});
		expect(data.thread_tainted).toBe(true);
		for (const key of ['thread_gate', 'threads', 'thread_members', 'thread_only_channels']) {
			expect(data).not.toHaveProperty(key);
		}
	});

	test('a tainted guild keeps the taint marker after the kill switch', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		await startThread();
		await setChannelThreadsConfig({enabled: false});
		const {data} = await rpc<Record<string, unknown>>({
			type: 'guild_collection',
			guild_id: guildId,
			collection: 'channels',
		});
		expect(data.thread_tainted).toBe(true);
		for (const key of ['thread_gate', 'threads', 'thread_members', 'thread_only_channels']) {
			expect(data).not.toHaveProperty(key);
		}
	});

	test('counts forum post unreads after the acknowledged message', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		await ensureSessionStarted(harness, owner.token);
		const forum = await threadsRequest<{id: string}>(harness, owner.token)
			.post(`/guilds/${guildId}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
			.execute();
		const createPost = () =>
			threadsRequest<{id: string}>(harness, owner.token)
				.post(`/channels/${forum.id}/threads`)
				.body({name: 'post', message: {content: 'hello'}})
				.expect(201)
				.execute();
		const post = await createPost();
		const quiet = await createPost();
		const replies: Array<string> = [];
		for (const content of ['one', 'two', 'three']) {
			const message = await threadsRequest<{id: string}>(harness, owner.token)
				.post(`/channels/${post.id}/messages`)
				.body({content})
				.execute();
			replies.push(message.id);
		}
		const textThread = await startThread();

		type Unreads = {threads: Array<{thread_id: string; count?: number; missing?: boolean}>};
		const request = {
			type: 'forum_unreads',
			guild_id: guildId,
			channel_id: forum.id,
			user_id: member.userId,
			threads: [
				{thread_id: post.id, ack_message_id: post.id},
				{thread_id: quiet.id},
				{thread_id: textThread.id, ack_message_id: textThread.id},
			],
		};
		const unreads = await rpc<Unreads>(request);
		expect(unreads.data.threads).toEqual([
			{thread_id: post.id, count: 3},
			{thread_id: quiet.id, missing: true},
		]);
		const caughtUp = await rpc<Unreads>({
			...request,
			threads: [{thread_id: post.id, ack_message_id: replies[1]}],
		});
		expect(caughtUp.data.threads).toEqual([{thread_id: post.id, count: 1}]);

		await setChannelThreadsConfig({enabled: false});
		const off = await rpc<Unreads>(request);
		expect(off.data.threads).toEqual([]);
	});

	test('serves memberships, member pages and flip data', async () => {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await setup();
		const active = await startThread();
		const privateThread = await startThread(ChannelTypes.PRIVATE_THREAD);
		const archived = await startThread();
		await new ChannelRepository().threads.updateState(createChannelID(BigInt(archived.id)), () => ({archived: true}));
		await threadsRequest(harness, member.token).put(`/channels/${active.id}/thread-members/@me`).expect(204).execute();

		const memberships = await rpc<{members: Array<{id: string; user_id: string}>}>({
			type: 'get_thread_memberships',
			guild_id: guildId,
			user_id: owner.userId,
		});
		expect(memberships.data.members.map((entry) => entry.id).sort()).toEqual([active.id, privateThread.id].sort());

		type MembersPage = {
			members: Array<{id: string; user_id: string}>;
			has_more: boolean;
			next_thread_id: string | null;
			next_after_user_id: string | null;
		};
		const firstPage = await rpc<MembersPage>({
			type: 'list_thread_members',
			guild_id: guildId,
			thread_ids: [active.id, privateThread.id],
			limit: 1,
		});
		expect(firstPage.data.members).toHaveLength(1);
		expect(firstPage.data).toMatchObject({has_more: true, next_thread_id: active.id});
		const secondPage = await rpc<MembersPage>({
			type: 'list_thread_members',
			guild_id: guildId,
			thread_ids: [active.id, privateThread.id],
			limit: 2,
			after_user_id: firstPage.data.next_after_user_id,
		});
		expect(secondPage.data).toMatchObject({has_more: false, next_thread_id: null});
		expect(secondPage.data.members.map((entry) => entry.id)).toEqual([active.id, privateThread.id]);
		expect([firstPage.data.members[0]!.user_id, secondPage.data.members[0]!.user_id].sort()).toEqual(
			[owner.userId, member.userId].sort(),
		);
		const boundary = await rpc<MembersPage>({
			type: 'list_thread_members',
			guild_id: guildId,
			thread_ids: [active.id, privateThread.id],
			limit: 2,
		});
		expect(boundary.data).toMatchObject({has_more: true, next_thread_id: privateThread.id, next_after_user_id: null});
		const foreign = await rpc<{members: Array<unknown>}>({
			type: 'list_thread_members',
			guild_id: '1',
			thread_ids: [active.id],
		});
		expect(foreign.data.members).toEqual([]);

		const flip = await rpc<{
			config_version: number;
			thread_gate: {active: boolean};
			thread_tainted: boolean;
			channels: Array<{id: string}>;
			roles: Array<unknown>;
			threads: Array<{id: string}>;
			thread_members: Array<{user_id: string}>;
		}>({type: 'guild_thread_flip_data', guild_id: guildId, config_version: 0});
		expect(flip.data.thread_gate.active).toBe(true);
		expect(flip.data.thread_tainted).toBe(true);
		expect(flip.data.channels.map((channel) => channel.id)).toContain(channelId);
		expect(flip.data.roles.length).toBeGreaterThan(0);
		expect(flip.data.threads.map((entry) => entry.id).sort()).toEqual([active.id, privateThread.id].sort());
		expect(flip.data.thread_members.map((entry) => entry.user_id).sort()).toEqual(
			[owner.userId, owner.userId, member.userId].sort(),
		);
		const paged = await rpc<Record<string, unknown> & {threads: Array<{id: string}>}>({
			type: 'guild_thread_flip_data',
			guild_id: guildId,
			config_version: 0,
			paged_members: true,
		});
		expect(paged.data.threads.map((entry) => entry.id).sort()).toEqual([active.id, privateThread.id].sort());
		expect(paged.data).not.toHaveProperty('thread_members');
		expect(paged.data).toHaveProperty('thread_parent_settings');

		await setChannelThreadsConfig({enabled: false});
		const off = await rpc<Record<string, unknown>>({
			type: 'guild_thread_flip_data',
			guild_id: guildId,
			config_version: 0,
		});
		expect(off.data.thread_gate).toMatchObject({active: false});
		expect(off.data).not.toHaveProperty('threads');
	});
});
