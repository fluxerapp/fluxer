// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {clearReadStateChannelMetaCacheForTesting} from '@app/api/read_state/ReadStateChannelMeta';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ReadStateFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface ReadStateWire {
	id: string;
	mention_count: number;
	last_message_id: string | null;
	flags?: number;
}

interface SessionData {
	read_states: Array<ReadStateWire>;
	user_guild_settings: Array<Record<string, unknown>>;
}

const THREAD_FLAGS = ReadStateFlags.IS_GUILD_CHANNEL | ReadStateFlags.IS_THREAD;

describe('thread read states', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
		clearReadStateChannelMetaCacheForTesting();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function session(account: TestAccount, capable = false): Promise<SessionData> {
		const response = await createBuilder<{data: SessionData}>(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'session',
				token: account.token,
				version: 1,
				ip: '127.0.0.1',
				...(capable ? {thread_channels_capable: true} : {}),
			})
			.expect(200)
			.execute();
		return response.data;
	}

	async function activeSetup() {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'threads');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		return {owner, member, guildId: guild.id, channelId: channel.id, thread};
	}

	it('keeps the READY payload of a control user identical once the experiment is live elsewhere', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		const guild = await createGuild(harness, account.token, 'control');
		const channel = await createChannel(harness, account.token, guild.id, 'general');
		const message = await sendMessage(harness, account.token, channel.id, 'hello');
		await createBuilder(harness, account.token)
			.post(`/channels/${channel.id}/messages/${message.id}/ack`)
			.body({})
			.expect(204)
			.execute();
		await createBuilder(harness, account.token)
			.patch(`/users/@me/guilds/${guild.id}/settings`)
			.body({
				channel_overrides: {
					[channel.id]: {collapsed: false, message_notifications: 1, muted: true, mute_config: null},
				},
			})
			.expect(200)
			.execute();
		const baseline = await session(account);
		const other = await createTestAccount(harness);
		const otherGuild = await createGuild(harness, other.token, 'enrolled');
		await setChannelThreadsConfig({
			enabled: true,
			enabled_guild_ids: [otherGuild.id],
			included_user_ids: [other.userId],
		});
		expect(await session(account)).toEqual(baseline);
		expect(await session(account, true)).toEqual(baseline);
	});

	it('stamps the author self-ack and shows thread read states only to capable sessions', async () => {
		const s = await activeSetup();
		const sent = await threadsRequest<MessageResponse>(harness, s.member.token)
			.post(`/channels/${s.thread.id}/messages`)
			.body({content: 'hello'})
			.execute();
		const capable = await session(s.member, true);
		expect(capable.read_states.find((readState) => readState.id === s.thread.id)).toMatchObject({
			last_message_id: sent.id,
			flags: THREAD_FLAGS,
		});
		const incapable = await session(s.member);
		expect(incapable.read_states.some((readState) => readState.id === s.thread.id)).toBe(false);
		await setChannelThreadsConfig({enabled: false});
		const killed = await session(s.member, true);
		expect(killed.read_states.some((readState) => readState.id === s.thread.id)).toBe(false);
	});

	it('stamps the actor self-ack of thread system messages', async () => {
		const s = await activeSetup();
		await threadsRequest(harness, s.owner.token)
			.put(`/channels/${s.thread.id}/thread-members/${s.member.userId}`)
			.expect(204)
			.execute();
		await threadsRequest(harness, s.owner.token)
			.patch(`/channels/${s.thread.id}`)
			.body({name: 'renamed'})
			.expect(200)
			.execute();
		const capable = await session(s.owner, true);
		expect(capable.read_states.find((readState) => readState.id === s.thread.id)).toMatchObject({
			flags: THREAD_FLAGS,
		});
		const incapable = await session(s.owner);
		expect(incapable.read_states.some((readState) => readState.id === s.thread.id)).toBe(false);
	});

	it('returns flags from the bulk ack and scopes MESSAGE_ACK to thread sessions', async () => {
		const s = await activeSetup();
		const sent = await threadsRequest<MessageResponse>(harness, s.member.token)
			.post(`/channels/${s.thread.id}/messages`)
			.body({content: 'hello'})
			.execute();
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		const acked = await threadsRequest<{read_states: Array<ReadStateWire>}>(harness, s.owner.token)
			.post('/read-states/ack')
			.body({read_states: [{channel_id: s.thread.id, message_id: sent.id}]})
			.expect(200)
			.execute();
		expect(acked.read_states).toEqual([expect.objectContaining({id: s.thread.id, flags: THREAD_FLAGS})]);
		const ack = dispatch.mock.calls.find(([params]) => params.event === 'MESSAGE_ACK')?.[0];
		expect(ack?.data).toMatchObject({flags: THREAD_FLAGS, __thread_scoped: s.guildId});
		const incapable = await threadsRequest<{read_states: Array<ReadStateWire>}>(harness, s.owner.token, {
			capable: false,
		})
			.post('/read-states/ack')
			.body({read_states: [{channel_id: s.thread.id, message_id: sent.id}]})
			.expect(200)
			.execute();
		expect(incapable.read_states).toEqual([]);
	});

	it('keeps a marked read state when any session calls the deprecated clear endpoint', async () => {
		const s = await activeSetup();
		await threadsRequest<MessageResponse>(harness, s.member.token)
			.post(`/channels/${s.thread.id}/messages`)
			.body({content: 'hello'})
			.execute();
		await threadsRequest(harness, s.member.token, {capable: false})
			.delete(`/channels/${s.thread.id}/messages/ack`)
			.expect(204)
			.execute();
		expect((await session(s.member, true)).read_states.some((readState) => readState.id === s.thread.id)).toBe(true);
		await threadsRequest(harness, s.member.token).delete(`/channels/${s.thread.id}/messages/ack`).expect(204).execute();
		expect((await session(s.member, true)).read_states.some((readState) => readState.id === s.thread.id)).toBe(true);
	});

	it('leaves control acks of ordinary channels unmarked for viewers', async () => {
		const s = await activeSetup();
		const message = await sendMessage(harness, s.owner.token, s.channelId, 'plain');
		await threadsRequest(harness, s.member.token)
			.post(`/channels/${s.channelId}/messages/${message.id}/ack`)
			.body({})
			.expect(204)
			.execute();
		const readState = (await session(s.member, true)).read_states.find((entry) => entry.id === s.channelId);
		expect(readState).toBeDefined();
		expect(readState).not.toHaveProperty('flags');
	});
});
