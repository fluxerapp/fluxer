// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags, ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	channelId: string;
}

const repository = new ChannelRepository();

describe('thread message lifecycle', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'lifecycle');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		return {owner, member, guildId: guild.id, channelId: channel.id};
	}

	async function threadFromMessage(s: Setup, sourceId: string) {
		return threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.channelId}/messages/${sourceId}/threads`)
			.body({name: 'discussion'})
			.expect(201)
			.execute();
	}

	async function send(token: string, channelId: string, content: string) {
		return threadsRequest<MessageResponse>(harness, token)
			.post(`/channels/${channelId}/messages`)
			.body({content})
			.execute();
	}

	async function page(token: string, channelId: string, query: string) {
		return threadsRequest<Array<MessageResponse>>(harness, token)
			.get(`/channels/${channelId}/messages?${query}`)
			.execute();
	}

	function hasStarter(messages: Array<MessageResponse>): boolean {
		return messages.some((message) => message.type === MessageTypes.THREAD_STARTER_MESSAGE);
	}

	async function messageCount(token: string, threadId: string): Promise<number | undefined> {
		const thread = await threadsRequest<ThreadChannelResponse>(harness, token).get(`/channels/${threadId}`).execute();
		return thread.message_count;
	}

	it('adds the starter only to pages that reach the start of the thread', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.member.token, s.channelId, 'source');
		const thread = await threadFromMessage(s, source.id);
		const sent: Array<MessageResponse> = [];
		for (const content of ['one', 'two', 'three', 'four']) sent.push(await send(s.member.token, thread.id, content));
		const [first, , , last] = sent;
		expect(hasStarter(await page(s.member.token, thread.id, 'limit=2'))).toBe(false);
		expect(hasStarter(await page(s.member.token, thread.id, `around=${last!.id}&limit=3`))).toBe(false);
		expect(hasStarter(await page(s.member.token, thread.id, `around=${first!.id}&limit=3`))).toBe(true);
		expect(await page(s.member.token, thread.id, `before=${first!.id}`)).toMatchObject([
			{id: thread.id, type: MessageTypes.THREAD_STARTER_MESSAGE},
		]);
		const latest = await page(s.member.token, thread.id, 'limit=50');
		expect(latest).toHaveLength(5);
		expect(latest.at(-1)?.type).toBe(MessageTypes.THREAD_STARTER_MESSAGE);
	});

	it('sends one member update with current flags when a send unarchives the thread', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.owner.token, s.channelId, 'source');
		const thread = await threadFromMessage(s, source.id);
		await threadsRequest(harness, s.member.token)
			.put(`/channels/${thread.id}/thread-members/@me`)
			.expect(204)
			.execute();
		await repository.threads.updateState(createChannelID(BigInt(thread.id)), () => ({archived: true}));
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await send(s.member.token, thread.id, 'back again');
		const events = dispatch.mock.calls.map(([params]) => params);
		expect(events.filter((event) => event.event === 'THREAD_MEMBER_UPDATE')).toEqual([]);
		const update = events.find((event) => event.event === 'THREAD_UPDATE')?.data as {
			_fluxer_members?: Array<{user_id: string; flags: number}>;
		};
		expect(update._fluxer_members?.find((member) => member.user_id === s.member.userId)?.flags).toBe(
			ThreadMemberFlags.HAS_INTERACTED,
		);
	});

	it('clears HAS_THREAD on the source when the thread is deleted and allows a new thread', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.member.token, s.channelId, 'source');
		const thread = await threadFromMessage(s, source.id);
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await threadsRequest(harness, s.owner.token).delete(`/channels/${thread.id}`).expect(204).execute();
		const events = dispatch.mock.calls.map(([params]) => params);
		expect(events.map((event) => event.event).slice(0, 2)).toEqual(['THREAD_DELETE', 'MESSAGE_UPDATE']);
		expect(events[1]?.data).toMatchObject({id: source.id, __thread_only_update: true});
		const stored = await repository.messages.getMessage(
			createChannelID(BigInt(s.channelId)),
			createMessageID(BigInt(source.id)),
		);
		expect((stored!.flags & ServerMessageFlags.HAS_THREAD) === 0).toBe(true);
		await threadFromMessage(s, source.id);
	});

	it('leaves a source deleted during thread creation deleted', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.member.token, s.channelId, 'source');
		const channelId = createChannelID(BigInt(s.channelId));
		const messageId = createMessageID(BigInt(source.id));
		const create = ThreadRepository.prototype.create;
		vi.spyOn(ThreadRepository.prototype, 'create').mockImplementation(async function (this: ThreadRepository, params) {
			await repository.messages.deleteMessage(channelId, messageId, createUserID(BigInt(s.member.userId)));
			return create.call(this, params);
		});
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await threadFromMessage(s, source.id);
		expect(await repository.messages.getMessage(channelId, messageId)).toBeNull();
		const events = dispatch.mock.calls.map(([params]) => params);
		expect(events.some((event) => event.event === 'MESSAGE_UPDATE')).toBe(false);
	});

	it('keeps the thread message count in step with admin and bulk deletes', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.owner.token, s.channelId, 'source');
		const thread = await threadFromMessage(s, source.id);
		const first = await send(s.member.token, thread.id, 'first');
		const second = await send(s.member.token, thread.id, 'second');
		expect(await messageCount(s.member.token, thread.id)).toBe(2);
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		await createBuilder(harness, admin.token)
			.delete(`/admin/channels/${thread.id}/messages/${first.id}`)
			.expect(200)
			.execute();
		expect(await messageCount(s.member.token, thread.id)).toBe(1);
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const unknownId = (BigInt(second.id) + 1n).toString();
		await threadsRequest(harness, s.owner.token)
			.post(`/channels/${thread.id}/messages/bulk-delete`)
			.body({message_ids: [second.id, unknownId]})
			.expect(204)
			.execute();
		expect(await messageCount(s.member.token, thread.id)).toBe(0);
		const bulk = dispatch.mock.calls.find(([params]) => params.event === 'MESSAGE_DELETE_BULK')?.[0].data;
		expect(bulk).toMatchObject({ids: [second.id]});
	});
});
