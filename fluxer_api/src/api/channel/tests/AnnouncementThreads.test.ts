// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {disableCrosspostWorker} from '@app/api/channel/tests/AnnouncementTestUtils';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	type FanoutWorld,
	followInto,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags, THREAD_MESSAGE_FLAG_MASK} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {
	ArchivedThreadsResponse,
	ThreadChannelResponse,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	textId: string;
	announcementId: string;
}

const repository = new ChannelRepository();

async function archive(threadId: string): Promise<void> {
	await repository.threads.updateState(createChannelID(BigInt(threadId)), () => ({archived: true}));
}

describe('announcement threads', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		clearChannelThreadsTaintCacheForTesting();
	});

	afterAll(async () => {
		clearChannelThreadsTaintCacheForTesting();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'news threads');
		const text = await createChannel(harness, owner.token, guild.id, 'general');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const invite = await createChannelInvite(harness, owner.token, text.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, member.token);
		return {owner, member, guildId: guild.id, textId: text.id, announcementId: announcement.id};
	}

	async function startThread(
		token: string,
		channelId: string,
		body: Record<string, unknown> = {name: 'topic', type: ChannelTypes.ANNOUNCEMENT_THREAD},
	): Promise<ThreadChannelResponse> {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body(body)
			.expect(201)
			.execute();
	}

	async function storedType(threadId: string): Promise<{state?: number; channel?: number}> {
		const id = createChannelID(BigInt(threadId));
		const [state, channel] = await Promise.all([
			repository.threads.getState(id),
			repository.channelData.findUnique(id),
		]);
		return {state: state?.type, channel: channel?.type};
	}

	describe('creation and lists', () => {
		it('creates an announcement thread for either public type and refuses private threads', async () => {
			const s = await setup();
			for (const type of [ChannelTypes.ANNOUNCEMENT_THREAD, ChannelTypes.PUBLIC_THREAD]) {
				const thread = await startThread(s.owner.token, s.announcementId, {name: `t${type}`, type});
				expect(thread.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
			}
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.announcementId}/threads`)
				.body({name: 'secret', type: ChannelTypes.PRIVATE_THREAD})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.textId}/threads`)
				.body({name: 'wrong', type: ChannelTypes.ANNOUNCEMENT_THREAD})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			const text = await startThread(s.owner.token, s.textId, {name: 'plain', type: ChannelTypes.PUBLIC_THREAD});
			expect(text.type).toBe(ChannelTypes.PUBLIC_THREAD);
		});

		it('derives permissions from the announcement channel', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.announcementId);
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'allowed'})
				.expect(200)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.announcementId}/permissions/${s.member.userId}`)
				.body({type: 1, allow: '0', deny: ThreadPermissionFlags.CREATE_PUBLIC_THREADS.toString()})
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.announcementId}/threads`)
				.body({name: 'denied', type: ChannelTypes.ANNOUNCEMENT_THREAD})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.announcementId}/permissions/${s.member.userId}`)
				.body({type: 1, allow: '0', deny: ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS.toString()})
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'blocked'})
				.expect(403)
				.execute();
		});

		it('returns announcement threads from the public archived list and refuses private lists', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.announcementId);
			await archive(thread.id);
			const page = await threadsRequest<ArchivedThreadsResponse>(harness, s.owner.token)
				.get(`/channels/${s.announcementId}/threads/archived/public`)
				.execute();
			expect(page.threads.map((entry) => [entry.id, entry.type])).toEqual([
				[thread.id, ChannelTypes.ANNOUNCEMENT_THREAD],
			]);
			for (const path of [
				`/channels/${s.announcementId}/threads/archived/private`,
				`/channels/${s.announcementId}/users/@me/threads/archived/private`,
			]) {
				await threadsRequest(harness, s.owner.token)
					.get(path)
					.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
					.execute();
			}
		});
	});

	describe('conversion', () => {
		it('refuses converting a text channel with private threads to announcement', async () => {
			const s = await setup();
			const secret = await startThread(s.owner.token, s.textId, {name: 'secret', type: ChannelTypes.PRIVATE_THREAD});
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(400, APIErrorCodes.CHANNEL_HAS_THREADS)
				.execute();
			expect(await storedType(secret.id)).toEqual({
				state: ChannelTypes.PRIVATE_THREAD,
				channel: ChannelTypes.PRIVATE_THREAD,
			});
			expect((await repository.channelData.findUnique(createChannelID(BigInt(s.textId))))?.type).toBe(
				ChannelTypes.GUILD_TEXT,
			);
		});

		it('refuses converting a parent with retained private threads for incapable callers', async () => {
			const s = await setup();
			await startThread(s.owner.token, s.textId, {name: 'secret', type: ChannelTypes.PRIVATE_THREAD});
			await threadsRequest(harness, s.owner.token, {capable: false})
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(400, APIErrorCodes.CHANNEL_HAS_THREADS)
				.execute();
		});
	});

	describe('announcements', () => {
		let world: FanoutWorld;

		afterEach(() => {
			disableCrosspostWorker();
		});

		it('publishes a message that has a thread without leaking thread flags into follower copies', async () => {
			world = await setupFanoutWorld(harness);
			await followInto(harness, world, world.b.t1.id);
			const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, 'breaking');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, world.a.owner.token)
				.post(`/channels/${world.a.ann.id}/messages/${message.id}/threads`)
				.body({name: 'comments'})
				.expect(201)
				.execute();
			expect(thread.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
			const published = await threadsRequest<MessageResponse>(harness, world.a.owner.token)
				.post(`/channels/${world.a.ann.id}/messages/${message.id}/crosspost`)
				.expect(200)
				.execute();
			await world.worker.drain();
			expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
			expect(published.thread?.id).toBe(thread.id);
			const copies = await copiesOf(harness, world.b.owner.token, world.b.t1.id, message.id);
			expect(copies).toHaveLength(1);
			expect(copies[0]!.flags & ServerMessageFlags.HAS_THREAD).toBe(0);
			expect(copies[0]!.thread).toBeUndefined();
		});

		it('never publishes or delivers messages sent inside an announcement thread', async () => {
			world = await setupFanoutWorld(harness);
			await followInto(harness, world, world.b.t1.id);
			const thread = await startThread(world.a.owner.token, world.a.ann.id);
			const inner = await threadsRequest<MessageResponse>(harness, world.a.owner.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'thread chatter'})
				.expect(200)
				.execute();
			await threadsRequest(harness, world.a.owner.token)
				.post(`/channels/${thread.id}/messages/${inner.id}/crosspost`)
				.expect(400, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
				.execute();
			await world.worker.drain();
			expect(world.worker.byTask('crosspostMessage')).toEqual([]);
			expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, inner.id)).toEqual([]);
		});
	});

	describe('control arm', () => {
		it('masks thread flags in the publish response for clients without the capability', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.announcementId, 'headline');
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.announcementId}/messages/${message.id}/threads`)
				.body({name: 'Discussion'})
				.expect(201)
				.execute();
			const published = await threadsRequest<MessageResponse>(harness, s.owner.token, {capable: false})
				.post(`/channels/${s.announcementId}/messages/${message.id}/crosspost`)
				.expect(200)
				.execute();
			expect(published.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
			expect(published.flags & THREAD_MESSAGE_FLAG_MASK).toBe(0);
			expect('thread' in published).toBe(false);
		});
	});
});
