// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {disableCrosspostWorker} from '@app/api/channel/tests/AnnouncementTestUtils';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	type FanoutWorld,
	followInto,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
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
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags, THREAD_MESSAGE_FLAG_MASK} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	ArchivedThreadsResponse,
	ThreadChannelResponse,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

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

	describe('creation', () => {
		it('starts an announcement thread from a message in an announcement channel', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.announcementId, 'headline');
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${s.announcementId}/messages/${message.id}/threads`)
				.body({name: 'Discussion'})
				.expect(201)
				.execute();
			expect(thread).toMatchObject({
				id: message.id,
				type: ChannelTypes.ANNOUNCEMENT_THREAD,
				parent_id: s.announcementId,
			});
			expect(thread.thread_metadata?.invitable).toBeUndefined();
			const events = dispatch.mock.calls.map(([params]) => params);
			expect(events.find((params) => params.event === 'THREAD_CREATE')?.data).toMatchObject({
				type: ChannelTypes.ANNOUNCEMENT_THREAD,
			});
			const starter = events.find(
				(params) => params.event === 'MESSAGE_CREATE' && (params.data as MessageResponse).channel_id === message.id,
			)?.data as MessageResponse | undefined;
			expect(starter?.referenced_message?.id).toBe(message.id);
			const stored = await repository.messages.getMessage(
				createChannelID(BigInt(s.announcementId)),
				createMessageID(BigInt(message.id)),
			);
			expect(stored!.flags & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
			expect(await storedType(thread.id)).toEqual({
				state: ChannelTypes.ANNOUNCEMENT_THREAD,
				channel: ChannelTypes.ANNOUNCEMENT_THREAD,
			});
		});

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

		it('posts a thread created message in the announcement channel', async () => {
			const s = await setup();
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const thread = await startThread(s.owner.token, s.announcementId);
			const created = dispatch.mock.calls.map(([params]) => params).find((params) => params.event === 'MESSAGE_CREATE')
				?.data as MessageResponse | undefined;
			expect(created).toMatchObject({channel_id: s.announcementId, message_reference: {channel_id: thread.id}});
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
	});

	describe('lists and members', () => {
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

		it('lets members join, chat in, rename and archive an announcement thread', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.announcementId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			const sent = await threadsRequest<MessageResponse>(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'reply'})
				.expect(200)
				.execute();
			expect(sent.channel_id).toBe(thread.id);
			const renamed = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({name: 'renamed', archived: true, invitable: false})
				.expect(200)
				.execute();
			expect(renamed).toMatchObject({type: ChannelTypes.ANNOUNCEMENT_THREAD, name: 'renamed'});
			expect(renamed.thread_metadata).toMatchObject({archived: true});
			expect(renamed.thread_metadata?.invitable).toBeUndefined();
		});

		it('fetches an announcement thread by id', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.announcementId);
			const channel = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.get(`/channels/${thread.id}`)
				.execute();
			expect(channel.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
		});
	});

	describe('conversion', () => {
		it('turns public threads into announcement threads and back', async () => {
			const s = await setup();
			const active = await startThread(s.owner.token, s.textId, {name: 'live', type: ChannelTypes.PUBLIC_THREAD});
			const archived = await startThread(s.owner.token, s.textId, {name: 'old', type: ChannelTypes.PUBLIC_THREAD});
			await archive(archived.id);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuildMany');
			const single = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const converted = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(200)
				.execute();
			expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
			for (const thread of [active, archived]) {
				expect(await storedType(thread.id)).toEqual({
					state: ChannelTypes.ANNOUNCEMENT_THREAD,
					channel: ChannelTypes.ANNOUNCEMENT_THREAD,
				});
			}
			const updates = [
				...single.mock.calls.map(([params]) => params),
				...dispatch.mock.calls.flatMap(([params]) => params.events),
			].filter((params) => params.event === 'THREAD_UPDATE');
			expect(updates.map((params) => (params.data as ThreadChannelResponse).id)).toEqual([active.id]);
			expect(updates[0]!.data).toMatchObject({type: ChannelTypes.ANNOUNCEMENT_THREAD});
			const page = await threadsRequest<ArchivedThreadsResponse>(harness, s.owner.token)
				.get(`/channels/${s.textId}/threads/archived/public`)
				.execute();
			expect(page.threads.map((entry) => entry.type)).toEqual([ChannelTypes.ANNOUNCEMENT_THREAD]);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_TEXT})
				.expect(200)
				.execute();
			for (const thread of [active, archived]) {
				expect(await storedType(thread.id)).toEqual({
					state: ChannelTypes.PUBLIC_THREAD,
					channel: ChannelTypes.PUBLIC_THREAD,
				});
			}
		});

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

		it('rolls back a failed retype and leaves the parent unconverted so a retry converges', async () => {
			const s = await setup();
			const threads = [];
			for (const name of ['a', 'b', 'c']) {
				threads.push(await startThread(s.owner.token, s.textId, {name, type: ChannelTypes.PUBLIC_THREAD}));
			}
			await archive(threads[2]!.id);
			vi.spyOn(ThreadRepository.prototype, 'setThreadType').mockRejectedValueOnce(new ServiceUnavailableError());
			const update = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(503)
				.execute();
			expect((await repository.channelData.findUnique(createChannelID(BigInt(s.textId))))?.type).toBe(
				ChannelTypes.GUILD_TEXT,
			);
			expect(update.mock.calls.filter(([params]) => params.event === 'CHANNEL_UPDATE')).toEqual([]);
			vi.restoreAllMocks();
			for (const thread of threads) {
				expect(await storedType(thread.id)).toEqual({
					state: ChannelTypes.PUBLIC_THREAD,
					channel: ChannelTypes.PUBLIC_THREAD,
				});
			}
			const converted = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(200)
				.execute();
			expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
			for (const thread of threads) {
				expect(await storedType(thread.id)).toEqual({
					state: ChannelTypes.ANNOUNCEMENT_THREAD,
					channel: ChannelTypes.ANNOUNCEMENT_THREAD,
				});
			}
		});

		it('refuses converting a parent with retained private threads for control callers and after the experiment is off', async () => {
			const s = await setup();
			const secret = await startThread(s.owner.token, s.textId, {name: 'secret', type: ChannelTypes.PRIVATE_THREAD});
			await threadsRequest(harness, s.owner.token, {capable: false})
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(400, APIErrorCodes.CHANNEL_HAS_THREADS)
				.execute();
			await setChannelThreadsConfig({enabled: false});
			await createBuilder(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(400, APIErrorCodes.CHANNEL_HAS_THREADS)
				.execute();
			expect((await repository.channelData.findUnique(createChannelID(BigInt(s.textId))))?.type).toBe(
				ChannelTypes.GUILD_TEXT,
			);
			expect(await storedType(secret.id)).toEqual({
				state: ChannelTypes.PRIVATE_THREAD,
				channel: ChannelTypes.PRIVATE_THREAD,
			});
		});

		it('keeps converting announcement channels with retained threads after the experiment is off', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.announcementId);
			await setChannelThreadsConfig({enabled: false});
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await createBuilder(harness, s.owner.token)
				.patch(`/channels/${s.announcementId}`)
				.body({type: ChannelTypes.GUILD_TEXT})
				.expect(200)
				.execute();
			expect(await storedType(thread.id)).toEqual({
				state: ChannelTypes.PUBLIC_THREAD,
				channel: ChannelTypes.PUBLIC_THREAD,
			});
			expect(dispatch.mock.calls.filter(([params]) => params.event.startsWith('THREAD_'))).toEqual([]);
		});
	});

	describe('announcements', () => {
		let world: FanoutWorld;

		afterEach(() => {
			disableCrosspostWorker();
		});

		it('publishes a message that has a thread without leaking thread flags into follower copies', async () => {
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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
		it('keeps announcement channels unchanged for clients without the capability', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.announcementId, 'headline');
			await threadsRequest(harness, s.owner.token, {capable: false})
				.post(`/channels/${s.announcementId}/messages/${message.id}/threads`)
				.body({name: 'nope'})
				.expect(404)
				.execute();
			const updated = await threadsRequest<Record<string, unknown>>(harness, s.owner.token, {capable: false})
				.patch(`/channels/${s.announcementId}`)
				.body({topic: 'news', default_auto_archive_duration: 60})
				.expect(200)
				.execute();
			expect(updated.topic).toBe('news');
			expect('default_auto_archive_duration' in updated).toBe(false);
			const config = await repository.threads.getParentConfig(
				createGuildID(BigInt(s.guildId)),
				createChannelID(BigInt(s.announcementId)),
			);
			expect(config?.defaultAutoArchiveDuration ?? null).toBeNull();
		});

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

		it('stores thread defaults on announcement channels for viewers', async () => {
			const s = await setup();
			const updated = await threadsRequest<Record<string, unknown>>(harness, s.owner.token)
				.patch(`/channels/${s.announcementId}`)
				.body({default_auto_archive_duration: 60, default_thread_rate_limit_per_user: 5})
				.expect(200)
				.execute();
			expect(updated).toMatchObject({default_auto_archive_duration: 60, default_thread_rate_limit_per_user: 5});
			const thread = await startThread(s.owner.token, s.announcementId);
			expect(thread.thread_metadata?.auto_archive_duration).toBe(4320);
			expect(thread.rate_limit_per_user).toBe(5);
		});

		it('converts announcement channels in a guild that never had threads exactly as before', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'plain');
			const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const listParentThreads = vi.spyOn(ThreadRepository.prototype, 'listParentThreads');
			const converted = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${announcement.id}`)
				.body({type: ChannelTypes.GUILD_TEXT})
				.expect(200)
				.execute();
			expect(converted.type).toBe(ChannelTypes.GUILD_TEXT);
			expect(listParentThreads).not.toHaveBeenCalled();
			expect(dispatch.mock.calls.filter(([params]) => params.event.startsWith('THREAD_'))).toEqual([]);
			const back = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${announcement.id}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(200)
				.execute();
			expect(back.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
			expect(listParentThreads).not.toHaveBeenCalled();
		});
	});
});
