// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {MessageInteractionRepository} from '@app/api/channel/repositories/MessageInteractionRepository';
import {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {ChannelOperationsService} from '@app/api/channel/services/channel_data/ChannelOperationsService';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {BatchBuilder} from '@app/api/database/CassandraQueryExecution';
import {sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {ChannelTypes, MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	guildId: string;
	textId: string;
	announcementId: string;
}

const repository = new ChannelRepository();

describe('announcement thread races', () => {
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
		const guild = await createGuild(harness, owner.token, 'news races');
		const text = await createChannel(harness, owner.token, guild.id, 'general');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		return {owner, guildId: guild.id, textId: text.id, announcementId: announcement.id};
	}

	async function startThread(token: string, channelId: string): Promise<ThreadChannelResponse> {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
	}

	it('keeps a thread archived during a retype out of the active index', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const threadId = createChannelID(BigInt(thread.id));
		const execute = BatchBuilder.prototype.execute;
		let armed = true;
		vi.spyOn(BatchBuilder.prototype, 'execute').mockImplementation(async function (
			this: BatchBuilder,
			...args: Parameters<BatchBuilder['execute']>
		) {
			if (armed) {
				armed = false;
				await repository.threads.updateState(threadId, () => ({archived: true}));
			}
			return execute.apply(this, args);
		});
		await repository.threads.setThreadType(threadId, ChannelTypes.ANNOUNCEMENT_THREAD);
		vi.restoreAllMocks();
		expect((await repository.threads.getState(threadId))?.archived).toBe(true);
		expect(await repository.threads.countActiveThreads(createGuildID(BigInt(s.guildId)))).toBe(0);
	});

	it('leaves no rows behind for a thread deleted during a retype', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const threadId = createChannelID(BigInt(thread.id));
		const execute = BatchBuilder.prototype.execute;
		let armed = true;
		vi.spyOn(BatchBuilder.prototype, 'execute').mockImplementation(async function (
			this: BatchBuilder,
			...args: Parameters<BatchBuilder['execute']>
		) {
			if (armed) {
				armed = false;
				await repository.threads.purgeThread(threadId);
			}
			return execute.apply(this, args);
		});
		expect(await repository.threads.setThreadType(threadId, ChannelTypes.ANNOUNCEMENT_THREAD)).toBeNull();
		vi.restoreAllMocks();
		expect(await repository.findUnique(threadId)).toBeNull();
		expect(await repository.threads.getState(threadId)).toBeNull();
		expect(await repository.threads.listParentThreads(createChannelID(BigInt(s.textId)))).toEqual([]);
		expect(await repository.threads.countActiveThreads(createGuildID(BigInt(s.guildId)))).toBe(0);
	});

	it('does not revert a retyped thread when a rename read the old row', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const threadId = createChannelID(BigInt(thread.id));
		const updateState = ThreadRepository.prototype.updateState;
		vi.spyOn(ThreadRepository.prototype, 'updateState').mockImplementationOnce(async function (
			this: ThreadRepository,
			...args: Parameters<ThreadRepository['updateState']>
		) {
			const result = await updateState.apply(this, args);
			await repository.threads.setThreadType(threadId, ChannelTypes.ANNOUNCEMENT_THREAD);
			return result;
		});
		await threadsRequest(harness, s.owner.token)
			.patch(`/channels/${thread.id}`)
			.body({name: 'renamed'})
			.expect(200)
			.execute();
		vi.restoreAllMocks();
		const stored = await repository.findUnique(threadId);
		expect(stored?.name).toBe('renamed');
		expect(stored?.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
		expect((await repository.threads.getState(threadId))?.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
	});

	it('does not revert a retyped thread when a pin read the old row', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const threadId = createChannelID(BigInt(thread.id));
		const message = await threadsRequest<MessageResponse>(harness, s.owner.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'pin me'})
			.expect(200)
			.execute();
		const addChannelPin = MessageInteractionRepository.prototype.addChannelPin;
		vi.spyOn(MessageInteractionRepository.prototype, 'addChannelPin').mockImplementationOnce(async function (
			this: MessageInteractionRepository,
			...args: Parameters<MessageInteractionRepository['addChannelPin']>
		) {
			await addChannelPin.apply(this, args);
			await repository.threads.setThreadType(threadId, ChannelTypes.ANNOUNCEMENT_THREAD);
		});
		await threadsRequest(harness, s.owner.token).put(`/channels/${thread.id}/pins/${message.id}`).expect(204).execute();
		vi.restoreAllMocks();
		const stored = await repository.findUnique(threadId);
		expect(stored?.lastPinTimestamp).not.toBeNull();
		expect(stored?.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
		expect((await repository.threads.getState(threadId))?.type).toBe(ChannelTypes.ANNOUNCEMENT_THREAD);
	});

	it('does not revert a converted parent when an overwrite edit read the old row', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const operations = ChannelOperationsService.prototype as unknown as {checkOverwritePermission: () => Promise<void>};
		vi.spyOn(operations, 'checkOverwritePermission').mockImplementationOnce(async () => {
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.textId}`)
				.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
				.expect(200)
				.execute();
		});
		await threadsRequest(harness, s.owner.token)
			.put(`/channels/${s.textId}/permissions/${s.owner.userId}`)
			.body({type: 1, allow: '0', deny: '0'})
			.expect(204)
			.execute();
		vi.restoreAllMocks();
		const parent = await repository.findUnique(createChannelID(BigInt(s.textId)));
		expect(parent?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(parent?.permissionOverwrites?.size ?? 0).toBeGreaterThan(0);
		expect((await repository.threads.getState(createChannelID(BigInt(thread.id))))?.type).toBe(
			ChannelTypes.ANNOUNCEMENT_THREAD,
		);
	});

	it('restores retyped threads when the parent write fails', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.textId);
		const threadId = createChannelID(BigInt(thread.id));
		const upsert = ChannelDataRepository.prototype.upsert;
		vi.spyOn(ChannelDataRepository.prototype, 'upsert').mockImplementation(async function (
			this: ChannelDataRepository,
			...args: Parameters<ChannelDataRepository['upsert']>
		) {
			if (
				args[0].channel_id === createChannelID(BigInt(s.textId)) &&
				args[0].type === ChannelTypes.GUILD_ANNOUNCEMENT
			) {
				throw new Error('write timeout');
			}
			return upsert.apply(this, args);
		});
		await threadsRequest(harness, s.owner.token)
			.patch(`/channels/${s.textId}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(500)
			.execute();
		vi.restoreAllMocks();
		expect((await repository.findUnique(createChannelID(BigInt(s.textId))))?.type).toBe(ChannelTypes.GUILD_TEXT);
		expect((await repository.findUnique(threadId))?.type).toBe(ChannelTypes.PUBLIC_THREAD);
		expect((await repository.threads.getState(threadId))?.type).toBe(ChannelTypes.PUBLIC_THREAD);
	});

	it('keeps both flags when a publish races a thread start on the same message', async () => {
		const s = await setup();
		const message = await sendMessage(harness, s.owner.token, s.announcementId, 'headline');
		const channelId = createChannelID(BigInt(s.announcementId));
		const messageId = createMessageID(BigInt(message.id));
		const getMessage = MessageRepository.prototype.getMessage;
		let publish: Promise<unknown> | null = null;
		vi.spyOn(MessageRepository.prototype, 'getMessage').mockImplementation(async function (
			this: MessageRepository,
			...args: Parameters<MessageRepository['getMessage']>
		) {
			if (
				publish === null &&
				args[1] === messageId &&
				(await repository.threads.getState(createChannelID(BigInt(message.id)))) !== null
			) {
				publish = threadsRequest(harness, s.owner.token)
					.post(`/channels/${s.announcementId}/messages/${message.id}/crosspost`)
					.expect(200)
					.execute();
				await Promise.race([publish, new Promise((resolve) => setTimeout(resolve, 200))]);
			}
			return getMessage.apply(this, args);
		});
		await threadsRequest(harness, s.owner.token)
			.post(`/channels/${s.announcementId}/messages/${message.id}/threads`)
			.body({name: 'Discussion'})
			.expect(201)
			.execute();
		expect(publish).not.toBeNull();
		await publish;
		vi.restoreAllMocks();
		const stored = await repository.messages.getMessage(channelId, messageId);
		expect(stored!.flags & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
		expect(stored!.flags & MessageFlags.CROSSPOSTED).toBe(MessageFlags.CROSSPOSTED);
	});
});
