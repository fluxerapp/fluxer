// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {deleteOneOrMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {clearChannelThreadsTaintCacheForTesting, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getChannelRepository} from '@app/api/middleware/ServiceSingletons';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {HarvestedMessagesSchema, harvestUserArchive, readStoredArchive} from '@app/api/test/UserArchiveTestUtils';
import deleteUserMessagesInGuildByTime from '@app/api/worker/tasks/DeleteUserMessagesInGuildByTime';
import harvestGuildData from '@app/api/worker/tasks/HarvestGuildData';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies, type WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	channelId: string;
	threadId: string;
	messageIds: Array<string>;
}

describe('thread maintenance sweeps', () => {
	let harness: ApiTestHarness;
	let deps: WorkerDependencies;
	let worker: SyncTaskWorkerService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		clearChannelThreadsTaintCacheForTesting();
		worker = new SyncTaskWorkerService({deleteUserMessagesInGuildByTime, harvestGuildData}, {deferred: true});
		setInjectedWorkerService(worker);
		deps = await initializeWorkerDependencies(getSnowflakeService());
		setWorkerDependencies(deps);
	});

	afterAll(async () => {
		clearWorkerDependencies();
		clearChannelThreadsTaintCacheForTesting();
		setInjectedWorkerService(undefined);
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'sweeps');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'sweep', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		const messageIds: Array<string> = [];
		for (const content of ['first', 'second']) {
			const message = await threadsRequest<MessageResponse>(harness, member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content})
				.execute();
			messageIds.push(message.id);
		}
		return {owner, member, guildId: guild.id, channelId: channel.id, threadId: thread.id, messageIds};
	}

	async function remaining(threadId: string): Promise<number> {
		return (await getChannelRepository().messages.listMessages(createChannelID(BigInt(threadId)))).length;
	}

	async function messageCount(threadId: string): Promise<number | undefined> {
		return (await getChannelRepository().threads.getStats(createChannelID(BigInt(threadId))))?.messageCount;
	}

	async function ban(s: Setup): Promise<void> {
		await createBuilder(harness, s.owner.token)
			.put(`/guilds/${s.guildId}/bans/${s.member.userId}`)
			.body({delete_message_seconds: 3600})
			.expect(204)
			.execute();
		expect((await worker.drain()).failed).toBe(0);
	}

	it('removes thread messages inside the ban delete window and decrements the count', async () => {
		const s = await setup();
		expect(await messageCount(s.threadId)).toBe(2);
		await ban(s);
		expect(await remaining(s.threadId)).toBe(0);
		expect(await messageCount(s.threadId)).toBe(0);
	});

	it('sweeps thread messages of a tainted guild', async () => {
		const s = await setup();

		await ban(s);
		expect(await remaining(s.threadId)).toBe(0);
	});

	it('sweeps forum posts in the ban delete window while a clean taint answer is remembered', async () => {
		const s = await setup();
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
			.post(`/guilds/${s.guildId}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
			.execute();
		const post = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
			.post(`/channels/${forum.id}/threads`)
			.body({name: 'post', message: {content: 'hello'}})
			.expect(201)
			.execute();
		expect(await remaining(post.id)).toBe(1);
		const guildId = createGuildID(BigInt(s.guildId));
		const marker = await getChannelRepository().threads.getGuildMarker(guildId);
		await deleteOneOrMany(GuildThreadState.deleteByPk({guild_id: guildId}));
		clearChannelThreadsTaintCacheForTesting();
		expect(await isTainted(guildId)).toBe(false);
		assert(marker);
		await upsertOne(GuildThreadState.upsertAll(marker));
		await ban(s);
		expect(await remaining(post.id)).toBe(0);
		expect(await remaining(s.threadId)).toBe(0);
	});

	it('removes thread messages in a guild scoped delete of my messages', async () => {
		const s = await setup();

		await deps.channelService.userMessageDeletion.deleteUserMessagesInScope(createUserID(BigInt(s.member.userId)), {
			guildId: createGuildID(BigInt(s.guildId)),
		});
		expect(await remaining(s.threadId)).toBe(0);
		expect(await messageCount(s.threadId)).toBe(0);
	});

	it('includes thread messages in user and guild harvests', async () => {
		const s = await setup();
		const userArchive = await harvestUserArchive(harness, deps, createUserID(BigInt(s.member.userId)));
		expect(
			HarvestedMessagesSchema.parse(userArchive.readJson(`channels/${s.threadId}/messages.json`))
				.map((message) => message.id)
				.sort(),
		).toEqual([...s.messageIds].sort());
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		const requested = await createBuilder<{archive_id: string}>(harness, admin.token)
			.post(`/admin/guilds/${s.guildId}/archives`)
			.body({})
			.execute();
		const drained = await worker.drain();
		expect(drained.failed).toBe(0);
		const completed = await deps.adminArchiveRepository.findBySubjectAndArchiveId(
			'guild',
			BigInt(s.guildId),
			BigInt(requested.archive_id),
		);
		assert(completed?.storageKey);
		const guildArchive = await readStoredArchive(harness, completed.storageKey);
		expect(
			HarvestedMessagesSchema.parse(guildArchive.readJson(`channels/${s.threadId}/messages.json`))
				.map((message) => message.id)
				.sort(),
		).toEqual([...s.messageIds].sort());
	});
});
