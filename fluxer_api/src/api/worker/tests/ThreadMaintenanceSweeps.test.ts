// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {deleteOneOrMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {clearChannelThreadsTaintCacheForTesting, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getChannelRepository} from '@app/api/middleware/ServiceSingletons';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import deleteUserMessagesInGuildByTime from '@app/api/worker/tasks/DeleteUserMessagesInGuildByTime';
import harvestGuildData from '@app/api/worker/tasks/HarvestGuildData';
import {harvestMessages} from '@app/api/worker/tasks/HarvestUserData';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies, type WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

function helpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
		attempt: {isLastAttempt: true},
	};
}

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
	let jobs: Array<{task: string; payload: Record<string, unknown>}>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
		deps = await initializeWorkerDependencies(getSnowflakeService());
		setWorkerDependencies(deps);
		jobs = [];
		const addJob = async (task: string, payload: Record<string, unknown>) => {
			jobs.push({task, payload});
			if (task === 'deleteUserMessagesInGuildByTime') await deleteUserMessagesInGuildByTime(payload, helpers());
			return 0n;
		};
		vi.spyOn(NoopWorkerService.prototype, 'addJob').mockImplementation(addJob as () => Promise<bigint>);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		clearWorkerDependencies();
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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
	}

	it('removes thread messages inside the ban delete window and decrements the count', async () => {
		const s = await setup();
		expect(await messageCount(s.threadId)).toBe(2);
		await ban(s);
		expect(await remaining(s.threadId)).toBe(0);
		expect(await messageCount(s.threadId)).toBe(0);
	});

	it('keeps sweeping thread messages of a tainted guild after the experiment is off', async () => {
		const s = await setup();
		await setChannelThreadsConfig({enabled: false});
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
		await upsertOne(GuildThreadState.upsertAll(marker!));
		await ban(s);
		expect(await remaining(post.id)).toBe(0);
		expect(await remaining(s.threadId)).toBe(0);
	});

	it('removes thread messages in a guild scoped delete of my messages', async () => {
		const s = await setup();
		await setChannelThreadsConfig({enabled: false});
		await deps.channelService.userMessageDeletion.deleteUserMessagesInScope(createUserID(BigInt(s.member.userId)), {
			guildId: createGuildID(BigInt(s.guildId)),
		});
		expect(await remaining(s.threadId)).toBe(0);
		expect(await messageCount(s.threadId)).toBe(0);
	});

	it('includes thread messages in user and guild harvests', async () => {
		const s = await setup();
		const harvested = await harvestMessages(
			getChannelRepository(),
			createUserID(BigInt(s.member.userId)),
			Date.now(),
			null,
		);
		expect(
			harvested.channelMessagesMap
				.get(s.threadId)
				?.map((message) => message.id)
				.sort(),
		).toEqual([...s.messageIds].sort());
		const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
		await createBuilder(harness, admin.token).post(`/admin/guilds/${s.guildId}/archives`).body({}).execute();
		const payload = jobs.find((job) => job.task === 'harvestGuildData')?.payload;
		expect(payload).toBeDefined();
		vi.spyOn(deps.storageService, 'uploadObjectFromFile').mockResolvedValue(undefined as never);
		const writeFile = vi.spyOn(fs.promises, 'writeFile');
		await harvestGuildData(payload!, helpers());
		const threadFile = writeFile.mock.calls.find(([file]) =>
			String(file).endsWith(`channels/${s.threadId}/messages.json`),
		);
		expect(threadFile).toBeDefined();
		const written = JSON.parse(String(threadFile![1])) as Array<{id: string}>;
		expect(written.map((message) => message.id).sort()).toEqual([...s.messageIds].sort());
	});
});
