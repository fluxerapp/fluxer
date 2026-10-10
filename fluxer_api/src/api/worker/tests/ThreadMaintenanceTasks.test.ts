// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {threadArchiveQueueKey} from '@app/api/infrastructure/KVThreadAutoArchiveQueueService';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {
	getChannelRepository,
	getGuildRepository,
	getInstanceConfigRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	archiveInactiveThreads,
	deleteChannelThreads,
	removeThreadMembershipsForGuildMember,
} from '@app/api/worker/tasks/ThreadMaintenanceTasks';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFlags, DEFAULT_THREAD_AUTO_ARCHIVE_DURATION} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
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
	};
}

const PAST_ARCHIVE_WINDOW_MS = (DEFAULT_THREAD_AUTO_ARCHIVE_DURATION + 1) * 60_000;

describe('thread maintenance tasks', () => {
	let harness: ApiTestHarness;
	let dispatchGuild: ReturnType<typeof vi.fn>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
		dispatchGuild = vi.fn(async () => {});
		setWorkerDependenciesForTest({
			kvClient: getKVClient(),
			channelRepository: getChannelRepository(),
			guildRepository: getGuildRepository(),
			instanceConfigRepository: getInstanceConfigRepository(),
			gatewayService: {dispatchGuild, dispatchGuildMany: vi.fn(async () => {})} as unknown as IGatewayService,
			channelService: {
				attachments: {purgeChannelAttachments: vi.fn(async () => {})},
			} as unknown as WorkerDependencies['channelService'],
		});
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		clearWorkerDependencies();
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<{owner: TestAccount; member: TestAccount; guildId: string; channelId: string}> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'sweep');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, member.token);
		return {owner, member, guildId: guild.id, channelId: channel.id};
	}

	async function startThread(token: string, channelId: string): Promise<ThreadChannelResponse> {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'sweep me', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
	}

	function state(threadId: string) {
		return getChannelRepository().threads.getState(createChannelID(BigInt(threadId)));
	}

	it('archives inactive threads, skips pinned ones and freezes inactive guilds', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.channelId);
		const fresh = await startThread(s.owner.token, s.channelId);
		await getChannelRepository().threads.updateState(createChannelID(BigInt(fresh.id)), () => ({
			flags: ChannelFlags.PINNED,
		}));
		const now = Date.now();
		vi.spyOn(Date, 'now').mockReturnValue(now + PAST_ARCHIVE_WINDOW_MS);
		await setChannelThreadsConfig({...ALL_THREADS_ACTIVE, guild_basis_points: 0});
		await archiveInactiveThreads({}, helpers());
		expect((await state(thread.id))?.archived).toBe(false);
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		await archiveInactiveThreads({}, helpers());
		expect((await state(thread.id))?.archived).toBe(true);
		expect((await state(fresh.id))?.archived).toBe(false);
		expect(dispatchGuild.mock.calls.some(([params]) => params.event === 'THREAD_UPDATE')).toBe(true);
		const queued = await getKVClient().zrangebyscore(
			threadArchiveQueueKey(createGuildID(BigInt(s.guildId))),
			'-inf',
			'+inf',
		);
		expect(queued).not.toContain(thread.id);
	});

	it('deletes the threads of a deleted parent in tainted guilds', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.channelId);
		await deleteChannelThreads({guildId: s.guildId, parentId: s.channelId}, helpers());
		expect(await state(thread.id)).toBeNull();
		expect(await getChannelRepository().channelData.findUnique(createChannelID(BigInt(thread.id)))).toBeNull();
	});

	it('purges thread memberships of a removed guild member and keeps those of a rejoined one', async () => {
		const s = await setup();
		const thread = await startThread(s.owner.token, s.channelId);
		await threadsRequest(harness, s.member.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'joined'})
			.execute();
		const threadId = createChannelID(BigInt(thread.id));
		const userId = createUserID(BigInt(s.member.userId));
		expect(await getChannelRepository().threads.getMember(threadId, userId)).not.toBeNull();
		await removeThreadMembershipsForGuildMember({guildId: s.guildId, userId: s.member.userId}, helpers());
		expect(await getChannelRepository().threads.getMember(threadId, userId)).not.toBeNull();
		await getGuildRepository().deleteMember(createGuildID(BigInt(s.guildId)), userId);
		await removeThreadMembershipsForGuildMember({guildId: s.guildId, userId: s.member.userId}, helpers());
		expect(await getChannelRepository().threads.getMember(threadId, userId)).toBeNull();
	});
});
