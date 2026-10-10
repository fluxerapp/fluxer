// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createRoleID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannelInvite} from '@app/api/channel/tests/ChannelTestUtils';
import {ALL_THREADS_ACTIVE, threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {
	clearChannelThreadsTaintCacheForTesting,
	pinChannelThreadsConfigForTesting,
} from '@app/api/experiment/ChannelThreadsGate';
import {addMemberRole, createGuild, createRole} from '@app/api/guild/tests/GuildTestUtils';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {
	getChannelRepository,
	getGuildRepository,
	getInstanceConfigRepository,
	getUserCacheService,
} from '@app/api/middleware/ServiceSingletons';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import seedThreadPermissions from '@app/api/worker/tasks/SeedThreadPermissions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DEFAULT_THREAD_PERMISSIONS, ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

function setConfig(patch: Partial<ChannelThreadsConfig> | null): void {
	pinChannelThreadsConfigForTesting(ChannelThreadsConfigSchema.parse(patch ?? {}));
	clearChannelThreadsTaintCacheForTesting();
}

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

describe('seedThreadPermissions', () => {
	let harness: ApiTestHarness;
	let dispatchGuild: ReturnType<typeof vi.fn>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setConfig(null);
		dispatchGuild = vi.fn(async () => {});
		setWorkerDependenciesForTest({
			channelRepository: getChannelRepository(),
			guildRepository: getGuildRepository(),
			userCacheService: getUserCacheService(),
			instanceConfigRepository: getInstanceConfigRepository(),
			gatewayService: {dispatchGuild} as unknown as IGatewayService,
		});
	});

	afterEach(() => {
		setConfig(null);
	});

	afterAll(async () => {
		clearWorkerDependencies();
		await harness?.shutdown();
	});

	it('clears raw overwrite bits, mirrors send permission into the thread bits, and stays idempotent', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Seed');
		const guildId = createGuildID(BigInt(guild.id));
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'announcements',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [{id: guild.id, type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()}],
			})
			.execute();
		const channelRepository = getChannelRepository();
		const stored = await channelRepository.channelData.findUnique(createChannelID(BigInt(channel.id)));
		await channelRepository.channelData.upsert({
			...stored!.toRow(),
			permission_overwrites: new Map([
				[
					createRoleID(BigInt(guild.id)),
					{type: 0, allow_: ThreadPermissionFlags.CREATE_PRIVATE_THREADS, deny_: Permissions.SEND_MESSAGES},
				],
			]),
		});

		await seedThreadPermissions({guildId: guild.id}, helpers());
		expect(dispatchGuild).not.toHaveBeenCalled();
		expect(
			await fetchOne<GuildThreadStateRow>(
				GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1}),
				{guild_id: guildId},
			),
		).toBeNull();

		setConfig({enabled: true, ever_enabled: true, enabled_guild_ids: [guild.id]});
		await seedThreadPermissions({guildId: guild.id}, helpers());
		const everyone = await getGuildRepository().getRole(createRoleID(BigInt(guild.id)), guildId);
		expect(everyone?.permissions).toBe(DEFAULT_PERMISSIONS | DEFAULT_THREAD_PERMISSIONS);
		const seeded = (await channelRepository.channelData.findUnique(
			createChannelID(BigInt(channel.id)),
		))!.permissionOverwrites.get(createRoleID(BigInt(guild.id)));
		expect(seeded?.allow).toBe(0n);
		expect(seeded?.deny).toBe(Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS);
		expect(dispatchGuild.mock.calls.map(([params]) => params.event)).toEqual([
			'GUILD_ROLE_UPDATE_BULK',
			'CHANNEL_UPDATE_BULK',
		]);
		const marker = await fetchOne<GuildThreadStateRow>(
			GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1}),
			{guild_id: guildId},
		);
		expect(marker?.perms_seeded_at).toBeInstanceOf(Date);

		await getGuildRepository().upsertRole({...everyone!.toRow(), permissions: DEFAULT_PERMISSIONS});
		await seedThreadPermissions({guildId: guild.id}, helpers());
		expect(dispatchGuild).toHaveBeenCalledTimes(2);
		expect((await getGuildRepository().getRole(createRoleID(BigInt(guild.id)), guildId))?.permissions).toBe(
			DEFAULT_PERMISSIONS,
		);
	});

	it('lets a role that is allowed to send in a read-only channel create threads and post in them', async () => {
		const owner = await createTestAccount(harness);
		const staff = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Staffed');
		const role = await createRole(harness, owner.token, guild.id, {name: 'Staff', permissions: '0'});
		const channel = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({
				name: 'announcements',
				type: ChannelTypes.GUILD_TEXT,
				permission_overwrites: [
					{id: guild.id, type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()},
					{id: role.id, type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'},
				],
			})
			.execute();
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, staff.token, invite.code);
		await acceptInvite(harness, member.token, invite.code);
		await addMemberRole(harness, owner.token, guild.id, staff.userId, role.id);

		setConfig({...ALL_THREADS_ACTIVE, ever_enabled: true, enabled_guild_ids: [guild.id]});
		await seedThreadPermissions({guildId: guild.id}, helpers());

		const seeded = (await getChannelRepository().channelData.findUnique(
			createChannelID(BigInt(channel.id)),
		))!.permissionOverwrites.get(createRoleID(BigInt(role.id)));
		expect(seeded?.allow).toBe(Permissions.SEND_MESSAGES | DEFAULT_THREAD_PERMISSIONS);
		const thread = await threadsRequest<ChannelResponse>(harness, staff.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'staff notes', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		await threadsRequest(harness, staff.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'hello'})
			.expect(200)
			.execute();
		await threadsRequest(harness, member.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'sneaky', type: ChannelTypes.PUBLIC_THREAD})
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
	});

	it('reloads a lagging config before deciding the guild is inactive, and retries while it stays behind', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Lagging');
		const guildId = createGuildID(BigInt(guild.id));
		const storedVersion = 5;
		await getInstanceConfigRepository().setConfig(
			'channel_threads_config',
			JSON.stringify(ChannelThreadsConfigSchema.parse({enabled: false, config_version: storedVersion})),
		);
		pinChannelThreadsConfigForTesting(null);

		await expect(
			seedThreadPermissions({guildId: guild.id, configVersion: storedVersion + 1}, helpers()),
		).rejects.toThrow();
		pinChannelThreadsConfigForTesting(null);

		await seedThreadPermissions({guildId: guild.id, configVersion: storedVersion}, helpers());
		const marker = await fetchOne<GuildThreadStateRow>(
			GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1}),
			{guild_id: guildId},
		);
		expect(marker?.perms_seeded_at).toBeInstanceOf(Date);
		expect((await getGuildRepository().getRole(createRoleID(BigInt(guild.id)), guildId))?.permissions).toBe(
			DEFAULT_PERMISSIONS | DEFAULT_THREAD_PERMISSIONS,
		);
	});
});
