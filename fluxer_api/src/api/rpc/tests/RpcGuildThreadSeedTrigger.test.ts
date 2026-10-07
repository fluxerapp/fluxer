// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {executeConditional, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {
	clearChannelThreadsTaintCacheForTesting,
	insertGuildThreadMarker,
	isTainted,
	syncChannelThreadsConfig,
} from '@app/api/experiment/ChannelThreadsGate';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getChannelRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	applyChannelThreadsConfigUpdate,
	ChannelThreadsConfigSchema,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const FETCH_MARKER = GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1});

function resetConfig(): void {
	syncChannelThreadsConfig(null, (raw) => ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}));
	clearChannelThreadsTaintCacheForTesting();
}

describe('RpcService guild load thread permission seeding', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
		resetConfig();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	const loadCollection = (guildId: string, collection: 'guild' | 'channels') =>
		createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'guild_collection', guild_id: guildId, collection})
			.expect(HTTP_STATUS.OK)
			.execute();
	const loadGuild = (guildId: string) => loadCollection(guildId, 'guild');
	const setEnabled = (enabled: boolean) =>
		getInstanceConfigRepository().updateChannelThreadsConfig((current) =>
			applyChannelThreadsConfigUpdate(current, {enabled, guild_basis_points: 10000}),
		);

	test('marks and seeds a bucketed guild on its first active load, and leaves control guilds alone', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bucketed');
		const guildId = createGuildID(BigInt(guild.id));
		const addJob = vi.spyOn(getWorkerService(), 'addJob');

		await loadGuild(guild.id);
		expect(await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: guildId})).toBeNull();
		expect(addJob).not.toHaveBeenCalled();

		const landed = await getInstanceConfigRepository().updateChannelThreadsConfig((current) =>
			applyChannelThreadsConfigUpdate(current, {enabled: true, guild_basis_points: 10000}),
		);
		await loadGuild(guild.id);
		const marker = await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: guildId});
		expect(marker?.first_active_at).toBeInstanceOf(Date);
		expect(marker?.perms_seeded_at).toBeNull();
		expect(addJob).toHaveBeenCalledWith(
			'seedThreadPermissions',
			{guildId: guild.id, configVersion: landed.config_version},
			{jobKey: `seed-thread-permissions-${guild.id}`},
		);

		await getChannelRepository().threads.markGuildPermsSeeded(guildId, new Date());
		addJob.mockClear();
		await loadGuild(guild.id);
		await loadGuild(guild.id);
		expect(addJob).not.toHaveBeenCalled();
	});

	test('channels loads skip the taint read for control guilds with no dangling references', async () => {
		await setEnabled(true);
		await setEnabled(false);
		clearChannelThreadsTaintCacheForTesting();
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Control');
		const guildId = createGuildID(BigInt(guild.id));
		await loadCollection(guild.id, 'channels');
		await executeConditional(
			GuildThreadState.insertIfNotExists({
				guild_id: guildId,
				first_active_at: new Date(),
				perms_seeded_at: null,
				search_backfilled_at: null,
			}),
		);
		expect(await isTainted(guildId, {fresh: true})).toBe(true);
	});

	test('channels loads keep references to dormant channels in retired guilds and repair real dangling ones', async () => {
		await setEnabled(true);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Retired');
		const guildId = createGuildID(BigInt(guild.id));
		await insertGuildThreadMarker(guildId, null);
		const channelRepository = getChannelRepository();
		const source = (await channelRepository.findUnique(createChannelID(BigInt(guild.system_channel_id!))))!;
		const forumId = createChannelID(BigInt(source.id) + 1n);
		await channelRepository.upsert({...source.toRow(), channel_id: forumId, type: ChannelTypes.GUILD_FORUM, name: 'f'});
		const guildRepository = new GuildRepository();
		const stored = (await guildRepository.findUnique(guildId))!;
		const missingId = createChannelID(BigInt(source.id) + 2n);
		await guildRepository.upsertPartial(
			guildId,
			{system_channel_id: forumId, rules_channel_id: missingId},
			stored.toRow(),
		);
		await setEnabled(false);
		await loadCollection(guild.id, 'channels');
		const repaired = (await guildRepository.findUnique(guildId))!;
		expect(repaired.systemChannelId).toBe(forumId);
		expect(repaired.rulesChannelId).toBeNull();
	});
});
