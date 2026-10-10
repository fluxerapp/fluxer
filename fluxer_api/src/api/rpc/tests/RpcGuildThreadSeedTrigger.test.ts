// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {resetChannelThreadsConfig, setChannelThreadsConfig} from '@app/api/channel/tests/ThreadTestUtils';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildThreadStateRow} from '@app/api/database/types/ThreadTypes';
import {getCompiledChannelThreadsConfig, insertGuildThreadMarker} from '@app/api/experiment/ChannelThreadsGate';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getChannelRepository} from '@app/api/middleware/ServiceSingletons';
import {GuildThreadState} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const FETCH_MARKER = GuildThreadState.selectCql({where: GuildThreadState.where.eq('guild_id'), limit: 1});

describe('RpcService guild load thread permission seeding', () => {
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
		resetChannelThreadsConfig();
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
	const setEnabled = (enabled: boolean) => setChannelThreadsConfig({enabled, guild_basis_points: 10000});

	test('marks and seeds a bucketed guild on its first active load, and leaves control guilds alone', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bucketed');
		const guildId = createGuildID(BigInt(guild.id));
		const addJob = vi.spyOn(getWorkerService(), 'addJob');

		await loadGuild(guild.id);
		expect(await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: guildId})).toBeNull();
		expect(addJob).not.toHaveBeenCalled();

		await setEnabled(true);
		await loadGuild(guild.id);
		const marker = await fetchOne<GuildThreadStateRow>(FETCH_MARKER, {guild_id: guildId});
		expect(marker?.first_active_at).toBeInstanceOf(Date);
		expect(marker?.perms_seeded_at).toBeNull();
		expect(addJob).toHaveBeenCalledWith(
			'seedThreadPermissions',
			{guildId: guild.id, configVersion: getCompiledChannelThreadsConfig().config.config_version},
			{jobKey: `seed-thread-permissions-${guild.id}`},
		);

		await getChannelRepository().threads.markGuildPermsSeeded(guildId, new Date());
		addJob.mockClear();
		await loadGuild(guild.id);
		await loadGuild(guild.id);
		expect(addJob).not.toHaveBeenCalled();
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
