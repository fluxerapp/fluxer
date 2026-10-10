// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {resolveThreadGateRpcData} from '@app/api/channel/services/thread/ThreadRpcData';
import {setCassandraQueryExecutorForTesting, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, PreparedQuery} from '@app/api/database/CassandraTypes';
import {
	clearChannelThreadsTaintCacheForTesting,
	guildActive,
	isTainted,
	noteGuildThreadMarker,
	SYSTEM_THREAD_VIEWER,
	syncChannelThreadsConfig,
	userActive,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {GuildThreadState} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
	everyoneChannelThreadsConfig,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

class CountingExecutor extends InMemoryCassandraQueryExecutor {
	reads = 0;
	markerReads = 0;

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (query.cql.trimStart().toUpperCase().startsWith('SELECT')) this.reads++;
		if (query.cql.includes('guild_thread_state')) this.markerReads++;
		return super.executeQuery<T>(query);
	}
}

const GUILD = createGuildID(100n);
const OTHER_GUILD = createGuildID(200n);
const USER = createUserID(10n);

function load(config: Partial<ChannelThreadsConfig>): void {
	const raw = JSON.stringify(ChannelThreadsConfigSchema.parse(config));
	syncChannelThreadsConfig(raw, (value) => ChannelThreadsConfigSchema.parse(JSON.parse(value ?? '{}')));
}

describe('ChannelThreadsGate', () => {
	let executor: CountingExecutor;

	beforeEach(() => {
		executor = new CountingExecutor();
		setCassandraQueryExecutorForTesting(executor);
		clearChannelThreadsTaintCacheForTesting();
	});

	afterEach(() => {
		load({});
		setCassandraQueryExecutorForTesting(null);
	});

	it('hands a missing config to the parser', () => {
		load({enabled: false});
		const compiled = syncChannelThreadsConfig(null, (raw) => everyoneChannelThreadsConfig(raw === null ? 3 : 0));
		expect(compiled.config.config_version).toBe(3);
		expect(guildActive(GUILD)).toBe(true);
		expect(userActive(USER)).toBe(true);
		expect(viewerActive(SYSTEM_THREAD_VIEWER, GUILD)).toBe(true);
	});

	it('parses a snapshot once and reuses the compiled value while the string is unchanged', () => {
		const raw = JSON.stringify(ChannelThreadsConfigSchema.parse({enabled: true, enabled_guild_ids: ['100']}));
		let parses = 0;
		const parse = (value: string | null) => {
			parses++;
			return ChannelThreadsConfigSchema.parse(JSON.parse(value ?? '{}'));
		};
		const first = syncChannelThreadsConfig(raw, parse);
		expect(syncChannelThreadsConfig(raw, parse)).toBe(first);
		expect(parses).toBe(1);
		expect(guildActive(GUILD)).toBe(true);
	});

	it('reads the marker once per guild and remembers the answer', async () => {
		load({enabled: false, ever_enabled: true});
		await upsertOne(
			GuildThreadState.upsertAll({
				guild_id: GUILD,
				first_active_at: new Date(),
				perms_seeded_at: null,
				search_backfilled_at: null,
			}),
		);
		expect(await isTainted(GUILD)).toBe(true);
		expect(await isTainted(GUILD)).toBe(true);
		expect(await isTainted(OTHER_GUILD)).toBe(false);
		expect(executor.reads).toBe(2);
		noteGuildThreadMarker(OTHER_GUILD);
		expect(await isTainted(OTHER_GUILD)).toBe(true);
		expect(executor.reads).toBe(2);
	});

	it('remembers a clean answer across gate-off guild collection loads', async () => {
		load({enabled: false, ever_enabled: true});
		expect(await resolveThreadGateRpcData(GUILD)).toBeNull();
		expect(await resolveThreadGateRpcData(GUILD)).toBeNull();
		expect(executor.markerReads).toBe(1);
	});

	it('rereads a remembered clean answer for fresh checks and trusts a remembered taint', async () => {
		load({enabled: false, ever_enabled: true});
		expect(await isTainted(GUILD)).toBe(false);
		await upsertOne(
			GuildThreadState.upsertAll({
				guild_id: GUILD,
				first_active_at: new Date(),
				perms_seeded_at: null,
				search_backfilled_at: null,
			}),
		);
		expect(await isTainted(GUILD)).toBe(false);
		expect(executor.reads).toBe(1);
		expect(await isTainted(GUILD, {fresh: true})).toBe(true);
		expect(executor.reads).toBe(2);
		expect(await isTainted(GUILD, {fresh: true})).toBe(true);
		expect(await isTainted(GUILD)).toBe(true);
		expect(executor.reads).toBe(2);
	});

	it('enqueues thread membership cleanup on member removal despite a remembered clean answer', async () => {
		load({enabled: false, ever_enabled: true});
		const worker = new NoopWorkerService();
		const addJob = vi.spyOn(worker, 'addJob');
		setInjectedWorkerService(worker);
		try {
			expect(await isTainted(GUILD)).toBe(false);
			await upsertOne(
				GuildThreadState.upsertAll({
					guild_id: GUILD,
					first_active_at: new Date(),
					perms_seeded_at: null,
					search_backfilled_at: null,
				}),
			);
			await new GuildRepository().deleteMember(GUILD, USER);
			expect(addJob).toHaveBeenCalledWith(
				'removeThreadMembershipsForGuildMember',
				{guildId: GUILD.toString(), userId: USER.toString()},
				{jobKey: `remove-thread-memberships-${GUILD}-${USER}`},
			);
		} finally {
			setInjectedWorkerService(undefined);
		}
	});
});
