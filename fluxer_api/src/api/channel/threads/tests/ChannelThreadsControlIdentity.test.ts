// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createDmChannel, createFriendship, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {resetChannelThreadsConfig, setChannelThreadsConfig} from '@app/api/channel/tests/ThreadTestUtils';
import {setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {getCompiledChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

class RecordingExecutor extends InMemoryCassandraQueryExecutor {
	statements: Array<string> = [];

	override async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		this.statements.push(`${query.cql}|${Object.keys(query.params ?? {}).sort()}`);
		return super.executeQuery<T>(query);
	}

	override async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		for (const entry of queries) this.statements.push(`${entry.query}|${Object.keys(entry.params).sort()}`);
		return super.executeBatch(queries, atomic);
	}
}

describe('channel threads control identity', () => {
	let harness: ApiTestHarness;
	let executor: RecordingExecutor;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
		executor = new RecordingExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(null);
		await harness?.shutdown();
	});

	async function recorded<T>(action: () => Promise<T>): Promise<{result: T; statements: Array<string>}> {
		executor.statements = [];
		const result = await action();
		return {result, statements: [...executor.statements].sort()};
	}

	async function createPair() {
		const owner = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, owner, friend);
		return {owner, friend};
	}

	const loadChannels = (guildId: string) =>
		createBuilder<{channels: unknown}>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'guild_collection', guild_id: guildId, collection: 'channels'})
			.expect(HTTP_STATUS.OK)
			.execute();

	it('keeps guild create, DM create and RPC channels identical for a guild outside the experiment', async () => {
		const baseline = await createPair();
		const baselineGuild = await recorded(() => createGuild(harness, baseline.owner.token, 'control'));
		const baselineDm = await recorded(() => createDmChannel(harness, baseline.owner.token, baseline.friend.userId));
		const baselineChannels = await loadChannels(baselineGuild.result.id);

		await setChannelThreadsConfig({enabled: true, enabled_guild_ids: ['1']});
		expect(getCompiledChannelThreadsConfig().config.ever_enabled).toBe(true);

		const enrolledOff = await createPair();
		const guild = await recorded(() => createGuild(harness, enrolledOff.owner.token, 'control'));
		const dm = await recorded(() => createDmChannel(harness, enrolledOff.owner.token, enrolledOff.friend.userId));

		expect(baselineGuild.statements.length).toBeGreaterThan(0);
		expect(baselineDm.statements.length).toBeGreaterThan(0);
		expect(guild.statements).toEqual(baselineGuild.statements);
		expect(dm.statements).toEqual(baselineDm.statements);
		expect(await loadChannels(baselineGuild.result.id)).toEqual(baselineChannels);
	});
});
