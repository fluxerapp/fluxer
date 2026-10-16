// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, createGuildID, createUserID, type GuildID} from '@app/api/BrandedTypes';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {accessibleThreadIds} from '@app/api/search/ThreadSearchScope';
import {ThreadsByParent} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@fluxer/constants/src/ThreadConstants', async (importOriginal) => ({
	...(await importOriginal<typeof import('@fluxer/constants/src/ThreadConstants')>()),
	THREAD_SCOPE_MAX: 300,
}));

const USER_ID = createUserID(1n);
const GUILD_A = createGuildID(10n);
const GUILD_B = createGuildID(20n);
const PARENT_A = createChannelID(100n);
const PARENT_B = createChannelID(200n);

class CountingExecutor implements CassandraQueryExecutorForTesting {
	readonly inner = new InMemoryCassandraQueryExecutor();
	parentReads = 0;

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (query.kvMeta?.action === 'select' && query.kvMeta.table.name === ThreadsByParent.name) this.parentReads++;
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		await this.inner.executeBatch(queries, atomic);
	}
}

async function seedThreads(guildId: GuildID, parentId: ChannelID, from: bigint, count: number, type: number) {
	for (let offset = 0n; offset < BigInt(count); offset++) {
		await upsertOne(
			ThreadsByParent.upsertAll({
				parent_id: parentId,
				thread_id: createChannelID(from + offset),
				guild_id: guildId,
				type,
			}),
		);
	}
}

describe('accessibleThreadIds', () => {
	let executor: CountingExecutor;

	beforeEach(() => {
		executor = new CountingExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.inner.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('merges guilds newest first and stops enumerating at the shared cap', async () => {
		await seedThreads(GUILD_A, PARENT_A, 1_000n, 600, ChannelTypes.PUBLIC_THREAD);
		await seedThreads(GUILD_B, PARENT_B, 2_000n, 600, ChannelTypes.PUBLIC_THREAD);
		await seedThreads(GUILD_B, PARENT_B, 3_000n, 1, ChannelTypes.PRIVATE_THREAD);
		const gatewayService = {getUserPermissions: vi.fn(async () => 0n)} as unknown as IGatewayService;
		executor.parentReads = 0;

		const scope = await accessibleThreadIds({
			gatewayService,
			userId: USER_ID,
			groups: [
				{guildId: GUILD_A, parentIds: [PARENT_A]},
				{guildId: GUILD_B, parentIds: [PARENT_B]},
			],
		});

		expect(scope.size).toBe(300);
		expect([...scope.values()].every((parentId) => parentId === PARENT_B)).toBe(true);
		expect(scope.has('2599')).toBe(true);
		expect(scope.has('2299')).toBe(false);
		expect(scope.has('3000')).toBe(false);
		expect(executor.parentReads).toBe(3);
	});

	it('keeps the newest threads across many interleaved parents', async () => {
		const parents = Array.from({length: 40}, (_, index) => createChannelID(500n + BigInt(index)));
		for (const [index, parentId] of parents.entries()) {
			for (let step = 0n; step < 10n; step++) {
				await upsertOne(
					ThreadsByParent.upsertAll({
						parent_id: parentId,
						thread_id: createChannelID(10_000n + step * 40n + BigInt(index)),
						guild_id: GUILD_A,
						type: ChannelTypes.PUBLIC_THREAD,
					}),
				);
			}
		}
		const gatewayService = {getUserPermissions: vi.fn(async () => 0n)} as unknown as IGatewayService;

		const scope = await accessibleThreadIds({
			gatewayService,
			userId: USER_ID,
			groups: [{guildId: GUILD_A, parentIds: parents}],
		});

		expect([...scope.keys()]).toEqual(Array.from({length: 300}, (_, index) => String(10_399 - index)));
		expect(scope.get('10399')).toBe(parents[39]);
	});
});
