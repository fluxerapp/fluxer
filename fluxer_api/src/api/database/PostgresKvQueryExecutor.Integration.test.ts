// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {existsSync} from 'node:fs';
import {getDefaultPostgresClient, initPostgres, quoteIdentifier, shutdownPostgres} from '@pkgs/postgres/src/Client';
import cassandra from 'cassandra-driver';
import {afterAll, beforeAll, describe, expect, test} from 'vitest';
import {defineTable} from './CassandraTableDsl';
import {Db} from './CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor, pruneExpiredPostgresKvRows} from './PostgresKvQueryExecutor';

interface TestRow {
	tenant_id: string;
	item_id: bigint;
	created_at: Date;
	payload: Buffer | null;
	tags: Set<string> | null;
	counts: Map<string, bigint> | null;
	local_day: cassandra.types.LocalDate | null;
	note: string | null;
}

function postgresHost(): string {
	return process.env.FLUXER_POSTGRES_TEST_HOST ?? (existsSync('/.dockerenv') ? 'host.docker.internal' : '127.0.0.1');
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

const kvTable = `fluxer_kv_test_${randomUUID().replaceAll('-', '_')}`;
const logicalTableName = `pg_kv_test_${randomUUID().replaceAll('-', '_')}`;
const TestRows = defineTable<TestRow, 'tenant_id' | 'item_id', 'tenant_id'>({
	name: logicalTableName,
	columns: ['tenant_id', 'item_id', 'created_at', 'payload', 'tags', 'counts', 'local_day', 'note'],
	primaryKey: ['tenant_id', 'item_id'],
	partitionKey: ['tenant_id'],
});

describe('PostgresKvQueryExecutor', () => {
	let executor: PostgresKvQueryExecutor;
	let initialized = false;

	beforeAll(async () => {
		await initPostgres({
			host: postgresHost(),
			port: Number(process.env.FLUXER_POSTGRES_PORT ?? 5432),
			database: process.env.FLUXER_POSTGRES_DATABASE ?? 'fluxer',
			username: process.env.FLUXER_POSTGRES_USERNAME ?? 'fluxer',
			password: process.env.FLUXER_POSTGRES_PASSWORD ?? 'fluxer',
			kvTable,
			maxConnections: 4,
		});
		initialized = true;
		const client = getDefaultPostgresClient();
		await ensurePostgresKvSchema(client);
		executor = new PostgresKvQueryExecutor(client);
	});

	afterAll(async () => {
		if (!initialized) return;
		try {
			await getDefaultPostgresClient().query(`DROP TABLE IF EXISTS ${quoteIdentifier(kvTable)}`);
		} finally {
			await shutdownPostgres();
		}
	});

	test('round trips typed values and conditional writes through a real Postgres KV table', async () => {
		const row: TestRow = {
			tenant_id: 'tenant-a',
			item_id: 10n,
			created_at: new Date('2026-01-02T03:04:05.006Z'),
			payload: Buffer.from('hello postgres kv'),
			tags: new Set(['alpha', 'beta']),
			counts: new Map([['seen', 5n]]),
			local_day: cassandra.types.LocalDate.fromString('2026-01-02'),
			note: 'first',
		};

		await expect(executor.executeQuery<{['[applied]']: boolean}>(TestRows.insertIfNotExists(row))).resolves.toEqual([
			{'[applied]': true},
		]);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(TestRows.insertIfNotExists({...row, note: 'second'})),
		).resolves.toEqual([{'[applied]': false}]);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkIf(
					{tenant_id: 'tenant-a', item_id: 10n},
					{note: Db.set('second')},
					{col: 'note', expected: 'first'},
				),
			),
		).resolves.toEqual([{'[applied]': true}]);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkIf(
					{tenant_id: 'tenant-a', item_id: 10n},
					{note: Db.set('third')},
					{col: 'note', expected: 'first'},
				),
			),
		).resolves.toEqual([{'[applied]': false}]);

		const fetched = await executor.executeQuery<TestRow>(
			TestRows.select({
				where: [TestRows.where.eq('tenant_id'), TestRows.where.eq('item_id')],
				limit: 1,
			}).bind({tenant_id: 'tenant-a', item_id: 10n}),
		);

		expect(fetched).toHaveLength(1);
		expect(fetched[0]!.item_id).toBe(10n);
		expect(fetched[0]!.created_at.getTime()).toBe(row.created_at.getTime());
		expect(fetched[0]!.payload?.equals(row.payload!)).toBe(true);
		expect([...fetched[0]!.tags!.values()].sort()).toEqual(['alpha', 'beta']);
		expect(fetched[0]!.counts!.get('seen')).toBe(5n);
		expect(fetched[0]!.local_day?.toString()).toBe('2026-01-02');
		expect(fetched[0]!.note).toBe('second');
	});

	test('applies every compare-and-set condition and treats missing legacy fields as null', async () => {
		const row: TestRow = {
			tenant_id: 'tenant-multi-condition',
			item_id: 1n,
			created_at: new Date('2026-01-03T00:00:00.000Z'),
			payload: null,
			tags: null,
			counts: null,
			local_day: null,
			note: null,
		};
		await executor.executeQuery(TestRows.upsertAll(row));
		await getDefaultPostgresClient().query(
			`UPDATE ${quoteIdentifier(kvTable)} SET row_data = row_data - 'note' WHERE table_name = $1`,
			[logicalTableName],
		);

		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkIf({tenant_id: row.tenant_id, item_id: row.item_id}, {note: Db.set('claimed')}, [
					{col: 'note', expected: null},
					{col: 'created_at', expected: row.created_at},
				]),
			),
		).resolves.toEqual([{'[applied]': true}]);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkIf({tenant_id: row.tenant_id, item_id: row.item_id}, {note: Db.set('stolen')}, [
					{col: 'note', expected: null},
					{col: 'created_at', expected: row.created_at},
				]),
			),
		).resolves.toEqual([{'[applied]': false}]);
	});

	test('filters, orders, patches, pages, and deletes logical rows', async () => {
		await executor.executeBatch(
			[
				TestRows.upsertAll({
					tenant_id: 'tenant-b',
					item_id: 1n,
					created_at: new Date('2026-02-01T00:00:00.000Z'),
					payload: null,
					tags: null,
					counts: null,
					local_day: null,
					note: 'one',
				}),
				TestRows.upsertAll({
					tenant_id: 'tenant-b',
					item_id: 2n,
					created_at: new Date('2026-02-02T00:00:00.000Z'),
					payload: null,
					tags: null,
					counts: null,
					local_day: null,
					note: 'two',
				}),
				TestRows.upsertAll({
					tenant_id: 'tenant-b',
					item_id: 3n,
					created_at: new Date('2026-02-03T00:00:00.000Z'),
					payload: null,
					tags: null,
					counts: null,
					local_day: null,
					note: 'three',
				}),
			].map((query) => ({query: query.cql, params: query.params, meta: query.kvMeta})),
		);

		const boundedFirst = await executor.executeQuery<TestRow>(
			TestRows.select({
				where: TestRows.where.eq('tenant_id'),
				orderBy: [
					{col: 'created_at', direction: 'DESC'},
					{col: 'item_id', direction: 'DESC'},
				],
				limit: 2,
			}).bind({tenant_id: 'tenant-b'}),
		);
		expect(boundedFirst.map((row) => row.item_id)).toEqual([3n, 2n]);
		const boundedSecond = await executor.executeQuery<TestRow>(
			TestRows.select({
				where: [
					TestRows.where.eq('tenant_id'),
					TestRows.where.tupleLt(['created_at', 'item_id'], ['cursor_created_at', 'cursor_item_id']),
				],
				orderBy: [
					{col: 'created_at', direction: 'DESC'},
					{col: 'item_id', direction: 'DESC'},
				],
				limit: 2,
			}).bind({
				tenant_id: 'tenant-b',
				cursor_created_at: boundedFirst[1]!.created_at,
				cursor_item_id: boundedFirst[1]!.item_id,
			}),
		);
		expect(boundedSecond.map((row) => row.item_id)).toEqual([1n]);

		const firstPage = await executor.executePagedQuery<TestRow>(
			TestRows.select({
				where: TestRows.where.eq('tenant_id'),
				orderBy: {col: 'item_id', direction: 'DESC'},
			}).bind({tenant_id: 'tenant-b'}),
			{pageSize: 2},
		);
		expect(firstPage.rows.map((row) => row.item_id)).toEqual([3n, 2n]);
		expect(firstPage.pageState).not.toBeNull();
		await executor.executeQuery(TestRows.deleteByPk({tenant_id: 'tenant-b', item_id: 3n}));

		const secondPage = await executor.executePagedQuery<TestRow>(
			TestRows.select({
				where: TestRows.where.eq('tenant_id'),
				orderBy: {col: 'item_id', direction: 'DESC'},
			}).bind({tenant_id: 'tenant-b'}),
			{pageSize: 2, pageState: firstPage.pageState},
		);
		expect(secondPage.rows.map((row) => row.item_id)).toEqual([1n]);
		expect(secondPage.pageState).toBeNull();

		await executor.executeQuery(TestRows.patchByPk({tenant_id: 'tenant-b', item_id: 2n}, {note: Db.clear()}));
		const patched = await executor.executeQuery<TestRow>(
			TestRows.select({where: [TestRows.where.eq('tenant_id'), TestRows.where.eq('item_id')], limit: 1}).bind({
				tenant_id: 'tenant-b',
				item_id: 2n,
			}),
		);
		expect(patched[0]!.note).toBeNull();

		await executor.executeQuery(TestRows.delete({where: TestRows.where.eq('tenant_id')}).bind({tenant_id: 'tenant-b'}));
		const remaining = await executor.executeQuery<TestRow>(
			TestRows.select({where: TestRows.where.eq('tenant_id')}).bind({tenant_id: 'tenant-b'}),
		);
		expect(remaining).toEqual([]);
	});

	test('uses stable mixed-direction keyset paging across concurrent deletion', async () => {
		const makeRow = (itemId: bigint, createdAt: string): TestRow => ({
			tenant_id: 'tenant-keyset',
			item_id: itemId,
			created_at: new Date(createdAt),
			payload: null,
			tags: null,
			counts: null,
			local_day: null,
			note: null,
		});
		for (const row of [
			makeRow(1n, '2026-03-02T00:00:00.000Z'),
			makeRow(2n, '2026-03-02T00:00:00.000Z'),
			makeRow(3n, '2026-03-03T00:00:00.000Z'),
		]) {
			await executor.executeQuery(TestRows.upsertAll(row));
		}
		const query = TestRows.select({
			where: TestRows.where.eq('tenant_id'),
			orderBy: [
				{col: 'created_at', direction: 'DESC'},
				{col: 'item_id', direction: 'ASC'},
			],
		}).bind({tenant_id: 'tenant-keyset'});

		const first = await executor.executePagedQuery<TestRow>(query, {pageSize: 2});
		expect(first.rows.map((row) => row.item_id)).toEqual([3n, 1n]);
		expect(first.pageState).not.toBeNull();
		await expect(
			executor.executePagedQuery<TestRow>(
				TestRows.select({
					where: TestRows.where.eq('tenant_id'),
					orderBy: [
						{col: 'created_at', direction: 'DESC'},
						{col: 'item_id', direction: 'ASC'},
					],
				}).bind({tenant_id: 'another-tenant'}),
				{pageSize: 2, pageState: first.pageState},
			),
		).rejects.toThrow('page state does not match query scope');
		await executor.executeQuery(TestRows.deleteByPk({tenant_id: 'tenant-keyset', item_id: 3n}));
		const second = await executor.executePagedQuery<TestRow>(query, {pageSize: 2, pageState: first.pageState});
		expect(second.rows.map((row) => row.item_id)).toEqual([2n]);
		expect(second.pageState).toBeNull();
	});

	test('applies conditional TTL patches atomically and rejects mismatched or expired rows', async () => {
		const row: TestRow = {
			tenant_id: 'tenant-conditional-ttl',
			item_id: 1n,
			created_at: new Date('2026-02-03T00:00:00.000Z'),
			payload: null,
			tags: null,
			counts: null,
			local_day: null,
			note: 'running',
		};
		await executor.executeQuery(TestRows.upsertAll(row));

		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkWithTtlIf({tenant_id: row.tenant_id, item_id: row.item_id}, {note: Db.set('deadletter')}, 1, {
					col: 'note',
					expected: 'running',
				}),
			),
		).resolves.toEqual([{'[applied]': true}]);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkWithTtlIf({tenant_id: row.tenant_id, item_id: row.item_id}, {note: Db.set('failed')}, 1, {
					col: 'note',
					expected: 'running',
				}),
			),
		).resolves.toEqual([{'[applied]': false}]);

		await sleep(1100);
		await expect(
			executor.executeQuery<{['[applied]']: boolean}>(
				TestRows.patchByPkWithTtlIf({tenant_id: row.tenant_id, item_id: row.item_id}, {note: Db.set('failed')}, 1, {
					col: 'note',
					expected: 'deadletter',
				}),
			),
		).resolves.toEqual([{'[applied]': false}]);
	});

	test('filters expired rows and prunes them physically', async () => {
		const client = getDefaultPostgresClient();
		await executor.executeQuery(
			TestRows.upsertAllWithTtl(
				{
					tenant_id: 'tenant-expired',
					item_id: 1n,
					created_at: new Date('2026-03-01T00:00:00.000Z'),
					payload: null,
					tags: null,
					counts: null,
					local_day: null,
					note: 'temporary',
				},
				1,
			),
		);

		await sleep(1100);

		const visibleRows = await executor.executeQuery<TestRow>(
			TestRows.select({where: TestRows.where.eq('tenant_id')}).bind({tenant_id: 'tenant-expired'}),
		);
		expect(visibleRows).toEqual([]);

		const physicalBefore = await client.query<{count: string}>(
			`SELECT count(*)::text AS count FROM ${quoteIdentifier(kvTable)} WHERE table_name = $1 AND row_data ->> 'tenant_id' = $2`,
			[logicalTableName, 'tenant-expired'],
		);
		expect(Number(physicalBefore.rows[0]!.count)).toBe(1);

		const pruned = await pruneExpiredPostgresKvRows(client, 10);
		expect(pruned).toBeGreaterThanOrEqual(1);

		const physicalAfter = await client.query<{count: string}>(
			`SELECT count(*)::text AS count FROM ${quoteIdentifier(kvTable)} WHERE table_name = $1 AND row_data ->> 'tenant_id' = $2`,
			[logicalTableName, 'tenant-expired'],
		);
		expect(Number(physicalAfter.rows[0]!.count)).toBe(0);
	});

	test('patching a logically expired row makes it visible again without keeping stale TTL', async () => {
		await executor.executeQuery(
			TestRows.upsertAllWithTtl(
				{
					tenant_id: 'tenant-resurrect',
					item_id: 1n,
					created_at: new Date('2026-04-01T00:00:00.000Z'),
					payload: null,
					tags: null,
					counts: null,
					local_day: null,
					note: 'temporary',
				},
				1,
			),
		);

		await sleep(1100);
		await executor.executeQuery(
			TestRows.patchByPk({tenant_id: 'tenant-resurrect', item_id: 1n}, {note: Db.set('resurrected')}),
		);

		const rows = await executor.executeQuery<TestRow>(
			TestRows.select({
				where: [TestRows.where.eq('tenant_id'), TestRows.where.eq('item_id')],
				limit: 1,
			}).bind({tenant_id: 'tenant-resurrect', item_id: 1n}),
		);
		expect(rows).toHaveLength(1);
		expect(rows[0]!.note).toBe('resurrected');
	});
});
