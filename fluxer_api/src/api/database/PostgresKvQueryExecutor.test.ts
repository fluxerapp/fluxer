// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {describe, expect, it, vi} from 'vitest';
import {defineTable} from './CassandraTableDsl';
import {PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

interface BoundedRow {
	bucket: string;
	created_at: Date;
	job_id: bigint;
}

const BoundedRows = defineTable<BoundedRow, 'bucket' | 'created_at' | 'job_id', 'bucket'>({
	name: 'bounded_rows',
	columns: ['bucket', 'created_at', 'job_id'],
	primaryKey: ['bucket', 'created_at', 'job_id'],
	partitionKey: ['bucket'],
});

interface SingleKeyRow {
	id: bigint;
}

interface NullableOrderRow {
	id: bigint;
	ordered_at: Date | null;
}

const NullableOrderRows = defineTable<NullableOrderRow, 'id'>({
	name: 'nullable_order_rows',
	columns: ['id', 'ordered_at'],
	primaryKey: ['id'],
});

const SingleKeyRows = defineTable<SingleKeyRow, 'id'>({
	name: 'single_key_rows',
	columns: ['id'],
	primaryKey: ['id'],
});

describe('PostgresKvQueryExecutor bounded selects', () => {
	it('pushes tuple predicates, ordering, and LIMIT into PostgreSQL', async () => {
		const query = vi.fn().mockResolvedValue({rows: [], rowCount: 0});
		const client = {
			kvTable: () => 'fluxer_kv_test',
			query,
		} as unknown as IPostgresClient;
		const executor = new PostgresKvQueryExecutor(client);

		await executor.executeQuery(
			BoundedRows.select({
				where: [
					BoundedRows.where.eq('bucket'),
					BoundedRows.where.tupleLt(['created_at', 'job_id'], ['cursor_created_at', 'cursor_job_id']),
				],
				orderBy: [
					{col: 'created_at', direction: 'DESC'},
					{col: 'job_id', direction: 'DESC'},
				],
				limit: 25,
			}).bind({
				bucket: '2026-08-11',
				cursor_created_at: new Date('2026-08-11T00:00:00.000Z'),
				cursor_job_id: 42n,
			}),
		);

		const sql = String(query.mock.calls[0]?.[0]);
		expect(sql).toContain('LIMIT');
		expect(sql).toContain('ORDER BY');
		expect(sql).toContain('<');
	});

	it('bounds paged compatibility queries in PostgreSQL instead of materializing the partition', async () => {
		const query = vi.fn().mockResolvedValue({rows: [], rowCount: 0});
		const client = {kvTable: () => 'fluxer_kv_test', query} as unknown as IPostgresClient;
		const executor = new PostgresKvQueryExecutor(client);

		await executor.executePagedQuery(BoundedRows.select().bind({}), {pageSize: 25, pageState: null});

		const sql = String(query.mock.calls[0]?.[0]);
		expect(sql).toContain('LIMIT');
		expect(sql).toContain('ORDER BY');
	});

	it('preserves explicit order and limit for primary-key IN selects', async () => {
		const query = vi.fn().mockResolvedValue({rows: [], rowCount: 0});
		const client = {kvTable: () => 'fluxer_kv_test', query} as unknown as IPostgresClient;
		const executor = new PostgresKvQueryExecutor(client);

		await executor.executeQuery(
			SingleKeyRows.select({
				where: SingleKeyRows.where.in('id', 'ids'),
				orderBy: {col: 'id', direction: 'DESC'},
				limit: 2,
			}).bind({ids: [3n, 2n, 1n]}),
		);

		const sql = String(query.mock.calls[0]?.[0]);
		expect(sql).toContain('LIMIT');
		expect(sql).toContain('ORDER BY');
	});

	it('continues keyset paging when the last ordered value is null', async () => {
		const encodedId = (id: string) => ({__fluxer_type: 'bigint', value: id});
		const query = vi
			.fn()
			.mockResolvedValueOnce({
				rows: [
					{row_key: '1', row_data: {id: encodedId('1'), ordered_at: null}},
					{row_key: '2', row_data: {id: encodedId('2'), ordered_at: null}},
				],
				rowCount: 2,
			})
			.mockResolvedValueOnce({rows: [], rowCount: 0});
		const client = {kvTable: () => 'fluxer_kv_test', query} as unknown as IPostgresClient;
		const executor = new PostgresKvQueryExecutor(client);
		const prepared = NullableOrderRows.select({
			orderBy: [
				{col: 'ordered_at', direction: 'DESC'},
				{col: 'id', direction: 'ASC'},
			],
		}).bind({});

		const first = await executor.executePagedQuery<NullableOrderRow>(prepared, {pageSize: 1});
		expect(first.pageState).not.toBeNull();
		await expect(
			executor.executePagedQuery<NullableOrderRow>(prepared, {pageSize: 1, pageState: first.pageState}),
		).resolves.toEqual({rows: [], pageState: null});
		expect(String(query.mock.calls[1]?.[0])).toContain('COALESCE(row_data ->');
	});
});
