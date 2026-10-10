// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KvQueryMeta, KvTableSpec} from '@app/api/database/CassandraTypes';
import {PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {describe, expect, it} from 'vitest';

type Row = Record<string, unknown>;

const Probe: KvTableSpec<Row> = {
	name: 'kv_unordered_probe',
	columns: ['user_id', 'username'],
	primaryKey: ['user_id'],
	partitionKey: ['user_id'],
};

function recordingClient(statements: Array<{text: string; values: Array<unknown> | undefined}>): IPostgresClient {
	const client = {
		async query(text: string, values?: Array<unknown>) {
			statements.push({text, values});
			const rows = [
				{row_key: 'a', row_data: {user_id: '2', username: 'second'}},
				{row_key: 'b', row_data: {user_id: '1', username: 'first'}},
			];
			return {rows, rowCount: rows.length};
		},
		async connect() {},
		async shutdown() {},
		async transaction(fn: (db: unknown) => Promise<unknown>) {
			return fn(client);
		},
		kvTable() {
			return 'kv_unordered_table';
		},
	} as never;
	return client;
}

function selectMeta(extra: Row): KvQueryMeta<Row> {
	return {action: 'select', table: Probe, where: [], columns: ['user_id'], ...extra} as unknown as KvQueryMeta<Row>;
}

describe('Postgres KV unordered scan', () => {
	it('pushes the limit into SQL for an unordered scan without filters', async () => {
		const statements: Array<{text: string; values: Array<unknown> | undefined}> = [];
		const executor = new PostgresKvQueryExecutor(recordingClient(statements));
		const rows = await executor.executeQuery({
			cql: '__unordered_probe',
			params: {},
			kvMeta: selectMeta({limit: 1, unordered: true}),
		});
		expect(statements).toHaveLength(1);
		expect(statements[0]!.text).toMatch(/LIMIT \$2$/);
		expect(statements[0]!.values).toEqual([Probe.name, 1]);
		expect(rows).toEqual([{user_id: '2'}, {user_id: '1'}]);
	});

	it('keeps the sorted full scan when order is asked for', async () => {
		const statements: Array<{text: string; values: Array<unknown> | undefined}> = [];
		const executor = new PostgresKvQueryExecutor(recordingClient(statements));
		const rows = await executor.executeQuery({
			cql: '__ordered_probe',
			params: {},
			kvMeta: selectMeta({limit: 1}),
		});
		expect(statements[0]!.text).not.toContain('LIMIT');
		expect(rows).toEqual([{user_id: '1'}]);
	});
});
