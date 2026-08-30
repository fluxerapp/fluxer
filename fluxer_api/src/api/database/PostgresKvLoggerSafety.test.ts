// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {defineTable} from './CassandraTableDsl';
import type {KvQueryMeta} from './CassandraTypes';

vi.mock('../Logger', () => ({
	Logger: new Proxy(
		{},
		{
			get() {
				throw new Error('Logger has not been initialized. Call initializeLogger() first.');
			},
		},
	),
}));

type Row = Record<string, unknown>;

const Probe = defineTable<{k: string; v: string}, 'k'>({
	name: 'kv_logger_probe',
	columns: ['k', 'v'],
	primaryKey: ['k'],
});

const client = {
	async query() {
		return {rows: [], rowCount: 0};
	},
	async connect() {},
	async shutdown() {},
	isConnected() {
		return true;
	},
	async transaction(fn: (db: unknown) => Promise<unknown>) {
		return fn(client);
	},
	kvTable() {
		return 'kv_logger_probe_table';
	},
} as never;

describe('PostgresKvQueryExecutor logging safety', () => {
	it('runs full scan queries in a process whose logger was never initialized', async () => {
		const {PostgresKvQueryExecutor} = await import('./PostgresKvQueryExecutor');
		const executor = new PostgresKvQueryExecutor(client);
		for (const action of ['select', 'count', 'delete'] as const) {
			const meta = {action, table: Probe, where: [], columns: ['k', 'v']} as unknown as KvQueryMeta<Row>;
			await expect(executor.executeQuery({cql: `__probe_${action}`, params: {}, kvMeta: meta})).resolves.toBeDefined();
		}
	});
});
