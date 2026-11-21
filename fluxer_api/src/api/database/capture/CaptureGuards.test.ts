// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {isReplayableCql} from '@app/api/capture/ReplayStatements';
import type {CassandraParams, PreparedQuery, Table} from '@app/api/database/CassandraTypes';
import * as Tables from '@app/api/Tables';
import {describe, expect, it} from 'vitest';

type AnyTable = Table<Record<string, unknown>, string, string>;

function isTable(value: unknown): value is AnyTable {
	return typeof value === 'object' && value !== null && 'upsertAll' in value && 'primaryKey' in value;
}

function statementsOf(table: AnyTable): Array<PreparedQuery> {
	const row: CassandraParams = Object.fromEntries(table.columns.map((column) => [column, 1]));
	const pk = Object.fromEntries(table.primaryKey.map((column) => [column, 1]));
	const partition = Object.fromEntries(table.partitionKey.map((column) => [column, 1]));
	const regular = table.columns.find((column) => !table.primaryKey.includes(column));
	const statements = [
		table.upsertAll(row),
		table.upsertAllWithTtl(row, 60),
		table.insert(row),
		table.insertWithTtl(row, 60),
		table.insertIfNotExists(row),
		table.insertIfNotExistsWithTtl(row, 60),
		table.insertWithNow(row, table.primaryKey.at(-1) ?? ''),
		table.deleteByPk(pk),
		table.deletePartition(partition),
		table.conditionalBatch([{action: 'insert', row}]),
	];
	if (regular === undefined) return statements;
	const patch = {[regular]: {kind: 'set' as const, value: 2}};
	return [
		...statements,
		table.patchByPk(pk, patch),
		table.patchByPkWithTtl(pk, patch, 60),
		table.conditionalPatchByPk(pk, patch, {[regular]: 1}),
		table.conditionalPatchByPkWithTtl(pk, patch, {[regular]: 1}, 60),
		table.conditionalDeleteByPk(pk, {[regular]: 1}),
	];
}

describe('capture guards', () => {
	it('finds no counter column in the schema', () => {
		const schema = JSON.parse(
			readFileSync(new URL('../../../../../tools/dev/cassandra_target_schema.json', import.meta.url), 'utf8'),
		) as {tables: Array<{name: string; columns: Array<{name: string; type: string}>}>};
		const counters = schema.tables.flatMap((table) =>
			table.columns.filter((column) => column.type === 'counter').map((column) => `${table.name}.${column.name}`),
		);
		expect(counters).toEqual([]);
	});

	it('builds no statement that depends on the server, sets its own timestamp or updates relative to a value', () => {
		const tables = (Object.values(Tables) as Array<unknown>).filter(isTable);
		expect(tables.length).toBeGreaterThan(100);
		const unreplayable = tables
			.flatMap(statementsOf)
			.map((statement) => statement.cql)
			.filter((cql) => !isReplayableCql(cql));
		expect(unreplayable).toEqual([]);
	});
});
