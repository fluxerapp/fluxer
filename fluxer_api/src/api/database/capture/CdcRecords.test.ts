// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import {
	CDC_DROPPED_COLUMNS,
	CDC_KEPT_SENSITIVE,
	CDC_SKIPPED,
	CDC_TABLES,
	type CdcRecord,
} from '@app/api/database/capture/CdcRecords';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

interface SchemaTable {
	name: string;
	columns: Array<{name: string}>;
	primary_key: string;
}

const schema = JSON.parse(
	readFileSync(new URL('../../../../../tools/dev/cassandra_target_schema.json', import.meta.url), 'utf8'),
) as {tables: Array<SchemaTable>};
const tables = new Map(schema.tables.map((table) => [table.name, table]));
const fixture = JSON.parse(readFileSync(new URL('./fixtures/records.json', import.meta.url), 'utf8')) as Array<
	CdcRecord | {kind: string}
>;

const SECRET_RULE = /secret|token|hash/;

function keyColumns(table: SchemaTable): Set<string> {
	return new Set(table.primary_key.match(/[A-Za-z0-9_]+/g) ?? []);
}

function syncedColumns(): Array<[string, string]> {
	return [...CDC_TABLES].flatMap((name) => (tables.get(name)?.columns ?? []).map(({name: column}) => [name, column]));
}

function listed(policy: Readonly<Record<string, Readonly<Record<string, string>> | ReadonlyArray<string>>>) {
	return Object.entries(policy).flatMap(([table, columns]) =>
		(Array.isArray(columns) ? columns : Object.keys(columns)).map((column) => [table, column] as [string, string]),
	);
}

describe('cdc table policy', () => {
	it('decides every table of the schema exactly once', () => {
		const undecided = [...tables.keys()].filter((name) => CDC_TABLES.has(name) === CDC_SKIPPED.has(name));
		expect(undecided).toEqual([]);
		expect([...CDC_TABLES].filter((name) => !tables.has(name))).toEqual([]);
		expect(CDC_TABLES.has('store_account_tokens')).toBe(false);
		expect(CDC_TABLES.has('auth_sessions')).toBe(true);
	});

	it('drops or keeps on purpose every synced column that looks like a secret', () => {
		const decided = new Set(
			[...listed(CDC_DROPPED_COLUMNS), ...listed(CDC_KEPT_SENSITIVE)].map(([table, column]) => `${table}.${column}`),
		);
		const undecided = syncedColumns()
			.filter(([, column]) => SECRET_RULE.test(column))
			.map(([table, column]) => `${table}.${column}`)
			.filter((column) => !decided.has(column));
		expect(undecided).toEqual([]);
	});

	it('lists only real synced columns and drops no key column', () => {
		const columnsOf = (name: string) => new Set(tables.get(name)?.columns.map((column) => column.name));
		const unknown = [...listed(CDC_DROPPED_COLUMNS), ...listed(CDC_KEPT_SENSITIVE)].filter(
			([table, column]) => !CDC_TABLES.has(table) || !columnsOf(table).has(column),
		);
		expect(unknown).toEqual([]);
		const droppedKeys = listed(CDC_DROPPED_COLUMNS).filter(([table, column]) => {
			const definition = tables.get(table);
			return definition !== undefined && keyColumns(definition).has(column);
		});
		expect(droppedKeys).toEqual([]);
	});
});

describe('cdc records in the shared fixture', () => {
	it('key only synced tables and survive a decode and encode unchanged', () => {
		const codec = new CaptureCodec(cassandra.types);
		const keyRecords = fixture.flatMap((record) => ((record as CdcRecord).kind === 'k' ? [record as CdcRecord] : []));
		expect(keyRecords.length).toBeGreaterThan(1);
		for (const record of keyRecords) {
			if (record.kind !== 'k') continue;
			for (const key of record.keys) {
				expect(CDC_TABLES.has(key.t), key.t).toBe(true);
				expect(codec.encodeRecord(codec.decodeRecord(key.c))).toEqual(key.c);
			}
		}
		const life = fixture.filter((record) => record.kind === 'life');
		for (const field of ['pod', 'from_seq', 'dropped']) {
			expect(
				life.some((record) => field in record),
				field,
			).toBe(true);
		}
		expect(life.some((record) => (record as CdcRecord & {state: string}).state === 'alive')).toBe(true);
	});
});
