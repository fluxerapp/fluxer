// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TableLayout} from '@app/api/capture/CaptureTables';
import {
	type CdcColumn,
	clickHouseType,
	isCollectionType,
	RowEncoder,
	rowExpiry,
	rowVersion,
	timestampText,
	UnmappableValue,
} from '@app/api/capture/cdc/CdcRows';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

const encoder = new RowEncoder(cassandra.types);

function column(name: string, cqlType: string): CdcColumn {
	return {name, cqlType, chType: clickHouseType(cqlType)};
}

const layout: TableLayout = {
	name: 'keys',
	primaryKey: ['user_id', 'key_id'],
	partitionKey: ['user_id'],
	regular: ['counter', 'public_key', 'roles'],
	multiCell: new Set(['roles']),
	defaultTtl: 0,
};

describe('clickhouse types', () => {
	it('keeps the sync.py type map for scalars and stores collections as strings', () => {
		expect(
			['text', 'bigint', 'counter', 'int', 'boolean', 'timestamp', 'date', 'double', 'blob', 'uuid', 'varint'].map(
				clickHouseType,
			),
		).toEqual([
			'String',
			'Int64',
			'Int64',
			'Int32',
			'UInt8',
			'Nullable(DateTime64(3))',
			'Nullable(Date32)',
			'Float64',
			'String',
			'String',
			'String',
		]);
		expect(
			['set<bigint>', 'map<text, text>', 'frozen<mute_config>', 'mute_config', 'frozen<list<int>>'].map(clickHouseType),
		).toEqual(['String', 'String', 'String', 'String', 'String']);
	});

	it('tells collection, tuple and UDT columns apart from scalar strings', () => {
		expect(['set<bigint>', 'frozen<mute_config>', 'tuple<int, text>', 'custom_status'].map(isCollectionType)).toEqual([
			true,
			true,
			true,
			true,
		]);
		expect(['text', 'uuid', 'blob', 'bigint', 'timestamp'].map(isCollectionType)).toEqual([
			false,
			false,
			false,
			false,
			false,
		]);
	});
});

describe('scalar values', () => {
	it('writes scalars the way sync.py does', () => {
		const at = new Date('2026-01-02T03:04:05.678Z');
		expect(encoder.column(column('a', 'text'), 'hi')).toBe('hi');
		expect(encoder.column(column('a', 'bigint'), 123456789012345678n)).toBe(123456789012345678n);
		expect(encoder.column(column('a', 'varint'), 42n)).toBe('42');
		expect(encoder.column(column('a', 'boolean'), true)).toBe(1);
		expect(encoder.column(column('a', 'boolean'), false)).toBe(0);
		expect(encoder.column(column('a', 'timestamp'), at)).toBe('2026-01-02 03:04:05.678');
		expect(encoder.column(column('a', 'date'), cassandra.types.LocalDate.fromString('2026-11-21'))).toBe('2026-11-21');
		expect(encoder.column(column('a', 'blob'), Buffer.from([0, 171, 255]))).toBe('00abff');
		const uuid = cassandra.types.Uuid.fromString('6a9f0d9e-2b8a-4c1e-9c55-1f3a2b4c5d6e');
		expect(encoder.column(column('a', 'uuid'), uuid)).toBe('6a9f0d9e-2b8a-4c1e-9c55-1f3a2b4c5d6e');
		expect(encoder.column(column('a', 'inet'), cassandra.types.InetAddress.fromString('192.0.2.1'))).toBe('192.0.2.1');
	});

	it('leaves NULL out so ClickHouse stores the type default', () => {
		expect(encoder.column(column('a', 'int'), null)).toBeUndefined();
		expect(encoder.column(column('a', 'text'), undefined)).toBeUndefined();
	});

	it('turns timestamps Python cannot hold into NULL', () => {
		expect(timestampText(Date.UTC(9999, 11, 31, 23, 59, 59, 999))).toBe('9999-12-31 23:59:59.999');
		expect(timestampText(Date.UTC(10000, 0, 1))).toBeNull();
		expect(timestampText(-62_135_596_800_001)).toBeNull();
	});

	it('quotes non-finite floats so ClickHouse parses them', () => {
		const line = encoder.tombstoneLine([column('ratio', 'double')], {ratio: Number.NaN}, 1);
		expect(line).toBe('{"ratio":"nan","_version":1,"_deleted":1}');
	});

	it('refuses values it cannot map', () => {
		expect(() => encoder.nested(Symbol('x'))).toThrow(UnmappableValue);
	});
});

describe('collection values', () => {
	it('writes real JSON for sets, maps, tuples and UDTs in Python json.dumps spacing', () => {
		const at = new Date('2026-01-02T03:04:05.678Z');
		expect(encoder.column(column('a', 'set<bigint>'), new Set([5n, 9007199254740993n]))).toBe('[5, 9007199254740993]');
		expect(encoder.column(column('a', 'list<text>'), ['a', 'b'])).toBe('["a", "b"]');
		expect(encoder.column(column('a', 'map<bigint, text>'), new Map([[7n, 'x\n"y"']]))).toBe('{"7": "x\\n\\"y\\""}');
		expect(encoder.column(column('a', 'tuple<int, boolean>'), cassandra.types.Tuple.fromArray([1, true]))).toBe(
			'[1, 1]',
		);
		expect(
			encoder.column(column('a', 'frozen<mute_config>'), {end_time: at, selected_time_window: 3, muted: null}),
		).toBe('{"end_time": "2026-01-02 03:04:05.678", "selected_time_window": 3, "muted": null}');
		expect(
			encoder.column(column('a', 'map<text, frozen<channel_override>>'), new Map([['c', {flags: 1, muted: false}]])),
		).toBe('{"c": {"flags": 1, "muted": 0}}');
	});
});

describe('row lines', () => {
	const columns = [
		column('counter', 'bigint'),
		column('key_id', 'bigint'),
		column('public_key', 'blob'),
		column('roles', 'set<text>'),
		column('user_id', 'bigint'),
	];

	it('writes key and regular columns with the version and expiry columns', () => {
		const row = {key: {user_id: 1n, key_id: 2n}, cells: {counter: {v: 9n, wt: 50, ttl: null}}};
		expect(encoder.rowLine(columns, row, 77, {expiresAtMs: Date.UTC(2026, 0, 1), rereadAtMs: 0})).toBe(
			'{"counter":9,"key_id":2,"user_id":1,"_version":77,"_deleted":0,"_expires_at":"2026-01-01 00:00:00.000"}',
		);
	});

	it('writes a tombstone with only the key columns', () => {
		const keys = [column('user_id', 'bigint'), column('key_id', 'bigint')];
		expect(encoder.tombstoneLine(keys, {user_id: 1n, key_id: 2n}, 300)).toBe(
			'{"user_id":1,"key_id":2,"_version":300,"_deleted":1}',
		);
	});

	it('reads ClickHouse key values back into driver values', () => {
		expect(encoder.fromClickHouse('bigint', '123456789012345678')).toBe(123456789012345678n);
		expect(encoder.fromClickHouse('timestamp', '2026-01-02 03:04:05.678')).toEqual(
			new Date('2026-01-02T03:04:05.678Z'),
		);
		expect(encoder.fromClickHouse('boolean', 1)).toBe(true);
		expect(encoder.fromClickHouse('text', 'a')).toBe('a');
		expect(String(encoder.fromClickHouse('timeuuid', 'd2177dd0-eaa2-11de-a572-001b779c76e3'))).toBe(
			'd2177dd0-eaa2-11de-a572-001b779c76e3',
		);
	});
});

describe('versions', () => {
	it('takes the record timestamp when a cell delete left only older cells', () => {
		const row = {key: {}, cells: {a: {v: 1, wt: 100, ttl: null}}};
		expect(rowVersion(300, row, 200)).toBe(300);
	});

	it('takes the largest write time when an LWT wrote after the record timestamp', () => {
		const row = {key: {}, cells: {a: {v: 1, wt: 100, ttl: null}, b: {v: 2, wt: 900, ttl: null}}};
		expect(rowVersion(300, row, 200)).toBe(900);
	});

	it('takes the read start for a missing row so a tombstone outranks every earlier read', () => {
		expect(rowVersion(0, null, 5000)).toBe(5000);
	});
});

describe('expiry', () => {
	const start = 1_000_000;
	const cell = (ttl: number | null) => ({v: 1, wt: 1, ttl});

	it('expires a row whose cells share one TTL', () => {
		const row = {key: {}, cells: {counter: cell(60), public_key: cell(60)}};
		expect(rowExpiry(layout, row, start)).toEqual({expiresAtMs: start + 60_000, rereadAtMs: 0});
	});

	it('rereads a row whose cells expire at different times and expires it at the last one', () => {
		const row = {key: {}, cells: {counter: cell(3600), public_key: cell(600)}};
		expect(rowExpiry(layout, row, start)).toEqual({expiresAtMs: start + 3_600_000, rereadAtMs: start + 600_000});
	});

	it('never expires a row with a cell that has no TTL, and rereads it when the others expire', () => {
		const row = {key: {}, cells: {counter: cell(null), public_key: cell(600)}};
		expect(rowExpiry(layout, row, start)).toEqual({expiresAtMs: 0, rereadAtMs: start + 600_000});
	});

	it('rereads instead of expiring when a collection has a TTL, since its elements can differ', () => {
		const row = {key: {}, cells: {roles: cell(600)}};
		expect(rowExpiry(layout, row, start)).toEqual({expiresAtMs: 0, rereadAtMs: start + 600_000});
	});

	it('rereads a key-only row of a default TTL table after the default TTL', () => {
		expect(rowExpiry({...layout, defaultTtl: 30}, {key: {}, cells: {}}, start)).toEqual({
			expiresAtMs: 0,
			rereadAtMs: start + 30_000,
		});
		expect(rowExpiry(layout, {key: {}, cells: {counter: cell(null)}}, start)).toEqual({expiresAtMs: 0, rereadAtMs: 0});
	});
});
