// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TableLayout} from '@app/api/capture/CaptureTables';
import {linesForKey, recheckKeyText} from '@app/api/capture/cdc/CdcApply';
import {PendingKeys} from '@app/api/capture/cdc/CdcPending';
import {clickHouseType, RowEncoder} from '@app/api/capture/cdc/CdcRows';
import type {CdcTable} from '@app/api/capture/cdc/CdcSchema';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

const encoder = new RowEncoder(cassandra.types);

const layout: TableLayout = {
	name: 'saved_messages',
	primaryKey: ['user_id', 'message_id'],
	partitionKey: ['user_id'],
	regular: ['channel_id'],
	multiCell: new Set(),
	defaultTtl: 0,
};

const column = (name: string) => ({name, cqlType: 'bigint', chType: clickHouseType('bigint')});
const table: CdcTable = {
	layout,
	columns: ['channel_id', 'message_id', 'user_id'].map(column),
	keyColumns: ['user_id', 'message_id'].map(column),
};

const row = (message: bigint) => ({
	key: {user_id: 1n, message_id: message},
	cells: {channel_id: {v: 5n, wt: 10, ttl: null}},
});

describe('lines for a key', () => {
	it('writes the row read back for a row key', () => {
		const {lines, tombstones} = linesForKey({
			encoder,
			table,
			scope: 'row',
			ts: 400,
			key: {user_id: 1n, message_id: 2n},
			rows: [row(2n)],
			live: [],
			readStartMs: 1,
		});
		expect(lines).toEqual(['{"channel_id":5,"message_id":2,"user_id":1,"_version":1000,"_deleted":0}']);
		expect(tombstones).toBe(0);
	});

	it('writes a tombstone above the record timestamp when the row is gone', () => {
		const {lines, tombstones} = linesForKey({
			encoder,
			table,
			scope: 'row',
			ts: 9000,
			key: {user_id: 1n, message_id: 2n},
			rows: [],
			live: [],
			readStartMs: 1,
		});
		expect(lines).toEqual(['{"user_id":1,"message_id":2,"_version":9000,"_deleted":1}']);
		expect(tombstones).toBe(1);
	});

	it('writes every row under a partition and a tombstone for each live key Cassandra no longer has', () => {
		const {lines, tombstones} = linesForKey({
			encoder,
			table,
			scope: 'partition',
			ts: 0,
			key: {user_id: 1n},
			rows: [row(2n)],
			live: [
				{user_id: 1n, message_id: 2n},
				{user_id: 1n, message_id: 3n},
			],
			readStartMs: 2,
		});
		expect(lines).toEqual([
			'{"channel_id":5,"message_id":2,"user_id":1,"_version":2000,"_deleted":0}',
			'{"user_id":1,"message_id":3,"_version":2000,"_deleted":1}',
		]);
		expect(tombstones).toBe(1);
	});
});

describe('pending keys', () => {
	it('coalesces keys of one table, scope and key, keeping the largest timestamp and every flag', () => {
		const pending = new PendingKeys();
		const base = {table: 'users', scope: 'row' as const, primaryKey: ['user_id'], serial: false, unknown: false};
		pending.add({...base, key: {user_id: {$t: 'i64', v: '1'}}, ts: 5});
		pending.add({...base, key: {user_id: {$t: 'i64', v: '1'}}, ts: 9, unknown: true, serial: true});
		pending.add({...base, key: {user_id: {$t: 'i64', v: '1'}}, ts: 2, recheckDue: '2026-01-01 00:00:00.000'});
		pending.add({...base, key: {user_id: {$t: 'i64', v: '2'}}, ts: 1});
		pending.add({...base, scope: 'partition', key: {user_id: {$t: 'i64', v: '1'}}, ts: 1});
		const keys = pending.take();
		expect(keys).toHaveLength(3);
		expect(keys[0]).toMatchObject({ts: 9, unknown: true, serial: true, recheckDue: '2026-01-01 00:00:00.000'});
		expect(pending.size).toBe(0);
	});

	it('writes recheck keys in primary key order whatever order the record used', () => {
		expect(recheckKeyText(['user_id', 'key_id'], {key_id: 'b', user_id: 'a'})).toBe('{"user_id":"a","key_id":"b"}');
	});
});
