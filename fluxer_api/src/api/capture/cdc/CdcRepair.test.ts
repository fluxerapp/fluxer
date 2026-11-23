// SPDX-License-Identifier: AGPL-3.0-or-later

import {nextRepair, REPAIR_INTERVAL_MS, TableLocks, vanishedTombstoneSql} from '@app/api/capture/cdc/CdcRepair';
import {clickHouseType} from '@app/api/capture/cdc/CdcRows';
import {settingDifferences} from '@app/api/capture/cdc/CdcSetup';
import {describe, expect, it} from 'vitest';

const DAY = 86_400_000;
const done = (scanStartMs: number) => ({scanStartMs, finishedMs: scanStartMs + 1000, rows: 1, tombstones: 0});

describe('next repair', () => {
	it('picks a table that was never scanned first, as a marked repair', () => {
		expect(nextRepair(['a', 'b'], new Map([['a', done(0)]]), new Map(), DAY)).toEqual({table: 'b', marked: true});
	});

	it('picks the earliest marked table before any routine repair', () => {
		const repairs = new Map([
			['a', done(0)],
			['b', done(0)],
			['c', done(0)],
		]);
		const due = new Map([
			['b', 50],
			['c', 20],
		]);
		expect(nextRepair(['a', 'b', 'c'], repairs, due, 30 * DAY)).toEqual({table: 'c', marked: true});
	});

	it('keeps a mark set while the table was being scanned', () => {
		expect(nextRepair(['a'], new Map([['a', done(100)]]), new Map([['a', 150]]), 200)).toEqual({
			table: 'a',
			marked: true,
		});
		expect(nextRepair(['a'], new Map([['a', done(100)]]), new Map([['a', 50]]), 200)).toBeNull();
	});

	it('picks the table repaired longest ago once it is 7 days old', () => {
		const repairs = new Map([
			['a', done(2 * DAY)],
			['b', done(DAY)],
		]);
		expect(nextRepair(['a', 'b'], repairs, new Map(), DAY + REPAIR_INTERVAL_MS)).toBeNull();
		expect(nextRepair(['a', 'b'], repairs, new Map(), DAY + 1000 + REPAIR_INTERVAL_MS)).toEqual({
			table: 'b',
			marked: false,
		});
	});
});

describe('repair tombstones', () => {
	it('tombstones every live key older than the scan start', () => {
		const column = (name: string) => ({name, cqlType: 'bigint', chType: clickHouseType('bigint')});
		const table = {
			layout: {
				name: 'notes',
				primaryKey: ['user_id', 'target_id'],
				partitionKey: ['user_id'],
				regular: [],
				multiCell: new Set<string>(),
				defaultTtl: 0,
			},
			columns: [column('target_id'), column('user_id')],
			keyColumns: [column('user_id'), column('target_id')],
		};
		expect(vanishedTombstoneSql(table, 1234)).toBe(
			'INSERT INTO fluxer_cdc.`notes` (`user_id`, `target_id`, _version, _deleted) SELECT `user_id`, `target_id`, toUInt64(1234), 1 FROM fluxer_cdc.`notes` FINAL WHERE _deleted = 0 AND _version < 1234',
		);
	});
});

describe('table locks', () => {
	it('runs holders of a shared table one after the other and others side by side', async () => {
		const locks = new TableLocks();
		const events: Array<string> = [];
		const first = Promise.withResolvers<void>();
		const run = (label: string, tables: Array<string>, wait?: Promise<void>) =>
			locks.hold(tables, async () => {
				events.push(`${label} start`);
				await wait;
				events.push(`${label} end`);
			});
		const held = [
			run('repair', ['users'], first.promise),
			run('flush', ['users', 'notes']),
			run('other', ['channels']),
		];
		await new Promise((resolve) => setImmediate(resolve));
		expect(events).toEqual(['repair start', 'other start', 'other end']);
		first.resolve();
		await Promise.all(held);
		expect(events.slice(3)).toEqual(['repair end', 'flush start', 'flush end']);
	});
});

describe('setup differences', () => {
	it('lists every setting that differs, ignoring subject order', () => {
		expect(
			settingDifferences(
				'stream S',
				{subjects: ['a', 'b'], max_age: 10, deny_purge: true},
				{subjects: ['b', 'a'], max_age: 20},
			),
		).toEqual(['stream S max_age is 20, wanted 10', 'stream S deny_purge is null, wanted true']);
	});
});
