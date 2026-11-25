// SPDX-License-Identifier: AGPL-3.0-or-later

import {CDC_DERIVED} from '@app/api/capture/cdc/CdcDerived';
import {
	nextRepair,
	REPAIR_INTERVAL_MS,
	type RepairDeps,
	type RepairProgress,
	repairCql,
	repairTable,
	resumableProgress,
	TableLocks,
	vanishedTombstoneSql,
} from '@app/api/capture/cdc/CdcRepair';
import {clickHouseType, RowEncoder} from '@app/api/capture/cdc/CdcRows';
import type {CdcTable} from '@app/api/capture/cdc/CdcSchema';
import {settingDifferences} from '@app/api/capture/cdc/CdcSetup';
import type {StateStore} from '@app/api/capture/cdc/CdcState';
import type {ClickHouseHttp} from '@app/api/capture/cdc/ClickHouseHttp';
import cassandra from 'cassandra-driver';
import {afterEach, describe, expect, it, vi} from 'vitest';

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
			'INSERT INTO fluxer_cdc.`notes` (`user_id`, `target_id`, _version, _deleted) SELECT `user_id`, `target_id`, toUInt64(1234), 1 FROM fluxer_cdc.`notes` FINAL WHERE _deleted = 0 AND (_expires_at = 0 OR _expires_at > now64(3)) AND _version < 1234',
		);
	});

	it('leaves rows past their retention window to the table TTL', () => {
		const column = (name: string) => ({name, cqlType: 'bigint', chType: clickHouseType('bigint')});
		const table: CdcTable = {
			layout: {
				name: 'messages',
				primaryKey: ['channel_id', 'bucket', 'message_id'],
				partitionKey: ['channel_id', 'bucket'],
				regular: [],
				multiCell: new Set<string>(),
				defaultTtl: 0,
			},
			columns: [column('bucket'), column('channel_id'), column('message_id')],
			keyColumns: [column('channel_id'), column('bucket'), column('message_id')],
			derived: CDC_DERIVED['messages'],
		};
		expect(vanishedTombstoneSql(table, 1234)).toBe(
			'INSERT INTO fluxer_cdc.`messages` (`channel_id`, `bucket`, `message_id`, _version, _deleted) SELECT `channel_id`, `bucket`, `message_id`, toUInt64(1234), 1 FROM fluxer_cdc.`messages` FINAL WHERE _deleted = 0 AND (_expires_at = 0 OR _expires_at > now64(3)) AND `message_created_at` + INTERVAL 90 DAY > now64(3) AND _version < 1234',
		);
	});
});

describe('resumable repair', () => {
	const column = (name: string) => ({name, cqlType: 'bigint', chType: clickHouseType('bigint')});
	const table: CdcTable = {
		layout: {
			name: 'notes',
			primaryKey: ['user_id'],
			partitionKey: ['user_id'],
			regular: [],
			multiCell: new Set<string>(),
			defaultTtl: 0,
		},
		columns: [column('user_id')],
		keyColumns: [column('user_id')],
	};
	const row = (id: bigint) => ({get: () => id, values: () => [id]});

	function harness(pages: Array<{rows: Array<ReturnType<typeof row>>; pageState?: string}>) {
		const asked: Array<string | undefined> = [];
		const commands: Array<string> = [];
		const writes: Array<Map<string, unknown>> = [];
		const progress: Array<[number, number]> = [];
		const deps: RepairDeps = {
			client: {
				execute: async (_cql: string, _params: Array<unknown>, options: {pageState?: string}) => {
					asked.push(options.pageState);
					return pages[asked.length - 1];
				},
			} as unknown as cassandra.Client,
			clickhouse: {
				insert: async () => undefined,
				command: async (sql: string) => {
					commands.push(sql);
					return {writtenRows: 0};
				},
			} as unknown as ClickHouseHttp,
			encoder: new RowEncoder(cassandra.types),
			state: {write: async (entries: Map<string, unknown>) => void writes.push(entries)} as unknown as StateStore,
			locks: new TableLocks(),
			consistency: 1,
			stopping: () => false,
			onError: () => undefined,
			onRows: () => undefined,
			onProgress: (_name, scanStartMs, read) => void progress.push([scanStartMs, read]),
		};
		return {deps, asked, commands, writes, progress};
	}

	afterEach(() => {
		vi.restoreAllMocks();
	});

	it('continues a saved scan from its page and keeps its start time', async () => {
		const run = harness([{rows: [row(3n)]}]);
		const saved: RepairProgress = {
			scanStartMs: 5000,
			cql: repairCql(table),
			pageState: 'abc',
			marked: true,
			read: 40,
			rows: 30,
		};
		const record = await repairTable(run.deps, table, 1_000_000, {marked: true, resume: saved});
		expect(run.asked).toEqual(['abc']);
		expect(record).toMatchObject({scanStartMs: 5000, rows: 31});
		expect(run.commands[0]).toContain('_version < 5000000');
		expect(run.progress.at(-1)).toEqual([5000, 41]);
		expect(run.writes.at(-1)?.get('repair_progress:notes')).toBeNull();
	});

	it('saves its place while it scans', async () => {
		let now = 1_000_000;
		vi.spyOn(Date, 'now').mockImplementation(() => {
			now += 20_000;
			return now;
		});
		const run = harness([{rows: [row(1n)], pageState: 'p1'}, {rows: [row(2n)], pageState: 'p2'}, {rows: [row(3n)]}]);
		await repairTable(run.deps, table, 1_000_000, {marked: false});
		const saved = run.writes
			.map((entries) => entries.get('repair_progress:notes') as RepairProgress | null | undefined)
			.filter((entry): entry is RepairProgress => entry !== null && entry !== undefined);
		expect(saved.map((entry) => entry.pageState)).toEqual(['p1', 'p2']);
		expect(saved[1]).toMatchObject({read: 2, marked: false, cql: repairCql(table)});
		expect(run.asked).toEqual([undefined, 'p1', 'p2']);
	});

	it('starts over when the table read changed since the scan was saved', () => {
		const saved: RepairProgress = {scanStartMs: 1, cql: 'SELECT old', pageState: 'x', marked: true, read: 1, rows: 1};
		expect(resumableProgress(saved, table)).toBeUndefined();
		expect(resumableProgress({...saved, cql: repairCql(table)}, table)).toBeDefined();
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
