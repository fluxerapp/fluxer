// SPDX-License-Identifier: AGPL-3.0-or-later

import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';
import '../Tables';
import {getTableMetadata} from './CassandraMetaRegistry';
import {defineTable} from './CassandraTableDsl';
import type {CassandraParams, PreparedQuery} from './CassandraTypes';
import {
	buildCandidatePlan,
	type CandidatePlan,
	keyFromColumns,
	matchesWhere,
	POSTGRES_KV_MIGRATION_TABLE,
} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;

const SEPARATOR = '\u001f';

const TRICKY_STRINGS = [
	'',
	'a',
	'a"b',
	'a\\b',
	'a\u0001b',
	'a\u001fb',
	'a b',
	'a\u007fb',
	'\u{1f600}',
	'\ufffd',
	'a\u0000b',
	'b',
];

function byteCompare(left: string, right: string): number {
	return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function withinRange(rowKey: string, lowerBound: string, upperBound: string): boolean {
	return byteCompare(rowKey, lowerBound) >= 0 && byteCompare(rowKey, upperBound) < 0;
}

function rangeBounds(plan: CandidatePlan): Array<[string, string]> {
	if (plan.kind === 'ranges') return plan.lowerBounds.map((lower, index) => [lower, plan.upperBounds[index]!]);
	if (plan.kind === 'rangeGroups') {
		return plan.groups.flatMap((group) =>
			group.lowerBounds.map((lower, index): [string, string] => [lower, group.upperBounds[index]!]),
		);
	}
	return [];
}

function planAccepts(plan: CandidatePlan, rowKey: string, partitionKey: string): boolean {
	switch (plan.kind) {
		case 'none':
			return false;
		case 'rowKeys':
			return plan.rowKeys.includes(rowKey);
		case 'range':
			return withinRange(rowKey, plan.lowerBound, plan.upperBound);
		case 'ranges':
		case 'rangeGroups':
			return rangeBounds(plan).some(([lower, upper]) => withinRange(rowKey, lower, upper));
		case 'partitionKeys':
			return plan.partitionKeys.includes(partitionKey);
		case 'scan':
			return true;
	}
}

function matchingRangeCount(plan: CandidatePlan, rowKey: string): number {
	return rangeBounds(plan).filter(([lower, upper]) => withinRange(rowKey, lower, upper)).length;
}

function expectKeyShape(key: string, separators: number): void {
	let seen = 0;
	for (const byte of Buffer.from(key, 'utf8')) {
		if (byte === 0x1f) {
			seen += 1;
			continue;
		}
		expect(byte).toBeGreaterThanOrEqual(0x20);
	}
	expect(seen).toBe(separators);
}

function planOf(query: PreparedQuery) {
	const meta = query.kvMeta;
	if (!meta) {
		throw new Error('Query is missing kv metadata');
	}
	return {meta, plan: buildCandidatePlan(meta, query.params)};
}

function checkQuery(query: PreparedQuery, rows: Array<Row>): CandidatePlan {
	const {meta, plan} = planOf(query);
	const primaryKey = meta.table.primaryKey as ReadonlyArray<string>;
	const partitionKey = meta.table.partitionKey as ReadonlyArray<string>;
	const accepted: Array<Row> = [];
	const matched: Array<Row> = [];
	for (const row of rows) {
		const rowKey = keyFromColumns(primaryKey, row);
		expectKeyShape(rowKey, primaryKey.length - 1);
		expect(matchingRangeCount(plan.candidates, rowKey)).toBeLessThanOrEqual(1);
		if (planAccepts(plan.candidates, rowKey, keyFromColumns(partitionKey, row))) accepted.push(row);
		if (matchesWhere(row, meta.where, query.params)) matched.push(row);
	}
	for (const row of matched) {
		expect(accepted).toContain(row);
	}
	if (plan.exact) {
		expect(accepted).toEqual(matched);
	}
	return plan.candidates;
}

interface SingleRow {
	a: string;
	payload: string;
}

const SingleKey = defineTable<SingleRow, 'a'>({
	name: 'kv_plan_single',
	columns: ['a', 'payload'],
	primaryKey: ['a'],
});

const singleRows: Array<Row> = TRICKY_STRINGS.map((a) => ({a, payload: 'p'}));

interface PairRow {
	a: string;
	b: bigint;
	payload: string;
}

const PairKey = defineTable<PairRow, 'a' | 'b'>({
	name: 'kv_plan_pair',
	columns: ['a', 'b', 'payload'],
	primaryKey: ['a', 'b'],
});

const pairRows: Array<Row> = TRICKY_STRINGS.flatMap((a) =>
	[-1n, 0n, 9n, 10n, 1000n].map((b) => ({a, b, payload: 'p'})),
);

interface TripleRow {
	a: string;
	b: bigint;
	c: boolean;
	payload: string;
}

const TripleKey = defineTable<TripleRow, 'a' | 'b' | 'c'>({
	name: 'kv_plan_triple',
	columns: ['a', 'b', 'c', 'payload'],
	primaryKey: ['a', 'b', 'c'],
});

const tripleRows: Array<Row> = TRICKY_STRINGS.flatMap((a) =>
	[9n, 10n].flatMap((b) => [true, false].map((c) => ({a, b, c, payload: 'p'}))),
);

interface WideRow {
	a: string;
	b: bigint;
	c: boolean;
	d: Date | null;
	e: Buffer;
	f: number;
	payload: string;
}

const WideKey = defineTable<WideRow, 'a' | 'b' | 'c' | 'd' | 'e' | 'f'>({
	name: 'kv_plan_wide',
	columns: ['a', 'b', 'c', 'd', 'e', 'f', 'payload'],
	primaryKey: ['a', 'b', 'c', 'd', 'e', 'f'],
});

const wideRows: Array<Row> = ['a', 'a"b', '\u{1f600}'].flatMap((a) =>
	[9n, 10n].flatMap((b) =>
		[true, false].flatMap((c) =>
			[null, new Date('2020-01-01T00:00:00.000Z')].flatMap((d) =>
				[Buffer.from('one'), Buffer.from([0x00, 0x1f, 0xff])].flatMap((e) =>
					[1, 2].map((f) => ({a, b, c, d, e, f, payload: 'p'})),
				),
			),
		),
	),
);

interface PartitionedRow {
	a: string;
	b: string;
	payload: string;
}

const PartitionedKey = defineTable<PartitionedRow, 'a' | 'b', 'b'>({
	name: 'kv_plan_partitioned',
	columns: ['a', 'b', 'payload'],
	primaryKey: ['a', 'b'],
	partitionKey: ['b'],
});

const partitionedRows: Array<Row> = TRICKY_STRINGS.flatMap((a) => TRICKY_STRINGS.map((b) => ({a, b, payload: 'p'})));

describe('PostgresKvQueryExecutor candidate planner', () => {
	it('pins a single-column primary key by equality', () => {
		for (const a of TRICKY_STRINGS) {
			expect(checkQuery(SingleKey.select({where: SingleKey.where.eq('a')}).bind({a}), singleRows).kind).toBe('rowKeys');
		}
	});

	it('pins a single-column primary key by IN', () => {
		const query = SingleKey.select({where: SingleKey.where.in('a', 'ids')}).bind({
			ids: ['a', 'a\u0001b', 'a"b', 'a'],
		});
		expect(checkQuery(query, singleRows).kind).toBe('rowKeys');
	});

	it('returns nothing for an empty IN list', () => {
		for (const params of [{ids: []}, {ids: new Set<string>()}, {}]) {
			const query = SingleKey.select({where: SingleKey.where.in('a', 'ids')}).bind(params as CassandraParams);
			expect(checkQuery(query, singleRows).kind).toBe('none');
		}
	});

	it('ranges over the leading column of a composite primary key', () => {
		for (const a of TRICKY_STRINGS) {
			expect(checkQuery(PairKey.select({where: PairKey.where.eq('a')}).bind({a}), pairRows).kind).toBe('range');
		}
	});

	it('ranges over the leading column when a comparison follows', () => {
		const query = PairKey.select({where: [PairKey.where.eq('a'), PairKey.where.lt('b')]}).bind({
			a: 'a\u0001b',
			b: 10n,
		});
		expect(checkQuery(query, pairRows).kind).toBe('range');
	});

	it('emits one range per value when the leading column uses IN', () => {
		const query = PairKey.select({where: PairKey.where.in('a', 'ids')}).bind({
			ids: ['a', 'a\u0001b', 'a\u001fb', 'a'],
		});
		expect(checkQuery(query, pairRows).kind).toBe('ranges');
	});

	it('pins the full primary key across a cartesian product of IN lists', () => {
		const query = PairKey.select({where: [PairKey.where.in('a', 'ids'), PairKey.where.in('b', 'bs')]}).bind({
			ids: ['a', 'a\u007fb'],
			bs: [9n, 10n],
		});
		expect(checkQuery(query, pairRows).kind).toBe('rowKeys');
	});

	it('ranges over a two-column prefix of a three-column primary key', () => {
		for (const a of TRICKY_STRINGS) {
			const query = TripleKey.select({where: [TripleKey.where.eq('a'), TripleKey.where.eq('b')]}).bind({a, b: 9n});
			expect(checkQuery(query, tripleRows).kind).toBe('range');
		}
	});

	it('falls back to a scan when the leading column is unpinned', () => {
		const query = TripleKey.select({where: [TripleKey.where.eq('b'), TripleKey.where.eq('c')]}).bind({b: 9n, c: true});
		expect(checkQuery(query, tripleRows).kind).toBe('scan');
	});

	it('pins every column of a six-column primary key', () => {
		const query = WideKey.select({
			where: [
				WideKey.where.eq('a'),
				WideKey.where.eq('b'),
				WideKey.where.eq('c'),
				WideKey.where.eq('d'),
				WideKey.where.eq('e'),
				WideKey.where.eq('f'),
			],
		}).bind({a: '\u{1f600}', b: 10n, c: false, d: null, e: Buffer.from([0x00, 0x1f, 0xff]), f: 2});
		expect(checkQuery(query, wideRows).kind).toBe('rowKeys');
	});

	it('stops at the first gap in the primary key', () => {
		const query = WideKey.select({
			where: [WideKey.where.eq('a'), WideKey.where.eq('b'), WideKey.where.eq('f')],
		}).bind({a: 'a', b: 9n, f: 1});
		expect(checkQuery(query, wideRows).kind).toBe('range');
	});

	it('uses the partition key when it is not a prefix of the primary key', () => {
		for (const b of TRICKY_STRINGS) {
			const query = PartitionedKey.select({where: PartitionedKey.where.eq('b')}).bind({b});
			expect(checkQuery(query, partitionedRows).kind).toBe('partitionKeys');
		}
	});

	it('uses the partition key for an IN on the partition column', () => {
		const query = PartitionedKey.select({where: PartitionedKey.where.in('b', 'bs')}).bind({
			bs: new Set(['a', 'a\u0001b', 'a\\b']),
		});
		expect(checkQuery(query, partitionedRows).kind).toBe('partitionKeys');
	});

	it('scans a table with no predicate', () => {
		expect(checkQuery(SingleKey.select().bind({}), singleRows).kind).toBe('scan');
	});
});

describe('PostgresKvQueryExecutor plan exactness', () => {
	it('is exact when the key predicate consumes every clause', () => {
		expect(planOf(PairKey.select({where: PairKey.where.eq('a')}).bind({a: 'a'})).plan.exact).toBe(true);
		expect(planOf(SingleKey.select().bind({})).plan.exact).toBe(true);
		expect(planOf(PartitionedKey.select({where: PartitionedKey.where.eq('b')}).bind({b: 'a'})).plan.exact).toBe(true);
	});

	it('is inexact when a clause is left for the JavaScript filter', () => {
		const bounded = PairKey.select({where: [PairKey.where.eq('a'), PairKey.where.lt('b')]}).bind({a: 'a', b: 9n});
		expect(planOf(bounded).plan.exact).toBe(false);
		expect(planOf(TripleKey.select({where: TripleKey.where.eq('b')}).bind({b: 9n})).plan.exact).toBe(false);
	});

	it('is inexact when a limit or an order is present', () => {
		expect(planOf(PairKey.select({where: PairKey.where.eq('a'), limit: 5}).bind({a: 'a'})).plan.exact).toBe(false);
		const ordered = PairKey.select({where: PairKey.where.eq('a'), orderBy: {col: 'b'}}).bind({a: 'a'});
		expect(planOf(ordered).plan.exact).toBe(false);
	});

	it('is inexact when a pinned value has no stable key encoding', () => {
		expect(planOf(SingleKey.select({where: SingleKey.where.eq('a')}).bind({a: {}})).plan.exact).toBe(false);
		expect(planOf(SingleKey.select({where: SingleKey.where.eq('a')}).bind({a: Number.NaN})).plan.exact).toBe(false);
	});

	it('is exact for an empty IN list regardless of the other clauses', () => {
		const query = PairKey.select({where: [PairKey.where.in('a', 'ids'), PairKey.where.lt('b')], limit: 3}).bind({
			ids: [],
			b: 9n,
		});
		expect(planOf(query).plan.exact).toBe(true);
	});
});

describe('PostgresKvQueryExecutor plan totality', () => {
	it('leaves a column unpinned when its value has no key encoding at all', () => {
		const invalid = PairKey.select({where: PairKey.where.eq('a')}).bind({a: new Date('nope')});
		const {plan} = planOf(invalid);
		expect(plan.candidates.kind).toBe('scan');
		expect(plan.exact).toBe(false);
	});

	it('leaves a trailing column unpinned without losing the leading prefix', () => {
		const query = PairKey.select({where: [PairKey.where.eq('a'), PairKey.where.eq('b')]}).bind({
			a: 'a',
			b: new Date('nope') as never,
		});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('range');
		expect(plan.exact).toBe(false);
	});

	it('does not treat a scalar bound to an IN parameter as an iterable', () => {
		const query = SingleKey.select({where: SingleKey.where.in('a', 'ids')}).bind({ids: 'ab' as never});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('scan');
		expect(plan.exact).toBe(false);
	});
});

describe('PostgresKvQueryExecutor plan size caps', () => {
	it('keeps a full primary key product as row keys rather than demoting it to ranges', () => {
		const ids = Array.from({length: 30}, (_, i) => `id${i}`);
		const seqs = Array.from({length: 30}, (_, i) => BigInt(i));
		const query = PairKey.select({where: [PairKey.where.in('a', 'ids'), PairKey.where.in('b', 'seqs')]}).bind({
			ids,
			seqs,
		});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('rowKeys');
		expect(plan.exact).toBe(true);
		if (plan.candidates.kind !== 'rowKeys') return;
		expect(plan.candidates.rowKeys.length).toBe(900);
	});

	it('chunks the prefix ranges into groups instead of degrading to a scan', () => {
		const under = TripleKey.select({where: TripleKey.where.in('a', 'ids')}).bind({
			ids: Array.from({length: 256}, (_, i) => `id${i}`),
		});
		expect(planOf(under).plan.candidates.kind).toBe('ranges');
		const over = TripleKey.select({where: TripleKey.where.in('a', 'ids')}).bind({
			ids: Array.from({length: 700}, (_, i) => `id${i}`),
		});
		const {plan} = planOf(over);
		expect(plan.candidates.kind).toBe('rangeGroups');
		expect(plan.exact).toBe(true);
		if (plan.candidates.kind !== 'rangeGroups') return;
		expect(plan.candidates.groups.map((group) => group.lowerBounds.length)).toEqual([256, 256, 188]);
		expect(plan.candidates.groups.map((group) => group.upperBounds.length)).toEqual([256, 256, 188]);
	});

	it('brackets exactly the matching rows once the prefix ranges are chunked', () => {
		const ids = [...TRICKY_STRINGS, ...Array.from({length: 260}, (_, i) => `id${i}`)];
		const query = TripleKey.select({where: TripleKey.where.in('a', 'ids')}).bind({ids});
		expect(checkQuery(query, tripleRows).kind).toBe('rangeGroups');
	});

	it('falls back to a scan when even the leading column blows the combination bound', () => {
		const query = TripleKey.select({where: TripleKey.where.in('a', 'ids')}).bind({
			ids: Array.from({length: 32_769}, (_, i) => `id${i}`),
		});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('scan');
		expect(plan.exact).toBe(false);
	});

	it('applies the cap to distinct IN values rather than to duplicates', () => {
		const query = TripleKey.select({where: TripleKey.where.in('a', 'ids')}).bind({
			ids: Array.from({length: 300}, (_, i) => `id${i % 50}`),
		});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('ranges');
		if (plan.candidates.kind !== 'ranges') return;
		expect(plan.candidates.lowerBounds.length).toBe(50);
	});

	it('applies the row key combination bound to distinct products too', () => {
		const query = PairKey.select({where: [PairKey.where.in('a', 'ids'), PairKey.where.in('b', 'seqs')]}).bind({
			ids: Array.from({length: 200}, (_, i) => `id${i % 50}`),
			seqs: Array.from({length: 200}, (_, i) => BigInt(i % 50)),
		});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('rowKeys');
		if (plan.candidates.kind !== 'rowKeys') return;
		expect(plan.candidates.rowKeys.length).toBe(2500);
	});

	it('enforces the cap even when the trailing pinned column is an equality', () => {
		const ids = Array.from({length: 40}, (_, i) => `id${i}`);
		const query = WideKey.select({
			where: [WideKey.where.in('a', 'ids'), WideKey.where.in('b', 'seqs'), WideKey.where.eq('c')],
		}).bind({ids, seqs: Array.from({length: 40}, (_, i) => BigInt(i)), c: true});
		const {plan} = planOf(query);
		expect(plan.candidates.kind).toBe('ranges');
		expect(plan.exact).toBe(false);
		if (plan.candidates.kind !== 'ranges') return;
		expect(plan.candidates.lowerBounds.length).toBe(40);
	});
});

describe('PostgresKvQueryExecutor prefix bounds', () => {
	it('brackets exactly the rows sharing the pinned prefix', () => {
		const {plan} = planOf(PairKey.select({where: PairKey.where.eq('a')}).bind({a: 'a'}));
		expect(plan.candidates.kind).toBe('range');
		if (plan.candidates.kind !== 'range') return;
		expect(plan.candidates.lowerBound).toBe(`"a"${SEPARATOR}`);
		expect(plan.candidates.upperBound).toBe('"a" ');
		expect(byteCompare(plan.candidates.lowerBound, plan.candidates.upperBound)).toBeLessThan(0);
	});
});

describe('PostgresKvQueryExecutor key equality', () => {
	const eqWhere = [{kind: 'eq', col: 'a', param: 'a'}] as never;
	const corpus: Array<[string, unknown]> = [
		['null', null],
		['undefined', undefined],
		['empty-string', ''],
		['date-string', '2020-01-01'],
		['days-string', '18262'],
		['zero', 0],
		['neg-zero', -0],
		['number', 18262],
		['bigint', 18262n],
		['true', true],
		['false', false],
		['date', new Date('2020-01-01T00:00:00.000Z')],
		['buffer', Buffer.from('2020-01-01')],
		['local-date', cassandra.types.LocalDate.fromString('2020-01-01')],
		['other-local-date', cassandra.types.LocalDate.fromString('2021-06-05')],
	];

	it('accepts an equality exactly when both sides share a row key', () => {
		const deltas: Array<string> = [];
		for (const [storedName, stored] of corpus) {
			for (const [paramName, param] of corpus) {
				const accepted = matchesWhere({a: stored}, eqWhere, {a: param} as CassandraParams);
				const sameKey = keyFromColumns(['a'], {a: stored}) === keyFromColumns(['a'], {a: param});
				if (accepted !== sameKey)
					deltas.push(`stored=${storedName} param=${paramName} match=${accepted} key=${sameKey}`);
			}
		}
		expect(deltas).toEqual([]);
	});

	it('does not equate a LocalDate with its string rendering', () => {
		const localDate = cassandra.types.LocalDate.fromString('2020-01-01');
		expect(matchesWhere({a: localDate}, eqWhere, {a: localDate} as CassandraParams)).toBe(true);
		expect(matchesWhere({a: localDate}, eqWhere, {a: '2020-01-01'} as CassandraParams)).toBe(false);
		expect(matchesWhere({a: '2020-01-01'}, eqWhere, {a: localDate} as CassandraParams)).toBe(false);
	});
});

describe('PostgresKvQueryExecutor migration bookkeeping', () => {
	it('reserves a table name that no logical table can claim', () => {
		expect(POSTGRES_KV_MIGRATION_TABLE.startsWith('__')).toBe(true);
		expect(getTableMetadata(POSTGRES_KV_MIGRATION_TABLE)).toBeUndefined();
	});
});
