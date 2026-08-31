// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {
	getDefaultPostgresClient,
	type IPostgresClient,
	initPostgres,
	type PostgresQueryable,
	shutdownPostgres,
} from '@pkgs/postgres/src/Client';
import cassandra from 'cassandra-driver';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';
import * as Tables from '../Tables';
import {startDockerContainer} from '../test/DockerTestContainer';
import {LegacyPostgresKvQueryExecutor} from './__testref__/LegacyPostgresKvQueryExecutor';
import {defineTable} from './CassandraTableDsl';
import type {CassandraParam, CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from './CassandraTypes';
import {buildCandidatePlan, ensurePostgresKvSchema, PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;
type AnyMeta = KvQueryMeta<Row>;
type AnyWhere = WhereExpr<Row>;
type AnyExecutor = LegacyPostgresKvQueryExecutor | PostgresKvQueryExecutor;

interface Shape {
	name: string;
	meta: AnyMeta;
	params: CassandraParams;
}

interface Outcome {
	fingerprint: string;
	rowsRead: number;
	threw: string | null;
}

interface Mismatch {
	kind: 'rowset' | 'order' | 'throw' | 'rowsread' | 'state';
	table: string;
	shape: string;
	legacy: string;
	next: string;
}

const LEGACY_TABLE = 'kv_legacy';
const NEXT_TABLE = 'kv_next';
const SEED_TABLE = 'kv_seed';
const ICU_TABLE = 'kv_icu';
const ICU_COLLATION = 'kvdiff_icu';
const MAX_ROWS_PER_TABLE = 24;
const POSTGRES_IMAGE = 'postgres:16-alpine';
const CONTAINER = `fluxer-kvdiff-${process.pid.toString(36)}-${Date.now().toString(36)}`;

const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

const BIGINT_DOMAIN: ReadonlyArray<CassandraParam> = [1n, 2n, 9n, 10n, 11n, 100n, -1n, 99999999999999999999n, 0n];
const NUMBER_DOMAIN: ReadonlyArray<CassandraParam> = [0, 1, 2, 9, 10, 11, 100, -1, 1e21];
const STRING_DOMAIN: ReadonlyArray<CassandraParam> = [
	'',
	'a',
	'b',
	'a"b',
	'a\\b',
	'a\u0001b',
	'a\u001fb',
	'a b',
	'a\u007fb',
	'\u{1f600}',
	'\ufffd',
	'z',
];
const DATE_DOMAIN: ReadonlyArray<CassandraParam> = [
	new Date(0),
	new Date('2020-01-02T03:04:05.006Z'),
	new Date('2021-06-07T08:09:10.011Z'),
	new Date('1969-12-31T23:59:59.999Z'),
	new Date('2999-01-01T00:00:00.000Z'),
];
const BUFFER_DOMAIN: ReadonlyArray<CassandraParam> = [
	Buffer.alloc(0),
	Buffer.from('a', 'utf8'),
	Buffer.from([0x00]),
	Buffer.from([0x1f]),
	Buffer.from([0x20]),
	Buffer.from('\u{1f600}', 'utf8'),
];
const BOOLEAN_DOMAIN: ReadonlyArray<CassandraParam> = [true, false];
const NULLABLE_STRING_DOMAIN: ReadonlyArray<CassandraParam> = [null, 'a', 'b', '', 'a\u001fb'];
const LOCAL_DATE_DOMAIN: ReadonlyArray<CassandraParam> = [
	cassandra.types.LocalDate.fromString('2020-01-01'),
	cassandra.types.LocalDate.fromString('2020-01-02'),
	cassandra.types.LocalDate.fromString('1999-12-31'),
];

const KEY_DOMAINS: ReadonlyArray<ReadonlyArray<CassandraParam>> = [
	BIGINT_DOMAIN,
	STRING_DOMAIN,
	NUMBER_DOMAIN,
	DATE_DOMAIN,
	BUFFER_DOMAIN,
	BOOLEAN_DOMAIN,
	NULLABLE_STRING_DOMAIN,
	LOCAL_DATE_DOMAIN,
];

const WILD_DOMAIN: ReadonlyArray<CassandraParam> = [
	null,
	0,
	1,
	42,
	-1,
	1.5,
	'value',
	'',
	'a"b\\c\u001f',
	true,
	false,
	5n,
	new Date('2022-02-02T02:02:02.002Z'),
	Buffer.from('xy', 'utf8'),
	new Set([1, 'a']),
	new Map<unknown, unknown>([['k', 'v']]),
	[1, 2, 3],
	{a: 1, b: null},
	cassandra.types.LocalDate.fromString('2020-03-04'),
	Number.NaN,
	Number.POSITIVE_INFINITY,
];

function hashString(value: string): number {
	let hash = 2166136261;
	for (let index = 0; index < value.length; index += 1) {
		hash ^= value.charCodeAt(index);
		hash = Math.imul(hash, 16777619);
	}
	return hash >>> 0;
}

function keyDomain(tableName: string, column: string): ReadonlyArray<CassandraParam> {
	return KEY_DOMAINS[hashString(`${tableName}.${column}`) % KEY_DOMAINS.length]!;
}

function sortableDomain(tableName: string, column: string): ReadonlyArray<CassandraParam> {
	return KEY_DOMAINS[hashString(`sort:${tableName}.${column}`) % KEY_DOMAINS.length]!;
}

function wildValue(tableName: string, column: string, index: number): CassandraParam {
	return WILD_DOMAIN[(hashString(`${tableName}#${column}`) + index * 5) % WILD_DOMAIN.length]!;
}

function canonical(value: unknown): unknown {
	if (value === undefined) return {__c: 'undefined'};
	if (value === null) return null;
	if (typeof value === 'bigint') return {__c: 'bigint', v: value.toString()};
	if (typeof value === 'number') {
		if (Number.isNaN(value)) return {__c: 'nan'};
		if (!Number.isFinite(value)) return {__c: 'inf', v: value > 0 ? 1 : -1};
		return Object.is(value, -0) ? {__c: 'negzero'} : value;
	}
	if (value instanceof Date) {
		return {__c: 'date', v: Number.isNaN(value.getTime()) ? 'invalid' : value.toISOString()};
	}
	if (Buffer.isBuffer(value)) return {__c: 'buffer', v: value.toString('base64')};
	if (value instanceof Set) return {__c: 'set', v: [...value.values()].map(canonical)};
	if (value instanceof Map) {
		return {__c: 'map', v: [...value.entries()].map(([key, entry]) => [canonical(key), canonical(entry)])};
	}
	if (typeof value === 'object' && value.constructor?.name === 'LocalDate') {
		return {__c: 'localdate', v: String(value)};
	}
	if (Array.isArray(value)) return value.map(canonical);
	if (typeof value === 'object') {
		const source = value as Record<string, unknown>;
		return {
			__c: 'obj',
			v: Object.keys(source)
				.sort()
				.map((key) => [key, canonical(source[key])]),
		};
	}
	return value;
}

function fingerprint(rows: ReadonlyArray<unknown>): string {
	return JSON.stringify(rows.map(canonical));
}

function multisetFingerprint(rows: ReadonlyArray<unknown>): string {
	return JSON.stringify(rows.map((row) => JSON.stringify(canonical(row))).sort());
}

class CountingClient implements IPostgresClient {
	rowsRead = 0;
	queries = 0;

	constructor(
		private readonly inner: IPostgresClient,
		private readonly table: string,
	) {}

	async query<T extends Record<string, unknown>>(text: string, values: Array<unknown> = []) {
		const result = await this.inner.query(text, values);
		this.record(text, result.rows.length);
		return result as unknown as Awaited<ReturnType<IPostgresClient['query']>> & {rows: Array<T>};
	}

	private record(text: string, rows: number): void {
		this.queries += 1;
		if (/^\s*SELECT/iu.test(text) && text.includes('row_data')) this.rowsRead += rows;
	}

	async connect(): Promise<void> {
		await this.inner.connect();
	}

	async shutdown(): Promise<void> {}

	isConnected(): boolean {
		return this.inner.isConnected();
	}

	async transaction<T>(fn: (client: PostgresQueryable) => Promise<T>): Promise<T> {
		return this.inner.transaction(async (db) =>
			fn({
				query: async <T extends Record<string, unknown>>(text: string, values?: Array<unknown>) => {
					const result = await db.query(text, values);
					this.record(text, result.rows.length);
					return result as unknown as Awaited<ReturnType<PostgresQueryable['query']>> & {rows: Array<T>};
				},
			}),
		);
	}

	kvTable(): string {
		return this.table;
	}

	reset(): void {
		this.rowsRead = 0;
		this.queries = 0;
	}
}

function tableSpecs(): Array<KvTableSpec<Row>> {
	const seen = new Set<string>();
	const specs: Array<KvTableSpec<Row>> = [];
	for (const value of Object.values(Tables as Record<string, unknown>)) {
		if (!value || typeof value !== 'object') continue;
		const candidate = value as Partial<KvTableSpec<Row>> & {selectCql?: unknown};
		if (typeof candidate.name !== 'string') continue;
		if (typeof candidate.selectCql !== 'function') continue;
		if (!Array.isArray(candidate.columns) || !Array.isArray(candidate.primaryKey)) continue;
		if (!Array.isArray(candidate.partitionKey)) continue;
		if (seen.has(candidate.name)) continue;
		seen.add(candidate.name);
		specs.push({
			name: candidate.name,
			columns: candidate.columns as ReadonlyArray<string>,
			primaryKey: candidate.primaryKey as ReadonlyArray<string>,
			partitionKey: candidate.partitionKey as ReadonlyArray<string>,
		});
	}
	return specs.sort((left, right) => (left.name < right.name ? -1 : 1));
}

function keyDomains(spec: KvTableSpec<Row>): Array<ReadonlyArray<CassandraParam>> {
	return (spec.primaryKey as ReadonlyArray<string>).map((column, index) => {
		const domain = keyDomain(spec.name, column);
		const size = index === 0 ? 4 : index === 1 ? 3 : 2;
		return domain.slice(0, Math.min(size, domain.length));
	});
}

function sortableColumn(spec: KvTableSpec<Row>): string {
	const primaryKey = spec.primaryKey as ReadonlyArray<string>;
	const nonKey = (spec.columns as ReadonlyArray<string>).filter((column) => !primaryKey.includes(column));
	return nonKey[0] ?? primaryKey[primaryKey.length - 1]!;
}

function buildRows(spec: KvTableSpec<Row>): Array<Row> {
	const primaryKey = spec.primaryKey as ReadonlyArray<string>;
	const domains = keyDomains(spec);
	let combos: Array<Array<CassandraParam>> = [[]];
	for (const domain of domains) {
		const next: Array<Array<CassandraParam>> = [];
		for (const combo of combos) {
			for (const value of domain) next.push([...combo, value]);
		}
		combos = next.slice(0, MAX_ROWS_PER_TABLE * 4);
	}
	combos = combos.slice(0, MAX_ROWS_PER_TABLE);
	const sortable = sortableColumn(spec);
	const sortValues = sortableDomain(spec.name, sortable);
	return combos.map((combo, index) => {
		const row: Row = {};
		for (const column of spec.columns as ReadonlyArray<string>) {
			row[column] = wildValue(spec.name, column, index);
		}
		row[sortable] = sortValues[index % sortValues.length]!;
		for (let position = 0; position < primaryKey.length; position += 1) {
			row[primaryKey[position]!] = combo[position]!;
		}
		return row;
	});
}

function eq(col: string, param: string): AnyWhere {
	return {kind: 'eq', col, param} as AnyWhere;
}

function inClause(col: string, param: string): AnyWhere {
	return {kind: 'in', col, param} as AnyWhere;
}

function cmp(kind: 'lt' | 'lte' | 'gt' | 'gte' | 'tokenGt', col: string, param: string): AnyWhere {
	return {kind, col, param} as AnyWhere;
}

function selectMeta(spec: KvTableSpec<Row>, where: Array<AnyWhere>, extra: Partial<AnyMeta> = {}): AnyMeta {
	return {action: 'select', table: spec, where, columns: spec.columns, ...extra} as AnyMeta;
}

function buildShapes(spec: KvTableSpec<Row>, rows: Array<Row>): Array<Shape> {
	const primaryKey = spec.primaryKey as ReadonlyArray<string>;
	const partition = spec.partitionKey as ReadonlyArray<string>;
	const columns = spec.columns as ReadonlyArray<string>;
	const domains = keyDomains(spec);
	const sortable = sortableColumn(spec);
	const sample = rows[Math.floor(rows.length / 2)]!;
	const shapes: Array<Shape> = [];
	const pkParams = (row: Row, count: number): CassandraParams => {
		const params: CassandraParams = {};
		for (const column of primaryKey.slice(0, count)) params[column] = row[column] as CassandraParam;
		return params;
	};
	const pkWhere = (count: number): Array<AnyWhere> => primaryKey.slice(0, count).map((column) => eq(column, column));

	shapes.push({
		name: 'pk-eq-full-hit',
		meta: selectMeta(spec, pkWhere(primaryKey.length)),
		params: pkParams(sample, primaryKey.length),
	});
	shapes.push({
		name: 'pk-eq-full-miss',
		meta: selectMeta(spec, pkWhere(primaryKey.length)),
		params: Object.fromEntries(primaryKey.map((column) => [column, '__absent__' as CassandraParam])) as CassandraParams,
	});

	for (let count = 1; count < primaryKey.length; count += 1) {
		shapes.push({name: `prefix-eq-${count}`, meta: selectMeta(spec, pkWhere(count)), params: pkParams(sample, count)});
		for (const kind of ['lt', 'gt', 'lte', 'gte'] as const) {
			const params = pkParams(sample, count);
			params[primaryKey[count]!] = sample[primaryKey[count]!] as CassandraParam;
			shapes.push({
				name: `prefix-eq-${count}-${kind}`,
				meta: selectMeta(spec, [...pkWhere(count), cmp(kind, primaryKey[count]!, primaryKey[count]!)]),
				params,
			});
		}
	}

	for (let position = 0; position < primaryKey.length; position += 1) {
		const pinColumn = primaryKey[position]!;
		const params = pkParams(sample, position);
		params[pinColumn] = domains[position]!.slice(0, 2) as unknown as CassandraParam;
		shapes.push({
			name: `in-at-${position}`,
			meta: selectMeta(spec, [...pkWhere(position), inClause(pinColumn, pinColumn)]),
			params,
		});
		const dupParams = pkParams(sample, position);
		const first = domains[position]![0]!;
		dupParams[pinColumn] = [first, first, domains[position]![1] ?? first] as unknown as CassandraParam;
		shapes.push({
			name: `in-dup-at-${position}`,
			meta: selectMeta(spec, [...pkWhere(position), inClause(pinColumn, pinColumn)]),
			params: dupParams,
		});
		const emptyParams = pkParams(sample, position);
		emptyParams[pinColumn] = [] as unknown as CassandraParam;
		shapes.push({
			name: `in-empty-at-${position}`,
			meta: selectMeta(spec, [...pkWhere(position), inClause(pinColumn, pinColumn)]),
			params: emptyParams,
		});
	}

	shapes.push({
		name: 'in-set-at-0',
		meta: selectMeta(spec, [inClause(primaryKey[0]!, primaryKey[0]!)]),
		params: {[primaryKey[0]!]: new Set(domains[0]!.slice(0, 2))},
	});

	const inAllParams: CassandraParams = {};
	const inAllWhere = primaryKey.map((column, index) => {
		inAllParams[column] = domains[index]!.slice(0, 2) as unknown as CassandraParam;
		return inClause(column, column);
	});
	shapes.push({name: 'in-all-pk', meta: selectMeta(spec, inAllWhere), params: inAllParams});

	if (partition.length > 0) {
		const partParams: CassandraParams = {};
		const partWhere = partition.map((column) => {
			partParams[column] = sample[column] as CassandraParam;
			return eq(column, column);
		});
		shapes.push({name: 'partition-eq', meta: selectMeta(spec, partWhere), params: partParams});
		shapes.push({
			name: 'partition-in-0',
			meta: selectMeta(spec, [inClause(partition[0]!, partition[0]!), ...partWhere.slice(1)]),
			params: {
				...partParams,
				[partition[0]!]: [sample[partition[0]!], rows[0]![partition[0]!]] as unknown as CassandraParam,
			},
		});
	}

	const nonKey = columns.filter((column) => !primaryKey.includes(column));
	if (nonKey.length > 0) {
		shapes.push({
			name: 'nonkey-eq',
			meta: selectMeta(spec, [eq(nonKey[0]!, nonKey[0]!)]),
			params: {[nonKey[0]!]: sample[nonKey[0]!] as CassandraParam},
		});
		shapes.push({
			name: 'prefix-eq-1-nonkey-eq',
			meta: selectMeta(spec, [...pkWhere(1), eq(nonKey[0]!, nonKey[0]!)]),
			params: {...pkParams(sample, 1), [nonKey[0]!]: sample[nonKey[0]!] as CassandraParam},
		});
	}

	shapes.push({
		name: 'tokenGt-0',
		meta: selectMeta(spec, [cmp('tokenGt', primaryKey[0]!, primaryKey[0]!)]),
		params: {[primaryKey[0]!]: sample[primaryKey[0]!] as CassandraParam},
	});

	if (primaryKey.length >= 2) {
		shapes.push({
			name: 'tupleGt',
			meta: selectMeta(spec, [
				{kind: 'tupleGt', cols: [primaryKey[0]!, primaryKey[1]!], params: [primaryKey[0]!, primaryKey[1]!]} as AnyWhere,
			]),
			params: {
				[primaryKey[0]!]: sample[primaryKey[0]!] as CassandraParam,
				[primaryKey[1]!]: sample[primaryKey[1]!] as CassandraParam,
			},
		});
	}

	shapes.push({name: 'no-where', meta: selectMeta(spec, []), params: {}});

	for (const direction of ['ASC', 'DESC'] as const) {
		shapes.push({
			name: `order-${direction}`,
			meta: selectMeta(spec, [], {orderBy: {col: sortable, direction}}),
			params: {},
		});
		shapes.push({
			name: `prefix-eq-1-order-${direction}`,
			meta: selectMeta(spec, pkWhere(1), {orderBy: {col: sortable, direction}}),
			params: pkParams(sample, 1),
		});
	}

	for (const limit of [1, 3, 1000]) {
		shapes.push({name: `limit-${limit}`, meta: selectMeta(spec, [], {limit}), params: {}});
		shapes.push({
			name: `prefix-eq-1-limit-${limit}`,
			meta: selectMeta(spec, pkWhere(1), {limit}),
			params: pkParams(sample, 1),
		});
	}

	shapes.push({
		name: 'prefix-eq-1-order-limit',
		meta: selectMeta(spec, pkWhere(1), {orderBy: {col: sortable, direction: 'DESC'}, limit: 2}),
		params: pkParams(sample, 1),
	});

	const subset = [...new Set([columns[0]!, columns[columns.length - 1]!, sortable])];
	shapes.push({
		name: 'columns-subset',
		meta: selectMeta(spec, pkWhere(1), {columns: subset as ReadonlyArray<string>}),
		params: pkParams(sample, 1),
	});
	shapes.push({
		name: 'columns-subset-order',
		meta: selectMeta(spec, [], {columns: subset as ReadonlyArray<string>, orderBy: {col: sortable}}),
		params: {},
	});

	return shapes;
}

const COUNT_SHAPE_NAMES = [
	'pk-eq-full-hit',
	'pk-eq-full-miss',
	'prefix-eq-1',
	'in-at-0',
	'in-dup-at-0',
	'in-empty-at-0',
	'in-set-at-0',
	'in-all-pk',
	'partition-eq',
	'partition-in-0',
	'nonkey-eq',
	'prefix-eq-1-nonkey-eq',
	'no-where',
	'tokenGt-0',
	'tupleGt',
];

const DELETE_SHAPE_NAMES = [
	'pk-eq-full-hit',
	'prefix-eq-1',
	'in-at-0',
	'in-dup-at-0',
	'in-empty-at-0',
	'in-all-pk',
	'partition-eq',
	'nonkey-eq',
	'tokenGt-0',
	'no-where',
];

function derivedShapes(
	spec: KvTableSpec<Row>,
	shapes: Array<Shape>,
	action: 'count' | 'delete',
	names: ReadonlyArray<string>,
): Array<Shape> {
	return shapes
		.filter((shape) => names.includes(shape.name))
		.map((shape) => ({
			name: `${action}:${shape.name}`,
			meta: {action, table: spec, where: shape.meta.where} as AnyMeta,
			params: shape.params,
		}));
}

const AdversarialSingle = defineTable<{k: unknown; a: unknown; b: unknown}, 'k'>({
	name: 'kvdiff_adversarial_single',
	columns: ['k', 'a', 'b'],
	primaryKey: ['k'],
});

const AdversarialWide = defineTable<
	{k0: unknown; k1: unknown; k2: unknown; k3: unknown; k4: unknown; k5: unknown; v: unknown},
	'k0' | 'k1' | 'k2' | 'k3' | 'k4' | 'k5'
>({
	name: 'kvdiff_adversarial_wide',
	columns: ['k0', 'k1', 'k2', 'k3', 'k4', 'k5', 'v'],
	primaryKey: ['k0', 'k1', 'k2', 'k3', 'k4', 'k5'],
});

const AdversarialSplit = defineTable<{p: unknown; c: unknown; v: unknown}, 'p' | 'c', 'p'>({
	name: 'kvdiff_adversarial_split',
	columns: ['p', 'c', 'v'],
	primaryKey: ['p', 'c'],
	partitionKey: ['p'],
});

const AdversarialTrailingPartition = defineTable<{p: unknown; c: unknown; v: unknown}, 'p' | 'c', 'c'>({
	name: 'kvdiff_adversarial_trailing',
	columns: ['p', 'c', 'v'],
	primaryKey: ['p', 'c'],
	partitionKey: ['c'],
});

const ADVERSARIAL_KEY_VALUES: ReadonlyArray<CassandraParam> = [
	null,
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
	'2020-01-01',
	1n,
	2n,
	9n,
	10n,
	11n,
	99999999999999999999n,
	0,
	1,
	9,
	10,
	true,
	false,
	new Date('2020-01-01T00:00:00.000Z'),
	Buffer.from([0x1f]),
	Buffer.from([0x20]),
	cassandra.types.LocalDate.fromString('2020-01-01'),
];

const AdversarialRows: Record<string, Array<Row>> = {};

function adversarialCorpus(): void {
	AdversarialRows[AdversarialSingle.name] = ADVERSARIAL_KEY_VALUES.map((value, index) => ({
		k: value,
		a: WILD_DOMAIN[index % WILD_DOMAIN.length]!,
		b: index,
	}));
	const splitRows: Array<Row> = [];
	for (const p of ADVERSARIAL_KEY_VALUES.slice(0, 10)) {
		for (const c of ADVERSARIAL_KEY_VALUES.slice(0, 6)) splitRows.push({p, c, v: `${String(p)}|${String(c)}`});
	}
	AdversarialRows[AdversarialSplit.name] = splitRows;
	AdversarialRows[AdversarialTrailingPartition.name] = splitRows.map((row) => ({...row}));
	const wideRows: Array<Row> = [];
	for (const k0 of ADVERSARIAL_KEY_VALUES.slice(0, 4)) {
		for (const k1 of ADVERSARIAL_KEY_VALUES.slice(6, 9)) {
			for (const k2 of ADVERSARIAL_KEY_VALUES.slice(12, 14)) {
				wideRows.push({k0, k1, k2, k3: null, k4: '', k5: 1n, v: 'x'});
			}
		}
	}
	AdversarialRows[AdversarialWide.name] = wideRows;
}

function adversarialShapes(): Array<Shape> {
	const single = AdversarialSingle as unknown as KvTableSpec<Row>;
	const wide = AdversarialWide as unknown as KvTableSpec<Row>;
	const split = AdversarialSplit as unknown as KvTableSpec<Row>;
	const trailing = AdversarialTrailingPartition as unknown as KvTableSpec<Row>;
	const shapes: Array<Shape> = [];
	for (const value of ADVERSARIAL_KEY_VALUES) {
		shapes.push({name: `single-eq-${String(value)}`, meta: selectMeta(single, [eq('k', 'k')]), params: {k: value}});
		shapes.push({name: `split-eq-p-${String(value)}`, meta: selectMeta(split, [eq('p', 'p')]), params: {p: value}});
		shapes.push({
			name: `split-eq-p-eq-c-${String(value)}`,
			meta: selectMeta(split, [eq('p', 'p'), eq('c', 'c')]),
			params: {p: value, c: value},
		});
		shapes.push({
			name: `trailing-eq-c-${String(value)}`,
			meta: selectMeta(trailing, [eq('c', 'c')]),
			params: {c: value},
		});
		shapes.push({
			name: `trailing-eq-p-${String(value)}`,
			meta: selectMeta(trailing, [eq('p', 'p')]),
			params: {p: value},
		});
	}
	shapes.push({
		name: 'single-eq-localdate-vs-string',
		meta: selectMeta(single, [eq('k', 'k')]),
		params: {k: cassandra.types.LocalDate.fromString('2020-01-01')},
	});
	shapes.push({
		name: 'single-eq-string-vs-localdate',
		meta: selectMeta(single, [eq('k', 'k')]),
		params: {k: '2020-01-01'},
	});
	shapes.push({name: 'single-eq-nan', meta: selectMeta(single, [eq('k', 'k')]), params: {k: Number.NaN}});
	shapes.push({
		name: 'single-eq-infinity',
		meta: selectMeta(single, [eq('k', 'k')]),
		params: {k: Number.POSITIVE_INFINITY},
	});
	shapes.push({name: 'single-eq-null', meta: selectMeta(single, [eq('k', 'k')]), params: {k: null}});
	shapes.push({name: 'single-eq-object', meta: selectMeta(single, [eq('k', 'k')]), params: {k: {a: 1}}});
	shapes.push({name: 'single-eq-set', meta: selectMeta(single, [eq('k', 'k')]), params: {k: new Set([1, 'a'])}});
	shapes.push({
		name: 'single-in-all',
		meta: selectMeta(single, [inClause('k', 'k')]),
		params: {k: [...ADVERSARIAL_KEY_VALUES] as unknown as CassandraParam},
	});
	shapes.push({
		name: 'single-in-empty',
		meta: selectMeta(single, [inClause('k', 'k')]),
		params: {k: [] as unknown as CassandraParam},
	});
	shapes.push({name: 'single-in-empty-set', meta: selectMeta(single, [inClause('k', 'k')]), params: {k: new Set()}});
	shapes.push({
		name: 'single-in-huge',
		meta: selectMeta(single, [inClause('k', 'k')]),
		params: {k: [...Array.from({length: 4000}, (_unused, index) => BigInt(index)), 1n] as unknown as CassandraParam},
	});
	shapes.push({
		name: 'split-in-p-cartesian',
		meta: selectMeta(split, [inClause('p', 'p'), inClause('c', 'c')]),
		params: {
			p: ADVERSARIAL_KEY_VALUES.slice(0, 10) as unknown as CassandraParam,
			c: ADVERSARIAL_KEY_VALUES.slice(0, 6) as unknown as CassandraParam,
		},
	});
	shapes.push({
		name: 'split-in-p-over-cap',
		meta: selectMeta(split, [inClause('p', 'p'), inClause('c', 'c')]),
		params: {
			p: Array.from({length: 40}, (_unused, index) => BigInt(index)) as unknown as CassandraParam,
			c: Array.from({length: 40}, (_unused, index) => BigInt(index)) as unknown as CassandraParam,
		},
	});
	shapes.push({
		name: 'split-eq-p-in-c-empty',
		meta: selectMeta(split, [eq('p', 'p'), inClause('c', 'c')]),
		params: {p: ADVERSARIAL_KEY_VALUES[2]!, c: [] as unknown as CassandraParam},
	});
	shapes.push({
		name: 'wide-prefix-eq-3',
		meta: selectMeta(wide, [eq('k0', 'k0'), eq('k1', 'k1'), eq('k2', 'k2')]),
		params: {k0: ADVERSARIAL_KEY_VALUES[0]!, k1: ADVERSARIAL_KEY_VALUES[6]!, k2: ADVERSARIAL_KEY_VALUES[12]!},
	});
	shapes.push({
		name: 'wide-prefix-eq-1',
		meta: selectMeta(wide, [eq('k0', 'k0')]),
		params: {k0: ADVERSARIAL_KEY_VALUES[0]!},
	});
	shapes.push({
		name: 'wide-gap-k0-k2',
		meta: selectMeta(wide, [eq('k0', 'k0'), eq('k2', 'k2')]),
		params: {k0: ADVERSARIAL_KEY_VALUES[0]!, k2: ADVERSARIAL_KEY_VALUES[12]!},
	});
	shapes.push({
		name: 'wide-full-pk',
		meta: selectMeta(wide, [
			eq('k0', 'k0'),
			eq('k1', 'k1'),
			eq('k2', 'k2'),
			eq('k3', 'k3'),
			eq('k4', 'k4'),
			eq('k5', 'k5'),
		]),
		params: {
			k0: ADVERSARIAL_KEY_VALUES[0]!,
			k1: ADVERSARIAL_KEY_VALUES[6]!,
			k2: ADVERSARIAL_KEY_VALUES[12]!,
			k3: null,
			k4: '',
			k5: 1n,
		},
	});
	shapes.push({
		name: 'trailing-partition-in',
		meta: selectMeta(trailing, [inClause('c', 'c')]),
		params: {c: ADVERSARIAL_KEY_VALUES.slice(0, 4) as unknown as CassandraParam},
	});
	shapes.push({name: 'single-missing-param', meta: selectMeta(single, [eq('k', 'k')]), params: {}});
	shapes.push({name: 'single-in-missing-param', meta: selectMeta(single, [inClause('k', 'k')]), params: {}});
	shapes.push({name: 'single-no-where', meta: selectMeta(single, []), params: {}});
	shapes.push({name: 'split-no-where', meta: selectMeta(split, []), params: {}});
	return shapes;
}

function keysetEligible(shape: Shape): boolean {
	if (shape.meta.action !== 'select') return false;
	if (shape.meta.orderBy !== undefined || shape.meta.limit !== undefined) return false;
	const plan = buildCandidatePlan(shape.meta, shape.params);
	return (
		plan.exact &&
		(plan.candidates.kind === 'none' || plan.candidates.kind === 'range' || plan.candidates.kind === 'scan')
	);
}

const HEAP_ORDERINGS: ReadonlyArray<string> = [
	'row_key COLLATE "C" ASC',
	'row_key COLLATE "C" DESC',
	"md5(row_key || 'a')",
	"md5(row_key || 'b')",
	"md5(row_key || 'c')",
	"md5(row_key || 'd')",
	"md5(row_key || 'e')",
	"md5(row_key || 'f')",
];

const suite = dockerAvailable ? describe : describe.skip;

suite('PostgresKvQueryExecutor differential', () => {
	let raw: IPostgresClient;
	let readLegacyClient: CountingClient;
	let readNextClient: CountingClient;
	let mutLegacyClient: CountingClient;
	let mutNextClient: CountingClient;
	let icuLegacyClient: CountingClient;
	let icuNextClient: CountingClient;
	let readLegacy: LegacyPostgresKvQueryExecutor;
	let readNext: PostgresKvQueryExecutor;
	let mutLegacy: LegacyPostgresKvQueryExecutor;
	let mutNext: PostgresKvQueryExecutor;
	let icuLegacy: LegacyPostgresKvQueryExecutor;
	let icuNext: PostgresKvQueryExecutor;
	const specs = tableSpecs();
	const adversarialSpecs = [
		AdversarialSingle as unknown as KvTableSpec<Row>,
		AdversarialSplit as unknown as KvTableSpec<Row>,
		AdversarialTrailingPartition as unknown as KvTableSpec<Row>,
		AdversarialWide as unknown as KvTableSpec<Row>,
	];
	const corpus = new Map<string, Array<Row>>();
	const shapeCache = new Map<string, Array<Shape>>();
	const unstableShapes: Array<string> = [];
	const traffic = {legacy: 0, next: 0, shapes: 0};
	const keysetOrderDeltas: Array<string> = [];
	const legacyThrowDeltas: Array<string> = [];

	function shapesFor(spec: KvTableSpec<Row>): Array<Shape> {
		const cached = shapeCache.get(spec.name);
		if (cached) return cached;
		const built = buildShapes(spec, corpus.get(spec.name)!);
		shapeCache.set(spec.name, built);
		return built;
	}

	async function freePort(): Promise<number> {
		return new Promise((resolve, reject) => {
			const server = createServer();
			server.on('error', reject);
			server.listen(0, '127.0.0.1', () => {
				const address = server.address();
				if (typeof address === 'string' || address === null) {
					reject(new Error('Could not allocate a port'));
					return;
				}
				const port = address.port;
				server.close(() => resolve(port));
			});
		});
	}

	async function sleep(ms: number): Promise<void> {
		await new Promise((resolve) => setTimeout(resolve, ms));
	}

	async function seedTable(spec: KvTableSpec<Row>, rows: ReadonlyArray<Row>, db: PostgresQueryable): Promise<void> {
		const meta = {action: 'upsert', table: spec} as AnyMeta;
		for (const row of rows) {
			await mutLegacy.executeQuery({cql: `__seed__:${spec.name}`, params: row as CassandraParams, kvMeta: meta}, db);
		}
	}

	async function restoreOne(kv: string, tableName: string, ordering: string): Promise<void> {
		await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [tableName]);
		await raw.query(`INSERT INTO ${kv} SELECT * FROM ${SEED_TABLE} WHERE table_name = $1 ORDER BY ${ordering}`, [
			tableName,
		]);
	}

	async function restoreBoth(tableName: string): Promise<void> {
		await restoreOne(LEGACY_TABLE, tableName, HEAP_ORDERINGS[0]!);
		await restoreOne(NEXT_TABLE, tableName, HEAP_ORDERINGS[0]!);
	}

	async function tableState(kv: string, tableName: string): Promise<string> {
		const result = await raw.query<{
			row_key: string;
			partition_key: string;
			row_data: string;
			expires_at: Date | null;
		}>(
			`SELECT row_key, partition_key, row_data::text AS row_data, expires_at FROM ${kv} WHERE table_name = $1 ORDER BY row_key COLLATE "C"`,
			[tableName],
		);
		return JSON.stringify(
			result.rows.map((row) => [row.row_key, row.partition_key, row.row_data, row.expires_at?.toISOString() ?? null]),
		);
	}

	async function run(executor: AnyExecutor, client: CountingClient, shape: Shape): Promise<Outcome> {
		client.reset();
		try {
			const rows = await executor.executeQuery({
				cql: `__diff__:${shape.name}`,
				params: shape.params,
				kvMeta: shape.meta,
			});
			return {fingerprint: fingerprint(rows), rowsRead: client.rowsRead, threw: null};
		} catch (error) {
			return {fingerprint: '', rowsRead: client.rowsRead, threw: (error as Error).message};
		}
	}

	async function legacyUnderHeapOrder(
		kv: string,
		executor: LegacyPostgresKvQueryExecutor,
		tableName: string,
		shape: Shape,
		ordering: string,
	): Promise<string> {
		await restoreOne(kv, tableName, ordering);
		let observed = '';
		await raw.transaction(async (db) => {
			await db.query('SET LOCAL enable_indexscan = off');
			await db.query('SET LOCAL enable_indexonlyscan = off');
			await db.query('SET LOCAL enable_bitmapscan = off');
			const rows = await executor.executeQuery(
				{cql: `__probe__:${shape.name}`, params: shape.params, kvMeta: shape.meta},
				db,
			);
			observed = fingerprint(rows);
		});
		return observed;
	}

	async function legacyIsOrderUnstable(
		kv: string,
		executor: LegacyPostgresKvQueryExecutor,
		tableName: string,
		shape: Shape,
	): Promise<boolean> {
		const observed = new Set<string>();
		for (const ordering of HEAP_ORDERINGS) {
			observed.add(await legacyUnderHeapOrder(kv, executor, tableName, shape, ordering));
		}
		await restoreOne(kv, tableName, HEAP_ORDERINGS[0]!);
		if (observed.size === 1) return false;
		return true;
	}

	async function compareRead(
		mismatches: Array<Mismatch>,
		kv: string,
		executor: LegacyPostgresKvQueryExecutor,
		tableName: string,
		shape: Shape,
		left: Outcome,
		right: Outcome,
	): Promise<void> {
		if (left.threw !== null && right.threw === null) {
			legacyThrowDeltas.push(`${tableName} ${shape.name}: legacy threw "${left.threw}" -> next ${right.fingerprint}`);
			return;
		}
		if (left.threw === null && right.threw !== null) {
			mismatches.push({
				kind: 'throw',
				table: tableName,
				shape: shape.name,
				legacy: left.fingerprint,
				next: right.threw,
			});
			return;
		}
		if (left.threw !== null && right.threw !== null) {
			if (left.threw !== right.threw) {
				mismatches.push({kind: 'throw', table: tableName, shape: shape.name, legacy: left.threw, next: right.threw});
			}
			return;
		}
		traffic.legacy += left.rowsRead;
		traffic.next += right.rowsRead;
		traffic.shapes += 1;
		if (left.fingerprint === right.fingerprint) {
			if (right.rowsRead > left.rowsRead) {
				mismatches.push({
					kind: 'rowsread',
					table: tableName,
					shape: shape.name,
					legacy: String(left.rowsRead),
					next: String(right.rowsRead),
				});
			}
			return;
		}
		const leftRows = JSON.parse(left.fingerprint) as Array<unknown>;
		const rightRows = JSON.parse(right.fingerprint) as Array<unknown>;
		const sameMultiset = multisetFingerprint(leftRows) === multisetFingerprint(rightRows);
		if (!sameMultiset && leftRows.length !== rightRows.length) {
			mismatches.push({
				kind: 'rowset',
				table: tableName,
				shape: shape.name,
				legacy: left.fingerprint.slice(0, 400),
				next: right.fingerprint.slice(0, 400),
			});
			return;
		}
		if (!(await legacyIsOrderUnstable(kv, executor, tableName, shape))) {
			mismatches.push({
				kind: sameMultiset ? 'order' : 'rowset',
				table: tableName,
				shape: shape.name,
				legacy: left.fingerprint.slice(0, 400),
				next: right.fingerprint.slice(0, 400),
			});
			return;
		}
		unstableShapes.push(`${tableName} ${shape.name}`);
	}

	function report(mismatches: Array<Mismatch>): string {
		const byKind = new Map<string, number>();
		for (const mismatch of mismatches) byKind.set(mismatch.kind, (byKind.get(mismatch.kind) ?? 0) + 1);
		const summary = [...byKind.entries()].map(([kind, count]) => `${kind}=${count}`).join(' ');
		const detail = mismatches
			.slice(0, 25)
			.map(
				(mismatch) =>
					`[${mismatch.kind}] ${mismatch.table} ${mismatch.shape}\n  legacy: ${mismatch.legacy}\n  next:   ${mismatch.next}`,
			)
			.join('\n');
		return `${mismatches.length} mismatches (${summary})\n${detail}`;
	}

	beforeAll(async () => {
		const port = await freePort();
		startDockerContainer([
			'run',
			'-d',
			'--name',
			CONTAINER,
			'-e',
			'POSTGRES_USER=fluxer',
			'-e',
			'POSTGRES_PASSWORD=fluxer',
			'-e',
			'POSTGRES_DB=fluxer',
			'-p',
			`127.0.0.1:${port}:5432`,
			POSTGRES_IMAGE,
			'-c',
			'fsync=off',
			'-c',
			'synchronous_commit=off',
			'-c',
			'full_page_writes=off',
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await sleep(500);
			const probe = spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`, maxConnections: 8});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('Throwaway postgres did not become ready');
		raw = getDefaultPostgresClient();
		readLegacyClient = new CountingClient(raw, NEXT_TABLE);
		readNextClient = new CountingClient(raw, NEXT_TABLE);
		mutLegacyClient = new CountingClient(raw, LEGACY_TABLE);
		mutNextClient = new CountingClient(raw, NEXT_TABLE);
		icuLegacyClient = new CountingClient(raw, ICU_TABLE);
		icuNextClient = new CountingClient(raw, ICU_TABLE);
		await ensurePostgresKvSchema(mutLegacyClient);
		await ensurePostgresKvSchema(mutNextClient);
		await raw.query(`CREATE COLLATION ${ICU_COLLATION} (provider = icu, locale = 'en-US')`);
		await ensurePostgresKvSchema(icuNextClient);
		for (const column of ['row_key', 'partition_key']) {
			await raw.query(`ALTER TABLE ${ICU_TABLE} ALTER COLUMN ${column} TYPE text COLLATE ${ICU_COLLATION}`);
		}
		readLegacy = new LegacyPostgresKvQueryExecutor(readLegacyClient);
		readNext = new PostgresKvQueryExecutor(readNextClient);
		mutLegacy = new LegacyPostgresKvQueryExecutor(mutLegacyClient);
		mutNext = new PostgresKvQueryExecutor(mutNextClient);
		icuLegacy = new LegacyPostgresKvQueryExecutor(icuLegacyClient);
		icuNext = new PostgresKvQueryExecutor(icuNextClient);

		adversarialCorpus();
		for (const spec of specs) corpus.set(spec.name, buildRows(spec));
		for (const spec of adversarialSpecs) corpus.set(spec.name, AdversarialRows[spec.name]!);

		await raw.transaction(async (db) => {
			for (const spec of [...specs, ...adversarialSpecs]) await seedTable(spec, corpus.get(spec.name)!, db);
		});
		await raw.query(
			`UPDATE ${LEGACY_TABLE} SET expires_at = now() - interval '1 hour' WHERE abs(hashtext(row_key)) % 7 = 3`,
		);
		await raw.query(
			`UPDATE ${LEGACY_TABLE} SET expires_at = now() + interval '1 hour' WHERE abs(hashtext(row_key)) % 7 = 5`,
		);
		await raw.query(`CREATE TABLE ${SEED_TABLE} AS TABLE ${LEGACY_TABLE}`);
		await raw.query(`CREATE INDEX ${SEED_TABLE}_table_idx ON ${SEED_TABLE} (table_name)`);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE, ICU_TABLE]) {
			await raw.query(`TRUNCATE ${kv}`);
			await raw.query(`INSERT INTO ${kv} SELECT * FROM ${SEED_TABLE} ORDER BY table_name, row_key COLLATE "C"`);
		}
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('seeds an identical corpus into both kv tables', async () => {
		expect(specs.length).toBeGreaterThan(150);
		const seeded = await raw.query<{count: string}>(`SELECT count(*) AS count FROM ${SEED_TABLE}`);
		expect(Number(seeded.rows[0]!.count)).toBeGreaterThan(specs.length * 4);
		const expired = await raw.query<{count: string}>(
			`SELECT count(*) AS count FROM ${SEED_TABLE} WHERE expires_at IS NOT NULL AND expires_at <= now()`,
		);
		expect(Number(expired.rows[0]!.count)).toBeGreaterThan(0);
		const future = await raw.query<{count: string}>(
			`SELECT count(*) AS count FROM ${SEED_TABLE} WHERE expires_at > now()`,
		);
		expect(Number(future.rows[0]!.count)).toBeGreaterThan(0);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			const drift = await raw.query<{count: string}>(
				`SELECT count(*) AS count FROM (
					SELECT table_name, partition_key, row_key, row_data, expires_at FROM ${kv}
					EXCEPT
					SELECT table_name, partition_key, row_key, row_data, expires_at FROM ${SEED_TABLE}
				) AS diff`,
			);
			expect(drift.rows[0]!.count).toBe('0');
		}
	});

	it('writes byte-identical stored rows through both executors', async () => {
		const mismatches: Array<Mismatch> = [];
		for (const spec of [...specs, ...adversarialSpecs]) {
			const writeName = `w_${spec.name}`;
			const writeMeta = {action: 'upsert', table: {...spec, name: writeName}} as AnyMeta;
			for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
				await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [writeName]);
			}
			for (const row of corpus.get(spec.name)!.slice(0, 4)) {
				await mutLegacy.executeQuery({cql: '__w__', params: row as CassandraParams, kvMeta: writeMeta});
				await mutNext.executeQuery({cql: '__w__', params: row as CassandraParams, kvMeta: writeMeta});
			}
			const left = await tableState(LEGACY_TABLE, writeName);
			const right = await tableState(NEXT_TABLE, writeName);
			if (left !== right) {
				mismatches.push({
					kind: 'state',
					table: spec.name,
					shape: 'upsert',
					legacy: left.slice(0, 400),
					next: right.slice(0, 400),
				});
			}
			for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
				await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [writeName]);
			}
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 900_000);

	it('returns identical select results for every generated shape', async () => {
		const mismatches: Array<Mismatch> = [];
		let shapeCount = 0;
		for (const spec of specs) {
			for (const shape of shapesFor(spec)) {
				shapeCount += 1;
				const left = await run(readLegacy, readLegacyClient, shape);
				const right = await run(readNext, readNextClient, shape);
				await compareRead(mismatches, NEXT_TABLE, readLegacy, spec.name, shape, left, right);
			}
		}
		expect(shapeCount).toBeGreaterThan(3000);
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('returns identical count results for every generated shape', async () => {
		const mismatches: Array<Mismatch> = [];
		for (const spec of specs) {
			for (const shape of derivedShapes(spec, shapesFor(spec), 'count', COUNT_SHAPE_NAMES)) {
				const left = await run(readLegacy, readLegacyClient, shape);
				const right = await run(readNext, readNextClient, shape);
				await compareRead(mismatches, NEXT_TABLE, readLegacy, spec.name, shape, left, right);
			}
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('leaves identical stored state after every generated delete shape', async () => {
		const mismatches: Array<Mismatch> = [];
		for (const spec of specs) {
			for (const shape of derivedShapes(spec, shapesFor(spec), 'delete', DELETE_SHAPE_NAMES)) {
				await restoreBoth(spec.name);
				const left = await run(mutLegacy, mutLegacyClient, shape);
				const right = await run(mutNext, mutNextClient, shape);
				if ((left.threw === null) !== (right.threw === null)) {
					if (left.threw !== null && right.threw === null) {
						legacyThrowDeltas.push(`${spec.name} ${shape.name}: legacy threw "${left.threw}"`);
					} else {
						mismatches.push({
							kind: 'throw',
							table: spec.name,
							shape: shape.name,
							legacy: left.threw ?? 'ok',
							next: right.threw ?? 'ok',
						});
						continue;
					}
				}
				const leftState = await tableState(LEGACY_TABLE, spec.name);
				const rightState = await tableState(NEXT_TABLE, spec.name);
				if (leftState !== rightState) {
					mismatches.push({
						kind: 'state',
						table: spec.name,
						shape: shape.name,
						legacy: leftState.slice(0, 400),
						next: rightState.slice(0, 400),
					});
				}
				traffic.legacy += left.rowsRead;
				traffic.next += right.rowsRead;
				traffic.shapes += 1;
				if (right.rowsRead > left.rowsRead) {
					mismatches.push({
						kind: 'rowsread',
						table: spec.name,
						shape: shape.name,
						legacy: String(left.rowsRead),
						next: String(right.rowsRead),
					});
				}
			}
			await restoreBoth(spec.name);
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('pages over identical row multisets with identical page boundaries', async () => {
		const mismatches: Array<Mismatch> = [];
		for (const spec of specs) {
			const primaryKey = spec.primaryKey as ReadonlyArray<string>;
			const rows = corpus.get(spec.name)!;
			const sample = rows[Math.floor(rows.length / 2)]!;
			const sortable = sortableColumn(spec);
			const pagedShapes: Array<Shape> = [
				{name: 'paged-no-where', meta: selectMeta(spec, []), params: {}},
				{
					name: 'paged-prefix-eq-1',
					meta: selectMeta(spec, [eq(primaryKey[0]!, primaryKey[0]!)]),
					params: {[primaryKey[0]!]: sample[primaryKey[0]!] as CassandraParam},
				},
				{
					name: 'paged-nonkey-eq',
					meta: selectMeta(spec, [eq(sortable, sortable)]),
					params: {[sortable]: sample[sortable] as CassandraParam},
				},
			];
			for (const shape of pagedShapes) {
				for (const pageSize of [2, 5]) {
					const collect = async (executor: AnyExecutor) => {
						const pages: Array<Array<unknown>> = [];
						let pageState: string | null = null;
						try {
							for (let guard = 0; guard < 500; guard += 1) {
								const page: {rows: Array<unknown>; pageState: string | null} = await executor.executePagedQuery(
									{cql: `__page__:${shape.name}`, params: shape.params, kvMeta: shape.meta},
									{pageSize, pageState},
								);
								pages.push(page.rows);
								pageState = page.pageState;
								if (pageState === null) break;
							}
						} catch (error) {
							return {pages, error: (error as Error).message};
						}
						return {pages, error: null as string | null};
					};
					const left = await collect(readLegacy);
					const right = await collect(readNext);
					const label = `${shape.name}/size${pageSize}`;
					if (left.error !== right.error) {
						mismatches.push({
							kind: 'throw',
							table: spec.name,
							shape: label,
							legacy: left.error ?? 'ok',
							next: right.error ?? 'ok',
						});
						continue;
					}
					if (left.error !== null) continue;
					const leftSizes = left.pages.map((page) => page.length).join(',');
					const rightSizes = right.pages.map((page) => page.length).join(',');
					if (leftSizes !== rightSizes) {
						mismatches.push({
							kind: 'rowset',
							table: spec.name,
							shape: label,
							legacy: `sizes=${leftSizes}`,
							next: `sizes=${rightSizes}`,
						});
						continue;
					}
					const leftAll = left.pages.flat();
					const rightAll = right.pages.flat();
					if (multisetFingerprint(leftAll) !== multisetFingerprint(rightAll)) {
						mismatches.push({
							kind: 'rowset',
							table: spec.name,
							shape: label,
							legacy: fingerprint(leftAll).slice(0, 400),
							next: fingerprint(rightAll).slice(0, 400),
						});
						continue;
					}
					if (fingerprint(leftAll) === fingerprint(rightAll)) continue;
					if (keysetEligible(shape)) {
						keysetOrderDeltas.push(`${spec.name} ${label}`);
						continue;
					}
					if (await legacyIsOrderUnstable(NEXT_TABLE, readLegacy, spec.name, shape)) {
						unstableShapes.push(`${spec.name} ${label}`);
						continue;
					}
					mismatches.push({
						kind: 'order',
						table: spec.name,
						shape: label,
						legacy: fingerprint(leftAll).slice(0, 400),
						next: fingerprint(rightAll).slice(0, 400),
					});
				}
			}
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('returns identical results on a database whose row_key collation is linguistic', async () => {
		const mismatches: Array<Mismatch> = [];
		const ordering = await raw.query<{icu: boolean; c: boolean}>(
			`SELECT (E'"a"\u001f"b"' COLLATE ${ICU_COLLATION}) < (E'"a"\u0020' COLLATE ${ICU_COLLATION}) AS icu,
			        (E'"a"\u001f"b"' COLLATE "C") < (E'"a"\u0020' COLLATE "C") AS c`,
		);
		expect(ordering.rows[0]!.c).toBe(true);
		expect(ordering.rows[0]!.icu).toBe(false);
		for (const spec of [...specs, ...adversarialSpecs]) {
			const shapes = spec.name.startsWith('kvdiff_') ? [] : shapesFor(spec);
			for (const shape of shapes) {
				const left = await run(icuLegacy, icuLegacyClient, shape);
				const right = await run(icuNext, icuNextClient, shape);
				await compareRead(mismatches, ICU_TABLE, icuLegacy, spec.name, shape, left, right);
			}
			for (const shape of derivedShapes(
				spec,
				spec.name.startsWith('kvdiff_') ? [] : shapesFor(spec),
				'count',
				COUNT_SHAPE_NAMES,
			)) {
				const left = await run(icuLegacy, icuLegacyClient, shape);
				const right = await run(icuNext, icuNextClient, shape);
				await compareRead(mismatches, ICU_TABLE, icuLegacy, spec.name, shape, left, right);
			}
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('accepts a legacy offset page token and reproduces the legacy page exactly', async () => {
		const mismatches: Array<Mismatch> = [];
		for (const spec of specs.slice(0, 40)) {
			const shape: Shape = {name: 'legacy-token', meta: selectMeta(spec, []), params: {}};
			const query = {cql: '__legacytoken__', params: shape.params, kvMeta: shape.meta};
			const first = await readLegacy.executePagedQuery(query, {pageSize: 3});
			if (first.pageState === null) continue;
			const legacySecond = await readLegacy.executePagedQuery(query, {pageSize: 3, pageState: first.pageState});
			const nextSecond = await readNext.executePagedQuery(query, {pageSize: 3, pageState: first.pageState});
			if (fingerprint(legacySecond.rows) !== fingerprint(nextSecond.rows)) {
				mismatches.push({
					kind: 'rowset',
					table: spec.name,
					shape: 'legacy-token',
					legacy: fingerprint(legacySecond.rows).slice(0, 400),
					next: fingerprint(nextSecond.rows).slice(0, 400),
				});
			}
		}
		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 900_000);

	it('agrees on adversarial key encodings, cross-type params and degenerate IN lists', async () => {
		const mismatches: Array<Mismatch> = [];
		const shapes = adversarialShapes();
		const derived = shapes.map((shape) => ({
			name: `count:${shape.name}`,
			meta: {action: 'count', table: shape.meta.table, where: shape.meta.where} as AnyMeta,
			params: shape.params,
		}));
		for (const shape of [...shapes, ...derived]) {
			const left = await run(readLegacy, readLegacyClient, shape);
			const right = await run(readNext, readNextClient, shape);
			await compareRead(mismatches, NEXT_TABLE, readLegacy, shape.meta.table.name, shape, left, right);
		}
		for (const shape of shapes) {
			const deleteShape: Shape = {
				name: `delete:${shape.name}`,
				meta: {action: 'delete', table: shape.meta.table, where: shape.meta.where} as AnyMeta,
				params: shape.params,
			};
			await restoreBoth(shape.meta.table.name);
			const left = await run(mutLegacy, mutLegacyClient, deleteShape);
			const right = await run(mutNext, mutNextClient, deleteShape);
			if ((left.threw === null) !== (right.threw === null)) {
				if (left.threw !== null && right.threw === null) {
					legacyThrowDeltas.push(`${shape.meta.table.name} ${deleteShape.name}: legacy threw "${left.threw}"`);
				} else {
					mismatches.push({
						kind: 'throw',
						table: shape.meta.table.name,
						shape: deleteShape.name,
						legacy: left.threw ?? 'ok',
						next: right.threw ?? 'ok',
					});
					continue;
				}
			}
			const leftState = await tableState(LEGACY_TABLE, shape.meta.table.name);
			const rightState = await tableState(NEXT_TABLE, shape.meta.table.name);
			if (leftState !== rightState) {
				mismatches.push({
					kind: 'state',
					table: shape.meta.table.name,
					shape: deleteShape.name,
					legacy: leftState.slice(0, 400),
					next: rightState.slice(0, 400),
				});
			}
		}
		for (const spec of adversarialSpecs) await restoreBoth(spec.name);

		const singleName = AdversarialSingle.name;
		const groundTruth = await raw.query<{count: string}>(
			`SELECT count(*) AS count FROM ${NEXT_TABLE} WHERE table_name = $1 AND (expires_at IS NULL OR expires_at > now())`,
			[singleName],
		);
		const pushedCount = await readNext.executeQuery<{count: number}>({
			cql: '__groundtruth__',
			params: {},
			kvMeta: {action: 'count', table: AdversarialSingle as unknown as KvTableSpec<Row>, where: []} as AnyMeta,
		});
		expect(pushedCount[0]!.count).toBe(Number(groundTruth.rows[0]!.count));

		expect(mismatches.length, report(mismatches)).toBe(0);
	}, 1_800_000);

	it('records every accepted delta and nothing else', () => {
		for (const entry of unstableShapes) {
			expect(entry, `unstable shape without a table name: ${entry}`).toContain(' ');
		}
		for (const entry of keysetOrderDeltas) {
			expect(entry, `keyset delta without a table name: ${entry}`).toContain(' ');
		}
		for (const entry of legacyThrowDeltas) {
			expect(entry, `unexpected legacy throw delta: ${entry}`).toContain('legacy threw');
		}
		expect(traffic.shapes).toBeGreaterThan(3000);
		expect(traffic.next).toBeLessThan(traffic.legacy);
	});
});
