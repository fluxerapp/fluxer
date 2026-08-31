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
import {startDockerContainer} from '../test/DockerTestContainer';
import {LegacyPostgresKvQueryExecutor} from './__testref__/LegacyPostgresKvQueryExecutor';
import type {CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from './CassandraTypes';
import {buildCandidatePlan, ensurePostgresKvSchema, PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;
type AnyMeta = KvQueryMeta<Row>;

const LEGACY_TABLE = 'kvm_legacy';
const NEXT_TABLE = 'kvm_next';
const CONTAINER = `fluxer-kvm-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

class TableClient implements IPostgresClient {
	constructor(
		private readonly inner: IPostgresClient,
		private readonly table: string,
	) {}
	async query<T extends Record<string, unknown>>(text: string, values: Array<unknown> = []) {
		return (await this.inner.query(text, values)) as unknown as Awaited<ReturnType<IPostgresClient['query']>> & {
			rows: Array<T>;
		};
	}
	async connect(): Promise<void> {
		await this.inner.connect();
	}
	async shutdown(): Promise<void> {}
	isConnected(): boolean {
		return this.inner.isConnected();
	}
	async transaction<T>(fn: (client: PostgresQueryable) => Promise<T>): Promise<T> {
		return this.inner.transaction(fn);
	}
	kvTable(): string {
		return this.table;
	}
}

const Two: KvTableSpec<Row> = {
	name: 'mx_two',
	columns: ['k', 'c', 'v'],
	primaryKey: ['k', 'c'],
	partitionKey: ['k', 'c'],
};
const PartTwo: KvTableSpec<Row> = {
	name: 'mx_part',
	columns: ['k', 'c', 'v'],
	primaryKey: ['k', 'c'],
	partitionKey: ['k'],
};
const Three: KvTableSpec<Row> = {
	name: 'mx_three',
	columns: ['k', 'c', 'd', 'v'],
	primaryKey: ['k', 'c', 'd'],
	partitionKey: ['k', 'c', 'd'],
};

const KEY_VALUES: Array<[string, unknown]> = [
	['null', null],
	['num0', 0],
	['num1', 1],
	['num10', 10],
	['big1', 1n],
	['str1', '1'],
	['strq', 'a"b\\c'],
	['strctl', 'xy'],
	['strastral', '\u{1f600}'],
	['strspace', 'a b'],
	['true', true],
	['false', false],
	['date', new Date(0)],
	['buf', Buffer.from('a')],
	['localdate', cassandra.types.LocalDate.fromString('2020-01-01')],
	['localdate-str', '2020-01-01'],
	['emptystr', ''],
];

function bigintJson(_key: string, value: unknown): unknown {
	if (typeof value === 'bigint') return `bigint:${value.toString()}`;
	if (Buffer.isBuffer(value)) return `buf:${value.toString('base64')}`;
	return value;
}

describe.skipIf(!dockerAvailable)('postgres kv non-select matrix', () => {
	let raw: IPostgresClient;
	let legacy: LegacyPostgresKvQueryExecutor;
	let next: PostgresKvQueryExecutor;

	async function freePort(): Promise<number> {
		return new Promise((resolve, reject) => {
			const server = createServer();
			server.on('error', reject);
			server.listen(0, '127.0.0.1', () => {
				const address = server.address();
				if (typeof address === 'string' || address === null) {
					reject(new Error('no port'));
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
	async function dump(kv: string, tableName: string): Promise<string> {
		const result = await raw.query<{row_key: string}>(
			`SELECT row_key FROM ${kv} WHERE table_name = $1 ORDER BY row_key COLLATE "C"`,
			[tableName],
		);
		return JSON.stringify(result.rows.map((r) => r.row_key));
	}
	async function seed(spec: KvTableSpec<Row>, rows: ReadonlyArray<Row>): Promise<void> {
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [spec.name]);
		}
		const meta = {action: 'upsert', table: spec} as AnyMeta;
		for (const row of rows) {
			await legacy.executeQuery({cql: '__s__', params: row as CassandraParams, kvMeta: meta});
			await next.executeQuery({cql: '__s__', params: row as CassandraParams, kvMeta: meta});
		}
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
			'postgres:16-alpine',
			'-c',
			'fsync=off',
		]);
		let ready = false;
		for (let attempt = 0; attempt < 180 && !ready; attempt += 1) {
			await sleep(500);
			if (
				spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {stdio: 'ignore'})
					.status !== 0
			)
				continue;
			try {
				await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`, maxConnections: 8});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		raw = getDefaultPostgresClient();
		await ensurePostgresKvSchema(new TableClient(raw, LEGACY_TABLE));
		await ensurePostgresKvSchema(new TableClient(raw, NEXT_TABLE));
		legacy = new LegacyPostgresKvQueryExecutor(new TableClient(raw, LEGACY_TABLE));
		next = new PostgresKvQueryExecutor(new TableClient(raw, NEXT_TABLE));
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('count and delete agree with legacy for every key value type', async () => {
		const rows = KEY_VALUES.map(([, value], i) => ({k: value, c: BigInt(i), v: i}));
		const countMismatch: Array<string> = [];
		const deleteMismatch: Array<string> = [];
		const selectMismatch: Array<string> = [];
		for (const [name, value] of KEY_VALUES) {
			await seed(Two, rows);
			const where = [{kind: 'eq', col: 'k', param: 'k'}] as Array<WhereExpr<Row>>;
			const params = {k: value} as CassandraParams;
			const sel = {action: 'select', table: Two, where, columns: Two.columns} as AnyMeta;
			const l = await legacy.executeQuery({cql: '__sel__', params, kvMeta: sel});
			const n = await next.executeQuery({cql: '__sel__', params, kvMeta: sel});
			if (JSON.stringify(l, bigintJson) !== JSON.stringify(n, bigintJson)) {
				selectMismatch.push(`${name}: legacy=${JSON.stringify(l, bigintJson)} next=${JSON.stringify(n, bigintJson)}`);
			}
			const cnt = {action: 'count', table: Two, where} as AnyMeta;
			const lc = await legacy.executeQuery({cql: '__cnt__', params, kvMeta: cnt});
			const nc = await next.executeQuery({cql: '__cnt__', params, kvMeta: cnt});
			if (JSON.stringify(lc) !== JSON.stringify(nc)) {
				countMismatch.push(`${name}: legacy=${JSON.stringify(lc)} next=${JSON.stringify(nc)}`);
			}
			const del = {action: 'delete', table: Two, where} as AnyMeta;
			await legacy.executeQuery({cql: '__del__', params, kvMeta: del});
			await next.executeQuery({cql: '__del__', params, kvMeta: del});
			const ld = await dump(LEGACY_TABLE, Two.name);
			const nd = await dump(NEXT_TABLE, Two.name);
			if (ld !== nd) deleteMismatch.push(`${name}: legacy=${ld} next=${nd}`);
		}
		expect({selectMismatch, countMismatch, deleteMismatch}).toEqual({
			selectMismatch: [],
			countMismatch: [],
			deleteMismatch: [],
		});
	}, 300_000);

	it('explicit partition key tables agree for count and delete', async () => {
		const rows = [
			{k: 1n, c: 1n, v: 1},
			{k: 1n, c: 2n, v: 2},
			{k: 2n, c: 1n, v: 3},
		];
		await seed(PartTwo, rows);
		const where = [{kind: 'eq', col: 'k', param: 'k'}] as Array<WhereExpr<Row>>;
		const params = {k: 1n} as CassandraParams;
		const lc = await legacy.executeQuery({
			cql: '__pc__',
			params,
			kvMeta: {action: 'count', table: PartTwo, where} as AnyMeta,
		});
		const nc = await next.executeQuery({
			cql: '__pc__',
			params,
			kvMeta: {action: 'count', table: PartTwo, where} as AnyMeta,
		});
		expect(nc).toEqual(lc);
		await legacy.executeQuery({cql: '__pd__', params, kvMeta: {action: 'delete', table: PartTwo, where} as AnyMeta});
		await next.executeQuery({cql: '__pd__', params, kvMeta: {action: 'delete', table: PartTwo, where} as AnyMeta});
		expect(await dump(NEXT_TABLE, PartTwo.name)).toBe(await dump(LEGACY_TABLE, PartTwo.name));
	});

	it('multi column IN products agree, including above the combination cap', async () => {
		const rows: Array<Row> = [];
		for (let a = 0; a < 40; a += 1) {
			for (let b = 0; b < 40; b += 1) rows.push({k: BigInt(a), c: BigInt(b), v: a * b});
		}
		await seed(Two, rows);
		const ks = Array.from({length: 40}, (_, i) => BigInt(i));
		const cs = Array.from({length: 40}, (_, i) => BigInt(i));
		const where = [
			{kind: 'in', col: 'k', param: 'ks'},
			{kind: 'in', col: 'c', param: 'cs'},
		] as Array<WhereExpr<Row>>;
		for (const [label, params] of [
			['small', {ks: ks.slice(0, 3), cs: cs.slice(0, 3)}],
			['cap', {ks, cs}],
			['dupes', {ks: [1n, 1n, 2n], cs: [1n, 1n]}],
			['empty', {ks: [], cs}],
		] as Array<[string, CassandraParams]>) {
			const lc = await legacy.executeQuery({
				cql: `__mc_${label}__`,
				params,
				kvMeta: {action: 'count', table: Two, where} as AnyMeta,
			});
			const nc = await next.executeQuery({
				cql: `__mc_${label}__`,
				params,
				kvMeta: {action: 'count', table: Two, where} as AnyMeta,
			});
			expect([label, nc]).toEqual([label, lc]);
			const ls = await legacy.executeQuery({
				cql: `__ms_${label}__`,
				params,
				kvMeta: {action: 'select', table: Two, where, columns: Two.columns} as AnyMeta,
			});
			const ns = await next.executeQuery({
				cql: `__ms_${label}__`,
				params,
				kvMeta: {action: 'select', table: Two, where, columns: Two.columns} as AnyMeta,
			});
			expect([label, JSON.stringify(ns, bigintJson)]).toEqual([label, JSON.stringify(ls, bigintJson)]);
		}
		await legacy.executeQuery({
			cql: '__md__',
			params: {ks, cs} as CassandraParams,
			kvMeta: {action: 'delete', table: Two, where} as AnyMeta,
		});
		await next.executeQuery({
			cql: '__md__',
			params: {ks, cs} as CassandraParams,
			kvMeta: {action: 'delete', table: Two, where} as AnyMeta,
		});
		expect(await dump(NEXT_TABLE, Two.name)).toBe(await dump(LEGACY_TABLE, Two.name));
	}, 300_000);

	it('IN lists above the range cap agree once the plan is chunked into groups', async () => {
		const rows: Array<Row> = [];
		for (let k = 0; k < 300; k += 1) {
			for (const c of [0n, 1n]) rows.push({k: BigInt(k), c, v: k});
		}
		await seed(Two, rows);
		const where = [{kind: 'in', col: 'k', param: 'ks'}] as Array<WhereExpr<Row>>;
		const params = {ks: Array.from({length: 260}, (_, i) => BigInt(i))} as CassandraParams;
		const sel = {action: 'select', table: Two, where, columns: Two.columns} as AnyMeta;
		expect(buildCandidatePlan(sel, params).candidates.kind).toBe('rangeGroups');
		const ls = await legacy.executeQuery({cql: '__chunk_sel__', params, kvMeta: sel});
		const ns = await next.executeQuery({cql: '__chunk_sel__', params, kvMeta: sel});
		expect(ns.length).toBe(520);
		expect(JSON.stringify(ns, bigintJson)).toBe(JSON.stringify(ls, bigintJson));
		const cnt = {action: 'count', table: Two, where} as AnyMeta;
		const lc = await legacy.executeQuery({cql: '__chunk_cnt__', params, kvMeta: cnt});
		const nc = await next.executeQuery({cql: '__chunk_cnt__', params, kvMeta: cnt});
		expect(nc).toEqual([{count: 520}]);
		expect(nc).toEqual(lc);
		const del = {action: 'delete', table: Two, where} as AnyMeta;
		await legacy.executeQuery({cql: '__chunk_del__', params, kvMeta: del});
		await next.executeQuery({cql: '__chunk_del__', params, kvMeta: del});
		const remaining = await dump(NEXT_TABLE, Two.name);
		expect(remaining).toBe(await dump(LEGACY_TABLE, Two.name));
		expect(JSON.parse(remaining).length).toBe(80);
	}, 300_000);

	it('count with a limit or an order by matches legacy', async () => {
		await seed(Two, [
			{k: 1n, c: 1n, v: 1},
			{k: 1n, c: 2n, v: 2},
			{k: 1n, c: 3n, v: 3},
		]);
		const where = [{kind: 'eq', col: 'k', param: 'k'}] as Array<WhereExpr<Row>>;
		for (const extra of [{limit: 2}, {orderBy: {col: 'c', direction: 'DESC'}}, {}]) {
			const meta = {action: 'count', table: Two, where, ...extra} as AnyMeta;
			const l = await legacy.executeQuery({cql: `__cl__${JSON.stringify(extra)}`, params: {k: 1n}, kvMeta: meta});
			const n = await next.executeQuery({cql: `__cl__${JSON.stringify(extra)}`, params: {k: 1n}, kvMeta: meta});
			expect([JSON.stringify(extra), n]).toEqual([JSON.stringify(extra), l]);
		}
	});

	it('count and delete with a gap in the primary key match legacy', async () => {
		const rows: Array<Row> = [];
		for (const c of [1n, 2n]) {
			for (const d of [1n, 2n]) rows.push({k: 1n, c, d, v: 1});
		}
		rows.push({k: 2n, c: 1n, d: 1n, v: 9});
		await seed(Three, rows);
		const where = [
			{kind: 'eq', col: 'k', param: 'k'},
			{kind: 'eq', col: 'd', param: 'd'},
		] as Array<WhereExpr<Row>>;
		const params = {k: 1n, d: 1n} as CassandraParams;
		const lc = await legacy.executeQuery({
			cql: '__gc__',
			params,
			kvMeta: {action: 'count', table: Three, where} as AnyMeta,
		});
		const nc = await next.executeQuery({
			cql: '__gc__',
			params,
			kvMeta: {action: 'count', table: Three, where} as AnyMeta,
		});
		expect(nc).toEqual(lc);
		await legacy.executeQuery({cql: '__gd__', params, kvMeta: {action: 'delete', table: Three, where} as AnyMeta});
		await next.executeQuery({cql: '__gd__', params, kvMeta: {action: 'delete', table: Three, where} as AnyMeta});
		expect(await dump(NEXT_TABLE, Three.name)).toBe(await dump(LEGACY_TABLE, Three.name));
	});

	it('delete with a non key where column matches legacy', async () => {
		await seed(Two, [
			{k: 1n, c: 1n, v: 5},
			{k: 1n, c: 2n, v: 6},
			{k: 2n, c: 1n, v: 5},
		]);
		const where = [
			{kind: 'eq', col: 'k', param: 'k'},
			{kind: 'eq', col: 'v', param: 'v'},
		] as Array<WhereExpr<Row>>;
		const params = {k: 1n, v: 5} as CassandraParams;
		await legacy.executeQuery({cql: '__nd__', params, kvMeta: {action: 'delete', table: Two, where} as AnyMeta});
		await next.executeQuery({cql: '__nd__', params, kvMeta: {action: 'delete', table: Two, where} as AnyMeta});
		expect(await dump(NEXT_TABLE, Two.name)).toBe(await dump(LEGACY_TABLE, Two.name));
	});
});
