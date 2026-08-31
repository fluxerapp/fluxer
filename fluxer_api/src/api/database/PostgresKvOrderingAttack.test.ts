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
import {defineTable} from './CassandraTableDsl';
import type {CassandraParam, CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from './CassandraTypes';
import {buildCandidatePlan, ensurePostgresKvSchema, PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;
type AnyMeta = KvQueryMeta<Row>;
type AnyWhere = WhereExpr<Row>;

const KV = 'kv_order_attack';
const POSTGRES_IMAGE = 'postgres:16-alpine';
const CONTAINER = `fluxer-kvorder-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

const BigChild = defineTable<{owner: unknown; seq: unknown; v: unknown}, 'owner' | 'seq'>({
	name: 'attack_bigint_child',
	columns: ['owner', 'seq', 'v'],
	primaryKey: ['owner', 'seq'],
});

const NumChild = defineTable<{owner: unknown; seq: unknown; v: unknown}, 'owner' | 'seq'>({
	name: 'attack_number_child',
	columns: ['owner', 'seq', 'v'],
	primaryKey: ['owner', 'seq'],
});

const DateChild = defineTable<{owner: unknown; at: unknown; id: unknown; v: unknown}, 'owner' | 'at' | 'id'>({
	name: 'attack_date_child',
	columns: ['owner', 'at', 'id', 'v'],
	primaryKey: ['owner', 'at', 'id'],
});

const StrChild = defineTable<{owner: unknown; seq: unknown; v: unknown}, 'owner' | 'seq'>({
	name: 'attack_string_child',
	columns: ['owner', 'seq', 'v'],
	primaryKey: ['owner', 'seq'],
});

const ScanBig = defineTable<{id: unknown; v: unknown}, 'id'>({
	name: 'attack_scan_bigint',
	columns: ['id', 'v'],
	primaryKey: ['id'],
});

const Messages = defineTable<
	{channel_id: unknown; bucket: unknown; message_id: unknown; content: unknown},
	'channel_id' | 'bucket' | 'message_id',
	'channel_id' | 'bucket'
>({
	name: 'attack_messages',
	columns: ['channel_id', 'bucket', 'message_id', 'content'],
	primaryKey: ['channel_id', 'bucket', 'message_id'],
	partitionKey: ['channel_id', 'bucket'],
});

const LocalDateChild = defineTable<{owner: unknown; seq: unknown; v: unknown}, 'owner' | 'seq'>({
	name: 'attack_localdate_child',
	columns: ['owner', 'seq', 'v'],
	primaryKey: ['owner', 'seq'],
});

function eq(col: string, param: string): AnyWhere {
	return {kind: 'eq', col, param} as AnyWhere;
}

function inClause(col: string, param: string): AnyWhere {
	return {kind: 'in', col, param} as AnyWhere;
}

function cmp(kind: 'lt' | 'lte' | 'gt' | 'gte', col: string, param: string): AnyWhere {
	return {kind, col, param} as AnyWhere;
}

function spec(table: unknown): KvTableSpec<Row> {
	return table as unknown as KvTableSpec<Row>;
}

function selectMeta(table: unknown, where: Array<AnyWhere>, extra: Partial<AnyMeta> = {}): AnyMeta {
	const s = spec(table);
	return {action: 'select', table: s, where, columns: s.columns, ...extra} as AnyMeta;
}

function canonical(value: unknown): unknown {
	if (value === undefined) return {__c: 'undefined'};
	if (value === null) return null;
	if (typeof value === 'bigint') return {__c: 'bigint', v: value.toString()};
	if (typeof value === 'number') {
		if (Number.isNaN(value)) return {__c: 'nan'};
		if (!Number.isFinite(value)) return {__c: 'inf', v: value > 0 ? 1 : -1};
		return value;
	}
	if (value instanceof Date) return {__c: 'date', v: value.toISOString()};
	if (Buffer.isBuffer(value)) return {__c: 'buffer', v: value.toString('base64')};
	if (typeof value === 'object' && value.constructor?.name === 'LocalDate') return {__c: 'localdate', v: String(value)};
	if (Array.isArray(value)) return value.map(canonical);
	if (typeof value === 'object') {
		const source = value as Record<string, unknown>;
		return {
			__c: 'obj',
			v: Object.keys(source)
				.sort()
				.map((k) => [k, canonical(source[k])]),
		};
	}
	return value;
}

function fp(rows: ReadonlyArray<unknown>): string {
	return JSON.stringify(rows.map(canonical));
}

function multiset(rows: ReadonlyArray<unknown>): string {
	return JSON.stringify(rows.map((r) => JSON.stringify(canonical(r))).sort());
}

const suite = dockerAvailable ? describe : describe.skip;

suite('PostgresKvQueryExecutor ordering and limit attack', () => {
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

	function client(): IPostgresClient {
		const inner = raw;
		return {
			query: (text: string, values?: Array<unknown>) => inner.query(text, values),
			connect: () => inner.connect(),
			shutdown: async () => {},
			isConnected: () => inner.isConnected(),
			transaction: <T>(fn: (db: PostgresQueryable) => Promise<T>) => inner.transaction(fn),
			kvTable: () => KV,
		} as unknown as IPostgresClient;
	}

	async function seed(table: unknown, rows: ReadonlyArray<Row>): Promise<void> {
		const s = spec(table);
		await raw.query(`DELETE FROM ${KV} WHERE table_name = $1`, [s.name]);
		const meta = {action: 'upsert', table: s} as AnyMeta;
		for (const row of rows) {
			await legacy.executeQuery({cql: `__seed__${s.name}`, params: row as CassandraParams, kvMeta: meta});
		}
	}

	async function collectPages(
		executor: LegacyPostgresKvQueryExecutor | PostgresKvQueryExecutor,
		meta: AnyMeta,
		params: CassandraParams,
		pageSize: number,
	): Promise<Array<Array<unknown>>> {
		const pages: Array<Array<unknown>> = [];
		let pageState: string | null = null;
		for (let guard = 0; guard < 200; guard += 1) {
			const page: {rows: Array<unknown>; pageState: string | null} = await executor.executePagedQuery(
				{cql: '__page__', params, kvMeta: meta},
				{pageSize, pageState},
			);
			pages.push(page.rows);
			pageState = page.pageState;
			if (pageState === null) break;
		}
		return pages;
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
				await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`, maxConnections: 6});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres not ready');
		raw = getDefaultPostgresClient();
		const c = client();
		await ensurePostgresKvSchema(c);
		legacy = new LegacyPostgresKvQueryExecutor(c);
		next = new PostgresKvQueryExecutor(c);
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('A1 keyset paging over a bigint clustering column keeps legacy page boundaries', async () => {
		const seqs = [1n, 2n, 9n, 10n, 11n, 100n, 101n, 999n, 1000n];
		await seed(
			BigChild,
			seqs.map((seq) => ({owner: 'o', seq, v: seq.toString()})),
		);
		const meta = selectMeta(BigChild, [eq('owner', 'owner')]);
		const params: CassandraParams = {owner: 'o'};
		const plan = buildCandidatePlan(meta, params);
		expect(plan.exact).toBe(true);
		expect(plan.candidates.kind).toBe('range');
		const left = await collectPages(legacy, meta, params, 3);
		const right = await collectPages(next, meta, params, 3);
		expect(multiset(left.flat())).toBe(multiset(right.flat()));
		expect(right.map((p) => p.map((r) => String((r as Row).seq)))).toStrictEqual(
			left.map((p) => p.map((r) => String((r as Row).seq))),
		);
	}, 120_000);

	it('A2 keyset paging over a plain number clustering column keeps legacy page boundaries', async () => {
		const seqs = [1, 2, 9, 10, 11, 100, 1e21, -1];
		await seed(
			NumChild,
			seqs.map((seq) => ({owner: 'o', seq, v: String(seq)})),
		);
		const meta = selectMeta(NumChild, [eq('owner', 'owner')]);
		const params: CassandraParams = {owner: 'o'};
		const left = await collectPages(legacy, meta, params, 3);
		const right = await collectPages(next, meta, params, 3);
		expect(multiset(left.flat())).toBe(multiset(right.flat()));
		expect(fp(right.flat())).toBe(fp(left.flat()));
	}, 120_000);

	it('A3 keyset paging over a whole-table bigint scan keeps legacy page boundaries', async () => {
		const ids = [1n, 2n, 9n, 10n, 11n, 100n, 1000n];
		await seed(
			ScanBig,
			ids.map((id) => ({id, v: id.toString()})),
		);
		const meta = selectMeta(ScanBig, []);
		const plan = buildCandidatePlan(meta, {});
		expect(plan.exact).toBe(true);
		expect(plan.candidates.kind).toBe('scan');
		const left = await collectPages(legacy, meta, {}, 3);
		const right = await collectPages(next, meta, {}, 3);
		expect(multiset(left.flat())).toBe(multiset(right.flat()));
		expect(fp(right.flat())).toBe(fp(left.flat()));
	}, 120_000);

	it('A4 keyset paging over a Date clustering column keeps legacy page boundaries', async () => {
		const dates = [
			new Date('1969-12-31T23:59:59.999Z'),
			new Date(0),
			new Date('2020-01-02T03:04:05.006Z'),
			new Date('2024-06-07T08:09:10.011Z'),
			new Date('2999-01-01T00:00:00.000Z'),
			new Date(-62167219200000),
			new Date(8640000000000000),
		];
		await seed(
			DateChild,
			dates.map((at, index) => ({owner: 'o', at, id: BigInt(index), v: at.toISOString()})),
		);
		const meta = selectMeta(DateChild, [eq('owner', 'owner')]);
		const params: CassandraParams = {owner: 'o'};
		const left = await collectPages(legacy, meta, params, 2);
		const right = await collectPages(next, meta, params, 2);
		expect(multiset(left.flat())).toBe(multiset(right.flat()));
		expect(fp(right.flat())).toBe(fp(left.flat()));
	}, 120_000);

	it('A5 keyset paging over a string clustering column keeps legacy page boundaries', async () => {
		const seqs = ['a', 'b', 'A', 'ab', 'a b', '\u{1f600}', '�', 'a"b', 'a\\b', ''];
		await seed(
			StrChild,
			seqs.map((seq, index) => ({owner: 'o', seq, v: index})),
		);
		const meta = selectMeta(StrChild, [eq('owner', 'owner')]);
		const params: CassandraParams = {owner: 'o'};
		const left = await collectPages(legacy, meta, params, 3);
		const right = await collectPages(next, meta, params, 3);
		expect(multiset(left.flat())).toBe(multiset(right.flat()));
		expect(fp(right.flat())).toBe(fp(left.flat()));
	}, 120_000);

	it('A6 a keyset page is never empty while it reports more pages', async () => {
		const seqs = [1n, 2n, 9n, 10n, 11n, 100n, 101n];
		await seed(
			BigChild,
			seqs.map((seq) => ({owner: 'o', seq, v: seq.toString()})),
		);
		const meta = selectMeta(BigChild, [eq('owner', 'owner')]);
		const pages = await collectPages(next, meta, {owner: 'o'}, 2);
		for (let index = 0; index < pages.length - 1; index += 1) {
			expect(pages[index]!.length).toBeGreaterThan(0);
		}
	}, 120_000);

	it('B1 select with a LIMIT returns the same rows as the legacy executor', async () => {
		const seqs = [1n, 2n, 9n, 10n, 11n, 100n, 101n, 999n, 1000n];
		const rows: Array<Row> = [];
		for (const owner of ['o', 'p']) for (const seq of seqs) rows.push({owner, seq, v: seq.toString()});
		await seed(BigChild, rows);
		const mismatches: Array<string> = [];
		for (const limit of [1, 2, 3, 5, 100]) {
			for (const direction of [undefined, 'ASC', 'DESC'] as const) {
				const extra: Partial<AnyMeta> = {limit};
				if (direction) extra.orderBy = {col: 'seq', direction} as AnyMeta['orderBy'];
				const meta = selectMeta(BigChild, [eq('owner', 'owner')], extra);
				const params: CassandraParams = {owner: 'o'};
				const l = await legacy.executeQuery({cql: '__l__', params, kvMeta: meta});
				const r = await next.executeQuery({cql: '__l__', params, kvMeta: meta});
				if (fp(l) !== fp(r)) mismatches.push(`limit=${limit} order=${direction}\n  legacy ${fp(l)}\n  next   ${fp(r)}`);
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 120_000);

	it('B2 select with a LIMIT and an IN over the leading key returns the same rows', async () => {
		const rows: Array<Row> = [];
		for (const owner of ['o', 'p', 'q']) {
			for (const seq of [1n, 2n, 9n, 10n, 11n, 100n]) rows.push({owner, seq, v: `${owner}${seq}`});
		}
		await seed(BigChild, rows);
		const mismatches: Array<string> = [];
		for (const limit of [1, 2, 4, 7, 100]) {
			for (const direction of [undefined, 'ASC', 'DESC'] as const) {
				const extra: Partial<AnyMeta> = {limit};
				if (direction) extra.orderBy = {col: 'seq', direction} as AnyMeta['orderBy'];
				const meta = selectMeta(BigChild, [inClause('owner', 'owners')], extra);
				const params: CassandraParams = {owners: ['o', 'q', 'o'] as unknown as CassandraParam};
				const l = await legacy.executeQuery({cql: '__l2__', params, kvMeta: meta});
				const r = await next.executeQuery({cql: '__l2__', params, kvMeta: meta});
				if (fp(l) !== fp(r)) mismatches.push(`limit=${limit} order=${direction}\n  legacy ${fp(l)}\n  next   ${fp(r)}`);
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 120_000);

	it('B3 select with a LIMIT and a range clause on the clustering column returns the same rows', async () => {
		const rows: Array<Row> = [];
		for (const owner of ['o', 'p']) {
			for (const seq of [1n, 2n, 9n, 10n, 11n, 100n, 101n, 1000n]) rows.push({owner, seq, v: `${owner}${seq}`});
		}
		await seed(BigChild, rows);
		const mismatches: Array<string> = [];
		for (const kind of ['lt', 'lte', 'gt', 'gte'] as const) {
			for (const bound of [1n, 10n, 11n, 100n]) {
				for (const limit of [1, 2, 3, 100]) {
					for (const direction of ['ASC', 'DESC'] as const) {
						const meta = selectMeta(BigChild, [eq('owner', 'owner'), cmp(kind, 'seq', 'bound')], {
							limit,
							orderBy: {col: 'seq', direction} as AnyMeta['orderBy'],
						});
						const params: CassandraParams = {owner: 'o', bound};
						const l = await legacy.executeQuery({cql: '__l3__', params, kvMeta: meta});
						const r = await next.executeQuery({cql: '__l3__', params, kvMeta: meta});
						if (fp(l) !== fp(r)) {
							mismatches.push(`${kind} ${bound} limit=${limit} ${direction}\n  legacy ${fp(l)}\n  next   ${fp(r)}`);
						}
					}
				}
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 240_000);

	it('B4 message history shaped select returns the same page as the legacy executor', async () => {
		const rows: Array<Row> = [];
		for (const channel of [10n, 11n]) {
			for (const bucket of [0, 1, 2, 10, 11]) {
				for (const messageId of [1n, 2n, 9n, 10n, 11n, 100n, 101n, 999n, 1000n, 9999999999999999999n]) {
					rows.push({channel_id: channel, bucket, message_id: messageId, content: `${channel}/${bucket}/${messageId}`});
				}
			}
		}
		await seed(Messages, rows);
		const mismatches: Array<string> = [];
		for (const bucket of [0, 1, 10]) {
			for (const cursor of [1n, 10n, 11n, 100n, 1000n, 9999999999999999999n]) {
				for (const kind of ['lt', 'gt'] as const) {
					for (const direction of ['ASC', 'DESC'] as const) {
						for (const limit of [1, 3, 50]) {
							const meta = selectMeta(
								Messages,
								[eq('channel_id', 'channel_id'), eq('bucket', 'bucket'), cmp(kind, 'message_id', 'cursor')],
								{limit, orderBy: {col: 'message_id', direction} as AnyMeta['orderBy']},
							);
							const params: CassandraParams = {channel_id: 10n, bucket, cursor};
							const l = await legacy.executeQuery({cql: '__b4__', params, kvMeta: meta});
							const r = await next.executeQuery({cql: '__b4__', params, kvMeta: meta});
							if (fp(l) !== fp(r)) {
								mismatches.push(`bucket=${bucket} ${kind} ${cursor} ${direction} limit=${limit}`);
							}
						}
					}
				}
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 300_000);

	it('B5 select with a LIMIT under a demoted over-cap cartesian plan returns the same rows', async () => {
		const rows: Array<Row> = [];
		for (const owner of ['o', 'p', 'q']) {
			for (const seq of [1n, 2n, 9n, 10n, 11n, 100n]) rows.push({owner, seq, v: `${owner}${seq}`});
		}
		await seed(BigChild, rows);
		const owners = Array.from({length: 40}, (_unused, index) => `x${index}`);
		owners.push('o', 'q');
		const seqs = Array.from({length: 40}, (_unused, index) => BigInt(index));
		seqs.push(100n);
		const mismatches: Array<string> = [];
		for (const limit of [1, 3, 100]) {
			for (const direction of ['ASC', 'DESC'] as const) {
				const meta = selectMeta(BigChild, [inClause('owner', 'owners'), inClause('seq', 'seqs')], {
					limit,
					orderBy: {col: 'seq', direction} as AnyMeta['orderBy'],
				});
				const params: CassandraParams = {
					owners: owners as unknown as CassandraParam,
					seqs: seqs as unknown as CassandraParam,
				};
				const l = await legacy.executeQuery({cql: '__b5__', params, kvMeta: meta});
				const r = await next.executeQuery({cql: '__b5__', params, kvMeta: meta});
				if (fp(l) !== fp(r)) mismatches.push(`limit=${limit} ${direction}\n  legacy ${fp(l)}\n  next   ${fp(r)}`);
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 180_000);

	it('B6 select ordered by a column outside the projection returns the same rows', async () => {
		const rows: Array<Row> = [];
		for (const seq of [1n, 2n, 9n, 10n, 11n, 100n]) rows.push({owner: 'o', seq, v: `v${seq}`});
		await seed(BigChild, rows);
		const mismatches: Array<string> = [];
		for (const limit of [1, 3, 100]) {
			for (const direction of ['ASC', 'DESC'] as const) {
				const meta = selectMeta(BigChild, [eq('owner', 'owner')], {
					columns: ['v'],
					limit,
					orderBy: {col: 'seq', direction} as AnyMeta['orderBy'],
				});
				const params: CassandraParams = {owner: 'o'};
				const l = await legacy.executeQuery({cql: '__b6__', params, kvMeta: meta});
				const r = await next.executeQuery({cql: '__b6__', params, kvMeta: meta});
				if (fp(l) !== fp(r)) mismatches.push(`limit=${limit} ${direction}\n  legacy ${fp(l)}\n  next   ${fp(r)}`);
			}
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 180_000);

	it('B7 keyset page sizes match legacy page sizes exactly', async () => {
		const rows: Array<Row> = [];
		for (const seq of [1n, 2n, 9n, 10n, 11n, 100n, 101n]) rows.push({owner: 'o', seq, v: `v${seq}`});
		await seed(BigChild, rows);
		const meta = selectMeta(BigChild, [eq('owner', 'owner')]);
		const mismatches: Array<string> = [];
		for (const pageSize of [1, 2, 3, 6, 7, 8, 100]) {
			const left = await collectPages(legacy, meta, {owner: 'o'}, pageSize);
			const right = await collectPages(next, meta, {owner: 'o'}, pageSize);
			const l = left.map((p) => p.length).join(',');
			const r = right.map((p) => p.length).join(',');
			if (l !== r) mismatches.push(`pageSize=${pageSize} legacy=${l} next=${r}`);
			if (multiset(left.flat()) !== multiset(right.flat())) mismatches.push(`pageSize=${pageSize} rowset`);
		}
		expect(mismatches.join('\n'), mismatches.join('\n')).toBe('');
	}, 180_000);

	it('C1 a LocalDate key value is still found by an equal string parameter', async () => {
		await seed(LocalDateChild, [
			{owner: cassandra.types.LocalDate.fromString('2020-01-01'), seq: 1n, v: 'localdate'},
			{owner: '2020-01-01', seq: 2n, v: 'string'},
		]);
		const meta = selectMeta(LocalDateChild, [eq('owner', 'owner')], {limit: 10});
		const params: CassandraParams = {owner: '2020-01-01'};
		const l = await legacy.executeQuery({cql: '__c1__', params, kvMeta: meta});
		const r = await next.executeQuery({cql: '__c1__', params, kvMeta: meta});
		expect(fp(r), `legacy ${fp(l)} next ${fp(r)}`).toBe(fp(l));
	}, 120_000);

	it('C2 a string key value is still found by an equal LocalDate parameter', async () => {
		await seed(LocalDateChild, [
			{owner: cassandra.types.LocalDate.fromString('2020-01-01'), seq: 1n, v: 'localdate'},
			{owner: '2020-01-01', seq: 2n, v: 'string'},
		]);
		const meta = selectMeta(LocalDateChild, [eq('owner', 'owner')], {limit: 10});
		const params: CassandraParams = {owner: cassandra.types.LocalDate.fromString('2020-01-01')};
		const l = await legacy.executeQuery({cql: '__c2__', params, kvMeta: meta});
		const r = await next.executeQuery({cql: '__c2__', params, kvMeta: meta});
		expect(fp(r), `legacy ${fp(l)} next ${fp(r)}`).toBe(fp(l));
	}, 120_000);
});
