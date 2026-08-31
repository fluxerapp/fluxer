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
import {afterAll, beforeAll, describe, expect, it} from 'vitest';
import {startDockerContainer} from '../test/DockerTestContainer';
import {LegacyPostgresKvQueryExecutor} from './__testref__/LegacyPostgresKvQueryExecutor';
import type {CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from './CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;
type AnyMeta = KvQueryMeta<Row>;
type AnyExec = LegacyPostgresKvQueryExecutor | PostgresKvQueryExecutor;

const KV = 'kv_pagead';
const ICU_KV = 'kv_pagead_icu';
const ICU_COLLATION = 'kvpagead_icu';
const POSTGRES_IMAGE = 'postgres:16-alpine';
const CONTAINER = `fluxer-kvpage-${process.pid.toString(36)}-${Date.now().toString(36)}`;

const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

const PagedTable: KvTableSpec<Row> = {
	name: 'pagead_items',
	columns: ['owner_id', 'item_id', 'payload'],
	primaryKey: ['owner_id', 'item_id'],
	partitionKey: ['owner_id', 'item_id'],
};

const FlatTable: KvTableSpec<Row> = {
	name: 'pagead_flat',
	columns: ['k', 'v'],
	primaryKey: ['k'],
	partitionKey: ['k'],
};

class PlainClient implements IPostgresClient {
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

function selectMeta(spec: KvTableSpec<Row>, where: Array<WhereExpr<Row>> = [], extra: Partial<AnyMeta> = {}): AnyMeta {
	return {action: 'select', table: spec, where, columns: spec.columns, ...extra} as AnyMeta;
}

const upsertMeta = (spec: KvTableSpec<Row>) => ({action: 'upsert', table: spec}) as AnyMeta;
const deleteMeta = (spec: KvTableSpec<Row>, where: Array<WhereExpr<Row>>) =>
	({action: 'delete', table: spec, where}) as AnyMeta;

function token(value: unknown): string {
	return Buffer.from(JSON.stringify(value)).toString('base64url');
}

describe.skipIf(!dockerAvailable)('postgres kv paging adversarial', () => {
	let raw: IPostgresClient;
	let legacy: LegacyPostgresKvQueryExecutor;
	let next: PostgresKvQueryExecutor;
	let icuNext: PostgresKvQueryExecutor;
	let icuLegacy: LegacyPostgresKvQueryExecutor;

	async function sleep(ms: number): Promise<void> {
		await new Promise((resolve) => setTimeout(resolve, ms));
	}

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

	async function upsert(exec: AnyExec, spec: KvTableSpec<Row>, row: Row) {
		await exec.executeQuery({cql: `__seed__${spec.name}`, params: row as CassandraParams, kvMeta: upsertMeta(spec)});
	}

	async function wipe(kv: string) {
		await raw.query(`DELETE FROM ${kv}`);
	}

	async function pageAll(
		exec: AnyExec,
		meta: AnyMeta,
		params: CassandraParams,
		pageSize: number,
		hook?: (pageIndex: number) => Promise<void>,
	): Promise<{pages: Array<Array<Row>>; error: string | null}> {
		const pages: Array<Array<Row>> = [];
		let pageState: string | null = null;
		try {
			for (let guard = 0; guard < 400; guard += 1) {
				const page: {rows: Array<Row>; pageState: string | null} = await exec.executePagedQuery<Row>(
					{cql: `__page__${meta.table.name}`, params, kvMeta: meta},
					{pageSize, pageState},
				);
				pages.push(page.rows);
				pageState = page.pageState;
				if (hook) await hook(guard);
				if (pageState === null) break;
				if (guard === 399) return {pages, error: 'NON_TERMINATING'};
			}
		} catch (error) {
			return {pages, error: (error as Error).message};
		}
		return {pages, error: null};
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
		if (!ready) throw new Error('postgres not ready');
		raw = getDefaultPostgresClient();
		const kvClient = new PlainClient(raw, KV);
		const icuClient = new PlainClient(raw, ICU_KV);
		await ensurePostgresKvSchema(kvClient);
		await raw.query(`CREATE COLLATION ${ICU_COLLATION} (provider = icu, locale = 'en-US')`);
		await ensurePostgresKvSchema(icuClient);
		for (const column of ['row_key', 'partition_key']) {
			await raw.query(`ALTER TABLE ${ICU_KV} ALTER COLUMN ${column} TYPE text COLLATE ${ICU_COLLATION}`);
		}
		legacy = new LegacyPostgresKvQueryExecutor(kvClient);
		next = new PostgresKvQueryExecutor(kvClient);
		icuLegacy = new LegacyPostgresKvQueryExecutor(icuClient);
		icuNext = new PostgresKvQueryExecutor(icuClient);
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('pages a bare scan without skipping or repeating rows', async () => {
		await wipe(KV);
		for (let i = 0; i < 23; i += 1) await upsert(next, FlatTable, {k: BigInt(i), v: `v${i}`});
		for (const pageSize of [1, 2, 3, 5, 7, 23, 24, 100]) {
			const result = await pageAll(next, selectMeta(FlatTable), {}, pageSize);
			expect(result.error, `pageSize=${pageSize}`).toBeNull();
			const keys = result.pages.flat().map((row) => String(row.k));
			expect(new Set(keys).size, `dupes at pageSize=${pageSize}: ${keys.join(',')}`).toBe(23);
			expect(keys.length, `count at pageSize=${pageSize}`).toBe(23);
		}
	}, 300_000);

	it('pages a prefix range without skipping or repeating rows', async () => {
		await wipe(KV);
		for (let owner = 0; owner < 3; owner += 1) {
			for (let i = 0; i < 17; i += 1) {
				await upsert(next, PagedTable, {owner_id: BigInt(owner), item_id: BigInt(i), payload: `p${owner}-${i}`});
			}
		}
		const meta = selectMeta(PagedTable, [{kind: 'eq', col: 'owner_id', param: 'owner_id'} as WhereExpr<Row>]);
		for (const pageSize of [1, 2, 4, 17, 18]) {
			const result = await pageAll(next, meta, {owner_id: 1n}, pageSize);
			expect(result.error).toBeNull();
			const rows = result.pages.flat();
			expect(rows.length, `pageSize=${pageSize}`).toBe(17);
			expect(new Set(rows.map((r) => String(r.item_id))).size).toBe(17);
			expect(rows.every((r) => String(r.owner_id) === '1')).toBe(true);
		}
	}, 300_000);

	it('never loses a row that existed for the whole scan while rows are inserted between pages', async () => {
		await wipe(KV);
		const stable = new Set<string>();
		for (let i = 0; i < 30; i += 1) {
			await upsert(next, FlatTable, {k: BigInt(i), v: `v${i}`});
			stable.add(String(i));
		}
		let inserted = 100;
		const result = await pageAll(next, selectMeta(FlatTable), {}, 4, async () => {
			await upsert(next, FlatTable, {k: BigInt(inserted), v: `late${inserted}`});
			inserted += 1;
		});
		expect(result.error).toBeNull();
		const seen = result.pages.flat().map((r) => String(r.k));
		const dupes = seen.filter((key, index) => seen.indexOf(key) !== index);
		const missing = [...stable].filter((key) => !seen.includes(key));
		expect(missing, `missing stable rows: ${missing.join(',')}`).toEqual([]);
		expect(dupes, `duplicate rows: ${dupes.join(',')}`).toEqual([]);
	}, 300_000);

	it('pages identically to the legacy executor while rows are deleted between pages', async () => {
		const run = async (exec: AnyExec) => {
			await wipe(KV);
			for (let i = 0; i < 30; i += 1) await upsert(exec, FlatTable, {k: BigInt(i), v: `v${i}`});
			let pageIndex = 0;
			const result = await pageAll(exec, selectMeta(FlatTable), {}, 4, async () => {
				const victim = pageIndex;
				pageIndex += 1;
				await exec.executeQuery({
					cql: '__del__',
					params: {k: BigInt(victim)},
					kvMeta: deleteMeta(FlatTable, [{kind: 'eq', col: 'k', param: 'k'} as WhereExpr<Row>]),
				});
			});
			return {
				error: result.error,
				pages: result.pages.map((page) => page.map((row) => String(row.k)).join(',')).join('|'),
			};
		};
		const legacyRun = await run(legacy);
		const nextRun = await run(next);
		expect(nextRun.error).toBeNull();
		expect(nextRun, `legacy=${legacyRun.pages} next=${nextRun.pages}`).toEqual(legacyRun);
	}, 300_000);

	it('pages identically to the legacy executor when the last row of each page is deleted', async () => {
		const run = async (exec: AnyExec) => {
			await wipe(KV);
			for (let i = 0; i < 20; i += 1)
				await upsert(exec, FlatTable, {k: `key${String(i).padStart(2, '0')}`, v: `v${i}`});
			const meta = selectMeta(FlatTable);
			const seen: Array<string> = [];
			let pageState: string | null = null;
			for (let guard = 0; guard < 100; guard += 1) {
				const page: {rows: Array<Row>; pageState: string | null} = await exec.executePagedQuery<Row>(
					{cql: '__cur__', params: {}, kvMeta: meta},
					{pageSize: 3, pageState},
				);
				for (const row of page.rows) seen.push(String(row.k));
				const cursorRow = page.rows[page.rows.length - 1];
				pageState = page.pageState;
				if (cursorRow) {
					await exec.executeQuery({
						cql: '__delcur__',
						params: {k: cursorRow.k as string},
						kvMeta: deleteMeta(FlatTable, [{kind: 'eq', col: 'k', param: 'k'} as WhereExpr<Row>]),
					});
				}
				if (pageState === null) break;
			}
			return seen.join(',');
		};
		const legacySeen = await run(legacy);
		const nextSeen = await run(next);
		expect(nextSeen, `legacy=${legacySeen} next=${nextSeen}`).toBe(legacySeen);
	}, 300_000);

	it('never returns an empty page together with a non-null page state', async () => {
		await wipe(KV);
		for (let owner = 0; owner < 4; owner += 1) {
			for (let i = 0; i < 11; i += 1) {
				await upsert(next, PagedTable, {owner_id: BigInt(owner), item_id: BigInt(i), payload: `p${owner}-${i}`});
			}
		}
		const shapes: Array<[string, AnyMeta, CassandraParams]> = [
			['scan', selectMeta(PagedTable), {}],
			[
				'eq-owner',
				selectMeta(PagedTable, [{kind: 'eq', col: 'owner_id', param: 'owner_id'} as WhereExpr<Row>]),
				{owner_id: 2n},
			],
			[
				'in-owner-single',
				selectMeta(PagedTable, [{kind: 'in', col: 'owner_id', param: 'owner_ids'} as WhereExpr<Row>]),
				{owner_ids: [2n]},
			],
			[
				'in-owner-multi',
				selectMeta(PagedTable, [{kind: 'in', col: 'owner_id', param: 'owner_ids'} as WhereExpr<Row>]),
				{owner_ids: [1n, 3n]},
			],
			[
				'in-owner-empty',
				selectMeta(PagedTable, [{kind: 'in', col: 'owner_id', param: 'owner_ids'} as WhereExpr<Row>]),
				{owner_ids: []},
			],
		];
		const offenders: Array<string> = [];
		for (const [name, meta, params] of shapes) {
			for (const pageSize of [1, 2, 3, 5, 11]) {
				let pageState: string | null = null;
				const flat: Array<Row> = [];
				for (let guard = 0; guard < 200; guard += 1) {
					const page: {rows: Array<Row>; pageState: string | null} = await next.executePagedQuery<Row>(
						{cql: `__empty__${name}`, params, kvMeta: meta},
						{pageSize, pageState},
					);
					if (page.rows.length === 0 && page.pageState !== null) {
						offenders.push(`${name}/size${pageSize} page ${guard}`);
					}
					flat.push(...page.rows);
					pageState = page.pageState;
					if (pageState === null) break;
				}
				const unpaged = await next.executeQuery<Row>({cql: `__whole__${name}`, params, kvMeta: meta});
				expect(flat.length, `${name}/size${pageSize} row count`).toBe(unpaged.length);
				const pagedKeys = flat.map((r) => `${String(r.owner_id)}:${String(r.item_id)}`).sort();
				const wholeKeys = unpaged.map((r) => `${String(r.owner_id)}:${String(r.item_id)}`).sort();
				expect(pagedKeys, `${name}/size${pageSize} multiset`).toEqual(wholeKeys);
			}
		}
		expect(offenders, `empty page with continuation: ${offenders.join(', ')}`).toEqual([]);
	}, 300_000);

	it('matches legacy throw/no-throw behaviour for hand-crafted and corrupt page states', async () => {
		await wipe(KV);
		for (let i = 0; i < 9; i += 1) await upsert(next, FlatTable, {k: BigInt(i), v: `v${i}`});
		const meta = selectMeta(FlatTable);
		const query = {cql: '__tok__', params: {}, kvMeta: meta};
		const tokens: Array<[string, string]> = [
			['empty-object', token({})],
			['number', token(123)],
			['string', token('hello')],
			['array', token([1, 2])],
			['unknown-key', token({foo: 1})],
			['offset-negative', token({offset: -1})],
			['offset-fractional', token({offset: 1.5})],
			['offset-string', token({offset: '2'})],
			['offset-huge', token({offset: Number.MAX_SAFE_INTEGER})],
			['after-number', token({after: 5})],
			['after-null', token({after: null})],
			['after-object', token({after: {a: 1}})],
			['after-and-offset', token({after: 'x', offset: 3})],
			['garbage', 'not-base64-at-all'],
		];
		const deltas: Array<string> = [];
		for (const [name, value] of tokens) {
			const legacyResult = await legacy
				.executePagedQuery<Row>(query, {pageSize: 3, pageState: value})
				.then((r) => ({err: null as string | null, rows: r.rows.length}))
				.catch((e: Error) => ({err: e.message, rows: -1}));
			const nextResult = await next
				.executePagedQuery<Row>(query, {pageSize: 3, pageState: value})
				.then((r) => ({err: null as string | null, rows: r.rows.length}))
				.catch((e: Error) => ({err: e.message, rows: -1}));
			if ((legacyResult.err === null) !== (nextResult.err === null)) {
				deltas.push(
					`${name}: legacy=${legacyResult.err ?? `ok(${legacyResult.rows})`} next=${nextResult.err ?? `ok(${nextResult.rows})`}`,
				);
			}
		}
		expect(deltas, `page-state error behaviour deltas:\n${deltas.join('\n')}`).toEqual([]);
	}, 300_000);

	it('matches legacy behaviour for degenerate page sizes', async () => {
		await wipe(KV);
		for (let i = 0; i < 9; i += 1) await upsert(next, FlatTable, {k: BigInt(i), v: `v${i}`});
		const query = {cql: '__ps__', params: {}, kvMeta: selectMeta(FlatTable)};
		const deltas: Array<string> = [];
		for (const pageSize of [0, -1, 1.5, 2.9, Number.MAX_SAFE_INTEGER, 2 ** 31, Number.NaN]) {
			const l = await legacy
				.executePagedQuery<Row>(query, {pageSize})
				.then((r) => `ok rows=${r.rows.length} more=${r.pageState !== null}`)
				.catch((e: Error) => `throw ${e.message}`);
			const n = await next
				.executePagedQuery<Row>(query, {pageSize})
				.then((r) => `ok rows=${r.rows.length} more=${r.pageState !== null}`)
				.catch((e: Error) => `throw ${e.message}`);
			if (l !== n) deltas.push(`pageSize=${String(pageSize)}: legacy=${l} next=${n}`);
		}
		expect(deltas, `page size deltas:\n${deltas.join('\n')}`).toEqual([]);
	}, 300_000);

	it('does not silently truncate when a page token is reused across a different query', async () => {
		await wipe(KV);
		for (let owner = 0; owner < 3; owner += 1) {
			for (let i = 0; i < 9; i += 1) {
				await upsert(next, PagedTable, {owner_id: BigInt(owner), item_id: BigInt(i), payload: `p${owner}-${i}`});
			}
		}
		const meta = selectMeta(PagedTable, [{kind: 'eq', col: 'owner_id', param: 'owner_id'} as WhereExpr<Row>]);
		const firstNext = await next.executePagedQuery<Row>(
			{cql: '__x__', params: {owner_id: 0n}, kvMeta: meta},
			{pageSize: 3},
		);
		const firstLegacy = await legacy.executePagedQuery<Row>(
			{cql: '__x__', params: {owner_id: 0n}, kvMeta: meta},
			{pageSize: 3},
		);
		expect(firstNext.pageState).not.toBeNull();
		const crossNext = await next.executePagedQuery<Row>(
			{cql: '__x__', params: {owner_id: 2n}, kvMeta: meta},
			{pageSize: 3, pageState: firstNext.pageState},
		);
		const crossLegacy = await legacy.executePagedQuery<Row>(
			{cql: '__x__', params: {owner_id: 2n}, kvMeta: meta},
			{pageSize: 3, pageState: firstLegacy.pageState},
		);
		expect(crossNext.rows.length, 'cross-query token row count diverges from legacy').toBe(crossLegacy.rows.length);
	}, 300_000);

	it('does not restart the scan when a keyset token reaches a non-keyset plan', async () => {
		await wipe(KV);
		for (let owner = 0; owner < 3; owner += 1) {
			for (let i = 0; i < 9; i += 1) {
				await upsert(next, PagedTable, {owner_id: BigInt(owner), item_id: BigInt(i), payload: `p${owner}-${i}`});
			}
		}
		const inMeta = selectMeta(PagedTable, [{kind: 'in', col: 'owner_id', param: 'owner_ids'} as WhereExpr<Row>]);
		const first = await next.executePagedQuery<Row>(
			{cql: '__flip__', params: {owner_ids: [1n]}, kvMeta: inMeta},
			{pageSize: 3},
		);
		expect(first.pageState).not.toBeNull();
		const flipped = await next.executePagedQuery<Row>(
			{cql: '__flip__', params: {owner_ids: [1n, 2n]}, kvMeta: inMeta},
			{pageSize: 3, pageState: first.pageState},
		);
		const firstKeys = first.rows.map((r) => String(r.item_id));
		const flippedKeys = flipped.rows.map((r) => String(r.item_id));
		const overlap = flippedKeys.filter((key) => firstKeys.includes(key));
		expect(overlap, `keyset token silently restarted the scan: first=${firstKeys} flipped=${flippedKeys}`).toEqual([]);
	}, 300_000);

	it('pages correctly on a database whose row_key column has a linguistic collation', async () => {
		await wipe(ICU_KV);
		const keys = ['a', 'A', 'a b', 'ab', 'B', 'b', '\u{1f600}', 'z', 'Z', '', '"', '\\', 'ab'];
		for (const key of keys) await upsert(icuNext, FlatTable, {k: key, v: `v:${key}`});
		for (const pageSize of [1, 2, 3, 5]) {
			const result = await pageAll(icuNext, selectMeta(FlatTable), {}, pageSize);
			expect(result.error, `icu pageSize=${pageSize}`).toBeNull();
			const seen = result.pages.flat().map((r) => String(r.k));
			const dupes = seen.filter((key, index) => seen.indexOf(key) !== index);
			expect(dupes, `icu dupes pageSize=${pageSize}: ${JSON.stringify(dupes)}`).toEqual([]);
			expect(seen.length, `icu count pageSize=${pageSize}`).toBe(keys.length);
		}
		const legacyAll = await pageAll(icuLegacy, selectMeta(FlatTable), {}, 3);
		expect(legacyAll.pages.flat().length).toBe(keys.length);
	}, 300_000);

	it('reports the paged order delta against legacy for key shapes that reach real callers', async () => {
		const DatedTable: KvTableSpec<Row> = {
			name: 'pagead_dated',
			columns: ['customer_id', 'created_at', 'provider_id', 'total'],
			primaryKey: ['customer_id', 'created_at', 'provider_id'],
			partitionKey: ['customer_id'],
		};
		const NumericTable: KvTableSpec<Row> = {
			name: 'pagead_numeric',
			columns: ['owner_id', 'seq', 'payload'],
			primaryKey: ['owner_id', 'seq'],
			partitionKey: ['owner_id'],
		};
		await wipe(KV);
		for (let i = 0; i < 15; i += 1) {
			await upsert(next, DatedTable, {
				customer_id: 'cus_1',
				created_at: new Date(Date.UTC(2024, 0, 1 + i, i, i)),
				provider_id: `in_${String(i).padStart(3, '0')}`,
				total: BigInt(i),
			});
			await upsert(next, NumericTable, {owner_id: 7n, seq: BigInt(i), payload: `p${i}`});
		}
		const datedMeta = selectMeta(DatedTable, [
			{kind: 'eq', col: 'customer_id', param: 'customer_id'} as WhereExpr<Row>,
		]);
		const numericMeta = selectMeta(NumericTable, [{kind: 'eq', col: 'owner_id', param: 'owner_id'} as WhereExpr<Row>]);
		const datedLegacy = await pageAll(legacy, datedMeta, {customer_id: 'cus_1'}, 4);
		const datedNext = await pageAll(next, datedMeta, {customer_id: 'cus_1'}, 4);
		const numericLegacy = await pageAll(legacy, numericMeta, {owner_id: 7n}, 4);
		const numericNext = await pageAll(next, numericMeta, {owner_id: 7n}, 4);
		const datedLegacyOrder = datedLegacy.pages.flat().map((r) => String(r.provider_id));
		const datedNextOrder = datedNext.pages.flat().map((r) => String(r.provider_id));
		const numericLegacyOrder = numericLegacy.pages.flat().map((r) => String(r.seq));
		const numericNextOrder = numericNext.pages.flat().map((r) => String(r.seq));
		const deltas: Array<string> = [];
		if (datedLegacyOrder.join(',') !== datedNextOrder.join(',')) {
			deltas.push(`date-keyed: legacy=${datedLegacyOrder.join(',')} next=${datedNextOrder.join(',')}`);
		}
		if (numericLegacyOrder.join(',') !== numericNextOrder.join(',')) {
			deltas.push(`bigint-keyed: legacy=${numericLegacyOrder.join(',')} next=${numericNextOrder.join(',')}`);
		}
		expect(datedNextOrder.slice().sort(), 'date-keyed row set').toEqual(datedLegacyOrder.slice().sort());
		expect(numericNextOrder.slice().sort(), 'bigint-keyed row set').toEqual(numericLegacyOrder.slice().sort());
		expect(deltas, `paged order deltas:\n${deltas.join('\n')}`).toEqual([]);
	}, 300_000);

	it('pages a prefix range whose keys sit adjacent to the range bounds', async () => {
		await wipe(KV);
		const owners = ['a', 'a b', 'ab', 'a', 'a"'];
		for (const owner of owners) {
			for (let i = 0; i < 5; i += 1) {
				await upsert(next, PagedTable, {owner_id: owner, item_id: BigInt(i), payload: `${owner}#${i}`});
			}
		}
		const meta = selectMeta(PagedTable, [{kind: 'eq', col: 'owner_id', param: 'owner_id'} as WhereExpr<Row>]);
		for (const owner of owners) {
			for (const pageSize of [1, 2, 5]) {
				const result = await pageAll(next, meta, {owner_id: owner}, pageSize);
				expect(result.error).toBeNull();
				const rows = result.pages.flat();
				expect(rows.length, `owner=${JSON.stringify(owner)} pageSize=${pageSize}`).toBe(5);
				expect(
					rows.every((r) => r.owner_id === owner),
					`owner leak for ${JSON.stringify(owner)}`,
				).toBe(true);
			}
		}
	}, 300_000);
});
