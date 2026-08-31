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

const LEGACY_TABLE = 'kvns_legacy';
const NEXT_TABLE = 'kvns_next';
const CONTAINER = `fluxer-kvns-${process.pid.toString(36)}-${Date.now().toString(36)}`;
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

const Parent: KvTableSpec<Row> = {
	name: 'ns_parent',
	columns: ['user_id', 'channel_id', 'note', 'blob_'],
	primaryKey: ['user_id', 'channel_id'],
	partitionKey: ['user_id', 'channel_id'],
};

const Single: KvTableSpec<Row> = {
	name: 'ns_single',
	columns: ['token_', 'note'],
	primaryKey: ['token_'],
	partitionKey: ['token_'],
};

function eq(col: string, param: string): WhereExpr<Row> {
	return {kind: 'eq', col, param} as WhereExpr<Row>;
}
function inClause(col: string, param: string): WhereExpr<Row> {
	return {kind: 'in', col, param} as WhereExpr<Row>;
}

describe.skipIf(!dockerAvailable)('postgres kv non-select drift', () => {
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
		const result = await raw.query<{row_key: string; partition_key: string; row_data: unknown; expires_at: Date}>(
			`SELECT row_key, partition_key, row_data, expires_at FROM ${kv} WHERE table_name = $1 ORDER BY row_key COLLATE "C"`,
			[tableName],
		);
		return JSON.stringify(
			result.rows.map((r) => [
				r.row_key,
				r.partition_key,
				r.row_data,
				r.expires_at === null ? null : r.expires_at instanceof Date ? 'ts' : String(r.expires_at),
			]),
		);
	}

	async function reset(spec: KvTableSpec<Row>, rows: ReadonlyArray<Row>): Promise<void> {
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [spec.name]);
		}
		const meta = {action: 'upsert', table: spec} as AnyMeta;
		for (const row of rows) {
			await legacy.executeQuery({cql: '__seed__', params: row as CassandraParams, kvMeta: meta});
			await next.executeQuery({cql: '__seed__', params: row as CassandraParams, kvMeta: meta});
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
		if (!ready) throw new Error('postgres never came up');
		raw = getDefaultPostgresClient();
		const legacyClient = new TableClient(raw, LEGACY_TABLE);
		const nextClient = new TableClient(raw, NEXT_TABLE);
		await ensurePostgresKvSchema(legacyClient);
		await ensurePostgresKvSchema(nextClient);
		legacy = new LegacyPostgresKvQueryExecutor(legacyClient);
		next = new PostgresKvQueryExecutor(nextClient);
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	const seedRows: Array<Row> = [
		{user_id: 1n, channel_id: 10n, note: 'a', blob_: null},
		{user_id: 1n, channel_id: 11n, note: 'b', blob_: null},
		{user_id: 2n, channel_id: 10n, note: 'c', blob_: null},
	];

	it('upsert / insert / ifNotExists behave identically', async () => {
		await reset(Parent, seedRows);
		const insMeta = {action: 'upsert', table: Parent, ifNotExists: true} as AnyMeta;
		const fresh = {user_id: 3n, channel_id: 30n, note: 'z', blob_: null};
		const l1 = await legacy.executeQuery({cql: '__i__', params: fresh as CassandraParams, kvMeta: insMeta});
		const n1 = await next.executeQuery({cql: '__i__', params: fresh as CassandraParams, kvMeta: insMeta});
		expect(n1).toEqual(l1);
		const l2 = await legacy.executeQuery({cql: '__i__', params: fresh as CassandraParams, kvMeta: insMeta});
		const n2 = await next.executeQuery({cql: '__i__', params: fresh as CassandraParams, kvMeta: insMeta});
		expect(n2).toEqual(l2);
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('patch with and without ttl leaves identical state', async () => {
		await reset(Parent, seedRows);
		const patchMeta = {
			action: 'patch',
			table: Parent,
			patchKeys: ['note'],
			pkColumns: ['user_id', 'channel_id'],
		} as unknown as AnyMeta;
		const p = {user_id: 1n, channel_id: 10n, note: 'patched'} as CassandraParams;
		await legacy.executeQuery({cql: '__p__', params: p, kvMeta: patchMeta});
		await next.executeQuery({cql: '__p__', params: p, kvMeta: patchMeta});
		const ttlMeta = {...patchMeta, ttlParamName: 'ttl_'} as unknown as AnyMeta;
		const pt = {user_id: 2n, channel_id: 10n, note: 'ttl', ttl_: 600} as CassandraParams;
		await legacy.executeQuery({cql: '__pt__', params: pt, kvMeta: ttlMeta});
		await next.executeQuery({cql: '__pt__', params: pt, kvMeta: ttlMeta});
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('partial upsert merges with the stored row the way legacy does', async () => {
		await reset(Parent, seedRows);
		const meta = {action: 'upsert', table: Parent} as AnyMeta;
		const partial = {user_id: 1n, channel_id: 10n, blob_: 'merged'} as CassandraParams;
		await legacy.executeQuery({cql: '__mu__', params: partial, kvMeta: meta});
		await next.executeQuery({cql: '__mu__', params: partial, kvMeta: meta});
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
		const merged = await raw.query<{note: string | null; blob_: string | null}>(
			`SELECT row_data->>'note' AS note, row_data->>'blob_' AS blob_ FROM ${NEXT_TABLE} WHERE table_name = $1 AND row_data->>'blob_' IS NOT NULL`,
			[Parent.name],
		);
		expect(merged.rows).toEqual([{note: 'a', blob_: 'merged'}]);
	});

	it('upsert and patch replace an expired row instead of merging into it', async () => {
		await reset(Parent, seedRows);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`UPDATE ${kv} SET expires_at = timestamptz '2000-01-01T00:00:00Z' WHERE table_name = $1`, [
				Parent.name,
			]);
		}
		const upsertMeta = {action: 'upsert', table: Parent} as AnyMeta;
		const patchMeta = {
			action: 'patch',
			table: Parent,
			patchKeys: ['note'],
			pkColumns: ['user_id', 'channel_id'],
		} as unknown as AnyMeta;
		const u = {user_id: 1n, channel_id: 10n, blob_: 'fresh'} as CassandraParams;
		const p = {user_id: 2n, channel_id: 10n, note: 'fresh'} as CassandraParams;
		for (const exec of [legacy, next]) {
			await exec.executeQuery({cql: '__xu__', params: u, kvMeta: upsertMeta});
			await exec.executeQuery({cql: '__xp__', params: p, kvMeta: patchMeta});
		}
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
		const rows = await raw.query<{note: string | null; blob_: string | null; expires_at: Date | null}>(
			`SELECT row_data->>'note' AS note, row_data->>'blob_' AS blob_, expires_at FROM ${NEXT_TABLE} WHERE table_name = $1 ORDER BY row_key COLLATE "C"`,
			[Parent.name],
		);
		expect(rows.rows.map((row) => [row.note, row.blob_, row.expires_at === null])).toEqual([
			[null, 'fresh', true],
			['b', null, false],
			['fresh', null, true],
		]);
	});

	it('patch without a ttl parameter keeps the stored expiry', async () => {
		await reset(Parent, seedRows);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`UPDATE ${kv} SET expires_at = timestamptz '2099-01-01T00:00:00Z' WHERE table_name = $1`, [
				Parent.name,
			]);
		}
		const patchMeta = {
			action: 'patch',
			table: Parent,
			patchKeys: ['note'],
			pkColumns: ['user_id', 'channel_id'],
		} as unknown as AnyMeta;
		const p = {user_id: 1n, channel_id: 10n, note: 'kept'} as CassandraParams;
		await legacy.executeQuery({cql: '__pk__', params: p, kvMeta: patchMeta});
		await next.executeQuery({cql: '__pk__', params: p, kvMeta: patchMeta});
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
		const kept = await raw.query<{expires_at: Date | null}>(
			`SELECT expires_at FROM ${NEXT_TABLE} WHERE table_name = $1 AND row_data->>'note' = 'kept'`,
			[Parent.name],
		);
		expect(kept.rows.map((row) => row.expires_at?.toISOString() ?? null)).toEqual(['2099-01-01T00:00:00.000Z']);
	});

	it('patch with a ttl parameter replaces the stored expiry', async () => {
		await reset(Parent, seedRows);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`UPDATE ${kv} SET expires_at = timestamptz '2099-01-01T00:00:00Z' WHERE table_name = $1`, [
				Parent.name,
			]);
		}
		const ttlMeta = {
			action: 'patch',
			table: Parent,
			patchKeys: ['note'],
			pkColumns: ['user_id', 'channel_id'],
			ttlParamName: 'ttl_',
		} as unknown as AnyMeta;
		const p = {user_id: 1n, channel_id: 10n, note: 'ttl', ttl_: 600} as CassandraParams;
		const before = Date.now();
		await legacy.executeQuery({cql: '__pt2__', params: p, kvMeta: ttlMeta});
		await next.executeQuery({cql: '__pt2__', params: p, kvMeta: ttlMeta});
		const after = Date.now();
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
		const set = await raw.query<{expires_at: Date}>(
			`SELECT expires_at FROM ${NEXT_TABLE} WHERE table_name = $1 AND row_data->>'note' = 'ttl'`,
			[Parent.name],
		);
		expect(set.rows.length).toBe(1);
		const expiry = set.rows[0]!.expires_at.getTime();
		expect(expiry).toBeGreaterThanOrEqual(before + 600_000);
		expect(expiry).toBeLessThanOrEqual(after + 600_000);
	});

	it('insert if not exists over an expired row matches legacy', async () => {
		await reset(Parent, seedRows);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`UPDATE ${kv} SET expires_at = now() - interval '1 hour' WHERE table_name = $1`, [Parent.name]);
		}
		const insMeta = {action: 'upsert', table: Parent, ifNotExists: true} as AnyMeta;
		const revived = {user_id: 1n, channel_id: 10n, note: 'revived', blob_: null} as CassandraParams;
		const l1 = await legacy.executeQuery({cql: '__ie__', params: revived, kvMeta: insMeta});
		const n1 = await next.executeQuery({cql: '__ie__', params: revived, kvMeta: insMeta});
		expect(n1).toEqual(l1);
		expect(n1).toEqual([{'[applied]': true}]);
		const l2 = await legacy.executeQuery({cql: '__ie__', params: revived, kvMeta: insMeta});
		const n2 = await next.executeQuery({cql: '__ie__', params: revived, kvMeta: insMeta});
		expect(n2).toEqual(l2);
		expect(n2).toEqual([{'[applied]': false}]);
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('count returns the same value and the same javascript type', async () => {
		await reset(Parent, seedRows);
		await raw.query(
			`UPDATE ${NEXT_TABLE} SET expires_at = now() - interval '1 hour' WHERE table_name = $1 AND row_key LIKE '%11%'`,
			[Parent.name],
		);
		await raw.query(
			`UPDATE ${LEGACY_TABLE} SET expires_at = now() - interval '1 hour' WHERE table_name = $1 AND row_key LIKE '%11%'`,
			[Parent.name],
		);
		const meta = {action: 'count', table: Parent, where: [eq('user_id', 'user_id')]} as AnyMeta;
		const l = await legacy.executeQuery<{count: unknown}>({cql: '__c__', params: {user_id: 1n}, kvMeta: meta});
		const n = await next.executeQuery<{count: unknown}>({cql: '__c__', params: {user_id: 1n}, kvMeta: meta});
		expect(n).toEqual(l);
		expect(typeof (n[0] as {count: unknown}).count).toBe(typeof (l[0] as {count: unknown}).count);
	});

	it('count over an empty logical table matches', async () => {
		const meta = {action: 'count', table: Single, where: []} as AnyMeta;
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`DELETE FROM ${kv} WHERE table_name = $1`, [Single.name]);
		}
		const l = await legacy.executeQuery({cql: '__ce__', params: {}, kvMeta: meta});
		const n = await next.executeQuery({cql: '__ce__', params: {}, kvMeta: meta});
		expect(n).toEqual(l);
	});

	it('count with a string bound to an IN parameter matches legacy', async () => {
		await reset(Single, [
			{token_: 'a', note: '1'},
			{token_: 'b', note: '2'},
		]);
		const meta = {action: 'count', table: Single, where: [inClause('token_', 'tokens')]} as AnyMeta;
		let legacyOut: unknown;
		let nextOut: unknown;
		try {
			legacyOut = await legacy.executeQuery({cql: '__cs__', params: {tokens: 'ab'} as CassandraParams, kvMeta: meta});
		} catch (error) {
			legacyOut = `throw:${(error as Error).constructor.name}`;
		}
		try {
			nextOut = await next.executeQuery({cql: '__cs__', params: {tokens: 'ab'} as CassandraParams, kvMeta: meta});
		} catch (error) {
			nextOut = `throw:${(error as Error).constructor.name}`;
		}
		expect(nextOut).toEqual(legacyOut);
	});

	it('rejects malformed page tokens the way legacy does', async () => {
		await reset(Single, [
			{token_: 'a', note: '1'},
			{token_: 'b', note: '2'},
			{token_: 'c', note: '3'},
		]);
		const meta = {action: 'select', table: Single, where: [], columns: Single.columns} as AnyMeta;
		const tokens = ['eyJmb28iOjF9', 'e30', 'MTIz', 'InN0ciI', 'eyJvZmZzZXQiOi0xfQ', 'eyJvZmZzZXQiOjF9'];
		const legacyOut: Array<string> = [];
		const nextOut: Array<string> = [];
		for (const token of tokens) {
			for (const [exec, sink] of [
				[legacy, legacyOut],
				[next, nextOut],
			] as const) {
				try {
					const page = await exec.executePagedQuery(
						{cql: '__pg__', params: {}, kvMeta: meta},
						{pageSize: 1, pageState: token},
					);
					sink.push(`${token} -> rows=${page.rows.length}`);
				} catch (error) {
					sink.push(`${token} -> throw:${(error as Error).message}`);
				}
			}
		}
		expect(nextOut).toEqual(legacyOut);
	});

	it('delete inside an atomic batch rolls back identically', async () => {
		await reset(Parent, seedRows);
		const delMeta = {action: 'delete', table: Parent, where: [eq('user_id', 'user_id')]} as AnyMeta;
		for (const [exec, kv] of [
			[legacy, LEGACY_TABLE],
			[next, NEXT_TABLE],
		] as const) {
			await expect(
				exec.executeBatch([
					{query: '__d__', params: {user_id: 1n}, meta: delMeta as KvQueryMeta},
					{query: '__boom__', params: {}, meta: {action: 'bogus'} as unknown as KvQueryMeta},
				]),
			).rejects.toThrow();
			void kv;
		}
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('delete of an expired-but-unpruned row matches legacy', async () => {
		await reset(Parent, seedRows);
		for (const kv of [LEGACY_TABLE, NEXT_TABLE]) {
			await raw.query(`UPDATE ${kv} SET expires_at = now() - interval '1 hour' WHERE table_name = $1`, [Parent.name]);
		}
		const delMeta = {action: 'delete', table: Parent, where: [eq('user_id', 'user_id')]} as AnyMeta;
		await legacy.executeQuery({cql: '__d__', params: {user_id: 1n}, kvMeta: delMeta});
		await next.executeQuery({cql: '__d__', params: {user_id: 1n}, kvMeta: delMeta});
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('delete with no where clause matches legacy', async () => {
		await reset(Parent, seedRows);
		const delMeta = {action: 'delete', table: Parent, where: []} as AnyMeta;
		await legacy.executeQuery({cql: '__da__', params: {}, kvMeta: delMeta});
		await next.executeQuery({cql: '__da__', params: {}, kvMeta: delMeta});
		expect(await dump(NEXT_TABLE, Parent.name)).toBe(await dump(LEGACY_TABLE, Parent.name));
	});

	it('delete does not touch a neighbouring logical table', async () => {
		await reset(Parent, seedRows);
		await reset(Single, [{token_: 'a', note: '1'}]);
		const delMeta = {action: 'delete', table: Parent, where: []} as AnyMeta;
		await next.executeQuery({cql: '__da2__', params: {}, kvMeta: delMeta});
		const left = await raw.query<{count: string}>(`SELECT count(*) AS count FROM ${NEXT_TABLE} WHERE table_name = $1`, [
			Single.name,
		]);
		expect(left.rows[0]!.count).toBe('1');
	});
});
