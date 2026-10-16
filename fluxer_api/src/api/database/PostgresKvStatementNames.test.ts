// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CassandraParams, KvQueryMeta, KvTableSpec, WhereExpr} from '@app/api/database/CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {dockerAvailable, startPostgresKv, stopPostgresKv} from '@app/api/test/DockerTestContainer';
import {getDefaultPostgresClient, type IPostgresClient, initPostgres} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

type Row = Record<string, unknown>;

interface Statement {
	text: string;
	name: string | undefined;
}

const KV_TABLE = 'kv_stmt_names';
const KV_TABLE_POOLED = 'kv_stmt_names_pooled';
const Composite: KvTableSpec<Row> = {
	name: 'stmt_composite',
	columns: ['owner_id', 'item_id', 'payload'],
	primaryKey: ['owner_id', 'item_id'],
	partitionKey: ['owner_id'],
};

const Expiring: KvTableSpec<Row> = {...Composite, name: 'stmt_expiring', defaultTtlSeconds: 600};

const Bucketed: KvTableSpec<Row> = {
	name: 'stmt_bucketed',
	columns: ['bucket', 'item_id', 'payload'],
	primaryKey: ['item_id'],
	partitionKey: ['bucket'],
};

function recordingClient(statements: Array<Statement>): IPostgresClient {
	const client = {
		async query(text: string, _values?: Array<unknown>, name?: string) {
			statements.push({text, name});
			const rows = text.startsWith('SELECT kv.row_key')
				? [{row_key: 'stmt_row', row_data: {owner_id: 'o1', item_id: 'i1', payload: 'p1'}}]
				: [];
			return {rows, rowCount: rows.length};
		},
		async connect() {},
		async shutdown() {},
		async transaction(fn: (db: unknown) => Promise<unknown>) {
			return fn(client);
		},
		kvTable() {
			return KV_TABLE;
		},
	} as never;
	return client;
}

function meta(spec: KvTableSpec<Row>, action: string, where: Array<WhereExpr<Row>>, extra: Row = {}): KvQueryMeta<Row> {
	return {action, table: spec, where, columns: spec.columns, ...extra} as unknown as KvQueryMeta<Row>;
}

const eq = (col: string): WhereExpr<Row> => ({kind: 'eq', col, param: col}) as WhereExpr<Row>;
const isIn = (col: string): WhereExpr<Row> => ({kind: 'in', col, param: col}) as WhereExpr<Row>;

const OWNER_ITEM = {owner_id: 'o1', item_id: 'i1', payload: 'p1'} as CassandraParams;
const BUCKET = {bucket: 'b1', item_id: 'i1', payload: 'p1'} as CassandraParams;

async function runShapes(): Promise<Array<Statement>> {
	const statements: Array<Statement> = [];
	const executor = new PostgresKvQueryExecutor(recordingClient(statements));
	const cases: Array<[KvQueryMeta<Row>, CassandraParams]> = [
		[meta(Composite, 'select', [eq('owner_id'), eq('item_id')]), OWNER_ITEM],
		[meta(Composite, 'select', [eq('owner_id')]), OWNER_ITEM],
		[meta(Composite, 'select', [isIn('owner_id')]), {owner_id: ['o1', 'o2']} as CassandraParams],
		[meta(Composite, 'select', []), {} as CassandraParams],
		[meta(Bucketed, 'select', [eq('bucket')]), BUCKET],
		[meta(Bucketed, 'select', [isIn('bucket')]), {bucket: ['b1', 'b2']} as CassandraParams],
		[meta(Composite, 'count', [eq('owner_id'), eq('item_id')]), OWNER_ITEM],
		[meta(Composite, 'count', [eq('owner_id')]), OWNER_ITEM],
		[meta(Composite, 'count', [isIn('owner_id')]), {owner_id: ['o1', 'o2']} as CassandraParams],
		[meta(Composite, 'count', []), {} as CassandraParams],
		[meta(Bucketed, 'count', [eq('bucket')]), BUCKET],
		[meta(Bucketed, 'count', [isIn('bucket')]), {bucket: ['b1', 'b2']} as CassandraParams],
		[meta(Composite, 'delete', [eq('owner_id'), eq('item_id')]), OWNER_ITEM],
		[meta(Composite, 'delete', [eq('owner_id'), eq('payload')]), OWNER_ITEM],
		[meta(Composite, 'upsert', []), OWNER_ITEM],
		[meta(Composite, 'upsert', [], {ifNotExists: true}), OWNER_ITEM],
		[meta(Composite, 'patch', [eq('owner_id'), eq('item_id')], {patchKeys: ['payload']}), OWNER_ITEM],
		[
			meta(Composite, 'patch', [eq('owner_id'), eq('item_id')], {patchKeys: ['payload'], ttlParamName: 'ttl_'}),
			{...OWNER_ITEM, ttl_: 600} as CassandraParams,
		],
		[meta(Expiring, 'patch', [eq('owner_id'), eq('item_id')], {patchKeys: ['payload']}), OWNER_ITEM],
	];
	for (const [kvMeta, params] of cases) {
		await executor.executeQuery({cql: `__stmt_${kvMeta.action}`, params, kvMeta: kvMeta as KvQueryMeta});
	}
	return statements;
}

describe('PostgresKvQueryExecutor statement names', () => {
	it('names every key-pinned statement shape exactly once', async () => {
		const statements = await runShapes();
		const named = new Map<string, string>();
		for (const statement of statements) {
			if (statement.name === undefined) continue;
			const seen = named.get(statement.name);
			expect(seen ?? statement.text).toBe(statement.text);
			named.set(statement.name, statement.text);
		}
		expect([...named.keys()].sort()).toEqual([
			'kv_count_range',
			'kv_count_rowkeys',
			'kv_del_expired',
			'kv_del_keys',
			'kv_del_rowkeys',
			'kv_get_row',
			'kv_patch_default_ttl',
			'kv_patch_keep_ttl',
			'kv_patch_set_ttl',
			'kv_sel_range',
			'kv_sel_rowkeys',
			'kv_upsert',
		]);
	});

	it('leaves the OR-of-ranges, partition and scan shapes unnamed', async () => {
		const statements = await runShapes();
		const shapes = [' OR (', 'kv.partition_key = $2', 'kv.partition_key = ANY($2::text[])', '$1 AND (kv.expires_at'];
		for (const shape of shapes) {
			const matching = statements.filter((statement) => statement.text.includes(shape));
			expect(matching.length).toBeGreaterThan(0);
			expect(matching.filter((statement) => statement.name !== undefined)).toEqual([]);
		}
	});
});

async function exerciseKvShapes(executor: PostgresKvQueryExecutor): Promise<void> {
	for (let index = 0; index < 4; index += 1) {
		await executor.executeQuery({
			cql: '__stmt_seed',
			params: {owner_id: `o${index % 2}`, item_id: `i${index}`, payload: `p${index}`} as CassandraParams,
			kvMeta: meta(Composite, 'upsert', []) as KvQueryMeta,
		});
	}
	for (let index = 0; index < 12; index += 1) {
		const point = await executor.executeQuery<Row>({
			cql: '__stmt_point',
			params: {owner_id: 'o0', item_id: 'i0'} as CassandraParams,
			kvMeta: meta(Composite, 'select', [eq('owner_id'), eq('item_id')]) as KvQueryMeta,
		});
		expect(point.map((row) => row.payload)).toEqual(['p0']);
		const range = await executor.executeQuery<Row>({
			cql: '__stmt_range',
			params: {owner_id: 'o0'} as CassandraParams,
			kvMeta: meta(Composite, 'select', [eq('owner_id')]) as KvQueryMeta,
		});
		expect(range).toHaveLength(2);
	}
	await executor.executeQuery({
		cql: '__stmt_patch',
		params: {owner_id: 'o0', item_id: 'i0', payload: 'patched'} as CassandraParams,
		kvMeta: meta(Composite, 'patch', [eq('owner_id'), eq('item_id')], {patchKeys: ['payload']}) as KvQueryMeta,
	});
	const reapplied = await executor.executeQuery<Row>({
		cql: '__stmt_lwt',
		params: {owner_id: 'o0', item_id: 'i0', payload: 'ignored'} as CassandraParams,
		kvMeta: meta(Composite, 'upsert', [], {ifNotExists: true}) as KvQueryMeta,
	});
	expect(reapplied).toEqual([{'[applied]': false}]);
	const claimed = await executor.executeQuery<Row>({
		cql: '__stmt_lwt',
		params: {owner_id: 'o9', item_id: 'i9', payload: 'claimed'} as CassandraParams,
		kvMeta: meta(Composite, 'upsert', [], {ifNotExists: true}) as KvQueryMeta,
	});
	expect(claimed).toEqual([{'[applied]': true}]);
	await executor.executeQuery({
		cql: '__stmt_patch_ttl',
		params: {owner_id: 'o9', item_id: 'i9', payload: 'expiring', ttl_: 600} as CassandraParams,
		kvMeta: meta(Composite, 'patch', [eq('owner_id'), eq('item_id')], {
			patchKeys: ['payload'],
			ttlParamName: 'ttl_',
		}) as KvQueryMeta,
	});
	const expiring = await executor.executeQuery<Row>({
		cql: '__stmt_point',
		params: {owner_id: 'o9', item_id: 'i9'} as CassandraParams,
		kvMeta: meta(Composite, 'select', [eq('owner_id'), eq('item_id')]) as KvQueryMeta,
	});
	expect(expiring.map((row) => row.payload)).toEqual(['expiring']);
	const patched = await executor.executeQuery<Row>({
		cql: '__stmt_point',
		params: {owner_id: 'o0', item_id: 'i0'} as CassandraParams,
		kvMeta: meta(Composite, 'select', [eq('owner_id'), eq('item_id')]) as KvQueryMeta,
	});
	expect(patched.map((row) => row.payload)).toEqual(['patched']);
	await executor.executeQuery({
		cql: '__stmt_patch_default_ttl',
		params: {owner_id: 'o5', item_id: 'i5', payload: 'defaulted'} as CassandraParams,
		kvMeta: meta(Expiring, 'patch', [eq('owner_id'), eq('item_id')], {patchKeys: ['payload']}) as KvQueryMeta,
	});
	const defaulted = await executor.executeQuery<Row>({
		cql: '__stmt_point',
		params: {owner_id: 'o5', item_id: 'i5'} as CassandraParams,
		kvMeta: meta(Expiring, 'select', [eq('owner_id'), eq('item_id')]) as KvQueryMeta,
	});
	expect(defaulted.map((row) => row.payload)).toEqual(['defaulted']);
	await executor.executeQuery({
		cql: '__stmt_delete',
		params: {owner_id: 'o0', item_id: 'i0'} as CassandraParams,
		kvMeta: meta(Composite, 'delete', [eq('owner_id'), eq('item_id')]) as KvQueryMeta,
	});
	const remaining = await executor.executeQuery<Row>({
		cql: '__stmt_range',
		params: {owner_id: 'o0'} as CassandraParams,
		kvMeta: meta(Composite, 'select', [eq('owner_id')]) as KvQueryMeta,
	});
	expect(remaining).toHaveLength(1);
	await executor.executeBatch([
		{
			query: '__stmt_batch',
			params: {owner_id: 'o7', item_id: 'i7', payload: 'batched'},
			meta: meta(Composite, 'upsert', []) as KvQueryMeta,
		},
		{
			query: '__stmt_batch',
			params: {owner_id: 'o7', item_id: 'i8', payload: 'batched'},
			meta: meta(Composite, 'upsert', []) as KvQueryMeta,
		},
	]);
	const batched = await executor.executeQuery<Row>({
		cql: '__stmt_range',
		params: {owner_id: 'o7'} as CassandraParams,
		kvMeta: meta(Composite, 'select', [eq('owner_id')]) as KvQueryMeta,
	});
	expect(batched.map((row) => row.payload)).toEqual(['batched', 'batched']);
}

describe.skipIf(!dockerAvailable)('PostgresKvQueryExecutor statement names against postgres', () => {
	let raw: IPostgresClient;
	let executor: PostgresKvQueryExecutor;
	let port: number;

	let postgresContainer: string;
	beforeAll(async () => {
		const started = await startPostgresKv({namePrefix: 'fluxer-kvstmt', kvTable: KV_TABLE, maxConnections: 1});
		postgresContainer = started.container;
		port = started.port;
		raw = started.client;
		executor = new PostgresKvQueryExecutor(raw);
	}, 900_000);

	afterAll(async () => {
		await stopPostgresKv(postgresContainer);
	});

	it('prepares each named shape server side and keeps reading the right rows', async () => {
		await exerciseKvShapes(executor);
		const prepared = await raw.query<{name: string}>('SELECT name FROM pg_prepared_statements ORDER BY name');
		expect(prepared.rows.map((row) => row.name)).toEqual([
			'kv_del_expired',
			'kv_del_rowkeys',
			'kv_get_row',
			'kv_patch_default_ttl',
			'kv_patch_keep_ttl',
			'kv_patch_set_ttl',
			'kv_sel_range',
			'kv_sel_rowkeys',
			'kv_upsert',
		]);
	});

	it('prepares nothing server side when prepared statements are disabled', async () => {
		await initPostgres({
			url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`,
			maxConnections: 1,
			kvTable: KV_TABLE_POOLED,
			preparedStatements: false,
		});
		const pooled = getDefaultPostgresClient();
		await ensurePostgresKvSchema(pooled);
		await exerciseKvShapes(new PostgresKvQueryExecutor(pooled));
		const prepared = await pooled.query<{name: string}>('SELECT name FROM pg_prepared_statements ORDER BY name');
		expect(prepared.rows).toEqual([]);
	});
});
