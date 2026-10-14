// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import type {PreparedQuery} from '@app/api/database/CassandraTypes';
import {keyFromColumns, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import {dockerAvailable, startPostgresKv, stopPostgresKv} from '@app/api/test/DockerTestContainer';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const KV_TABLE = 'kv_keyset_scan';

type Row = Record<string, unknown>;

interface UserRow {
	user_id: bigint;
	name: string;
	kind: number;
}

interface BucketRow {
	channel_id: bigint;
	bucket: number;
	note: string;
}

interface NameRow {
	name: string;
	value: number;
}

const KeysetUsers = defineTable<UserRow, 'user_id'>({
	name: 'keyset_users',
	columns: ['user_id', 'name', 'kind'],
	primaryKey: ['user_id'],
});

const KeysetBuckets = defineTable<BucketRow, 'channel_id' | 'bucket', 'channel_id'>({
	name: 'keyset_buckets',
	columns: ['channel_id', 'bucket', 'note'],
	primaryKey: ['channel_id', 'bucket'],
	partitionKey: ['channel_id'],
});

const KeysetNames = defineTable<NameRow, 'name'>({
	name: 'keyset_names',
	columns: ['name', 'value'],
	primaryKey: ['name'],
});

interface Statement {
	text: string;
	rows: number;
}

function recordingClient(client: IPostgresClient, statements: Array<Statement>): IPostgresClient {
	return {
		query: (async (text: string, values?: Array<unknown>, name?: string) => {
			const result = await client.query(text, values, name);
			statements.push({text, rows: result.rows.length});
			return result;
		}) as IPostgresClient['query'],
		connect: () => client.connect(),
		shutdown: () => client.shutdown(),
		transaction: (fn) => client.transaction(fn),
		kvTable: () => client.kvTable(),
	};
}

function pageState(state: Row): string {
	return Buffer.from(JSON.stringify(state)).toString('base64url');
}

function readPageState(state: string): Row {
	return JSON.parse(Buffer.from(state, 'base64url').toString('utf8')) as Row;
}

function userId(index: number): bigint {
	return 1_000_000_000_000_000_000n + BigInt(index) * 7_919n;
}

describe.skipIf(!dockerAvailable)('Postgres KV keyset scans', () => {
	let raw: IPostgresClient;
	let statements: Array<Statement>;
	let executor: PostgresKvQueryExecutor;
	let postgresContainer: string;

	beforeAll(async () => {
		const started = await startPostgresKv({namePrefix: 'fluxer-kvkeyset', kvTable: KV_TABLE, maxConnections: 4});
		postgresContainer = started.container;
		raw = started.client;
	}, 900_000);

	beforeEach(async () => {
		await raw.query(`DELETE FROM ${KV_TABLE}`);
		statements = [];
		executor = new PostgresKvQueryExecutor(recordingClient(raw, statements));
	});

	afterAll(async () => {
		await stopPostgresKv(postgresContainer);
	});

	async function seed(queries: ReadonlyArray<PreparedQuery>): Promise<void> {
		await executor.executeBatch(queries.map((query) => ({query: query.cql, params: query.params, meta: query.kvMeta})));
	}

	async function seedUsers(count: number): Promise<Array<UserRow>> {
		const rows = Array.from({length: count}, (_, index) => ({
			user_id: userId((index * 37) % count),
			name: `user-${index}`,
			kind: index % 7,
		}));
		await seed(rows.map((row) => KeysetUsers.insert(row)));
		return rows;
	}

	async function seedBuckets(channels: number, buckets: number): Promise<void> {
		const queries: Array<PreparedQuery> = [];
		for (let channel = 0; channel < channels; channel += 1) {
			for (let bucket = 0; bucket < buckets; bucket += 1) {
				queries.push(
					KeysetBuckets.insert({
						channel_id: userId((channel * 13) % channels),
						bucket: bucket * 11 - 20,
						note: `note-${(channel + bucket) % 5}`,
					}),
				);
			}
		}
		await seed(queries);
	}

	async function oracle(query: PreparedQuery): Promise<Array<Row>> {
		const rows = await executor.executeQuery<Row>(query);
		statements.length = 0;
		return rows;
	}

	async function walk(
		query: PreparedQuery,
		pageSize: number,
		start: string | null = null,
	): Promise<{pages: Array<Array<Row>>; states: Array<string>}> {
		const pages: Array<Array<Row>> = [];
		const states: Array<string> = [];
		let state = start;
		do {
			const page = await executor.executePagedQuery<Row>(query, {pageSize, pageState: state});
			pages.push(page.rows);
			state = page.pageState;
			if (state !== null) states.push(state);
			expect(pages.length).toBeLessThan(10_000);
		} while (state !== null);
		return {pages, states};
	}

	function expectBounded(batch: number): void {
		expect(statements.length).toBeGreaterThan(0);
		for (const statement of statements) {
			expect(statement.rows).toBeLessThanOrEqual(batch);
			if (statement.text.includes('row_data FROM')) expect(statement.text).toMatch(/LIMIT \$2$/);
		}
	}

	function byKey(columns: ReadonlyArray<string>, rows: ReadonlyArray<Row>): Array<Row> {
		return [...rows].sort((left, right) => {
			const l = keyFromColumns(columns, left);
			const r = keyFromColumns(columns, right);
			return l < r ? -1 : l > r ? 1 : 0;
		});
	}

	it('walks a numeric single key table in the same order without a full load', async () => {
		await seedUsers(250);
		const query = KeysetUsers.select({}).bind({});
		const expected = await oracle(query);
		expect(expected).toHaveLength(250);
		const {pages, states} = await walk(query, 40);
		expect(pages.flat()).toEqual(expected);
		expect(pages.slice(0, -1).every((page) => page.length === 40)).toBe(true);
		expect(readPageState(states[0]!)).toEqual({
			offset: 40,
			after: keyFromColumns(['user_id'], expected[39]!),
			keyed: true,
		});
		expectBounded(41);
	});

	it('walks a composite key table by row key without reloading it per page', async () => {
		await seedBuckets(30, 7);
		const query = KeysetBuckets.select({}).bind({});
		const expected = await oracle(query);
		expect(expected).toHaveLength(210);
		const {pages} = await walk(query, 25);
		const rows = pages.flat();
		expect(byKey(['channel_id', 'bucket'], rows)).toEqual(byKey(['channel_id', 'bucket'], expected));
		expect(new Set(rows.map((row) => keyFromColumns(['channel_id', 'bucket'], row))).size).toBe(210);
		const channelRuns = rows.filter((row, index) => index === 0 || rows[index - 1]!.channel_id !== row.channel_id);
		expect(channelRuns).toHaveLength(30);
		expect(statements.filter((statement) => statement.text.includes('row_data FROM'))).toHaveLength(pages.length);
		expectBounded(26);
	});

	it('walks a text key table in the same order', async () => {
		await seed(
			Array.from({length: 90}, (_, index) => KeysetNames.insert({name: `name-${(index * 31) % 90}-x`, value: index})),
		);
		const query = KeysetNames.select({}).bind({});
		const expected = await oracle(query);
		const {pages} = await walk(query, 16);
		expect(pages.flat()).toEqual(expected);
		expectBounded(17);
	});

	it('fills filtered pages from bounded batches on numeric and composite tables', async () => {
		await seedUsers(300);
		await seedBuckets(20, 9);
		const users = KeysetUsers.select({where: KeysetUsers.where.eq('kind')}).bind({kind: 3});
		const expectedUsers = await oracle(users);
		const userWalk = await walk(users, 10);
		expect(userWalk.pages.flat()).toEqual(expectedUsers);
		expect(userWalk.pages.slice(0, -1).every((page) => page.length === 10)).toBe(true);
		expectBounded(11);
		const buckets = KeysetBuckets.select({where: KeysetBuckets.where.eq('note')}).bind({note: 'note-2'});
		const expectedBuckets = await oracle(buckets);
		const bucketWalk = await walk(buckets, 6);
		expect(byKey(['channel_id', 'bucket'], bucketWalk.pages.flat())).toEqual(
			byKey(['channel_id', 'bucket'], expectedBuckets),
		);
		expect(bucketWalk.pages.slice(0, -1).every((page) => page.length === 6)).toBe(true);
		expectBounded(7);
	});

	it('skips expired rows', async () => {
		await seedUsers(60);
		await seedBuckets(6, 5);
		await raw.query(
			`UPDATE ${KV_TABLE} SET expires_at = now() - interval '1 minute' WHERE (row_data ->> 'kind')::int = 2 OR row_data ->> 'note' = 'note-1'`,
		);
		const users = KeysetUsers.select({}).bind({});
		const expectedUsers = await oracle(users);
		expect(expectedUsers.length).toBeLessThan(60);
		expect((await walk(users, 7)).pages.flat()).toEqual(expectedUsers);
		const buckets = KeysetBuckets.select({}).bind({});
		const expectedBuckets = await oracle(buckets);
		expect(expectedBuckets.length).toBeLessThan(30);
		expect(byKey(['channel_id', 'bucket'], (await walk(buckets, 7)).pages.flat())).toEqual(
			byKey(['channel_id', 'bucket'], expectedBuckets),
		);
		expectBounded(8);
	});

	it('returns one empty page for an empty table', async () => {
		for (const query of [KeysetUsers.select({}).bind({}), KeysetBuckets.select({}).bind({})]) {
			expect(await executor.executePagedQuery(query, {pageSize: 10})).toEqual({rows: [], pageState: null});
		}
		expectBounded(1);
	});

	it('never drops rows with non-numeric keys from a single key table', async () => {
		await seedUsers(40);
		await executor.executeQuery(KeysetUsers.insert({user_id: 'stray' as unknown as bigint, name: 'stray', kind: 0}));
		const {pages} = await walk(KeysetUsers.select({}).bind({}), 9);
		const names = pages.flat().map((row) => row.name);
		expect(names).toHaveLength(41);
		expect(new Set(names).size).toBe(41);
		expect(names).toContain('stray');
		expectBounded(10);
	});

	it('resumes every page state shape the previous release produced', async () => {
		await seedUsers(120);
		await seedBuckets(12, 6);
		const users = KeysetUsers.select({}).bind({});
		const expectedUsers = await oracle(users);
		const userKey = keyFromColumns(['user_id'], expectedUsers[49]!);
		for (const state of [{offset: 50, after: userKey, keyed: true}, {offset: 50, after: userKey}, {offset: 50}]) {
			expect((await walk(users, 15, pageState(state))).pages.flat()).toEqual(expectedUsers.slice(50));
		}
		const buckets = KeysetBuckets.select({}).bind({});
		const expectedBuckets = await oracle(buckets);
		const bucketKey = keyFromColumns(['channel_id', 'bucket'], expectedBuckets[29]!);
		statements.length = 0;
		const resumed = await walk(buckets, 11, pageState({offset: 30, after: bucketKey}));
		expect(byKey(['channel_id', 'bucket'], resumed.pages.flat())).toEqual(
			byKey(['channel_id', 'bucket'], expectedBuckets.slice(30)),
		);
		expect(readPageState(resumed.states[0]!)).toMatchObject({byRowKey: true, since: bucketKey});
		expectBounded(12);
	});

	it('streams limited scans in key order instead of loading the table', async () => {
		const seeded = await seedUsers(200);
		const sorted = [...seeded].sort((left, right) => (left.user_id < right.user_id ? -1 : 1));
		const first = KeysetUsers.select({limit: 20}).bind({});
		const next = KeysetUsers.select({where: KeysetUsers.where.tokenGt('user_id', 'last_user_id'), limit: 20});
		const expectedFirst = await oracle(KeysetUsers.select({}).bind({}));
		expect(await executor.executeQuery(first)).toEqual(expectedFirst.slice(0, 20));
		expectBounded(21);
		statements.length = 0;
		const rows = await executor.executeQuery(next.bind({last_user_id: sorted[99]!.user_id}));
		expect(rows).toEqual(expectedFirst.slice(100, 120));
		expect(statements.filter((statement) => statement.text.includes('row_data FROM'))).toHaveLength(1);
		expectBounded(21);
		statements.length = 0;
		expect(await executor.executeQuery(next.bind({last_user_id: sorted[195]!.user_id}))).toEqual(
			expectedFirst.slice(196),
		);
		expectBounded(21);
	});

	it('probes for non-numeric keys through the primary key while other tables hold string keys', async () => {
		await raw.query(
			`INSERT INTO ${KV_TABLE} (table_name, partition_key, row_key, row_data) SELECT 'keyset_users', k, k, '{}'::jsonb FROM generate_series(1, 20000) g, LATERAL (SELECT '{"__fluxer_type":"bigint","value":"' || g || '"}' AS k) s`,
		);
		await raw.query(
			`INSERT INTO ${KV_TABLE} (table_name, partition_key, row_key, row_data) SELECT 'keyset_strings', g::text, '"' || lpad(g::text, 8, '0') || '"', '{}'::jsonb FROM generate_series(1, 200000) g`,
		);
		await raw.query(`ANALYZE ${KV_TABLE}`);
		await executor.executePagedQuery(KeysetUsers.select({}).bind({}), {pageSize: 10});
		const probe = statements.find((statement) => statement.text.endsWith('AS found'));
		expect(probe).toBeDefined();
		const plan = await raw.query<{'QUERY PLAN': string}>(`EXPLAIN (COSTS OFF) ${probe!.text}`, ['keyset_users']);
		expect(plan.rows.map((row) => row['QUERY PLAN']).join('\n')).not.toContain('Seq Scan');
	});

	it('keeps the sorted load when a limited scan asks for an order the key walk cannot give', async () => {
		await seedUsers(30);
		const descending = KeysetUsers.select({orderBy: {col: 'user_id', direction: 'DESC'}, limit: 5}).bind({});
		const rows = await executor.executeQuery<Row>(descending);
		const all = await oracle(KeysetUsers.select({}).bind({}));
		expect(rows).toEqual(all.slice(-5).reverse());
	});
});
