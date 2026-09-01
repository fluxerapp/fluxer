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
import {GuildMembers, ReadStates, Users} from '../Tables';
import {startDockerContainer} from '../test/DockerTestContainer';
import {LegacyPostgresKvQueryExecutor, legacyEnsurePostgresKvSchema} from './__testref__/LegacyPostgresKvQueryExecutor';
import {ensurePostgresKvSchema, POSTGRES_KV_MIGRATION_TABLE, PostgresKvQueryExecutor} from './PostgresKvQueryExecutor';

type Row = Record<string, unknown>;

const POSTGRES_IMAGE = 'postgres:16-alpine';
const CONTAINER = `fluxer-kvupgrade-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const KV = 'kv_upgrade';
const dockerUp = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;

let PORT = 0;

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

if (dockerUp) {
	beforeAll(async () => {
		PORT = await freePort();
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
			`127.0.0.1:${PORT}:5432`,
			POSTGRES_IMAGE,
			'-c',
			'fsync=off',
			'-c',
			'synchronous_commit=off',
		]);
		for (let attempt = 0; attempt < 180; attempt += 1) {
			await sleep(500);
			const probe = spawnSync('docker', ['exec', CONTAINER, 'pg_isready', '-U', 'fluxer', '-d', 'fluxer'], {
				stdio: 'ignore',
			});
			if (probe.status !== 0) continue;
			try {
				await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${PORT}/fluxer`, maxConnections: 8});
				await getDefaultPostgresClient().query('SELECT 1');
				return;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		throw new Error('postgres not ready');
	}, 900_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});
}

class TableClient implements IPostgresClient {
	constructor(
		private readonly inner: IPostgresClient,
		private readonly name: string,
	) {}
	query: IPostgresClient['query'] = (text, values) => this.inner.query(text, values);
	async connect() {
		await this.inner.connect();
	}
	async shutdown() {}
	isConnected() {
		return this.inner.isConnected();
	}
	async transaction<T>(fn: (c: PostgresQueryable) => Promise<T>) {
		return this.inner.transaction(fn);
	}
	kvTable() {
		return this.name;
	}
}

function fp(rows: ReadonlyArray<Row>): string {
	return rows
		.map((r) =>
			Object.entries(r)
				.map(([k, v]) => `${k}=${String(v)}`)
				.join('|'),
		)
		.join(';');
}

function pq(cql: string, params: Record<string, unknown>) {
	return {cql, params} as never;
}

const suite = dockerUp ? describe : describe.skip;

suite('postgres kv upgrade safety', () => {
	let raw: IPostgresClient;
	let client: TableClient;
	let legacy: LegacyPostgresKvQueryExecutor;
	let next: PostgresKvQueryExecutor;

	const usersScan = () => pq(Users.selectCql(), {});

	beforeAll(async () => {
		await initPostgres({url: `postgres://fluxer:fluxer@127.0.0.1:${PORT}/fluxer`, maxConnections: 8});
		raw = getDefaultPostgresClient();
		await raw.query(`DROP TABLE IF EXISTS ${KV}`);
		client = new TableClient(raw, KV);
		await legacyEnsurePostgresKvSchema(client);
		legacy = new LegacyPostgresKvQueryExecutor(client);
		next = new PostgresKvQueryExecutor(client);
		for (let i = 0; i < 40; i += 1) {
			await legacy.executeQuery(
				Users.upsertAll({user_id: BigInt(1000 + i), username: `u${i}`, discriminator: `${i}`} as never) as never,
			);
		}
		for (let g = 0; g < 3; g += 1) {
			for (let u = 0; u < 12; u += 1) {
				await legacy.executeQuery(
					GuildMembers.upsertAll({guild_id: BigInt(500 + g), user_id: BigInt(1000 + u)} as never) as never,
				);
			}
		}
		for (let u = 0; u < 5; u += 1) {
			for (let c = 0; c < 4; c += 1) {
				await legacy.executeQuery(
					ReadStates.upsertAll({user_id: BigInt(1000 + u), channel_id: BigInt(7000 + c)} as never) as never,
				);
			}
		}
	}, 300_000);

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
	});

	it('reads data written by the old image identically', async () => {
		const shapes = [
			{name: 'users scan', q: usersScan()},
			{
				name: 'guild members by guild',
				q: pq(GuildMembers.selectCql({where: GuildMembers.where.eq('guild_id')}), {guild_id: 501n}),
			},
			{
				name: 'read states by user',
				q: pq(ReadStates.selectCql({where: ReadStates.where.eq('user_id')}), {user_id: 1002n}),
			},
			{name: 'user by id', q: pq(Users.selectCql({where: Users.where.eq('user_id')}), {user_id: 1005n})},
		];
		for (const shape of shapes) {
			const a = await legacy.executeQuery(shape.q);
			const b = await next.executeQuery(shape.q);
			expect(fp(b), shape.name).toBe(fp(a));
			expect(a.length, shape.name).toBeGreaterThan(0);
		}
	});

	async function drain(order: Array<'legacy' | 'next'>, pageSize: number) {
		const keys: Array<string> = [];
		let state: string | null = null;
		let pages = 0;
		try {
			for (;;) {
				const who = order[pages % order.length]!;
				const exec = who === 'legacy' ? legacy : next;
				const page: {rows: Array<Row>; pageState: string | null} = await exec.executePagedQuery(usersScan(), {
					pageSize,
					pageState: state,
				});
				pages += 1;
				for (const row of page.rows) keys.push(String(row.user_id));
				state = page.pageState;
				if (!state) break;
				if (pages > 200) throw new Error('runaway paging loop');
			}
		} catch (error) {
			return {keys, error: (error as Error).message};
		}
		return {keys, error: null};
	}

	it('pages consistently with only the old image running', async () => {
		const r = await drain(['legacy'], 7);
		expect(r.error).toBeNull();
		expect(r.keys.length).toBe(40);
		expect(new Set(r.keys).size).toBe(40);
	});

	it('pages consistently with only the new image running', async () => {
		const r = await drain(['next'], 7);
		expect(r.error).toBeNull();
		expect(r.keys.length).toBe(40);
		expect(new Set(r.keys).size).toBe(40);
	});

	it('ROLLING RESTART: an old replica consumes a token minted by the new one', async () => {
		const first = await next.executePagedQuery(usersScan(), {pageSize: 7, pageState: null});
		expect(first.pageState).toBeTruthy();
		const second = await legacy.executePagedQuery(usersScan(), {pageSize: 7, pageState: first.pageState});
		expect(second.rows).toHaveLength(7);
	});

	it('ROLLING RESTART: alternating old and new replicas drain the scan', async () => {
		const a = await drain(['next', 'legacy'], 7);
		const b = await drain(['legacy', 'next'], 7);
		expect(a.error).toBeNull();
		expect(a.keys).toHaveLength(40);
		expect(new Set(a.keys).size).toBe(40);
		expect(b.error).toBeNull();
		expect(b.keys).toHaveLength(40);
		expect(new Set(b.keys).size).toBe(40);
	});

	it('a dual {offset, after} token would survive both directions', async () => {
		const first = await next.executePagedQuery(usersScan(), {pageSize: 7, pageState: null});
		const cursor = JSON.parse(Buffer.from(first.pageState!, 'base64url').toString('utf8')) as {after: string};
		const dual = Buffer.from(JSON.stringify({offset: first.rows.length, after: cursor.after})).toString('base64url');
		const viaOld = await legacy.executePagedQuery(usersScan(), {pageSize: 7, pageState: dual});
		const viaNew = await next.executePagedQuery(usersScan(), {pageSize: 7, pageState: dual});
		expect(viaOld.rows).toHaveLength(7);
		expect(viaNew.rows).toHaveLength(7);
		expect(fp(viaNew.rows)).toBe(fp(viaOld.rows));
	});

	it('deletes exactly the same rows as the old image', async () => {
		const seed = async (exec: LegacyPostgresKvQueryExecutor | PostgresKvQueryExecutor, base: bigint) => {
			for (let c = 0; c < 5; c += 1) {
				await exec.executeQuery(ReadStates.upsertAll({user_id: base, channel_id: BigInt(9000 + c)} as never) as never);
			}
		};
		await seed(legacy, 4001n);
		await seed(legacy, 4002n);
		await legacy.executeQuery(
			ReadStates.delete({where: ReadStates.where.eq('user_id')}).bind({user_id: 4001n}) as never,
		);
		await next.executeQuery(ReadStates.delete({where: ReadStates.where.eq('user_id')}).bind({user_id: 4002n}) as never);
		const left = await raw.query<{row_key: string}>(
			`SELECT row_key FROM ${KV} WHERE table_name = 'read_states' AND row_key LIKE '%400%' ORDER BY row_key`,
		);
		expect(left.rows).toHaveLength(0);
	});

	it('writes byte-identical row_key and partition_key to the old image', async () => {
		const OLD = `${KV}_w_old`;
		const NEW = `${KV}_w_new`;
		await raw.query(`DROP TABLE IF EXISTS ${OLD}`);
		await raw.query(`DROP TABLE IF EXISTS ${NEW}`);
		const oldClient = new TableClient(raw, OLD);
		const newClient = new TableClient(raw, NEW);
		await legacyEnsurePostgresKvSchema(oldClient);
		await ensurePostgresKvSchema(newClient);
		const oldExec = new LegacyPostgresKvQueryExecutor(oldClient);
		const newExec = new PostgresKvQueryExecutor(newClient);
		for (let i = 0; i < 30; i += 1) {
			const row = {user_id: BigInt(9000 + i), channel_id: BigInt(500 + i)} as never;
			await oldExec.executeQuery(ReadStates.upsertAll(row) as never);
			await newExec.executeQuery(ReadStates.upsertAll(row) as never);
			const g = {guild_id: BigInt(70 + i), user_id: BigInt(9000 + i)} as never;
			await oldExec.executeQuery(GuildMembers.upsertAll(g) as never);
			await newExec.executeQuery(GuildMembers.upsertAll(g) as never);
		}
		const diff = await raw.query<{n: string}>(`
			SELECT count(*) AS n FROM (
				(SELECT table_name, partition_key, row_key FROM ${OLD} WHERE table_name <> '${POSTGRES_KV_MIGRATION_TABLE}'
				 EXCEPT SELECT table_name, partition_key, row_key FROM ${NEW} WHERE table_name <> '${POSTGRES_KV_MIGRATION_TABLE}')
				UNION ALL
				(SELECT table_name, partition_key, row_key FROM ${NEW} WHERE table_name <> '${POSTGRES_KV_MIGRATION_TABLE}'
				 EXCEPT SELECT table_name, partition_key, row_key FROM ${OLD} WHERE table_name <> '${POSTGRES_KV_MIGRATION_TABLE}')
			) d`);
		const schemaDiff = await raw.query<{n: string}>(`
			SELECT count(*) AS n FROM (
				(SELECT replace(indexdef, '${OLD}', 'KV') FROM pg_indexes
				 WHERE tablename = '${OLD}' AND indexname <> '${OLD}_row_key_c_idx'
				 EXCEPT SELECT replace(indexdef, '${NEW}', 'KV') FROM pg_indexes WHERE tablename = '${NEW}')
				UNION ALL
				(SELECT replace(indexdef, '${NEW}', 'KV') FROM pg_indexes
				 WHERE tablename = '${NEW}' AND indexname <> '${NEW}_row_key_numeric_idx'
				 EXCEPT SELECT replace(indexdef, '${OLD}', 'KV') FROM pg_indexes WHERE tablename = '${OLD}')
			) d`);
		const added = await raw.query<{indexname: string}>(`
			SELECT indexname FROM pg_indexes WHERE tablename = '${NEW}'
			EXCEPT SELECT replace(indexname, '${OLD}', '${NEW}') FROM pg_indexes WHERE tablename = '${OLD}'`);
		const collations = await raw.query<{tablename: string; attname: string; collname: string}>(`
			SELECT cls.relname AS tablename, att.attname, col.collname
			FROM pg_attribute att
			JOIN pg_class cls ON cls.oid = att.attrelid
			JOIN pg_collation col ON col.oid = att.attcollation
			WHERE att.attrelid IN ('${OLD}'::regclass, '${NEW}'::regclass)
				AND att.attname IN ('partition_key', 'row_key')
			ORDER BY cls.relname, att.attname`);
		const cIndexes = await raw.query<{tablename: string}>(
			`SELECT tablename FROM pg_indexes WHERE indexname IN ('${OLD}_row_key_c_idx', '${NEW}_row_key_c_idx')`,
		);
		expect(Number(diff.rows[0]!.n)).toBe(0);
		expect(Number(schemaDiff.rows[0]!.n)).toBe(0);
		expect(added.rows.map((r) => r.indexname)).toEqual([`${NEW}_row_key_numeric_idx`]);
		expect(collations.rows.filter((r) => r.tablename === OLD).map((r) => r.collname)).toEqual(['default', 'default']);
		expect(collations.rows.filter((r) => r.tablename === NEW).map((r) => r.collname)).toEqual(['C', 'C']);
		expect(cIndexes.rows.map((r) => r.tablename)).toEqual([OLD]);
	});

	it('never recollates an existing table and keeps its C index', async () => {
		const OLD = `${KV}_keep_old`;
		await raw.query(`DROP TABLE IF EXISTS ${OLD}`);
		const oldClient = new TableClient(raw, OLD);
		await legacyEnsurePostgresKvSchema(oldClient);
		await ensurePostgresKvSchema(oldClient);
		await ensurePostgresKvSchema(oldClient);
		const collations = await raw.query<{attname: string; collname: string}>(`
			SELECT att.attname, col.collname
			FROM pg_attribute att
			JOIN pg_collation col ON col.oid = att.attcollation
			WHERE att.attrelid = '${OLD}'::regclass AND att.attname IN ('partition_key', 'row_key')
			ORDER BY att.attname`);
		const indexes = await raw.query<{indexname: string}>(
			`SELECT indexname FROM pg_indexes WHERE tablename = '${OLD}' ORDER BY indexname`,
		);
		expect(collations.rows.map((r) => r.collname)).toEqual(['default', 'default']);
		expect(indexes.rows.map((r) => r.indexname)).toContain(`${OLD}_row_key_c_idx`);
	});

	it('survives three simultaneous boots (two api replicas and a worker)', async () => {
		const BOOT = `${KV}_boot`;
		await raw.query(`DROP TABLE IF EXISTS ${BOOT}`);
		const results = await Promise.allSettled([
			ensurePostgresKvSchema(new TableClient(raw, BOOT)),
			ensurePostgresKvSchema(new TableClient(raw, BOOT)),
			ensurePostgresKvSchema(new TableClient(raw, BOOT)),
		]);
		expect(results).toEqual([
			{status: 'fulfilled', value: undefined},
			{status: 'fulfilled', value: undefined},
			{status: 'fulfilled', value: undefined},
		]);
	}, 120_000);

	it('survives a peer that creates the table without the schema lock', async () => {
		const RACE = `${KV}_race`;
		await raw.query(`DROP TABLE IF EXISTS ${RACE}`);
		let release = () => {};
		const gate = new Promise<void>((resolve) => {
			release = resolve;
		});
		const peer = raw.transaction(async (db) => {
			await db.query(`
CREATE TABLE IF NOT EXISTS ${RACE} (
	table_name text NOT NULL,
	partition_key text COLLATE "C" NOT NULL,
	row_key text COLLATE "C" NOT NULL,
	row_data jsonb NOT NULL,
	expires_at timestamptz,
	updated_at timestamptz NOT NULL DEFAULT now(),
	PRIMARY KEY (table_name, row_key)
)`);
			await gate;
		});
		const booting = ensurePostgresKvSchema(new TableClient(raw, RACE)).then(
			() => 'ok',
			(error: Error) => `failed: ${error.message}`,
		);
		await sleep(500);
		release();
		await peer;
		expect(await booting).toBe('ok');
		await raw.query(`DROP TABLE IF EXISTS ${RACE}`);
	}, 120_000);

	it('backfills the messages partition key once and never scans for it again', async () => {
		const BACKFILL = 'kv_backfill';
		const SEP = String.fromCharCode(31);
		const backfillClient = new TableClient(raw, BACKFILL);
		const legacyKey = (id: string) => `"c"${SEP}"b"${SEP}"${id}"`;
		const insertLegacy = async (id: string) => {
			await raw.query(
				`INSERT INTO ${BACKFILL} (table_name, partition_key, row_key, row_data) VALUES ('messages', $1, $1, '{}'::jsonb)`,
				[legacyKey(id)],
			);
		};
		const partitionOf = async (id: string) => {
			const result = await raw.query<{partition_key: string}>(
				`SELECT partition_key FROM ${BACKFILL} WHERE table_name = 'messages' AND row_key = $1`,
				[legacyKey(id)],
			);
			return result.rows[0]?.partition_key;
		};
		await raw.query(`DROP TABLE IF EXISTS ${BACKFILL}`);
		await legacyEnsurePostgresKvSchema(backfillClient);
		await insertLegacy('m1');
		const pendingBefore = await raw.query(
			`SELECT 1 FROM ${BACKFILL} WHERE table_name = 'messages' AND partition_key = row_key AND split_part(row_key, chr(31), 3) <> ''`,
		);
		expect(pendingBefore.rows.length).toBe(1);
		await ensurePostgresKvSchema(backfillClient);
		const marker = await raw.query(
			`SELECT 1 FROM ${BACKFILL} WHERE table_name = $1 AND row_key = 'messages_partition_key_v1'`,
			[POSTGRES_KV_MIGRATION_TABLE],
		);
		const pendingAfter = await raw.query(
			`SELECT 1 FROM ${BACKFILL} WHERE table_name = 'messages' AND partition_key = row_key AND split_part(row_key, chr(31), 3) <> ''`,
		);
		expect(await partitionOf('m1')).toBe(`"c"${SEP}"b"`);
		expect(pendingAfter.rows.length).toBe(0);
		expect(marker.rows.length).toBe(1);
		await insertLegacy('m2');
		await ensurePostgresKvSchema(backfillClient);
		const skipped = await partitionOf('m2');
		expect(skipped).toBe(legacyKey('m2'));
		expect(await partitionOf('m1')).toBe(`"c"${SEP}"b"`);
		await raw.query(`DROP TABLE IF EXISTS ${BACKFILL}`);
	}, 120_000);

	it('boot does not repair an invalid C-collation index', async () => {
		const q = pq(ReadStates.selectCql({where: ReadStates.where.eq('user_id')}), {user_id: 1002n});
		const before = await next.executeQuery(q);
		await raw.query(`UPDATE pg_index SET indisvalid = false WHERE indexrelid = '${KV}_row_key_c_idx'::regclass`);
		await ensurePostgresKvSchema(client);
		const check = await raw.query<{indisvalid: boolean}>(
			`SELECT indisvalid FROM pg_index WHERE indexrelid = '${KV}_row_key_c_idx'::regclass`,
		);
		const after = await next.executeQuery(q);
		await raw.query(`UPDATE pg_index SET indisvalid = true WHERE indexrelid = '${KV}_row_key_c_idx'::regclass`);
		expect(String(after.map((r) => Object.values(r).map(String).join(',')))).toBe(
			String(before.map((r) => Object.values(r).map(String).join(','))),
		);
		expect(check.rows[0]?.indisvalid).toBe(true);
	});

	describe('multi-range plan shape on a populated upgraded database', () => {
		const PERF = 'kv_ranges_perf';
		const BASE = 1456074443984486400n;
		let perf: IPostgresClient;

		beforeAll(async () => {
			const db = getDefaultPostgresClient();
			await db.query(`DROP TABLE IF EXISTS ${PERF}`);
			perf = new TableClient(db, PERF);
			await ensurePostgresKvSchema(perf);
			await db.query(
				`INSERT INTO ${PERF} (table_name, partition_key, row_key, row_data)
SELECT 'push_subscriptions',
	format('{"__fluxer_type":"bigint","value":"%s"}%s{"__fluxer_type":"bigint","value":"%s"}', u, chr(31), s),
	format('{"__fluxer_type":"bigint","value":"%s"}%s{"__fluxer_type":"bigint","value":"%s"}', u, chr(31), s),
	jsonb_build_object(
		'user_id', jsonb_build_object('__fluxer_type', 'bigint', 'value', u::text),
		'subscription_id', jsonb_build_object('__fluxer_type', 'bigint', 'value', s::text),
		'endpoint', repeat('e', 300))
FROM generate_series($1::bigint, $1::bigint + 19999) u, generate_series(1, 3) s`,
				[BASE.toString()],
			);
			await db.query(`ANALYZE ${PERF}`);
		}, 300_000);

		it('emits a multi-range plan that is not slower than the tier-3 scan it replaces', async () => {
			const {PushSubscriptions} = await import('../Tables');
			const {buildCandidatePlan, planFragmentGroups} = await import('./PostgresKvQueryExecutor');
			const {getKvMeta} = await import('./CassandraMetaRegistry');
			const db = getDefaultPostgresClient();
			const explain = async (sql: string, params: Array<unknown>) => {
				const r = await db.query<Record<string, string>>(`EXPLAIN (ANALYZE, BUFFERS) ${sql}`, params);
				const plan = r.rows.map((row) => row['QUERY PLAN']!).join('\n');
				return {plan, ms: Number(/Execution Time: ([\d.]+)/.exec(plan)?.[1] ?? 'NaN')};
			};
			const cql = PushSubscriptions.selectCql({where: PushSubscriptions.where.in('user_id', 'user_ids')});
			const meta = getKvMeta(cql)!;
			const scan = await explain(
				`SELECT kv.row_key, kv.row_data FROM ${PERF} kv WHERE kv.table_name = $1 AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
				['push_subscriptions'],
			);
			expect(Number.isFinite(scan.ms)).toBe(true);
			const deltas: Array<string> = [];
			for (const size of [5, 10, 25, 50, 100]) {
				const params = {user_ids: Array.from({length: size}, (_, i) => BASE + BigInt(i * 3))};
				const plan = buildCandidatePlan(meta, params as never);
				expect(plan.candidates.kind).toBe('ranges');
				const groups = planFragmentGroups(plan.candidates);
				expect(groups.length).toBe(1);
				const fragments = groups[0]!;
				expect(fragments.predicate).not.toContain('unnest');
				const pushed = await explain(
					`SELECT kv.row_key, kv.row_data FROM ${PERF} kv WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
					['push_subscriptions', ...fragments.params],
				);
				expect(Number.isFinite(pushed.ms)).toBe(true);
				if (pushed.ms > scan.ms) {
					deltas.push(`n=${size} pushdown=${pushed.ms.toFixed(2)}ms scan=${scan.ms.toFixed(2)}ms\n${pushed.plan}`);
				}
			}
			expect(deltas.join('\n')).toBe('');
			const overCap = buildCandidatePlan(meta, {
				user_ids: Array.from({length: 700}, (_, i) => BASE + BigInt(i * 3)),
			} as never);
			expect(overCap.candidates.kind).toBe('rangeGroups');
			expect(overCap.exact).toBe(true);
			const overCapGroups = planFragmentGroups(overCap.candidates);
			expect(overCapGroups.map((group) => group.params.length)).toEqual([512, 512, 376]);
			for (const fragments of overCapGroups) {
				const pushed = await explain(
					`SELECT kv.row_key, kv.row_data FROM ${PERF} kv WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
					['push_subscriptions', ...fragments.params],
				);
				expect(pushed.plan).not.toContain('Seq Scan');
			}
		}, 300_000);
	});
});

describe('candidate plan shapes reached by real queries', () => {
	it('classifies the push fan-out and webhook lookups', async () => {
		const {PushSubscriptions, Webhooks} = await import('../Tables');
		const {buildCandidatePlan} = await import('./PostgresKvQueryExecutor');
		const {getKvMeta} = await import('./CassandraMetaRegistry');
		const cases = [
			{
				name: 'push_subscriptions IN(user_id) x100',
				cql: PushSubscriptions.selectCql({where: PushSubscriptions.where.in('user_id', 'user_ids')}),
				params: {user_ids: Array.from({length: 100}, (_, i) => BigInt(1000 + i))},
			},
			{
				name: 'webhooks IN(webhook_id) x100',
				cql: Webhooks.selectCql({where: Webhooks.where.in('webhook_id', 'webhook_ids')}),
				params: {webhook_ids: Array.from({length: 100}, (_, i) => BigInt(1000 + i))},
			},
		];
		for (const c of cases) {
			const meta = getKvMeta(c.cql)!;
			const plan = buildCandidatePlan(meta, c.params as never);
			const size =
				plan.candidates.kind === 'ranges'
					? plan.candidates.lowerBounds.length
					: plan.candidates.kind === 'rowKeys'
						? plan.candidates.rowKeys.length
						: 0;
			expect({kind: plan.candidates.kind, size, exact: plan.exact}, c.name).toEqual({
				kind: 'ranges',
				size: 100,
				exact: true,
			});
		}
	});
});
