// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {Db} from '@app/api/database/CassandraTypes';
import {
	DEFAULT_TTL_EXPIRY_RESUME,
	DEFAULT_TTL_TABLES,
	expireLegacyDefaultTtlRows,
} from '@app/api/database/PostgresKvDefaultTtlExpiry';
import {
	ensurePostgresKvSchema,
	PostgresKvQueryExecutor,
	pruneExpiredPostgresKvRows,
} from '@app/api/database/PostgresKvQueryExecutor';
import {startDockerContainer} from '@app/api/test/DockerTestContainer';
import {
	getDefaultPostgresClient,
	type IPostgresClient,
	initPostgres,
	shutdownPostgres,
} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const KV_TABLE = 'kv_default_ttl';
const CONTAINER = `fluxer-kvttl-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;
const DEFAULT_TTL_SECONDS = 600;

interface ProbeRow {
	id: string;
	value: string | null;
	note: string | null;
}

interface OwnedProbeRow {
	owner: string;
	id: string;
	value: string | null;
}

const DefaultTtlProbe = defineTable<ProbeRow, 'id'>({
	name: 'default_ttl_probe',
	columns: ['id', 'value', 'note'],
	primaryKey: ['id'],
	defaultTtlSeconds: DEFAULT_TTL_SECONDS,
});

const DefaultTtlProbeRows = defineTable<OwnedProbeRow, 'owner' | 'id', 'owner'>({
	name: 'default_ttl_probe_rows',
	columns: ['owner', 'id', 'value'],
	primaryKey: ['owner', 'id'],
	partitionKey: ['owner'],
	defaultTtlSeconds: DEFAULT_TTL_SECONDS,
});

const NoTtlProbe = defineTable<ProbeRow, 'id'>({
	name: 'no_ttl_probe',
	columns: ['id', 'value', 'note'],
	primaryKey: ['id'],
});

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

function expectExpiresIn(value: Date | number | null, ttlSeconds: number): void {
	expect(value).toBeInstanceOf(Date);
	const remainingSeconds = ((value as Date).getTime() - Date.now()) / 1000;
	expect(remainingSeconds).toBeGreaterThan(ttlSeconds - 60);
	expect(remainingSeconds).toBeLessThanOrEqual(ttlSeconds);
}

describe.skipIf(!dockerAvailable)('Postgres KV default TTL', () => {
	let raw: IPostgresClient;
	let executor: PostgresKvQueryExecutor;

	async function stored(table: string, id: string): Promise<{expires_at: Date | number | null; row_data: object}> {
		const result = await raw.query<{expires_at: Date | number | null; row_data: object}>(
			`SELECT expires_at, row_data FROM ${KV_TABLE} WHERE table_name = $1 AND row_data ->> 'id' = $2`,
			[table, id],
		);
		expect(result.rows).toHaveLength(1);
		return result.rows[0]!;
	}

	async function expiresAt(table: string, id: string): Promise<Date | number | null> {
		return (await stored(table, id)).expires_at;
	}

	async function neverExpires(table: string, id: string): Promise<boolean> {
		const result = await raw.query<{forever: boolean}>(
			`SELECT expires_at = 'infinity'::timestamptz AS forever FROM ${KV_TABLE} WHERE table_name = $1 AND row_data ->> 'id' = $2`,
			[table, id],
		);
		return result.rows[0]?.forever === true;
	}

	async function setExpiry(table: string, id: string, expression: string): Promise<void> {
		await raw.query(
			`UPDATE ${KV_TABLE} SET expires_at = ${expression} WHERE table_name = $1 AND row_data ->> 'id' = $2`,
			[table, id],
		);
	}

	async function seed(table: string, key: string, age: string, expires: Date | string | null = null): Promise<string> {
		const result = await raw.query<{updated_at: string}>(
			`INSERT INTO ${KV_TABLE} (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $2, '{}'::jsonb, $3::timestamptz, now() - $4::interval)
RETURNING updated_at::text`,
			[table, key, expires, age],
		);
		return result.rows[0]!.updated_at;
	}

	async function remaining(): Promise<Array<{table_name: string; row_key: string}>> {
		const result = await raw.query<{table_name: string; row_key: string}>(
			`SELECT table_name, row_key FROM ${KV_TABLE} WHERE table_name <> '__fluxer_schema_migrations' ORDER BY table_name, row_key`,
		);
		return result.rows;
	}

	async function ageMarker(): Promise<void> {
		await raw.query(
			`UPDATE ${KV_TABLE} SET row_data = jsonb_build_object('applied_at', now() - interval '2 days') WHERE table_name = '__fluxer_schema_migrations' AND row_key = 'default_ttl_expiry_v1'`,
		);
	}

	async function resumePoint(): Promise<object | null> {
		const result = await raw.query<{row_data: object}>(
			`SELECT row_data FROM ${KV_TABLE} WHERE table_name = '__fluxer_schema_migrations' AND row_key = $1`,
			[DEFAULT_TTL_EXPIRY_RESUME],
		);
		return result.rows[0]?.row_data ?? null;
	}

	async function markerCount(): Promise<number> {
		const result = await raw.query<{n: number}>(
			`SELECT count(*)::int AS n FROM ${KV_TABLE} WHERE table_name = '__fluxer_schema_migrations' AND row_key = 'default_ttl_expiry_v1'`,
		);
		return result.rows[0]!.n;
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
				await initPostgres({
					url: `postgres://fluxer:fluxer@127.0.0.1:${port}/fluxer`,
					maxConnections: 4,
					kvTable: KV_TABLE,
				});
				await getDefaultPostgresClient().query('SELECT 1');
				ready = true;
			} catch {
				await shutdownPostgres().catch(() => {});
			}
		}
		if (!ready) throw new Error('postgres never came up');
		raw = getDefaultPostgresClient();
		await ensurePostgresKvSchema(raw);
		executor = new PostgresKvQueryExecutor(raw);
	}, 900_000);

	beforeEach(async () => {
		await raw.query(`DELETE FROM ${KV_TABLE}`);
	});

	afterAll(async () => {
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('gives every full-row write without a TTL the table default', async () => {
		await executor.executeQuery(DefaultTtlProbe.insert({id: 'insert', value: 'a', note: null}));
		await executor.executeQuery(DefaultTtlProbe.upsertAll({id: 'upsert', value: 'b', note: 'n'}));
		expect(
			await executor.executeQuery(DefaultTtlProbe.insertIfNotExists({id: 'claimed', value: 'c', note: null})),
		).toEqual([{'[applied]': true}]);
		expect(
			await executor.executeQuery(
				DefaultTtlProbeRows.conditionalBatch([{action: 'insert', row: {owner: 'o', id: 'batched', value: 'd'}}]),
			),
		).toEqual([{'[applied]': true}]);

		for (const id of ['insert', 'upsert', 'claimed']) {
			expectExpiresIn(await expiresAt('default_ttl_probe', id), DEFAULT_TTL_SECONDS);
		}
		expectExpiresIn(await expiresAt('default_ttl_probe_rows', 'batched'), DEFAULT_TTL_SECONDS);
	});

	it('keeps an explicit TTL ahead of the default', async () => {
		await executor.executeQuery(DefaultTtlProbe.insertWithTtl({id: 'short', value: 'a', note: null}, 60));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'short'), 60);

		await executor.executeQuery(DefaultTtlProbe.insert({id: 'patched', value: 'a', note: null}));
		await executor.executeQuery(DefaultTtlProbe.patchByPkWithTtl({id: 'patched'}, {value: Db.set('b')}, 60));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'patched'), 60);
	});

	it('keeps an explicit TTL of zero as no expiry', async () => {
		await executor.executeQuery(DefaultTtlProbe.insertWithTtl({id: 'forever', value: 'a', note: null}, 0));
		expect(await neverExpires('default_ttl_probe', 'forever')).toBe(true);
		expect(
			await executor.executeQuery(
				DefaultTtlProbe.select({where: DefaultTtlProbe.where.eq('id')}).bind({id: 'forever'}),
			),
		).toEqual([{id: 'forever', value: 'a', note: null}]);

		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'forever'}, {note: Db.set('patched')}));
		expect(await neverExpires('default_ttl_probe', 'forever')).toBe(true);

		await pruneExpiredPostgresKvRows(raw);
		expect(await neverExpires('default_ttl_probe', 'forever')).toBe(true);
	});

	it('raises a patched row to the default but never lowers it', async () => {
		await executor.executeQuery(DefaultTtlProbe.insertWithTtl({id: 'longer', value: 'a', note: null}, 3600));
		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'longer'}, {note: Db.set('patched')}));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'longer'), 3600);

		await executor.executeQuery(DefaultTtlProbe.insert({id: 'soon', value: 'a', note: null}));
		await setExpiry('default_ttl_probe', 'soon', "now() + interval '5 seconds'");
		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'soon'}, {note: Db.set('patched')}));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'soon'), DEFAULT_TTL_SECONDS);

		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'missing'}, {note: Db.set('created')}));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'missing'), DEFAULT_TTL_SECONDS);

		await executor.executeQuery(DefaultTtlProbe.insert({id: 'unset', value: 'a', note: null}));
		await setExpiry('default_ttl_probe', 'unset', 'NULL');
		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'unset'}, {note: Db.set('patched')}));
		expectExpiresIn(await expiresAt('default_ttl_probe', 'unset'), DEFAULT_TTL_SECONDS);

		await executor.executeQuery(DefaultTtlProbe.insert({id: 'expired', value: 'a', note: null}));
		await setExpiry('default_ttl_probe', 'expired', "now() - interval '1 second'");
		await executor.executeQuery(DefaultTtlProbe.patchByPk({id: 'expired'}, {note: Db.set('patched')}));
		const revived = await stored('default_ttl_probe', 'expired');
		expect(revived.row_data).toEqual({id: 'expired', note: 'patched'});
		expectExpiresIn(revived.expires_at, DEFAULT_TTL_SECONDS);
	});

	it('raises conditional patches the same way', async () => {
		await executor.executeQuery(DefaultTtlProbe.insert({id: 'soon', value: 'a', note: null}));
		await setExpiry('default_ttl_probe', 'soon', "now() + interval '5 seconds'");
		expect(
			await executor.executeQuery(
				DefaultTtlProbe.conditionalPatchByPk({id: 'soon'}, {note: Db.set('patched')}, {value: 'a'}),
			),
		).toEqual([{'[applied]': true}]);
		expectExpiresIn(await expiresAt('default_ttl_probe', 'soon'), DEFAULT_TTL_SECONDS);

		await executor.executeQuery(DefaultTtlProbe.insertWithTtl({id: 'longer', value: 'a', note: null}, 3600));
		expect(
			await executor.executeQuery(
				DefaultTtlProbe.conditionalPatchByPk({id: 'longer'}, {note: Db.set('patched')}, {value: 'a'}),
			),
		).toEqual([{'[applied]': true}]);
		expectExpiresIn(await expiresAt('default_ttl_probe', 'longer'), 3600);

		await executor.executeQuery(DefaultTtlProbeRows.insert({owner: 'o', id: 'existing', value: 'old'}));
		await setExpiry('default_ttl_probe_rows', 'existing', 'NULL');
		expect(
			await executor.executeQuery(
				DefaultTtlProbeRows.conditionalBatch([
					{action: 'insert', row: {owner: 'o', id: 'added', value: 'new'}},
					{
						action: 'patch',
						pk: {owner: 'o', id: 'existing'},
						patch: {value: Db.set('updated')},
						expected: {value: 'old'},
					},
				]),
			),
		).toEqual([{'[applied]': true}]);
		expectExpiresIn(await expiresAt('default_ttl_probe_rows', 'added'), DEFAULT_TTL_SECONDS);
		expectExpiresIn(await expiresAt('default_ttl_probe_rows', 'existing'), DEFAULT_TTL_SECONDS);
	});

	it('leaves tables without a default untouched', async () => {
		await executor.executeQuery(NoTtlProbe.insert({id: 'plain', value: 'a', note: null}));
		expect(await expiresAt('no_ttl_probe', 'plain')).toBeNull();
		await executor.executeQuery(NoTtlProbe.patchByPk({id: 'plain'}, {note: Db.set('patched')}));
		expect(await expiresAt('no_ttl_probe', 'plain')).toBeNull();
		await executor.executeQuery(NoTtlProbe.insertWithTtl({id: 'zero', value: 'a', note: null}, 0));
		expect(await expiresAt('no_ttl_probe', 'zero')).toBeNull();
	});

	it('gives rows an older image wrote the expiry of their last write and deletes the ones past it', async () => {
		const mentionWrittenAt = await seed('recent_mentions', 'rm-day', '1 day');
		await seed('recent_mentions', 'rm-week', '8 days');
		await seed('attachment_upload_traces_by_key', 'at-31', '31 days');
		await seed('attachment_upload_traces_by_key', 'at-29', '29 days');
		await seed('phone_lookup_cache', 'pl-8', '8 days');
		await seed('donor_magic_link_tokens', 'dm-hour', '1 hour');
		await seed('ipinfo_requests_by_hour', 'ip-day', '1 day');
		await seed('jobs_by_id', 'job', '100 days');
		await seed('users', 'user', '100 days');
		await seed('recent_mentions', 'rm-forever', '1 day', 'infinity');
		await seed('recent_mentions', 'rm-hour', '30 days', new Date(Date.now() + 3_600_000));

		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 4,
			expiring: 3,
			complete: true,
		});
		expect(await remaining()).toEqual([
			{table_name: 'attachment_upload_traces_by_key', row_key: 'at-29'},
			{table_name: 'ipinfo_requests_by_hour', row_key: 'ip-day'},
			{table_name: 'jobs_by_id', row_key: 'job'},
			{table_name: 'recent_mentions', row_key: 'rm-day'},
			{table_name: 'recent_mentions', row_key: 'rm-forever'},
			{table_name: 'recent_mentions', row_key: 'rm-hour'},
			{table_name: 'users', row_key: 'user'},
		]);

		const exact = await raw.query<{row_key: string; exact: boolean; unchanged: boolean | null}>(
			`SELECT row_key,
	expires_at = updated_at + CASE table_name WHEN 'recent_mentions' THEN interval '7 days' WHEN 'attachment_upload_traces_by_key' THEN interval '30 days' ELSE interval '90 days' END AS exact,
	CASE WHEN row_key = 'rm-day' THEN updated_at = $1::timestamptz END AS unchanged
FROM ${KV_TABLE}
WHERE row_key IN ('rm-day', 'at-29', 'ip-day')
ORDER BY row_key`,
			[mentionWrittenAt],
		);
		expect(exact.rows).toEqual([
			{row_key: 'at-29', exact: true, unchanged: null},
			{row_key: 'ip-day', exact: true, unchanged: null},
			{row_key: 'rm-day', exact: true, unchanged: true},
		]);
		const untouched = await raw.query<{row_key: string; state: string}>(
			`SELECT row_key, CASE WHEN expires_at IS NULL THEN 'unset' WHEN expires_at = 'infinity' THEN 'forever' ELSE 'set' END AS state
FROM ${KV_TABLE}
WHERE row_key IN ('job', 'user', 'rm-forever', 'rm-hour')
ORDER BY row_key`,
		);
		expect(untouched.rows).toEqual([
			{row_key: 'job', state: 'unset'},
			{row_key: 'rm-forever', state: 'forever'},
			{row_key: 'rm-hour', state: 'set'},
			{row_key: 'user', state: 'unset'},
		]);

		expect(await markerCount()).toBe(0);
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await markerCount()).toBe(1);
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toBeNull();
	});

	it('checks again a day after a clean pass', async () => {
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toBeNull();

		await seed('recent_mentions', 'rm-rolled-back', '1 day');
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toBeNull();
		const before = await raw.query(`SELECT expires_at FROM ${KV_TABLE} WHERE row_key = 'rm-rolled-back'`);
		expect(before.rows).toEqual([{expires_at: null}]);

		await ageMarker();
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 1,
			complete: true,
		});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toBeNull();
	});

	it('pages through more rows than one page holds and stops at its deadline', async () => {
		await raw.query(
			`INSERT INTO ${KV_TABLE} (table_name, partition_key, row_key, row_data, updated_at)
SELECT 'recent_mentions', 'rm-' || lpad(g::text, 5, '0'), 'rm-' || lpad(g::text, 5, '0'), '{}'::jsonb, now() - interval '1 day'
FROM generate_series(1, 2300) g`,
		);

		expect(await expireLegacyDefaultTtlRows(raw, Date.now() - 1)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: false,
		});
		expect(await markerCount()).toBe(0);
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 2300,
			complete: true,
		});
		const unset = await raw.query<{n: number}>(
			`SELECT count(*)::int AS n FROM ${KV_TABLE} WHERE table_name = 'recent_mentions' AND expires_at IS NULL`,
		);
		expect(unset.rows[0]).toEqual({n: 0});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toBeNull();
	});

	it('saves where a run stopped and starts the next run there', async () => {
		const first = DEFAULT_TTL_TABLES[0]!.name;
		const last = DEFAULT_TTL_TABLES.at(-1)!.name;
		await seed(first, 'a', '1 hour');
		await seed(first, 'z', '1 hour');
		await seed(last, 'k', '1 hour');

		expect(await expireLegacyDefaultTtlRows(raw, Date.now() - 1)).toEqual({deleted: 0, expiring: 0, complete: false});
		expect(await resumePoint()).toEqual({table: first, row_key: '', unset: 0});

		await raw.query(
			`UPDATE ${KV_TABLE} SET row_data = jsonb_build_object('table', $1::text, 'row_key', 'm', 'unset', 0) WHERE table_name = '__fluxer_schema_migrations' AND row_key = $2`,
			[first, DEFAULT_TTL_EXPIRY_RESUME],
		);
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 2,
			complete: true,
		});
		const untouched = await raw.query<{expires_at: Date | null}>(
			`SELECT expires_at FROM ${KV_TABLE} WHERE table_name = $1 AND row_key = 'a'`,
			[first],
		);
		expect(untouched.rows).toEqual([{expires_at: null}]);
		expect(await resumePoint()).toBeNull();
		expect(await markerCount()).toBe(0);

		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 1,
			complete: true,
		});
		expect(await expireLegacyDefaultTtlRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await markerCount()).toBe(1);
	});
});
