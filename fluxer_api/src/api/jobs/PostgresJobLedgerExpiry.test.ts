// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import {createServer} from 'node:net';
import {BatchBuilder, setCassandraQueryExecutorForTesting} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {ensurePostgresKvSchema, PostgresKvQueryExecutor} from '@app/api/database/PostgresKvQueryExecutor';
import type {JobActiveRow, JobByDayBucketRow, JobByIdRow, JobStatus} from '@app/api/database/types/JobLedgerTypes';
import {
	EXPIRED_JOB_ERROR,
	JOB_LEDGER_TTL_SECONDS,
	JOB_STALE_AFTER_MS,
	JobLedgerRepository,
} from '@app/api/jobs/JobLedgerRepository';
import {describeListJobsPaging} from '@app/api/jobs/ListJobsPagingSuite';
import {expireLegacyJobLedgerRows} from '@app/api/jobs/PostgresJobLedgerExpiry';
import {JobsActive, JobsByDayBucket, JobsById} from '@app/api/Tables';
import {startDockerContainer} from '@app/api/test/DockerTestContainer';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {
	getDefaultPostgresClient,
	type IPostgresClient,
	initPostgres,
	shutdownPostgres,
} from '@pkgs/postgres/src/Client';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const KV_TABLE = 'kv_job_ledger_expiry';
const CONTAINER = `fluxer-kvjobs-${process.pid.toString(36)}-${Date.now().toString(36)}`;
const dockerAvailable = spawnSync('docker', ['version'], {stdio: 'ignore'}).status === 0;
const DAY_MS = 86_400_000;
const ADMIN_USER_ID = 1_234_567_890_123n;

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

let sequence = 0;

function jobIdAgedDays(days: number): bigint {
	sequence += 1;
	return createSnowflake({timestamp: Date.now() - days * DAY_MS, sequence: sequence % 4096, workerId: 1});
}

interface LegacyJob {
	jobId: bigint;
	createdAt: Date;
}

async function seedLegacyJob(
	executor: PostgresKvQueryExecutor,
	opts: {ageDays: number; status: JobStatus; requestedBy: bigint | null; active: boolean},
): Promise<LegacyJob> {
	const jobId = jobIdAgedDays(opts.ageDays);
	const createdAt = new Date(Date.now() - opts.ageDays * DAY_MS);
	const idRow: JobByIdRow = {
		job_id: jobId,
		task_type: opts.requestedBy === null ? 'flushUserActivityBuffer' : 'refreshSearchIndex',
		status: opts.status,
		progress_current: null,
		progress_total: null,
		progress_message: null,
		payload: '{}',
		result: null,
		error_message: null,
		created_at: createdAt,
		started_at: null,
		completed_at: opts.status === 'succeeded' ? createdAt : null,
		requested_by_user_id: opts.requestedBy,
		audit_log_reason: null,
		jet_stream_seq: '1',
		jet_stream_lane: 'batch',
		attempts: 0,
		max_attempts: 5,
		run_at: null,
		cancel_requested: false,
		context_link: null,
	};
	const bucketRow: JobByDayBucketRow = {
		bucket_day: createdAt.toISOString().slice(0, 10),
		created_at: createdAt,
		job_id: jobId,
		task_type: idRow.task_type,
		status: 'queued',
		requested_by_user_id: opts.requestedBy,
	};
	await executor.executeQuery(JobsById.insert(idRow));
	await executor.executeQuery(JobsByDayBucket.insert(bucketRow));
	if (opts.active) {
		const activeRow: JobActiveRow = {
			job_id: jobId,
			task_type: idRow.task_type,
			status: opts.status,
			requested_by_user_id: opts.requestedBy,
			created_at: createdAt,
			started_at: null,
		};
		await executor.executeQuery(JobsActive.insert(activeRow));
	}
	return {jobId, createdAt};
}

describe.skipIf(!dockerAvailable)('job ledger expiry against postgres', () => {
	let raw: IPostgresClient;
	let executor: PostgresKvQueryExecutor;

	async function jobRows(
		jobId: bigint,
	): Promise<Array<{table_name: string; expires_at: Date | null; row_data: never}>> {
		const result = await raw.query<{table_name: string; expires_at: Date | null; row_data: never}>(
			`SELECT table_name, expires_at, row_data FROM ${KV_TABLE}
WHERE table_name IN ('jobs_by_id', 'jobs_active', 'jobs_by_day_bucket')
	AND (row_key = $1 OR split_part(row_key, chr(31), 3) = $1)
ORDER BY table_name`,
			[JSON.stringify({__fluxer_type: 'bigint', value: jobId.toString()})],
		);
		return result.rows;
	}

	async function waitForLockWait(): Promise<void> {
		for (let attempt = 0; attempt < 400; attempt += 1) {
			const waiting = await raw.query<{n: number}>(
				`SELECT count(*)::int AS n FROM pg_stat_activity WHERE datname = current_database() AND wait_event_type = 'Lock'`,
			);
			if (waiting.rows[0]!.n > 0) return;
			await sleep(25);
		}
		throw new Error('the pass never waited on the writer');
	}

	async function forgetLedgerExpiry(): Promise<void> {
		await raw.query(
			`UPDATE ${KV_TABLE} SET expires_at = NULL WHERE table_name IN ('jobs_by_id', 'jobs_by_day_bucket')`,
		);
	}

	async function forgetExpiryOf(...jobIds: Array<bigint>): Promise<void> {
		const keys = jobIds.map((jobId) => JSON.stringify({__fluxer_type: 'bigint', value: jobId.toString()}));
		await raw.query(
			`UPDATE ${KV_TABLE} SET expires_at = NULL
WHERE table_name IN ('jobs_by_id', 'jobs_by_day_bucket')
	AND (row_key = ANY($1::text[]) OR split_part(row_key, chr(31), 3) = ANY($1::text[]))`,
			[keys],
		);
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
		setCassandraQueryExecutorForTesting(executor);
	});

	afterAll(async () => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
		await shutdownPostgres().catch(() => {});
		spawnSync('docker', ['rm', '-f', CONTAINER], {stdio: 'ignore'});
	});

	it('writes every ledger row with an expiry and keeps it through the job lifecycle', async () => {
		const repository = new JobLedgerRepository();
		const jobId = jobIdAgedDays(0);
		await repository.createJob({
			jobId,
			taskType: 'refreshSearchIndex',
			payload: {},
			requestedByUserId: ADMIN_USER_ID,
			auditLogReason: null,
			maxAttempts: 5,
			runAt: null,
			jetStreamLane: 'batch',
			jetStreamSeq: null,
		});
		await repository.setJetStreamSeq(jobId, '7');
		await repository.markRunning(jobId, 'batch');
		await repository.reportProgress(jobId, 1, 2, 'half');

		const running = await jobRows(jobId);
		expect(running.map((row) => row.table_name)).toEqual(['jobs_active', 'jobs_by_day_bucket', 'jobs_by_id']);
		for (const row of running) {
			expect(row.expires_at).not.toBeNull();
			const remainingSeconds = (row.expires_at!.getTime() - Date.now()) / 1000;
			expect(remainingSeconds).toBeGreaterThan(JOB_LEDGER_TTL_SECONDS - 60);
			expect(remainingSeconds).toBeLessThanOrEqual(JOB_LEDGER_TTL_SECONDS);
		}

		await repository.markSucceeded(jobId, {ok: true});
		const done = await jobRows(jobId);
		expect(done.map((row) => row.table_name)).toEqual(['jobs_by_day_bucket', 'jobs_by_id']);
		expect(done.every((row) => row.expires_at !== null)).toBe(true);
		expect((await repository.getJob(jobId))?.status).toBe('succeeded');
	});

	it('never leaves a row without an expiry when a patch lands on a job that is gone', async () => {
		const repository = new JobLedgerRepository();
		const patches: Array<(jobId: bigint) => Promise<void>> = [
			(jobId) => repository.markRunning(jobId, 'batch'),
			(jobId) => repository.markSucceeded(jobId, null),
			(jobId) => repository.markCancelled(jobId),
			(jobId) => repository.markDeadletter(jobId, 'boom'),
			(jobId) => repository.reportProgress(jobId, 1, null, null),
			(jobId) => repository.setContextLink(jobId, '/admin/jobs'),
			(jobId) => repository.setJetStreamSeq(jobId, '1'),
			(jobId) => repository.requestCancel(jobId),
		];
		for (const patch of patches) {
			const jobId = jobIdAgedDays(0);
			await patch(jobId);
			const rows = await jobRows(jobId);
			expect(rows.length).toBeGreaterThan(0);
			expect(rows.every((row) => row.expires_at !== null)).toBe(true);
			expect(await repository.getJob(jobId)).toBeNull();
		}
		expect(await repository.listActiveJobs()).toEqual([]);
	});

	it('discards every row of a job that never reached the stream', async () => {
		const repository = new JobLedgerRepository();
		const jobId = jobIdAgedDays(0);
		const createdAt = await repository.createJob({
			jobId,
			taskType: 'batchGuildAuditLogMessageDeletes',
			payload: {guildId: '1'},
			requestedByUserId: null,
			auditLogReason: null,
			maxAttempts: 3,
			runAt: new Date(Date.now() + 30_000),
			jetStreamLane: 'batch',
			jetStreamSeq: null,
		});
		expect(await jobRows(jobId)).toHaveLength(3);

		await repository.discardJob(jobId, createdAt);

		expect(await jobRows(jobId)).toEqual([]);
	});

	it('clears legacy rows by the same rules the expiry now enforces', async () => {
		const repository = new JobLedgerRepository();
		const cronDone = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'succeeded',
			requestedBy: null,
			active: false,
		});
		const cronStuck = await seedLegacyJob(executor, {ageDays: 20, status: 'queued', requestedBy: null, active: true});
		const partialId = jobIdAgedDays(20);
		await executor.executeQuery(
			JobsById.patchByPk({job_id: partialId}, {status: Db.set('succeeded'), completed_at: Db.set(new Date())}),
		);
		const adminDone = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'succeeded',
			requestedBy: ADMIN_USER_ID,
			active: false,
		});
		const adminStuck = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'queued',
			requestedBy: ADMIN_USER_ID,
			active: true,
		});
		const adminRunning = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'running',
			requestedBy: ADMIN_USER_ID,
			active: true,
		});
		const adminAncient = await seedLegacyJob(executor, {
			ageDays: 100,
			status: 'succeeded',
			requestedBy: ADMIN_USER_ID,
			active: false,
		});
		const cronInFlight = await seedLegacyJob(executor, {
			ageDays: 2,
			status: 'queued',
			requestedBy: null,
			active: true,
		});
		await forgetLedgerExpiry();
		const fresh = jobIdAgedDays(0);
		await repository.createJob({
			jobId: fresh,
			taskType: 'syncUrlBlocklists',
			payload: {},
			requestedByUserId: null,
			auditLogReason: null,
			maxAttempts: 5,
			runAt: null,
			jetStreamLane: 'batch',
			jetStreamSeq: null,
		});
		const freshBefore = await jobRows(fresh);
		await raw.query(
			`INSERT INTO ${KV_TABLE} (table_name, partition_key, row_key, row_data) VALUES ('users', 'u1', 'u1', '{}'::jsonb)`,
		);

		expect(await repository.getJob(partialId)).toBeNull();

		const first = await expireLegacyJobLedgerRows(raw, Date.now() + 60_000);
		expect(first).toEqual({deleted: 10, expiring: 6, complete: true});

		expect(await jobRows(cronDone.jobId)).toEqual([]);
		expect(await jobRows(cronStuck.jobId)).toEqual([]);
		expect(await jobRows(partialId)).toEqual([]);
		expect(await jobRows(adminAncient.jobId)).toEqual([]);

		for (const kept of [adminDone, adminStuck, adminRunning]) {
			const rows = await jobRows(kept.jobId);
			expect(rows.map((row) => row.table_name)).toEqual(['jobs_by_day_bucket', 'jobs_by_id']);
			for (const row of rows) {
				const expected = kept.createdAt.getTime() + JOB_LEDGER_TTL_SECONDS * 1000;
				expect(Math.abs(row.expires_at!.getTime() - expected)).toBeLessThan(2000);
			}
		}
		expect((await repository.getJob(adminDone.jobId))?.status).toBe('succeeded');
		for (const stuck of [adminStuck, adminRunning]) {
			const expired = await repository.getJob(stuck.jobId);
			expect(expired?.status).toBe('deadletter');
			expect(expired?.error_message).toBe('Expired from the job queue');
			expect(expired?.completed_at).toBeInstanceOf(Date);
		}

		const inFlight = await jobRows(cronInFlight.jobId);
		expect(inFlight.map((row) => row.table_name)).toEqual(['jobs_active', 'jobs_by_day_bucket', 'jobs_by_id']);
		expect(inFlight.every((row) => row.expires_at === null)).toBe(true);
		expect((await repository.getJob(cronInFlight.jobId))?.status).toBe('queued');

		expect(await jobRows(fresh)).toEqual(freshBefore);
		const users = await raw.query(`SELECT expires_at FROM ${KV_TABLE} WHERE table_name = 'users'`);
		expect(users.rows).toEqual([{expires_at: null}]);

		const listed = await repository.listJobs({limit: 50, cursor: null, filters: {}, maxLookbackDays: 30});
		expect(listed.jobs.map((job) => job.job_id).sort()).toEqual(
			[adminDone.jobId, adminStuck.jobId, adminRunning.jobId, cronInFlight.jobId, fresh].sort(),
		);

		for (let pass = 0; pass < 2; pass += 1) {
			expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
				deleted: 0,
				expiring: 0,
				complete: true,
			});
		}
	});

	it('runs again a day after a clean pass and clears rows an older image wrote in between', async () => {
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		const rolledBack = await seedLegacyJob(executor, {ageDays: 30, status: 'queued', requestedBy: null, active: true});
		await forgetLedgerExpiry();
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toBeNull();

		await raw.query(
			`UPDATE ${KV_TABLE} SET row_data = jsonb_build_object('applied_at', now() - interval '2 days') WHERE row_key = 'job_ledger_expiry_v1'`,
		);
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 3,
			expiring: 0,
			complete: true,
		});
		expect(await jobRows(rolledBack.jobId)).toEqual([]);
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toBeNull();
	});

	it('expires stale active jobs page by page without fighting the legacy pass', async () => {
		const repository = new JobLedgerRepository();
		const legacyFirst = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'queued',
			requestedBy: ADMIN_USER_ID,
			active: true,
		});
		await forgetExpiryOf(legacyFirst.jobId);
		await expireLegacyJobLedgerRows(raw, Date.now() + 60_000);
		const legacyFirstRows = await jobRows(legacyFirst.jobId);

		const stale: Array<bigint> = [];
		for (let index = 0; index < 4; index += 1) {
			const jobId = jobIdAgedDays(9);
			await repository.createJob({
				jobId,
				taskType: 'syncUrlBlocklists',
				payload: {},
				requestedByUserId: null,
				auditLogReason: null,
				maxAttempts: 5,
				runAt: null,
				jetStreamLane: 'batch',
				jetStreamSeq: null,
			});
			stale.push(jobId);
		}
		await repository.markRunning(stale[0]!, 'batch');
		const fresh = jobIdAgedDays(0);
		await repository.createJob({
			jobId: fresh,
			taskType: 'syncUrlBlocklists',
			payload: {},
			requestedByUserId: null,
			auditLogReason: null,
			maxAttempts: 5,
			runAt: null,
			jetStreamLane: 'batch',
			jetStreamSeq: null,
		});
		const legacyStale = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'running',
			requestedBy: ADMIN_USER_ID,
			active: true,
		});
		const legacyYoung = await seedLegacyJob(executor, {ageDays: 2, status: 'queued', requestedBy: null, active: true});
		await forgetExpiryOf(legacyStale.jobId, legacyYoung.jobId);
		const orphan = jobIdAgedDays(9);
		await executor.executeQuery(
			JobsActive.patchByPkWithTtl({job_id: orphan}, {status: Db.set('running')}, JOB_LEDGER_TTL_SECONDS),
		);

		const sweep = () =>
			repository.expireStaleActiveJobs({staleBeforeMs: Date.now() - JOB_STALE_AFTER_MS, pageSize: 2, maxCleared: 100});
		expect(await sweep()).toEqual({cleared: 6, expired: 5, complete: true});

		const active = (await repository.listActiveJobs()).map((job) => job.job_id).sort();
		expect(active).toEqual([fresh, legacyYoung.jobId].sort());
		for (const jobId of [...stale, legacyStale.jobId]) {
			const job = await repository.getJob(jobId);
			expect(job?.status).toBe('deadletter');
			expect(job?.error_message).toBe(EXPIRED_JOB_ERROR);
			const byId = (await jobRows(jobId)).find((row) => row.table_name === 'jobs_by_id');
			expect(Math.abs(byId!.expires_at!.getTime() - (Date.now() + JOB_LEDGER_TTL_SECONDS * 1000))).toBeLessThan(60_000);
		}
		expect(await jobRows(orphan)).toEqual([]);
		expect(await jobRows(legacyFirst.jobId)).toEqual(legacyFirstRows);

		const legacyStaleById = (await jobRows(legacyStale.jobId)).find((row) => row.table_name === 'jobs_by_id');
		await expireLegacyJobLedgerRows(raw, Date.now() + 60_000);
		expect((await jobRows(legacyStale.jobId)).find((row) => row.table_name === 'jobs_by_id')).toEqual(legacyStaleById);
		expect((await repository.listActiveJobs()).map((job) => job.job_id)).toContain(legacyYoung.jobId);
		expect(await sweep()).toEqual({cleared: 0, expired: 0, complete: true});
	});

	it('leaves rows alone when a live writer gives them an expiry while the pass waits on them', async () => {
		const cronDone = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'succeeded',
			requestedBy: null,
			active: false,
		});
		const adminDone = await seedLegacyJob(executor, {
			ageDays: 20,
			status: 'succeeded',
			requestedBy: ADMIN_USER_ID,
			active: false,
		});
		await forgetLedgerExpiry();
		const liveKeys = [cronDone.jobId, adminDone.jobId].map((jobId) =>
			JSON.stringify({__fluxer_type: 'bigint', value: jobId.toString()}),
		);
		let written!: () => void;
		const writerHoldsRows = new Promise<void>((resolve) => {
			written = resolve;
		});
		let release!: () => void;
		const released = new Promise<void>((resolve) => {
			release = resolve;
		});
		const writer = raw.transaction(async (db) => {
			await db.query(
				`UPDATE ${KV_TABLE} SET expires_at = now() + interval '1 hour', updated_at = now() WHERE table_name = 'jobs_by_id' AND row_key = ANY($1::text[])`,
				[liveKeys],
			);
			written();
			await released;
		});
		await writerHoldsRows;
		const pass = expireLegacyJobLedgerRows(raw, Date.now() + 60_000);
		await waitForLockWait();
		release();
		await writer;

		expect(await pass).toEqual({deleted: 1, expiring: 1, complete: true});
		for (const job of [cronDone, adminDone]) {
			const byId = (await jobRows(job.jobId)).find((row) => row.table_name === 'jobs_by_id');
			expect(byId).toBeDefined();
			const remainingSeconds = (byId!.expires_at!.getTime() - Date.now()) / 1000;
			expect(remainingSeconds).toBeGreaterThan(3000);
			expect(remainingSeconds).toBeLessThanOrEqual(3660);
		}
		expect((await new JobLedgerRepository().getJob(adminDone.jobId))?.status).toBe('succeeded');
	});

	it('pages through more legacy rows than one page holds and stops at its deadline', async () => {
		const batch = new BatchBuilder();
		for (let index = 0; index < 2300; index += 1) {
			const jobId = jobIdAgedDays(30);
			const createdAt = new Date(Date.now() - 30 * DAY_MS);
			batch.addPrepared(
				JobsByDayBucket.insert({
					bucket_day: createdAt.toISOString().slice(0, 10),
					created_at: createdAt,
					job_id: jobId,
					task_type: 'flushUserActivityBuffer',
					status: 'queued',
					requested_by_user_id: null,
				}),
			);
		}
		await batch.executeChunked(500, false);
		await forgetLedgerExpiry();

		expect(await expireLegacyJobLedgerRows(raw, Date.now() - 1)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: false,
		});
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 2300,
			expiring: 0,
			complete: true,
		});
		const left = await raw.query(`SELECT count(*)::int AS n FROM ${KV_TABLE} WHERE table_name = 'jobs_by_day_bucket'`);
		expect(left.rows[0]).toEqual({n: 0});
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toEqual({
			deleted: 0,
			expiring: 0,
			complete: true,
		});
		expect(await expireLegacyJobLedgerRows(raw, Date.now() + 60_000)).toBeNull();
	});
	describeListJobsPaging();
});
