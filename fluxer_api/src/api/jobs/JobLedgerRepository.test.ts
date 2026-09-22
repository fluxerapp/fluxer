// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CassandraQueryExecutorForTesting,
	executeQuery,
	fetchMany,
	fetchOne,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import type {JobStatus} from '@app/api/database/types/JobLedgerTypes';
import {
	EXPIRED_JOB_ERROR,
	JOB_LEDGER_TTL_SECONDS,
	JOB_STALE_AFTER_MS,
	JobLedgerRepository,
} from '@app/api/jobs/JobLedgerRepository';
import {describeListJobsPaging} from '@app/api/jobs/ListJobsPagingSuite';
import {JobsActive, JobsByDayBucket, JobsById} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

let executor: InMemoryCassandraQueryExecutor;

async function createJob(repository: JobLedgerRepository, jobId: bigint, taskType: string): Promise<void> {
	await repository.createJob({
		jobId,
		taskType,
		payload: {},
		requestedByUserId: null,
		auditLogReason: null,
		maxAttempts: 3,
		runAt: null,
		jetStreamLane: null,
		jetStreamSeq: null,
	});
}

async function createJobAt(repository: JobLedgerRepository, jobId: bigint): Promise<Date> {
	return repository.createJob({
		jobId,
		taskType: 'batchGuildAuditLogMessageDeletes',
		payload: {},
		requestedByUserId: null,
		auditLogReason: null,
		maxAttempts: 3,
		runAt: null,
		jetStreamLane: null,
		jetStreamSeq: null,
	});
}

async function listJobIdsByStatus(repository: JobLedgerRepository, status: JobStatus): Promise<Array<bigint>> {
	const result = await repository.listJobs({limit: 50, cursor: null, filters: {status}, maxLookbackDays: 1});
	return result.jobs.map((job) => job.job_id);
}

describe('JobLedgerRepository listJobs status filter', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('matches the live status of a succeeded job rather than the creation-time bucket snapshot', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 1n, 'syncDisposableEmailDomains');
		await repository.markSucceeded(1n, null);

		expect(await listJobIdsByStatus(repository, 'succeeded')).toEqual([1n]);
		expect(await listJobIdsByStatus(repository, 'queued')).toEqual([]);
	});

	it('matches the live status of a dead-lettered job', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 2n, 'syncDisposableEmailDomains');
		await repository.markDeadletter(2n, 'boom');

		expect(await listJobIdsByStatus(repository, 'deadletter')).toEqual([2n]);
		expect(await listJobIdsByStatus(repository, 'queued')).toEqual([]);
	});

	it('still returns a job that has not left the queue under status=queued', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 3n, 'syncDisposableEmailDomains');

		expect(await listJobIdsByStatus(repository, 'queued')).toEqual([3n]);
		expect(await listJobIdsByStatus(repository, 'running')).toEqual([]);
	});

	it('keeps the other filters working alongside the status filter', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 4n, 'syncDisposableEmailDomains');
		await createJob(repository, 5n, 'processExpiredPremium');
		await repository.markSucceeded(4n, null);
		await repository.markSucceeded(5n, null);

		const result = await repository.listJobs({
			limit: 50,
			cursor: null,
			filters: {status: 'succeeded', taskType: 'processExpiredPremium'},
			maxLookbackDays: 1,
		});
		expect(result.jobs.map((job) => job.job_id)).toEqual([5n]);
	});
});

describe('JobLedgerRepository listJobs pagination', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('emits a cursor for a page that filled exactly on the bucket boundary', async () => {
		const repository = new JobLedgerRepository();
		for (let index = 0; index < 3; index++) {
			await createJob(repository, BigInt(index + 1), 'syncDisposableEmailDomains');
		}

		const result = await repository.listJobs({limit: 3, cursor: null, filters: {}, maxLookbackDays: 1});

		expect(result.jobs).toHaveLength(3);
		expect(result.nextCursor).not.toBeNull();
	});

	it('emits no cursor for a page that did not fill', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 1n, 'syncDisposableEmailDomains');

		const result = await repository.listJobs({limit: 3, cursor: null, filters: {}, maxLookbackDays: 1});

		expect(result.jobs).toHaveLength(1);
		expect(result.nextCursor).toBeNull();
	});

	it('returns every match of a task type filter that sits past the unfiltered page window', async () => {
		const repository = new JobLedgerRepository();
		for (let index = 0; index < 60; index++) {
			await createJob(repository, BigInt(index + 1), 'syncDisposableEmailDomains');
		}
		for (let index = 0; index < 5; index++) {
			await createJob(repository, BigInt(1_000 + index), 'processExpiredPremium');
		}

		const result = await repository.listJobs({
			limit: 50,
			cursor: null,
			filters: {taskType: 'processExpiredPremium'},
			maxLookbackDays: 1,
		});

		expect(result.jobs.map((job) => job.job_id)).toEqual([1_004n, 1_003n, 1_002n, 1_001n, 1_000n]);
	});
});

describe('JobLedgerRepository listJobs on the in-memory executor', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	describeListJobsPaging();
});

let staleSequence = 0;

function jobIdAgedDays(days: number): bigint {
	staleSequence += 1;
	return createSnowflake({timestamp: Date.now() - days * 86_400_000, sequence: staleSequence % 4096, workerId: 1});
}

async function createAgedJob(repository: JobLedgerRepository, days: number): Promise<bigint> {
	const jobId = jobIdAgedDays(days);
	await createJob(repository, jobId, 'syncUrlBlocklists');
	return jobId;
}

async function activeJobIds(repository: JobLedgerRepository): Promise<Array<bigint>> {
	return (await repository.listActiveJobs()).map((job) => job.job_id).sort((a, b) => (a < b ? -1 : 1));
}

function sweep(repository: JobLedgerRepository, maxCleared = 100) {
	return repository.expireStaleActiveJobs({
		staleBeforeMs: Date.now() - JOB_STALE_AFTER_MS,
		pageSize: 100,
		maxCleared,
	});
}

describe('JobLedgerRepository expireStaleActiveJobs', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('dead-letters queued and running jobs the jobs stream has outlived and keeps younger ones active', async () => {
		const repository = new JobLedgerRepository();
		const staleQueued = await createAgedJob(repository, 9);
		const staleRunning = await createAgedJob(repository, 9);
		await repository.markRunning(staleRunning, 'batch');
		const weekOld = await createAgedJob(repository, 7);
		const fresh = await createAgedJob(repository, 0);

		expect(await sweep(repository)).toEqual({cleared: 2, expired: 2, complete: true});

		for (const jobId of [staleQueued, staleRunning]) {
			const job = await repository.getJob(jobId);
			expect(job?.status).toBe('deadletter');
			expect(job?.error_message).toBe(EXPIRED_JOB_ERROR);
			expect(job?.completed_at).toBeInstanceOf(Date);
		}
		expect(await activeJobIds(repository)).toEqual([weekOld, fresh].sort((a, b) => (a < b ? -1 : 1)));
		expect((await repository.getJob(weekOld))?.status).toBe('queued');
	});

	it('drops a stale active row without touching a finished or missing job', async () => {
		const repository = new JobLedgerRepository();
		const finished = await createAgedJob(repository, 9);
		await repository.markSucceeded(finished, null);
		const orphan = jobIdAgedDays(9);
		for (const jobId of [finished, orphan]) {
			await executeQuery(
				JobsActive.patchByPkWithTtl({job_id: jobId}, {status: Db.set('running')}, JOB_LEDGER_TTL_SECONDS),
			);
		}

		expect(await sweep(repository)).toEqual({cleared: 2, expired: 0, complete: true});

		const job = await repository.getJob(finished);
		expect(job?.status).toBe('succeeded');
		expect(job?.error_message).toBeNull();
		expect(await fetchOne(JobsById.select({where: JobsById.where.eq('job_id')}).bind({job_id: orphan}))).toBeNull();
		expect(await fetchMany(JobsActive.select().bind({}))).toEqual([]);
	});

	it('stops at its per-run cap and picks up the rest on the next run', async () => {
		const repository = new JobLedgerRepository();
		for (let index = 0; index < 3; index += 1) {
			await createAgedJob(repository, 9);
		}

		expect(await sweep(repository, 2)).toEqual({cleared: 2, expired: 2, complete: false});
		expect(await activeJobIds(repository)).toHaveLength(1);
		expect(await sweep(repository, 2)).toEqual({cleared: 1, expired: 1, complete: true});
		expect(await activeJobIds(repository)).toEqual([]);
	});
});

describe('JobLedgerRepository getJob', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('hides a job row that lost its status, creation time or task type', async () => {
		const repository = new JobLedgerRepository();
		await createJob(repository, 9n, 'syncUrlBlocklists');
		expect((await repository.getJob(9n))?.status).toBe('queued');
		for (const column of ['status', 'created_at', 'task_type'] as const) {
			await createJob(repository, 9n, 'syncUrlBlocklists');
			await executeQuery(JobsById.patchByPk({job_id: 9n}, {[column]: Db.clear()}));
			expect(await repository.getJob(9n)).toBeNull();
		}
	});
});

describe('JobLedgerRepository discardJob', () => {
	let inner: InMemoryCassandraQueryExecutor;
	let log: Array<string>;
	let failDeleteOn: string | null;

	beforeEach(() => {
		inner = new InMemoryCassandraQueryExecutor();
		log = [];
		failDeleteOn = null;
		const wrapper: CassandraQueryExecutorForTesting = {
			async executeQuery<T>(query: PreparedQuery) {
				const meta = query.kvMeta;
				if (meta) log.push(`${meta.action} ${meta.table.name}`);
				if (meta?.action === 'delete' && meta.table.name === failDeleteOn) {
					throw new Error('write timeout');
				}
				return inner.executeQuery<T>(query);
			},
			executeBatch: (queries, atomic) => inner.executeBatch(queries, atomic),
		};
		setCassandraQueryExecutorForTesting(wrapper);
	});

	afterEach(() => {
		inner.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('removes every ledger row of a duplicate with three deletes and no read', async () => {
		const repository = new JobLedgerRepository();
		const createdAt = await createJobAt(repository, 7n);
		log.length = 0;

		await repository.discardJob(7n, createdAt);

		expect([...log].sort()).toEqual(['delete jobs_active', 'delete jobs_by_day_bucket', 'delete jobs_by_id']);
		expect(await repository.getJob(7n)).toBeNull();
		expect(await repository.listActiveJobs()).toEqual([]);
		expect(
			await fetchMany(
				JobsByDayBucket.select({where: JobsByDayBucket.where.eq('bucket_day')}).bind({
					bucket_day: createdAt.toISOString().slice(0, 10),
				}),
			),
		).toEqual([]);
	});

	it('keeps deleting the other ledger rows when one delete fails', async () => {
		const repository = new JobLedgerRepository();
		const createdAt = await createJobAt(repository, 8n);
		failDeleteOn = 'jobs_by_id';

		await expect(repository.discardJob(8n, createdAt)).rejects.toThrow('write timeout');

		expect(await repository.listActiveJobs()).toEqual([]);
		expect(
			await fetchMany(
				JobsByDayBucket.select({where: JobsByDayBucket.where.eq('bucket_day')}).bind({
					bucket_day: createdAt.toISOString().slice(0, 10),
				}),
			),
		).toEqual([]);
	});
});
