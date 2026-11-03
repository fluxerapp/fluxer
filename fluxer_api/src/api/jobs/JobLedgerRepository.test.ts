// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CassandraQueryExecutorForTesting,
	executeQuery,
	fetchMany,
	fetchOne,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {
	EXPIRED_JOB_ERROR,
	JOB_LEDGER_TTL_SECONDS,
	JOB_STALE_AFTER_MS,
	JobLedgerRepository,
} from '@app/api/jobs/JobLedgerRepository';
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

describe('JobLedgerRepository discardJob', () => {
	let inner: InMemoryCassandraQueryExecutor;
	let failDeleteOn: string | null;

	beforeEach(() => {
		inner = new InMemoryCassandraQueryExecutor();
		failDeleteOn = null;
		const wrapper: CassandraQueryExecutorForTesting = {
			async executeQuery<T>(query: PreparedQuery) {
				const meta = query.kvMeta;
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
