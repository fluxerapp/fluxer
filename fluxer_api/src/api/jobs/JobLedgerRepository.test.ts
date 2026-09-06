// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {setCassandraQueryExecutorForTesting} from '../database/CassandraQueryExecution';
import type {JobStatus} from '../database/types/JobLedgerTypes';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {JobLedgerRepository} from './JobLedgerRepository';

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

		expect(result.jobs.map((job) => job.job_id)).toEqual([1_000n, 1_001n, 1_002n, 1_003n, 1_004n]);
	});
});
