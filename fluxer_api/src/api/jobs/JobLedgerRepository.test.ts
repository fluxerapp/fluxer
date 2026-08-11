// SPDX-License-Identifier: AGPL-3.0-or-later

import {ListJobsRequest} from '@fluxer/schema/src/domains/admin/JobsSchemas';
import {seconds} from 'itty-time';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {fetchMany, fetchOne, setCassandraQueryExecutorForTesting, upsertOne} from '../database/CassandraQueryExecution';
import {type CassandraParams, Db, type KvQueryMeta, type PreparedQuery} from '../database/CassandraTypes';
import type {JobActiveRow, JobActiveV2Row, JobByDayBucketRow, JobByIdRow} from '../database/types/JobLedgerTypes';
import {JobsActive, JobsActiveLegacy, JobsByDayBucket, JobsById} from '../Tables';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {JOB_HISTORY_RETENTION_SECONDS, JobLedgerRepository} from './JobLedgerRepository';

interface RecordedBatchQuery {
	query: string;
	params: object;
	meta?: KvQueryMeta;
}

class RecordingExecutor extends InMemoryCassandraQueryExecutor {
	readonly batchQueries: Array<RecordedBatchQuery> = [];
	readonly queries: Array<PreparedQuery> = [];
	beforeConditional: (() => Promise<void>) | null = null;
	afterConditional: (() => Promise<void>) | null = null;
	onActiveUpsert: (() => Promise<void>) | null = null;
	beforeActiveDelete: (() => Promise<void>) | null = null;
	onBucketUpsert: ((query: PreparedQuery) => Promise<void>) | null = null;

	override async executeQuery<T = Record<string, unknown>>(query: PreparedQuery): Promise<Array<T>> {
		this.queries.push(query);
		if (query.kvMeta?.table.name === 'jobs_active' && query.kvMeta.action === 'upsert' && this.onActiveUpsert) {
			await this.onActiveUpsert();
		}
		if (query.kvMeta?.table.name === 'jobs_active' && query.kvMeta.action === 'delete' && this.beforeActiveDelete) {
			const hook = this.beforeActiveDelete;
			this.beforeActiveDelete = null;
			await hook();
		}
		if (query.kvMeta?.table.name === 'jobs_by_day_bucket' && query.kvMeta.action === 'upsert' && this.onBucketUpsert) {
			await this.onBucketUpsert(query);
		}
		if ((query.kvMeta?.condition || query.kvMeta?.conditions) && this.beforeConditional) {
			const hook = this.beforeConditional;
			this.beforeConditional = null;
			await hook();
		}
		const result = await super.executeQuery<T>(query);
		if ((query.kvMeta?.condition || query.kvMeta?.conditions) && this.afterConditional) {
			const hook = this.afterConditional;
			this.afterConditional = null;
			await hook();
		}
		return result;
	}

	override async executeBatch(queries: Array<RecordedBatchQuery>): Promise<void> {
		this.batchQueries.push(...queries);
		await super.executeBatch(queries);
	}
}

let executor: RecordingExecutor;
let repository: JobLedgerRepository;
let leaseCounter: number;
const claimedLeaseTokens = new Map<bigint, string>();

async function claimRunning(jobId: bigint, lane: string): Promise<boolean> {
	leaseCounter += 1;
	const leaseToken = `test-lease-${leaseCounter}`;
	const claimed = await repository.claimJob(jobId, lane, leaseToken, new Date(), seconds('1 day') * 1000);
	if (claimed !== null) claimedLeaseTokens.set(jobId, leaseToken);
	return claimed !== null;
}

function leaseTokenFor(jobId: bigint): string {
	const leaseToken = claimedLeaseTokens.get(jobId);
	if (!leaseToken) throw new Error(`Job ${jobId.toString()} has no claimed test lease`);
	return leaseToken;
}

async function fetchBucket(jobId: bigint, createdAt: Date): Promise<JobByDayBucketRow | null> {
	return fetchOne<JobByDayBucketRow>(
		JobsByDayBucket.select({
			where: [
				JobsByDayBucket.where.eq('bucket_day'),
				JobsByDayBucket.where.eq('created_at'),
				JobsByDayBucket.where.eq('job_id'),
			],
		}).bind({bucket_day: createdAt.toISOString().slice(0, 10), created_at: createdAt, job_id: jobId}),
	);
}

describe('JobLedgerRepository', () => {
	beforeEach(() => {
		executor = new RecordingExecutor();
		setCassandraQueryExecutorForTesting(executor);
		leaseCounter = 0;
		claimedLeaseTokens.clear();
		repository = new JobLedgerRepository();
	});

	afterEach(() => {
		vi.useRealTimers();
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('uses the last returned row as a stable cursor without skipping the next job', async () => {
		vi.useFakeTimers();
		for (const [jobId, timestamp] of [
			[65n, '2026-08-10T00:01:00.000Z'],
			[66n, '2026-08-10T00:02:00.000Z'],
			[67n, '2026-08-10T00:03:00.000Z'],
		] as const) {
			vi.setSystemTime(new Date(timestamp));
			await createQueuedJob(jobId, null);
		}
		const first = await repository.listJobs({limit: 1, cursor: null, filters: {}, maxLookbackDays: 1});
		const second = await repository.listJobs({limit: 1, cursor: first.nextCursor, filters: {}, maxLookbackDays: 1});

		expect(first.jobs.map((job) => job.job_id)).toEqual([67n]);
		expect(first.nextCursor?.jobId).toBe(67n);
		expect(second.jobs.map((job) => job.job_id)).toEqual([66n]);
	});

	it('rejects a history cursor whose timestamp does not belong to its UTC day bucket', () => {
		expect(
			ListJobsRequest.safeParse({
				cursor: {
					bucket_day: '2026-08-10',
					created_at: '2026-08-11T00:00:00.000Z',
					job_id: '1',
				},
			}),
		).toMatchObject({success: false});
	});

	it('bounds active-index reads and authority hydration with an opaque continuation', async () => {
		await createQueuedJob(3001n, null);
		await createQueuedJob(3002n, null);
		await createQueuedJob(3003n, null);

		const jobs = await collectAllActiveJobs(2);
		expect(jobs.map((job) => job.job_id).sort()).toEqual([3001n, 3002n, 3003n]);
	});

	it('continuously sweeps legacy rows skipped by mutable offset paging', async () => {
		const createdAt = new Date('2026-08-01T00:00:00.000Z');
		for (const jobId of [1n, 2n, 3n, 4n]) {
			await upsertOne(
				JobsActiveLegacy.upsertAll({
					job_id: jobId,
					task_type: 'legacy',
					status: 'queued',
					requested_by_user_id: null,
					created_at: createdAt,
					started_at: null,
				}),
			);
		}
		const input = {limit: 2, now: createdAt, staleBefore: createdAt};
		expect((await repository.reconcileActiveJobs(input)).removedMissing).toBe(2);
		expect((await repository.reconcileActiveJobs(input)).removedMissing).toBe(0);
		expect((await repository.reconcileActiveJobs(input)).removedMissing).toBe(2);
		expect(await fetchMany<JobActiveRow>(JobsActiveLegacy.select().bind({}))).toEqual([]);
	});

	it('paginates equal-timestamp rows by descending job id', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T00:04:00.000Z'));
		await createQueuedJob(71n, null);
		await createQueuedJob(72n, null);

		const first = await repository.listJobs({limit: 1, cursor: null, filters: {}, maxLookbackDays: 1});
		const second = await repository.listJobs({limit: 1, cursor: first.nextCursor, filters: {}, maxLookbackDays: 1});

		expect(first.jobs.map((job) => job.job_id)).toEqual([72n]);
		expect(second.jobs.map((job) => job.job_id)).toEqual([71n]);
	});

	it('continues scanning a bucket until sparse filters fill the page', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T00:01:00.000Z'));
		await createQueuedJob(68n, null);
		expect(await repository.markEnqueueFailed(68n, 'expected')).toBe(true);
		vi.setSystemTime(new Date('2026-08-10T00:02:00.000Z'));
		await createQueuedJob(69n, null);
		vi.setSystemTime(new Date('2026-08-10T00:03:00.000Z'));
		await createQueuedJob(70n, null);

		const page = await repository.listJobs({
			limit: 1,
			cursor: null,
			filters: {status: 'failed'},
			maxLookbackDays: 1,
		});

		expect(page.jobs.map((job) => job.job_id)).toEqual([68n]);
	});

	it('writes terminal bucket retention before the authoritative terminal CAS', async () => {
		await createQueuedJob(76n, null);
		const leaseToken = 'retention-lease';
		expect(await repository.claimJob(76n, 'worker-batch', leaseToken, new Date(), 60_000)).not.toBeNull();
		let retainedBucketBeforeCas = false;
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			retainedBucketBeforeCas = executor.queries.some(
				(query) => query.kvMeta?.table.name === 'jobs_by_day_bucket' && typeof query.kvMeta.ttlParamName === 'string',
			);
		};

		expect(await repository.markSucceeded(76n, {ok: true}, leaseToken)).toBe(true);
		expect(retainedBucketBeforeCas).toBe(true);
	});

	it('repairs the history bucket when reconciliation terminalization loses to lease renewal', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(78n, null);
		const before = await repository.getJob(78n);
		expect(before).not.toBeNull();
		expect(await repository.claimJob(78n, 'worker-batch', 'lease-a', new Date(), 60_000)).not.toBeNull();
		vi.setSystemTime(new Date('2026-08-20T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(await repository.renewLease(78n, 'lease-a', new Date(), 120_000)).toBe(true);
		};

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-12T00:00:00.000Z'),
			limit: 100,
		});

		const authoritative = await repository.getJob(78n);
		const bucket = await fetchBucket(78n, before!.created_at);
		expect(result.failedExpiredRunning).toBe(0);
		expect(authoritative?.status).toBe('running');
		expect(bucket?.status).toBe('running');
	});

	it('repairs the winning terminal bucket when a conflicting terminal CAS loses', async () => {
		await createQueuedJob(82n, null);
		const before = await repository.getJob(82n);
		expect(await repository.claimJob(82n, 'worker-batch', 'lease-terminal', new Date(), 60_000)).not.toBeNull();
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(await repository.markSucceeded(82n, {winner: true}, 'lease-terminal')).toBe(true);
		};

		expect(await repository.markFailed(82n, 'loser', 'lease-terminal')).toBe(false);
		expect(await repository.getJob(82n)).toMatchObject({status: 'succeeded'});
		expect(await fetchBucket(82n, before!.created_at)).toMatchObject({status: 'succeeded'});
	});

	it('repairs a speculative terminal bucket when the authoritative CAS errors', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(79n, null);
		const before = await repository.getJob(79n);
		expect(await repository.claimJob(79n, 'worker-batch', 'lease-error', new Date(), 60_000)).not.toBeNull();
		executor.beforeConditional = async () => {
			throw new Error('conditional write unavailable');
		};

		await expect(repository.markFailed(79n, 'failed', 'lease-error')).rejects.toThrow('conditional write unavailable');
		expect((await repository.getJob(79n))?.status).toBe('running');
		expect((await fetchBucket(79n, before!.created_at))?.status).toBe('running');
		vi.setSystemTime(new Date('2026-08-09T00:00:00.000Z'));
		expect(await fetchBucket(79n, before!.created_at)).not.toBeNull();
	});

	it('uses reconciliation to repair a speculative bucket after immediate conflict repair also fails', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(81n, null);
		const before = await repository.getJob(81n);
		expect(await repository.claimJob(81n, 'worker-batch', 'lease-repair', new Date(), 60_000)).not.toBeNull();
		executor.beforeConditional = async () => {
			throw new Error('conditional write unavailable');
		};
		executor.onBucketUpsert = async (query) => {
			if ((query.params as CassandraParams).status === 'running') {
				executor.onBucketUpsert = null;
				throw new Error('bucket repair unavailable');
			}
		};

		await expect(repository.markFailed(81n, 'failed', 'lease-repair')).rejects.toThrow('conditional write unavailable');
		expect((await fetchBucket(81n, before!.created_at))?.status).toBe('failed');

		await repository.reconcileActiveJobs({
			now: new Date('2026-08-02T00:00:00.000Z'),
			staleBefore: new Date('2026-07-25T00:00:00.000Z'),
			limit: 100,
		});
		expect((await fetchBucket(81n, before!.created_at))?.status).toBe('running');
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		expect(await fetchBucket(81n, before!.created_at)).not.toBeNull();
	});

	it('repairs terminal indexes when retrying after active-index deletion fails', async () => {
		await createQueuedJob(80n, null);
		expect(await repository.claimJob(80n, 'worker-batch', 'lease-delete', new Date(), 60_000)).not.toBeNull();
		executor.beforeActiveDelete = async () => {
			throw new Error('active delete unavailable');
		};

		await expect(repository.markSucceeded(80n, {ok: true}, 'lease-delete')).rejects.toThrow(
			'active delete unavailable',
		);
		expect((await repository.getJob(80n))?.status).toBe('succeeded');
		expect((await collectAllActiveJobs()).map((job) => job.job_id)).not.toContain(80n);

		expect(await repository.markSucceeded(80n, {ok: true}, 'lease-delete')).toBe(true);
		expect(
			(await repository.listActiveJobs({limit: 200, pageState: null})).jobs.map((job) => job.job_id),
		).not.toContain(80n);
	});

	it('publishes terminal status consistently and removes active state', async () => {
		await repository.createJob({
			auditLogReason: null,
			jetStreamLane: 'maintenance',
			jetStreamSeq: null,
			jobId: 42n,
			maxAttempts: 5,
			payload: {},
			requestedByUserId: null,
			runAt: null,
			taskType: 'flushUserActivityBuffer',
		});

		expect(await claimRunning(42n, 'worker-batch')).toBe(true);
		await repository.markSucceeded(42n, null, leaseTokenFor(42n));

		expect((await repository.getJob(42n))?.status).toBe('succeeded');
		expect((await repository.listActiveJobs({limit: 200, pageState: null})).jobs).toEqual([]);
		expect(
			(
				await repository.listJobs({
					filters: {status: 'succeeded'},
					limit: 10,
					maxLookbackDays: 1,
					cursor: null,
				})
			).jobs.map((job) => job.job_id),
		).toEqual([42n]);
		expect(
			(
				await repository.listJobs({
					filters: {status: 'queued'},
					limit: 10,
					maxLookbackDays: 1,
					cursor: null,
				})
			).jobs,
		).toEqual([]);
	});

	it('expires complete terminal history rows after the retention window', async () => {
		await repository.createJob({
			auditLogReason: null,
			jetStreamLane: 'maintenance',
			jetStreamSeq: null,
			jobId: 43n,
			maxAttempts: 5,
			payload: {},
			requestedByUserId: null,
			runAt: null,
			taskType: 'flushUserActivityBuffer',
		});
		executor.batchQueries.length = 0;
		executor.queries.length = 0;

		expect(await claimRunning(43n, 'worker-batch')).toBe(true);
		await repository.markSucceeded(43n, null, leaseTokenFor(43n));

		const historyWrites = executor.queries.filter(
			(query) =>
				(query.kvMeta?.table.name === 'jobs_by_id' || query.kvMeta?.table.name === 'jobs_by_day_bucket') &&
				query.kvMeta.ttlParamName,
		);
		expect(historyWrites).toHaveLength(2);
		for (const write of historyWrites) {
			expect(write.kvMeta?.ttlParamName).toBeTruthy();
			const ttlParamName = write.kvMeta?.ttlParamName as string;
			expect((write.params as CassandraParams)[ttlParamName]).toBe(JOB_HISTORY_RETENTION_SECONDS);
		}
		expect(historyWrites.find((write) => write.kvMeta?.table.name === 'jobs_by_id')?.kvMeta?.condition).toEqual(
			expect.objectContaining({col: 'lease_token', expectedValue: leaseTokenFor(43n)}),
		);
	});

	it('fails stale due jobs while preserving fresh and future-scheduled jobs', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(44n, null);
		await createQueuedJob(45n, new Date('2026-08-12T00:00:00.000Z'));
		await createQueuedJob(47n, null);
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		await claimRunning(47n, 'worker-batch');
		await createQueuedJob(46n, null);

		const result = await repository.reconcileActiveJobs({
			limit: 100,
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
		});

		expect(result).toMatchObject({
			failedStaleQueued: 1,
			removedMissing: 0,
			removedTerminal: 0,
			skippedFresh: 1,
			skippedScheduled: 1,
			skippedRunning: 1,
			skippedStateChanged: 0,
		});
		expect((await repository.getJob(44n))?.status).toBe('failed');
		expect((await repository.getJob(47n))?.status).toBe('running');
		expect((await collectAllActiveJobs()).map((job) => job.job_id).sort()).toEqual([45n, 46n, 47n]);
	});

	it('does not fail a queued job that becomes running during reconciliation', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(50n, null);
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			await claimRunning(50n, 'worker-batch');
		};

		const result = await repository.reconcileActiveJobs({
			limit: 100,
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
		});

		expect(result.failedStaleQueued).toBe(0);
		expect((await repository.getJob(50n))?.status).toBe('running');
	});

	it('atomically releases running work to a claimable queued retry', async () => {
		await createQueuedJob(54n, null);
		expect(await claimRunning(54n, 'worker-batch')).toBe(true);

		expect(await repository.releaseForRetry(54n, 'temporary failure', true, leaseTokenFor(54n))).toBe(true);
		expect(await repository.getJob(54n)).toMatchObject({
			status: 'queued',
			attempts: 1,
			error_message: 'temporary failure',
			started_at: null,
		});
		expect(await claimRunning(54n, 'worker-batch')).toBe(true);
	});

	it('does not overwrite a newer running active index while releasing a retry', async () => {
		await createQueuedJob(55n, null);
		expect(await claimRunning(55n, 'worker-batch')).toBe(true);
		const releasingLease = leaseTokenFor(55n);
		executor.afterConditional = async () => {
			expect(await claimRunning(55n, 'worker-retry')).toBe(true);
		};

		expect(await repository.releaseForRetry(55n, 'temporary failure', true, releasingLease)).toBe(true);

		expect((await repository.getJob(55n))?.status).toBe('running');
		const active = await fetchOne<JobActiveV2Row>(
			JobsActive.select({where: [JobsActive.where.eq('shard'), JobsActive.where.eq('job_id')], limit: 1}).bind({
				shard: 55,
				job_id: 55n,
			}),
		);
		expect(active).toMatchObject({status: 'running'});
	});

	it('does not resurrect an active row when a newer claimant terminalizes during retry repair', async () => {
		await createQueuedJob(56n, null);
		expect(await claimRunning(56n, 'worker-batch')).toBe(true);
		let activeUpserts = 0;
		const releasingLease = leaseTokenFor(56n);
		executor.afterConditional = async () => {
			expect(await claimRunning(56n, 'worker-retry')).toBe(true);
		};
		executor.onActiveUpsert = async () => {
			activeUpserts += 1;
			if (activeUpserts === 3) {
				executor.onActiveUpsert = null;
				expect(await repository.markSucceeded(56n, null, leaseTokenFor(56n))).toBe(true);
			}
		};

		expect(await repository.releaseForRetry(56n, 'temporary failure', true, releasingLease)).toBe(true);

		expect((await repository.getJob(56n))?.status).toBe('succeeded');
		const active = await fetchOne<JobActiveV2Row>(
			JobsActive.select({where: [JobsActive.where.eq('shard'), JobsActive.where.eq('job_id')], limit: 1}).bind({
				shard: 56,
				job_id: 56n,
			}),
		);
		expect(active).toBeNull();
	});

	it('persists a dead-letter-pending state before publication and terminalizes it conditionally', async () => {
		await createQueuedJob(57n, null);
		expect(await claimRunning(57n, 'worker-batch')).toBe(true);

		const firstPublication = await repository.markDeadletterPending(57n, 'permanent failure', leaseTokenFor(57n));
		expect(firstPublication).not.toBeNull();
		expect(await repository.getJob(57n)).toMatchObject({
			status: 'deadletter_pending',
			error_message: 'permanent failure',
			dlq_attempts: 0,
		});
		expect(await repository.recordDlqPublishFailure(57n, firstPublication!.leaseToken)).toBe(1);
		const secondPublication = await repository.markDeadletterPending(57n, 'ignored after durable transition');
		expect(secondPublication).not.toBeNull();
		expect(await repository.recordDlqPublishFailure(57n, secondPublication!.leaseToken)).toBe(2);
		const finalPublication = await repository.markDeadletterPending(57n, 'ignored after durable transition');
		expect(finalPublication).not.toBeNull();
		expect(await repository.markDeadletter(57n, 'permanent failure', finalPublication!.leaseToken)).toBe(true);
		expect(await repository.getJob(57n)).toMatchObject({status: 'deadletter'});
	});

	it('allows only one dead-letter publisher per renewable lease generation', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(85n, null);
		expect(await repository.claimJob(85n, 'worker-batch', 'task-owner', new Date(), 60_000)).not.toBeNull();
		const first = await repository.markDeadletterPending(85n, 'permanent', 'task-owner', new Date(), 60_000);
		expect(first).not.toBeNull();
		expect(await repository.markDeadletterPending(85n, 'permanent', null, new Date(), 60_000)).toBeNull();
		expect(
			await repository.renewDeadletterPublicationLease(
				85n,
				first!.leaseToken,
				new Date('2026-08-01T00:00:30.000Z'),
				60_000,
			),
		).toBe(true);
		expect(
			await repository.markDeadletterPending(85n, 'permanent', null, new Date('2026-08-01T00:01:01.000Z'), 60_000),
		).toBeNull();
		const second = await repository.markDeadletterPending(
			85n,
			'permanent',
			null,
			new Date('2026-08-01T00:01:31.000Z'),
			60_000,
		);
		expect(second).not.toBeNull();
		expect(await repository.markDeadletter(85n, 'permanent', first!.leaseToken)).toBe(false);
		expect(await repository.markDeadletter(85n, 'permanent', second!.leaseToken)).toBe(true);
	});

	it('does not steal an expired lease that is concurrently renewed by its owner', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		await createQueuedJob(61n, null);
		expect(
			await repository.claimJob(61n, 'worker-batch', 'lease-a', new Date('2026-08-10T00:00:00.000Z'), 60_000),
		).not.toBeNull();
		executor.beforeConditional = async () => {
			expect(await repository.renewLease(61n, 'lease-a', new Date('2026-08-10T00:01:00.000Z'), 60_000)).toBe(true);
		};

		expect(
			await repository.claimJob(61n, 'worker-batch', 'lease-b', new Date('2026-08-10T00:01:01.000Z'), 60_000),
		).toBeNull();
		expect(await repository.getJob(61n)).toMatchObject({
			lease_token: 'lease-a',
			lease_expires_at: new Date('2026-08-10T00:02:00.000Z'),
		});
	});

	it('never shortens a worker lease when overlapping renewals complete out of order', async () => {
		await createQueuedJob(62n, null);
		expect(
			await repository.claimJob(62n, 'worker-batch', 'lease-a', new Date('2026-08-10T00:00:00.000Z'), 60_000),
		).not.toBeNull();
		executor.beforeConditional = async () => {
			expect(await repository.renewLease(62n, 'lease-a', new Date('2026-08-10T00:02:00.000Z'), 60_000)).toBe(true);
		};

		expect(await repository.renewLease(62n, 'lease-a', new Date('2026-08-10T00:01:00.000Z'), 60_000)).toBe(true);
		expect((await repository.getJob(62n))?.lease_expires_at).toEqual(new Date('2026-08-10T00:03:00.000Z'));
	});

	it('never shortens a dead-letter publication lease when renewals complete out of order', async () => {
		await createQueuedJob(86n, null);
		expect(
			await repository.claimJob(86n, 'worker-batch', 'task-owner', new Date('2026-08-10T00:00:00.000Z'), 60_000),
		).not.toBeNull();
		const publication = await repository.markDeadletterPending(
			86n,
			'permanent',
			'task-owner',
			new Date('2026-08-10T00:00:00.000Z'),
			60_000,
		);
		expect(publication).not.toBeNull();
		executor.beforeConditional = async () => {
			expect(
				await repository.renewDeadletterPublicationLease(
					86n,
					publication!.leaseToken,
					new Date('2026-08-10T00:02:00.000Z'),
					60_000,
				),
			).toBe(true);
		};

		expect(
			await repository.renewDeadletterPublicationLease(
				86n,
				publication!.leaseToken,
				new Date('2026-08-10T00:01:00.000Z'),
				60_000,
			),
		).toBe(true);
		expect((await repository.getJob(86n))?.lease_expires_at).toEqual(new Date('2026-08-10T00:03:00.000Z'));
	});

	it('reclaims a stale pre-upgrade running row without lease fields', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(63n, null);
		expect(await claimRunning(63n, 'worker-batch')).toBe(true);
		await upsertOne(
			JobsById.patchByPk(
				{job_id: 63n},
				{lease_token: Db.clear(), lease_expires_at: Db.clear(), state_changed_at: Db.clear()},
			),
		);

		const reclaimed = await repository.claimJob(
			63n,
			'worker-batch',
			'upgraded-lease',
			new Date('2026-08-10T00:00:00.000Z'),
			60_000,
		);

		expect(reclaimed).toMatchObject({status: 'running', lease_token: 'upgraded-lease'});
	});

	it('terminalizes a stale pre-upgrade running row when no broker delivery remains', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(64n, null);
		expect(await claimRunning(64n, 'worker-batch')).toBe(true);
		await upsertOne(
			JobsById.patchByPk(
				{job_id: 64n},
				{lease_token: Db.clear(), lease_expires_at: Db.clear(), state_changed_at: Db.clear()},
			),
		);

		const result = await repository.reconcileActiveJobs({
			now: new Date('2026-08-10T00:00:00.000Z'),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredRunning).toBe(1);
		expect(await repository.getJob(64n)).toMatchObject({status: 'failed'});
	});

	it('does not fail a running job that renews while reconciliation is terminalizing it', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(73n, null);
		expect(
			await repository.claimJob(73n, 'worker-batch', 'lease-a', new Date(), seconds('1 minute') * 1000),
		).not.toBeNull();
		vi.setSystemTime(new Date('2026-08-02T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(await repository.renewLease(73n, 'lease-a', new Date(), seconds('1 day') * 1000)).toBe(true);
		};

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-01T12:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredRunning).toBe(0);
		expect((await repository.getJob(73n))?.status).toBe('running');
	});

	it('does not fail a legacy row that is concurrently claimed during reconciliation', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(74n, null);
		expect(
			await repository.claimJob(74n, 'worker-batch', 'legacy-owner', new Date(), seconds('1 minute') * 1000),
		).not.toBeNull();
		await upsertOne(
			JobsById.patchByPk(
				{job_id: 74n},
				{lease_token: Db.clear(), lease_expires_at: Db.clear(), state_changed_at: Db.clear()},
			),
		);
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(
				await repository.claimJob(74n, 'worker-recovery', 'lease-b', new Date(), seconds('1 day') * 1000),
			).not.toBeNull();
		};

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredRunning).toBe(0);
		expect((await repository.getJob(74n))?.lease_token).toBe('lease-b');
	});

	it('reclaims an expired running lease while rejecting a fresh lease and stale owner completion', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		await createQueuedJob(58n, null);

		expect(
			await repository.claimJob(58n, 'worker-batch', 'lease-a', new Date('2026-08-10T00:00:00.000Z'), 60_000),
		).toMatchObject({status: 'running', lease_token: 'lease-a'});
		expect(
			await repository.claimJob(58n, 'worker-batch', 'lease-b', new Date('2026-08-10T00:00:30.000Z'), 60_000),
		).toBeNull();
		expect(
			await repository.claimJob(58n, 'worker-batch', 'lease-b', new Date('2026-08-10T00:01:01.000Z'), 60_000),
		).toMatchObject({status: 'running', lease_token: 'lease-b'});

		expect(await repository.markSucceeded(58n, null, 'lease-a')).toBe(false);
		await repository.reportProgress(58n, 1, 2, 'stale', 'lease-a');
		expect(await repository.getJob(58n)).toMatchObject({progress_current: null, progress_message: null});
		await repository.reportProgress(58n, 1, 2, 'current', 'lease-b');
		expect(await repository.getJob(58n)).toMatchObject({progress_current: 1n, progress_message: 'current'});
		expect(await repository.markSucceeded(58n, null, 'lease-b')).toBe(true);
		expect(await repository.getJob(58n)).toMatchObject({
			status: 'succeeded',
			lease_token: null,
			lease_expires_at: null,
		});
	});

	it('fails an abandoned running lease only after both its lease and queue recovery window expire', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(59n, null);
		await repository.claimJob(59n, 'worker-batch', 'abandoned', new Date(), 60_000);

		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredRunning).toBe(1);
		expect(await repository.getJob(59n)).toMatchObject({status: 'failed'});
	});

	it('preserves a newly pending scheduled job for a full recovery window after it runs', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		await createQueuedJob(62n, new Date('2026-02-01T00:00:00.000Z'));
		const claimNow = new Date('2026-02-01T00:00:00.000Z');
		vi.setSystemTime(claimNow);
		const leaseToken = 'scheduled-lease';
		expect(
			await repository.claimJob(62n, 'worker-batch', leaseToken, claimNow, seconds('1 day') * 1000),
		).not.toBeNull();
		expect(await repository.markDeadletterPending(62n, 'publish later', leaseToken)).not.toBeNull();

		const result = await repository.reconcileActiveJobs({
			now: new Date('2026-02-01T00:01:00.000Z'),
			staleBefore: new Date('2026-01-24T00:01:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredDeadletter).toBe(0);
		expect(await repository.getJob(62n)).toMatchObject({status: 'deadletter_pending'});
	});

	it('fails a scheduled queued job only after the recovery window measured from its due time', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		await createQueuedJob(75n, new Date('2026-02-01T00:00:00.000Z'));
		vi.setSystemTime(new Date('2026-02-10T00:00:00.000Z'));

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-02-02T00:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedStaleQueued).toBe(1);
		expect((await repository.getJob(75n))?.status).toBe('failed');
	});

	it('does not terminalize a queued retry released after stale reconciliation observation', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(83n, null);
		vi.setSystemTime(new Date('2026-08-20T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(await repository.claimJob(83n, 'worker-batch', 'retry-owner', new Date(), 60_000)).not.toBeNull();
			expect(await repository.releaseForRetry(83n, 'retry', true, 'retry-owner')).toBe(true);
		};

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-12T00:00:00.000Z'),
			limit: 100,
		});

		expect(result.failedStaleQueued).toBe(0);
		expect(result.skippedStateChanged).toBe(1);
		expect(await repository.getJob(83n)).toMatchObject({status: 'queued', attempts: 1});
	});

	it('ages a scheduled retry from its latest queued transition after the original due time', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(77n, new Date('2026-08-02T00:00:00.000Z'));
		vi.setSystemTime(new Date('2026-08-20T00:00:00.000Z'));
		const claimed = await repository.claimJob(77n, 'worker-batch', 'retry-lease', new Date(), 60_000);
		expect(claimed).not.toBeNull();
		expect(await repository.releaseForRetry(77n, 'temporary failure', true, 'retry-lease')).toBe(true);

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-12T00:00:00.000Z'),
			limit: 100,
		});

		expect(result.failedStaleQueued).toBe(0);
		expect((await repository.getJob(77n))?.status).toBe('queued');
	});

	it('does not fail dead-letter work claimed for publication after stale reconciliation observation', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(84n, null);
		expect(await repository.claimJob(84n, 'worker-batch', 'task-owner', new Date(), 60_000)).not.toBeNull();
		expect(await repository.markDeadletterPending(84n, 'publish me', 'task-owner', new Date(), 1000)).not.toBeNull();
		vi.setSystemTime(new Date('2026-08-20T00:00:00.000Z'));
		executor.beforeConditional = async () => {
			executor.beforeConditional = null;
			expect(await repository.markDeadletterPending(84n, 'durable error', null, new Date(), 60_000)).not.toBeNull();
		};

		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-12T00:00:00.000Z'),
			limit: 100,
		});

		expect(result.failedExpiredDeadletter).toBe(0);
		expect(result.skippedStateChanged).toBe(1);
		expect(await repository.getJob(84n)).toMatchObject({status: 'deadletter_pending'});
	});

	it('fails dead-letter-pending work only after the source stream recovery window expires', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(60n, null);
		await repository.claimJob(60n, 'worker-batch', 'owner', new Date(), 60_000);
		await repository.markDeadletterPending(60n, 'permanent', 'owner');

		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));
		const result = await repository.reconcileActiveJobs({
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
			limit: 500,
		});

		expect(result.failedExpiredDeadletter).toBe(1);
		expect(await repository.getJob(60n)).toMatchObject({status: 'failed'});
	});

	it('preserves an old scheduled job when its run time becomes due', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		await createQueuedJob(51n, new Date('2026-08-10T00:00:00.000Z'));
		vi.setSystemTime(new Date('2026-08-10T00:00:00.000Z'));

		const result = await repository.reconcileActiveJobs({
			limit: 100,
			now: new Date(),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
		});

		expect(result.failedStaleQueued).toBe(0);
		expect((await repository.getJob(51n))?.status).toBe('queued');
	});

	it('does not regress or resurrect terminal jobs through late worker mutations', async () => {
		await createQueuedJob(52n, null);
		await claimRunning(52n, 'worker-batch');
		await repository.markSucceeded(52n, {ok: true}, leaseTokenFor(52n));

		await claimRunning(52n, 'late-worker');
		await repository.reportProgress(52n, 1, 2, 'late progress', leaseTokenFor(52n));
		await repository.setContextLink(52n, '/late', leaseTokenFor(52n));
		await repository.requestCancel(52n);
		expect(await repository.releaseForRetry(52n, 'late retry', true, leaseTokenFor(52n))).toBe(false);

		const job = await repository.getJob(52n);
		expect(job).toMatchObject({
			status: 'succeeded',
			attempts: 0,
			cancel_requested: false,
			context_link: null,
			progress_current: null,
			jet_stream_lane: 'worker-batch',
		});
		expect((await repository.listActiveJobs({limit: 200, pageState: null})).jobs).toEqual([]);
	});

	it('removes orphaned and terminal active-index rows', async () => {
		const createdAt = new Date('2026-08-01T00:00:00.000Z');
		vi.useFakeTimers();
		vi.setSystemTime(createdAt);
		await createQueuedJob(48n, null);
		expect(await claimRunning(48n, 'worker-batch')).toBe(true);
		await repository.markSucceeded(48n, null, leaseTokenFor(48n));
		await upsertOne(
			JobsActive.upsertAll({
				shard: 48,
				job_id: 48n,
				created_at: createdAt,
				task_type: 'flushUserActivityBuffer',
				status: 'succeeded',
				requested_by_user_id: null,
				started_at: null,
			}),
		);
		await createQueuedJob(99n, null);
		await executor.executeQuery(JobsById.deleteByPk({job_id: 99n}));
		expect((await repository.listActiveJobs({limit: 200, pageState: null})).jobs).toEqual([]);

		const result = await repository.reconcileActiveJobs({
			limit: 100,
			now: new Date('2026-08-10T00:00:00.000Z'),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
		});

		expect(result).toMatchObject({removedMissing: 1, removedTerminal: 1});
		expect((await repository.listActiveJobs({limit: 200, pageState: null})).jobs).toEqual([]);
		expect(await fetchBucket(99n, createdAt)).toBeNull();
	});

	it('rotates bounded reconciliation batches past preserved rows', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('1969-01-01T00:00:00.000Z'));
		for (let index = 0; index < 500; index += 1) {
			await createQueuedJob(BigInt(64_000 + index * 64), new Date('1971-01-01T00:00:00.000Z'));
		}
		await createQueuedJob(1n, null);

		const first = await repository.reconcileActiveJobs({
			limit: 500,
			now: new Date('1970-01-01T00:00:00.000Z'),
			staleBefore: new Date('1969-12-31T00:00:00.000Z'),
		});
		expect(first.failedStaleQueued).toBe(0);
		expect((await repository.getJob(1n))?.status).toBe('queued');

		const second = await repository.reconcileActiveJobs({
			limit: 500,
			now: new Date('1970-01-01T01:00:00.000Z'),
			staleBefore: new Date('1969-12-31T00:00:00.000Z'),
		});

		expect(second.failedStaleQueued).toBe(1);
		expect((await repository.getJob(1n))?.status).toBe('failed');
	});

	it('rotates fairly across shuffled active-row insertion order', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-01T00:00:00.000Z'));
		for (const jobId of [66n, 64n, 65n]) await createQueuedJob(jobId, null);

		for (let pass = 0; pass < 6; pass += 1) {
			await repository.reconcileActiveJobs({
				limit: 1,
				now: new Date('2026-08-20T00:00:00.000Z'),
				staleBefore: new Date('2026-08-12T00:00:00.000Z'),
			});
		}

		for (const jobId of [64n, 65n, 66n]) expect((await repository.getJob(jobId))?.status).toBe('failed');
	});

	it('repairs a terminal day bucket using only the authoritative remaining retention', async () => {
		vi.useFakeTimers();
		const completedAt = new Date('2026-08-01T00:00:00.000Z');
		vi.setSystemTime(completedAt);
		await createQueuedJob(53n, null);
		expect(await claimRunning(53n, 'worker-batch')).toBe(true);
		await repository.markSucceeded(53n, null, leaseTokenFor(53n));
		await upsertOne(
			JobsActive.upsertAll({
				shard: 53,
				job_id: 53n,
				created_at: completedAt,
				task_type: 'flushUserActivityBuffer',
				status: 'succeeded',
				requested_by_user_id: null,
				started_at: completedAt,
			}),
		);
		executor.queries.length = 0;

		await repository.reconcileActiveJobs({
			limit: 100,
			now: new Date('2026-08-04T00:00:00.000Z'),
			staleBefore: new Date('2026-08-02T00:00:00.000Z'),
		});

		const bucketRepair = executor.queries.find(
			(query) => query.kvMeta?.table.name === 'jobs_by_day_bucket' && query.kvMeta.ttlParamName,
		);
		expect(bucketRepair).toBeDefined();
		const ttlParamName = bucketRepair?.kvMeta?.ttlParamName as string;
		expect((bucketRepair?.params as CassandraParams)[ttlParamName]).toBe(seconds('4 days'));
	});
});

async function createQueuedJob(jobId: bigint, runAt: Date | null): Promise<void> {
	await repository.createJob({
		auditLogReason: null,
		jetStreamLane: 'maintenance',
		jetStreamSeq: null,
		jobId,
		maxAttempts: 5,
		payload: {},
		requestedByUserId: null,
		runAt,
		taskType: 'flushUserActivityBuffer',
	});
}

async function collectAllActiveJobs(limit = 200): Promise<Array<JobByIdRow>> {
	const jobs: Array<JobByIdRow> = [];
	let pageState: string | null = null;
	for (let page = 0; page < 65; page += 1) {
		const result = await repository.listActiveJobs({limit, pageState});
		expect(result.jobs.length).toBeLessThanOrEqual(limit);
		jobs.push(...result.jobs);
		pageState = result.nextPageState;
		if (pageState === null) return jobs;
	}
	throw new Error('active-job pagination did not terminate');
}
