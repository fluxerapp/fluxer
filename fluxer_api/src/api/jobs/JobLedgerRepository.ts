// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BatchBuilder,
	deleteOneOrMany,
	fetchMany,
	fetchOne,
	fetchPage,
	type PagedQueryResult,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {JobActiveRow, JobByDayBucketRow, JobByIdRow, JobStatus} from '@app/api/database/types/JobLedgerTypes';
import {
	type CreateJobInput,
	IJobLedgerRepository,
	type ListJobsCursor,
	type ListJobsFilters,
	type ListJobsResult,
} from '@app/api/jobs/IJobLedgerRepository';
import {JobsActive, JobsByDayBucket, JobsById} from '@app/api/Tables';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {JOBS_STREAM_MAX_AGE_MS} from '@app/api/worker/JetStreamWorkerQueue';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {ms, seconds} from 'itty-time';

export const JOB_LEDGER_TTL_SECONDS = seconds('90 days');
export const JOB_STALE_AFTER_MS = JOBS_STREAM_MAX_AGE_MS + ms('1 day');
export const EXPIRED_JOB_ERROR = 'Expired from the job queue';

const JOB_LEDGER_RETENTION_DAYS = JOB_LEDGER_TTL_SECONDS / seconds('1 day');
const NEWEST_FIRST = {col: 'created_at', direction: 'DESC'} as const;

const FETCH_JOB_BY_ID_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const FETCH_CANCEL_REQUESTED_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const ACTIVE_JOBS_QUERY = JobsActive.select();
const ACTIVE_JOB_IDS_QUERY = JobsActive.select({columns: ['job_id']});
const JOBS_AFTER_IN_TIE_QUERY = JobsByDayBucket.select({
	where: [
		JobsByDayBucket.where.eq('bucket_day'),
		JobsByDayBucket.where.eq('created_at'),
		JobsByDayBucket.where.lt('job_id'),
	],
	orderBy: NEWEST_FIRST,
});

type LedgerPosition = Pick<ListJobsCursor, 'createdAt' | 'jobId'>;

function bucketDayFor(d: Date): string {
	return d.toISOString().slice(0, 10);
}

function previousBucketDay(day: string): string {
	const date = new Date(`${day}T00:00:00Z`);
	date.setUTCDate(date.getUTCDate() - 1);
	return bucketDayFor(date);
}

function dayJobsQuery(olderThanPosition: boolean, limit: number | null) {
	return JobsByDayBucket.select({
		where: olderThanPosition
			? [JobsByDayBucket.where.eq('bucket_day'), JobsByDayBucket.where.lt('created_at')]
			: JobsByDayBucket.where.eq('bucket_day'),
		orderBy: NEWEST_FIRST,
		...(limit === null ? {} : {limit}),
	});
}

async function fetchDayAfter(
	bucketDay: string,
	after: LedgerPosition | null,
	limit: number | null,
): Promise<{rows: Array<JobByDayBucketRow>; exhausted: boolean}> {
	if (after === null) {
		const rows = await fetchMany<JobByDayBucketRow>(dayJobsQuery(false, limit).bind({bucket_day: bucketDay}));
		return {rows, exhausted: limit === null || rows.length < limit};
	}
	const ties = await fetchMany<JobByDayBucketRow>(
		JOBS_AFTER_IN_TIE_QUERY.bind({bucket_day: bucketDay, created_at: after.createdAt, job_id: after.jobId}),
	);
	const older = await fetchMany<JobByDayBucketRow>(
		dayJobsQuery(true, limit).bind({bucket_day: bucketDay, created_at: after.createdAt}),
	);
	return {rows: [...ties, ...older], exhausted: limit === null || older.length < limit};
}

export class JobLedgerRepository extends IJobLedgerRepository {
	async createJob(input: CreateJobInput): Promise<Date> {
		const now = new Date();
		const status: JobStatus = 'queued';
		const idRow: JobByIdRow = {
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			progress_current: null,
			progress_total: null,
			progress_message: null,
			payload: JSON.stringify(input.payload),
			result: null,
			error_message: null,
			created_at: now,
			started_at: null,
			completed_at: null,
			requested_by_user_id: input.requestedByUserId,
			audit_log_reason: input.auditLogReason,
			jet_stream_seq: input.jetStreamSeq,
			jet_stream_lane: input.jetStreamLane,
			attempts: 0,
			max_attempts: input.maxAttempts,
			run_at: input.runAt,
			cancel_requested: false,
			context_link: null,
		};
		const bucketRow: JobByDayBucketRow = {
			bucket_day: bucketDayFor(now),
			created_at: now,
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			requested_by_user_id: input.requestedByUserId,
		};
		const activeRow: JobActiveRow = {
			job_id: input.jobId,
			task_type: input.taskType,
			status,
			requested_by_user_id: input.requestedByUserId,
			created_at: now,
			started_at: null,
		};
		const batch = new BatchBuilder();
		batch.addPrepared(JobsById.insertWithTtl(idRow, JOB_LEDGER_TTL_SECONDS));
		batch.addPrepared(JobsByDayBucket.insertWithTtl(bucketRow, JOB_LEDGER_TTL_SECONDS));
		batch.addPrepared(JobsActive.insertWithTtl(activeRow, JOB_LEDGER_TTL_SECONDS));
		await batch.executeChunked(10, false);
		return now;
	}

	async getJob(jobId: bigint): Promise<JobByIdRow | null> {
		const row = await fetchOne<JobByIdRow>(FETCH_JOB_BY_ID_QUERY.bind({job_id: jobId}));
		return row?.created_at && row.task_type && row.status ? row : null;
	}

	async discardJob(jobId: bigint, createdAt: Date): Promise<void> {
		await awaitAll(
			[
				deleteOneOrMany(
					JobsByDayBucket.deleteByPk({bucket_day: bucketDayFor(createdAt), created_at: createdAt, job_id: jobId}),
				),
				deleteOneOrMany(JobsById.deleteByPk({job_id: jobId})),
				deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId})),
			],
			'Ledger discard left rows behind',
		);
	}

	async markRunning(jobId: bigint, lane: string): Promise<void> {
		const startedAt = new Date();
		const status: JobStatus = 'running';
		await upsertOne(
			JobsById.patchByPkWithTtl(
				{job_id: jobId},
				{status: Db.set(status), started_at: Db.set(startedAt), jet_stream_lane: Db.set(lane)},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
		await upsertOne(
			JobsActive.patchByPkWithTtl(
				{job_id: jobId},
				{status: Db.set(status), started_at: Db.set(startedAt)},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
	}

	async markSucceeded(jobId: bigint, result: Record<string, unknown> | null): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'succeeded';
		await upsertOne(
			JobsById.patchByPkWithTtl(
				{job_id: jobId},
				{
					status: Db.set(status),
					completed_at: Db.set(completedAt),
					result: result === null ? Db.clear() : Db.set(JSON.stringify(result)),
				},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async markCancelled(jobId: bigint): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'cancelled';
		await upsertOne(
			JobsById.patchByPkWithTtl(
				{job_id: jobId},
				{status: Db.set(status), completed_at: Db.set(completedAt)},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async markDeadletter(jobId: bigint, errorMessage: string): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'deadletter';
		await upsertOne(
			JobsById.patchByPkWithTtl(
				{job_id: jobId},
				{status: Db.set(status), completed_at: Db.set(completedAt), error_message: Db.set(errorMessage)},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async reportProgress(jobId: bigint, current: number, total: number | null, message: string | null): Promise<void> {
		await upsertOne(
			JobsById.patchByPkWithTtl(
				{job_id: jobId},
				{
					progress_current: Db.set(BigInt(current)),
					progress_total: total === null ? Db.clear() : Db.set(BigInt(total)),
					progress_message: message === null ? Db.clear() : Db.set(message),
				},
				JOB_LEDGER_TTL_SECONDS,
			),
		);
	}

	async setContextLink(jobId: bigint, link: string): Promise<void> {
		await upsertOne(JobsById.patchByPkWithTtl({job_id: jobId}, {context_link: Db.set(link)}, JOB_LEDGER_TTL_SECONDS));
	}

	async setJetStreamSeq(jobId: bigint, seq: string): Promise<void> {
		await upsertOne(JobsById.patchByPkWithTtl({job_id: jobId}, {jet_stream_seq: Db.set(seq)}, JOB_LEDGER_TTL_SECONDS));
	}

	async requestCancel(jobId: bigint): Promise<void> {
		await upsertOne(
			JobsById.patchByPkWithTtl({job_id: jobId}, {cancel_requested: Db.set(true)}, JOB_LEDGER_TTL_SECONDS),
		);
	}

	async isCancelRequested(jobId: bigint): Promise<boolean> {
		const row = await fetchOne<{
			cancel_requested: boolean | null;
		}>(FETCH_CANCEL_REQUESTED_QUERY.bind({job_id: jobId}));
		return row?.cancel_requested === true;
	}

	async incrementAttempts(jobId: bigint): Promise<void> {
		const row = await this.getJob(jobId);
		if (!row) return;
		await upsertOne(
			JobsById.patchByPkWithTtl({job_id: jobId}, {attempts: Db.set(row.attempts + 1)}, JOB_LEDGER_TTL_SECONDS),
		);
	}

	async listJobs(opts: {
		limit: number;
		cursor: ListJobsCursor | null;
		filters: ListJobsFilters;
		maxLookbackDays: number;
	}): Promise<ListJobsResult> {
		const {limit, cursor, filters} = opts;
		const now = Date.now();
		const lookbackDays = Math.min(opts.maxLookbackDays, JOB_LEDGER_RETENTION_DAYS);
		const oldestDay = bucketDayFor(new Date(now - lookbackDays * ms('1 day')));
		const requestedBy = filters.requestedByUserId ?? null;
		const wholeDays = Boolean(filters.status || filters.taskType || requestedBy !== null);
		const jobs: Array<JobByIdRow> = [];
		let lastRow: JobByDayBucketRow | null = null;
		let day = bucketDayFor(new Date(Math.min(cursor ? cursor.createdAt.getTime() : now, now)));
		let after: LedgerPosition | null = cursor;
		while (jobs.length < limit && day >= oldestDay) {
			const {rows, exhausted} = await fetchDayAfter(day, after, wholeDays ? null : limit - jobs.length);
			for (const row of rows) {
				after = {createdAt: row.created_at, jobId: row.job_id};
				if (filters.taskType && row.task_type !== filters.taskType) continue;
				if (requestedBy !== null && row.requested_by_user_id !== requestedBy) continue;
				const job = await this.getJob(row.job_id);
				if (!job || (filters.status && job.status !== filters.status)) continue;
				jobs.push(job);
				lastRow = row;
				if (jobs.length === limit) break;
			}
			if (exhausted) {
				day = previousBucketDay(day);
				after = null;
			}
		}
		return {
			jobs,
			nextCursor:
				lastRow && jobs.length === limit
					? {bucketDay: lastRow.bucket_day, createdAt: lastRow.created_at, jobId: lastRow.job_id}
					: null,
		};
	}

	async expireStaleActiveJobs(opts: {
		staleBeforeMs: number;
		pageSize: number;
		maxCleared: number;
	}): Promise<{cleared: number; expired: number; complete: boolean}> {
		let cleared = 0;
		let expired = 0;
		let pageState: string | null = null;
		do {
			const page: PagedQueryResult<Pick<JobActiveRow, 'job_id'>> = await fetchPage(
				ACTIVE_JOB_IDS_QUERY.bind({}),
				undefined,
				{
					pageSize: opts.pageSize,
					pageState,
				},
			);
			for (const {job_id: jobId} of page.rows) {
				if (snowflakeToDate(jobId).getTime() >= opts.staleBeforeMs) continue;
				if (cleared >= opts.maxCleared) return {cleared, expired, complete: false};
				const job = await this.getJob(jobId);
				if (job?.status === 'queued' || job?.status === 'running') {
					await this.markDeadletter(jobId, EXPIRED_JOB_ERROR);
					expired += 1;
				} else {
					await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
				}
				cleared += 1;
			}
			pageState = page.pageState;
		} while (pageState !== null);
		return {cleared, expired, complete: true};
	}

	async listActiveJobs(): Promise<Array<JobByIdRow>> {
		const activeRows = await fetchMany<JobActiveRow>(ACTIVE_JOBS_QUERY.bind({}));
		const fullRows = await Promise.all(activeRows.map((r) => this.getJob(r.job_id)));
		return fullRows.filter((r): r is JobByIdRow => r !== null);
	}

	async listActiveJobsByTaskType(taskType: string): Promise<Array<JobByIdRow>> {
		const activeRows = await fetchMany<JobActiveRow>(ACTIVE_JOBS_QUERY.bind({}));
		const matching = activeRows.filter((r) => r.task_type === taskType);
		const fullRows = await Promise.all(matching.map((r) => this.getJob(r.job_id)));
		return fullRows.filter((r): r is JobByIdRow => r !== null);
	}
}
