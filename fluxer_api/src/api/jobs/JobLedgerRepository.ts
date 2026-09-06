// SPDX-License-Identifier: AGPL-3.0-or-later

import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import {Db} from '../database/CassandraTypes';
import type {JobActiveRow, JobByDayBucketRow, JobByIdRow, JobStatus} from '../database/types/JobLedgerTypes';
import {JobsActive, JobsByDayBucket, JobsById} from '../Tables';
import {
	type CreateJobInput,
	IJobLedgerRepository,
	type ListJobsCursor,
	type ListJobsFilters,
	type ListJobsResult,
} from './IJobLedgerRepository';

const FETCH_JOB_BY_ID_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const FETCH_CANCEL_REQUESTED_QUERY = JobsById.select({
	where: JobsById.where.eq('job_id'),
});
const ACTIVE_JOBS_QUERY = JobsActive.select();

function bucketDayFor(d: Date): string {
	return d.toISOString().slice(0, 10);
}

export class JobLedgerRepository extends IJobLedgerRepository {
	async createJob(input: CreateJobInput): Promise<void> {
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
		batch.addPrepared(JobsById.insert(idRow));
		batch.addPrepared(JobsByDayBucket.insert(bucketRow));
		batch.addPrepared(JobsActive.insert(activeRow));
		await batch.executeChunked(10, false);
	}

	async getJob(jobId: bigint): Promise<JobByIdRow | null> {
		return fetchOne<JobByIdRow>(FETCH_JOB_BY_ID_QUERY.bind({job_id: jobId}));
	}

	async markRunning(jobId: bigint, lane: string): Promise<void> {
		const startedAt = new Date();
		const status: JobStatus = 'running';
		await upsertOne(
			JobsById.patchByPk(
				{job_id: jobId},
				{status: Db.set(status), started_at: Db.set(startedAt), jet_stream_lane: Db.set(lane)},
			),
		);
		await upsertOne(JobsActive.patchByPk({job_id: jobId}, {status: Db.set(status), started_at: Db.set(startedAt)}));
	}

	async markSucceeded(jobId: bigint, result: Record<string, unknown> | null): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'succeeded';
		await upsertOne(
			JobsById.patchByPk(
				{job_id: jobId},
				{
					status: Db.set(status),
					completed_at: Db.set(completedAt),
					result: result === null ? Db.clear() : Db.set(JSON.stringify(result)),
				},
			),
		);
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async markCancelled(jobId: bigint): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'cancelled';
		await upsertOne(JobsById.patchByPk({job_id: jobId}, {status: Db.set(status), completed_at: Db.set(completedAt)}));
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async markDeadletter(jobId: bigint, errorMessage: string): Promise<void> {
		const completedAt = new Date();
		const status: JobStatus = 'deadletter';
		await upsertOne(
			JobsById.patchByPk(
				{job_id: jobId},
				{status: Db.set(status), completed_at: Db.set(completedAt), error_message: Db.set(errorMessage)},
			),
		);
		await deleteOneOrMany(JobsActive.deleteByPk({job_id: jobId}));
	}

	async reportProgress(jobId: bigint, current: number, total: number | null, message: string | null): Promise<void> {
		await upsertOne(
			JobsById.patchByPk(
				{job_id: jobId},
				{
					progress_current: Db.set(BigInt(current)),
					progress_total: total === null ? Db.clear() : Db.set(BigInt(total)),
					progress_message: message === null ? Db.clear() : Db.set(message),
				},
			),
		);
	}

	async setContextLink(jobId: bigint, link: string): Promise<void> {
		await upsertOne(JobsById.patchByPk({job_id: jobId}, {context_link: Db.set(link)}));
	}

	async setJetStreamSeq(jobId: bigint, seq: string): Promise<void> {
		await upsertOne(JobsById.patchByPk({job_id: jobId}, {jet_stream_seq: Db.set(seq)}));
	}

	async requestCancel(jobId: bigint): Promise<void> {
		await upsertOne(JobsById.patchByPk({job_id: jobId}, {cancel_requested: Db.set(true)}));
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
		await upsertOne(JobsById.patchByPk({job_id: jobId}, {attempts: Db.set(row.attempts + 1)}));
	}

	async listJobs(opts: {
		limit: number;
		cursor: ListJobsCursor | null;
		filters: ListJobsFilters;
		maxLookbackDays: number;
	}): Promise<ListJobsResult> {
		const {limit, cursor, filters, maxLookbackDays} = opts;
		const startBucket = cursor ? new Date(`${cursor.bucketDay}T00:00:00Z`) : new Date();
		const hasFilters = Boolean(
			filters.status ||
				filters.taskType ||
				(filters.requestedByUserId !== undefined && filters.requestedByUserId !== null),
		);
		const collected: Array<JobByIdRow> = [];
		let nextCursor: ListJobsCursor | null = null;
		for (let dayOffset = 0; dayOffset <= maxLookbackDays && collected.length < limit; dayOffset++) {
			const bucketDate = new Date(startBucket);
			bucketDate.setUTCDate(bucketDate.getUTCDate() - dayOffset);
			const bucketDay = bucketDayFor(bucketDate);
			const remaining = limit - collected.length + 1;
			const bucketLimit = hasFilters ? {} : {limit: remaining};
			const useCursor = dayOffset === 0 && cursor !== null;
			let bucketRows: Array<JobByDayBucketRow>;
			if (useCursor && cursor) {
				const query = JobsByDayBucket.select({
					where: [JobsByDayBucket.where.eq('bucket_day'), JobsByDayBucket.where.lt('created_at')],
					...bucketLimit,
				});
				bucketRows = await fetchMany<JobByDayBucketRow>(
					query.bind({bucket_day: bucketDay, created_at: cursor.createdAt}),
				);
			} else {
				const query = JobsByDayBucket.select({
					where: JobsByDayBucket.where.eq('bucket_day'),
					...bucketLimit,
				});
				bucketRows = await fetchMany<JobByDayBucketRow>(query.bind({bucket_day: bucketDay}));
			}
			for (const r of bucketRows) {
				if (filters.taskType && r.task_type !== filters.taskType) continue;
				if (filters.requestedByUserId !== undefined && filters.requestedByUserId !== null) {
					if (r.requested_by_user_id !== filters.requestedByUserId) continue;
				}
				const fullRow = await this.getJob(r.job_id);
				if (!fullRow) continue;
				if (filters.status && fullRow.status !== filters.status) continue;
				if (collected.length >= limit) {
					nextCursor = {bucketDay, createdAt: r.created_at, jobId: r.job_id};
					break;
				}
				collected.push(fullRow);
			}
			if (nextCursor) break;
		}
		if (nextCursor === null && collected.length >= limit) {
			const last = collected[collected.length - 1];
			nextCursor = {bucketDay: bucketDayFor(last.created_at), createdAt: last.created_at, jobId: last.job_id};
		}
		return {jobs: collected, nextCursor};
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
