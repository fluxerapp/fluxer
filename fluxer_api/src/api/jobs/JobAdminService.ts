// SPDX-License-Identifier: AGPL-3.0-or-later

import type {JobByIdRow} from '@app/api/database/types/JobLedgerTypes';
import type {IJobLedgerRepository} from '@app/api/jobs/IJobLedgerRepository';
import type {JobLedgerEntry, ListJobsRequest} from '@fluxer/schema/src/domains/admin/JobsSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

export class JobAdminService {
	constructor(
		private readonly ledger: IJobLedgerRepository,
		private readonly workerService: IWorkerService,
	) {}

	async listJobs(req: ListJobsRequest): Promise<{
		jobs: Array<JobLedgerEntry>;
		next_cursor: {
			bucket_day: string;
			created_at: string;
			job_id: string;
		} | null;
	}> {
		const result = await this.ledger.listJobs({
			limit: req.limit,
			cursor: req.cursor
				? {
						bucketDay: req.cursor.bucket_day,
						createdAt: new Date(req.cursor.created_at),
						jobId: BigInt(req.cursor.job_id),
					}
				: null,
			maxLookbackDays: req.max_lookback_days,
			filters: {
				status: req.status ?? null,
				taskType: req.task_type ?? null,
				requestedByUserId: req.requested_by_user_id ?? null,
			},
		});
		return {
			jobs: result.jobs.map(rowToEntry),
			next_cursor: result.nextCursor
				? {
						bucket_day: result.nextCursor.bucketDay,
						created_at: result.nextCursor.createdAt.toISOString(),
						job_id: result.nextCursor.jobId.toString(),
					}
				: null,
		};
	}

	async getJob(jobId: bigint): Promise<{
		job: JobLedgerEntry;
	} | null> {
		const row = await this.ledger.getJob(jobId);
		if (!row) return null;
		return {job: rowToEntry(row)};
	}

	async cancelJob(jobId: bigint): Promise<{
		cancelled: boolean;
	}> {
		const cancelled = await this.workerService.cancelJob(jobId);
		return {cancelled};
	}

	async listActiveJobs(): Promise<{
		jobs: Array<JobLedgerEntry>;
	}> {
		const rows = await this.ledger.listActiveJobs();
		return {jobs: rows.map(rowToEntry)};
	}
}

function rowToEntry(row: JobByIdRow): JobLedgerEntry {
	return {
		job_id: row.job_id.toString(),
		task_type: row.task_type,
		status: row.status,
		progress_current: row.progress_current === null ? null : Number(row.progress_current),
		progress_total: row.progress_total === null ? null : Number(row.progress_total),
		progress_message: row.progress_message,
		error_message: row.error_message,
		created_at: row.created_at.toISOString(),
		started_at: row.started_at ? row.started_at.toISOString() : null,
		completed_at: row.completed_at ? row.completed_at.toISOString() : null,
		requested_by_user_id: row.requested_by_user_id === null ? null : row.requested_by_user_id.toString(),
		audit_log_reason: row.audit_log_reason,
		jet_stream_lane: row.jet_stream_lane,
		jet_stream_seq: row.jet_stream_seq,
		attempts: row.attempts,
		max_attempts: row.max_attempts,
		run_at: row.run_at ? row.run_at.toISOString() : null,
		cancel_requested: row.cancel_requested,
		context_link: row.context_link,
		payload: row.payload,
		result: row.result,
	};
}
