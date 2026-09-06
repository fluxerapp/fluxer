// SPDX-License-Identifier: AGPL-3.0-or-later

import type {JobByIdRow, JobStatus} from '../database/types/JobLedgerTypes';

export interface CreateJobInput {
	jobId: bigint;
	taskType: string;
	payload: Record<string, unknown>;
	requestedByUserId: bigint | null;
	auditLogReason: string | null;
	maxAttempts: number;
	runAt: Date | null;
	jetStreamLane: string | null;
	jetStreamSeq: string | null;
}

export interface ListJobsCursor {
	bucketDay: string;
	createdAt: Date;
	jobId: bigint;
}

export interface ListJobsFilters {
	status?: JobStatus | null;
	taskType?: string | null;
	requestedByUserId?: bigint | null;
}

export interface ListJobsResult {
	jobs: Array<JobByIdRow>;
	nextCursor: ListJobsCursor | null;
}

export abstract class IJobLedgerRepository {
	abstract createJob(input: CreateJobInput): Promise<void>;

	abstract getJob(jobId: bigint): Promise<JobByIdRow | null>;

	abstract markRunning(jobId: bigint, lane: string): Promise<void>;

	abstract markSucceeded(jobId: bigint, result: Record<string, unknown> | null): Promise<void>;

	abstract markCancelled(jobId: bigint): Promise<void>;

	abstract markDeadletter(jobId: bigint, errorMessage: string): Promise<void>;

	abstract reportProgress(jobId: bigint, current: number, total: number | null, message: string | null): Promise<void>;

	abstract setContextLink(jobId: bigint, link: string): Promise<void>;

	abstract setJetStreamSeq(jobId: bigint, seq: string): Promise<void>;

	abstract requestCancel(jobId: bigint): Promise<void>;

	abstract isCancelRequested(jobId: bigint): Promise<boolean>;

	abstract incrementAttempts(jobId: bigint): Promise<void>;

	abstract listJobs(opts: {
		limit: number;
		cursor: ListJobsCursor | null;
		filters: ListJobsFilters;
		maxLookbackDays: number;
	}): Promise<ListJobsResult>;

	abstract listActiveJobs(): Promise<Array<JobByIdRow>>;

	abstract listActiveJobsByTaskType(taskType: string): Promise<Array<JobByIdRow>>;
}
