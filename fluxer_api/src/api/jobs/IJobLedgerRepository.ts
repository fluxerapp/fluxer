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

export interface ReconcileActiveJobsInput {
	now: Date;
	staleBefore: Date;
	limit: number;
}

export interface ReconcileActiveJobsResult {
	scanned: number;
	removedMissing: number;
	removedTerminal: number;
	failedStaleQueued: number;
	failedExpiredRunning: number;
	failedExpiredDeadletter: number;
	skippedFresh: number;
	skippedScheduled: number;
	skippedRunning: number;
	skippedStateChanged: number;
}

export interface ListActiveJobsInput {
	limit: number;
	pageState: string | null;
	taskType?: string | null;
}

export interface ListActiveJobsResult {
	jobs: Array<JobByIdRow>;
	nextPageState: string | null;
}

export interface DeadletterPublicationLease {
	leaseToken: string;
	errorMessage: string;
}

export abstract class IJobLedgerRepository {
	abstract createJob(input: CreateJobInput): Promise<void>;

	abstract getJob(jobId: bigint): Promise<JobByIdRow | null>;

	abstract claimJob(
		jobId: bigint,
		lane: string,
		leaseToken: string,
		now: Date,
		leaseDurationMs: number,
	): Promise<JobByIdRow | null>;

	abstract renewLease(jobId: bigint, leaseToken: string, now: Date, leaseDurationMs: number): Promise<boolean>;

	abstract releaseForRetry(
		jobId: bigint,
		errorMessage: string,
		incrementAttempt: boolean,
		leaseToken: string,
	): Promise<boolean>;

	abstract markEnqueueFailed(jobId: bigint, errorMessage: string): Promise<boolean>;

	abstract markSucceeded(jobId: bigint, result: Record<string, unknown> | null, leaseToken: string): Promise<boolean>;

	abstract markFailed(jobId: bigint, errorMessage: string, leaseToken?: string | null): Promise<boolean>;

	abstract markCancelled(jobId: bigint, leaseToken: string): Promise<boolean>;

	abstract markDeadletterPending(
		jobId: bigint,
		errorMessage: string,
		leaseToken?: string | null,
		now?: Date,
		leaseDurationMs?: number,
	): Promise<DeadletterPublicationLease | null>;

	abstract renewDeadletterPublicationLease(
		jobId: bigint,
		leaseToken: string,
		now: Date,
		leaseDurationMs: number,
	): Promise<boolean>;

	abstract recordDlqPublishFailure(jobId: bigint, leaseToken: string): Promise<number | null>;

	abstract markDeadletter(jobId: bigint, errorMessage: string, leaseToken: string): Promise<boolean>;

	abstract reportProgress(
		jobId: bigint,
		current: number,
		total: number | null,
		message: string | null,
		leaseToken: string,
	): Promise<void>;

	abstract setContextLink(jobId: bigint, link: string, leaseToken: string): Promise<void>;

	abstract requestCancel(jobId: bigint): Promise<boolean>;

	abstract isCancelRequested(jobId: bigint): Promise<boolean>;

	abstract listJobs(opts: {
		limit: number;
		cursor: ListJobsCursor | null;
		filters: ListJobsFilters;
		maxLookbackDays: number;
	}): Promise<ListJobsResult>;

	abstract reconcileActiveJobs(input: ReconcileActiveJobsInput): Promise<ReconcileActiveJobsResult>;

	abstract listActiveJobs(input: ListActiveJobsInput): Promise<ListActiveJobsResult>;

	abstract listActiveJobsByTaskType(
		taskType: string,
		input: Omit<ListActiveJobsInput, 'taskType'>,
	): Promise<ListActiveJobsResult>;
}
