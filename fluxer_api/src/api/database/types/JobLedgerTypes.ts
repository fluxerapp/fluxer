// SPDX-License-Identifier: AGPL-3.0-or-later

export type JobStatus =
	| 'queued'
	| 'running'
	| 'deadletter_pending'
	| 'succeeded'
	| 'failed'
	| 'cancelled'
	| 'deadletter';

export interface JobByIdRow {
	job_id: bigint;
	task_type: string;
	status: JobStatus;
	progress_current: bigint | null;
	progress_total: bigint | null;
	progress_message: string | null;
	payload: string | null;
	result: string | null;
	error_message: string | null;
	created_at: Date;
	state_changed_at: Date | null;
	started_at: Date | null;
	completed_at: Date | null;
	requested_by_user_id: bigint | null;
	audit_log_reason: string | null;
	jet_stream_seq: string | null;
	jet_stream_lane: string | null;
	lease_token: string | null;
	lease_expires_at: Date | null;
	dlq_attempts: number;
	attempts: number;
	max_attempts: number;
	run_at: Date | null;
	cancel_requested: boolean;
	context_link: string | null;
}

export const JOB_BY_ID_COLUMNS = [
	'job_id',
	'task_type',
	'status',
	'progress_current',
	'progress_total',
	'progress_message',
	'payload',
	'result',
	'error_message',
	'created_at',
	'state_changed_at',
	'started_at',
	'completed_at',
	'requested_by_user_id',
	'audit_log_reason',
	'jet_stream_seq',
	'jet_stream_lane',
	'lease_token',
	'lease_expires_at',
	'dlq_attempts',
	'attempts',
	'max_attempts',
	'run_at',
	'cancel_requested',
	'context_link',
] as const satisfies ReadonlyArray<keyof JobByIdRow>;

export interface JobByDayBucketRow {
	bucket_day: string;
	created_at: Date;
	job_id: bigint;
	task_type: string;
	status: JobStatus;
	requested_by_user_id: bigint | null;
}

export const JOB_BY_DAY_BUCKET_COLUMNS = [
	'bucket_day',
	'created_at',
	'job_id',
	'task_type',
	'status',
	'requested_by_user_id',
] as const satisfies ReadonlyArray<keyof JobByDayBucketRow>;

export interface JobActiveRow {
	job_id: bigint;
	task_type: string;
	status: JobStatus;
	requested_by_user_id: bigint | null;
	created_at: Date;
	started_at: Date | null;
}

export const JOB_ACTIVE_COLUMNS = [
	'job_id',
	'task_type',
	'status',
	'requested_by_user_id',
	'created_at',
	'started_at',
] as const satisfies ReadonlyArray<keyof JobActiveRow>;

export interface JobActiveV2Row extends JobActiveRow {
	shard: number;
}

export const JOB_ACTIVE_V2_COLUMNS = ['shard', ...JOB_ACTIVE_COLUMNS] as const satisfies ReadonlyArray<
	keyof JobActiveV2Row
>;
