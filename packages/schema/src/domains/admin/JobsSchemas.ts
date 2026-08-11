// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const JobStatusEnum = z.enum([
	'queued',
	'running',
	'deadletter_pending',
	'succeeded',
	'failed',
	'cancelled',
	'deadletter',
]);

export const JobLedgerEntrySchema = z.object({
	job_id: SnowflakeStringType,
	task_type: z.string(),
	status: JobStatusEnum,
	progress_current: z.number().nullable(),
	progress_total: z.number().nullable(),
	progress_message: z.string().nullable(),
	error_message: z.string().nullable(),
	created_at: z.string().describe('ISO 8601'),
	started_at: z.string().nullable(),
	completed_at: z.string().nullable(),
	requested_by_user_id: SnowflakeStringType.nullable(),
	audit_log_reason: z.string().nullable(),
	jet_stream_lane: z.string().nullable(),
	jet_stream_seq: z.string().nullable(),
	attempts: z.number().int(),
	max_attempts: z.number().int(),
	run_at: z.string().nullable(),
	cancel_requested: z.boolean(),
	context_link: z.string().nullable(),
	payload: z.string().nullable().describe('JSON-encoded original payload'),
	result: z.string().nullable().describe('JSON-encoded result, if any'),
});

export type JobLedgerEntry = z.infer<typeof JobLedgerEntrySchema>;

const ListJobsCursorSchema = z
	.object({
		bucket_day: z.iso.date(),
		created_at: z.iso.datetime({offset: true}),
		job_id: SnowflakeStringType,
	})
	.refine((cursor) => new Date(cursor.created_at).toISOString().slice(0, 10) === cursor.bucket_day, {
		message: 'created_at must fall within bucket_day in UTC',
		path: ['created_at'],
	});

export const ListJobsRequest = z.object({
	limit: z.number().int().min(1).max(200).default(50).describe('Page size'),
	cursor: ListJobsCursorSchema.optional().describe('Cursor returned by a previous page'),
	max_lookback_days: z.number().int().min(1).max(60).default(14).describe('How many days back to scan'),
	status: JobStatusEnum.optional().describe('Filter by job status'),
	task_type: z.string().optional().describe('Filter by task type'),
	requested_by_user_id: SnowflakeType.optional().describe('Filter by admin user who scheduled the job'),
});

export type ListJobsRequest = z.infer<typeof ListJobsRequest>;

export const ListJobsResponseSchema = z.object({
	jobs: z.array(JobLedgerEntrySchema),
	next_cursor: ListJobsCursorSchema.nullable(),
});

export const GetJobRequest = z.object({
	job_id: SnowflakeType,
});

export type GetJobRequest = z.infer<typeof GetJobRequest>;

export const GetJobResponseSchema = z.object({
	job: JobLedgerEntrySchema,
});

export const CancelJobRequest = z.object({
	job_id: SnowflakeType,
});

export type CancelJobRequest = z.infer<typeof CancelJobRequest>;

export const CancelJobResponseSchema = z.object({
	cancelled: z.boolean().describe('True if a cancel request was recorded; false if the job was already terminal.'),
});

export const ActiveJobsRequest = z.object({
	limit: z.number().int().min(1).max(200).default(50),
	page_state: z.string().max(16_384).nullish(),
	task_type: z.string().optional(),
});

export type ActiveJobsRequest = z.infer<typeof ActiveJobsRequest>;

export const ActiveJobsResponseSchema = z.object({
	jobs: z.array(JobLedgerEntrySchema),
	next_page_state: z.string().nullable(),
});
