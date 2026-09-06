// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {createStringType, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const JobStatusEnum = z.enum(['queued', 'running', 'succeeded', 'cancelled', 'deadletter']);

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

const ListJobsCursorSchema = z.object({
	bucket_day: z.string(),
	created_at: z.string(),
	job_id: SnowflakeStringType,
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

const CURSOR_FIELD_NAMES = ['cursor_bucket_day', 'cursor_created_at', 'cursor_job_id'] as const;

const BUCKET_DAY_REGEX = /^\d{4}-\d{2}-\d{2}$/;
const ISO_TIMESTAMP_REGEX = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:?\d{2})?$/;

function isCalendarDay(value: string): boolean {
	return BUCKET_DAY_REGEX.test(value) && !Number.isNaN(new Date(`${value}T00:00:00Z`).getTime());
}

function isIsoTimestamp(value: string): boolean {
	return ISO_TIMESTAMP_REGEX.test(value) && !Number.isNaN(new Date(value).getTime());
}

export const ListJobsQuery = z
	.object({
		limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 200}).describe(
			'Maximum number of jobs to return (1-200, default 50)',
		),
		cursor_bucket_day: createStringType(10, 10)
			.refine(isCalendarDay, ValidationErrorCodes.INVALID_FORMAT)
			.optional()
			.describe('Day bucket to resume from as a YYYY-MM-DD UTC date, taken from next_cursor.bucket_day'),
		cursor_created_at: createStringType(1, 64)
			.refine(isIsoTimestamp, ValidationErrorCodes.INVALID_ISO_TIMESTAMP)
			.optional()
			.describe('Creation time to resume before as an ISO 8601 timestamp, taken from next_cursor.created_at'),
		cursor_job_id: SnowflakeStringType.optional().describe('Job to resume from, taken from next_cursor.job_id'),
		max_lookback_days: createQueryIntegerType({defaultValue: 14, minValue: 1, maxValue: 60}).describe(
			'How many day buckets to scan back through (1-60, default 14)',
		),
		status: JobStatusEnum.optional().describe('Filter by job status'),
		task_type: createStringType(1, 128).optional().describe('Filter by task type'),
		requested_by_user_id: SnowflakeType.optional().describe('Filter by admin user who scheduled the job'),
	})
	.superRefine((value, ctx) => {
		const supplied = CURSOR_FIELD_NAMES.filter((name) => value[name] !== undefined);
		if (supplied.length === 0 || supplied.length === CURSOR_FIELD_NAMES.length) return;
		for (const name of CURSOR_FIELD_NAMES) {
			if (value[name] !== undefined) continue;
			ctx.addIssue({code: 'custom', message: ValidationErrorCodes.INVALID_FORMAT, path: [name]});
		}
	});

export type ListJobsQuery = z.infer<typeof ListJobsQuery>;

export const ListJobsResponseSchema = z.object({
	jobs: z.array(JobLedgerEntrySchema),
	next_cursor: ListJobsCursorSchema.nullable(),
});

export const GetJobResponseSchema = z.object({
	job: JobLedgerEntrySchema,
});

export const CancelJobResponseSchema = z.object({
	cancelled: z.boolean().describe('True if a cancel request was recorded; false if the job was already terminal.'),
});

export const ActiveJobsResponseSchema = z.object({
	jobs: z.array(JobLedgerEntrySchema),
});
