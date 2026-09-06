// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	ActiveJobsResponseSchema,
	CancelJobResponseSchema,
	GetJobResponseSchema,
	ListJobsQuery,
	type ListJobsRequest,
	ListJobsResponseSchema,
} from '@fluxer/schema/src/domains/admin/JobsSchemas';
import {JobIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

function toListJobsRequest(query: ListJobsQuery): ListJobsRequest {
	return {
		limit: query.limit,
		max_lookback_days: query.max_lookback_days,
		...(query.cursor_bucket_day !== undefined &&
			query.cursor_created_at !== undefined &&
			query.cursor_job_id !== undefined && {
				cursor: {
					bucket_day: query.cursor_bucket_day,
					created_at: query.cursor_created_at,
					job_id: query.cursor_job_id,
				},
			}),
		...(query.status !== undefined && {status: query.status}),
		...(query.task_type !== undefined && {task_type: query.task_type}),
		...(query.requested_by_user_id !== undefined && {requested_by_user_id: query.requested_by_user_id}),
	};
}

export function JobsAdminController(app: HonoApp) {
	app.get(
		'/admin/jobs',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		Validator('query', ListJobsQuery),
		OpenAPI({
			operationId: 'list_admin_jobs',
			summary: 'List jobs',
			responseSchema: ListJobsResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Paginated, filterable list of background jobs from the human-facing ledger. Walks back through day-buckets and applies status / task-type / requester filters in-process. The three cursor query parameters come from the previous page `next_cursor` and must be supplied together.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.jobAdminService.listJobs(toListJobsRequest(ctx.req.valid('query'))));
		},
	);
	app.get(
		'/admin/jobs/active',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		OpenAPI({
			operationId: 'list_admin_active_jobs',
			summary: 'List active jobs',
			responseSchema: ActiveJobsResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Polling endpoint for the Jobs page. Returns only currently-active jobs (queued or running) from their own index, so the UI can refresh progress without scanning historical day-buckets.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.jobAdminService.listActiveJobs());
		},
	);
	app.get(
		'/admin/jobs/:job_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		Validator('param', JobIdParam),
		OpenAPI({
			operationId: 'get_admin_job',
			summary: 'Get job detail',
			responseSchema: GetJobResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description: 'Fetch a single job ledger entry with full payload, result, and progress.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const result = await adminService.jobAdminService.getJob(ctx.req.valid('param').job_id);
			if (!result) return ctx.json({error: 'job_not_found'}, 404);
			return ctx.json(result);
		},
	);
	app.put(
		'/admin/jobs/:job_id/cancellation',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_CANCEL),
		Validator('param', JobIdParam),
		OpenAPI({
			operationId: 'create_admin_job_cancellation',
			summary: 'Request cancellation of a running job',
			responseSchema: CancelJobResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Mark a job as cancel-requested. The handler must be cooperatively cancellable — it will see the flag at its next `helpers.shouldCancel()` check. Returns `{cancelled: false}` for already-terminal jobs.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.jobAdminService.cancelJob(ctx.req.valid('param').job_id));
		},
	);
}
