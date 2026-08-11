// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	ActiveJobsRequest,
	ActiveJobsResponseSchema,
	CancelJobRequest,
	CancelJobResponseSchema,
	GetJobRequest,
	GetJobResponseSchema,
	ListJobsRequest,
	ListJobsResponseSchema,
} from '@fluxer/schema/src/domains/admin/JobsSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function JobsAdminController(app: HonoApp) {
	app.post(
		'/admin/jobs/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		Validator('json', ListJobsRequest),
		OpenAPI({
			operationId: 'list_jobs',
			summary: 'List jobs',
			responseSchema: ListJobsResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Paginated, filterable list of background jobs from the human-facing ledger. Walks back through day-buckets and applies status / task-type / requester filters in-process.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.jobAdminService.listJobs(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/jobs/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		Validator('json', GetJobRequest),
		OpenAPI({
			operationId: 'get_job',
			summary: 'Get job detail',
			responseSchema: GetJobResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description: 'Fetch a single job ledger entry with full payload, result, and progress.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const result = await adminService.jobAdminService.getJob(ctx.req.valid('json').job_id);
			if (!result) return ctx.json({error: 'job_not_found'}, 404);
			return ctx.json(result);
		},
	);
	app.post(
		'/admin/jobs/cancel',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_CANCEL),
		Validator('json', CancelJobRequest),
		OpenAPI({
			operationId: 'cancel_job',
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
			return ctx.json(await adminService.jobAdminService.cancelJob(ctx.req.valid('json').job_id));
		},
	);
	app.post(
		'/admin/jobs/active',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_JOBS_VIEW),
		requireAdminACL(AdminACLs.JOBS_VIEW),
		Validator('json', ActiveJobsRequest),
		OpenAPI({
			operationId: 'list_active_jobs',
			summary: 'List active (queued + running) jobs',
			responseSchema: ActiveJobsResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Polling endpoint for the Jobs page. Returns only currently-active jobs (queued or running) so the UI can refresh progress without scanning historical data.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.jobAdminService.listActiveJobs(ctx.req.valid('json')));
		},
	);
}
