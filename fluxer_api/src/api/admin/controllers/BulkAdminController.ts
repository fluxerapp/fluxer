// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	BulkAddGuildMembersRequest,
	BulkUpdateGuildFeaturesRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {BulkDeleteUserMessagesRequest} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import {BulkJobResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	BulkScheduleUserDeletionRequest,
	BulkUpdateSuspiciousActivityFlagsRequest,
	BulkUpdateUserFlagsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {getWorkerService} from '../../middleware/ServiceRegistry';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function BulkAdminController(app: HonoApp) {
	app.post(
		'/admin/bulk/update-user-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_UPDATE_USER_FLAGS),
		Validator('json', BulkUpdateUserFlagsRequest),
		OpenAPI({
			operationId: 'bulk_update_user_flags',
			summary: 'Bulk update user flags',
			description:
				'Enqueue a background job that modifies user flags (e.g., verified, bot, system) for multiple users. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkUpdateUserFlags',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					add_flags: body.add_flags,
					remove_flags: body.remove_flags,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.post(
		'/admin/bulk/update-suspicious-activity-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_UPDATE_SUSPICIOUS_ACTIVITY),
		Validator('json', BulkUpdateSuspiciousActivityFlagsRequest),
		OpenAPI({
			operationId: 'bulk_update_suspicious_activity_flags',
			summary: 'Bulk update suspicious activity flags',
			description:
				'Enqueue a background job that modifies suspicious activity flags for multiple users. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkUpdateSuspiciousActivityFlags',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					add_flags: body.add_flags,
					remove_flags: body.remove_flags,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.post(
		'/admin/bulk/update-guild-features',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_UPDATE_GUILD_FEATURES),
		Validator('json', BulkUpdateGuildFeaturesRequest),
		OpenAPI({
			operationId: 'bulk_update_guild_features',
			summary: 'Bulk update guild features',
			description:
				'Enqueue a background job that modifies guild features across multiple servers. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkUpdateGuildFeatures',
				{
					guild_ids: body.guild_ids.map((id) => id.toString()),
					add_features: body.add_features,
					remove_features: body.remove_features,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.post(
		'/admin/bulk/add-guild-members',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_ADD_GUILD_MEMBERS),
		Validator('json', BulkAddGuildMembersRequest),
		OpenAPI({
			operationId: 'bulk_add_guild_members',
			summary: 'Bulk add guild members',
			description:
				'Enqueue a background job that adds multiple users to a guild. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkAddGuildMembers',
				{
					guild_id: body.guild_id.toString(),
					user_ids: body.user_ids.map((id) => id.toString()),
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.post(
		'/admin/bulk/schedule-user-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_DELETE_USERS),
		Validator('json', BulkScheduleUserDeletionRequest),
		OpenAPI({
			operationId: 'schedule_bulk_user_deletion',
			summary: 'Schedule bulk user deletion',
			description:
				'Enqueue a background job that schedules account deletions for multiple users. Returns a job_id immediately; observe progress at /admin/jobs/:job_id. Note: the worker version skips Stripe refunds, session termination, and identifier banning — apply those separately for high-risk accounts.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkScheduleUserDeletion',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					reason_code: body.reason_code,
					days_until_deletion: body.days_until_deletion,
					public_reason: body.public_reason ?? null,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
	app.post(
		'/admin/bulk/delete-user-messages',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_DELETE_USER_MESSAGES),
		Validator('json', BulkDeleteUserMessagesRequest),
		OpenAPI({
			operationId: 'bulk_delete_user_messages',
			summary: 'Bulk delete user messages',
			description:
				'Enqueue a background job that deletes every message authored by each of the given users across all channels. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const jobId = await getWorkerService().addJob(
				'bulkDeleteMessagesForUsers',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				{requestedByUserId: adminUserId, ...(auditLogReason && {auditLogReason})},
			);
			return ctx.json({job_id: jobId.toString()});
		},
	);
}
