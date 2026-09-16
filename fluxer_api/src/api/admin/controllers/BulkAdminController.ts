// SPDX-License-Identifier: AGPL-3.0-or-later

import {recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import type {UserID} from '@app/api/BrandedTypes';
import {requireAnyAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {AdminBulkJobCreateRequest, AdminBulkTaskType} from '@fluxer/schema/src/domains/admin/AdminBulkSchemas';
import {BulkJobResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';

const BULK_TASK_ACLS: Record<AdminBulkTaskType, string> = {
	[AdminBulkTaskType.UPDATE_USER_FLAGS]: AdminACLs.BULK_UPDATE_USER_FLAGS,
	[AdminBulkTaskType.UPDATE_SUSPICIOUS_ACTIVITY_FLAGS]: AdminACLs.BULK_UPDATE_SUSPICIOUS_ACTIVITY,
	[AdminBulkTaskType.UPDATE_GUILD_FEATURES]: AdminACLs.BULK_UPDATE_GUILD_FEATURES,
	[AdminBulkTaskType.ADD_GUILD_MEMBERS]: AdminACLs.BULK_ADD_GUILD_MEMBERS,
	[AdminBulkTaskType.SCHEDULE_USER_DELETION]: AdminACLs.BULK_DELETE_USERS,
	[AdminBulkTaskType.DELETE_USER_MESSAGES]: AdminACLs.BULK_DELETE_USER_MESSAGES,
};

async function queueBulkJob(
	body: AdminBulkJobCreateRequest,
	adminUserId: UserID,
	auditLogReason: string | null,
): Promise<bigint> {
	const workerService = getWorkerService();
	const options = {requestedByUserId: adminUserId, requireLedger: true, ...(auditLogReason && {auditLogReason})};
	switch (body.task) {
		case AdminBulkTaskType.UPDATE_USER_FLAGS:
			return await workerService.addJob(
				'bulkUpdateUserFlags',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					add_flags: body.add_flags,
					remove_flags: body.remove_flags,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
		case AdminBulkTaskType.UPDATE_SUSPICIOUS_ACTIVITY_FLAGS:
			return await workerService.addJob(
				'bulkUpdateSuspiciousActivityFlags',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					add_flags: body.add_flags,
					remove_flags: body.remove_flags,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
		case AdminBulkTaskType.UPDATE_GUILD_FEATURES:
			return await workerService.addJob(
				'bulkUpdateGuildFeatures',
				{
					guild_ids: body.guild_ids.map((id) => id.toString()),
					add_features: body.add_features,
					remove_features: body.remove_features,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
		case AdminBulkTaskType.ADD_GUILD_MEMBERS:
			return await workerService.addJob(
				'bulkAddGuildMembers',
				{
					guild_id: body.guild_id.toString(),
					user_ids: body.user_ids.map((id) => id.toString()),
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
		case AdminBulkTaskType.SCHEDULE_USER_DELETION:
			return await workerService.addJob(
				'bulkScheduleUserDeletion',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					reason_code: body.reason_code,
					days_until_deletion: body.days_until_deletion,
					public_reason: body.public_reason ?? null,
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
		case AdminBulkTaskType.DELETE_USER_MESSAGES:
			return await workerService.addJob(
				'bulkDeleteMessagesForUsers',
				{
					user_ids: body.user_ids.map((id) => id.toString()),
					admin_user_id: adminUserId.toString(),
					audit_log_reason: auditLogReason,
				},
				options,
			);
	}
}

export function BulkAdminController(app: HonoApp) {
	app.post(
		'/admin/bulk-jobs',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		Validator('json', AdminBulkJobCreateRequest),
		requireAnyAdminACL([
			AdminACLs.BULK_UPDATE_USER_FLAGS,
			AdminACLs.BULK_UPDATE_SUSPICIOUS_ACTIVITY,
			AdminACLs.BULK_UPDATE_GUILD_FEATURES,
			AdminACLs.BULK_ADD_GUILD_MEMBERS,
			AdminACLs.BULK_DELETE_USERS,
			AdminACLs.BULK_DELETE_USER_MESSAGES,
		]),
		OpenAPI({
			operationId: 'create_admin_bulk_job',
			summary: 'Queue a bulk job',
			description:
				'Enqueue one background administrative job. The `task` discriminator selects both the body variant and the ACL evaluated for the request: `update_user_flags` needs bulk:update:user_flags, `update_suspicious_activity_flags` needs bulk:update:suspicious_activity, `update_guild_features` needs bulk:update:guild_features, `add_guild_members` needs bulk:add:guild_members, `schedule_user_deletion` needs bulk:delete:users, and `delete_user_messages` needs bulk:delete:user_messages. Returns a job_id immediately; observe progress at /admin/jobs/:job_id.',
			responseSchema: BulkJobResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminUserId = ctx.get('adminUserId');
			const adminAcls = ctx.get('adminUserAcls');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const requiredAcl = BULK_TASK_ACLS[body.task];
			if (!adminAcls.has(requiredAcl) && !adminAcls.has(AdminACLs.WILDCARD)) {
				throw new MissingACLError(requiredAcl);
			}
			const jobId = await queueBulkJob(body, adminUserId, auditLogReason);
			await recordAdminWrite(ctx, {
				targetType: 'bulk_job',
				targetId: jobId,
				action: 'queue_bulk_job',
				metadata: {
					task: body.task,
					entity_count: 'guild_ids' in body ? body.guild_ids.length : body.user_ids.length,
					guild_id: body.task === AdminBulkTaskType.ADD_GUILD_MEMBERS ? body.guild_id : undefined,
				},
			});
			return ctx.json({job_id: jobId.toString()});
		},
	);
}
