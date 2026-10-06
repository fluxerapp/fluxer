// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, snowflakeOrUndefined} from '@app/api/admin/AdminAuditRecorder';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {
	AdminAuditLogResponseSchema,
	AuditLogIdParam,
	AuditLogsListResponseSchema,
	ListAdminAuditLogsQuery,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';

const AUDIT_TARGET_TYPE_PATTERN = /^[a-z][a-z_]{0,63}$/;

export function AuditLogAdminController(app: HonoApp) {
	app.get(
		'/admin/audit-logs',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_AUDIT_LOG),
		requireAdminACL(AdminACLs.AUDIT_LOG_VIEW),
		Validator('query', ListAdminAuditLogsQuery),
		OpenAPI({
			operationId: 'list_admin_audit_logs',
			summary: 'List admin audit logs',
			responseSchema: AuditLogsListResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Retrieve a paginated page of audit logs with optional filtering by acting admin, target type, or target ID. Passing q runs a full-text search across the audit log index instead of paging through the log in order, and sort_by with sort_order then order the matches. Used for tracking administrative operations, compliance auditing, and incident response.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {q, admin_user_id, target_type, target_id, access, sort_by, sort_order, limit, offset} =
				ctx.req.valid('query');
			const response =
				q === undefined
					? await adminService.auditService.listAuditLogs({
							admin_user_id,
							target_type,
							target_id,
							access,
							limit,
							offset,
						})
					: await adminService.auditService.searchAuditLogs({
							query: q,
							admin_user_id,
							target_type,
							target_id,
							access,
							sort_by,
							sort_order,
							limit,
							offset,
						});
			await recordAdminRead(ctx, {
				targetType: 'audit_log',
				targetId: 0n,
				action: q === undefined ? AdminAuditReadActions.LIST_AUDIT_LOGS : AdminAuditReadActions.SEARCH_AUDIT_LOGS,
				metadata: {
					filter_admin_user_id: admin_user_id,
					filter_target_type:
						target_type !== undefined && AUDIT_TARGET_TYPE_PATTERN.test(target_type) ? target_type : undefined,
					has_target_type_filter:
						target_type !== undefined && !AUDIT_TARGET_TYPE_PATTERN.test(target_type) ? true : undefined,
					filter_target_id: snowflakeOrUndefined(target_id),
					access,
					sort_by: q === undefined ? undefined : sort_by,
					sort_order: q === undefined ? undefined : sort_order,
					limit,
					offset,
					result_count: response.logs.length,
					total: response.total,
				},
			});
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/audit-logs/:log_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_AUDIT_LOG),
		requireAdminACL(AdminACLs.AUDIT_LOG_VIEW),
		Validator('param', AuditLogIdParam),
		OpenAPI({
			operationId: 'get_admin_audit_log',
			summary: 'Get admin audit log entry',
			responseSchema: AdminAuditLogResponseSchema,
			statusCode: 200,
			security: ['adminApiKey'],
			tags: ['Admin'],
			description:
				'Retrieve a single admin audit log entry by ID, with the same resolved user, guild, and channel summaries the listing returns. Used to inspect one administrative operation during compliance investigations or incident response.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {log_id} = ctx.req.valid('param');
			const log = await adminService.auditService.getAuditLog(log_id);
			if (!log) {
				throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
			}
			await recordAdminRead(ctx, {
				targetType: 'audit_log',
				targetId: log_id,
				action: AdminAuditReadActions.GET_AUDIT_LOG,
			});
			return ctx.json(log);
		},
	);
}
