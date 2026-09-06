// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import {
	AdminAuditLogResponseSchema,
	AuditLogIdParam,
	AuditLogsListResponseSchema,
	ListAdminAuditLogsQuery,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

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
			const {q, admin_user_id, target_type, target_id, sort_by, sort_order, limit, offset} = ctx.req.valid('query');
			if (q === undefined) {
				return ctx.json(
					await adminService.auditService.listAuditLogs({admin_user_id, target_type, target_id, limit, offset}),
				);
			}
			return ctx.json(
				await adminService.auditService.searchAuditLogs({
					query: q,
					admin_user_id,
					target_type,
					target_id,
					sort_by,
					sort_order,
					limit,
					offset,
				}),
			);
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
			return ctx.json(log);
		},
	);
}
