// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	AdminReportListResponse,
	ListReportsQuery,
	ReportAdminResponseSchema,
	ResolveReportResponse,
	type SearchReportsRequest,
	UpdateReportRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {ReportIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createReportID} from '../../BrandedTypes';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

const REPORT_STATUS_BY_FILTER = {
	pending: 0,
	resolved: 1,
} as const;

const REPORT_TYPE_BY_FILTER = {
	message: 0,
	user: 1,
	guild: 2,
} as const;

const REPORT_SORT_FIELD_BY_QUERY = {
	created_at: 'createdAt',
	reported_at: 'reportedAt',
	resolved_at: 'resolvedAt',
} as const;

function usesReportSearchIndex(query: ListReportsQuery): boolean {
	return (
		query.q !== undefined ||
		query.report_type !== undefined ||
		query.category !== undefined ||
		query.reporter_id !== undefined ||
		query.reported_user_id !== undefined ||
		query.reported_guild_id !== undefined ||
		query.reported_channel_id !== undefined ||
		query.guild_context_id !== undefined ||
		query.resolved_by_admin_id !== undefined
	);
}

function toSearchReportsRequest(query: ListReportsQuery): SearchReportsRequest {
	return {
		query: query.q,
		limit: query.limit,
		offset: query.offset,
		reporter_id: query.reporter_id,
		status: query.status === undefined ? undefined : REPORT_STATUS_BY_FILTER[query.status],
		report_type: query.report_type === undefined ? undefined : REPORT_TYPE_BY_FILTER[query.report_type],
		category: query.category,
		reported_user_id: query.reported_user_id,
		reported_guild_id: query.reported_guild_id,
		reported_channel_id: query.reported_channel_id,
		guild_context_id: query.guild_context_id,
		resolved_by_admin_id: query.resolved_by_admin_id,
		sort_by: REPORT_SORT_FIELD_BY_QUERY[query.sort_by],
		sort_order: query.sort_order,
	};
}

export function ReportAdminController(app: HonoApp) {
	app.get(
		'/admin/reports',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		Validator('query', ListReportsQuery),
		OpenAPI({
			operationId: 'list_admin_reports',
			summary: 'List reports',
			description:
				'Lists user and content reports with pagination. Filtering by status alone reads them straight from the database; supplying a free-text query or any of the entity, category and resolver filters searches the report index instead and adds the total, offset and limit of the page to the response. Reporter contact details are redacted unless the caller also holds REPORT_VIEW_REPORTER_PII. Requires REPORT_VIEW permission.',
			responseSchema: AdminReportListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const query = ctx.req.valid('query');
			if (usesReportSearchIndex(query)) {
				return ctx.json(
					await adminService.reportServiceAggregate.searchReports(toSearchReportsRequest(query), adminUserAcls),
				);
			}
			const status = query.status === undefined ? 0 : REPORT_STATUS_BY_FILTER[query.status];
			return ctx.json(
				await adminService.reportServiceAggregate.listReports(status, adminUserAcls, query.limit, query.offset),
			);
		},
	);
	app.get(
		'/admin/reports/:report_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		Validator('param', ReportIdParam),
		OpenAPI({
			operationId: 'get_admin_report',
			summary: 'Get report',
			description:
				'Retrieves detailed information about a specific report including content, reporter, reason, and the message context captured when it was filed. Requires REPORT_VIEW permission.',
			responseSchema: ReportAdminResponseSchema,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {report_id} = ctx.req.valid('param');
			const report = await adminService.reportServiceAggregate.getReport(createReportID(report_id), adminUserAcls);
			return ctx.json(report);
		},
	);
	app.patch(
		'/admin/reports/:report_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_RESOLVE),
		Validator('param', ReportIdParam),
		Validator('json', UpdateReportRequest),
		OpenAPI({
			operationId: 'update_admin_report',
			summary: 'Update report',
			description:
				'Moves a report to the resolved status with an optional public comment shown to the reporter. Marks the report as handled, notifies the reporter, and creates an audit log entry. Requires REPORT_RESOLVE permission.',
			responseSchema: ResolveReportResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {report_id} = ctx.req.valid('param');
			const {public_comment} = ctx.req.valid('json');
			return ctx.json(
				await adminService.reportServiceAggregate.resolveReport(
					createReportID(report_id),
					adminUserId,
					public_comment || null,
					auditLogReason,
				),
			);
		},
	);
}
