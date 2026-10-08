// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead} from '@app/api/admin/AdminAuditRecorder';
import {createReportID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getReportRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {deleteReportNow} from '@app/api/report/ReportDeletion';
import {getReportSearchService} from '@app/api/SearchFactory';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	ReportLegalHoldRequest,
	ReportLegalHoldResponse,
} from '@fluxer/schema/src/domains/admin/AdminReportLegalHoldSchemas';
import {
	AdminReportListResponse,
	AdminReportReasonsResponse,
	ListReportsQuery,
	ReportAdminResponseSchema,
	ResolveReportResponse,
	type SearchReportsRequest,
	UpdateReportRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {ReportIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

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
		query.reason !== undefined ||
		query.reporter_id !== undefined ||
		query.reported_user_id !== undefined ||
		query.reported_webhook_id !== undefined ||
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
		reason: query.reason,
		reported_user_id: query.reported_user_id,
		reported_webhook_id: query.reported_webhook_id,
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
				'Lists user and content reports from the report search index with pagination. Without a status filter the list covers reports of every status. A request that filters by status alone lists the reports with that status newest first and ignores sort_by and sort_order. A free-text query and the entity, category, reason and resolver filters narrow the results. Every response includes the total, offset and limit of the page. Reporter contact details are redacted unless the caller also holds REPORT_VIEW_REPORTER_PII. Requires REPORT_VIEW permission.',
			responseSchema: AdminReportListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const query = ctx.req.valid('query');
			const status = query.status;
			const searched = status === undefined || usesReportSearchIndex(query);
			const response = searched
				? await adminService.reportServiceAggregate.searchReports(toSearchReportsRequest(query), adminUserAcls)
				: await adminService.reportServiceAggregate.listReports(
						REPORT_STATUS_BY_FILTER[status],
						adminUserAcls,
						query.limit,
						query.offset,
					);
			await recordAdminRead(ctx, {
				targetType: 'report',
				targetId: 0n,
				action: AdminAuditReadActions.SEARCH_REPORTS,
				metadata: {
					has_query: query.q === undefined ? undefined : true,
					status,
					report_type: query.report_type,
					category: query.category,
					reason: query.reason,
					reporter_user_id: query.reporter_id,
					reported_user_id: query.reported_user_id,
					reported_webhook_id: query.reported_webhook_id,
					reported_guild_id: query.reported_guild_id,
					reported_channel_id: query.reported_channel_id,
					context_guild_id: query.guild_context_id,
					resolved_by_admin_user_id: query.resolved_by_admin_id,
					sort_by: searched ? query.sort_by : undefined,
					sort_order: searched ? query.sort_order : undefined,
					limit: query.limit,
					offset: query.offset,
					result_count: response.reports.length,
					total: response.total,
				},
			});
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/report-reasons',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		OpenAPI({
			operationId: 'list_admin_report_reasons',
			summary: 'List report reasons',
			description:
				'Lists every reason key a report filed through a report flow can carry, with its English label, whether it is a highest-priority reason, and the legacy category it maps to for message, user and community reports. Requires REPORT_VIEW permission.',
			responseSchema: AdminReportReasonsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const response = adminService.reportServiceAggregate.listReportReasons();
			await recordAdminRead(ctx, {
				targetType: 'report',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_REPORT_REASONS,
				metadata: {result_count: response.reasons.length},
			});
			return ctx.json(response);
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
				'Retrieves detailed information about a specific report including content, reporter, reason, and the message context and profile snapshot captured when it was filed. Requires REPORT_VIEW permission.',
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
			await recordAdminRead(ctx, {
				targetType: 'report',
				targetId: report_id,
				action: AdminAuditReadActions.GET_REPORT,
				metadata: {
					report_type: report.report_type,
					status: report.status,
				},
			});
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
				'Moves a report to the resolved status. The reporter is always told the report was reviewed: an account reporter by system DM and email, a Digital Services Act notifier by email. notify_reporter false leaves the public comment out of that notice. Nothing is sent when the reporter account is deleted or barred from reporting. Creates an audit log entry. Requires REPORT_RESOLVE permission.',
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
			const {public_comment, notify_reporter, resolution} = ctx.req.valid('json');
			return ctx.json(
				await adminService.reportServiceAggregate.resolveReport(
					createReportID(report_id),
					adminUserId,
					public_comment || null,
					auditLogReason,
					notify_reporter,
					resolution,
				),
			);
		},
	);
	app.post(
		'/admin/reports/:report_id/legal-hold',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_RESOLVE),
		Validator('param', ReportIdParam),
		Validator('json', ReportLegalHoldRequest),
		OpenAPI({
			operationId: 'set_admin_report_legal_hold',
			summary: 'Set report legal hold',
			description:
				'Places or clears a legal hold on a report. A report is deleted together with its stored evidence when its retention period ends, unless a hold that ends later is in place, and a held report cannot be deleted through DELETE /admin/reports/{report_id} either. A hold needs an end time in the future and a reason. A null legal_hold_until clears the hold. Creates an audit log entry. Requires REPORT_RESOLVE permission.',
			responseSchema: ReportLegalHoldResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {report_id} = ctx.req.valid('param');
			const {legal_hold_until, legal_hold_reason} = ctx.req.valid('json');
			const until = legal_hold_until === null ? null : new Date(legal_hold_until);
			const reason = until === null ? null : (legal_hold_reason ?? null);
			if (until !== null && until.getTime() <= Date.now()) {
				throw InputValidationError.create('legal_hold_until', 'The hold must end in the future');
			}
			if (until !== null && reason === null) {
				throw InputValidationError.create('legal_hold_reason', 'A reason is required to place a hold');
			}
			const reportId = createReportID(report_id);
			const reportRepository = getReportRepository();
			const previous = await reportRepository.getReport(reportId);
			const report = await reportRepository.setReportLegalHold(reportId, until, reason);
			const metadata = new Map([
				['report_id', reportId.toString()],
				['report_type', report.reportType.toString()],
			]);
			if (until !== null && reason !== null) {
				metadata.set('legal_hold_until', until.toISOString());
				metadata.set('legal_hold_reason', reason);
			}
			if (previous?.legalHoldUntil) {
				metadata.set('previous_legal_hold_until', previous.legalHoldUntil.toISOString());
			}
			await adminService.auditService.createAuditLog({
				adminUserId: ctx.get('adminUserId'),
				targetType: 'report',
				targetId: BigInt(reportId),
				action: until === null ? 'clear_report_legal_hold' : 'set_report_legal_hold',
				auditLogReason: ctx.get('auditLogReason'),
				metadata,
			});
			return ctx.json({
				report_id: reportId.toString(),
				legal_hold_until: report.legalHoldUntil?.toISOString() ?? null,
				legal_hold_reason: report.legalHoldReason,
			});
		},
	);
	app.delete(
		'/admin/reports/:report_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_DELETE),
		Validator('param', ReportIdParam),
		OpenAPI({
			operationId: 'delete_admin_report',
			summary: 'Delete report',
			description:
				'Deletes a report now: its record with the captured message context and profile snapshot, its stored evidence copies, its search index entry, and the duplicate-report reservation it holds for the reporter. An evidence copy that another report also uses is kept. A report under a legal hold that has not ended is refused with REPORT_UNDER_LEGAL_HOLD. This cannot be undone. Creates an audit log entry. Requires REPORT_DELETE permission.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {report_id} = ctx.req.valid('param');
			const reportId = createReportID(report_id);
			const deleted = await deleteReportNow(
				{
					reportRepository: getReportRepository(),
					storageService: ctx.get('storageService'),
					reportSearchService: getReportSearchService(),
				},
				reportId,
				new Date(),
			);
			await ctx.get('adminService').auditService.createAuditLog({
				adminUserId: ctx.get('adminUserId'),
				targetType: 'report',
				targetId: BigInt(reportId),
				action: 'delete_report',
				auditLogReason: ctx.get('auditLogReason'),
				metadata: new Map([
					['report_id', reportId.toString()],
					['report_type', deleted.report.reportType.toString()],
					['status', deleted.report.status.toString()],
					['objects_deleted', deleted.objectsDeleted.toString()],
					['shared_objects_kept', deleted.sharedObjectsKept.toString()],
				]),
			});
			return ctx.body(null, 204);
		},
	);
}
