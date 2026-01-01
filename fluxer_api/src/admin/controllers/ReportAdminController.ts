/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {HonoApp} from '~/App';
import {createReportID} from '~/BrandedTypes';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';
import {SearchReportsRequest} from '../AdminModel';

export const ReportAdminController = (app: HonoApp) => {
	app.post(
		'/admin/reports/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		Validator(
			'json',
			z.object({
				status: z.number().optional(),
				limit: z.number().optional(),
				offset: z.number().optional(),
			}),
		),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {status, limit, offset} = ctx.req.valid('json');
			return ctx.json(await adminService.listReports(status ?? 0, limit, offset));
		},
	);

	app.get(
		'/admin/reports/:report_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		Validator('param', z.object({report_id: Int64Type})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {report_id} = ctx.req.valid('param');
			const report = await adminService.getReport(createReportID(report_id));
			return ctx.json(report);
		},
	);

	app.post(
		'/admin/reports/resolve',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_RESOLVE),
		Validator(
			'json',
			z.object({
				report_id: Int64Type,
				public_comment: createStringType(0, 512).optional(),
			}),
		),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {report_id, public_comment} = ctx.req.valid('json');
			return ctx.json(
				await adminService.resolveReport(
					createReportID(report_id),
					adminUserId,
					public_comment || null,
					auditLogReason,
				),
			);
		},
	);

	app.post(
		'/admin/reports/search',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.REPORT_VIEW),
		Validator('json', SearchReportsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.searchReports(body));
		},
	);
};
