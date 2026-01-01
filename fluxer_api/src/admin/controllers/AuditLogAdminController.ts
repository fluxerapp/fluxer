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
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import {ListAuditLogsRequest, SearchAuditLogsRequest} from '../AdminModel';

export const AuditLogAdminController = (app: HonoApp) => {
	app.post(
		'/admin/audit-logs',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_AUDIT_LOG),
		requireAdminACL(AdminACLs.AUDIT_LOG_VIEW),
		Validator('json', ListAuditLogsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.listAuditLogs(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/audit-logs/search',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_AUDIT_LOG),
		requireAdminACL(AdminACLs.AUDIT_LOG_VIEW),
		Validator('json', SearchAuditLogsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.searchAuditLogs(ctx.req.valid('json')));
		},
	);
};
