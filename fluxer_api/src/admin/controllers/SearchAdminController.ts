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
import {
	GetIndexRefreshStatusRequest,
	RefreshSearchIndexRequest,
	SearchGuildsRequest,
	SearchUsersRequest,
} from '../AdminModel';

export const SearchAdminController = (app: HonoApp) => {
	app.post(
		'/admin/guilds/search',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('json', SearchGuildsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.searchGuilds(body));
		},
	);

	app.post(
		'/admin/users/search',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', SearchUsersRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.searchUsers(body));
		},
	);

	app.post(
		'/admin/search/refresh-index',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('json', RefreshSearchIndexRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.refreshSearchIndex(body, adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/search/refresh-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('json', GetIndexRefreshStatusRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.getIndexRefreshStatus(body.job_id));
		},
	);
};
