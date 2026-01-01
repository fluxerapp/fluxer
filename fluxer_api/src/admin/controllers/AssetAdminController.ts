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
import {AdminRateLimitConfigs} from '~/rate_limit_configs/AdminRateLimitConfig';
import {Validator} from '~/Validator';
import {PurgeGuildAssetsRequest} from '../AdminModel';

export const AssetAdminController = (app: HonoApp) => {
	app.post(
		'/admin/assets/purge',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_GUILD_MODIFY),
		requireAdminACL(AdminACLs.ASSET_PURGE),
		Validator('json', PurgeGuildAssetsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.purgeGuildAssets(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);
};
