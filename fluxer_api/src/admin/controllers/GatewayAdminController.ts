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
import {createGuildID} from '~/BrandedTypes';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';
import {GetProcessMemoryStatsRequest} from '../AdminModel';

export const GatewayAdminController = (app: HonoApp) => {
	app.post(
		'/admin/gateway/memory-stats',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GATEWAY_MEMORY_STATS),
		Validator('json', GetProcessMemoryStatsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.getGuildMemoryStats(body.limit));
		},
	);

	app.post(
		'/admin/gateway/reload-all',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_GATEWAY_RELOAD),
		requireAdminACL(AdminACLs.GATEWAY_RELOAD_ALL),
		Validator('json', z.object({guild_ids: z.array(Int64Type)})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			const guildIds = body.guild_ids.map((id) => createGuildID(id));
			return ctx.json(await adminService.reloadAllGuilds(guildIds));
		},
	);

	app.get(
		'/admin/gateway/stats',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GATEWAY_MEMORY_STATS),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.getNodeStats());
		},
	);
};
