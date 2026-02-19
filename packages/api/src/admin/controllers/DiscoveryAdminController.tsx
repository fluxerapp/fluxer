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

import {createGuildID} from '@fluxer/api/src/BrandedTypes';
import type {GuildDiscoveryRow} from '@fluxer/api/src/database/types/GuildDiscoveryTypes';
import {requireAdminACL} from '@fluxer/api/src/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	DiscoveryAdminListQuery,
	DiscoveryAdminRejectRequest,
	DiscoveryAdminRemoveRequest,
	DiscoveryAdminReviewRequest,
	DiscoveryApplicationResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {z} from 'zod';

function mapDiscoveryRowToResponse(row: GuildDiscoveryRow) {
	return {
		guild_id: row.guild_id.toString(),
		status: row.status,
		description: row.description,
		category_type: row.category_type,
		applied_at: row.applied_at.toISOString(),
		reviewed_at: row.removed_at?.toISOString() ?? row.reviewed_at?.toISOString() ?? null,
		review_reason: row.removal_reason ?? row.review_reason ?? null,
	};
}

export function DiscoveryAdminController(app: HonoApp) {
	app.get(
		'/admin/discovery/applications',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_LIST),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('query', DiscoveryAdminListQuery),
		OpenAPI({
			operationId: 'list_discovery_applications',
			summary: 'List discovery applications',
			description: 'List discovery applications filtered by status. Requires DISCOVERY_REVIEW permission.',
			responseSchema: z.array(DiscoveryApplicationResponse),
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const query = ctx.req.valid('query');
			const discoveryService = ctx.get('discoveryService');

			const rows = await discoveryService.listByStatus({
				status: query.status,
				limit: query.limit,
			});

			return ctx.json(rows.map(mapDiscoveryRowToResponse));
		},
	);

	app.post(
		'/admin/discovery/applications/:guild_id/approve',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryAdminReviewRequest),
		OpenAPI({
			operationId: 'approve_discovery_application',
			summary: 'Approve discovery application',
			description: 'Approve a pending discovery application. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');

			const row = await discoveryService.approve({
				guildId,
				adminUserId,
				reason: data.reason,
			});

			return ctx.json(mapDiscoveryRowToResponse(row));
		},
	);

	app.post(
		'/admin/discovery/applications/:guild_id/reject',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryAdminRejectRequest),
		OpenAPI({
			operationId: 'reject_discovery_application',
			summary: 'Reject discovery application',
			description: 'Reject a pending discovery application. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');

			const row = await discoveryService.reject({
				guildId,
				adminUserId,
				reason: data.reason,
			});

			return ctx.json(mapDiscoveryRowToResponse(row));
		},
	);

	app.post(
		'/admin/discovery/guilds/:guild_id/remove',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REMOVE),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryAdminRemoveRequest),
		OpenAPI({
			operationId: 'remove_from_discovery',
			summary: 'Remove guild from discovery',
			description: 'Remove an approved guild from discovery. Requires DISCOVERY_REMOVE permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');

			const row = await discoveryService.remove({
				guildId,
				adminUserId,
				reason: data.reason,
			});

			return ctx.json(mapDiscoveryRowToResponse(row));
		},
	);
}
