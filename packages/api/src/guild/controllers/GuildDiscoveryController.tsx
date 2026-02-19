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
import {Config} from '@fluxer/api/src/Config';
import type {GuildDiscoveryRow} from '@fluxer/api/src/database/types/GuildDiscoveryTypes';
import {LoginRequired} from '@fluxer/api/src/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@fluxer/api/src/RateLimitConfig';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus, DiscoveryCategoryLabels} from '@fluxer/constants/src/DiscoveryConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {DiscoveryDisabledError} from '@fluxer/errors/src/domains/discovery/DiscoveryDisabledError';
import {DiscoveryNotDiscoverableError} from '@fluxer/errors/src/domains/discovery/DiscoveryNotDiscoverableError';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	DiscoveryApplicationPatchRequest,
	DiscoveryApplicationRequest,
	DiscoveryApplicationResponse,
	DiscoveryCategoryListResponse,
	DiscoveryGuildListResponse,
	DiscoverySearchQuery,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';

function ensureDiscoveryEnabled(): void {
	if (!Config.discovery.enabled) {
		throw new DiscoveryDisabledError();
	}
}

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

export function GuildDiscoveryController(app: HonoApp) {
	app.get(
		'/discovery/guilds',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_SEARCH),
		LoginRequired,
		Validator('query', DiscoverySearchQuery),
		OpenAPI({
			operationId: 'search_discovery_guilds',
			summary: 'Search discoverable guilds',
			description: 'Search for guilds listed in the discovery directory.',
			responseSchema: DiscoveryGuildListResponse,
			statusCode: 200,
			security: ['sessionToken', 'bearerToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			ensureDiscoveryEnabled();
			const query = ctx.req.valid('query');
			const discoveryService = ctx.get('discoveryService');

			const results = await discoveryService.searchDiscoverable({
				query: query.query,
				categoryId: query.category,
				sortBy: query.sort_by,
				limit: query.limit,
				offset: query.offset,
			});

			return ctx.json(results);
		},
	);

	app.get(
		'/discovery/categories',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_CATEGORIES),
		LoginRequired,
		OpenAPI({
			operationId: 'list_discovery_categories',
			summary: 'List discovery categories',
			description: 'Returns the list of available discovery categories.',
			responseSchema: DiscoveryCategoryListResponse,
			statusCode: 200,
			security: ['sessionToken', 'bearerToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			const categories = Object.entries(DiscoveryCategoryLabels).map(([id, name]) => ({
				id: Number(id),
				name,
			}));
			return ctx.json(categories);
		},
	);

	app.post(
		'/discovery/guilds/:guild_id/join',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_JOIN),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'join_discovery_guild',
			summary: 'Join a discoverable guild',
			description: 'Join a guild that is listed in discovery without needing an invite.',
			responseSchema: null,
			statusCode: 204,
			security: ['sessionToken', 'bearerToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			ensureDiscoveryEnabled();
			const user = ctx.get('user');
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);

			const status = await ctx.get('discoveryService').getStatus(guildId);
			if (!status || status.status !== DiscoveryApplicationStatus.APPROVED) {
				throw new DiscoveryNotDiscoverableError();
			}

			await ctx.get('guildService').addUserToGuild({
				userId: user.id,
				guildId,
				sendJoinMessage: true,
				joinSourceType: 6,
				requestCache: ctx.get('requestCache'),
			});

			return ctx.body(null, 204);
		},
	);

	app.post(
		'/guilds/:guild_id/discovery',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_APPLY),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryApplicationRequest),
		OpenAPI({
			operationId: 'apply_for_discovery',
			summary: 'Apply for guild discovery',
			description: 'Submit a discovery application for a guild. Requires MANAGE_GUILD permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: ['sessionToken', 'bearerToken', 'botToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			ensureDiscoveryEnabled();
			const user = ctx.get('user');
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');

			const hasPermission = await ctx.get('gatewayService').checkPermission({
				guildId,
				userId: user.id,
				permission: Permissions.MANAGE_GUILD,
			});
			if (!hasPermission) {
				throw new MissingPermissionsError();
			}

			const row = await ctx.get('discoveryService').apply({
				guildId,
				userId: user.id,
				description: data.description,
				categoryId: data.category_type,
			});

			return ctx.json(mapDiscoveryRowToResponse(row));
		},
	);

	app.patch(
		'/guilds/:guild_id/discovery',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_APPLY),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryApplicationPatchRequest),
		OpenAPI({
			operationId: 'edit_discovery_application',
			summary: 'Edit discovery application',
			description:
				'Update the description or category of an existing discovery application. Requires MANAGE_GUILD permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: ['sessionToken', 'bearerToken', 'botToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			ensureDiscoveryEnabled();
			const user = ctx.get('user');
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');

			const hasPermission = await ctx.get('gatewayService').checkPermission({
				guildId,
				userId: user.id,
				permission: Permissions.MANAGE_GUILD,
			});
			if (!hasPermission) {
				throw new MissingPermissionsError();
			}

			const row = await ctx.get('discoveryService').editApplication({
				guildId,
				userId: user.id,
				data,
			});

			return ctx.json(mapDiscoveryRowToResponse(row));
		},
	);

	app.delete(
		'/guilds/:guild_id/discovery',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_APPLY),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'withdraw_discovery_application',
			summary: 'Withdraw discovery application',
			description:
				'Withdraw a discovery application or remove a guild from discovery. Requires MANAGE_GUILD permission.',
			responseSchema: null,
			statusCode: 204,
			security: ['sessionToken', 'bearerToken', 'botToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			ensureDiscoveryEnabled();
			const user = ctx.get('user');
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);

			const hasPermission = await ctx.get('gatewayService').checkPermission({
				guildId,
				userId: user.id,
				permission: Permissions.MANAGE_GUILD,
			});
			if (!hasPermission) {
				throw new MissingPermissionsError();
			}

			await ctx.get('discoveryService').withdraw({guildId, userId: user.id});

			return ctx.body(null, 204);
		},
	);

	app.get(
		'/guilds/:guild_id/discovery',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_STATUS),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'get_discovery_status',
			summary: 'Get discovery status',
			description: 'Get the current discovery status and eligibility of a guild. Requires MANAGE_GUILD permission.',
			responseSchema: DiscoveryStatusResponse,
			statusCode: 200,
			security: ['sessionToken', 'bearerToken', 'botToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);

			const hasPermission = await ctx.get('gatewayService').checkPermission({
				guildId,
				userId: user.id,
				permission: Permissions.MANAGE_GUILD,
			});
			if (!hasPermission) {
				throw new MissingPermissionsError();
			}

			const discoveryService = ctx.get('discoveryService');
			const row = await discoveryService.getStatus(guildId);
			const eligibility = await discoveryService.getEligibility(guildId);

			return ctx.json({
				application: row ? mapDiscoveryRowToResponse(row) : null,
				eligible: Config.discovery.enabled && eligibility.eligible,
				min_member_count: eligibility.min_member_count,
			});
		},
	);
}
