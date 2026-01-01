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
import {createGuildID, createRoleID} from '~/BrandedTypes';
import {GuildRoleCreateRequest, GuildRoleUpdateRequest} from '~/guild/GuildModel';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const GuildRoleController = (app: HonoApp) => {
	app.get(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_LIST),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').listRoles({userId, guildId}));
		},
	);

	app.post(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_CREATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', GuildRoleCreateRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').createRole({userId, guildId, data}, auditLogReason));
		},
	);

	app.patch(
		'/guilds/:guild_id/roles/hoist-positions',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_HOIST_POSITIONS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator(
			'json',
			z.array(
				z.object({
					id: Int64Type,
					hoist_position: z.number().int(),
				}),
			),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').updateHoistPositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({roleId: createRoleID(item.id), hoistPosition: item.hoist_position})),
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/roles/hoist-positions',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_HOIST_POSITIONS_RESET),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').resetHoistPositions({userId, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.patch(
		'/guilds/:guild_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, role_id: Int64Type})),
		Validator('json', GuildRoleUpdateRequest),
		async (ctx) => {
			const {guild_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').updateRole({userId, guildId, roleId, data}, auditLogReason));
		},
	);

	app.patch(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_POSITIONS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator(
			'json',
			z.array(
				z.object({
					id: Int64Type,
					position: z.number().int().optional(),
				}),
			),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').updateRolePositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({roleId: createRoleID(item.id), position: item.position})),
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_DELETE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, role_id: Int64Type})),
		async (ctx) => {
			const {guild_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').deleteRole({userId, guildId, roleId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
};
