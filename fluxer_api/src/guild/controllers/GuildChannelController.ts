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
import {createChannelID, createGuildID} from '~/BrandedTypes';
import {ChannelCreateRequest} from '~/channel/ChannelModel';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const GuildChannelController = (app: HonoApp) => {
	app.get(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNELS_LIST),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').getChannels({userId, guildId, requestCache}));
		},
	);

	app.post(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNEL_CREATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', ChannelCreateRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').createChannel({userId, guildId, data, requestCache}, auditLogReason),
			);
		},
	);

	app.patch(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNEL_POSITIONS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator(
			'json',
			z.array(
				z.object({
					id: Int64Type,
					position: z.number().int().nonnegative().optional(),
					parent_id: Int64Type.nullish(),
					lock_permissions: z.boolean().optional(),
				}),
			),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;

			await ctx.get('guildService').updateChannelPositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({
						channelId: createChannelID(item.id),
						position: item.position,
						parentId: item.parent_id == null ? item.parent_id : createChannelID(item.parent_id),
						lockPermissions: item.lock_permissions ?? false,
					})),
					requestCache,
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
};
