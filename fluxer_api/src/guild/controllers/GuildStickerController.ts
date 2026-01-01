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
import {createGuildID, createStickerID} from '~/BrandedTypes';
import {GuildStickerBulkCreateRequest, GuildStickerCreateRequest, GuildStickerUpdateRequest} from '~/guild/GuildModel';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, QueryBooleanType, z} from '~/Schema';
import {Validator} from '~/Validator';

export const GuildStickerController = (app: HonoApp) => {
	app.post(
		'/guilds/:guild_id/stickers',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_CREATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', GuildStickerCreateRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {name, description, tags, image} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').createSticker({user, guildId, name, description, tags, image}, auditLogReason),
			);
		},
	);

	app.post(
		'/guilds/:guild_id/stickers/bulk',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_BULK_CREATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', GuildStickerBulkCreateRequest),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {stickers} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').bulkCreateStickers({user, guildId, stickers}, auditLogReason));
		},
	);

	app.get(
		'/guilds/:guild_id/stickers',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKERS_LIST),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').getStickers({userId, guildId, requestCache}));
		},
	);

	app.patch(
		'/guilds/:guild_id/stickers/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, sticker_id: Int64Type})),
		Validator('json', GuildStickerUpdateRequest),
		async (ctx) => {
			const {guild_id, sticker_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const stickerId = createStickerID(sticker_id);
			const {name, description, tags} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.updateSticker({userId, guildId, stickerId, name, description, tags}, auditLogReason),
			);
		},
	);

	app.delete(
		'/guilds/:guild_id/stickers/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_STICKER_DELETE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, sticker_id: Int64Type})),
		Validator('query', z.object({purge: QueryBooleanType.optional()})),
		async (ctx) => {
			const {guild_id, sticker_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const stickerId = createStickerID(sticker_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {purge = false} = ctx.req.valid('query');
			await ctx.get('guildService').deleteSticker({userId, guildId, stickerId, purge}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
};
