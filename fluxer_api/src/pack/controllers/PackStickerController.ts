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

export const PackStickerController = (app: HonoApp) => {
	app.post(
		'/packs/stickers/:pack_id',
		RateLimitMiddleware(RateLimitConfigs.PACKS_STICKER_CREATE),
		LoginRequired,
		Validator('param', z.object({pack_id: Int64Type})),
		Validator('json', GuildStickerCreateRequest),
		async (ctx) => {
			const packId = createGuildID(ctx.req.valid('param').pack_id);
			const {name, description, tags, image} = ctx.req.valid('json');
			const user = ctx.get('user');
			return ctx.json(await ctx.get('packService').createPackSticker({user, packId, name, description, tags, image}));
		},
	);

	app.post(
		'/packs/stickers/:pack_id/bulk',
		RateLimitMiddleware(RateLimitConfigs.PACKS_STICKER_BULK_CREATE),
		LoginRequired,
		Validator('param', z.object({pack_id: Int64Type})),
		Validator('json', GuildStickerBulkCreateRequest),
		async (ctx) => {
			const packId = createGuildID(ctx.req.valid('param').pack_id);
			const {stickers} = ctx.req.valid('json');
			const user = ctx.get('user');
			return ctx.json(await ctx.get('packService').bulkCreatePackStickers({user, packId, stickers}));
		},
	);

	app.get(
		'/packs/stickers/:pack_id',
		RateLimitMiddleware(RateLimitConfigs.PACKS_STICKERS_LIST),
		LoginRequired,
		Validator('param', z.object({pack_id: Int64Type})),
		async (ctx) => {
			const packId = createGuildID(ctx.req.valid('param').pack_id);
			const userId = ctx.get('user').id;
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('packService').getPackStickers({userId, packId, requestCache}));
		},
	);

	app.patch(
		'/packs/stickers/:pack_id/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.PACKS_STICKER_UPDATE),
		LoginRequired,
		Validator('param', z.object({pack_id: Int64Type, sticker_id: Int64Type})),
		Validator('json', GuildStickerUpdateRequest),
		async (ctx) => {
			const {pack_id, sticker_id} = ctx.req.valid('param');
			const packId = createGuildID(pack_id);
			const stickerId = createStickerID(sticker_id);
			const {name, description, tags} = ctx.req.valid('json');
			return ctx.json(
				await ctx
					.get('packService')
					.updatePackSticker({userId: ctx.get('user').id, packId, stickerId, name, description, tags}),
			);
		},
	);

	app.delete(
		'/packs/stickers/:pack_id/:sticker_id',
		RateLimitMiddleware(RateLimitConfigs.PACKS_STICKER_DELETE),
		LoginRequired,
		Validator('param', z.object({pack_id: Int64Type, sticker_id: Int64Type})),
		Validator('query', z.object({purge: QueryBooleanType.optional()})),
		async (ctx) => {
			const {pack_id, sticker_id} = ctx.req.valid('param');
			const packId = createGuildID(pack_id);
			const stickerId = createStickerID(sticker_id);
			const {purge = false} = ctx.req.valid('query');
			await ctx.get('packService').deletePackSticker({
				userId: ctx.get('user').id,
				packId,
				stickerId,
				purge,
			});
			return ctx.body(null, 204);
		},
	);
};
