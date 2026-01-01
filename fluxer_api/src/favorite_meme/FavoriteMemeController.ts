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
import {createChannelID, createMemeID, createMessageID} from '~/BrandedTypes';
import {UnknownFavoriteMemeError} from '~/Errors';
import {
	CreateFavoriteMemeBodySchema,
	CreateFavoriteMemeFromUrlBodySchema,
	mapFavoriteMemeToResponse,
	UpdateFavoriteMemeBodySchema,
} from '~/favorite_meme/FavoriteMemeModel';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

const channelIdParamSchema = z.object({channel_id: Int64Type});
const messageIdParamSchema = z.object({message_id: Int64Type});
const memeIdParamSchema = z.object({meme_id: Int64Type});

export const FavoriteMemeController = (app: HonoApp) => {
	app.get(
		'/users/@me/memes',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_LIST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const user = ctx.get('user');
			const memes = await ctx.get('favoriteMemeService').listFavoriteMemes(user.id);
			return ctx.json(memes.map((meme) => mapFavoriteMemeToResponse(meme)));
		},
	);

	app.post(
		'/users/@me/memes',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_CREATE_FROM_URL),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', CreateFavoriteMemeFromUrlBodySchema),
		async (ctx) => {
			const user = ctx.get('user');
			const {url, name, alt_text, tags, tenor_id} = ctx.req.valid('json');

			const meme = await ctx.get('favoriteMemeService').createFromUrl({
				user,
				url,
				name,
				altText: alt_text ?? undefined,
				tags: tags ?? undefined,
				tenorId: tenor_id ?? undefined,
			});

			return ctx.json(mapFavoriteMemeToResponse(meme), 201);
		},
	);

	app.post(
		'/channels/:channel_id/messages/:message_id/memes',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_CREATE_FROM_MESSAGE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', channelIdParamSchema.merge(messageIdParamSchema)),
		Validator('json', CreateFavoriteMemeBodySchema),
		async (ctx) => {
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const messageId = createMessageID(ctx.req.valid('param').message_id);
			const {attachment_id, embed_index, name, alt_text, tags} = ctx.req.valid('json');

			const meme = await ctx.get('favoriteMemeService').createFromMessage({
				user,
				channelId,
				messageId,
				attachmentId: attachment_id?.toString(),
				embedIndex: embed_index ?? undefined,
				name,
				altText: alt_text ?? undefined,
				tags: tags ?? undefined,
			});

			return ctx.json(mapFavoriteMemeToResponse(meme), 201);
		},
	);

	app.get(
		'/users/@me/memes/:meme_id',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', memeIdParamSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const memeId = createMemeID(ctx.req.valid('param').meme_id);
			const meme = await ctx.get('favoriteMemeService').getFavoriteMeme(user.id, memeId);
			if (!meme) {
				throw new UnknownFavoriteMemeError();
			}
			return ctx.json(mapFavoriteMemeToResponse(meme));
		},
	);

	app.patch(
		'/users/@me/memes/:meme_id',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', memeIdParamSchema),
		Validator('json', UpdateFavoriteMemeBodySchema),
		async (ctx) => {
			const user = ctx.get('user');
			const memeId = createMemeID(ctx.req.valid('param').meme_id);
			const {name, alt_text, tags} = ctx.req.valid('json');
			const meme = await ctx.get('favoriteMemeService').update({
				user,
				memeId,
				name: name ?? undefined,
				altText: alt_text === undefined ? undefined : alt_text,
				tags: tags ?? undefined,
			});
			return ctx.json(mapFavoriteMemeToResponse(meme));
		},
	);

	app.delete(
		'/users/@me/memes/:meme_id',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_MEME_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', memeIdParamSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const memeId = createMemeID(ctx.req.valid('param').meme_id);
			await ctx.get('favoriteMemeService').delete(user.id, memeId);
			return ctx.body(null, 204);
		},
	);
};
