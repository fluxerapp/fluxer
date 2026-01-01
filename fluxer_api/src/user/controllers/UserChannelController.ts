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
import {createChannelID} from '~/BrandedTypes';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {CreatePrivateChannelRequest} from '~/user/UserModel';
import {Validator} from '~/Validator';

export const UserChannelController = (app: HonoApp) => {
	app.get(
		'/users/@me/channels',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channels = await ctx.get('userService').getPrivateChannels(userId);
			const responses = await Promise.all(
				channels.map((channel) =>
					mapChannelToResponse({
						channel,
						currentUserId: userId,
						userCacheService: ctx.get('userCacheService'),
						requestCache: ctx.get('requestCache'),
					}),
				),
			);
			return ctx.json(responses);
		},
	);

	app.post(
		'/users/@me/channels',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', CreatePrivateChannelRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channel = await ctx.get('userService').createOrOpenDMChannel({
				userId,
				data: ctx.req.valid('json'),
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(
				await mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);

	app.put(
		'/users/@me/channels/:channel_id/pin',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			await ctx.get('userService').pinDmChannel({
				userId,
				channelId,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/users/@me/channels/:channel_id/pin',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			await ctx.get('userService').unpinDmChannel({
				userId,
				channelId,
			});
			return ctx.body(null, 204);
		},
	);
};
