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

import type {Context} from 'hono';
import type {HonoApp, HonoEnv} from '~/App';
import {createChannelID, createUserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import {ChannelUpdateRequest, mapChannelToResponse} from '~/channel/ChannelModel';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, QueryBooleanType, z} from '~/Schema';
import {Validator} from '~/Validator';

export const ChannelController = (app: HonoApp) => {
	app.get(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const channel = await ctx.get('channelService').getChannel({userId, channelId});
			return ctx.json(
				await mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: ctx.get('userCacheService'),
					requestCache,
				}),
			);
		},
	);

	app.get(
		'/channels/:channel_id/rtc-regions',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);

			const regions = await ctx.get('channelService').getAvailableRtcRegions({
				userId,
				channelId,
			});

			return ctx.json(
				regions.map((region) => ({
					id: region.id,
					name: region.name,
					emoji: region.emoji,
				})),
			);
		},
	);

	app.patch(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', ChannelUpdateRequest, {
			pre: async (raw: unknown, ctx: Context<HonoEnv>) => {
				const userId = ctx.get('user').id;
				// @ts-expect-error not well typed
				const channelId = createChannelID(ctx.req.valid('param').channel_id);
				const existing = await ctx.get('channelService').getChannel({
					userId,
					channelId,
				});
				const body = (typeof raw === 'object' && raw !== null ? raw : {}) as Record<string, unknown>;
				return {...body, type: existing.type};
			},
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const channel = await ctx.get('channelService').editChannel({userId, channelId, data, requestCache});
			return ctx.json(
				await mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: ctx.get('userCacheService'),
					requestCache,
				}),
			);
		},
	);

	app.delete(
		'/channels/:channel_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_DELETE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('query', z.object({silent: QueryBooleanType})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {silent} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');

			const channel = await ctx.get('channelService').getChannel({userId, channelId});
			if (channel.type === ChannelTypes.GROUP_DM) {
				await ctx.get('channelService').removeRecipientFromChannel({
					userId,
					channelId,
					recipientId: userId,
					requestCache,
					silent,
				});
			} else {
				await ctx.get('channelService').deleteChannel({userId, channelId, requestCache});
			}

			return ctx.body(null, 204);
		},
	);

	app.put(
		'/channels/:channel_id/recipients/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, user_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const recipientId = createUserID(ctx.req.valid('param').user_id);
			const requestCache = ctx.get('requestCache');

			await ctx.get('channelService').groupDms.addRecipientToChannel({
				userId,
				channelId,
				recipientId,
				requestCache,
			});

			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/recipients/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, user_id: Int64Type})),
		Validator('query', z.object({silent: QueryBooleanType})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const recipientId = createUserID(ctx.req.valid('param').user_id);
			const {silent} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			await ctx
				.get('channelService')
				.removeRecipientFromChannel({userId, channelId, recipientId, requestCache, silent});
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/channels/:channel_id/permissions/:overwrite_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, overwrite_id: Int64Type})),
		Validator(
			'json',
			z.object({
				type: z.union([z.literal(0), z.literal(1)]),
				allow: Int64Type.nullish(),
				deny: Int64Type.nullish(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const overwriteId = ctx.req.valid('param').overwrite_id;
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');

			await ctx.get('channelService').setChannelPermissionOverwrite({
				userId,
				channelId,
				overwriteId,
				overwrite: {
					type: data.type,
					allow_: data.allow ? data.allow : 0n,
					deny_: data.deny ? data.deny : 0n,
				},
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/permissions/:overwrite_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, overwrite_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const overwriteId = ctx.req.valid('param').overwrite_id;
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').deleteChannelPermissionOverwrite({userId, channelId, overwriteId, requestCache});
			return ctx.body(null, 204);
		},
	);
};
