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
import {createChannelID, createMessageID, type UserID} from '~/BrandedTypes';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createQueryIntegerType, createStringType, Int64Type, QueryBooleanType, z} from '~/Schema';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import {mapBetaCodeToResponse} from '~/user/UserModel';
import type {SavedMessageEntryResponse} from '~/user/UserTypes';
import {Validator} from '~/Validator';

const createUserPartialResolver =
	(userCacheService: UserCacheService, requestCache: RequestCache) => (userId: UserID) =>
		getCachedUserPartialResponse({userId, userCacheService, requestCache});

export const UserContentController = (app: HonoApp) => {
	app.get(
		'/users/@me/beta-codes',
		RateLimitMiddleware(RateLimitConfigs.USER_BETA_CODES_READ),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			const userService = ctx.get('userService');
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));

			const [betaCodes, allowanceInfo] = await Promise.all([
				userService.getBetaCodes(userId),
				userService.getBetaCodeAllowanceInfo(userId),
			]);

			const responses = await Promise.all(
				betaCodes.map((betaCode) => mapBetaCodeToResponse({betaCode, userPartialResolver})),
			);

			return ctx.json({
				beta_codes: responses,
				allowance: allowanceInfo.allowance,
				next_reset_at: allowanceInfo.nextResetAt?.toISOString() ?? null,
			});
		},
	);

	app.post(
		'/users/@me/beta-codes',
		RateLimitMiddleware(RateLimitConfigs.USER_BETA_CODES_CREATE),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const betaCode = await ctx.get('userService').createBetaCode(ctx.get('user').id);
			return ctx.json(await mapBetaCodeToResponse({betaCode, userPartialResolver}));
		},
	);

	app.delete(
		'/users/@me/beta-codes/:code',
		RateLimitMiddleware(RateLimitConfigs.USER_BETA_CODES_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({code: createStringType()})),
		async (ctx) => {
			await ctx.get('userService').deleteBetaCode({
				userId: ctx.get('user').id,
				code: ctx.req.valid('param').code,
			});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/@me/mentions',
		RateLimitMiddleware(RateLimitConfigs.USER_MENTIONS_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator(
			'query',
			z.object({
				limit: createQueryIntegerType({minValue: 1, maxValue: 100, defaultValue: 25}),
				roles: QueryBooleanType.optional().default(true),
				everyone: QueryBooleanType.optional().default(true),
				guilds: QueryBooleanType.optional().default(true),
				before: Int64Type.optional(),
			}),
		),
		async (ctx) => {
			const {limit, roles, everyone, guilds, before} = ctx.req.valid('query');
			const userId = ctx.get('user').id;
			const messages = await ctx.get('userService').getRecentMentions({
				userId,
				limit,
				everyone,
				roles,
				guilds,
				before: before ? createMessageID(before) : undefined,
			});
			const responses = await Promise.all(
				messages.map((message) =>
					mapMessageToResponse({
						message,
						currentUserId: userId,
						userCacheService: ctx.get('userCacheService'),
						requestCache: ctx.get('requestCache'),
						mediaService: ctx.get('mediaService'),
					}),
				),
			);
			return ctx.json(responses);
		},
	);

	app.delete(
		'/users/@me/mentions/:message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_MENTIONS_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({message_id: Int64Type})),
		async (ctx) => {
			await ctx.get('userService').deleteRecentMention({
				userId: ctx.get('user').id,
				messageId: createMessageID(ctx.req.valid('param').message_id),
			});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/users/@me/saved-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('query', z.object({limit: createQueryIntegerType({minValue: 1, maxValue: 100, defaultValue: 25})})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const entries = await ctx.get('userService').getSavedMessages({
				userId,
				limit: ctx.req.valid('query').limit,
			});
			const responses = await Promise.all(
				entries.map(async (entry) => {
					const response: SavedMessageEntryResponse = {
						id: entry.messageId.toString(),
						channel_id: entry.channelId.toString(),
						message_id: entry.messageId.toString(),
						status: entry.status,
						message: entry.message
							? await mapMessageToResponse({
									message: entry.message,
									currentUserId: userId,
									userCacheService: ctx.get('userCacheService'),
									requestCache: ctx.get('requestCache'),
									mediaService: ctx.get('mediaService'),
								})
							: null,
					};
					return response;
				}),
			);
			return ctx.json(responses, 200);
		},
	);

	app.post(
		'/users/@me/saved-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('json');
			await ctx.get('userService').saveMessage({
				userId: ctx.get('user').id,
				channelId: createChannelID(channel_id),
				messageId: createMessageID(message_id),
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/users/@me/saved-messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({message_id: Int64Type})),
		async (ctx) => {
			await ctx.get('userService').unsaveMessage({
				userId: ctx.get('user').id,
				messageId: createMessageID(ctx.req.valid('param').message_id),
			});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/users/@me/harvest',
		RateLimitMiddleware(RateLimitConfigs.USER_DATA_HARVEST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const result = await ctx.get('userService').requestDataHarvest(ctx.get('user').id);
			return ctx.json(result, 200);
		},
	);

	app.get(
		'/users/@me/harvest/latest',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_LATEST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const harvest = await ctx.get('userService').getLatestHarvest(ctx.get('user').id);
			return ctx.json(harvest, 200);
		},
	);

	app.get(
		'/users/@me/harvest/:harvestId',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_STATUS),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const harvestId = BigInt(ctx.req.param('harvestId'));
			const harvest = await ctx.get('userService').getHarvestStatus(ctx.get('user').id, harvestId);
			return ctx.json(harvest, 200);
		},
	);

	app.get(
		'/users/@me/harvest/:harvestId/download',
		RateLimitMiddleware(RateLimitConfigs.USER_HARVEST_DOWNLOAD),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const harvestId = BigInt(ctx.req.param('harvestId'));
			const result = await ctx
				.get('userService')
				.getHarvestDownloadUrl(ctx.get('user').id, harvestId, ctx.get('storageService'));
			return ctx.json(result, 200);
		},
	);
};
