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
import {createUserID, type UserID} from '~/BrandedTypes';
import {RelationshipTypes} from '~/Constants';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import {
	FriendRequestByTagRequest,
	mapRelationshipToResponse,
	RelationshipNicknameUpdateRequest,
} from '~/user/UserModel';
import {Validator} from '~/Validator';

const createUserPartialResolver =
	(userCacheService: UserCacheService, requestCache: RequestCache) => (userId: UserID) =>
		getCachedUserPartialResponse({userId, userCacheService, requestCache});

export const UserRelationshipController = (app: HonoApp) => {
	app.get(
		'/users/@me/relationships',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIPS_LIST),
		LoginRequired,
		DefaultUserOnly,
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const relationships = await ctx.get('userService').getRelationships(ctx.get('user').id);
			const responses = await Promise.all(
				relationships.map((relationship) => mapRelationshipToResponse({relationship, userPartialResolver})),
			);
			return ctx.json(responses);
		},
	);

	app.post(
		'/users/@me/relationships',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_SEND),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', FriendRequestByTagRequest),
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const relationship = await ctx.get('userService').sendFriendRequestByTag({
				userId: ctx.get('user').id,
				data: ctx.req.valid('json'),
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(await mapRelationshipToResponse({relationship, userPartialResolver}));
		},
	);

	app.post(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_SEND),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({user_id: Int64Type})),
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const relationship = await ctx.get('userService').sendFriendRequest({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(await mapRelationshipToResponse({relationship, userPartialResolver}));
		},
	);

	app.put(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_FRIEND_REQUEST_ACCEPT),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({user_id: Int64Type})),
		Validator('json', z.object({type: z.number().optional()}).optional()),
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const body = ctx.req.valid('json');
			const targetId = createUserID(ctx.req.valid('param').user_id);
			if (body?.type === RelationshipTypes.BLOCKED) {
				const relationship = await ctx.get('userService').blockUser({
					userId: ctx.get('user').id,
					targetId,
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				});
				return ctx.json(await mapRelationshipToResponse({relationship, userPartialResolver}));
			} else {
				const relationship = await ctx.get('userService').acceptFriendRequest({
					userId: ctx.get('user').id,
					targetId,
					userCacheService: ctx.get('userCacheService'),
					requestCache: ctx.get('requestCache'),
				});
				return ctx.json(await mapRelationshipToResponse({relationship, userPartialResolver}));
			}
		},
	);

	app.delete(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIP_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({user_id: Int64Type})),
		async (ctx) => {
			await ctx.get('userService').removeRelationship({
				userId: ctx.get('user').id,
				targetId: createUserID(ctx.req.valid('param').user_id),
			});
			return ctx.body(null, 204);
		},
	);

	app.patch(
		'/users/@me/relationships/:user_id',
		RateLimitMiddleware(RateLimitConfigs.USER_RELATIONSHIP_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({user_id: Int64Type})),
		Validator('json', RelationshipNicknameUpdateRequest),
		async (ctx) => {
			const userPartialResolver = createUserPartialResolver(ctx.get('userCacheService'), ctx.get('requestCache'));
			const targetId = createUserID(ctx.req.valid('param').user_id);
			const requestBody = ctx.req.valid('json');
			const relationship = await ctx.get('userService').updateFriendNickname({
				userId: ctx.get('user').id,
				targetId,
				nickname: requestBody.nickname ?? null,
				userCacheService: ctx.get('userCacheService'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(await mapRelationshipToResponse({relationship, userPartialResolver}));
		},
	);
};
