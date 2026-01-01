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
import {createChannelID, createMessageID, createUserID} from '~/BrandedTypes';
import {isPersonalNotesChannel} from '~/channel/services/message/MessageHelpers';
import {UnclaimedAccountRestrictedError} from '~/Errors';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';
export const MessageInteractionController = (app: HonoApp) => {
	app.get(
		'/channels/:channel_id/messages/pins',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator(
			'query',
			z.object({
				limit: z.coerce.number().int().min(1).max(50).optional(),
				before: z.coerce.date().optional(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const {limit, before} = ctx.req.valid('query');
			return ctx.json(
				await ctx
					.get('channelService')
					.getChannelPins({userId, channelId, requestCache, limit, beforeTimestamp: before}),
			);
		},
	);

	app.post(
		'/channels/:channel_id/pins/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {channel} = await ctx.get('channelService').getChannelAuthenticated({userId, channelId});
			const timestamp = channel.lastPinTimestamp;
			if (timestamp != null) {
				await ctx.get('channelService').ackPins({userId, channelId, timestamp});
			}
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/channels/:channel_id/pins/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').pinMessage({
				userId,
				channelId,
				messageId,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/pins/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').unpinMessage({
				userId,
				channelId,
				messageId,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator(
			'param',
			z.object({
				channel_id: Int64Type,
				message_id: Int64Type,
				emoji: createStringType(1, 64),
			}),
		),
		Validator(
			'query',
			z.object({
				limit: z.coerce.number().int().min(1).max(100).optional(),
				after: Int64Type.optional(),
			}),
		),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const {limit, after} = ctx.req.valid('query');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const afterUserId = after ? createUserID(after) : undefined;
			return ctx.json(
				await ctx
					.get('channelService')
					.getUsersForReaction({userId, channelId, messageId, emoji, limit, after: afterUserId}),
			);
		},
	);

	app.put(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji/@me',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator(
			'param',
			z.object({
				channel_id: Int64Type,
				message_id: Int64Type,
				emoji: createStringType(1, 64),
			}),
		),
		Validator('query', z.object({session_id: z.optional(createStringType(1, 64))})),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const user = ctx.get('user');
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');

			if (!user.passwordHash && !isPersonalNotesChannel({userId: user.id, channelId})) {
				throw new UnclaimedAccountRestrictedError('add reactions');
			}

			await ctx.get('channelService').addReaction({
				userId: user.id,
				sessionId,
				channelId,
				messageId,
				emoji,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji/@me',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator(
			'param',
			z.object({
				channel_id: Int64Type,
				message_id: Int64Type,
				emoji: createStringType(1, 64),
			}),
		),
		Validator('query', z.object({session_id: z.optional(createStringType(1, 64))})),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').removeOwnReaction({
				userId,
				sessionId,
				channelId,
				messageId,
				emoji,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji/:target_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator(
			'param',
			z.object({
				channel_id: Int64Type,
				message_id: Int64Type,
				emoji: createStringType(1, 64),
				target_id: Int64Type,
			}),
		),
		Validator('query', z.object({session_id: z.optional(createStringType(1, 64))})),
		async (ctx) => {
			const {channel_id, message_id, emoji, target_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(target_id);
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').removeReaction({
				userId,
				sessionId,
				channelId,
				messageId,
				emoji,
				targetId,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator(
			'param',
			z.object({
				channel_id: Int64Type,
				message_id: Int64Type,
				emoji: createStringType(1, 64),
			}),
		),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			await ctx.get('channelService').removeAllReactionsForEmoji({userId, channelId, messageId, emoji});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id/reactions',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			await ctx.get('channelService').removeAllReactions({userId, channelId, messageId});
			return ctx.body(null, 204);
		},
	);
};
