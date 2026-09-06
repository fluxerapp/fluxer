// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnclaimedAccountCannotAddReactionsError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotAddReactionsError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	ChannelIdMessageIdEmojiParam,
	ChannelIdMessageIdEmojiTargetIdParam,
	ChannelIdMessageIdParam,
	ChannelIdParam,
	SessionIdQuerySchema,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	ChannelPinsQuerySchema,
	ReactionUsersQuerySchema,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {
	ChannelPinsResponse,
	ReactionUsersListResponse,
	ReactionUsersPageResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import {SYSTEM_USER_ID} from '../../constants/Core';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {isPersonalNotesChannel} from '../services/message/MessageHelpers';

export function MessageInteractionController(app: HonoApp) {
	app.get(
		'/channels/:channel_id/messages/pins',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ChannelPinsQuerySchema),
		OpenAPI({
			operationId: 'list_pinned_messages',
			summary: 'List pinned messages',
			description:
				'Retrieves a paginated list of messages pinned in a channel. User must have permission to view the channel. Supports pagination via limit and before parameters. Returns pinned messages with their pin timestamps.',
			responseSchema: ChannelPinsResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const {limit, before} = ctx.req.valid('query');
			return ctx.json(
				await ctx
					.get('channelService')
					.interactions.getChannelPins({userId, channelId, requestCache, limit, beforeTimestamp: before}),
			);
		},
	);
	app.post(
		'/channels/:channel_id/pins/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'acknowledge_pins',
			summary: 'Acknowledge new pin notifications',
			description:
				'Marks all new pin notifications in a channel as acknowledged. Clears the notification badge for pinned messages. Returns 204 No Content on success.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channel = await ctx.get('channelService').channelData.operations.getChannelSystem(channelId);
			const timestamp = channel?.lastPinTimestamp;
			if (timestamp != null) {
				await ctx.get('readStateService').ackPins({userId, channelId, timestamp});
			}
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/channels/:channel_id/pins/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_PINS),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'pin_message',
			summary: 'Pin a message',
			description:
				'Pins a message to the channel. Requires permission to manage pins (typically moderator or higher). Pinned messages are highlighted and searchable. Returns 204 No Content on success.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').interactions.pinMessage({
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
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'unpin_message',
			summary: 'Unpin a message',
			description:
				'Unpins a message from the channel. Requires permission to manage pins. The message remains in the channel but is no longer highlighted. Returns 204 No Content on success.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').interactions.unpinMessage({
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
		Validator('param', ChannelIdMessageIdEmojiParam),
		Validator('query', ReactionUsersQuerySchema),
		OpenAPI({
			operationId: 'list_reaction_users',
			summary: 'List users who reacted with emoji',
			description:
				'Retrieves a paginated list of users who reacted to a message with a specific emoji. Supports pagination via limit and after parameters. Returns user objects for each reaction.',
			responseSchema: ReactionUsersListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const {limit, after} = ctx.req.valid('query');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const afterUserId = after ? createUserID(after) : undefined;
			const result = await ctx
				.get('channelService')
				.interactions.getUsersForReaction({userId, channelId, messageId, emoji, limit, after: afterUserId});
			ctx.header('X-Has-More', result.has_more ? 'true' : 'false');
			if (result.next_after !== null) {
				ctx.header('X-Next-After', result.next_after);
			}
			return ctx.json(result.users, 200);
		},
	);
	app.get(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji/users',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator('param', ChannelIdMessageIdEmojiParam),
		Validator('query', ReactionUsersQuerySchema),
		OpenAPI({
			operationId: 'list_reaction_users_v2',
			summary: 'List users who reacted with emoji',
			description:
				'Retrieves a paginated list of users who reacted to a message with a specific emoji. The response body includes pagination metadata, including the next cursor, so clients do not need to infer it from returned users.',
			responseSchema: ReactionUsersPageResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const {limit, after} = ctx.req.valid('query');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const afterUserId = after ? createUserID(after) : undefined;
			const result = await ctx
				.get('channelService')
				.interactions.getUsersForReaction({userId, channelId, messageId, emoji, limit, after: afterUserId});
			return ctx.json(
				{
					items: result.users,
					has_more: result.has_more,
					next_after: result.next_after,
				},
				200,
			);
		},
	);
	app.put(
		'/channels/:channel_id/messages/:message_id/reactions/:emoji/@me',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator('param', ChannelIdMessageIdEmojiParam),
		Validator('query', SessionIdQuerySchema),
		OpenAPI({
			operationId: 'add_reaction',
			summary: 'Add reaction to message',
			description:
				'Adds an emoji reaction to a message. Each user can react once with each emoji. Cannot be used from unclaimed accounts outside personal notes. Returns 204 No Content on success.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const user = ctx.get('user');
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');
			if (user.isUnclaimedAccount() && !isPersonalNotesChannel({userId: user.id, channelId})) {
				throw new UnclaimedAccountCannotAddReactionsError();
			}
			if (!user.isBot && user.id !== SYSTEM_USER_ID && !(user.flags & UserFlags.HAS_SESSION_STARTED)) {
				throw InputValidationError.fromCode('emoji', ValidationErrorCodes.MUST_START_SESSION_BEFORE_SENDING);
			}
			await ctx.get('channelService').interactions.addReaction({
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
		Validator('param', ChannelIdMessageIdEmojiParam),
		Validator('query', SessionIdQuerySchema),
		OpenAPI({
			operationId: 'remove_own_reaction',
			summary: 'Remove own reaction from message',
			description:
				"Removes your own emoji reaction from a message. Returns 204 No Content on success. Has no effect if you haven't reacted with that emoji.",
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').interactions.removeOwnReaction({
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
		Validator('param', ChannelIdMessageIdEmojiTargetIdParam),
		Validator('query', SessionIdQuerySchema),
		OpenAPI({
			operationId: 'remove_reaction',
			summary: 'Remove reaction from message',
			description:
				"Removes a specific user's emoji reaction from a message. Requires moderator or higher permissions to remove reactions from other users. Returns 204 No Content on success.",
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji, target_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(target_id);
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const sessionId = ctx.req.valid('query').session_id;
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').interactions.removeReaction({
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
		Validator('param', ChannelIdMessageIdEmojiParam),
		OpenAPI({
			operationId: 'remove_all_reactions_for_emoji',
			summary: 'Remove all reactions with emoji',
			description:
				"Removes all emoji reactions of a specific type from a message. All users' reactions with that emoji are deleted. Requires moderator or higher permissions. Returns 204 No Content on success.",
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id, emoji} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			await ctx.get('channelService').interactions.removeAllReactionsForEmoji({userId, channelId, messageId, emoji});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/messages/:message_id/reactions',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_REACTIONS),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'remove_all_reactions',
			summary: 'Remove all reactions from message',
			description:
				'Removes all emoji reactions from a message, regardless of emoji type or user. All reactions are permanently deleted. Requires moderator or higher permissions. Returns 204 No Content on success.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			await ctx.get('channelService').interactions.removeAllReactions({userId, channelId, messageId});
			return ctx.body(null, 204);
		},
	);
}
