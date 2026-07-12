// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ThreadCreateRequest,
	ThreadListArchivedQuery,
	ThreadSearchQuery,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {
	ChannelResponse,
	ThreadListResponse,
	ThreadMemberResponse,
} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	ChannelIdMessageIdParam,
	ChannelIdParam,
	ChannelIdUserIdParam,
	GuildIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {z} from 'zod';
import {createChannelID, createGuildID, createMessageID, createUserID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import type {ThreadListResult} from '../services/ThreadService';

function toThreadListResponse(result: ThreadListResult): ThreadListResponse {
	return {
		threads: result.threads,
		members: result.members,
		has_more: result.hasMore,
	};
}

export function ThreadController(app: HonoApp) {
	app.post(
		'/channels/:channel_id/messages/:message_id/threads',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		Validator('json', ThreadCreateRequest),
		OpenAPI({
			operationId: 'create_thread_from_message',
			summary: 'Start a thread from a message',
			description:
				'Creates a public thread from an existing message. The thread shares the ID of the starter message, inherits the permissions of the parent channel, and the creator is added as the first member. Requires the CREATE_PUBLIC_THREADS permission in the parent channel.',
			requestSchema: ThreadCreateRequest,
			responseSchema: ChannelResponse,
			statusCode: 201,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, message_id} = ctx.req.valid('param');
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const response = await threadService.createThreadFromMessage({
				userId,
				channelId: createChannelID(channel_id),
				messageId: createMessageID(message_id),
				data,
				requestCache,
			});
			return ctx.json(response, 201);
		},
	);
	app.post(
		'/channels/:channel_id/threads',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', ThreadCreateRequest),
		OpenAPI({
			operationId: 'create_thread',
			summary: 'Start a thread without a message',
			description:
				'Creates a public thread that is not attached to an existing message. The thread inherits the permissions of the parent channel and the creator is added as the first member. Requires the CREATE_PUBLIC_THREADS permission in the parent channel.',
			requestSchema: ThreadCreateRequest,
			responseSchema: ChannelResponse,
			statusCode: 201,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const response = await threadService.createThread({userId, channelId, data, requestCache});
			return ctx.json(response, 201);
		},
	);
	app.get(
		'/guilds/:guild_id/threads/active',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_active_guild_threads',
			summary: 'List active guild threads',
			description:
				'Returns all active (non-archived) threads in the guild that the calling user can view, along with the thread member objects of the calling user for threads they have joined.',
			responseSchema: ThreadListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const result = await threadService.listActiveGuildThreads({userId, guildId, requestCache});
			return ctx.json(toThreadListResponse(result));
		},
	);
	app.get(
		'/channels/:channel_id/threads/active',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'list_active_threads',
			summary: 'List active threads',
			description:
				'Returns the active (non-archived) threads in the channel, along with the thread member objects of the calling user for threads they have joined.',
			responseSchema: ThreadListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const result = await threadService.listActiveThreads({userId, channelId, requestCache});
			return ctx.json(toThreadListResponse(result));
		},
	);
	app.get(
		'/channels/:channel_id/threads/archived/public',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadListArchivedQuery),
		OpenAPI({
			operationId: 'list_archived_threads',
			summary: 'List archived threads',
			description:
				'Returns archived threads in the channel ordered by archive timestamp descending. Supports pagination with the `before` query parameter.',
			responseSchema: ThreadListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {before, limit} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const result = await threadService.listArchivedThreads({userId, channelId, before, limit, requestCache});
			return ctx.json(toThreadListResponse(result));
		},
	);
	app.get(
		'/channels/:channel_id/threads/search',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadSearchQuery),
		OpenAPI({
			operationId: 'search_threads',
			summary: 'Search threads',
			description:
				'Returns threads in the channel whose name contains the query string, ordered by most recent activity. Archived threads are included when `archived` is true.',
			responseSchema: ThreadListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {q, archived, limit} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			const threadService = ctx.get('channelService').threads;
			const result = await threadService.searchThreads({
				userId,
				channelId,
				query: q,
				includeArchived: archived,
				limit,
				requestCache,
			});
			return ctx.json(toThreadListResponse(result));
		},
	);
	app.get(
		'/channels/:channel_id/thread-members',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'list_thread_members',
			summary: 'List thread members',
			description: 'Returns the members of the thread. The channel must be a thread the calling user can view.',
			responseSchema: z.array(ThreadMemberResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const threadId = createChannelID(ctx.req.valid('param').channel_id);
			const threadService = ctx.get('channelService').threads;
			return ctx.json(await threadService.listThreadMembers({userId, threadId}));
		},
	);
	app.put(
		'/channels/:channel_id/thread-members/@me',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'join_thread',
			summary: 'Join a thread',
			description: 'Adds the calling user to the thread. The thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const threadId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').threads.joinThread({userId, threadId, requestCache});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/thread-members/@me',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'leave_thread',
			summary: 'Leave a thread',
			description: 'Removes the calling user from the thread.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const threadId = createChannelID(ctx.req.valid('param').channel_id);
			await ctx.get('channelService').threads.leaveThread({userId, threadId});
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/channels/:channel_id/thread-members/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdUserIdParam),
		OpenAPI({
			operationId: 'add_thread_member',
			summary: 'Add a member to a thread',
			description: 'Adds another user to the thread. The thread must not be archived.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, user_id} = ctx.req.valid('param');
			await ctx.get('channelService').threads.addThreadMember({
				userId,
				threadId: createChannelID(channel_id),
				targetId: createUserID(user_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/thread-members/:user_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdUserIdParam),
		OpenAPI({
			operationId: 'remove_thread_member',
			summary: 'Remove a member from a thread',
			description: 'Removes another user from the thread. Requires the MANAGE_THREADS permission.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, user_id} = ctx.req.valid('param');
			await ctx.get('channelService').threads.removeThreadMember({
				userId,
				threadId: createChannelID(channel_id),
				targetId: createUserID(user_id),
			});
			return ctx.body(null, 204);
		},
	);
}
