// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CreateThreadRequest,
	ListThreadsQuery,
	UpdateThreadRequest,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {ThreadMemberResponse, ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	ChannelIdParam,
	ChannelIdThreadIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {z} from 'zod';
import {createChannelID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {mapThreadToResponse} from '../services/ThreadService';

export function ThreadController(app: HonoApp) {
	app.get(
		'/users/@me/thread-members',
		RateLimitMiddleware(RateLimitConfigs.THREAD_GET),
		LoginRequired,
		OpenAPI({
			operationId: 'get_my_thread_memberships',
			summary: 'Get joined threads',
			description: 'Returns all threads the current user has joined, with last message preview.',
			responseSchema: z.array(ThreadResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const threads = await ctx.get('threadService').listJoinedThreads({userId});
			return ctx.json(threads);
		},
	);

	app.post(
		'/channels/:channel_id/threads',
		RateLimitMiddleware(RateLimitConfigs.THREAD_CREATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', CreateThreadRequest),
		OpenAPI({
			operationId: 'create_thread',
			summary: 'Create a thread',
			description:
				'Creates a new thread in a text channel. Requires the CREATE_THREADS permission. A thread is a sub-channel that inherits permissions from its parent.',
			requestSchema: CreateThreadRequest,
			responseSchema: ThreadResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const thread = await ctx.get('threadService').createThread({userId, channelId, data, requestCache});
			return ctx.json(mapThreadToResponse(thread));
		},
	);

	app.get(
		'/channels/:channel_id/threads',
		RateLimitMiddleware(RateLimitConfigs.THREAD_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ListThreadsQuery),
		OpenAPI({
			operationId: 'list_threads',
			summary: 'List threads in a channel',
			description:
				'Returns all threads in a channel. Optionally filter by state (open, closed, archived). Requires the user to have VIEW_CHANNEL permission on the parent channel.',
			responseSchema: z.array(ThreadResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {state, limit, before} = ctx.req.valid('query');
			const threads = await ctx.get('threadService').listThreads({
				userId,
				channelId,
				stateFilter: state,
				limit,
				before: before ? createChannelID(before) : undefined,
			});
			const enriched = await ctx.get('threadService').enrichThreadsWithLastMessage(threads);
			return ctx.json(enriched);
		},
	);

	app.get(
		'/channels/:channel_id/threads/:thread_id',
		RateLimitMiddleware(RateLimitConfigs.THREAD_GET),
		LoginRequired,
		Validator('param', ChannelIdThreadIdParam),
		OpenAPI({
			operationId: 'get_thread',
			summary: 'Get a thread',
			description: 'Returns a specific thread. Requires VIEW_CHANNEL on the parent channel.',
			responseSchema: ThreadResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, thread_id} = ctx.req.valid('param');
			const channelId = createChannelID(channel_id);
			const threadId = createChannelID(thread_id);
			const thread = await ctx.get('threadService').getThread({userId, channelId, threadId});
			return ctx.json(mapThreadToResponse(thread));
		},
	);

	app.patch(
		'/channels/:channel_id/threads/:thread_id',
		RateLimitMiddleware(RateLimitConfigs.THREAD_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdThreadIdParam),
		Validator('json', UpdateThreadRequest),
		OpenAPI({
			operationId: 'update_thread',
			summary: 'Update a thread',
			description:
				'Updates a thread name, state, or expiry. Changing name or state requires MANAGE_CHANNELS permission.',
			requestSchema: UpdateThreadRequest,
			responseSchema: ThreadResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, thread_id} = ctx.req.valid('param');
			const channelId = createChannelID(channel_id);
			const threadId = createChannelID(thread_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const thread = await ctx.get('threadService').updateThread({userId, channelId, threadId, data, requestCache});
			return ctx.json(mapThreadToResponse(thread));
		},
	);

	app.delete(
		'/channels/:channel_id/threads/:thread_id',
		RateLimitMiddleware(RateLimitConfigs.THREAD_DELETE),
		LoginRequired,
		Validator('param', ChannelIdThreadIdParam),
		OpenAPI({
			operationId: 'delete_thread',
			summary: 'Delete a thread',
			description: 'Permanently deletes a thread and all its messages. Requires MANAGE_CHANNELS permission.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, thread_id} = ctx.req.valid('param');
			const channelId = createChannelID(channel_id);
			const threadId = createChannelID(thread_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('threadService').deleteThread({userId, channelId, threadId, requestCache});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/channels/:channel_id/threads/:thread_id/members/@me',
		RateLimitMiddleware(RateLimitConfigs.THREAD_MEMBER),
		LoginRequired,
		Validator('param', ChannelIdThreadIdParam),
		OpenAPI({
			operationId: 'join_thread',
			summary: 'Join a thread',
			description:
				'Adds the current user to a thread. Thread must be open or closed (not archived). The thread will appear in the sidebar.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, thread_id} = ctx.req.valid('param');
			const channelId = createChannelID(channel_id);
			const threadId = createChannelID(thread_id);
			await ctx.get('threadService').joinThread({userId, channelId, threadId});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/threads/:thread_id/members/@me',
		RateLimitMiddleware(RateLimitConfigs.THREAD_MEMBER),
		LoginRequired,
		Validator('param', ChannelIdThreadIdParam),
		OpenAPI({
			operationId: 'leave_thread',
			summary: 'Leave a thread',
			description:
				'Removes the current user from a thread. Thread must be open or closed (not archived). The thread will no longer appear in the sidebar.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {channel_id, thread_id} = ctx.req.valid('param');
			const channelId = createChannelID(channel_id);
			const threadId = createChannelID(thread_id);
			await ctx.get('threadService').leaveThread({userId, channelId, threadId});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/channels/:channel_id/thread-members',
		RateLimitMiddleware(RateLimitConfigs.THREAD_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'list_thread_members',
			summary: 'List thread members',
			description: 'Returns the list of members who have joined a thread. Requires VIEW_CHANNEL on the parent channel.',
			responseSchema: z.array(ThreadMemberResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Threads',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const members = await ctx.get('threadService').listThreadMembers({
				userId,
				threadId: channelId,
			});
			return ctx.json(
				members.map((m) => ({
					thread_id: channelId.toString(),
					user_id: m.userId.toString(),
					joined_at: m.joinedAt.toISOString(),
					notification_override: null,
				})),
			);
		},
	);
}
