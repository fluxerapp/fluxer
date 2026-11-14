// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelThreadsRouteGuard} from '@app/api/channel/threads/ChannelThreadsRouteGuard';
import {viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	ChannelIdTagIdParam,
	ForumTagRequest,
	SearchIndexNotReadyResponse,
	ThreadPostDataRequest,
	ThreadPostDataResponse,
	ThreadSearchQuery,
	ThreadSearchResult,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import {ChannelIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import type {Context} from 'hono';

const EXPERIMENT = 'channel_threads';
const TAGS = 'Channels';

function tagEditParams(ctx: Context<HonoEnv>) {
	return {
		userId: ctx.get('user').id,
		viewer: viewerFromCtx(ctx),
		clientFeatures: ctx.get('clientFeatures'),
		requestCache: ctx.get('requestCache'),
		auditLogReason: ctx.get('auditLogReason') ?? null,
	};
}

export function ForumController(app: HonoApp) {
	app.get(
		'/channels/:channel_id/threads/search',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREADS_SEARCH),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', ThreadSearchQuery),
		OpenAPI({
			operationId: 'search_threads',
			summary: 'Search threads',
			description:
				'Returns threads of the channel that match the search. Requires the read message history permission. While the search index of the guild is being built, responds with 202 and a body that says when to retry.',
			responseSchema: ThreadSearchResult,
			acceptedResponseSchema: SearchIndexNotReadyResponse,
			statusCode: [200, 202],
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('threadService').forum.search({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					query: ctx.req.valid('query'),
				}),
			);
		},
	);
	app.post(
		'/channels/:channel_id/post-data',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_POST_DATA),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', ThreadPostDataRequest),
		OpenAPI({
			operationId: 'get_channel_post_data',
			summary: 'Get forum post data',
			description:
				'Returns the owner and first message of each requested post in a forum or media channel. Requires the read message history permission.',
			responseSchema: ThreadPostDataResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('threadService').forum.postData({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					threadIds: ctx.req.valid('json').thread_ids.map((id) => createChannelID(id)),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.post(
		'/channels/:channel_id/tags',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_FORUM_TAGS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', ForumTagRequest),
		OpenAPI({
			operationId: 'create_forum_tag',
			summary: 'Create a forum tag',
			description:
				'Adds a tag to a forum or media channel. Requires the manage channels permission. Returns the updated channel.',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			return ctx.json(
				await ctx.get('channelRequestService').editForumTags({
					...tagEditParams(ctx),
					channelId: createChannelID(ctx.req.valid('param').channel_id),
					edit: {kind: 'create', tag: ctx.req.valid('json')},
				}),
			);
		},
	);
	app.put(
		'/channels/:channel_id/tags/:tag_id',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_FORUM_TAGS),
		LoginRequired,
		Validator('param', ChannelIdTagIdParam),
		Validator('json', ForumTagRequest),
		OpenAPI({
			operationId: 'update_forum_tag',
			summary: 'Update a forum tag',
			description:
				'Replaces a tag of a forum or media channel. Requires the manage channels permission. Returns the updated channel.',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, tag_id} = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('channelRequestService').editForumTags({
					...tagEditParams(ctx),
					channelId: createChannelID(channel_id),
					edit: {kind: 'update', tagId: tag_id, tag: ctx.req.valid('json')},
				}),
			);
		},
	);
	app.delete(
		'/channels/:channel_id/tags/:tag_id',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_FORUM_TAGS),
		LoginRequired,
		Validator('param', ChannelIdTagIdParam),
		OpenAPI({
			operationId: 'delete_forum_tag',
			summary: 'Delete a forum tag',
			description:
				'Removes a tag from a forum or media channel. Requires the manage channels permission. Returns the updated channel.',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'sessionToken'],
			tags: TAGS,
			experiment: EXPERIMENT,
		}),
		async (ctx) => {
			const {channel_id, tag_id} = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('channelRequestService').editForumTags({
					...tagEditParams(ctx),
					channelId: createChannelID(channel_id),
					edit: {kind: 'delete', tagId: tag_id},
				}),
			);
		},
	);
}
