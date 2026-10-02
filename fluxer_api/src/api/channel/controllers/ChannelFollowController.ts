// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {Validator} from '@app/api/Validator';
import {
	ChannelFollowerStatsResponse,
	ChannelFollowRequest,
	FollowedChannelResponse,
} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';
import {ChannelIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function ChannelFollowController(app: HonoApp) {
	app.post(
		'/channels/:channel_id/followers',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_FOLLOW),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', ChannelFollowRequest),
		OpenAPI({
			operationId: 'follow_channel',
			summary: 'Follow an announcement channel',
			description:
				'Follows an announcement channel into a text channel. Creates a channel follower webhook in the target channel that receives every message published in the announcement channel. Requires Manage Webhooks in the target channel and View Channel on the announcement channel.',
			requestSchema: ChannelFollowRequest,
			responseSchema: FollowedChannelResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			assertAccountNotLimited(ctx.get('user'));
			const followed = await ctx.get('channelFollowService').followChannel({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
				webhookChannelId: createChannelID(ctx.req.valid('json').webhook_channel_id),
				requestCache: ctx.get('requestCache'),
				auditLogReason: ctx.get('auditLogReason') ?? null,
			});
			return ctx.json({
				channel_id: followed.channelId.toString(),
				webhook_id: followed.webhookId.toString(),
			} satisfies FollowedChannelResponse);
		},
	);
	app.get(
		'/channels/:channel_id/follower-stats',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_FOLLOWER_STATS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'get_channel_follower_stats',
			summary: 'Get announcement channel follower stats',
			description:
				'Returns how many channels and distinct guilds follow an announcement channel. Requires View Channel on the announcement channel.',
			responseSchema: ChannelFollowerStatsResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const stats = await ctx.get('channelFollowService').getFollowerStats({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
			});
			return ctx.json(stats);
		},
	);
}
