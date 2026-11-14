// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelThreadsRouteGuard} from '@app/api/channel/threads/ChannelThreadsRouteGuard';
import {viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {ChannelIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {ThreadMemberSettingsRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';

export function ThreadMemberSettingsController(app: HonoApp) {
	app.patch(
		'/channels/:channel_id/thread-members/@me/settings',
		ChannelThreadsRouteGuard(),
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_THREAD_MEMBER_SETTINGS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		Validator('json', ThreadMemberSettingsRequest),
		OpenAPI({
			operationId: 'update_thread_member_settings',
			summary: 'Update thread settings',
			description:
				"Updates the current user's notification settings for a thread they are a member of. Returns the thread member, or 204 when nothing changed.",
			responseSchema: ThreadMemberResponse,
			statusCode: [200, 204],
			bodylessStatusCodes: [204],
			security: ['sessionToken'],
			tags: 'Channels',
			experiment: 'channel_threads',
		}),
		async (ctx) => {
			const member = await ctx.get('threadService').memberSettings.update({
				viewer: viewerFromCtx(ctx),
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
				data: ctx.req.valid('json'),
			});
			return member ? ctx.json(member, 200) : ctx.body(null, 204);
		},
	);
}
