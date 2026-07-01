// SPDX-License-Identifier: AGPL-3.0-or-later

import {DirectMessagesDisabledError} from '@fluxer/errors/src/domains/channel/DirectMessagesDisabledError';
import {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {ChannelIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {CreatePrivateChannelRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {z} from 'zod';
import {createChannelID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {GroupDmCreateProtectionMiddleware} from '../../middleware/GroupDmProtectionMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function UserChannelController(app: HonoApp) {
	app.get(
		'/users/@me/channels',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		OpenAPI({
			operationId: 'list_private_channels',
			summary: 'List private channels',
			responseSchema: z.array(ChannelResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves all private channels (direct messages) accessible to the current user. Returns list of channel objects with metadata including recipient information.',
		}),
		async (ctx) => {
			const response = await ctx.get('userChannelRequestService').listPrivateChannels({
				userId: ctx.get('user').id,
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/users/@me/channels',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		Validator('json', CreatePrivateChannelRequest),
		GroupDmCreateProtectionMiddleware,
		OpenAPI({
			operationId: 'create_private_channel',
			summary: 'Create private channel',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Creates a new private channel (direct message) between the current user and one or more recipients. Group DM creation requires CAPTCHA verification. Returns the newly created channel object.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			if ((await ctx.get('instanceConfigRepository').getInstancePolicyConfig()).direct_messages_disabled) {
				throw new DirectMessagesDisabledError();
			}
			const response = await ctx.get('userChannelRequestService').createPrivateChannel({
				userId: user.id,
				data: ctx.req.valid('json'),
				requestCache: ctx.get('requestCache'),
			});
			return ctx.json(response);
		},
	);
	app.put(
		'/users/@me/channels/:channel_id/pin',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'pin_direct_message_channel',
			summary: 'Pin direct message channel',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Pins a private message channel for the current user. Pinned channels appear at the top of the channel list for easy access.',
		}),
		async (ctx) => {
			await ctx.get('userChannelRequestService').pinChannel({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/users/@me/channels/:channel_id/pin',
		RateLimitMiddleware(RateLimitConfigs.USER_CHANNELS),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'unpin_direct_message_channel',
			summary: 'Unpin direct message channel',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Unpins a private message channel for the current user. The channel will return to its normal position in the channel list based on activity.',
		}),
		async (ctx) => {
			await ctx.get('userChannelRequestService').unpinChannel({
				userId: ctx.get('user').id,
				channelId: createChannelID(ctx.req.valid('param').channel_id),
			});
			return ctx.body(null, 204);
		},
	);
}
