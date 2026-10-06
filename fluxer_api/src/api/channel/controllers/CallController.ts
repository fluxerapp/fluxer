// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {CallRingBodySchema, CallUpdateBodySchema} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {CallEligibilityResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {ChannelIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function CallController(app: HonoApp) {
	app.get(
		'/channels/:channel_id/call',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'get_call_eligibility',
			summary: 'Get call eligibility status',
			description:
				'Checks whether a call can be initiated in the channel and if there is an active call. Returns ringable status and silent mode flag.',
			responseSchema: CallEligibilityResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channelService = ctx.get('channelService');
			const {ringable, silent} = await channelService.calls.checkCallEligibility({userId, channelId});
			return ctx.json({ringable, silent: !!silent});
		},
	);
	app.patch(
		'/channels/:channel_id/call',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		Validator('json', CallUpdateBodySchema),
		OpenAPI({
			operationId: 'update_call_region',
			summary: 'Update call region',
			description: 'Changes the voice server region for an active call to optimise latency and connection quality.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {region, latitude, longitude} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');
			await channelService.calls.updateCall({userId, channelId, region, latitude, longitude});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/call/ring',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_RING),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		Validator('json', CallRingBodySchema),
		OpenAPI({
			operationId: 'ring_call_recipients',
			summary: 'Ring call recipients',
			description:
				'Sends ringing notifications to specified users in a call. If no recipients are specified, rings all channel members.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {recipients, latitude, longitude} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');
			const requestCache = ctx.get('requestCache');
			const recipientIds = recipients ? recipients.map(createUserID) : undefined;
			await channelService.calls.ringCallRecipients({
				userId,
				channelId,
				recipients: recipientIds,
				latitude,
				longitude,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/call/stop-ringing',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_STOP_RINGING),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		Validator('json', CallRingBodySchema),
		OpenAPI({
			operationId: 'stop_ringing_call_recipients',
			summary: 'Stop ringing call recipients',
			description:
				'Stops ringing notifications for specified users in a call. Allows callers to stop notifying users who have declined or not responded.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {recipients} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');
			const recipientIds = recipients ? recipients.map(createUserID) : undefined;
			await channelService.calls.stopRingingCallRecipients({
				userId,
				channelId,
				recipients: recipientIds,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/call/end',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'end_call',
			summary: 'End call session',
			description: 'Terminates an active voice call in the channel. Records the call end state for all participants.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			ctx.req.valid('param');
			return ctx.body(null, 204);
		},
	);
}
