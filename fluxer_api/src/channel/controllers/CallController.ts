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
import {createChannelID, createUserID} from '~/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const CallController = (app: HonoApp) => {
	app.get(
		'/channels/:channel_id/call',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channelService = ctx.get('channelService');

			const {ringable, silent} = await channelService.checkCallEligibility({userId, channelId});

			return ctx.json({ringable, silent});
		},
	);

	app.patch(
		'/channels/:channel_id/call',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', z.object({region: createStringType(1, 64).optional()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {region} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');

			await channelService.updateCall({userId, channelId, region});

			return ctx.body(null, 204);
		},
	);

	app.post(
		'/channels/:channel_id/call/ring',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_CALL_RING),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', z.object({recipients: z.array(Int64Type).optional()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {recipients} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');
			const requestCache = ctx.get('requestCache');

			const recipientIds = recipients ? recipients.map(createUserID) : undefined;

			await channelService.ringCallRecipients({
				userId,
				channelId,
				recipients: recipientIds,
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
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', z.object({recipients: z.array(Int64Type).optional()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {recipients} = ctx.req.valid('json');
			const channelService = ctx.get('channelService');

			const recipientIds = recipients ? recipients.map(createUserID) : undefined;

			await channelService.stopRingingCallRecipients({
				userId,
				channelId,
				recipients: recipientIds,
			});

			return ctx.body(null, 204);
		},
	);
};
