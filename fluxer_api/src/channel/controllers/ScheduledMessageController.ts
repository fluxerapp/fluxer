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
import {createChannelID} from '~/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';
import {parseScheduledMessageInput} from './ScheduledMessageParsing';

export const ScheduledMessageController = (app: HonoApp) => {
	app.post(
		'/channels/:channel_id/messages/schedule',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const scheduledMessageService = ctx.get('scheduledMessageService');

			const {message, scheduledLocalAt, timezone} = await parseScheduledMessageInput({
				ctx,
				userId: user.id,
				channelId,
			});

			const scheduledMessage = await scheduledMessageService.createScheduledMessage({
				user,
				channelId,
				data: message,
				scheduledLocalAt,
				timezone,
			});

			return ctx.json(scheduledMessage.toResponse(), 201);
		},
	);
};
