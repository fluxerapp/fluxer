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

import type {Context} from 'hono';
import type {HonoApp, HonoEnv} from '~/App';
import {createMessageID} from '~/BrandedTypes';
import {parseScheduledMessageInput} from '~/channel/controllers/ScheduledMessageParsing';
import {UnknownMessageError} from '~/Errors';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const UserScheduledMessageController = (app: HonoApp) => {
	app.get(
		'/users/@me/scheduled-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		async (ctx: Context<HonoEnv>) => {
			const userId = ctx.get('user').id;
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessages = await scheduledMessageService.listScheduledMessages(userId);

			return ctx.json(
				scheduledMessages.map((message) => message.toResponse()),
				200,
			);
		},
	);

	app.get(
		'/users/@me/scheduled-messages/:scheduled_message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({scheduled_message_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const scheduledMessageId = createMessageID(BigInt(ctx.req.valid('param').scheduled_message_id));
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessage = await scheduledMessageService.getScheduledMessage(userId, scheduledMessageId);

			if (!scheduledMessage) {
				throw new UnknownMessageError();
			}

			return ctx.json(scheduledMessage.toResponse(), 200);
		},
	);

	app.delete(
		'/users/@me/scheduled-messages/:scheduled_message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({scheduled_message_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const scheduledMessageId = createMessageID(BigInt(ctx.req.valid('param').scheduled_message_id));
			const scheduledMessageService = ctx.get('scheduledMessageService');
			await scheduledMessageService.cancelScheduledMessage(userId, scheduledMessageId);
			return ctx.body(null, 204);
		},
	);

	app.patch(
		'/users/@me/scheduled-messages/:scheduled_message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({scheduled_message_id: Int64Type})),
		async (ctx) => {
			const user = ctx.get('user');
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessageId = createMessageID(BigInt(ctx.req.valid('param').scheduled_message_id));

			const existingMessage = await scheduledMessageService.getScheduledMessage(user.id, scheduledMessageId);
			if (!existingMessage) {
				throw new UnknownMessageError();
			}
			const channelId = existingMessage.channelId;

			const {message, scheduledLocalAt, timezone} = await parseScheduledMessageInput({
				ctx,
				userId: user.id,
				channelId,
			});

			const scheduledMessage = await scheduledMessageService.updateScheduledMessage({
				user,
				channelId,
				data: message,
				scheduledLocalAt,
				timezone,
				scheduledMessageId,
				existing: existingMessage,
			});

			return ctx.json(scheduledMessage.toResponse(), 200);
		},
	);
};
