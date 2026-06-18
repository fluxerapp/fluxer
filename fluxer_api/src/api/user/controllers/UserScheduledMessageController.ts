// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {ScheduledMessageIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	ScheduledMessageRequestSchema,
	ScheduledMessageResponseSchema,
} from '@fluxer/schema/src/domains/message/ScheduledMessageSchemas';
import type {Context} from 'hono';
import {z} from 'zod';
import {createMessageID} from '../../BrandedTypes';
import {parseScheduledMessageInput} from '../../channel/controllers/ScheduledMessageParsing';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {buildScheduledMessagesCalendar} from '../UserCalendarExport';

function sendCalendar(ctx: Context<HonoEnv>, body: string, filename: string) {
	ctx.header('Content-Type', 'text/calendar; charset=utf-8');
	ctx.header('Content-Disposition', `attachment; filename="${filename}"`);
	return ctx.body(body, 200);
}

export function UserScheduledMessageController(app: HonoApp) {
	app.get(
		'/users/@me/scheduled-messages',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_scheduled_messages',
			summary: 'List scheduled messages',
			responseSchema: z.array(ScheduledMessageResponseSchema),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves all scheduled messages for the current user. Returns list of messages that are scheduled to be sent at a future date and time.',
		}),
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
		'/users/@me/scheduled-messages/calendar.ics',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'export_scheduled_messages_calendar',
			summary: 'Export scheduled messages as iCalendar',
			responseSchema: null,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Exports scheduled messages for the current user as an iCalendar file that can be imported into external calendar providers.',
		}),
		async (ctx: Context<HonoEnv>) => {
			const userId = ctx.get('user').id;
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessages = await scheduledMessageService.listScheduledMessages(userId);
			return sendCalendar(ctx, buildScheduledMessagesCalendar(scheduledMessages), 'fluxer-scheduled-messages.ics');
		},
	);
	app.get(
		'/users/@me/scheduled-messages/:scheduled_message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ScheduledMessageIdParam),
		OpenAPI({
			operationId: 'get_scheduled_message',
			summary: 'Get scheduled message',
			responseSchema: ScheduledMessageResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Retrieves details of a specific scheduled message by ID. Returns the message content, scheduled send time, and status.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const scheduledMessageId = createMessageID(ctx.req.valid('param').scheduled_message_id);
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessage = await scheduledMessageService.getScheduledMessage(userId, scheduledMessageId);
			if (!scheduledMessage) {
				throw new UnknownMessageError();
			}
			return ctx.json(scheduledMessage.toResponse(), 200);
		},
	);
	app.get(
		'/users/@me/scheduled-messages/:scheduled_message_id/calendar.ics',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_READ),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ScheduledMessageIdParam),
		OpenAPI({
			operationId: 'export_scheduled_message_calendar',
			summary: 'Export a scheduled message as iCalendar',
			responseSchema: null,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Exports one scheduled message for the current user as an iCalendar file that can be imported into external calendar providers.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const scheduledMessageId = createMessageID(ctx.req.valid('param').scheduled_message_id);
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessage = await scheduledMessageService.getScheduledMessage(userId, scheduledMessageId);
			if (!scheduledMessage) {
				throw new UnknownMessageError();
			}
			return sendCalendar(
				ctx,
				buildScheduledMessagesCalendar([scheduledMessage]),
				`fluxer-scheduled-message-${scheduledMessage.id.toString()}.ics`,
			);
		},
	);
	app.delete(
		'/users/@me/scheduled-messages/:scheduled_message_id',
		RateLimitMiddleware(RateLimitConfigs.USER_SAVED_MESSAGES_WRITE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ScheduledMessageIdParam),
		OpenAPI({
			operationId: 'cancel_scheduled_message',
			summary: 'Cancel scheduled message',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Cancels and deletes a scheduled message before it is sent. The message will not be delivered if cancelled.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const scheduledMessageId = createMessageID(ctx.req.valid('param').scheduled_message_id);
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
		Validator('param', ScheduledMessageIdParam),
		OpenAPI({
			operationId: 'update_scheduled_message',
			summary: 'Update scheduled message',
			responseSchema: ScheduledMessageResponseSchema,
			requestSchema: ScheduledMessageRequestSchema,
			requestFormSchema: ScheduledMessageRequestSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
			description:
				'Updates an existing scheduled message before it is sent. Can modify message content, scheduled time, and timezone. Returns updated scheduled message details.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const scheduledMessageService = ctx.get('scheduledMessageService');
			const scheduledMessageId = createMessageID(ctx.req.valid('param').scheduled_message_id);
			const existingMessage = await scheduledMessageService.getScheduledMessage(user.id, scheduledMessageId);
			if (!existingMessage) {
				throw new UnknownMessageError();
			}
			const channelId = existingMessage.channelId;
			const {message, scheduledLocalAt, timezone} = await parseScheduledMessageInput({
				ctx,
				user,
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
}
