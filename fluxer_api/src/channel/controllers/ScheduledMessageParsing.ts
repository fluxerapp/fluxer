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
import type {HonoEnv} from '~/App';
import type {ChannelID, UserID} from '~/BrandedTypes';
import type {MessageRequest} from '~/channel/ChannelModel';
import {MessageRequest as MessageRequestSchema} from '~/channel/ChannelModel';
import {InputValidationError} from '~/Errors';
import {createStringType, type z} from '~/Schema';
import {parseMultipartMessageData} from './MessageController';

export const SCHEDULED_ATTACHMENT_TTL_MS = 32 * 24 * 60 * 60 * 1000;

export const ScheduledMessageSchema = MessageRequestSchema.extend({
	scheduled_local_at: createStringType(1, 64),
	timezone: createStringType(1, 128),
});

export type ScheduledMessageSchemaType = z.infer<typeof ScheduledMessageSchema>;

export function extractScheduleFields(data: ScheduledMessageSchemaType): {
	scheduled_local_at: string;
	timezone: string;
	message: MessageRequest;
} {
	const {scheduled_local_at, timezone, ...messageData} = data;
	return {
		scheduled_local_at,
		timezone,
		message: messageData as MessageRequest,
	};
}

export async function parseScheduledMessageInput({
	ctx,
	userId,
	channelId,
}: {
	ctx: Context<HonoEnv>;
	userId: UserID;
	channelId: ChannelID;
}): Promise<{message: MessageRequest; scheduledLocalAt: string; timezone: string}> {
	const contentType = ctx.req.header('content-type') ?? '';
	const isMultipart = contentType.includes('multipart/form-data');

	if (isMultipart) {
		let parsedPayload: unknown = null;
		const message = (await parseMultipartMessageData(ctx, userId, channelId, MessageRequestSchema, {
			uploadExpiresAt: new Date(Date.now() + SCHEDULED_ATTACHMENT_TTL_MS),
			onPayloadParsed(payload) {
				parsedPayload = payload;
			},
		})) as MessageRequest;

		if (!parsedPayload) {
			throw InputValidationError.create('scheduled_message', 'Failed to parse multipart payload');
		}

		const validation = ScheduledMessageSchema.safeParse(parsedPayload);
		if (!validation.success) {
			throw InputValidationError.create('scheduled_message', 'Invalid scheduled message payload');
		}

		const {scheduled_local_at, timezone} = extractScheduleFields(validation.data);
		return {message, scheduledLocalAt: scheduled_local_at, timezone};
	}

	const body: unknown = await ctx.req.json();
	const validation = ScheduledMessageSchema.safeParse(body);
	if (!validation.success) {
		throw InputValidationError.create('scheduled_message', 'Invalid scheduled message payload');
	}

	const {scheduled_local_at, timezone, message} = extractScheduleFields(validation.data);
	return {message, scheduledLocalAt: scheduled_local_at, timezone};
}
