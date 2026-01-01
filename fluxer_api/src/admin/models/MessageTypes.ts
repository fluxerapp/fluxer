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

import {FilenameType, Int64Type, z} from '~/Schema';

export const LookupMessageRequest = z.object({
	channel_id: Int64Type,
	message_id: Int64Type,
	context_limit: z.number().default(50),
});

export type LookupMessageRequest = z.infer<typeof LookupMessageRequest>;

export const LookupMessageByAttachmentRequest = z.object({
	channel_id: Int64Type,
	attachment_id: Int64Type,
	filename: FilenameType,
	context_limit: z.number().default(50),
});

export type LookupMessageByAttachmentRequest = z.infer<typeof LookupMessageByAttachmentRequest>;

export const DeleteMessageRequest = z.object({
	channel_id: Int64Type,
	message_id: Int64Type,
});

export type DeleteMessageRequest = z.infer<typeof DeleteMessageRequest>;

const MessageShredEntryType = z.object({
	channel_id: Int64Type,
	message_id: Int64Type,
});

export const MessageShredRequest = z.object({
	user_id: Int64Type,
	entries: z.array(MessageShredEntryType).min(1),
});

export type MessageShredRequest = z.infer<typeof MessageShredRequest>;

export const MessageShredResponse = z.object({
	success: z.literal(true),
	job_id: z.string(),
	requested: z.number().int().min(0).optional(),
});

export type MessageShredResponse = z.infer<typeof MessageShredResponse>;

export const MessageShredStatusRequest = z.object({
	job_id: z.string(),
});

export type MessageShredStatusRequest = z.infer<typeof MessageShredStatusRequest>;

export const DeleteAllUserMessagesRequest = z.object({
	user_id: Int64Type,
	dry_run: z.boolean().default(true),
});

export type DeleteAllUserMessagesRequest = z.infer<typeof DeleteAllUserMessagesRequest>;

export const DeleteAllUserMessagesResponse = z.object({
	success: z.literal(true),
	dry_run: z.boolean(),
	channel_count: z.number().int(),
	message_count: z.number().int(),
	job_id: z.string().optional(),
});

export type DeleteAllUserMessagesResponse = z.infer<typeof DeleteAllUserMessagesResponse>;
