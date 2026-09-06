// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminMessageSchema, LookupMessageResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {createStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const BrowseChannelRequest = z.object({
	channel_id: SnowflakeType,
	before: SnowflakeType.optional(),
	after: SnowflakeType.optional(),
	limit: z.number().int().min(1).max(100).default(50),
});

export type BrowseChannelRequest = z.infer<typeof BrowseChannelRequest>;

export const BrowseChannelResponse = z.object({
	messages: z.array(AdminMessageSchema).max(100),
	message_responses: z.array(MessageResponseSchema).max(100).optional(),
	has_more: z.boolean(),
});

export type BrowseChannelResponse = z.infer<typeof BrowseChannelResponse>;

export const SearchChannelMessagesRequest = z.object({
	channel_id: SnowflakeType,
	query: z.string().min(1).max(200),
	limit: z.number().int().min(1).max(100).default(25),
});

export type SearchChannelMessagesRequest = z.infer<typeof SearchChannelMessagesRequest>;

const SearchChannelMessagesResponse = z.object({
	messages: z.array(AdminMessageSchema).max(100),
	message_responses: z.array(MessageResponseSchema).max(100).optional(),
	total: z.number().int().min(0),
});

export const AdminChannelMessageListQuery = z.object({
	limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}).describe(
		'Number of messages to return (1-100, default 50)',
	),
	before: SnowflakeType.optional().describe('Return messages older than this message ID'),
	after: SnowflakeType.optional().describe('Return messages newer than this message ID'),
});

export type AdminChannelMessageListQuery = z.infer<typeof AdminChannelMessageListQuery>;

export const AdminMessageSearchQuery = z.object({
	channel_id: SnowflakeType.describe('Return messages sent in this channel'),
	q: createStringType(1, 200).optional().describe('Free-text query matched against message content'),
	message_id: SnowflakeType.optional().describe(
		'Return the single message with this ID together with its surrounding context; ignores every other filter',
	),
	attachment_id: SnowflakeType.optional().describe(
		'Return the single message carrying this attachment together with its surrounding context; requires filename',
	),
	filename: FilenameType.optional().describe('The filename of the attachment named by attachment_id'),
	context_limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}).describe(
		'How many messages surrounding a message resolved by message_id or attachment_id to return as context (1-100, default 50)',
	),
	limit: createQueryIntegerType({defaultValue: 25, minValue: 1, maxValue: 100}).describe(
		'Maximum number of messages to return when searching (1-100, default 25)',
	),
});

export type AdminMessageSearchQuery = z.infer<typeof AdminMessageSearchQuery>;

export const AdminMessageSearchResponse = z.union([SearchChannelMessagesResponse, LookupMessageResponse]);

export type AdminMessageSearchResponse = z.infer<typeof AdminMessageSearchResponse>;
