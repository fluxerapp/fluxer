// SPDX-License-Identifier: AGPL-3.0-or-later

import {FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {Int32Type, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const FALSE_QUERY_VALUES = ['false', 'False', '0'];

export const LookupMessageRequest = z.object({
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
	context_limit: z.number().int().min(1).max(100).default(50),
});

export type LookupMessageRequest = z.infer<typeof LookupMessageRequest>;

export const LookupMessageByAttachmentRequest = z.object({
	channel_id: SnowflakeType,
	attachment_id: SnowflakeType,
	filename: FilenameType,
	context_limit: z.number().int().min(1).max(100).default(50),
});

export type LookupMessageByAttachmentRequest = z.infer<typeof LookupMessageByAttachmentRequest>;

export const DeleteMessageRequest = z.object({
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
});

export type DeleteMessageRequest = z.infer<typeof DeleteMessageRequest>;

export const ReportAttachmentToNcmecRequest = z.object({
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
	attachment_id: SnowflakeType,
	filename: FilenameType,
	source_report_id: SnowflakeType.optional(),
	reporter_full_name: z.string().trim().min(1).max(200),
	confirmed_viewed: z.literal(true),
});

export type ReportAttachmentToNcmecRequest = z.infer<typeof ReportAttachmentToNcmecRequest>;

const MessageShredEntryType = z.object({
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
});
export const MessageShredRequest = z.object({
	user_id: SnowflakeType,
	entries: z.array(MessageShredEntryType).min(1).max(1000),
});

export type MessageShredRequest = z.infer<typeof MessageShredRequest>;

export const MessageShredResponse = z.object({
	success: z.literal(true),
	job_id: z.string(),
	requested: z.number().int().min(0).optional(),
});

export type MessageShredResponse = z.infer<typeof MessageShredResponse>;

export const DeleteAllUserMessagesRequest = z.object({
	user_id: SnowflakeType,
	dry_run: z.boolean().default(true),
});

export type DeleteAllUserMessagesRequest = z.infer<typeof DeleteAllUserMessagesRequest>;

export const DeleteAllUserMessagesResponse = z.object({
	success: z.literal(true),
	dry_run: z.boolean(),
	channel_count: Int32Type,
	message_count: Int32Type,
	job_id: z.string().optional(),
});

export type DeleteAllUserMessagesResponse = z.infer<typeof DeleteAllUserMessagesResponse>;

export const BulkDeleteUserMessagesRequest = z.object({
	user_ids: z.array(SnowflakeType).max(1000).describe('List of user IDs whose messages will be deleted'),
});

export type BulkDeleteUserMessagesRequest = z.infer<typeof BulkDeleteUserMessagesRequest>;
export const AdminMessageDetailQuery = z.object({
	context_limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}).describe(
		'How many messages surrounding the requested message to return as context (1-100, default 50)',
	),
});

export type AdminMessageDetailQuery = z.infer<typeof AdminMessageDetailQuery>;

export const AdminUserMessageShredRequest = z.object({
	entries: z.array(MessageShredEntryType).min(1).max(1000),
});

export type AdminUserMessageShredRequest = z.infer<typeof AdminUserMessageShredRequest>;

export const AdminUserMessageDeleteQuery = z.object({
	dry_run: z
		.string()
		.trim()
		.optional()
		.default('true')
		.transform((value) => !FALSE_QUERY_VALUES.includes(value))
		.describe('Count the messages that would be deleted without deleting anything (default true)'),
});

export type AdminUserMessageDeleteQuery = z.infer<typeof AdminUserMessageDeleteQuery>;

export const MessageShredJobIdParam = z.object({
	job_id: SnowflakeType.describe('The ID of the message shred job'),
});

export type MessageShredJobIdParam = z.infer<typeof MessageShredJobIdParam>;
