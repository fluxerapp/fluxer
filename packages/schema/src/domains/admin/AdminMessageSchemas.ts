// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BulkModerationBasis,
	MessageDeletionBasis,
	ModerationBasis,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {FilenameType} from '@fluxer/schema/src/primitives/FileValidators';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {Int32Type, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
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

export const AdminMessageDeleteRequest = z.object({
	basis: MessageDeletionBasis,
});

export type AdminMessageDeleteRequest = z.infer<typeof AdminMessageDeleteRequest>;

const MessageShredEntryType = z.object({
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
});
export const MessageShredResponse = z.object({
	success: z.literal(true),
	job_id: z.string(),
	requested: z.number().int().min(0).optional(),
	decision_id: SnowflakeStringType.optional().describe('The content removal decision, for an enforcement basis'),
});

export type MessageShredResponse = z.infer<typeof MessageShredResponse>;

export const DeleteAllUserMessagesResponse = z.object({
	success: z.literal(true),
	dry_run: z.boolean(),
	channel_count: Int32Type,
	message_count: Int32Type,
	job_id: z.string().optional(),
	decision_id: SnowflakeStringType.optional().describe('The content removal decision, for an enforcement basis'),
});

export type DeleteAllUserMessagesResponse = z.infer<typeof DeleteAllUserMessagesResponse>;

export const BulkDeleteUserMessagesRequest = z.object({
	user_ids: z.array(SnowflakeType).max(1000).describe('List of user IDs whose messages will be deleted'),
	basis: BulkModerationBasis,
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
	basis: ModerationBasis,
	preview_token: z
		.string()
		.regex(/^[0-9a-f]{64}$/)
		.optional()
		.describe(
			'The preview_token of a preview of the same entries. When given, exactly the previewed messages are removed, and the request is refused if they changed since the preview',
		),
});

export type AdminUserMessageShredRequest = z.infer<typeof AdminUserMessageShredRequest>;

export const AdminUserMessageShredPreviewRequest = z.object({
	entries: z.array(MessageShredEntryType).min(1).max(1000),
});

export type AdminUserMessageShredPreviewRequest = z.infer<typeof AdminUserMessageShredPreviewRequest>;

const MessageShredPreviewMessage = z.object({
	channel_id: SnowflakeStringType,
	message_id: SnowflakeStringType,
	timestamp: z.string().describe('When the message was sent, as an ISO 8601 timestamp'),
	content: z.string().describe('The start of the message text, up to 200 characters'),
	attachment_count: Int32Type,
});

const MessageShredPreviewSkipped = z.object({
	channel_id: SnowflakeStringType,
	message_id: SnowflakeStringType,
	reason: z
		.enum(['not_found', 'other_author', 'duplicate'])
		.describe('not_found when the message is not in that channel, other_author when someone else sent it'),
});

export const MessageShredPreviewResponse = z.object({
	requested: Int32Type,
	message_count: Int32Type.describe('How many messages a shred of these entries removes'),
	messages: z.array(MessageShredPreviewMessage).describe('Every message a shred of these entries removes'),
	skipped: z.array(MessageShredPreviewSkipped).describe('Entries a shred leaves alone, with the reason'),
	preview_token: z.string().describe('Pass this with the shred to remove exactly these messages'),
});

export type MessageShredPreviewResponse = z.infer<typeof MessageShredPreviewResponse>;

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

export const AdminUserMessageDeleteRequest = z.object({
	basis: ModerationBasis.optional().describe(
		'Why the messages are removed. Required unless dry_run is true. An enforcement basis refuses child safety and violent extremism reasons',
	),
});

export type AdminUserMessageDeleteRequest = z.infer<typeof AdminUserMessageDeleteRequest>;

export const MessageShredJobIdParam = z.object({
	job_id: SnowflakeType.describe('The ID of the message shred job'),
});

export type MessageShredJobIdParam = z.infer<typeof MessageShredJobIdParam>;
