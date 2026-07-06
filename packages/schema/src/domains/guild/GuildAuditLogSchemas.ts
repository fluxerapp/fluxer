// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {AuditLogActionTypeSchema} from '@fluxer/schema/src/primitives/AuditLogValidators';
import {
	coerceNumberFromString,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {WebhookTypeSchema} from '@fluxer/schema/src/primitives/WebhookValidators';
import {z} from 'zod';

const PermissionsDiffSchema = z.object({
	added: z.array(z.string()),
	removed: z.array(z.string()),
});
const AuditLogChangeValueSchema = z.union([
	z.string(),
	z.number(),
	z.boolean(),
	z.array(z.string()),
	z.array(z.number()),
	PermissionsDiffSchema,
	z.null(),
]);
export const AuditLogChangeSchema = z.object({
	key: z.string().describe('The field that changed'),
	old_value: AuditLogChangeValueSchema.optional().describe('Value before the change'),
	new_value: AuditLogChangeValueSchema.optional().describe('Value after the change'),
});

export type AuditLogChange = z.infer<typeof AuditLogChangeSchema>;

const AuditLogOptionsSchema = z.object({
	channel_id: z.string().optional().describe('Channel ID for relevant actions'),
	count: z.number().optional().describe('Count of items affected'),
	delete_member_days: z.string().optional().describe('Number of days of messages to delete on member ban'),
	id: z.string().optional().describe('ID of the affected entity'),
	integration_type: z.number().optional().describe('Type of integration'),
	message_id: z.string().optional().describe('Message ID for relevant actions'),
	option_id: z.string().optional().describe('Poll option ID for relevant poll actions'),
	poll_id: z.string().optional().describe('Poll ID for relevant poll actions'),
	members_removed: z.number().optional().describe('Number of members removed'),
	role_name: z.string().optional().describe('Name of the role'),
	type: z.number().optional().describe('Type identifier'),
	inviter_id: z.string().optional().describe('ID of the user who created the invite'),
	max_age: z.number().optional().describe('Maximum age of the invite in seconds'),
	max_uses: z.number().optional().describe('Maximum number of uses for the invite'),
	temporary: z.boolean().optional().describe('Whether the invite grants temporary membership'),
	uses: z.number().optional().describe('Number of times the invite has been used'),
});
export const GuildAuditLogEntryResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this audit log entry'),
	action_type: AuditLogActionTypeSchema,
	user_id: SnowflakeStringType.nullish().describe('The user ID of the user who performed the action'),
	target_id: z.string().nullish().describe('The ID of the affected entity (user, channel, role, invite code, etc.)'),
	reason: z.string().optional().describe('The reason provided for the action'),
	options: AuditLogOptionsSchema.optional().describe('Additional options depending on action type'),
	changes: z.array(AuditLogChangeSchema).optional().describe('Changes made to the target'),
});

export type GuildAuditLogEntryResponse = z.infer<typeof GuildAuditLogEntryResponse>;

export const AuditLogWebhookResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this webhook'),
	type: WebhookTypeSchema,
	guild_id: SnowflakeStringType.nullish().describe('The guild ID this webhook belongs to'),
	channel_id: SnowflakeStringType.nullish().describe('The channel ID this webhook posts to'),
	name: z.string().describe('The name of the webhook'),
	avatar_hash: z.string().nullish().describe('The hash of the webhook avatar'),
});

export type AuditLogWebhookResponse = z.infer<typeof AuditLogWebhookResponse>;

export const GuildAuditLogListResponse = z.object({
	audit_log_entries: z.array(GuildAuditLogEntryResponse).describe('Array of audit log entries'),
	users: z.array(UserPartialResponse).describe('Users referenced in the audit log entries'),
	webhooks: z.array(AuditLogWebhookResponse).describe('Webhooks referenced in the audit log entries'),
});

export type GuildAuditLogListResponse = z.infer<typeof GuildAuditLogListResponse>;

export const GuildAuditLogListQuery = z.object({
	limit: coerceNumberFromString(Int32Type.max(100))
		.optional()
		.describe('Maximum number of audit log entries to return (1-100)'),
	before: SnowflakeType.optional().describe('Get entries before this audit log entry ID'),
	after: SnowflakeType.optional().describe('Get entries after this audit log entry ID'),
	user_id: SnowflakeType.optional().describe('Filter entries by the user who performed the action'),
	action_type: coerceNumberFromString(AuditLogActionTypeSchema)
		.optional()
		.describe('Filter entries by the type of action'),
});

export type GuildAuditLogListQuery = z.infer<typeof GuildAuditLogListQuery>;
