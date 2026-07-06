// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CONTENT_WARNING_TEXT_MAX_LENGTH,
	GuildOperations,
	GuildOperationsDescriptions,
} from '@fluxer/constants/src/GuildConstants';
import {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {GuildBanCreateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {GuildFeatureSchema} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {AuditLogActionTypeSchema} from '@fluxer/schema/src/primitives/AuditLogValidators';
import {VanityURLCodeType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {
	ContentWarningLevelSchema,
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	NSFWLevelSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {
	createBitflagInt32Type,
	createNamedStringLiteralUnion,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
	withFieldDescription,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildAdminResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this guild'),
	name: z.string().describe('The name of the guild'),
	features: z.array(GuildFeatureSchema).max(100).describe('Array of guild feature flags'),
	owner_id: SnowflakeStringType.describe('The ID of the guild owner'),
	owner_username: z.string().nullable().describe('The username of the guild owner'),
	owner_global_name: z.string().nullable().describe('The display name of the guild owner, if set'),
	owner_discriminator: z.string().nullable().describe('The discriminator of the guild owner'),
	icon: z.string().nullable().describe('The hash of the guild icon'),
	banner: z.string().nullable().describe('The hash of the guild banner'),
	member_count: Int32Type.describe('The number of members in the guild'),
	nsfw_level: NSFWLevelSchema.optional().describe('The NSFW level of the guild'),
	nsfw: z.boolean().optional().describe('Whether the guild is flagged as adult content'),
	content_warning_level: ContentWarningLevelSchema.optional().describe('The content warning level for the guild'),
	content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH)
		.nullable()
		.optional()
		.describe('Custom content warning text shown before entry'),
	approximate_member_count: Int32Type.optional().describe(
		'Approximate total member count (only when with_counts is true)'
	),
	approximate_presence_count: Int32Type.optional().describe(
		'Approximate online member count (only when with_counts is true)'
	),
});

export type GuildAdminResponse = z.infer<typeof GuildAdminResponse>;

export const ListUserGuildsResponse = z.object({
	guilds: z.array(GuildAdminResponse),
});

export type ListUserGuildsResponse = z.infer<typeof ListUserGuildsResponse>;

export const ListUserGuildsRequest = z.object({
	user_id: SnowflakeType,
	before: SnowflakeType.optional(),
	after: SnowflakeType.optional(),
	limit: z.number().int().min(1).max(200).default(200),
	with_counts: z.boolean().default(false),
});

export type ListUserGuildsRequest = z.infer<typeof ListUserGuildsRequest>;

export const LookupGuildRequest = z.object({
	guild_id: SnowflakeType,
});

export type LookupGuildRequest = z.infer<typeof LookupGuildRequest>;

export const ListGuildMembersRequest = z.object({
	guild_id: SnowflakeType,
	limit: z.number().int().min(1).max(200).default(50),
	offset: z.number().int().min(0).default(0),
});

export type ListGuildMembersRequest = z.infer<typeof ListGuildMembersRequest>;

export const BanGuildMemberRequest = GuildBanCreateRequest.extend({
	guild_id: SnowflakeType,
	user_id: SnowflakeType,
});

export type BanGuildMemberRequest = z.infer<typeof BanGuildMemberRequest>;

export const KickGuildMemberRequest = z.object({
	guild_id: SnowflakeType,
	user_id: SnowflakeType,
});

export type KickGuildMemberRequest = z.infer<typeof KickGuildMemberRequest>;

export const SearchGuildsRequest = z.object({
	query: createStringType(1, 1024).optional(),
	limit: z.number().int().min(1).max(200).default(50),
	offset: z.number().int().min(0).default(0),
});

export type SearchGuildsRequest = z.infer<typeof SearchGuildsRequest>;

export const ReloadGuildRequest = z.object({
	guild_id: SnowflakeType,
});

export type ReloadGuildRequest = z.infer<typeof ReloadGuildRequest>;

export const ShutdownGuildRequest = z.object({
	guild_id: SnowflakeType,
});

export type ShutdownGuildRequest = z.infer<typeof ShutdownGuildRequest>;

export const GetProcessMemoryStatsRequest = z.object({
	limit: z.number().int().min(100).max(1000).default(100),
});

export type GetProcessMemoryStatsRequest = z.infer<typeof GetProcessMemoryStatsRequest>;

export const UpdateGuildFeaturesRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to update'),
	add_features: z.array(GuildFeatureSchema).max(100).default([]).describe('Guild features to add'),
	remove_features: z.array(GuildFeatureSchema).max(100).default([]).describe('Guild features to remove'),
});

export type UpdateGuildFeaturesRequest = z.infer<typeof UpdateGuildFeaturesRequest>;

export const ForceAddUserToGuildRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to add to the guild'),
	guild_id: SnowflakeType.describe('ID of the guild to add the user to'),
});

export type ForceAddUserToGuildRequest = z.infer<typeof ForceAddUserToGuildRequest>;

const GuildImageFieldEnum = createNamedStringLiteralUnion(
	[
		['icon', 'icon', 'Guild icon image'],
		['banner', 'banner', 'Guild banner image'],
		['splash', 'splash', 'Guild invite splash image'],
		['embed_splash', 'embed_splash', 'Guild embedded invite splash image'],
	],
	'Guild image field that can be cleared',
);
export const ClearGuildFieldsRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to clear fields for'),
	fields: z.array(GuildImageFieldEnum).max(10).describe('List of guild image fields to clear'),
});

export type ClearGuildFieldsRequest = z.infer<typeof ClearGuildFieldsRequest>;

export const DeleteGuildRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to delete'),
});

export type DeleteGuildRequest = z.infer<typeof DeleteGuildRequest>;

export const UpdateGuildVanityRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to update'),
	vanity_url_code: VanityURLCodeType.nullable().describe('New vanity URL code, or null to remove'),
});

export type UpdateGuildVanityRequest = z.infer<typeof UpdateGuildVanityRequest>;

export const UpdateGuildNameRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to update'),
	name: createStringType(1, 100).describe('New name for the guild'),
});

export type UpdateGuildNameRequest = z.infer<typeof UpdateGuildNameRequest>;

export const UpdateGuildSettingsRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to update'),
	verification_level: withFieldDescription(
		GuildVerificationLevelSchema,
		'Required verification level for guild members',
	).optional(),
	mfa_level: withFieldDescription(GuildMFALevelSchema, 'Required MFA level for moderators').optional(),
	nsfw_level: withFieldDescription(NSFWLevelSchema, 'NSFW content level for the guild').optional(),
	nsfw: z.boolean().optional().describe('Whether the guild is flagged as adult content'),
	content_warning_level: ContentWarningLevelSchema.optional().describe('Content warning level for the guild'),
	content_warning_text: createStringType(0, CONTENT_WARNING_TEXT_MAX_LENGTH)
		.nullable()
		.optional()
		.describe('Custom content warning text shown before entry'),
	explicit_content_filter: withFieldDescription(
		GuildExplicitContentFilterSchema,
		'Explicit content filter level',
	).optional(),
	default_message_notifications: withFieldDescription(
		DefaultMessageNotificationsSchema,
		'Default notification setting for new members',
	).optional(),
	disabled_operations: createBitflagInt32Type(
		GuildOperations,
		GuildOperationsDescriptions,
		'Bitmask of disabled guild operations',
		'GuildOperations',
	).optional(),
});

export type UpdateGuildSettingsRequest = z.infer<typeof UpdateGuildSettingsRequest>;

export const TransferGuildOwnershipRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to transfer'),
	new_owner_id: SnowflakeType.describe('ID of the user to transfer ownership to'),
});

export type TransferGuildOwnershipRequest = z.infer<typeof TransferGuildOwnershipRequest>;

export const BulkUpdateGuildFeaturesRequest = z.object({
	guild_ids: z.array(SnowflakeType).max(1000).describe('List of guild IDs to update'),
	add_features: z
		.array(GuildFeatureSchema)
		.max(100)
		.default([])
		.describe('Guild features to add to all specified guilds'),
	remove_features: z
		.array(GuildFeatureSchema)
		.max(100)
		.default([])
		.describe('Guild features to remove from all specified guilds'),
});

export type BulkUpdateGuildFeaturesRequest = z.infer<typeof BulkUpdateGuildFeaturesRequest>;

export const BulkAddGuildMembersRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild to add members to'),
	user_ids: z.array(SnowflakeType).max(1000).describe('List of user IDs to add as members'),
});

export type BulkAddGuildMembersRequest = z.infer<typeof BulkAddGuildMembersRequest>;

export const ListGuildAuditLogsRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild whose audit log to read'),
	limit: Int32Type.min(1).max(100).optional().describe('Maximum entries to return (1-100, default 50)'),
	before: SnowflakeType.optional().describe('Return entries before this log ID'),
	after: SnowflakeType.optional().describe('Return entries after this log ID'),
	user_id: SnowflakeType.optional().describe('Filter to entries performed by this user'),
	action_type: AuditLogActionTypeSchema.optional().describe('Filter to a specific action type'),
});

export type ListGuildAuditLogsRequest = z.infer<typeof ListGuildAuditLogsRequest>;

export const ListGuildAuditLogsResponse = GuildAuditLogListResponse;

export type ListGuildAuditLogsResponse = z.infer<typeof ListGuildAuditLogsResponse>;
