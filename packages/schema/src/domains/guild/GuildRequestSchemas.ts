// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CONTENT_WARNING_TEXT_MAX_LENGTH,
	GuildMemberProfileFlags,
	GuildMemberProfileFlagsDescriptions,
	SystemChannelFlags,
	SystemChannelFlagsDescriptions,
} from '@fluxer/constants/src/GuildConstants';
import {
	AVATAR_MAX_SIZE,
	EMOJI_MAX_SIZE,
	MAX_GUILD_ROLES,
	MAX_GUILD_STICKER_TAGS,
	MAX_TEMP_BAN_DURATION_SECONDS,
	MIN_TEMP_BAN_DURATION_SECONDS,
	STICKER_MAX_SIZE,
} from '@fluxer/constants/src/LimitConstants';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {GuildFeatureSchema} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {TemplateSerializedGuild} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {VanityURLCodeType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {
	ContentWarningLevelSchema,
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	NSFWLevelSchema,
	SplashCardAlignmentSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	ColorType,
	createBitflagInt32Type,
	createStringType,
	SnowflakeType,
	UnsignedInt64Type,
	withFieldDescription,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {MentionReplyPreferencesSchema} from '@fluxer/schema/src/primitives/UserSettingsValidators';
import {PasswordType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

function coerceBlankStringToNull(value: unknown): unknown {
	if (typeof value === 'string' && value.trim().length === 0) {
		return null;
	}
	return value;
}

export const GuildCreateRequest = z.object({
	name: createStringType(1, 100).describe('The name of the guild (1-100 characters)'),
	icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded image data for the guild icon'),
	empty_features: z.boolean().optional().describe('Whether to create the guild without default features'),
	template: TemplateSerializedGuild.optional().describe('Serialised template data to use for guild structure'),
});

export type GuildCreateRequest = z.infer<typeof GuildCreateRequest>;

export const GuildUpdateRequest = z
	.object({
		name: createStringType(1, 100).describe('The name of the guild (1-100 characters)'),
		icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('Base64-encoded image data for the guild icon'),
		system_channel_id: SnowflakeType.nullish().describe('The ID of the channel where system messages are sent'),
		system_channel_flags: createBitflagInt32Type(
			SystemChannelFlags,
			SystemChannelFlagsDescriptions,
			'Bitfield of system channel flags controlling which messages are suppressed',
			'SystemChannelFlags',
		),
		afk_channel_id: SnowflakeType.nullish().describe('The ID of the AFK voice channel'),
		afk_timeout: z
			.number()
			.int()
			.min(60)
			.max(3600)
			.describe('AFK timeout in seconds (60-3600) before moving users to the AFK channel'),
		default_message_notifications: withFieldDescription(
			DefaultMessageNotificationsSchema,
			'Default notification level for new members',
		),
		verification_level: withFieldDescription(
			GuildVerificationLevelSchema,
			'Required verification level for members to participate',
		),
		mfa_level: withFieldDescription(GuildMFALevelSchema, 'Required MFA level for moderation actions'),
		nsfw_level: withFieldDescription(
			NSFWLevelSchema,
			'Legacy: setting this translates to the modern nsfw flag and content warning level',
		),
		nsfw: z.boolean().optional().describe('Whether the guild is marked as adult (18+) content'),
		content_warning_level: withFieldDescription(
			ContentWarningLevelSchema,
			'Whether the guild displays a content warning before entering',
		),
		content_warning_text: z
			.string()
			.max(CONTENT_WARNING_TEXT_MAX_LENGTH)
			.nullish()
			.describe('Custom guild-wide content warning text (max 200 characters); null falls back to a localized default'),
		explicit_content_filter: withFieldDescription(
			GuildExplicitContentFilterSchema,
			'Level of content filtering for explicit media',
		),
		banner: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('Base64-encoded image data for the guild banner'),
		splash: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('Base64-encoded image data for the guild splash screen'),
		embed_splash: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('Base64-encoded image data for the embedded invite splash'),
		splash_card_alignment: SplashCardAlignmentSchema.optional().describe(
			'Alignment of the splash card (center, left, or right)',
		),
		features: z
			.array(GuildFeatureSchema)
			.max(100)
			.describe(
				'Complete desired feature set for the guild. Only user-toggleable features may differ from the current set; non-toggleable features must be preserved as-is.',
			),
		message_history_cutoff: z.iso
			.datetime()
			.nullish()
			.describe(
				'ISO8601 timestamp controlling how far back members without Read Message History can access messages. Set to null to disable historical access.',
			),
	})
	.partial()
	.merge(SudoVerificationSchema);

export type GuildUpdateRequest = z.infer<typeof GuildUpdateRequest>;

export const GuildMemberUpdateRequest = z.object({
	nick: z
		.preprocess(coerceBlankStringToNull, createStringType(1, 32).nullish())
		.describe('The nickname to set for the member (1-32 characters)'),
	roles: z
		.array(SnowflakeType)
		.max(MAX_GUILD_ROLES, `Maximum ${MAX_GUILD_ROLES} roles allowed`)
		.optional()
		.transform((ids) => (ids ? new Set(ids) : undefined))
		.describe(`Array of role IDs to assign to the member (max ${MAX_GUILD_ROLES})`),
	avatar: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded image data for the member guild avatar'),
	banner: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded image data for the member guild banner'),
	bio: createStringType(1, 320).nullish().describe('The member guild profile bio (1-320 characters)'),
	pronouns: createStringType(1, 40).nullish().describe('The member guild profile pronouns (1-40 characters)'),
	accent_color: ColorType.nullish().describe('The accent color for the member guild profile as an integer'),
	profile_flags: createBitflagInt32Type(
		GuildMemberProfileFlags,
		GuildMemberProfileFlagsDescriptions,
		'Bitfield of profile flags for the member',
		'GuildMemberProfileFlags',
	).nullish(),
	mention_flags: withFieldDescription(
		MentionReplyPreferencesSchema,
		"Per-guild reply mention preference override for this member; NO_PREFERENCE inherits the user's account-wide setting",
	).nullish(),
	mute: z.boolean().optional().describe('Whether the member is muted in voice channels'),
	deaf: z.boolean().optional().describe('Whether the member is deafened in voice channels'),
	communication_disabled_until: z
		.preprocess(coerceBlankStringToNull, z.iso.datetime().nullish())
		.describe('ISO8601 timestamp until which the member is timed out'),
	timeout_reason: createStringType(1, 512)
		.nullish()
		.describe('The reason for timing out the member (1-512 characters)'),
	channel_id: SnowflakeType.nullish().describe('The voice channel ID to move the member to'),
	connection_id: createStringType(1, 32).nullish().describe('The voice connection ID for the member'),
});

export type GuildMemberUpdateRequest = z.infer<typeof GuildMemberUpdateRequest>;

export const MyGuildMemberUpdateRequest = GuildMemberUpdateRequest.omit({roles: true}).partial();

export type MyGuildMemberUpdateRequest = z.infer<typeof MyGuildMemberUpdateRequest>;

export const GuildRoleCreateRequest = z.object({
	name: createStringType(1, 100).describe('The name of the role (1-100 characters)'),
	color: ColorType.default(0x000000).describe('The color of the role as an integer (default: 0)'),
	permissions: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type The permissions bitfield for the role'),
});

export type GuildRoleCreateRequest = z.infer<typeof GuildRoleCreateRequest>;

export const GuildRoleUpdateRequest = z.object({
	name: createStringType(1, 100).optional().describe('The name of the role (1-100 characters)'),
	color: ColorType.optional().describe('The color of the role as an integer'),
	permissions: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type The permissions bitfield for the role'),
	hoist: z.boolean().optional().describe('Whether the role should be displayed separately in the member list'),
	hoist_position: z.number().int().nullish().describe('The position of the role in the hoisted member list'),
	mentionable: z.boolean().optional().describe('Whether the role can be mentioned by anyone'),
});

export type GuildRoleUpdateRequest = z.infer<typeof GuildRoleUpdateRequest>;

export const GuildEmojiCreateRequest = z.object({
	name: createStringType(2, 32)
		.refine((value) => /^[a-zA-Z0-9_]+$/.test(value), 'Emoji name can only contain letters, numbers, and underscores')
		.describe('The name of the emoji (2-32 characters, alphanumeric and underscores only)'),
	image: createBase64StringType(1, Math.ceil(EMOJI_MAX_SIZE * (4 / 3))).describe(
		'Base64-encoded image data for the emoji',
	),
});

export type GuildEmojiCreateRequest = z.infer<typeof GuildEmojiCreateRequest>;

export const GuildEmojiUpdateRequest = GuildEmojiCreateRequest.pick({name: true});

export type GuildEmojiUpdateRequest = z.infer<typeof GuildEmojiUpdateRequest>;

export const GuildEmojiBulkCreateRequest = z.object({
	emojis: z
		.array(GuildEmojiCreateRequest)
		.min(1, 'At least one emoji is required')
		.max(50, 'Maximum 50 emojis per batch')
		.describe('Array of emoji objects to create (1-50 emojis per batch)'),
});

export type GuildEmojiBulkCreateRequest = z.infer<typeof GuildEmojiBulkCreateRequest>;

export const GuildEmojiCloneRequest = z.object({
	source_emoji_id: SnowflakeType.describe(
		'The ID of the existing emoji to clone. Its name and image are copied as-is; no other fields are accepted',
	),
});

export type GuildEmojiCloneRequest = z.infer<typeof GuildEmojiCloneRequest>;

export const GuildStickerCreateRequest = z.object({
	name: createStringType(2, 30).describe('The name of the sticker (2-30 characters)'),
	description: createStringType(1, 500).nullish().describe('Description of the sticker (1-500 characters)'),
	tags: z
		.array(createStringType(1, 30))
		.min(0)
		.max(MAX_GUILD_STICKER_TAGS)
		.optional()
		.default([])
		.describe(`Array of autocomplete/suggestion tags (max ${MAX_GUILD_STICKER_TAGS} tags, each 1-30 characters)`),
	image: createBase64StringType(1, Math.ceil(STICKER_MAX_SIZE * (4 / 3))).describe(
		'Base64-encoded image data for the sticker',
	),
});

export type GuildStickerCreateRequest = z.infer<typeof GuildStickerCreateRequest>;

export const GuildStickerUpdateRequest = GuildStickerCreateRequest.pick({
	name: true,
	description: true,
	tags: true,
});

export type GuildStickerUpdateRequest = z.infer<typeof GuildStickerUpdateRequest>;

export const GuildStickerBulkCreateRequest = z.object({
	stickers: z
		.array(GuildStickerCreateRequest)
		.min(1, 'At least one sticker is required')
		.max(50, 'Maximum 50 stickers per batch')
		.describe('Array of sticker objects to create (1-50 stickers per batch)'),
});

export type GuildStickerBulkCreateRequest = z.infer<typeof GuildStickerBulkCreateRequest>;

export const GuildStickerCloneRequest = z.object({
	source_sticker_id: SnowflakeType.describe(
		'The ID of the existing sticker to clone. Its name, description, tags, and image are copied as-is; no other fields are accepted',
	),
});

export type GuildStickerCloneRequest = z.infer<typeof GuildStickerCloneRequest>;

export const GuildTransferOwnershipRequest = z.object({
	new_owner_id: SnowflakeType.describe('The ID of the user to transfer ownership to'),
	password: PasswordType.optional().describe('The current owner password for verification'),
});

export type GuildTransferOwnershipRequest = z.infer<typeof GuildTransferOwnershipRequest>;

export const GuildBanCreateRequest = z.object({
	delete_message_days: z
		.number()
		.int()
		.min(0)
		.max(7)
		.default(0)
		.describe(
			'Number of days of messages to delete from the banned user (0-7). Deprecated in favor of delete_message_seconds.',
		),
	delete_message_seconds: z
		.number()
		.int()
		.min(0)
		.max(604800)
		.optional()
		.describe('Number of seconds of messages to delete for the banned user (0-604800, default 0)'),
	reason: createStringType(0, 512).nullish().describe('The reason for the ban (max 512 characters)'),
	ban_duration_seconds: z
		.number()
		.int()
		.refine((val) => val === 0 || (val >= MIN_TEMP_BAN_DURATION_SECONDS && val <= MAX_TEMP_BAN_DURATION_SECONDS), {
			message: `Ban duration must be 0 (permanent) or between ${MIN_TEMP_BAN_DURATION_SECONDS} and ${MAX_TEMP_BAN_DURATION_SECONDS} seconds`,
		})
		.optional()
		.describe(
			`Duration of the ban in seconds (0 for permanent, or between ${MIN_TEMP_BAN_DURATION_SECONDS} and ${MAX_TEMP_BAN_DURATION_SECONDS} seconds for a temporary ban)`,
		),
});

export type GuildBanCreateRequest = z.infer<typeof GuildBanCreateRequest>;

export const GuildListQuery = z.object({
	before: SnowflakeType.optional().describe('Get guilds before this guild ID'),
	after: SnowflakeType.optional().describe('Get guilds after this guild ID'),
	limit: z.coerce.number().int().min(1).max(200).default(200).describe('Maximum number of guilds to return (1-200)'),
	with_counts: QueryBooleanType.describe('Include approximate member and presence counts'),
});

export type GuildListQuery = z.infer<typeof GuildListQuery>;

export const GuildLeaveQuery = z.object({
	delete_messages: QueryBooleanType.optional().describe(
		'Also delete every message the caller has authored in the guild before leaving',
	),
});

export type GuildLeaveQuery = z.infer<typeof GuildLeaveQuery>;

export const GuildDeleteRequest = z
	.object({
		password: PasswordType.optional().describe('The owner password for verification'),
	})
	.merge(SudoVerificationSchema);

export type GuildDeleteRequest = z.infer<typeof GuildDeleteRequest>;

export const GuildVanityURLUpdateRequest = z.object({
	code: VanityURLCodeType.nullish().describe('The new vanity URL code (2-32 characters, alphanumeric and hyphens)'),
});

export type GuildVanityURLUpdateRequest = z.infer<typeof GuildVanityURLUpdateRequest>;

export const GuildVanityURLUpdateResponse = z.object({
	code: createStringType(2, 32).nullable().describe('The new vanity URL code, or null when the vanity URL was removed'),
});

export type GuildVanityURLUpdateResponse = z.infer<typeof GuildVanityURLUpdateResponse>;

const GuildRoleHoistPositionItem = z.object({
	id: SnowflakeType.describe('The ID of the role'),
	hoist_position: z.number().int().describe('The new hoist position for the role'),
});

export const GuildRoleHoistPositionsRequest = z.array(GuildRoleHoistPositionItem);

export type GuildRoleHoistPositionsRequest = z.infer<typeof GuildRoleHoistPositionsRequest>;

const GuildRolePositionItem = z.object({
	id: SnowflakeType.describe('The ID of the role'),
	position: z.number().int().optional().describe('The new position for the role'),
});

export const GuildRolePositionsRequest = z.array(GuildRolePositionItem);

export type GuildRolePositionsRequest = z.infer<typeof GuildRolePositionsRequest>;

export const GuildMemberListQuery = z.object({
	limit: z.coerce
		.number()
		.int()
		.min(1)
		.max(1000)
		.default(1)
		.describe('Maximum number of members to return (1-1000, default 1)'),
	after: SnowflakeType.optional().describe('Get members after this user ID for pagination'),
});

export type GuildMemberListQuery = z.infer<typeof GuildMemberListQuery>;
