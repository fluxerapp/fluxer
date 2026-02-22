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

import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import {
	GuildFeatures,
	GuildOperations,
	GuildOperationsDescriptions,
	SystemChannelFlags,
	SystemChannelFlagsDescriptions,
} from '@fluxer/constants/src/GuildConstants';
import {
	DefaultMessageNotificationsSchema,
	GuildExplicitContentFilterSchema,
	GuildMFALevelSchema,
	GuildVerificationLevelSchema,
	NSFWLevelSchema,
	SplashCardAlignmentSchema,
} from '@fluxer/schema/src/primitives/GuildValidators';
import {PermissionStringType} from '@fluxer/schema/src/primitives/PermissionValidators';
import {
	createBitflagInt32Type,
	createFlexibleStringLiteralUnion,
	Int32Type,
	SnowflakeStringType,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

function normalizeGuildFeatures(features: Array<string>): Array<string> {
	return Array.from(new Set(features)).sort((first, second) => first.localeCompare(second));
}

export const GuildFeatureSchema = withOpenApiType(
	createFlexibleStringLiteralUnion(
		[
			[GuildFeatures.ANIMATED_ICON, 'ANIMATED_ICON', 'Guild can have an animated icon'],
			[GuildFeatures.ANIMATED_BANNER, 'ANIMATED_BANNER', 'Guild can have an animated banner'],
			[GuildFeatures.BANNER, 'BANNER', 'Guild can have a banner'],
			[GuildFeatures.DETACHED_BANNER, 'DETACHED_BANNER', 'Guild banner is detached from splash'],
			[GuildFeatures.INVITE_SPLASH, 'INVITE_SPLASH', 'Guild can have an invite splash'],
			[GuildFeatures.INVITES_DISABLED, 'INVITES_DISABLED', 'Guild has invites disabled'],
			[
				GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES,
				'TEXT_CHANNEL_FLEXIBLE_NAMES',
				'Guild allows flexible text channel names',
			],
			[GuildFeatures.MORE_EMOJI, 'MORE_EMOJI', 'Guild has increased emoji slots'],
			[GuildFeatures.MORE_STICKERS, 'MORE_STICKERS', 'Guild has increased sticker slots'],
			[GuildFeatures.UNLIMITED_EMOJI, 'UNLIMITED_EMOJI', 'Guild has unlimited emoji slots'],
			[GuildFeatures.UNLIMITED_STICKERS, 'UNLIMITED_STICKERS', 'Guild has unlimited sticker slots'],
			[GuildFeatures.EXPRESSION_PURGE_ALLOWED, 'EXPRESSION_PURGE_ALLOWED', 'Guild allows purging expressions'],
			[GuildFeatures.VANITY_URL, 'VANITY_URL', 'Guild can have a vanity URL'],
			[GuildFeatures.VERIFIED, 'VERIFIED', 'Guild is verified'],
			[GuildFeatures.VIP_VOICE, 'VIP_VOICE', 'Guild has VIP voice features'],
			[GuildFeatures.UNAVAILABLE_FOR_EVERYONE, 'UNAVAILABLE_FOR_EVERYONE', 'Guild is unavailable for everyone'],
			[
				GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF,
				'UNAVAILABLE_FOR_EVERYONE_BUT_STAFF',
				'Guild is unavailable except for staff',
			],
			[GuildFeatures.VISIONARY, 'VISIONARY', 'Guild is a visionary guild'],
			[GuildFeatures.OPERATOR, 'OPERATOR', 'Guild is an operator guild'],
			[GuildFeatures.LARGE_GUILD_OVERRIDE, 'LARGE_GUILD_OVERRIDE', 'Guild has large guild overrides enabled'],
			[GuildFeatures.VERY_LARGE_GUILD, 'VERY_LARGE_GUILD', 'Guild has increased member capacity enabled'],
		],
		'A guild feature flag',
	),
	'GuildFeature',
);

const GuildFeatureListSchema = z
	.array(GuildFeatureSchema)
	.max(100)
	.transform(normalizeGuildFeatures)
	.describe('Array of guild feature flags');

export const GuildResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this guild'),
	name: z.string().describe('The name of the guild'),
	icon: z.string().nullish().describe('The hash of the guild icon'),
	banner: z.string().nullish().describe('The hash of the guild banner'),
	banner_width: Int32Type.nullish().describe('The width of the guild banner in pixels'),
	banner_height: Int32Type.nullish().describe('The height of the guild banner in pixels'),
	splash: z.string().nullish().describe('The hash of the guild splash screen'),
	splash_width: Int32Type.nullish().describe('The width of the guild splash in pixels'),
	splash_height: Int32Type.nullish().describe('The height of the guild splash in pixels'),
	splash_card_alignment: SplashCardAlignmentSchema.describe('The alignment of the splash card'),
	embed_splash: z.string().nullish().describe('The hash of the embedded invite splash'),
	embed_splash_width: Int32Type.nullish().describe('The width of the embedded invite splash in pixels'),
	embed_splash_height: Int32Type.nullish().describe('The height of the embedded invite splash in pixels'),
	vanity_url_code: z.string().nullish().describe('The vanity URL code for the guild'),
	owner_id: SnowflakeStringType.describe('The ID of the guild owner'),
	system_channel_id: SnowflakeStringType.nullish().describe('The ID of the channel where system messages are sent'),
	system_channel_flags: createBitflagInt32Type(
		SystemChannelFlags,
		SystemChannelFlagsDescriptions,
		'System channel message flags',
		'SystemChannelFlags',
	),
	rules_channel_id: SnowflakeStringType.nullish().describe('The ID of the rules channel'),
	afk_channel_id: SnowflakeStringType.nullish().describe('The ID of the AFK voice channel'),
	afk_timeout: Int32Type.describe('AFK timeout in seconds before moving users to the AFK channel'),
	features: GuildFeatureListSchema,
	verification_level: withFieldDescription(
		GuildVerificationLevelSchema,
		'Required verification level for members to participate',
	),
	mfa_level: withFieldDescription(GuildMFALevelSchema, 'Required MFA level for moderation actions'),
	nsfw_level: withFieldDescription(NSFWLevelSchema, 'The NSFW level of the guild'),
	explicit_content_filter: withFieldDescription(
		GuildExplicitContentFilterSchema,
		'Level of content filtering for explicit media',
	),
	default_message_notifications: withFieldDescription(
		DefaultMessageNotificationsSchema,
		'Default notification level for new members',
	),
	disabled_operations: createBitflagInt32Type(
		GuildOperations,
		GuildOperationsDescriptions,
		'Bitfield of disabled operations in the guild',
		'GuildOperations',
	),
	message_history_cutoff: z.iso
		.datetime()
		.nullish()
		.describe(
			'ISO8601 timestamp controlling how far back members without Read Message History can access messages. When null, no historical access is allowed.',
		),
	permissions: PermissionStringType.describe(
		'fluxer:PermissionStringType The current user permissions in this guild',
	).nullish(),
});

export type GuildResponse = z.infer<typeof GuildResponse>;

export const GuildPartialResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this guild'),
	name: z.string().describe('The name of the guild'),
	icon: z.string().nullish().describe('The hash of the guild icon'),
	banner: z.string().nullish().describe('The hash of the guild banner'),
	banner_width: Int32Type.nullish().describe('The width of the guild banner in pixels'),
	banner_height: Int32Type.nullish().describe('The height of the guild banner in pixels'),
	splash: z.string().nullish().describe('The hash of the guild splash screen'),
	splash_width: Int32Type.nullish().describe('The width of the guild splash in pixels'),
	splash_height: Int32Type.nullish().describe('The height of the guild splash in pixels'),
	splash_card_alignment: SplashCardAlignmentSchema.describe('The alignment of the splash card'),
	embed_splash: z.string().nullish().describe('The hash of the embedded invite splash'),
	embed_splash_width: Int32Type.nullish().describe('The width of the embedded invite splash in pixels'),
	embed_splash_height: Int32Type.nullish().describe('The height of the embedded invite splash in pixels'),
	features: GuildFeatureListSchema,
});

export type GuildPartialResponse = z.infer<typeof GuildPartialResponse>;

export const GuildVanityURLResponse = z.object({
	code: z.string().nullish().describe('The vanity URL code for the guild'),
	uses: Int32Type.describe('The number of times this vanity URL has been used'),
});

export type GuildVanityURLResponse = z.infer<typeof GuildVanityURLResponse>;

export const GuildListResponse = z.array(GuildResponse).max(200).describe('A list of guilds');
export type GuildListResponse = z.infer<typeof GuildListResponse>;

export interface Guild {
	readonly id: string;
	readonly name: string;
	readonly icon: string | null;
	readonly banner?: string | null;
	readonly banner_width?: number | null;
	readonly banner_height?: number | null;
	readonly splash?: string | null;
	readonly splash_width?: number | null;
	readonly splash_height?: number | null;
	readonly splash_card_alignment?: GuildSplashCardAlignmentValue;
	readonly embed_splash?: string | null;
	readonly embed_splash_width?: number | null;
	readonly embed_splash_height?: number | null;
	readonly vanity_url_code: string | null;
	readonly owner_id: string;
	readonly system_channel_id: string | null;
	readonly system_channel_flags?: number;
	readonly rules_channel_id?: string | null;
	readonly afk_channel_id?: string | null;
	readonly afk_timeout?: number;
	readonly features: ReadonlyArray<string>;
	readonly verification_level?: number;
	readonly mfa_level?: number;
	readonly nsfw_level?: number;
	readonly explicit_content_filter?: number;
	readonly default_message_notifications?: number;
	readonly disabled_operations?: number;
	readonly message_history_cutoff?: string | null;
	readonly joined_at?: string;
	readonly unavailable?: boolean;
	readonly member_count?: number;
}
