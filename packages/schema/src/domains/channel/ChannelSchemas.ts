// SPDX-License-Identifier: AGPL-3.0-or-later

import {CONTENT_WARNING_TEXT_MAX_LENGTH} from '@fluxer/constants/src/GuildConstants';
import {MAX_GROUP_DM_OTHER_RECIPIENTS, MAX_GROUP_DM_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {type UserPartial, UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {ChannelOverwriteTypeSchema, ChannelTypeSchema} from '@fluxer/schema/src/primitives/ChannelValidators';
import {ContentWarningLevelSchema} from '@fluxer/schema/src/primitives/GuildValidators';
import {PermissionStringType} from '@fluxer/schema/src/primitives/PermissionValidators';
import {createStringType, Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ChannelOverwriteResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for the role or user this overwrite applies to'),
	type: ChannelOverwriteTypeSchema.describe('The type of entity the overwrite applies to'),
	allow: PermissionStringType.describe('fluxer:PermissionStringType The bitwise value of allowed permissions'),
	deny: PermissionStringType.describe('fluxer:PermissionStringType The bitwise value of denied permissions'),
});

export type ChannelOverwriteResponse = z.infer<typeof ChannelOverwriteResponse>;

export const RtcRegionResponse = z.object({
	id: z.string().describe('The unique identifier for this RTC region'),
	name: z.string().describe('The display name of the RTC region'),
	emoji: z.string().describe('The emoji associated with this RTC region'),
});

export type RtcRegionResponse = z.infer<typeof RtcRegionResponse>;

export const ChannelSlowmodeStateResponse = z.object({
	rate_limit_per_user: Int32Type.describe('The configured slowmode interval in seconds (0 if disabled)'),
	retry_after_ms: Int32Type.describe(
		'Milliseconds the current user must wait before sending the next message (0 if allowed now)',
	),
	next_send_allowed_at: z.iso
		.datetime()
		.nullable()
		.describe('Absolute timestamp at which the current user is next allowed to send a message, or null if allowed now'),
	can_bypass: z.boolean().describe('Whether the current user has permission to bypass slowmode'),
});

export type ChannelSlowmodeStateResponse = z.infer<typeof ChannelSlowmodeStateResponse>;

export const CallEligibilityResponse = z.object({
	ringable: z.boolean().describe('Whether the current user can ring this call'),
	silent: z.boolean().describe('Whether the call should be joined silently'),
});

export type CallEligibilityResponse = z.infer<typeof CallEligibilityResponse>;

export const ChannelResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this channel'),
	guild_id: SnowflakeStringType.optional().describe('The ID of the guild this channel belongs to'),
	name: z.string().optional().describe('The name of the channel'),
	topic: z.string().nullish().describe('The topic of the channel'),
	url: z.url().nullish().describe('The URL associated with the channel'),
	icon: z.string().nullish().describe('The icon hash of the channel (for group DMs)'),
	owner_id: SnowflakeStringType.nullish().describe('The ID of the owner of the channel (for group DMs)'),
	type: ChannelTypeSchema.describe('The type of the channel'),
	position: Int32Type.optional().describe('The sorting position of the channel'),
	parent_id: SnowflakeStringType.nullish().describe('The ID of the parent category for this channel'),
	bitrate: Int32Type.nullish().describe('The bitrate of the voice channel in bits per second'),
	user_limit: Int32Type.nullish().describe('The maximum number of users allowed in the voice channel'),
	voice_connection_limit: Int32Type.nullish().describe(
		'The maximum active voice connections allowed per user in the voice channel',
	),
	rtc_region: z.string().nullish().describe('The voice region ID for the voice channel'),
	last_message_id: SnowflakeStringType.nullish().describe('The ID of the last message sent in this channel'),
	last_pin_timestamp: z.iso
		.datetime()
		.nullish()
		.describe('The ISO 8601 timestamp of when the last pinned message was pinned'),
	permission_overwrites: z
		.array(ChannelOverwriteResponse)
		.optional()
		.describe('The permission overwrites for this channel'),
	recipients: z
		.array(z.lazy(() => UserPartialResponse))
		.max(MAX_GROUP_DM_OTHER_RECIPIENTS)
		.optional()
		.describe('The recipients of the DM channel'),
	nsfw: z
		.boolean()
		.optional()
		.describe('Whether the channel is marked as NSFW (effective value, walking channel → category → guild)'),
	nsfw_override: z
		.boolean()
		.nullish()
		.describe(
			'Per-channel adult-content override; null means inherit from parent category and then guild. Categories use this same field as their own override.',
		),
	content_warning_level: ContentWarningLevelSchema.optional().describe(
		'Channel-level content warning override (0=inherit, 1=force-warn)',
	),
	content_warning_text: z
		.string()
		.max(CONTENT_WARNING_TEXT_MAX_LENGTH)
		.nullish()
		.describe('Custom channel content warning text (max 200 characters); null inherits from parent or guild'),
	rate_limit_per_user: Int32Type.optional().describe('The slowmode rate limit in seconds'),
	nicks: z
		.record(z.string(), createStringType(1, 32))
		.optional()
		.describe('Custom nicknames for users in this channel (for group DMs)'),
});

export type ChannelResponse = z.infer<typeof ChannelResponse>;

export const ChannelNicknameOverrides = z
	.record(
		z.string().describe('User ID'),
		z.union([createStringType(0, 32), z.null()]).describe('Nickname or null to clear'),
	)
	.describe('User nickname overrides (user ID to nickname mapping)');

export type ChannelNicknameOverrides = z.infer<typeof ChannelNicknameOverrides>;

const ChannelPartialRecipientResponse = z.object({
	username: z.string().describe('The username of the recipient'),
});

export const ChannelPartialResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier (snowflake) for this channel'),
	name: z.string().nullish().describe('The name of the channel'),
	type: ChannelTypeSchema.describe('The type of the channel'),
	recipients: z
		.array(ChannelPartialRecipientResponse)
		.max(MAX_GROUP_DM_RECIPIENTS)
		.optional()
		.describe('The recipients of the DM channel'),
});

export type ChannelPartialResponse = z.infer<typeof ChannelPartialResponse>;

export interface ChannelOverwrite {
	readonly id: string;
	readonly type: number;
	readonly allow: string;
	readonly deny: string;
}

export interface Channel {
	readonly id: string;
	readonly guild_id?: string;
	readonly name?: string;
	readonly topic?: string | null;
	readonly url?: string | null;
	readonly icon?: string | null;
	readonly owner_id?: string | null;
	readonly type: number;
	readonly position?: number;
	readonly parent_id?: string | null;
	readonly bitrate?: number | null;
	readonly user_limit?: number | null;
	readonly voice_connection_limit?: number | null;
	readonly rtc_region?: string | null;
	readonly last_message_id?: string | null;
	readonly last_pin_timestamp?: string | null;
	readonly permission_overwrites?: ReadonlyArray<ChannelOverwrite>;
	readonly recipients?: ReadonlyArray<UserPartial>;
	readonly nsfw?: boolean;
	readonly nsfw_override?: boolean | null;
	readonly content_warning_level?: number;
	readonly content_warning_text?: string | null;
	readonly rate_limit_per_user?: number;
	readonly nicks?: Readonly<Record<string, string>>;
}
