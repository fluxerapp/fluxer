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

import {MAX_GROUP_DM_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {type UserPartial, UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {ChannelOverwriteTypeSchema, ChannelTypeSchema} from '@fluxer/schema/src/primitives/ChannelValidators';
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
	rtc_region: z.string().nullish().describe('The voice region ID for the voice channel'),
	last_message_id: SnowflakeStringType.nullish().describe('The ID of the last message sent in this channel'),
	last_pin_timestamp: z.iso
		.datetime()
		.nullish()
		.describe('The ISO 8601 timestamp of when the last pinned message was pinned'),
	permission_overwrites: z
		.array(ChannelOverwriteResponse)
		.max(500)
		.optional()
		.describe('The permission overwrites for this channel'),
	recipients: z
		.array(z.lazy(() => UserPartialResponse))
		.max(MAX_GROUP_DM_RECIPIENTS)
		.optional()
		.describe('The recipients of the DM channel'),
	nsfw: z.boolean().optional().describe('Whether the channel is marked as NSFW'),
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

export const ChannelPartialRecipientResponse = z.object({
	username: z.string().describe('The username of the recipient'),
});
export type ChannelPartialRecipientResponse = z.infer<typeof ChannelPartialRecipientResponse>;

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

export const ChannelListResponse = z.array(ChannelResponse).max(500).describe('A list of channels');
export type ChannelListResponse = z.infer<typeof ChannelListResponse>;

export const RtcRegionListResponse = z.array(RtcRegionResponse).max(100).describe('A list of RTC regions');
export type RtcRegionListResponse = z.infer<typeof RtcRegionListResponse>;

export interface ChannelOverwrite {
	readonly id: string;
	readonly type: number;
	readonly allow: string;
	readonly deny: string;
}

export interface DefaultReactionEmoji {
	readonly emoji_id: string | null;
	readonly emoji_name: string | null;
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
	readonly rtc_region?: string | null;
	readonly last_message_id?: string | null;
	readonly last_pin_timestamp?: string | null;
	readonly permission_overwrites?: ReadonlyArray<ChannelOverwrite>;
	readonly recipients?: ReadonlyArray<UserPartial>;
	readonly nsfw?: boolean;
	readonly rate_limit_per_user?: number;
	readonly nicks?: Readonly<Record<string, string>>;
	readonly flags?: number;
	readonly member_count?: number;
	readonly message_count?: number;
	readonly total_message_sent?: number;
	readonly default_reaction_emoji?: DefaultReactionEmoji | null;
}
