// SPDX-License-Identifier: AGPL-3.0-or-later

import {CONTENT_WARNING_TEXT_MAX_LENGTH} from '@fluxer/constants/src/GuildConstants';
import {MAX_GROUP_DM_OTHER_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
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

export const VoiceDebugLoggingStatusResponse = z.object({
	active: z.boolean().describe('Whether clients in this channel should currently send voice diagnostics'),
	session_id: z.string().nullable().describe('Current debug logging session id, if active'),
	activated_by_user_id: SnowflakeStringType.nullable().describe('Staff user that activated the session, if active'),
	started_at_ms: z.number().int().nonnegative().nullable().describe('Session start Unix timestamp in milliseconds'),
	expires_at_ms: z
		.number()
		.int()
		.nonnegative()
		.nullable()
		.describe('Session expiration Unix timestamp in milliseconds'),
	poll_interval_ms: Int32Type.describe('Recommended client polling interval in milliseconds'),
	upload_interval_ms: Int32Type.describe('Recommended client telemetry batch upload interval in milliseconds'),
});

export type VoiceDebugLoggingStatusResponse = z.infer<typeof VoiceDebugLoggingStatusResponse>;

export const VoiceDebugLoggingEventsResponse = z.object({
	accepted: z.boolean().describe('Whether the telemetry batch was accepted for storage'),
	active: z.boolean().describe('Whether the server still considers this logging session active'),
	stored_event_count: Int32Type.describe('Number of events written to diagnostics storage'),
});

export type VoiceDebugLoggingEventsResponse = z.infer<typeof VoiceDebugLoggingEventsResponse>;

export const VoicePresenceHeartbeatResponse = z.object({
	ok: z.boolean().describe('Whether the heartbeat was accepted'),
	heartbeat_interval_ms: Int32Type.describe('Recommended client heartbeat interval in milliseconds'),
	heartbeat_ttl_ms: Int32Type.describe('Server-side heartbeat expiration window in milliseconds'),
	expires_at_ms: z.number().int().nonnegative().describe('Unix timestamp in milliseconds when this heartbeat expires'),
});

export type VoicePresenceHeartbeatResponse = z.infer<typeof VoicePresenceHeartbeatResponse>;

export const VoicePresenceHeartbeatEndResponse = z.object({
	ok: z.boolean().describe('Whether the heartbeat was ended'),
});

export type VoicePresenceHeartbeatEndResponse = z.infer<typeof VoicePresenceHeartbeatEndResponse>;

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
		.max(500)
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
		.max(MAX_GROUP_DM_OTHER_RECIPIENTS)
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


export const ThreadPreviewCard = z.object({
	last_message_preview: z.string().nullish().describe('Truncated content of the last message in the thread'),
	last_message_at: z.iso.datetime().nullish().describe('Timestamp of the last message in the thread'),
	last_message_author_id: SnowflakeStringType.nullish().describe('User ID of the last message author'),
	last_message_author_username: z.string().nullish().describe('Username of the last message author'),
	last_message_author_avatar: z.string().nullish().describe('Avatar hash of the last message author'),
	recent_member_avatars: z.array(z.object({
		id: SnowflakeStringType.describe('User ID'),
		avatar: z.string().nullish().describe('Avatar hash'),
	})).max(3).nullish().describe('Up to 3 recent member avatars for the thread card'),
});

export type ThreadPreviewCard = z.infer<typeof ThreadPreviewCard>;

export const ThreadResponse = ChannelResponse.extend({
	thread_state: z.number().int().describe('Thread state: 0=open, 1=closed, 2=archived'),
	thread_metadata: z.object({
		archived: z.boolean().describe('Whether the thread is archived'),
		locked: z.boolean().describe('Whether the thread is locked (only MANAGE_THREADS can unarchive a locked thread)'),
		auto_archive_duration: z.number().int().describe('Duration in minutes after which the thread auto-archives (60, 1440, 4320, or 10080)'),
		archive_timestamp: z.iso.datetime().nullish().describe('ISO 8601 timestamp when the thread was archived, or null'),
	}).optional().describe('Discord-compatible thread metadata object'),
	thread_parent_channel_id: SnowflakeStringType.describe('ID of the parent channel this thread belongs to'),
	thread_creator_id: SnowflakeStringType.nullish().describe('ID of the user who created the thread'),
	thread_creator_username: z.string().nullish().describe('Username of the thread creator at time of creation'),
	thread_expires_at: z.iso.datetime().nullish().describe('ISO 8601 timestamp when the thread auto-closes'),
	thread_member_count: Int32Type.optional().describe('Approximate number of members in the thread (from message count field)'),
	thread_total_message_sent: Int32Type.optional().describe('Total messages ever sent in the thread (non-decrementing)'),
	thread_member_count_actual: Int32Type.optional().describe('Actual member count, capped at 50'),
	thread_source_message_id: SnowflakeStringType.nullish().describe('ID of the message this thread was started from'),
}).merge(ThreadPreviewCard);

export type ThreadResponse = z.infer<typeof ThreadResponse>;

export const ThreadMemberResponse = z.object({
	thread_id: SnowflakeStringType.describe('The thread ID'),
	user_id: SnowflakeStringType.describe('The user ID'),
	joined_at: z.iso.datetime().describe('When the user joined the thread'),
	notification_override: z.number().int().nullish().describe('Per-thread notification override (null = inherit from channel)'),
});

export type ThreadMemberResponse = z.infer<typeof ThreadMemberResponse>;
