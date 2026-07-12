// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ChannelID,
	GuildID,
	InviteCode,
	MessageID,
	RoleID,
	UserID,
	WebhookID,
	WebhookToken,
} from '../../BrandedTypes';

type Nullish<T> = T | null;

export interface PermissionOverwrite {
	type: number;
	allow_: Nullish<bigint>;
	deny_: Nullish<bigint>;
}

export interface ThreadMetadata {
	archived: boolean;
	locked: boolean;
	auto_archive_duration: number;
	archive_timestamp: Nullish<Date>;
}

export interface ChannelRow {
	channel_id: ChannelID;
	guild_id: Nullish<GuildID>;
	type: number;
	name: Nullish<string>;
	topic: Nullish<string>;
	icon_hash: Nullish<string>;
	url: Nullish<string>;
	parent_id: Nullish<ChannelID>;
	position: Nullish<number>;
	owner_id: Nullish<UserID>;
	recipient_ids: Nullish<Set<UserID>>;
	nsfw: Nullish<boolean>;
	content_warning_level?: Nullish<number>;
	content_warning_text?: Nullish<string>;
	rate_limit_per_user: Nullish<number>;
	bitrate: Nullish<number>;
	user_limit: Nullish<number>;
	voice_connection_limit: Nullish<number>;
	rtc_region: Nullish<string>;
	last_message_id: Nullish<MessageID>;
	last_pin_timestamp: Nullish<Date>;
	permission_overwrites: Nullish<Map<RoleID | UserID, PermissionOverwrite>>;
	nicks: Nullish<Map<string, string>>;
	soft_deleted: boolean;
	indexed_at: Nullish<Date>;
	version: number;
	thread_metadata: Nullish<ThreadMetadata>;
	message_count: Nullish<number>;
	total_message_sent: Nullish<number>;
	member_count: Nullish<number>;
}

export interface InviteRow {
	code: InviteCode;
	type: number;
	guild_id: Nullish<GuildID>;
	channel_id: Nullish<ChannelID>;
	inviter_id: Nullish<UserID>;
	created_at: Date;
	uses: number;
	max_uses: number;
	max_age: number;
	temporary: Nullish<boolean>;
	version: number;
}

export interface WebhookRow {
	webhook_id: WebhookID;
	webhook_token: WebhookToken;
	type: number;
	guild_id: Nullish<GuildID>;
	channel_id: Nullish<ChannelID>;
	creator_id: Nullish<UserID>;
	name: string;
	avatar_hash: Nullish<string>;
	version: number;
}

export interface PrivateChannelRow {
	user_id: UserID;
	channel_id: ChannelID;
	is_gdm: boolean;
	channel_type?: Nullish<number>;
	channel_name?: Nullish<string>;
	channel_icon_hash?: Nullish<string>;
	channel_owner_id?: Nullish<UserID>;
	channel_recipient_ids?: Nullish<Set<UserID>>;
	channel_last_message_id?: Nullish<MessageID>;
	channel_last_pin_timestamp?: Nullish<Date>;
	channel_nicks?: Nullish<Map<string, string>>;
	channel_rate_limit_per_user?: Nullish<number>;
	channel_nsfw?: Nullish<boolean>;
	channel_version?: Nullish<number>;
	snapshot_at?: Nullish<Date>;
}

export interface DmStateRow {
	hi_user_id: UserID;
	lo_user_id: UserID;
	channel_id: ChannelID;
}

export interface ReadStateRow {
	user_id: UserID;
	channel_id: ChannelID;
	message_id: Nullish<MessageID>;
	mention_count: number;
	last_pin_timestamp: Nullish<Date>;
}

export const CHANNEL_COLUMNS = [
	'channel_id',
	'guild_id',
	'type',
	'name',
	'topic',
	'icon_hash',
	'url',
	'parent_id',
	'position',
	'owner_id',
	'recipient_ids',
	'nsfw',
	'content_warning_level',
	'content_warning_text',
	'rate_limit_per_user',
	'bitrate',
	'user_limit',
	'voice_connection_limit',
	'rtc_region',
	'last_message_id',
	'last_pin_timestamp',
	'permission_overwrites',
	'nicks',
	'soft_deleted',
	'indexed_at',
	'version',
	'thread_metadata',
	'message_count',
	'total_message_sent',
	'member_count',
] as const satisfies ReadonlyArray<keyof ChannelRow>;

export interface ChannelsByGuildRow {
	guild_id: GuildID;
	channel_id: ChannelID;
}

export const CHANNELS_BY_GUILD_COLUMNS = ['guild_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof ChannelsByGuildRow
>;

export interface ThreadMemberRow {
	thread_id: ChannelID;
	user_id: UserID;
	join_timestamp: Date;
}

export const THREAD_MEMBER_COLUMNS = ['thread_id', 'user_id', 'join_timestamp'] as const satisfies ReadonlyArray<
	keyof ThreadMemberRow
>;

export interface ThreadMembersByUserRow {
	user_id: UserID;
	guild_id: GuildID;
	thread_id: ChannelID;
	join_timestamp: Date;
}

export const THREAD_MEMBERS_BY_USER_COLUMNS = [
	'user_id',
	'guild_id',
	'thread_id',
	'join_timestamp',
] as const satisfies ReadonlyArray<keyof ThreadMembersByUserRow>;

export interface ThreadsByParentRow {
	parent_id: ChannelID;
	thread_id: ChannelID;
	archived: Nullish<boolean>;
}

export const THREADS_BY_PARENT_COLUMNS = ['parent_id', 'thread_id', 'archived'] as const satisfies ReadonlyArray<
	keyof ThreadsByParentRow
>;

export interface ThreadsByGuildRow {
	guild_id: GuildID;
	thread_id: ChannelID;
	parent_id: Nullish<ChannelID>;
	archived: Nullish<boolean>;
}

export const THREADS_BY_GUILD_COLUMNS = [
	'guild_id',
	'thread_id',
	'parent_id',
	'archived',
] as const satisfies ReadonlyArray<keyof ThreadsByGuildRow>;

export interface ThreadArchiveDueRow {
	due_bucket: number;
	archive_due_at: Date;
	thread_id: ChannelID;
	guild_id: GuildID;
	parent_id: Nullish<ChannelID>;
}

export const THREAD_ARCHIVE_DUE_COLUMNS = [
	'due_bucket',
	'archive_due_at',
	'thread_id',
	'guild_id',
	'parent_id',
] as const satisfies ReadonlyArray<keyof ThreadArchiveDueRow>;
export const INVITE_COLUMNS = [
	'code',
	'type',
	'guild_id',
	'channel_id',
	'inviter_id',
	'created_at',
	'uses',
	'max_uses',
	'max_age',
	'temporary',
	'version',
] as const satisfies ReadonlyArray<keyof InviteRow>;
export const WEBHOOK_COLUMNS = [
	'webhook_id',
	'webhook_token',
	'type',
	'guild_id',
	'channel_id',
	'creator_id',
	'name',
	'avatar_hash',
	'version',
] as const satisfies ReadonlyArray<keyof WebhookRow>;
export const READ_STATE_COLUMNS = [
	'user_id',
	'channel_id',
	'message_id',
	'mention_count',
	'last_pin_timestamp',
] as const satisfies ReadonlyArray<keyof ReadStateRow>;
export const PRIVATE_CHANNEL_COLUMNS = [
	'user_id',
	'channel_id',
	'is_gdm',
	'channel_type',
	'channel_name',
	'channel_icon_hash',
	'channel_owner_id',
	'channel_recipient_ids',
	'channel_last_message_id',
	'channel_last_pin_timestamp',
	'channel_nicks',
	'channel_rate_limit_per_user',
	'channel_nsfw',
	'channel_version',
	'snapshot_at',
] as const satisfies ReadonlyArray<keyof PrivateChannelRow>;
export const DM_STATE_COLUMNS = ['hi_user_id', 'lo_user_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof DmStateRow
>;
