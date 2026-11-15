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
} from '@app/api/BrandedTypes';

export interface PermissionOverwrite {
	type: number;
	allow_: bigint | null;
	deny_: bigint | null;
}

export interface ChannelRow {
	channel_id: ChannelID;
	guild_id: GuildID | null;
	type: number;
	name: string | null;
	topic: string | null;
	icon_hash: string | null;
	url: string | null;
	parent_id: ChannelID | null;
	position: number | null;
	owner_id: UserID | null;
	recipient_ids: Set<UserID> | null;
	nsfw: boolean | null;
	content_warning_level?: number | null;
	content_warning_text?: string | null;
	rate_limit_per_user: number | null;
	bitrate: number | null;
	user_limit: number | null;
	voice_connection_limit: number | null;
	rtc_region: string | null;
	rtc_p2p: boolean | null;
	last_message_id: MessageID | null;
	last_pin_timestamp: Date | null;
	permission_overwrites: Map<RoleID | UserID, PermissionOverwrite> | null;
	nicks: Map<string, string> | null;
	soft_deleted: boolean;
	indexed_at: Date | null;
	version: number;
}

export interface InviteRow {
	code: InviteCode;
	type: number;
	guild_id: GuildID | null;
	channel_id: ChannelID | null;
	inviter_id: UserID | null;
	created_at: Date;
	uses: number;
	max_uses: number;
	max_age: number;
	temporary: boolean | null;
	version: number;
}

export interface WebhookRow {
	webhook_id: WebhookID;
	webhook_token: WebhookToken;
	type: number;
	guild_id: GuildID | null;
	channel_id: ChannelID | null;
	creator_id: UserID | null;
	name: string;
	avatar_hash: string | null;
	source_guild_id: GuildID | null;
	source_channel_id: ChannelID | null;
	version: number;
}

export interface WebhooksBySourceChannelRow {
	source_channel_id: ChannelID;
	webhook_id: WebhookID;
	guild_id: GuildID;
}

type CrosspostedMessageState = 'pending' | 'delivered';

export interface CrosspostedMessageRow {
	source_message_id: MessageID;
	webhook_id: WebhookID;
	source_channel_id: ChannelID;
	target_guild_id: GuildID;
	target_channel_id: ChannelID;
	target_message_id: MessageID;
	state: CrosspostedMessageState;
	reserved_at: Date;
	source_fingerprint: string | null;
	created_at: Date;
}

export interface CrosspostSourceByChannelRow {
	source_channel_id: ChannelID;
	source_message_id: MessageID;
}

export interface PrivateChannelRow {
	user_id: UserID;
	channel_id: ChannelID;
	is_gdm: boolean;
	channel_type?: number | null;
	channel_name?: string | null;
	channel_icon_hash?: string | null;
	channel_owner_id?: UserID | null;
	channel_recipient_ids?: Set<UserID> | null;
	channel_last_message_id?: MessageID | null;
	channel_last_pin_timestamp?: Date | null;
	channel_nicks?: Map<string, string> | null;
	channel_rate_limit_per_user?: number | null;
	channel_nsfw?: boolean | null;
	channel_version?: number | null;
	snapshot_at?: Date | null;
}

export interface DmStateRow {
	hi_user_id: UserID;
	lo_user_id: UserID;
	channel_id: ChannelID;
}

export interface ReadStateRow {
	user_id: UserID;
	channel_id: ChannelID;
	message_id: MessageID | null;
	mention_count: number;
	last_pin_timestamp: Date | null;
	flags?: number | null;
	guild_id?: GuildID | null;
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
	'rtc_p2p',
	'last_message_id',
	'last_pin_timestamp',
	'permission_overwrites',
	'nicks',
	'soft_deleted',
	'indexed_at',
	'version',
] as const satisfies ReadonlyArray<keyof ChannelRow>;

export interface ChannelsByGuildRow {
	guild_id: GuildID;
	channel_id: ChannelID;
}

export const CHANNELS_BY_GUILD_COLUMNS = ['guild_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof ChannelsByGuildRow
>;
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
	'source_guild_id',
	'source_channel_id',
	'version',
] as const satisfies ReadonlyArray<keyof WebhookRow>;
export const WEBHOOKS_BY_SOURCE_CHANNEL_COLUMNS = [
	'source_channel_id',
	'webhook_id',
	'guild_id',
] as const satisfies ReadonlyArray<keyof WebhooksBySourceChannelRow>;
export const CROSSPOSTED_MESSAGE_COLUMNS = [
	'source_message_id',
	'webhook_id',
	'source_channel_id',
	'target_guild_id',
	'target_channel_id',
	'target_message_id',
	'state',
	'reserved_at',
	'source_fingerprint',
	'created_at',
] as const satisfies ReadonlyArray<keyof CrosspostedMessageRow>;
export const CROSSPOST_SOURCE_BY_CHANNEL_COLUMNS = [
	'source_channel_id',
	'source_message_id',
] as const satisfies ReadonlyArray<keyof CrosspostSourceByChannelRow>;
export const READ_STATE_COLUMNS = [
	'user_id',
	'channel_id',
	'message_id',
	'mention_count',
	'last_pin_timestamp',
	'flags',
	'guild_id',
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
