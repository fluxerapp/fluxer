// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ChannelID,
	EmojiID,
	GuildID,
	InviteCode,
	RoleID,
	StickerID,
	UserID,
	VanityURLCode,
} from '@app/api/BrandedTypes';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {GuildSplashCardAlignmentValue} from '@fluxer/constants/src/GuildConstants';
import type {MentionReplyPreference} from '@fluxer/constants/src/UserConstants';

export interface GuildRow {
	guild_id: GuildID;
	owner_id: UserID;
	name: string;
	vanity_url_code: VanityURLCode | null;
	icon_hash: string | null;
	banner_hash: string | null;
	banner_width: number | null;
	banner_height: number | null;
	splash_hash: string | null;
	splash_width: number | null;
	splash_height: number | null;
	splash_card_alignment: GuildSplashCardAlignmentValue | null;
	embed_splash_hash: string | null;
	embed_splash_width: number | null;
	embed_splash_height: number | null;
	features: Set<string> | null;
	verification_level: number;
	mfa_level: number;
	nsfw_level: number;
	nsfw?: boolean | null;
	content_warning_level?: number | null;
	content_warning_text?: string | null;
	explicit_content_filter: number;
	default_message_notifications: number;
	system_channel_id: ChannelID | null;
	system_channel_flags: number;
	rules_channel_id: ChannelID | null;
	afk_channel_id: ChannelID | null;
	afk_timeout: number;
	disabled_operations: number;
	member_count: number;
	audit_logs_indexed_at: Date | null;
	members_indexed_at: Date | null;
	message_history_cutoff: Date | null;
	invites_paused_until: Date | null;
	version: number;
}

export const GUILD_COLUMNS = [
	'guild_id',
	'owner_id',
	'name',
	'vanity_url_code',
	'icon_hash',
	'banner_hash',
	'banner_width',
	'banner_height',
	'splash_hash',
	'splash_width',
	'splash_height',
	'splash_card_alignment',
	'embed_splash_hash',
	'embed_splash_width',
	'embed_splash_height',
	'features',
	'verification_level',
	'mfa_level',
	'nsfw_level',
	'nsfw',
	'content_warning_level',
	'content_warning_text',
	'explicit_content_filter',
	'default_message_notifications',
	'system_channel_id',
	'system_channel_flags',
	'rules_channel_id',
	'afk_channel_id',
	'afk_timeout',
	'disabled_operations',
	'member_count',
	'audit_logs_indexed_at',
	'members_indexed_at',
	'message_history_cutoff',
	'invites_paused_until',
	'version',
] as const satisfies ReadonlyArray<keyof GuildRow>;

export interface GuildMemberRow {
	guild_id: GuildID;
	user_id: UserID;
	joined_at: Date;
	nick: string | null;
	avatar_hash: string | null;
	banner_hash: string | null;
	bio: string | null;
	pronouns: string | null;
	accent_color: number | null;
	join_source_type: number | null;
	source_invite_code: InviteCode | null;
	inviter_id: UserID | null;
	deaf: boolean;
	mute: boolean;
	communication_disabled_until: Date | null;
	role_ids: Set<RoleID> | null;
	is_premium_sanitized: boolean | null;
	temporary: boolean | null;
	profile_flags: number | null;
	mention_flags?: MentionReplyPreference | null;
	version: number;
}

export const GUILD_MEMBER_COLUMNS = [
	'guild_id',
	'user_id',
	'joined_at',
	'nick',
	'avatar_hash',
	'banner_hash',
	'bio',
	'pronouns',
	'accent_color',
	'join_source_type',
	'source_invite_code',
	'inviter_id',
	'deaf',
	'mute',
	'communication_disabled_until',
	'role_ids',
	'is_premium_sanitized',
	'temporary',
	'profile_flags',
	'mention_flags',
	'version',
] as const satisfies ReadonlyArray<keyof GuildMemberRow>;

export interface GuildAuditLogRow {
	guild_id: GuildID;
	log_id: bigint;
	user_id: UserID;
	target_id: string | null;
	action_type: AuditLogActionType;
	reason: string | null;
	options: Map<string, string> | null;
	changes: string | null;
}

export const GUILD_AUDIT_LOG_COLUMNS = [
	'guild_id',
	'log_id',
	'user_id',
	'target_id',
	'action_type',
	'reason',
	'options',
	'changes',
] as const satisfies ReadonlyArray<keyof GuildAuditLogRow>;

export interface GuildRoleRow {
	guild_id: GuildID;
	role_id: RoleID;
	name: string;
	permissions: bigint;
	position: number;
	hoist_position: number | null;
	color: number;
	icon_hash: string | null;
	unicode_emoji: string | null;
	hoist: boolean;
	mentionable: boolean;
	version: number;
}

export const GUILD_ROLE_COLUMNS = [
	'guild_id',
	'role_id',
	'name',
	'permissions',
	'position',
	'hoist_position',
	'color',
	'icon_hash',
	'unicode_emoji',
	'hoist',
	'mentionable',
	'version',
] as const satisfies ReadonlyArray<keyof GuildRoleRow>;

export interface GuildBanRow {
	guild_id: GuildID;
	user_id: UserID;
	moderator_id: UserID;
	banned_at: Date;
	expires_at: Date | null;
	reason: string | null;
	ip: string | null;
	email: string | null;
}

export const GUILD_BAN_COLUMNS = [
	'guild_id',
	'user_id',
	'moderator_id',
	'banned_at',
	'expires_at',
	'reason',
	'ip',
	'email',
] as const satisfies ReadonlyArray<keyof GuildBanRow>;

export interface GuildBanByEmailRow {
	guild_id: GuildID;
	email: string;
	user_id: UserID;
}

export const GUILD_BAN_BY_EMAIL_COLUMNS = ['guild_id', 'email', 'user_id'] as const satisfies ReadonlyArray<
	keyof GuildBanByEmailRow
>;

export interface GuildBanByUserIdRow {
	user_id: UserID;
	guild_id: GuildID;
	email: string | null;
}

export const GUILD_BAN_BY_USER_ID_COLUMNS = ['user_id', 'guild_id', 'email'] as const satisfies ReadonlyArray<
	keyof GuildBanByUserIdRow
>;

export interface GuildEmojiRow {
	guild_id: GuildID;
	emoji_id: EmojiID;
	name: string;
	creator_id: UserID;
	animated: boolean;
	version: number;
}

export const GUILD_EMOJI_COLUMNS = [
	'guild_id',
	'emoji_id',
	'name',
	'creator_id',
	'animated',
	'version',
] as const satisfies ReadonlyArray<keyof GuildEmojiRow>;
export const GUILD_EMOJI_BY_EMOJI_ID_COLUMNS = [
	'guild_id',
	'emoji_id',
	'name',
	'creator_id',
	'animated',
] as const satisfies ReadonlyArray<keyof GuildEmojiRow>;

export interface GuildStickerRow {
	guild_id: GuildID;
	sticker_id: StickerID;
	name: string;
	description: string | null;
	animated: boolean;
	tags: Array<string> | null;
	creator_id: UserID;
	version: number;
}

export const GUILD_STICKER_COLUMNS = [
	'guild_id',
	'sticker_id',
	'name',
	'description',
	'animated',
	'tags',
	'creator_id',
	'version',
] as const satisfies ReadonlyArray<keyof GuildStickerRow>;
export const GUILD_STICKER_BY_STICKER_ID_COLUMNS = [
	'guild_id',
	'sticker_id',
	'name',
	'description',
	'animated',
	'tags',
	'creator_id',
] as const satisfies ReadonlyArray<keyof GuildStickerRow>;

export interface GuildMembershipMetadataRow {
	guild_id: GuildID;
	user_id: UserID;
	communication_disabled_until: Date | null;
	first_joined_at: Date | null;
	last_left_at: Date | null;
}

export const GUILD_MEMBERSHIP_METADATA_COLUMNS = [
	'guild_id',
	'user_id',
	'communication_disabled_until',
	'first_joined_at',
	'last_left_at',
] as const satisfies ReadonlyArray<keyof GuildMembershipMetadataRow>;

export interface GuildMemberByUserIdRow {
	user_id: UserID;
	guild_id: GuildID;
}

export const GUILD_MEMBER_BY_USER_ID_COLUMNS = ['user_id', 'guild_id'] as const satisfies ReadonlyArray<
	keyof GuildMemberByUserIdRow
>;
