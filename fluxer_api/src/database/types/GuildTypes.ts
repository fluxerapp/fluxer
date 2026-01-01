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

import type {ChannelID, EmojiID, GuildID, InviteCode, RoleID, StickerID, UserID, VanityURLCode} from '~/BrandedTypes';
import type {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {GuildSplashCardAlignmentValue} from '~/constants/Guild';

type Nullish<T> = T | null;

export interface GuildRow {
	guild_id: GuildID;
	owner_id: UserID;
	name: string;
	vanity_url_code: Nullish<VanityURLCode>;
	icon_hash: Nullish<string>;
	banner_hash: Nullish<string>;
	banner_width: Nullish<number>;
	banner_height: Nullish<number>;
	splash_hash: Nullish<string>;
	splash_width: Nullish<number>;
	splash_height: Nullish<number>;
	splash_card_alignment: Nullish<GuildSplashCardAlignmentValue>;
	embed_splash_hash: Nullish<string>;
	embed_splash_width: Nullish<number>;
	embed_splash_height: Nullish<number>;
	features: Nullish<Set<string>>;
	verification_level: number;
	mfa_level: number;
	nsfw_level: number;
	explicit_content_filter: number;
	default_message_notifications: number;
	system_channel_id: Nullish<ChannelID>;
	system_channel_flags: number;
	rules_channel_id: Nullish<ChannelID>;
	afk_channel_id: Nullish<ChannelID>;
	afk_timeout: number;
	disabled_operations: number;
	member_count: number;
	audit_logs_indexed_at: Nullish<Date>;
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
	'version',
] as const satisfies ReadonlyArray<keyof GuildRow>;

export interface GuildMemberRow {
	guild_id: GuildID;
	user_id: UserID;
	joined_at: Date;
	nick: Nullish<string>;
	avatar_hash: Nullish<string>;
	banner_hash: Nullish<string>;
	bio: Nullish<string>;
	pronouns: Nullish<string>;
	accent_color: Nullish<number>;
	join_source_type: Nullish<number>;
	source_invite_code: Nullish<InviteCode>;
	inviter_id: Nullish<UserID>;
	deaf: boolean;
	mute: boolean;
	communication_disabled_until: Nullish<Date>;
	role_ids: Nullish<Set<RoleID>>;
	is_premium_sanitized: Nullish<boolean>;
	temporary: Nullish<boolean>;
	profile_flags: Nullish<number>;
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
	'version',
] as const satisfies ReadonlyArray<keyof GuildMemberRow>;

export interface GuildAuditLogRow {
	guild_id: GuildID;
	log_id: bigint;
	user_id: UserID;
	target_id: Nullish<string>;
	action_type: AuditLogActionType;
	reason: Nullish<string>;
	options: Nullish<Map<string, string>>;
	changes: Nullish<string>;
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
	hoist_position: Nullish<number>;
	color: number;
	icon_hash: Nullish<string>;
	unicode_emoji: Nullish<string>;
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
	expires_at: Nullish<Date>;
	reason: Nullish<string>;
	ip: Nullish<string>;
}

export const GUILD_BAN_COLUMNS = [
	'guild_id',
	'user_id',
	'moderator_id',
	'banned_at',
	'expires_at',
	'reason',
	'ip',
] as const satisfies ReadonlyArray<keyof GuildBanRow>;

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
	description: Nullish<string>;
	format_type: number;
	tags: Nullish<Array<string>>;
	creator_id: UserID;
	version: number;
}

export const GUILD_STICKER_COLUMNS = [
	'guild_id',
	'sticker_id',
	'name',
	'description',
	'format_type',
	'tags',
	'creator_id',
	'version',
] as const satisfies ReadonlyArray<keyof GuildStickerRow>;

export const GUILD_STICKER_BY_STICKER_ID_COLUMNS = [
	'guild_id',
	'sticker_id',
	'name',
	'description',
	'format_type',
	'tags',
	'creator_id',
] as const satisfies ReadonlyArray<keyof GuildStickerRow>;

export interface GuildMemberByUserIdRow {
	user_id: UserID;
	guild_id: GuildID;
}

export const GUILD_MEMBER_BY_USER_ID_COLUMNS = ['user_id', 'guild_id'] as const satisfies ReadonlyArray<
	keyof GuildMemberByUserIdRow
>;

export interface GuildByOwnerIdRow {
	owner_id: UserID;
	guild_id: GuildID;
}

export const GUILD_BY_OWNER_ID_COLUMNS = ['owner_id', 'guild_id'] as const satisfies ReadonlyArray<
	keyof GuildByOwnerIdRow
>;
