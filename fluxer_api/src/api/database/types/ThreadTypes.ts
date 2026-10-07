// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {MuteConfig} from '@app/api/database/types/UserTypes';

type Nullish<T> = T | null;

export interface ForumTagUdt {
	id: bigint;
	name: string;
	moderated: boolean;
	emoji_id: Nullish<bigint>;
	emoji_name: Nullish<string>;
}

export interface ThreadStateRow {
	thread_id: ChannelID;
	guild_id: GuildID;
	parent_id: ChannelID;
	type: number;
	archived: boolean;
	locked: boolean;
	invitable: Nullish<boolean>;
	auto_archive_duration: number;
	archive_timestamp: Nullish<Date>;
	created_at: Date;
	flags: number;
	applied_tags: Nullish<Array<bigint>>;
	member_count: number;
	member_ids_preview: Nullish<Array<UserID>>;
	has_starter: boolean;
	state_version: number;
}

export const THREAD_STATE_COLUMNS = [
	'thread_id',
	'guild_id',
	'parent_id',
	'type',
	'archived',
	'locked',
	'invitable',
	'auto_archive_duration',
	'archive_timestamp',
	'created_at',
	'flags',
	'applied_tags',
	'member_count',
	'member_ids_preview',
	'has_starter',
	'state_version',
] as const satisfies ReadonlyArray<keyof ThreadStateRow>;

export interface ThreadStatsRow {
	thread_id: ChannelID;
	message_count: Nullish<number>;
	total_message_sent: Nullish<number>;
}

export const THREAD_STATS_COLUMNS = [
	'thread_id',
	'message_count',
	'total_message_sent',
] as const satisfies ReadonlyArray<keyof ThreadStatsRow>;

export interface ThreadsByParentRow {
	parent_id: ChannelID;
	thread_id: ChannelID;
	guild_id: GuildID;
	type: number;
}

export const THREADS_BY_PARENT_COLUMNS = [
	'parent_id',
	'thread_id',
	'guild_id',
	'type',
] as const satisfies ReadonlyArray<keyof ThreadsByParentRow>;

export interface ActiveThreadsByGuildRow {
	guild_id: GuildID;
	thread_id: ChannelID;
	parent_id: ChannelID;
	type: number;
}

export const ACTIVE_THREADS_BY_GUILD_COLUMNS = [
	'guild_id',
	'thread_id',
	'parent_id',
	'type',
] as const satisfies ReadonlyArray<keyof ActiveThreadsByGuildRow>;

export interface ArchivedThreadsByParentRow {
	parent_id: ChannelID;
	is_private: boolean;
	archive_timestamp: Date;
	thread_id: ChannelID;
	guild_id: GuildID;
}

export const ARCHIVED_THREADS_BY_PARENT_COLUMNS = [
	'parent_id',
	'is_private',
	'archive_timestamp',
	'thread_id',
	'guild_id',
] as const satisfies ReadonlyArray<keyof ArchivedThreadsByParentRow>;

export interface ThreadMemberRow {
	thread_id: ChannelID;
	user_id: UserID;
	guild_id: GuildID;
	parent_id: ChannelID;
	join_timestamp: Date;
	flags: number;
	muted: boolean;
	mute_config: Nullish<MuteConfig>;
}

export const THREAD_MEMBER_COLUMNS = [
	'thread_id',
	'user_id',
	'guild_id',
	'parent_id',
	'join_timestamp',
	'flags',
	'muted',
	'mute_config',
] as const satisfies ReadonlyArray<keyof ThreadMemberRow>;

export interface ThreadMembersByUserRow {
	user_id: UserID;
	guild_id: GuildID;
	parent_id: ChannelID;
	is_private: boolean;
	thread_id: ChannelID;
}

export const THREAD_MEMBERS_BY_USER_COLUMNS = [
	'user_id',
	'guild_id',
	'parent_id',
	'is_private',
	'thread_id',
] as const satisfies ReadonlyArray<keyof ThreadMembersByUserRow>;

export interface ThreadParentConfigRow {
	guild_id: GuildID;
	channel_id: ChannelID;
	flags: Nullish<number>;
	default_auto_archive_duration: Nullish<number>;
	default_thread_rate_limit_per_user: Nullish<number>;
	available_tags: Nullish<Array<ForumTagUdt>>;
	default_reaction_emoji_id: Nullish<bigint>;
	default_reaction_emoji_name: Nullish<string>;
	default_sort_order: Nullish<number>;
	default_forum_layout: Nullish<number>;
	default_tag_setting: Nullish<string>;
	has_threads: Nullish<boolean>;
}

export const THREAD_PARENT_CONFIG_COLUMNS = [
	'guild_id',
	'channel_id',
	'flags',
	'default_auto_archive_duration',
	'default_thread_rate_limit_per_user',
	'available_tags',
	'default_reaction_emoji_id',
	'default_reaction_emoji_name',
	'default_sort_order',
	'default_forum_layout',
	'default_tag_setting',
	'has_threads',
] as const satisfies ReadonlyArray<keyof ThreadParentConfigRow>;

export interface ForumPinnedThreadRow {
	parent_id: ChannelID;
	thread_id: ChannelID;
}

export const FORUM_PINNED_THREAD_COLUMNS = ['parent_id', 'thread_id'] as const satisfies ReadonlyArray<
	keyof ForumPinnedThreadRow
>;

export interface ThreadOnlyChannelsByGuildRow {
	guild_id: GuildID;
	channel_id: ChannelID;
}

export const THREAD_ONLY_CHANNELS_BY_GUILD_COLUMNS = ['guild_id', 'channel_id'] as const satisfies ReadonlyArray<
	keyof ThreadOnlyChannelsByGuildRow
>;

export interface GuildThreadStateRow {
	guild_id: GuildID;
	first_active_at: Nullish<Date>;
	perms_seeded_at: Nullish<Date>;
	search_backfilled_at: Nullish<Date>;
}

export const GUILD_THREAD_STATE_COLUMNS = [
	'guild_id',
	'first_active_at',
	'perms_seeded_at',
	'search_backfilled_at',
] as const satisfies ReadonlyArray<keyof GuildThreadStateRow>;
