// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import type {ThreadParentConfigRow} from '@app/api/database/types/ThreadTypes';
import {ForumTag} from '@app/api/models/ForumTag';

export class ThreadParentConfig {
	readonly guildId: GuildID;
	readonly channelId: ChannelID;
	readonly flags: number;
	readonly defaultAutoArchiveDuration: number | null;
	readonly defaultThreadRateLimitPerUser: number | null;
	readonly availableTags: Array<ForumTag>;
	readonly defaultReactionEmojiId: bigint | null;
	readonly defaultReactionEmojiName: string | null;
	readonly defaultSortOrder: number | null;
	readonly defaultForumLayout: number | null;
	readonly defaultTagSetting: string | null;
	readonly hasThreads: boolean;

	constructor(row: ThreadParentConfigRow) {
		this.guildId = row.guild_id;
		this.channelId = row.channel_id;
		this.flags = row.flags ?? 0;
		this.defaultAutoArchiveDuration = row.default_auto_archive_duration ?? null;
		this.defaultThreadRateLimitPerUser = row.default_thread_rate_limit_per_user ?? null;
		this.availableTags = (row.available_tags ?? []).map((tag) => new ForumTag(tag));
		this.defaultReactionEmojiId = row.default_reaction_emoji_id ?? null;
		this.defaultReactionEmojiName = row.default_reaction_emoji_name ?? null;
		this.defaultSortOrder = row.default_sort_order ?? null;
		this.defaultForumLayout = row.default_forum_layout ?? null;
		this.defaultTagSetting = row.default_tag_setting ?? null;
		this.hasThreads = row.has_threads ?? false;
	}

	toRow(): ThreadParentConfigRow {
		return {
			guild_id: this.guildId,
			channel_id: this.channelId,
			flags: this.flags,
			default_auto_archive_duration: this.defaultAutoArchiveDuration,
			default_thread_rate_limit_per_user: this.defaultThreadRateLimitPerUser,
			available_tags: this.availableTags.length > 0 ? this.availableTags.map((tag) => tag.toUdt()) : null,
			default_reaction_emoji_id: this.defaultReactionEmojiId,
			default_reaction_emoji_name: this.defaultReactionEmojiName,
			default_sort_order: this.defaultSortOrder,
			default_forum_layout: this.defaultForumLayout,
			default_tag_setting: this.defaultTagSetting,
			has_threads: this.hasThreads,
		};
	}
}
