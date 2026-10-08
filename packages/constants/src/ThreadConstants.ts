// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const THREAD_CHANNEL_TYPES = new Set<number>([
	ChannelTypes.ANNOUNCEMENT_THREAD,
	ChannelTypes.PUBLIC_THREAD,
	ChannelTypes.PRIVATE_THREAD,
]);
export const PUBLIC_THREAD_CHANNEL_TYPES = new Set<number>([
	ChannelTypes.ANNOUNCEMENT_THREAD,
	ChannelTypes.PUBLIC_THREAD,
]);
export const THREAD_ONLY_CHANNEL_TYPES = new Set<number>([ChannelTypes.GUILD_FORUM, ChannelTypes.GUILD_MEDIA]);
export const TEXT_THREAD_PARENT_CHANNEL_TYPES = new Set<number>([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.GUILD_ANNOUNCEMENT,
]);
export const THREAD_PARENT_CHANNEL_TYPES = new Set<number>([
	...TEXT_THREAD_PARENT_CHANNEL_TYPES,
	...THREAD_ONLY_CHANNEL_TYPES,
]);
export const THREAD_FEATURE_CHANNEL_TYPES = new Set<number>([...THREAD_CHANNEL_TYPES, ...THREAD_ONLY_CHANNEL_TYPES]);

export function publicThreadTypeFor(parentType: number): number {
	return parentType === ChannelTypes.GUILD_ANNOUNCEMENT ? ChannelTypes.ANNOUNCEMENT_THREAD : ChannelTypes.PUBLIC_THREAD;
}

export function resolveTextThreadType(parentType: number, requestedType: number): number | null {
	if (parentType === ChannelTypes.GUILD_ANNOUNCEMENT) {
		return PUBLIC_THREAD_CHANNEL_TYPES.has(requestedType) ? ChannelTypes.ANNOUNCEMENT_THREAD : null;
	}
	if (parentType !== ChannelTypes.GUILD_TEXT || requestedType === ChannelTypes.ANNOUNCEMENT_THREAD) return null;
	return THREAD_CHANNEL_TYPES.has(requestedType) ? requestedType : null;
}

export const ServerMessageFlags = {
	HAS_THREAD: 1 << 5,
	FAILED_TO_MENTION_SOME_ROLES_IN_THREAD: 1 << 8,
} as const;
export const THREAD_MESSAGE_FLAG_MASK =
	ServerMessageFlags.HAS_THREAD | ServerMessageFlags.FAILED_TO_MENTION_SOME_ROLES_IN_THREAD;

export const ChannelFlags = {
	PINNED: 1 << 1,
	REQUIRE_TAG: 1 << 4,
	HIDE_MEDIA_DOWNLOAD_OPTIONS: 1 << 15,
} as const;

export type ChannelFlag = ValueOf<typeof ChannelFlags>;

export function settableChannelFlags(type: number, parentType?: number | null): number {
	switch (type) {
		case ChannelTypes.PUBLIC_THREAD:
			return parentType != null && THREAD_ONLY_CHANNEL_TYPES.has(parentType) ? ChannelFlags.PINNED : 0;
		case ChannelTypes.GUILD_FORUM:
			return ChannelFlags.REQUIRE_TAG;
		case ChannelTypes.GUILD_MEDIA:
			return ChannelFlags.REQUIRE_TAG | ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS;
		default:
			return 0;
	}
}

export const ThreadMemberFlags = {
	HAS_INTERACTED: 1 << 0,
	ALL_MESSAGES: 1 << 1,
	ONLY_MENTIONS: 1 << 2,
	NO_MESSAGES: 1 << 3,
} as const;

export const THREAD_MEMBER_NOTIFICATION_FLAG_MASK =
	ThreadMemberFlags.ALL_MESSAGES | ThreadMemberFlags.ONLY_MENTIONS | ThreadMemberFlags.NO_MESSAGES;

export function isValidThreadMemberSettingsFlags(flags: number): boolean {
	if (!Number.isInteger(flags) || flags < 0 || flags > THREAD_MEMBER_NOTIFICATION_FLAG_MASK) return false;
	if ((flags & ~THREAD_MEMBER_NOTIFICATION_FLAG_MASK) !== 0) return false;
	const notify = flags & THREAD_MEMBER_NOTIFICATION_FLAG_MASK;
	return (notify & (notify - 1)) === 0;
}

export const ReadStateFlags = {
	IS_GUILD_CHANNEL: 1 << 0,
	IS_THREAD: 1 << 1,
} as const;

export const ChannelOverrideFlags = {
	NEW_FORUM_THREADS_OFF: 1 << 13,
	NEW_FORUM_THREADS_ON: 1 << 14,
} as const;

export const CHANNEL_OVERRIDE_FLAG_MASK =
	ChannelOverrideFlags.NEW_FORUM_THREADS_OFF | ChannelOverrideFlags.NEW_FORUM_THREADS_ON;

export const ForumLayoutTypes = {
	DEFAULT: 0,
	LIST: 1,
	GRID: 2,
} as const;

export type ForumLayoutType = ValueOf<typeof ForumLayoutTypes>;

export const ForumSortOrderTypes = {
	LATEST_ACTIVITY: 0,
	CREATION_TIME: 1,
} as const;

export type ForumSortOrderType = ValueOf<typeof ForumSortOrderTypes>;

export const ForumTagSettings = {
	MATCH_SOME: 'match_some',
	MATCH_ALL: 'match_all',
} as const;

export type ForumTagSetting = ValueOf<typeof ForumTagSettings>;

export const THREAD_AUTO_ARCHIVE_DURATIONS = [60, 1440, 4320, 10080] as const;

export type ThreadAutoArchiveDuration = (typeof THREAD_AUTO_ARCHIVE_DURATIONS)[number];

export const DEFAULT_THREAD_AUTO_ARCHIVE_DURATION: ThreadAutoArchiveDuration = 4320;

export function isThreadAutoArchiveDuration(value: number): value is ThreadAutoArchiveDuration {
	return (THREAD_AUTO_ARCHIVE_DURATIONS as ReadonlyArray<number>).includes(value);
}

export const THREAD_NAME_MIN_LENGTH = 1;
export const THREAD_NAME_MAX_LENGTH = 100;
export const FORUM_TOPIC_MAX_LENGTH = 4096;
export const MAX_FORUM_TAGS_PER_CHANNEL = 20;
export const FORUM_TAG_NAME_MAX_LENGTH = 50;
export const MAX_APPLIED_TAGS_PER_THREAD = 5;
export const MAX_THREAD_MEMBERS = 1000;
export const THREAD_MEMBER_COUNT_DISPLAY_CAP = 50;
export const THREAD_MEMBER_IDS_PREVIEW_SIZE = 8;
export const MAX_ACTIVE_THREADS_PER_GUILD = 1000;
export const MAX_PINNED_THREADS_PER_FORUM = 1;
export const MAX_ROLE_MENTION_THREAD_ADDS = 250;
export const THREAD_ARCHIVED_LIST_MIN_LIMIT = 2;
export const THREAD_ARCHIVED_LIST_MAX_LIMIT = 100;
export const THREAD_ARCHIVED_LIST_DEFAULT_LIMIT = 50;
export const THREAD_SEARCH_MIN_LIMIT = 1;
export const THREAD_SEARCH_MAX_LIMIT = 25;
export const THREAD_SEARCH_MAX_OFFSET = 9975;
export const THREAD_SEARCH_NAME_MAX_LENGTH = 100;
export const THREAD_SEARCH_MAX_TAGS = 20;
export const POST_DATA_MAX_IDS = 100;
export const FORUM_UNREADS_MAX_THREADS = 40;
export const FORUM_UNREAD_COUNT_CAP = 25;
export const MAX_THREAD_ARCHIVES_PER_SWEEP_TICK = 200;
export const THREAD_SCOPE_MAX = 5000;
export const SEARCH_INDEX_ENQUEUE_MAX = 50;
export const THREAD_RATE_LIMIT_PER_USER_MAX = 21600;
