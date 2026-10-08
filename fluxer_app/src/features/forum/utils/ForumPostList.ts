// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {lastActivityId} from '@app/features/threads/state/ChannelThreads';
import {
	ChannelFlags,
	type ForumSortOrderType,
	ForumSortOrderTypes,
	type ForumTagSetting,
	ForumTagSettings,
} from '@fluxer/constants/src/ThreadConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

export function isPinnedPost(post: Channel): boolean {
	return (post.flags & ChannelFlags.PINNED) !== 0;
}

export function postSortKey(post: Channel, sortOrder: ForumSortOrderType): string {
	return sortOrder === ForumSortOrderTypes.CREATION_TIME ? post.id : lastActivityId(post);
}

export function matchesTagFilter(
	appliedTags: ReadonlyArray<string>,
	tagIds: ReadonlyArray<string>,
	tagSetting: ForumTagSetting,
): boolean {
	if (tagIds.length === 0) return true;
	if (tagSetting === ForumTagSettings.MATCH_ALL) return tagIds.every((tagId) => appliedTags.includes(tagId));
	return tagIds.some((tagId) => appliedTags.includes(tagId));
}

export interface ForumPostListInput {
	known: ReadonlyArray<Channel>;
	extra: ReadonlyArray<Channel>;
	sortOrder: ForumSortOrderType;
	tagIds: ReadonlyArray<string>;
	tagSetting: ForumTagSetting;
}

export interface ForumPostListResult {
	pinned: Channel | null;
	active: Array<Channel>;
	archived: Array<Channel>;
}

export function buildForumPostList({
	known,
	extra,
	sortOrder,
	tagIds,
	tagSetting,
}: ForumPostListInput): ForumPostListResult {
	const seen = new Set<string>();
	const activePosts: Array<Channel> = [];
	const archivedPosts: Array<Channel> = [];
	let pinned: Channel | null = null;
	for (const post of [...known, ...extra]) {
		if (seen.has(post.id)) continue;
		seen.add(post.id);
		if (!matchesTagFilter(post.appliedTags, tagIds, tagSetting)) continue;
		if (isPinnedPost(post) && !post.isArchived && pinned == null) {
			pinned = post;
			continue;
		}
		(post.isArchived ? archivedPosts : activePosts).push(post);
	}
	const byKey = (a: Channel, b: Channel) =>
		SnowflakeUtils.compare(postSortKey(b, sortOrder), postSortKey(a, sortOrder));
	activePosts.sort(byKey);
	archivedPosts.sort(byKey);
	return {pinned, active: activePosts, archived: archivedPosts};
}
