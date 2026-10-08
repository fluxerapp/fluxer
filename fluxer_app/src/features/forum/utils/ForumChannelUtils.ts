// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelFlags,
	type ForumLayoutType,
	ForumLayoutTypes,
	type ForumSortOrderType,
	ForumSortOrderTypes,
	type ForumTagSetting,
	ForumTagSettings,
} from '@fluxer/constants/src/ThreadConstants';
import type {DefaultReactionEmojiResponse, ForumTagResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';

export function isMediaChannel(forum: Channel): boolean {
	return forum.type === ChannelTypes.GUILD_MEDIA;
}

export function getDefaultSortOrder(forum: Channel): ForumSortOrderType {
	return forum.threadFields?.default_sort_order === ForumSortOrderTypes.CREATION_TIME
		? ForumSortOrderTypes.CREATION_TIME
		: ForumSortOrderTypes.LATEST_ACTIVITY;
}

export function getDefaultLayout(forum: Channel): ForumLayoutType {
	if (isMediaChannel(forum)) return ForumLayoutTypes.GRID;
	return forum.threadFields?.default_forum_layout === ForumLayoutTypes.GRID
		? ForumLayoutTypes.GRID
		: ForumLayoutTypes.LIST;
}

export function getDefaultTagSetting(forum: Channel): ForumTagSetting {
	return forum.threadFields?.default_tag_setting === ForumTagSettings.MATCH_ALL
		? ForumTagSettings.MATCH_ALL
		: ForumTagSettings.MATCH_SOME;
}

export function getDefaultReaction(forum: Channel): DefaultReactionEmojiResponse | null {
	const reaction = forum.threadFields?.default_reaction_emoji;
	if (reaction == null || (reaction.emoji_id == null && reaction.emoji_name == null)) return null;
	return reaction;
}

export function isTagRequired(forum: Channel): boolean {
	return (forum.flags & ChannelFlags.REQUIRE_TAG) !== 0;
}

export function hidesMediaDownloads(forum: Channel): boolean {
	return isMediaChannel(forum) && (forum.flags & ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS) !== 0;
}

export function getForumTags(forum: Channel, tagIds: ReadonlyArray<string>): Array<ForumTagResponse> {
	if (tagIds.length === 0) return [];
	const byId = new Map(forum.availableTags.map((tag) => [tag.id, tag]));
	const tags: Array<ForumTagResponse> = [];
	for (const tagId of tagIds) {
		const tag = byId.get(tagId);
		if (tag) tags.push(tag);
	}
	return tags;
}

export function getPostForum(post: Channel): Channel | undefined {
	if (!post.isThread() || !post.parentId) return undefined;
	const parent = Channels.getChannel(post.parentId);
	return parent?.isThreadOnly() ? parent : undefined;
}

export interface ForumEmojiRef {
	emoji_id: string | null;
	emoji_name: string | null;
}

export function isOriginalPoster(channelId: string, authorId: string): boolean {
	const post = ChannelThreads.getThread(channelId);
	if (!post || !getPostForum(post)) return false;
	return post.ownerId === authorId;
}
