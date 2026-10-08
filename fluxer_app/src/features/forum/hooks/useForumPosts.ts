// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import ForumPosts, {canRequestForumPosts, type ForumListState} from '@app/features/forum/state/ForumPosts';
import ForumReadState from '@app/features/forum/state/ForumReadState';
import {getDefaultTagSetting} from '@app/features/forum/utils/ForumChannelUtils';
import {buildForumPostList} from '@app/features/forum/utils/ForumPostList';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import Window from '@app/features/window/state/Window';
import {useEffect} from 'react';

export interface ForumPostListView {
	searching: boolean;
	pinned: Channel | null;
	posts: ReadonlyArray<Channel>;
	archived: ReadonlyArray<Channel>;
	list: ForumListState;
}

function resolvePosts(forumId: string, ids: ReadonlyArray<string>): Array<Channel> {
	const posts: Array<Channel> = [];
	for (const id of ids) {
		const post = ChannelThreads.getThread(id);
		if (post?.parentId === forumId) posts.push(post);
	}
	return posts;
}

export function getForumPostListView(forum: Channel): ForumPostListView {
	const searching = ForumPosts.isSearching(forum.id);
	if (searching) {
		const list = ForumPosts.getList(forum.id, 'search');
		return {searching, pinned: null, posts: resolvePosts(forum.id, list.ids), archived: [], list};
	}
	const list = ForumPosts.getList(forum.id, 'archived');
	const {pinned, active, archived} = buildForumPostList({
		known: ChannelThreads.getThreadsForParent(forum.id),
		extra: resolvePosts(forum.id, list.ids),
		sortOrder: ForumPosts.getSortOrder(forum),
		tagIds: ForumPosts.getTagFilter(forum.id),
		tagSetting: getDefaultTagSetting(forum),
	});
	return {searching, pinned, posts: active, archived, list};
}

function requestPostUnreads(forum: Channel, postIds: ReadonlyArray<string>): void {
	if (!forum.guildId || !canRequestForumPosts(forum)) return;
	if (!GatewayConnection.socket?.isConnected()) return;
	ForumReadState.requestPostUnreads(forum.guildId, forum.id, postIds);
}

export function usePostDataPrefetch(forum: Channel, posts: ReadonlyArray<Channel>): void {
	const key = posts.map((post) => post.id).join(',');
	useEffect(() => {
		if (!key) return;
		const postIds = key.split(',');
		ForumPosts.requestPostData(forum, postIds);
		requestPostUnreads(forum, postIds);
	}, [forum, key]);
}

export function useForumViewing(forum: Channel): void {
	const forumId = forum.id;
	useEffect(() => {
		ForumReadState.beginViewing(forumId);
		return () => ForumReadState.endViewing(forumId);
	}, [forumId]);
	const lastMessageId = ReadStates.lastMessageId(forumId);
	const unread = ReadStates.hasUnread(forumId);
	const focused = Window.focused;
	useEffect(() => {
		if (focused && unread && Channels.getChannel(forumId)) ReadStateCommands.ack(forumId);
	}, [forumId, lastMessageId, unread, focused]);
}
