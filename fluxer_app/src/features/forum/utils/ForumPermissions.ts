// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {
	canCreateThreadIn,
	canPatchThread,
	getThreadActorContext,
	isModeratorOfParent,
} from '@app/features/threads/utils/ThreadActionRules';
import {canSetTags} from '@fluxer/constants/src/ThreadPermissionUtils';

export function canCreatePost(forum: Channel): boolean {
	return ThreadGuilds.isActive(forum.guildId) && forum.isThreadOnly() && canCreateThreadIn(forum, 'forum_post');
}

export function canUseTag(forum: Channel, tag: {moderated: boolean}): boolean {
	return !tag.moderated || isModeratorOfParent(forum);
}

export function canEditPostTags(post: Channel, touchesModeratedTag = false): boolean {
	const ctx = getThreadActorContext(post);
	if (ctx == null || !canPatchThread(post, {applied_tags: post.appliedTags})) return false;
	return canSetTags(ctx, {touchesModeratedTag}) === null;
}
