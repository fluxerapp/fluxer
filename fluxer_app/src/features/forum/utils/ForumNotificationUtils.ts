// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import type {I18n} from '@lingui/core';

export function resolveNewPostNotificationLevel(post: Channel, messageId: string): number | null {
	if (messageId !== post.id) return null;
	const forum = getPostForum(post);
	if (!forum) return null;
	return UserGuildSettings.resolveEffectiveMessageNotifications({
		id: forum.id,
		guildId: forum.guildId,
		parentId: forum.parentId ?? undefined,
		type: forum.type,
	});
}

export function formatNewPostNotificationSuffix(i18n: I18n, post: Channel, messageId: string): string | null {
	if (messageId !== post.id) return null;
	const forum = getPostForum(post);
	return forum ? i18n._(D.NEW_POST_IN_FORUM_DESCRIPTOR, {forumName: forum.name ?? ''}) : null;
}
