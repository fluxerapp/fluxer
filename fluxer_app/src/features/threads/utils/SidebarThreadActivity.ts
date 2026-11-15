// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {isThreadMutedInSidebar} from '@app/features/threads/utils/ThreadNotificationUtils';

export interface SidebarThreadActivity {
	readonly hasActivity: boolean;
	readonly hasMentions: boolean;
}

export function isSidebarThreadUnread(thread: Channel): boolean {
	return ThreadMemberships.isMember(thread.id) && !isThreadMutedInSidebar(thread) && ReadStates.hasUnread(thread.id);
}

export function hasSidebarThreadActivity(thread: Channel): boolean {
	return ReadStates.getMentionCount(thread.id) > 0 || isSidebarThreadUnread(thread);
}

export function getSidebarThreadActivity(parentId: string): SidebarThreadActivity {
	let hasActivity = false;
	for (const thread of ChannelThreads.getSidebarThreads(parentId, null)) {
		if (ReadStates.getMentionCount(thread.id) > 0) return {hasActivity: true, hasMentions: true};
		if (isSidebarThreadUnread(thread)) hasActivity = true;
	}
	return {hasActivity, hasMentions: false};
}
