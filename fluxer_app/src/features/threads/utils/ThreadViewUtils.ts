// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Navigation from '@app/features/navigation/state/Navigation';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';

export function getOpenThreadIds(): ReadonlySet<string> {
	const ids = new Set<string>();
	if (Navigation.threadId) ids.add(Navigation.threadId);
	const channelId = Navigation.channelId;
	if (channelId && Channels.getChannel(channelId)?.isThread()) ids.add(channelId);
	return ids;
}

export function getUnreadThreadIds(guildId: string): Array<string> {
	if (!ThreadGuilds.isActive(guildId)) return [];
	const ids: Array<string> = [];
	for (const threadId of ChannelThreads.getGuildThreadIds(guildId)) {
		const joinedUnread = ThreadMemberships.isMember(threadId) && ReadStates.hasUnread(threadId);
		if (joinedUnread || ReadStates.getMentionCount(threadId) > 0) ids.push(threadId);
	}
	return ids;
}
