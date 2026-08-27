// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveChannelUnreadState} from './ChannelUnreadStateMachine';

export interface ChannelUnreadStateInput {
	hasUnread: boolean;
	unreadCount: number;
	mentionCount: number;
	isMuted: boolean;
	showFadedUnreadOnMutedChannels: boolean;
	unreadBadgesLevel?: number | null;
}

export interface ChannelUnreadState {
	hasUnreadMessages: boolean;
	hasMentions: boolean;
	isHighlight: boolean;
	shouldShowUnreadIndicator: boolean;
	isUnreadIndicatorMuted: boolean;
	hasVisibleUnread: boolean;
}

export function getChannelUnreadState({
	hasUnread,
	unreadCount,
	mentionCount,
	isMuted,
	showFadedUnreadOnMutedChannels,
	unreadBadgesLevel,
}: ChannelUnreadStateInput): ChannelUnreadState {
	return resolveChannelUnreadState({
		hasUnread,
		unreadCount,
		mentionCount,
		isMuted,
		showFadedUnreadOnMutedChannels,
		unreadBadgesLevel,
	});
}
