// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';

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

export function getChannelUnreadState(context: ChannelUnreadStateInput): ChannelUnreadState {
	const hasUnreadMessages = context.hasUnread;
	const rawHasMentions = context.mentionCount > 0;
	switch (context.unreadBadgesLevel) {
		case MessageNotifications.NO_MESSAGES:
			return {
				hasUnreadMessages,
				hasMentions: false,
				isHighlight: false,
				shouldShowUnreadIndicator: false,
				isUnreadIndicatorMuted: false,
				hasVisibleUnread: false,
			};
		case MessageNotifications.ONLY_MENTIONS:
			return {
				hasUnreadMessages,
				hasMentions: rawHasMentions,
				isHighlight: rawHasMentions,
				shouldShowUnreadIndicator: hasUnreadMessages,
				isUnreadIndicatorMuted: hasUnreadMessages,
				hasVisibleUnread: rawHasMentions || hasUnreadMessages,
			};
		case MessageNotifications.ALL_MESSAGES:
			return {
				hasUnreadMessages,
				hasMentions: rawHasMentions,
				isHighlight: rawHasMentions || hasUnreadMessages,
				shouldShowUnreadIndicator: hasUnreadMessages,
				isUnreadIndicatorMuted: false,
				hasVisibleUnread: rawHasMentions || hasUnreadMessages,
			};
		default: {
			const shouldShowUnreadIndicator =
				hasUnreadMessages && (!context.isMuted || context.showFadedUnreadOnMutedChannels);
			const hasMentions = rawHasMentions;
			return {
				hasUnreadMessages,
				hasMentions,
				isHighlight: rawHasMentions || (hasUnreadMessages && !context.isMuted),
				shouldShowUnreadIndicator,
				isUnreadIndicatorMuted: shouldShowUnreadIndicator && context.isMuted,
				hasVisibleUnread: hasMentions || shouldShowUnreadIndicator,
			};
		}
	}
}
