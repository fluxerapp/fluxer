// SPDX-License-Identifier: AGPL-3.0-or-later

export const UNREAD_INDICATOR = -1;

interface AppBadgeInput {
	readonly foregroundMentionCount: number;
	readonly otherAccountsMentionCount: number;
	readonly pendingFriendRequestCount: number;
	readonly foregroundHasUnread: boolean;
	readonly otherAccountsHaveUnread: boolean;
	readonly unreadMessageBadgeEnabled: boolean;
}

export interface AppBadgeState {
	readonly badge: number;
	readonly totalCount: number;
	readonly showUnread: boolean;
}

export function computeAppBadge(input: AppBadgeInput): AppBadgeState {
	const totalCount = input.foregroundMentionCount + input.otherAccountsMentionCount + input.pendingFriendRequestCount;
	const showUnread = (input.foregroundHasUnread || input.otherAccountsHaveUnread) && input.unreadMessageBadgeEnabled;
	if (totalCount > 0) return {badge: totalCount, totalCount, showUnread};
	return {badge: showUnread ? UNREAD_INDICATOR : 0, totalCount, showUnread};
}
