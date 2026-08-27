// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {describe, expect, it} from 'vitest';
import type {ChannelUnreadStateInput} from './ChannelUnreadState';
import {
	type ChannelUnreadStateValue,
	createChannelUnreadSnapshot,
	selectChannelUnreadState,
	transitionChannelUnreadSnapshot,
} from './ChannelUnreadStateMachine';

function input(overrides: Partial<ChannelUnreadStateInput> = {}): ChannelUnreadStateInput {
	return {
		hasUnread: false,
		unreadCount: 0,
		mentionCount: 0,
		isMuted: false,
		showFadedUnreadOnMutedChannels: false,
		unreadBadgesLevel: null,
		...overrides,
	};
}

function expectState(overrides: Partial<ChannelUnreadStateInput>, expected: ChannelUnreadStateValue): void {
	const snapshot = createChannelUnreadSnapshot(input(overrides));
	expect(snapshot.value).toBe(expected);
}

describe('channelUnreadStateMachine', () => {
	it('routes each unread badge policy to an explicit state', () => {
		expectState({unreadBadgesLevel: MessageNotifications.NO_MESSAGES}, 'disabled');
		expectState({unreadBadgesLevel: MessageNotifications.ONLY_MENTIONS}, 'onlyMentions');
		expectState({unreadBadgesLevel: MessageNotifications.ALL_MESSAGES}, 'allMessages');
		expectState({unreadBadgesLevel: null}, 'legacy');
	});

	it('suppresses all visible unread surfaces when badges are disabled', () => {
		const snapshot = createChannelUnreadSnapshot(
			input({
				unreadBadgesLevel: MessageNotifications.NO_MESSAGES,
				hasUnread: true,
				unreadCount: 1,
				mentionCount: 1,
			}),
		);

		expect(selectChannelUnreadState(snapshot)).toMatchObject({
			hasUnreadMessages: true,
			hasMentions: false,
			isHighlight: false,
			shouldShowUnreadIndicator: false,
			hasVisibleUnread: false,
		});
	});

	it('transitions without preserving stale policy output', () => {
		const legacySnapshot = createChannelUnreadSnapshot(
			input({
				hasUnread: true,
				unreadCount: 2,
				isMuted: true,
				showFadedUnreadOnMutedChannels: false,
			}),
		);
		expect(selectChannelUnreadState(legacySnapshot).shouldShowUnreadIndicator).toBe(false);

		const allMessagesSnapshot = transitionChannelUnreadSnapshot(legacySnapshot, {
			type: 'channelUnread.updated',
			input: input({
				unreadBadgesLevel: MessageNotifications.ALL_MESSAGES,
				hasUnread: true,
				unreadCount: 2,
				isMuted: true,
			}),
		});

		expect(allMessagesSnapshot.value).toBe('allMessages');
		expect(selectChannelUnreadState(allMessagesSnapshot)).toMatchObject({
			isHighlight: true,
			shouldShowUnreadIndicator: true,
			isUnreadIndicatorMuted: false,
		});
	});
});
