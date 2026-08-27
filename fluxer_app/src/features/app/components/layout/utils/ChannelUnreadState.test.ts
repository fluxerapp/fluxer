// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {describe, expect, it} from 'vitest';
import {getChannelUnreadState} from './ChannelUnreadState';

describe('getChannelUnreadState', () => {
	it('shows a normal unread indicator for all-messages unread badges', () => {
		const state = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 3,
			mentionCount: 0,
			isMuted: false,
			showFadedUnreadOnMutedChannels: false,
			unreadBadgesLevel: MessageNotifications.ALL_MESSAGES,
		});
		expect(state.shouldShowUnreadIndicator).toBe(true);
		expect(state.isUnreadIndicatorMuted).toBe(false);
		expect(state.isHighlight).toBe(true);
	});
	it('shows a muted unread indicator for only-mentions unread badges without highlighting the channel', () => {
		const state = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 3,
			mentionCount: 0,
			isMuted: false,
			showFadedUnreadOnMutedChannels: false,
			unreadBadgesLevel: MessageNotifications.ONLY_MENTIONS,
		});
		expect(state.shouldShowUnreadIndicator).toBe(true);
		expect(state.isUnreadIndicatorMuted).toBe(true);
		expect(state.isHighlight).toBe(false);
	});
	it('hides unread and mention surfaces when unread badges are disabled', () => {
		const state = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 3,
			mentionCount: 1,
			isMuted: false,
			showFadedUnreadOnMutedChannels: true,
			unreadBadgesLevel: MessageNotifications.NO_MESSAGES,
		});
		expect(state.shouldShowUnreadIndicator).toBe(false);
		expect(state.hasMentions).toBe(false);
		expect(state.hasVisibleUnread).toBe(false);
	});
	it('keeps legacy muted-channel fading for channels without an unread-badges level', () => {
		const hiddenState = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 3,
			mentionCount: 0,
			isMuted: true,
			showFadedUnreadOnMutedChannels: false,
			unreadBadgesLevel: null,
		});
		const fadedState = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 3,
			mentionCount: 0,
			isMuted: true,
			showFadedUnreadOnMutedChannels: true,
			unreadBadgesLevel: null,
		});
		expect(hiddenState.shouldShowUnreadIndicator).toBe(false);
		expect(fadedState.shouldShowUnreadIndicator).toBe(true);
		expect(fadedState.isUnreadIndicatorMuted).toBe(true);
	});
	it('shows the indicator from the unread flag rather than the message count', () => {
		const state = getChannelUnreadState({
			hasUnread: true,
			unreadCount: 0,
			mentionCount: 0,
			isMuted: false,
			showFadedUnreadOnMutedChannels: false,
			unreadBadgesLevel: null,
		});
		expect(state.shouldShowUnreadIndicator).toBe(true);
	});
	it('hides the indicator for a read channel even if a stale count survives', () => {
		const state = getChannelUnreadState({
			hasUnread: false,
			unreadCount: 7,
			mentionCount: 0,
			isMuted: false,
			showFadedUnreadOnMutedChannels: false,
			unreadBadgesLevel: null,
		});
		expect(state.shouldShowUnreadIndicator).toBe(false);
		expect(state.hasVisibleUnread).toBe(false);
	});
});
