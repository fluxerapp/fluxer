// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {describe, expect, it} from 'vitest';
import {resolveUnreadInboxVisibility, type UnreadInboxVisibilityInput} from './UnreadInboxVisibility';

function input(overrides: Partial<UnreadInboxVisibilityInput> = {}): UnreadInboxVisibilityInput {
	return {
		unreadBadgesLevel: null,
		isMuted: false,
		hasUnread: true,
		hasMentions: false,
		...overrides,
	};
}

describe('resolveUnreadInboxVisibility', () => {
	it('shows an unmuted channel with plain unread messages', () => {
		expect(resolveUnreadInboxVisibility(input())).toBe(true);
	});

	it('shows a channel in a large community, where notification level is forced to only-mentions', () => {
		expect(resolveUnreadInboxVisibility(input({unreadBadgesLevel: null, hasMentions: false}))).toBe(true);
	});

	it('hides a channel with nothing unread and no mentions', () => {
		expect(resolveUnreadInboxVisibility(input({hasUnread: false, hasMentions: false}))).toBe(false);
	});

	it('hides plain unread in a muted channel', () => {
		expect(resolveUnreadInboxVisibility(input({isMuted: true}))).toBe(false);
	});

	it('shows a muted channel when it has a mention', () => {
		expect(resolveUnreadInboxVisibility(input({isMuted: true, hasMentions: true}))).toBe(true);
	});

	it('hides a channel whose explicit unread badges level is nothing, even with a mention', () => {
		expect(
			resolveUnreadInboxVisibility(input({unreadBadgesLevel: MessageNotifications.NO_MESSAGES, hasMentions: true})),
		).toBe(false);
	});

	it('hides plain unread when the explicit unread badges level is only-mentions', () => {
		expect(resolveUnreadInboxVisibility(input({unreadBadgesLevel: MessageNotifications.ONLY_MENTIONS}))).toBe(false);
	});

	it('shows a mention when the explicit unread badges level is only-mentions', () => {
		expect(
			resolveUnreadInboxVisibility(input({unreadBadgesLevel: MessageNotifications.ONLY_MENTIONS, hasMentions: true})),
		).toBe(true);
	});

	it('shows plain unread when the explicit unread badges level is all messages', () => {
		expect(resolveUnreadInboxVisibility(input({unreadBadgesLevel: MessageNotifications.ALL_MESSAGES}))).toBe(true);
	});

	it('takes no notification-level input at all, so a forced community level cannot gate it', () => {
		const keys = Object.keys(input()).sort();
		expect(keys).toEqual(['hasMentions', 'hasUnread', 'isMuted', 'unreadBadgesLevel']);
	});
});
