// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {resolveUnreadJumpAnchor, type UnreadJumpAnchorInput} from './ReadStateUnreadAnchor';

function input(overrides: Partial<UnreadJumpAnchorInput> = {}): UnreadJumpAnchorInput {
	return {
		canBeUnread: true,
		canTrackUnreads: true,
		hasUnread: true,
		oldestUnreadMessageId: '900',
		ackMessageId: '100',
		...overrides,
	};
}

describe('resolveUnreadJumpAnchor', () => {
	it('anchors on the oldest unread message when it is known', () => {
		expect(resolveUnreadJumpAnchor(input())).toEqual({messageId: '900', offset: 0});
	});

	it('anchors just after the ack when the oldest unread message is unknown', () => {
		expect(resolveUnreadJumpAnchor(input({oldestUnreadMessageId: null}))).toEqual({messageId: '100', offset: 1});
	});

	it('returns no anchor when the channel has nothing unread', () => {
		expect(resolveUnreadJumpAnchor(input({hasUnread: false}))).toBeNull();
	});

	it('returns no anchor when the channel cannot be unread', () => {
		expect(resolveUnreadJumpAnchor(input({canBeUnread: false}))).toBeNull();
	});

	it('returns no anchor when unreads are not trackable for the channel', () => {
		expect(resolveUnreadJumpAnchor(input({canTrackUnreads: false}))).toBeNull();
	});

	it('returns no anchor when neither an unread boundary nor an ack exists', () => {
		expect(resolveUnreadJumpAnchor(input({oldestUnreadMessageId: null, ackMessageId: null}))).toBeNull();
	});

	it('anchors on the unread boundary regardless of how far behind the ack is', () => {
		const anchor = resolveUnreadJumpAnchor(input({ackMessageId: '1', oldestUnreadMessageId: '2'}));
		expect(anchor).toEqual({messageId: '2', offset: 0});
	});
});
