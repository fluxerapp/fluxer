// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUnreadDividerBeforeMessageId} from '@app/features/channel/components/ChannelMessageStreamUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ChannelStreamItem} from '@app/features/messaging/utils/MessageGroupingUtils';
import {describe, expect, it} from 'vitest';

function item(messageId: string, showUnreadDividerBefore = false): ChannelStreamItem {
	return {
		type: 'MESSAGE',
		content: {id: messageId} as Message,
		groupId: 'group-1',
		showUnreadDividerBefore,
	};
}

describe('ChannelMessageStreamUtils', () => {
	it('returns the first pending message with a visible unread divider', () => {
		expect(getUnreadDividerBeforeMessageId([item('a'), item('b', true), item('c', true)])).toBe('b');
	});

	it('returns null when unread indicators are suppressed', () => {
		expect(getUnreadDividerBeforeMessageId([item('a', true)], true)).toBeNull();
	});
});
