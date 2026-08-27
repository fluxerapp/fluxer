// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import {ChannelStreamType, createChannelStream} from '@app/features/messaging/utils/MessageGroupingUtils';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'me'}}));
vi.mock('@app/features/moderation/state/LocalUserSpamOverride', () => ({
	default: {isUserMarkedAsSpammer: () => false},
}));
vi.mock('@app/features/user/utils/DateFormatting', () => ({
	getFormattedFullDate: (date: Date) => date.toISOString().slice(0, 10),
}));

const AUTHOR = {id: 'author-1', flags: 0} as Message['author'];

const ID = {
	first: '1519773906704011264',
	second: '1519773906708205568',
	third: '1519773906712399872',
	fourth: '1519773906716594176',
} as const;

function message(id: string, minute: number, blocked = false): Message {
	return {
		id,
		author: AUTHOR,
		blocked,
		ignored: false,
		timestamp: new Date(Date.UTC(2026, 7, 27, 12, minute)),
		type: 0,
		webhookId: null,
		mentions: [],
		mentionRoles: [],
		mentionEveryone: false,
		isUserMessage: () => true,
		hasFlag: () => false,
	} as unknown as Message;
}

function channelMessages(messages: Array<Message>): ChannelMessages {
	return {
		forEach: (callback: (message: Message) => boolean | undefined) => {
			for (const entry of messages) {
				if (callback(entry) === false) return;
			}
		},
		jumpDestinationId: null,
		jumpHighlight: false,
		jumpTicket: null,
	} as unknown as ChannelMessages;
}

const channel = {id: 'channel-1', isPrivate: () => false, getGuildId: () => 'guild-1'} as unknown as Channel;

function buildStream(messages: Array<Message>, oldestUnreadMessageId: string | null) {
	return createChannelStream({
		channel,
		messages: channelMessages(messages),
		oldestUnreadMessageId,
		treatSpam: false,
	});
}

function countUnreadMarkers(stream: Array<ReturnType<typeof buildStream>[number]>): number {
	let count = 0;
	for (const item of stream) {
		if (item.type === ChannelStreamType.DIVIDER && item.unreadId != null) count++;
		if (item.type === ChannelStreamType.MESSAGE && item.showUnreadDividerBefore) count++;
		if (Array.isArray(item.content)) {
			count += countUnreadMarkers(item.content as Array<ReturnType<typeof buildStream>[number]>);
		}
	}
	return count;
}

function findUnreadDivider(stream: Array<ReturnType<typeof buildStream>[number]>) {
	for (const item of stream) {
		if (item.type === ChannelStreamType.DIVIDER && item.unreadId != null) return item;
		if (item.type === ChannelStreamType.MESSAGE && item.showUnreadDividerBefore) return item;
		if (item.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED) {
			const nested = item.content as Array<ReturnType<typeof buildStream>[number]>;
			const divider = nested.find((entry) => entry.type === ChannelStreamType.DIVIDER && entry.unreadId != null);
			if (divider) return divider;
		}
	}
	return null;
}

describe('createChannelStream unread divider', () => {
	it('marks the unread boundary on a plain message', () => {
		const messages = [message(ID.first, 0), message(ID.second, 1), message(ID.third, 2)];
		expect(findUnreadDivider(buildStream(messages, ID.second))).not.toBeNull();
	});

	it('emits a divider inside the collapsed group when the boundary is a blocked message', () => {
		const messages = [message(ID.first, 0), message(ID.second, 1, true), message(ID.third, 2, true)];
		const stream = buildStream(messages, ID.second);
		const group = stream.find((item) => item.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED);
		expect(group).toBeDefined();
		const nested = group!.content as Array<(typeof stream)[number]>;
		expect(nested.some((item) => item.type === ChannelStreamType.DIVIDER && item.unreadId === ID.second)).toBe(true);
		expect(group!.hasUnread).toBe(true);
	});

	it('emits a divider inside the collapsed group when the stored boundary is no longer loaded', () => {
		const messages = [message(ID.third, 0, true), message(ID.fourth, 1, true)];
		const stream = buildStream(messages, ID.first);
		const group = stream.find((item) => item.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED);
		expect(group).toBeDefined();
		const nested = group!.content as Array<(typeof stream)[number]>;
		expect(nested.some((item) => item.type === ChannelStreamType.DIVIDER && item.unreadId === ID.third)).toBe(true);
	});

	it('never leaves the boundary unrepresented when there is an unread message id', () => {
		const messages = [message(ID.first, 0), message(ID.second, 1, true), message(ID.third, 2)];
		expect(findUnreadDivider(buildStream(messages, ID.second))).not.toBeNull();
	});

	it('merges the boundary into the date divider when it is the first message of a day', () => {
		const dayOne = message(ID.first, 0);
		const dayTwo = message(ID.third, 1);
		(dayTwo as {timestamp: Date}).timestamp = new Date(Date.UTC(2026, 7, 28, 9, 0));
		const stream = buildStream([dayOne, dayTwo], ID.third);
		const dateDivider = stream.find((item) => item.type === ChannelStreamType.DIVIDER && item.unreadId === ID.third);
		expect(dateDivider).toBeDefined();
		expect(countUnreadMarkers(stream)).toBe(1);
	});

	it('marks the boundary exactly once', () => {
		const messages = [message(ID.first, 0), message(ID.second, 1, true), message(ID.third, 2)];
		expect(countUnreadMarkers(buildStream(messages, ID.second))).toBe(1);
		expect(countUnreadMarkers(buildStream([message(ID.first, 0), message(ID.second, 1)], ID.second))).toBe(1);
	});
});
