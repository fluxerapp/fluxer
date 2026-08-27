// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const makeChannel = (id: string) => ({
	id,
	type: 0,
	guildId: 'guild-1',
	isPrivate: () => false,
	getGuildId: () => 'guild-1',
});
const loadedMessages: Array<{id: string; author: {id: string}}> = [];
let hasMoreBefore = false;
let hasNewestMessages = true;

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => makeChannel(id), getBasicChannel: (id: string) => makeChannel(id)},
}));
vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {
		getMessages: () => ({
			get hasMoreBefore() {
				return hasMoreBefore;
			},
			get length() {
				return loadedMessages.length;
			},
			jumpDestinationId: null,
			hasNewestMessages: () => hasNewestMessages,
			has: (id: string) => loadedMessages.some((m) => m.id === id),
			last: () => loadedMessages[loadedMessages.length - 1],
			forEachBuffered: (cb: (m: unknown) => void) => {
				for (const m of loadedMessages) cb(m);
			},
		}),
	},
}));
vi.mock('@app/features/user/state/Users', () => ({default: {getCurrentUser: () => ({id: 'me'})}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {getMember: () => null}}));
vi.mock('@app/features/user/state/UserGuildSettings', () => ({
	default: {
		isEveryoneMentionSuppressed: () => false,
		isRoleMentionSuppressed: () => false,
		isGuildOrChannelMuted: () => false,
	},
}));
vi.mock('@app/features/ui/state/Dimension', () => ({default: {channelPinnedToEnd: () => false}}));
vi.mock('@app/features/notification/state/NotificationAutoAck', () => ({
	default: {isAutomaticAckEnabled: () => false},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {post: vi.fn(), get: vi.fn()}}));

const {default: ReadStates} = await import('@app/features/read_state/state/ReadStates');

const ID = {
	ack: '1519773906704011264',
	newer: '1519773906708205568',
};

let nextChannelId = 0;

function seedReadChannel() {
	const channelId = `channel-${++nextChannelId}`;
	const state = ReadStates.get(channelId);
	state.readStateKnown = true;
	state.ackMessageId = ID.ack;
	state.lastMessageId = ID.ack;
	state.unreadCount = 0;
	state.oldestUnreadMessageId = null;
	return {channelId, state};
}

describe('ReadStates unread invariant', () => {
	beforeEach(() => {
		loadedMessages.length = 0;
		hasMoreBefore = false;
		hasNewestMessages = true;
	});

	it('never reports a positive unread count without an unread anchor after a passive update', () => {
		const {channelId} = seedReadChannel();
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.newer}, 'guild-1');
		const count = ReadStates.getUnreadCount(channelId);
		const anchor = ReadStates.getVisualUnreadMessageId(channelId);
		expect(count > 0).toBe(anchor != null);
	});

	it('keeps the channel unread for the sidebar even with no anchor to draw a divider at', () => {
		const {channelId} = seedReadChannel();
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.newer}, 'guild-1');
		expect(ReadStates.hasUnread(channelId)).toBe(true);
	});

	it('clears a stale unread once the server walks the last message id back', () => {
		const {channelId} = seedReadChannel();
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.newer}, 'guild-1');
		expect(ReadStates.hasUnread(channelId)).toBe(true);
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.ack}, 'guild-1');
		expect(ReadStates.hasUnread(channelId)).toBe(false);
		expect(ReadStates.getUnreadCount(channelId)).toBe(0);
		expect(ReadStates.getVisualUnreadMessageId(channelId)).toBeNull();
	});

	it('anchors the divider when a window is loaded whose ack sits outside it', () => {
		const {channelId, state} = seedReadChannel();
		state.lastMessageId = ID.newer;
		hasMoreBefore = true;
		loadedMessages.push({id: ID.newer, author: {id: 'someone'}});
		ReadStates.handleLoadMessages({channelId, messages: []});
		expect(ReadStates.getVisualUnreadMessageId(channelId)).toBe(ID.newer);
		expect(ReadStates.getUnreadCount(channelId) > 0).toBe(true);
	});
});
