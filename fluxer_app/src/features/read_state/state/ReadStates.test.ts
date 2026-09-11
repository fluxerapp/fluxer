// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {Channel} from '@app/features/channel/models/Channel';
import {ACK_BATCH_DELAY_MS, type GatewayReadState} from '@app/features/read_state/state/read_states/shared';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const channels = new Map<string, Channel>();
const guilds = new Map<string, {joinedAt: string | null}>();
const blockedUserIds = new Set<string>();
let pinnedToEnd = false;
let automaticAck = false;
const loadedMessages: Array<{id: string; author: {id: string}}> = [];
let hasMoreBefore = false;
let hasNewestMessages = true;

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => channels.get(id)},
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
vi.mock('@app/features/user/state/Users', () => ({
	default: {getCurrentUser: () => ({id: 'me'}), cacheUsers: () => {}},
}));
vi.mock('@app/features/relationship/state/Relationships', () => ({
	default: {isBlocked: (id: string) => blockedUserIds.has(id)},
}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: (id: string) => guilds.get(id)}}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {getMember: () => null}}));
vi.mock('@app/features/user/state/UserGuildSettings', () => ({
	default: {
		isEveryoneMentionSuppressed: () => false,
		isRoleMentionSuppressed: () => false,
		isGuildOrChannelMuted: () => false,
	},
}));
vi.mock('@app/features/ui/state/Dimension', () => ({default: {channelPinnedToEnd: () => pinnedToEnd}}));
vi.mock('@app/features/notification/state/NotificationAutoAck', () => ({
	default: {isAutomaticAckEnabled: () => automaticAck, disableForChannel: () => {}},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {post: vi.fn(async () => ({body: {read_states: []}})), get: vi.fn()},
}));

const {default: ReadStates} = await import('@app/features/read_state/state/ReadStates');
const {http} = await import('@app/features/platform/transport/RestTransport');

const ID = {
	ack: '1519773906704011264',
	newer: '1519773906708205568',
};

let nextChannelId = 0;

function seedReadChannel() {
	const channelId = `channel-${++nextChannelId}`;
	channels.set(channelId, new Channel({id: channelId, type: ChannelTypes.GUILD_TEXT, guild_id: 'guild-1'}));
	const state = ReadStates.get(channelId);
	state.ackMessageId = ID.ack;
	state.lastMessageId = ID.ack;
	state.unreadCount = 0;
	state.oldestUnreadMessageId = null;
	return {channelId, state};
}

beforeEach(() => {
	loadedMessages.length = 0;
	hasMoreBefore = false;
	hasNewestMessages = true;
	blockedUserIds.clear();
	pinnedToEnd = false;
	automaticAck = false;
	vi.mocked(http.post).mockClear();
});

describe('ReadStates unread invariant', () => {
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

	it('ignores a passive update that walks the last message id back', () => {
		const {channelId} = seedReadChannel();
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.newer}, 'guild-1');
		expect(ReadStates.hasUnread(channelId)).toBe(true);
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.ack}, 'guild-1');
		expect(ReadStates.lastMessageId(channelId)).toBe(ID.newer);
		expect(ReadStates.hasUnread(channelId)).toBe(true);
	});

	it('never lowers the watermark when an after page comes back empty', () => {
		const {channelId} = seedReadChannel();
		loadedMessages.push({id: ID.ack, author: {id: 'someone'}});
		ReadStates.handlePassiveLastMessageUpdates({[channelId]: ID.newer}, 'guild-1');
		ReadStates.handleLoadMessages({channelId, isAfter: true, messages: []});
		expect(ReadStates.lastMessageId(channelId)).toBe(ID.newer);
		expect(ReadStates.hasUnread(channelId)).toBe(true);
	});

	it('keeps a watermark that points at a message no longer in the channel', () => {
		const {channelId, state} = seedReadChannel();
		state.lastMessageId = ID.newer;
		loadedMessages.push({id: ID.ack, author: {id: 'someone'}});
		ReadStates.handleLoadMessages({channelId, isAfter: true, messages: []});
		expect(ReadStates.lastMessageId(channelId)).toBe(ID.newer);
		expect(ReadStates.hasUnread(channelId)).toBe(true);
	});

	it('acks up to the watermark so a deleted newest message cannot keep the channel unread', () => {
		const {channelId, state} = seedReadChannel();
		state.lastMessageId = ID.newer;
		loadedMessages.push({id: ID.ack, author: {id: 'someone'}});
		ReadStates.handleLoadMessages({channelId, isAfter: true, messages: []});
		ReadStates.handleChannelAckWithStickyUnread({channelId});
		expect(ReadStates.ackMessageId(channelId)).toBe(ID.newer);
		expect(ReadStates.hasUnread(channelId)).toBe(false);
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

const ME = 'me';
const TUNA = 'tuna';

const CHANNEL = {
	dm: '1485064866382176259',
	groupDm: '1485064866382176260',
	notes: '1485064866382176261',
	guildText: '1485064866382176262',
	newDm: '1547743690000000000',
};

const MESSAGE = {
	older: '1547700000000000000',
	last: '1547743000000000000',
	incoming: '1547743701495717888',
	next: '1547743701495717890',
	own: '1547743701495717892',
	reply: '1547743701495717893',
};

function user(id: string): UserPartial {
	return {id, username: id, discriminator: '0', global_name: null, avatar: null, avatar_color: null, flags: 0};
}

function dm(id: string, lastMessageId: string | null): WireChannel {
	return {
		id,
		type: ChannelTypes.DM,
		last_message_id: lastMessageId,
		last_pin_timestamp: null,
		recipients: [user(TUNA)],
	};
}

function groupDm(lastMessageId: string | null): WireChannel {
	return {
		id: CHANNEL.groupDm,
		type: ChannelTypes.GROUP_DM,
		last_message_id: lastMessageId,
		last_pin_timestamp: null,
		recipients: [user(TUNA)],
	};
}

function guildText(lastMessageId: string): WireChannel {
	return {id: CHANNEL.guildText, type: ChannelTypes.GUILD_TEXT, guild_id: 'guild-1', last_message_id: lastMessageId};
}

function wireMessage(
	id: string,
	channelId: string,
	authorId: string,
	overrides: Partial<WireMessage> = {},
): WireMessage {
	return {
		id,
		channel_id: channelId,
		author: user(authorId),
		type: MessageTypes.DEFAULT,
		flags: 0,
		pinned: false,
		tts: false,
		mention_everyone: false,
		content: 'test',
		timestamp: '2026-09-11T00:00:00.000Z',
		mentions: [],
		mention_roles: [],
		...overrides,
	} as WireMessage;
}

function recipientAddMessage(): WireMessage {
	return wireMessage(MESSAGE.next, CHANNEL.groupDm, TUNA, {type: MessageTypes.RECIPIENT_ADD, mentions: [user(ME)]});
}

function readState(channelId: string, ackMessageId: string, mentionCount = 0): GatewayReadState {
	return {id: channelId, last_message_id: ackMessageId, mention_count: mentionCount, version: '1'};
}

function ready(readStates: Array<GatewayReadState>, openChannels: Array<WireChannel>): void {
	channels.clear();
	for (const channel of openChannels) {
		channels.set(channel.id, new Channel(channel));
	}
	ReadStates.handleGatewayReady({readState: readStates, channels: openChannels});
}

function channelCreate(channel: WireChannel): void {
	channels.set(channel.id, new Channel(channel));
	ReadStates.handleChannelCreate({channel});
}

function channelDelete(channel: {id: string; type?: number; guild_id?: string}): void {
	channels.delete(channel.id);
	ReadStates.handleChannelDelete({channel});
}

function messageCreate(message: WireMessage): void {
	ReadStates.handleIncomingMessage({channelId: message.channel_id, message});
}

function unreadState(channelId: string) {
	return {
		unread: ReadStates.hasUnread(channelId),
		unreadCount: ReadStates.getUnreadCount(channelId),
		mentionCount: ReadStates.getMentionCount(channelId),
		ackMessageId: ReadStates.ackMessageId(channelId),
	};
}

describe('ReadStates private channel open, close and reopen', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		ReadStates.clearAll();
		vi.useRealTimers();
	});

	it('shows a DM closed before READY as unread when CHANNEL_CREATE and MESSAGE_CREATE reopen it', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		expect(ReadStates.hasUnread(CHANNEL.dm)).toBe(true);
		expect(ReadStates.hasUnreadPrivateChannel(CHANNEL.dm)).toBe(true);
		ReadStates.consumePendingChanges();
		const privateChannelVersion = ReadStates.privateChannelVersion;
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(ReadStates.privateChannelVersion).toBe(privateChannelVersion + 1);
		expect(ReadStates.consumePendingChanges()).toEqual({
			all: false,
			channelIds: [CHANNEL.dm],
			changes: [{channelId: CHANNEL.dm, guildId: null}],
		});
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.older,
		});
		expect(ReadStates.hasUnreadPrivateChannel(CHANNEL.dm)).toBe(true);
		expect(ReadStates.getPrivateChannelUnreadCount(CHANNEL.dm)).toBe(1);
		expect(ReadStates.getPrivateChannelMentionCount(CHANNEL.dm)).toBe(1);
		expect(ReadStates.getOldestUnreadMessageId(CHANNEL.dm)).toBe(MESSAGE.incoming);
		expect(ReadStates.mentionChannelIds).toContain(CHANNEL.dm);
	});

	it('keeps the mentions of a DM READY closed over the limit when a message reopens it', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older, 2)], []);
		expect(unreadState(CHANNEL.dm)).toEqual({unread: false, unreadCount: 0, mentionCount: 0, ackMessageId: null});
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 2,
			ackMessageId: MESSAGE.older,
		});
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 3,
			ackMessageId: MESSAGE.older,
		});
	});

	it('shows a DM closed earlier in the session as unread when a message reopens it', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.older)]);
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.older,
		});
		expect(ReadStates.getOldestUnreadMessageId(CHANNEL.dm)).toBe(MESSAGE.incoming);
	});

	it('counts only the messages that arrive after a closed DM reopens', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.older)]);
		messageCreate(wireMessage(MESSAGE.last, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.older,
		});
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 2,
			ackMessageId: MESSAGE.older,
		});
		expect(ReadStates.getOldestUnreadMessageId(CHANNEL.dm)).toBe(MESSAGE.incoming);
	});

	it('hides a closed DM and brings its mentions back when a message reopens it', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older, 2)], [dm(CHANNEL.dm, MESSAGE.last)]);
		ReadStates.consumePendingChanges();
		const privateChannelVersion = ReadStates.privateChannelVersion;
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		expect(ReadStates.privateChannelVersion).toBe(privateChannelVersion + 1);
		expect(ReadStates.consumePendingChanges().changes).toEqual([{channelId: CHANNEL.dm, guildId: null}]);
		expect(unreadState(CHANNEL.dm)).toEqual({unread: false, unreadCount: 0, mentionCount: 0, ackMessageId: null});
		expect(ReadStates.mentionChannelIds).not.toContain(CHANNEL.dm);
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 3,
			ackMessageId: MESSAGE.older,
		});
	});

	it('keeps a closed DM out of every private channel count while its entry survives', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older, 2)], [dm(CHANNEL.dm, MESSAGE.last)]);
		expect(ReadStates.isUnreadOrMentioned(CHANNEL.dm)).toBe(true);
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		expect(ReadStates.getChannelIds()).toContain(CHANNEL.dm);
		expect(ReadStates.hasUnreadPrivateChannel(CHANNEL.dm)).toBe(false);
		expect(ReadStates.getPrivateChannelUnreadCount(CHANNEL.dm)).toBe(0);
		expect(ReadStates.getPrivateChannelMentionCount(CHANNEL.dm)).toBe(0);
		expect(ReadStates.isUnreadOrMentioned(CHANNEL.dm)).toBe(false);
	});

	it('forgets the view state of a DM when it is closed', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.last)]);
		const state = ReadStates.get(CHANNEL.dm);
		state.messagesLoaded = true;
		state.ackedManually = true;
		state.stickyUnreadMessageId = MESSAGE.last;
		state.unreadCount = 3;
		state.oldestUnreadMessageId = MESSAGE.last;
		state.estimated = true;
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		expect(ReadStates.getIfExists(CHANNEL.dm)).toMatchObject({
			messagesLoaded: false,
			ackedManually: false,
			stickyUnreadMessageId: null,
			ackMessageId: MESSAGE.older,
			unreadCount: 0,
			oldestUnreadMessageId: null,
			estimated: false,
		});
	});

	it('keeps an ack that arrives while the DM is closed', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older, 1)], [dm(CHANNEL.dm, MESSAGE.last)]);
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		ReadStates.handleMessageAck({
			channelId: CHANNEL.dm,
			messageId: MESSAGE.last,
			mentionCount: 0,
			manual: false,
			version: '2',
		});
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.last,
		});
	});

	it('ignores an older ack that arrives while the DM is closed', () => {
		ready([readState(CHANNEL.dm, MESSAGE.last, 1)], [dm(CHANNEL.dm, MESSAGE.last)]);
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		ReadStates.handleMessageAck({
			channelId: CHANNEL.dm,
			messageId: MESSAGE.older,
			mentionCount: 9,
			manual: false,
			version: '2',
		});
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 2,
			ackMessageId: MESSAGE.last,
		});
	});

	it('applies a manual ack that arrives while the DM is closed', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older, 2)], [dm(CHANNEL.dm, MESSAGE.last)]);
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		ReadStates.handleMessageAck({
			channelId: CHANNEL.dm,
			messageId: MESSAGE.last,
			mentionCount: 0,
			manual: true,
			version: '2',
		});
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.last,
		});
	});

	it('still sends the ack for a DM read and closed within the batch delay', async () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.last)]);
		ReadStates.handleChannelAck({channelId: CHANNEL.dm});
		channelDelete({id: CHANNEL.dm, type: ChannelTypes.DM});
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS);
		expect(http.post).toHaveBeenCalledWith(Endpoints.READ_STATES_ACK, {
			body: {read_states: [{channel_id: CHANNEL.dm, message_id: MESSAGE.last}]},
		});
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.last,
		});
	});

	it('shows the first messages from a new contact as unread mentions', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.newDm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.newDm, TUNA));
		expect(unreadState(CHANNEL.newDm)).toEqual({unread: true, unreadCount: 1, mentionCount: 1, ackMessageId: null});
		expect(ReadStates.getOldestUnreadMessageId(CHANNEL.newDm)).toBe(MESSAGE.incoming);
		messageCreate(wireMessage(MESSAGE.next, CHANNEL.newDm, TUNA));
		expect(unreadState(CHANNEL.newDm)).toEqual({unread: true, unreadCount: 2, mentionCount: 2, ackMessageId: null});
	});

	it('guesses the ack at the newest message for a mention-free DM without read state', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		loadedMessages.push({id: MESSAGE.last, author: {id: TUNA}}, {id: MESSAGE.incoming, author: {id: TUNA}});
		ReadStates.handleLoadMessages({channelId: CHANNEL.dm, messages: []});
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.incoming,
		});
	});

	it('characterisation: keeps a self-opened DM read when its read state covers the last message', () => {
		ready([readState(CHANNEL.dm, MESSAGE.last)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.last,
		});
	});

	it('shows a self-opened DM as unread when READY had it unread', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.older,
		});
	});

	it('shows a self-opened DM with history and no read state as unread', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({unread: true, unreadCount: 0, mentionCount: 0, ackMessageId: null});
	});

	it('keeps an open unread DM unread when a duplicate CHANNEL_CREATE arrives', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.last)]);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.older,
		});
	});

	it("characterisation: acks the current user's first message in a new DM and counts the reply", () => {
		ready([], []);
		channelCreate(dm(CHANNEL.newDm, null));
		expect(ReadStates.hasUnread(CHANNEL.newDm)).toBe(false);
		messageCreate(wireMessage(MESSAGE.own, CHANNEL.newDm, ME));
		expect(unreadState(CHANNEL.newDm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.own,
		});
		messageCreate(wireMessage(MESSAGE.reply, CHANNEL.newDm, TUNA));
		expect(unreadState(CHANNEL.newDm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.own,
		});
	});

	it("acks the current user's message into a reopened DM that READY had unread", () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		expect(ReadStates.hasUnread(CHANNEL.dm)).toBe(true);
		messageCreate(wireMessage(MESSAGE.own, CHANNEL.dm, ME));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.own,
		});
	});

	it("characterisation: acks the current user's message that CHANNEL_CREATE already carries", () => {
		ready([], []);
		channelCreate(dm(CHANNEL.newDm, MESSAGE.own));
		messageCreate(wireMessage(MESSAGE.own, CHANNEL.newDm, ME));
		expect(unreadState(CHANNEL.newDm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.own,
		});
	});

	it('characterisation: counts only the recipient add message when the user joins a group DM with history', () => {
		ready([], []);
		channelCreate(groupDm(MESSAGE.last));
		expect(ReadStates.hasUnread(CHANNEL.groupDm)).toBe(true);
		messageCreate(recipientAddMessage());
		expect(unreadState(CHANNEL.groupDm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: null,
		});
	});

	it('characterisation: counts the recipient add message when the user joins an empty group DM', () => {
		ready([], []);
		channelCreate(groupDm(null));
		messageCreate(recipientAddMessage());
		expect(unreadState(CHANNEL.groupDm)).toEqual({unread: true, unreadCount: 1, mentionCount: 1, ackMessageId: null});
	});

	it('keeps the read state of a group DM the user leaves and rejoins', () => {
		ready([readState(CHANNEL.groupDm, MESSAGE.older)], [groupDm(MESSAGE.older)]);
		channelDelete({id: CHANNEL.groupDm, type: ChannelTypes.GROUP_DM});
		channelCreate(groupDm(MESSAGE.last));
		expect(unreadState(CHANNEL.groupDm)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.older,
		});
		messageCreate(recipientAddMessage());
		expect(unreadState(CHANNEL.groupDm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.older,
		});
	});

	it('characterisation: drops the pending ack of a group DM the user leaves', async () => {
		ready([readState(CHANNEL.groupDm, MESSAGE.older)], [groupDm(MESSAGE.last)]);
		ReadStates.handleChannelAck({channelId: CHANNEL.groupDm});
		channelDelete({id: CHANNEL.groupDm, type: ChannelTypes.GROUP_DM});
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS);
		expect(http.post).not.toHaveBeenCalled();
	});

	it('keeps the read state of personal notes closed and reopened', () => {
		const notes: WireChannel = {
			id: CHANNEL.notes,
			type: ChannelTypes.DM_PERSONAL_NOTES,
			last_message_id: MESSAGE.older,
			last_pin_timestamp: null,
		};
		ready([readState(CHANNEL.notes, MESSAGE.older)], [notes]);
		channelDelete({id: CHANNEL.notes, type: ChannelTypes.DM_PERSONAL_NOTES});
		channelCreate({...notes, last_message_id: MESSAGE.last});
		expect(unreadState(CHANNEL.notes)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.older,
		});
	});

	it('characterisation: acks own messages in personal notes', () => {
		ready([], []);
		channelCreate({
			id: CHANNEL.notes,
			type: ChannelTypes.DM_PERSONAL_NOTES,
			last_message_id: MESSAGE.own,
			last_pin_timestamp: null,
		});
		messageCreate(wireMessage(MESSAGE.own, CHANNEL.notes, ME));
		expect(unreadState(CHANNEL.notes)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.own,
		});
	});

	it('characterisation: auto-acks a message in the open DM pinned to the bottom', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], [dm(CHANNEL.dm, MESSAGE.older)]);
		ReadStates.handleLoadMessages({channelId: CHANNEL.dm, messages: []});
		automaticAck = true;
		pinnedToEnd = true;
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.incoming,
		});
		expect(ReadStates.getIfExists(CHANNEL.dm)?.inFlightAckMessageId).toBe(MESSAGE.incoming);
	});

	it('characterisation: keeps a DM reopened by a blocked author read', () => {
		blockedUserIds.add(TUNA);
		ready([readState(CHANNEL.dm, MESSAGE.older)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.incoming,
		});
	});

	it('characterisation: keeps a first DM from a blocked author read', () => {
		blockedUserIds.add(TUNA);
		ready([], []);
		channelCreate(dm(CHANNEL.newDm, MESSAGE.incoming));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.newDm, TUNA));
		expect(unreadState(CHANNEL.newDm)).toEqual({
			unread: false,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: MESSAGE.incoming,
		});
	});

	it('shows the call message that reopens a DM with read state as unread', () => {
		ready([readState(CHANNEL.dm, MESSAGE.older)], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA, {type: MessageTypes.CALL}));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: MESSAGE.older,
		});
	});

	it('characterisation: shows the call message that reopens a DM without read state as unread', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.dm, TUNA, {type: MessageTypes.CALL}));
		expect(unreadState(CHANNEL.dm)).toEqual({
			unread: true,
			unreadCount: 1,
			mentionCount: 1,
			ackMessageId: null,
		});
	});

	it('shows a guild message unread when there is no read state to cover it', () => {
		ready([], [guildText(MESSAGE.incoming)]);
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.guildText, TUNA, {guild_id: 'guild-1'}));
		expect(unreadState(CHANNEL.guildText)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: null,
		});
	});

	it('characterisation: still forgets a guild channel that a visibility change hides and shows again', () => {
		ready([readState(CHANNEL.guildText, MESSAGE.older, 2)], [guildText(MESSAGE.last)]);
		expect(unreadState(CHANNEL.guildText)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 2,
			ackMessageId: MESSAGE.older,
		});
		channelDelete({id: CHANNEL.guildText, guild_id: 'guild-1'});
		channelCreate(guildText(MESSAGE.last));
		expect(unreadState(CHANNEL.guildText)).toEqual({
			unread: true,
			unreadCount: 0,
			mentionCount: 0,
			ackMessageId: null,
		});
	});
});

describe('ReadStates ack floor for channels with no read state', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		ReadStates.clearAll();
		guilds.clear();
		vi.useRealTimers();
	});

	it('shows a guild channel unread when its newest message postdates the join', () => {
		guilds.set('guild-1', {joinedAt: '2026-09-01T00:00:00.000Z'});
		ready([], [guildText(MESSAGE.last)]);
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(true);
	});

	it('leaves a guild channel read when its newest message predates the join', () => {
		guilds.set('guild-1', {joinedAt: '2026-09-11T00:00:00.000Z'});
		ready([], [guildText(MESSAGE.last)]);
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(false);
	});

	it('falls back to the channel snowflake while its guild record is missing', () => {
		ready([], [guildText(MESSAGE.last)]);
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(true);
	});

	it('recomputes the floor once the guild record arrives instead of freezing it', () => {
		ready([], [guildText(MESSAGE.last)]);
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(true);
		guilds.set('guild-1', {joinedAt: '2026-09-11T00:00:00.000Z'});
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(false);
	});

	it('never lets the floor drift forward with the wall clock', () => {
		guilds.set('guild-1', {joinedAt: null});
		ready([], [guildText(MESSAGE.last)]);
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.guildText, TUNA, {guild_id: 'guild-1'}));
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(true);
		vi.advanceTimersByTime(60 * 60 * 1000);
		expect(ReadStates.hasUnread(CHANNEL.guildText)).toBe(true);
		expect(ReadStates.getUnreadCount(CHANNEL.guildText)).toBe(1);
	});

	it('counts only the messages that postdate the join', () => {
		guilds.set('guild-1', {joinedAt: '2026-09-10T21:00:00.000Z'});
		ready([], [guildText(MESSAGE.incoming)]);
		loadedMessages.push(
			{id: MESSAGE.older, author: {id: TUNA}},
			{id: MESSAGE.last, author: {id: TUNA}},
			{id: MESSAGE.incoming, author: {id: TUNA}},
		);
		ReadStates.handleLoadMessages({channelId: CHANNEL.guildText, messages: []});
		expect(ReadStates.getUnreadCount(CHANNEL.guildText)).toBe(2);
		expect(ReadStates.getOldestUnreadMessageId(CHANNEL.guildText)).toBe(MESSAGE.last);
	});

	it("never writes an ack for someone else's message in a channel with no read state", () => {
		guilds.set('guild-1', {joinedAt: '2026-09-01T00:00:00.000Z'});
		ready([], [guildText(MESSAGE.last)]);
		messageCreate(wireMessage(MESSAGE.incoming, CHANNEL.guildText, TUNA, {guild_id: 'guild-1'}));
		expect(ReadStates.ackMessageId(CHANNEL.guildText)).toBeNull();
	});
});

describe('ReadStates guessed ack for private channels', () => {
	beforeEach(() => {
		vi.useFakeTimers();
	});

	afterEach(() => {
		ReadStates.clearAll();
		vi.useRealTimers();
	});

	it('takes the newest message as the ack when a DM has no mentions', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		loadedMessages.push({id: MESSAGE.older, author: {id: TUNA}}, {id: MESSAGE.last, author: {id: TUNA}});
		ReadStates.handleLoadMessages({channelId: CHANNEL.dm, messages: []});
		expect(ReadStates.ackMessageId(CHANNEL.dm)).toBe(MESSAGE.last);
		expect(ReadStates.hasUnread(CHANNEL.dm)).toBe(false);
	});

	it('walks back one message per mention, skipping the current user', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.incoming));
		loadedMessages.push(
			{id: MESSAGE.older, author: {id: TUNA}},
			{id: MESSAGE.last, author: {id: ME}},
			{id: MESSAGE.incoming, author: {id: TUNA}},
		);
		const state = ReadStates.get(CHANNEL.dm);
		state.mentionCount = 1;
		state.rebuild();
		expect(ReadStates.ackMessageId(CHANNEL.dm)).toBe(MESSAGE.last);
	});

	it('leaves the ack null when the window does not reach the newest message', () => {
		ready([], []);
		channelCreate(dm(CHANNEL.dm, MESSAGE.last));
		hasNewestMessages = false;
		loadedMessages.push({id: MESSAGE.older, author: {id: TUNA}});
		ReadStates.handleLoadMessages({channelId: CHANNEL.dm, messages: []});
		expect(ReadStates.ackMessageId(CHANNEL.dm)).toBeNull();
	});
});
