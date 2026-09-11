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
let pinnedToEnd = false;
let automaticAck = false;
const loadedMessages: Array<{id: string; author: {id: string}}> = [];

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => channels.get(id)},
}));
vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {
		getMessages: () => ({
			get hasMoreBefore() {
				return false;
			},
			get length() {
				return loadedMessages.length;
			},
			jumpDestinationId: null,
			hasNewestMessages: () => true,
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
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
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

const CHANNEL = '1485064866382176262';
const MESSAGE = {
	acked: '1547700000000000000',
	unread: '1547743000000000000',
};

function user(id: string): UserPartial {
	return {id, username: id, discriminator: '0', global_name: null, avatar: null, avatar_color: null, flags: 0};
}

function guildText(lastMessageId: string): WireChannel {
	return {id: CHANNEL, type: ChannelTypes.GUILD_TEXT, guild_id: 'guild-1', last_message_id: lastMessageId};
}

function readState(ackMessageId: string): GatewayReadState {
	return {id: CHANNEL, last_message_id: ackMessageId, mention_count: 0, version: '1'};
}

function ready(): void {
	channels.clear();
	channels.set(CHANNEL, new Channel(guildText(MESSAGE.unread)));
	ReadStates.handleGatewayReady({readState: [readState(MESSAGE.acked)], channels: [guildText(MESSAGE.unread)]});
}

function wireMessage(id: string, authorId: string): WireMessage {
	return {
		id,
		channel_id: CHANNEL,
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
	} as unknown as WireMessage;
}

function openChannel(): void {
	loadedMessages.push({id: MESSAGE.unread, author: {id: 'tuna'}});
	ReadStates.handleLoadMessages({channelId: CHANNEL, isAfter: false, messages: [{id: MESSAGE.unread}] as never});
	pinnedToEnd = true;
	automaticAck = true;
	ReadStates.handleChannelAckWithStickyUnread({channelId: CHANNEL});
}

describe('ReadStates ack batching', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		loadedMessages.length = 0;
		pinnedToEnd = false;
		automaticAck = false;
		vi.mocked(http.post).mockClear();
	});

	afterEach(() => {
		ReadStates.clearAll();
		vi.useRealTimers();
	});

	it('sends the ack one batch delay after a quiet channel is read', async () => {
		ready();
		expect(ReadStates.hasUnread(CHANNEL)).toBe(true);
		openChannel();
		expect(ReadStates.hasUnread(CHANNEL)).toBe(false);
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS - 1);
		expect(http.post).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(1);
		expect(http.post).toHaveBeenCalledWith(Endpoints.READ_STATES_ACK, {
			body: {read_states: [{channel_id: CHANNEL, message_id: MESSAGE.unread}]},
		});
	});

	it('keeps the batch window anchored to the first ack while messages keep arriving', async () => {
		ready();
		openChannel();
		let messageId = 1547743900000000000n;
		for (let i = 0; i < 3; i++) {
			await vi.advanceTimersByTimeAsync(900);
			messageId += 1000000n;
			ReadStates.handleIncomingMessage({channelId: CHANNEL, message: wireMessage(messageId.toString(), 'tuna')});
		}
		expect(http.post).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS - 2700);
		expect(http.post).toHaveBeenCalledTimes(1);
		expect(http.post).toHaveBeenCalledWith(Endpoints.READ_STATES_ACK, {
			body: {read_states: [{channel_id: CHANNEL, message_id: messageId.toString()}]},
		});
	});

	it('keeps acking a channel read for a long stretch of steady traffic', async () => {
		ready();
		openChannel();
		let messageId = 1547743900000000000n;
		for (let i = 0; i < 22; i++) {
			await vi.advanceTimersByTimeAsync(900);
			messageId += 1000000n;
			ReadStates.handleIncomingMessage({channelId: CHANNEL, message: wireMessage(messageId.toString(), 'tuna')});
		}
		expect(vi.mocked(http.post).mock.calls.length).toBeGreaterThanOrEqual(3);
	});
});
