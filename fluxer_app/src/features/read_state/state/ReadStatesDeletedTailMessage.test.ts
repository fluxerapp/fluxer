// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {Channel} from '@app/features/channel/models/Channel';
import {ACK_BATCH_DELAY_MS, type GatewayReadState} from '@app/features/read_state/state/read_states/shared';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const channels = new Map<string, Channel>();
let pinnedToEnd = false;
let automaticAck = false;
let hasNewest = true;
const loadedMessages: Array<{id: string; author: {id: string}}> = [];

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: (id: string) => channels.get(id)}}));
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
			hasNewestMessages: () => hasNewest,
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

const CHANNEL = '1431490439357088063';
const PHANTOM = '1547836192152621056';
const REAL = '1546984669772255232';
const OLD_ACK = '1546000000000000000';

function guildText(lastMessageId: string): WireChannel {
	return {id: CHANNEL, type: ChannelTypes.GUILD_TEXT, guild_id: '1431490056488128806', last_message_id: lastMessageId};
}

function readState(ackMessageId: string): GatewayReadState {
	return {id: CHANNEL, last_message_id: ackMessageId, mention_count: 0, version: '1'};
}

function ready(ackMessageId: string): void {
	channels.clear();
	channels.set(CHANNEL, new Channel(guildText(PHANTOM)));
	ReadStates.handleGatewayReady({readState: [readState(ackMessageId)], channels: [guildText(PHANTOM)]});
}

function lastAckedMessageId(): string | null {
	const calls = vi.mocked(http.post).mock.calls;
	if (calls.length === 0) return null;
	const body = calls[calls.length - 1][1] as {body: {read_states: Array<{message_id: string}>}};
	return body.body.read_states[0].message_id;
}

describe('channel whose newest message was deleted', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		loadedMessages.length = 0;
		pinnedToEnd = false;
		automaticAck = false;
		hasNewest = true;
		vi.mocked(http.post).mockClear();
	});

	afterEach(() => {
		ReadStates.clearAll();
		vi.useRealTimers();
	});

	it('acks up to the channel watermark rather than the newest surviving message', async () => {
		ready(OLD_ACK);
		expect(ReadStates.hasUnread(CHANNEL)).toBe(true);
		loadedMessages.push({id: REAL, author: {id: 'tuna'}});
		ReadStates.handleLoadMessages({channelId: CHANNEL, isAfter: false, messages: [{id: REAL}] as never});
		ReadStates.handleLoadMessages({channelId: CHANNEL, isAfter: true, messages: []});
		expect(ReadStates.lastMessageId(CHANNEL)).toBe(PHANTOM);
		pinnedToEnd = true;
		automaticAck = true;
		ReadStates.handleChannelAckWithStickyUnread({channelId: CHANNEL});
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS);
		expect(http.post).toHaveBeenCalledWith(Endpoints.READ_STATES_ACK, {
			body: {read_states: [{channel_id: CHANNEL, message_id: PHANTOM}]},
		});
		expect(ReadStates.hasUnread(CHANNEL)).toBe(false);
	});

	it('stays read after a reload', async () => {
		ready(OLD_ACK);
		loadedMessages.push({id: REAL, author: {id: 'tuna'}});
		ReadStates.handleLoadMessages({channelId: CHANNEL, isAfter: false, messages: [{id: REAL}] as never});
		ReadStates.handleLoadMessages({channelId: CHANNEL, isAfter: true, messages: []});
		pinnedToEnd = true;
		automaticAck = true;
		ReadStates.handleChannelAckWithStickyUnread({channelId: CHANNEL});
		await vi.advanceTimersByTimeAsync(ACK_BATCH_DELAY_MS);
		const acked = lastAckedMessageId();
		expect(acked).not.toBeNull();
		ReadStates.clearAll();
		ready(acked as string);
		expect(ReadStates.hasUnread(CHANNEL)).toBe(false);
	});
});
