// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

interface AckRequestBody {
	read_states: Array<{channel_id: string; message_id: string}>;
}

const makeChannel = (id: string) => ({
	id,
	type: 0,
	guildId: 'guild-1',
	isPrivate: () => false,
	getGuildId: () => 'guild-1',
});

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => makeChannel(id), getBasicChannel: (id: string) => makeChannel(id)},
}));
vi.mock('@app/features/messaging/state/MessagingMessages', () => ({
	default: {
		getMessages: () => ({
			hasMoreBefore: false,
			length: 0,
			jumpDestinationId: null,
			hasNewestMessages: () => true,
			has: () => false,
			last: () => undefined,
			forEachBuffered: () => undefined,
		}),
	},
}));
let currentUserId = 'me';
vi.mock('@app/features/user/state/Users', () => ({default: {getCurrentUser: () => ({id: currentUserId})}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => undefined}}));
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
	default: {isAutomaticAckEnabled: () => false, disableForChannel: () => undefined},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {post: vi.fn(async () => ({body: {read_states: []}})), get: vi.fn()},
}));

const {default: ReadStates} = await import('@app/features/read_state/state/ReadStates');
const {Endpoints} = await import('@app/features/app/constants/Endpoints');
const {http} = await import('@app/features/platform/transport/RestTransport');
const post = vi.mocked(http.post);

const ID = {
	ack: '1519773906704011264',
	newer: '1519773906708205568',
};

let nextChannelId = 0;

function seedReadChannel() {
	const channelId = `ack-channel-${++nextChannelId}`;
	const state = ReadStates.get(channelId);
	state.ackMessageId = ID.ack;
	state.lastMessageId = ID.ack;
	state.unreadCount = 0;
	state.oldestUnreadMessageId = null;
	return {channelId, state};
}

function ackedMessageIdsFor(channelId: string): Array<string> {
	return post.mock.calls
		.filter(([url]) => url === Endpoints.READ_STATES_ACK)
		.flatMap(([, options]) => (options?.body as AckRequestBody | undefined)?.read_states ?? [])
		.filter((entry) => entry.channel_id === channelId)
		.map((entry) => entry.message_id);
}

function hydrateReadChannelSnapshot(channelId: string): void {
	ReadStates.hydrateFromSnapshot(
		new Map([
			[
				channelId,
				{
					ackMessageId: ID.ack,
					ackPinTimestamp: 0,
					mentionCount: 0,
					serverVersion: null,
					readStateKnown: true,
					lastMessageId: ID.ack,
					guildId: 'guild-1',
				},
			],
		]),
	);
}

describe('ReadStates ack queue across gateway ready', () => {
	beforeEach(() => {
		currentUserId = 'me';
		post.mockClear();
		vi.useFakeTimers();
	});

	afterEach(() => {
		vi.clearAllTimers();
		vi.useRealTimers();
	});

	it('flushes a queued ack before gateway ready reseeds the store', async () => {
		const {channelId} = seedReadChannel();
		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		ReadStates.handleGatewayReady({readState: [{id: channelId, last_message_id: ID.ack}], channels: []});
		expect(ackedMessageIdsFor(channelId)).toEqual([ID.newer]);
		await vi.advanceTimersByTimeAsync(0);
	});

	it('keeps a queued ack across gateway ready even while an earlier ack is still in flight', async () => {
		const {channelId: stalledChannelId} = seedReadChannel();
		const {channelId} = seedReadChannel();
		const stalledPost: {release: () => void} = {release: () => undefined};
		post.mockImplementationOnce(
			async () =>
				await new Promise((resolve) => {
					stalledPost.release = () => resolve({body: {}} as never);
				}),
		);
		ReadStates.handleChannelAck({channelId: stalledChannelId, messageId: ID.newer});
		await vi.advanceTimersByTimeAsync(3000);

		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		ReadStates.handleGatewayReady({readState: [{id: channelId, last_message_id: ID.ack}], channels: []});
		stalledPost.release();
		await vi.advanceTimersByTimeAsync(5000);

		expect(ackedMessageIdsFor(channelId)).toContain(ID.newer);
	});

	it('does not carry a queued ack across an account switch', async () => {
		const {channelId} = seedReadChannel();
		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		post.mockClear();

		currentUserId = 'someone-else';
		ReadStates.handleGatewayReady({readState: [{id: channelId, last_message_id: ID.ack}], channels: []});
		await vi.advanceTimersByTimeAsync(5000);

		expect(ackedMessageIdsFor(channelId)).toEqual([]);
	});

	it('flushes a queued ack before a snapshot hydrate reseeds the store', async () => {
		const {channelId} = seedReadChannel();
		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		hydrateReadChannelSnapshot(channelId);
		expect(ackedMessageIdsFor(channelId)).toEqual([ID.newer]);
		await vi.advanceTimersByTimeAsync(0);
	});

	it('does not carry a queued ack across an account switch snapshot hydrate', async () => {
		const {channelId} = seedReadChannel();
		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		post.mockClear();

		currentUserId = 'someone-else';
		hydrateReadChannelSnapshot(channelId);
		await vi.advanceTimersByTimeAsync(5000);

		expect(ackedMessageIdsFor(channelId)).toEqual([]);
	});

	it('drops a queued ack the server already covers', async () => {
		const {channelId} = seedReadChannel();
		ReadStates.handleChannelAck({channelId, messageId: ID.newer});
		post.mockClear();

		ReadStates.handleGatewayReady({readState: [{id: channelId, last_message_id: ID.newer}], channels: []});
		await vi.advanceTimersByTimeAsync(5000);

		expect(ackedMessageIdsFor(channelId)).toEqual([]);
	});
});
