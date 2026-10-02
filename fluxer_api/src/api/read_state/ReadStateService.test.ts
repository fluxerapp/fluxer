// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {ReadState} from '@app/api/models/ReadState';
import type {IReadStateRepository} from '@app/api/read_state/IReadStateRepository';
import {ReadStateService} from '@app/api/read_state/ReadStateService';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {describe, expect, it, vi} from 'vitest';

const USER_ID = createUserID(20n);
const CHANNEL_ID = createChannelID(21n);
const MESSAGE_ID = createMessageID(22n);

function makeReadState(channelId: ChannelID, messageId: MessageID, mentionCount = 0): ReadState {
	return new ReadState({
		user_id: USER_ID,
		channel_id: channelId,
		message_id: messageId,
		mention_count: mentionCount,
		last_pin_timestamp: null,
		version: 5n,
	});
}

describe('ReadStateService gateway side effects after the write', () => {
	it('returns the committed read state when clearing push notifications fails', async () => {
		const stored: Array<{channelId: ChannelID; messageId: MessageID}> = [];
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => {
				stored.push({channelId, messageId});
				return {readState: makeReadState(channelId, messageId), previous: null};
			}),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			clearPushChannelNotifications: vi.fn().mockRejectedValue(new BadGatewayError()),
			dispatchPresence: vi.fn().mockResolvedValue(undefined),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		const readState = await service.ackMessage({
			userId: USER_ID,
			channelId: CHANNEL_ID,
			messageId: MESSAGE_ID,
			mentionCount: 0,
		});

		expect(readState.channelId).toBe(CHANNEL_ID);
		expect(readState.lastMessageId).toBe(MESSAGE_ID);
		expect(stored).toEqual([{channelId: CHANNEL_ID, messageId: MESSAGE_ID}]);
		expect(gatewayService.dispatchPresence).toHaveBeenCalledTimes(1);
	});

	it('acknowledges the message when the MESSAGE_ACK dispatch fails', async () => {
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => ({
				readState: makeReadState(channelId, messageId),
				previous: null,
			})),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			clearPushChannelNotifications: vi.fn().mockResolvedValue(undefined),
			dispatchPresence: vi.fn().mockRejectedValue(new BadGatewayError()),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		const readState = await service.ackMessage({
			userId: USER_ID,
			channelId: CHANNEL_ID,
			messageId: MESSAGE_ID,
			mentionCount: 0,
		});

		expect(readState.lastMessageId).toBe(MESSAGE_ID);
	});

	it('returns every entry of the entry-by-entry path when the dispatch fails', async () => {
		const stored: Array<string> = [];
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => {
				stored.push(channelId.toString());
				return {readState: makeReadState(channelId, messageId, 1), previous: null};
			}),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			clearPushChannelNotifications: vi.fn().mockResolvedValue(undefined),
			dispatchPresence: vi.fn().mockRejectedValue(new BadGatewayError()),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		const readStates = await service.ackReadStates({
			userId: USER_ID,
			readStates: [
				{channelId: CHANNEL_ID, messageId: MESSAGE_ID, manual: true},
				{channelId: createChannelID(23n), messageId: createMessageID(24n), manual: true},
			],
		});

		expect(readStates.map((readState) => readState.channelId.toString())).toEqual(['21', '23']);
		expect(stored).toEqual(['21', '23']);
	});

	it('returns the bulk acknowledged states when clearing push notifications fails', async () => {
		const updated = [makeReadState(CHANNEL_ID, MESSAGE_ID)];
		const repository = {
			bulkAckMessages: vi.fn().mockResolvedValue(updated),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			clearPushChannelNotifications: vi.fn().mockRejectedValue(new BadGatewayError()),
			dispatchPresence: vi.fn().mockResolvedValue(undefined),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		const readStates = await service.bulkAckMessages({
			userId: USER_ID,
			readStates: [{channelId: CHANNEL_ID, messageId: MESSAGE_ID}],
		});

		expect(readStates).toBe(updated);
	});
});

describe('ReadStateService implicit acknowledgements', () => {
	async function implicitAck(params: {
		previous: ReadState | null;
		messageId: MessageID;
		unreadThrough: MessageID | null;
	}): Promise<Array<MessageID>> {
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => ({
				readState: makeReadState(channelId, messageId),
				previous: params.previous,
			})),
		} as unknown as IReadStateRepository;
		const clearPushChannelNotifications = vi.fn().mockResolvedValue(undefined);
		const gatewayService = {
			clearPushChannelNotifications,
			dispatchPresence: vi.fn().mockResolvedValue(undefined),
		} as unknown as IGatewayService;
		await new ReadStateService(repository, gatewayService).ackMessage({
			userId: USER_ID,
			channelId: CHANNEL_ID,
			messageId: params.messageId,
			mentionCount: 0,
			implicit: {unreadThrough: params.unreadThrough},
			emitGateway: false,
		});
		return clearPushChannelNotifications.mock.calls.map(([call]) => call.messageId);
	}

	it('clears when the previous read state lagged the latest earlier message', async () => {
		const cleared = await implicitAck({
			previous: makeReadState(CHANNEL_ID, createMessageID(30n)),
			messageId: createMessageID(40n),
			unreadThrough: createMessageID(35n),
		});
		expect(cleared).toEqual([createMessageID(40n)]);
	});

	it('does not clear when the previous read state already covered the latest earlier message', async () => {
		const cleared = await implicitAck({
			previous: makeReadState(CHANNEL_ID, createMessageID(35n)),
			messageId: createMessageID(40n),
			unreadThrough: createMessageID(35n),
		});
		expect(cleared).toEqual([]);
	});

	it('clears when unread mentions remained', async () => {
		const cleared = await implicitAck({
			previous: makeReadState(CHANNEL_ID, createMessageID(35n), 2),
			messageId: createMessageID(40n),
			unreadThrough: createMessageID(35n),
		});
		expect(cleared).toEqual([createMessageID(40n)]);
	});

	it('does not clear when the read state was already past the acknowledged message', async () => {
		const cleared = await implicitAck({
			previous: makeReadState(CHANNEL_ID, createMessageID(50n), 2),
			messageId: createMessageID(40n),
			unreadThrough: createMessageID(40n),
		});
		expect(cleared).toEqual([]);
	});

	it('clears when there was no read state and the channel had earlier messages', async () => {
		const cleared = await implicitAck({
			previous: null,
			messageId: createMessageID(40n),
			unreadThrough: createMessageID(35n),
		});
		expect(cleared).toEqual([createMessageID(40n)]);
	});

	it('does not clear when there was no read state and no earlier message', async () => {
		const cleared = await implicitAck({previous: null, messageId: createMessageID(40n), unreadThrough: null});
		expect(cleared).toEqual([]);
	});

	it('does not wait for the push clear to finish', async () => {
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => ({
				readState: makeReadState(channelId, messageId),
				previous: makeReadState(channelId, createMessageID(30n), 1),
			})),
		} as unknown as IReadStateRepository;
		const clearPushChannelNotifications = vi.fn(() => new Promise<void>(() => {}));
		const gatewayService = {clearPushChannelNotifications} as unknown as IGatewayService;
		const readState = await new ReadStateService(repository, gatewayService).ackMessage({
			userId: USER_ID,
			channelId: CHANNEL_ID,
			messageId: createMessageID(40n),
			mentionCount: 0,
			implicit: {unreadThrough: createMessageID(35n)},
			emitGateway: false,
		});
		expect(readState.lastMessageId).toBe(createMessageID(40n));
		expect(clearPushChannelNotifications).toHaveBeenCalledTimes(1);
	});
});
