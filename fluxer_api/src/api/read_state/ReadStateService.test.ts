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
				return makeReadState(channelId, messageId);
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
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) =>
				makeReadState(channelId, messageId),
			),
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
				return makeReadState(channelId, messageId, 1);
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
