// SPDX-License-Identifier: AGPL-3.0-or-later

import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {describe, expect, it, vi} from 'vitest';
import {
	type ChannelID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '../BrandedTypes';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import {ReadState} from '../models/ReadState';
import type {IReadStateRepository} from './IReadStateRepository';
import {ReadStateService} from './ReadStateService';

describe('ReadStateService.bulkIncrementMentionCounts', () => {
	it('invalidates badge counts for touched users in a single bulk call', async () => {
		const channelId = createChannelID(2n);
		const messageId = createMessageID(3n);
		const touched: Array<{userId: UserID; channelId: ChannelID}> = [
			{userId: createUserID(10n), channelId},
			{userId: createUserID(11n), channelId},
			{userId: createUserID(10n), channelId: createChannelID(4n)},
		];
		const repository = {
			bulkIncrementMentionCounts: vi.fn().mockResolvedValue(touched),
		} as unknown as IReadStateRepository;
		const invalidatePushBadgeCounts = vi.fn().mockResolvedValue(undefined);
		const invalidatePushBadgeCount = vi.fn().mockResolvedValue(undefined);
		const gatewayService = {
			invalidatePushBadgeCounts,
			invalidatePushBadgeCount,
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		await service.bulkIncrementMentionCounts([
			{userId: createUserID(10n), channelId, messageId},
			{userId: createUserID(11n), channelId, messageId},
			{userId: createUserID(12n), channelId, messageId},
		]);

		expect(invalidatePushBadgeCount).not.toHaveBeenCalled();
		expect(invalidatePushBadgeCounts).toHaveBeenCalledTimes(1);
		expect(invalidatePushBadgeCounts).toHaveBeenCalledWith({userIds: [createUserID(10n), createUserID(11n)]});
	});

	it('skips the bulk call when no read state was touched', async () => {
		const repository = {
			bulkIncrementMentionCounts: vi.fn().mockResolvedValue([]),
		} as unknown as IReadStateRepository;
		const invalidatePushBadgeCounts = vi.fn().mockResolvedValue(undefined);
		const gatewayService = {invalidatePushBadgeCounts} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		await service.bulkIncrementMentionCounts([
			{userId: createUserID(10n), channelId: createChannelID(2n), messageId: createMessageID(3n)},
		]);

		expect(invalidatePushBadgeCounts).not.toHaveBeenCalled();
	});
});

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
	it('returns the committed read state when the badge invalidation fails', async () => {
		const stored: Array<{channelId: ChannelID; messageId: MessageID}> = [];
		const repository = {
			upsertReadState: vi.fn(async (_userId: UserID, channelId: ChannelID, messageId: MessageID) => {
				stored.push({channelId, messageId});
				return makeReadState(channelId, messageId);
			}),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			invalidatePushBadgeCount: vi.fn().mockRejectedValue(new BadGatewayError()),
			clearPushChannelNotifications: vi.fn().mockResolvedValue(undefined),
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
			invalidatePushBadgeCount: vi.fn().mockResolvedValue(undefined),
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
			invalidatePushBadgeCount: vi.fn().mockResolvedValue(undefined),
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

	it('deletes the read state when the badge invalidation fails', async () => {
		const deleteReadState = vi.fn().mockResolvedValue(undefined);
		const repository = {deleteReadState} as unknown as IReadStateRepository;
		const gatewayService = {
			invalidatePushBadgeCount: vi.fn().mockRejectedValue(new BadGatewayError()),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		await expect(service.deleteReadState({userId: USER_ID, channelId: CHANNEL_ID})).resolves.toBeUndefined();

		expect(deleteReadState).toHaveBeenCalledWith(USER_ID, CHANNEL_ID);
	});

	it('increments the mention count when the badge invalidation fails', async () => {
		const incrementReadStateMentions = vi.fn().mockResolvedValue(makeReadState(CHANNEL_ID, MESSAGE_ID, 1));
		const repository = {incrementReadStateMentions} as unknown as IReadStateRepository;
		const gatewayService = {
			invalidatePushBadgeCount: vi.fn().mockRejectedValue(new BadGatewayError()),
		} as unknown as IGatewayService;
		const service = new ReadStateService(repository, gatewayService);

		await expect(
			service.incrementMentionCount({userId: USER_ID, channelId: CHANNEL_ID, messageId: MESSAGE_ID}),
		).resolves.toBeUndefined();

		expect(incrementReadStateMentions).toHaveBeenCalledTimes(1);
	});

	it('returns the bulk acknowledged states when the badge invalidation fails', async () => {
		const updated = [makeReadState(CHANNEL_ID, MESSAGE_ID)];
		const repository = {
			bulkAckMessages: vi.fn().mockResolvedValue(updated),
		} as unknown as IReadStateRepository;
		const gatewayService = {
			invalidatePushBadgeCount: vi.fn().mockRejectedValue(new BadGatewayError()),
			clearPushChannelNotifications: vi.fn().mockResolvedValue(undefined),
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
