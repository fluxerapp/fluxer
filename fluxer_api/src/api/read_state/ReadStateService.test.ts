// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {type ChannelID, createChannelID, createMessageID, createUserID, type UserID} from '../BrandedTypes';
import type {IGatewayService} from '../infrastructure/IGatewayService';
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
