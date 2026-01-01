/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {ReadState} from '~/Models';
import type {IReadStateRepository} from './IReadStateRepository';

export class ReadStateService {
	constructor(
		private repository: IReadStateRepository,
		private gatewayService: IGatewayService,
	) {}

	async getReadStates(userId: UserID): Promise<Array<ReadState>> {
		return await this.repository.listReadStates(userId);
	}

	async ackMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
		silent?: boolean;
	}): Promise<void> {
		const {userId, channelId, messageId, mentionCount, manual, silent} = params;
		await this.repository.upsertReadState(userId, channelId, messageId, mentionCount);
		await this.gatewayService.invalidatePushBadgeCount({userId});

		if (!silent) {
			await this.dispatchMessageAck({
				userId,
				channelId,
				messageId,
				mentionCount,
				manual,
			});
		}
	}

	async bulkAckMessages({
		userId,
		readStates,
	}: {
		userId: UserID;
		readStates: Array<{channelId: ChannelID; messageId: MessageID}>;
	}): Promise<void> {
		await Promise.all(readStates.map((readState) => this.ackMessage({...readState, userId, mentionCount: 0})));
		await this.gatewayService.invalidatePushBadgeCount({userId});
	}

	async deleteReadState({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		await this.repository.deleteReadState(userId, channelId);
		await this.gatewayService.invalidatePushBadgeCount({userId});
	}

	async incrementMentionCount({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		await this.repository.incrementReadStateMentions(userId, channelId, 1);
		await this.gatewayService.invalidatePushBadgeCount({userId});
	}

	async bulkIncrementMentionCounts(updates: Array<{userId: UserID; channelId: ChannelID}>): Promise<void> {
		if (updates.length === 0) {
			return;
		}
		await this.repository.bulkIncrementMentionCounts(updates);
		const uniqueUserIds = Array.from(new Set(updates.map((update) => update.userId)));
		await Promise.all(uniqueUserIds.map((userId) => this.gatewayService.invalidatePushBadgeCount({userId})));
	}

	async ackPins(params: {userId: UserID; channelId: ChannelID; timestamp: Date}): Promise<void> {
		const {userId, channelId, timestamp} = params;
		await this.repository.upsertPinAck(userId, channelId, timestamp);
		await this.dispatchPinsAck({userId, channelId, timestamp});
	}

	private async dispatchMessageAck(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
	}): Promise<void> {
		const {userId, channelId, messageId, mentionCount, manual} = params;
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'MESSAGE_ACK',
			data: {
				channel_id: channelId.toString(),
				message_id: messageId.toString(),
				mention_count: mentionCount,
				manual,
			},
		});
	}

	private async dispatchPinsAck(params: {userId: UserID; channelId: ChannelID; timestamp: Date}): Promise<void> {
		const {userId, channelId, timestamp} = params;
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'CHANNEL_PINS_ACK',
			data: {
				channel_id: channelId.toString(),
				timestamp: timestamp.toISOString(),
			},
		});
	}
}
