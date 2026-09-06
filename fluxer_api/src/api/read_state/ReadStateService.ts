// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '../BrandedTypes';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import {Logger} from '../Logger';
import type {ReadState} from '../models/ReadState';
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
		emitGateway?: boolean;
	}): Promise<ReadState> {
		const {userId, channelId, messageId, mentionCount, manual, silent, emitGateway = true} = params;
		const readState = await this.repository.upsertReadState(
			userId,
			channelId,
			messageId,
			mentionCount,
			undefined,
			manual ?? false,
		);
		await this.invalidatePushBadgeCount(userId);
		if (!silent) {
			await this.clearPushChannelNotifications({userId, channelId, messageId});
		}
		if (emitGateway) {
			await this.dispatchMessageAck({
				userId,
				channelId,
				messageId: readState.lastMessageId ?? messageId,
				mentionCount: readState.mentionCount,
				manual,
				version: readState.version,
			}).catch((error) => {
				Logger.error(
					{userId: userId.toString(), channelId: channelId.toString(), error},
					'Failed to dispatch MESSAGE_ACK',
				);
				return null;
			});
		}
		return readState;
	}

	async ackReadStates({
		userId,
		readStates,
	}: {
		userId: UserID;
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			mentionCount?: number;
			manual?: boolean;
		}>;
	}): Promise<Array<ReadState>> {
		if (readStates.length === 0) {
			return [];
		}
		const canUseBulkAck = readStates.every(
			(readState) => !readState.manual && (readState.mentionCount == null || readState.mentionCount === 0),
		);
		if (canUseBulkAck) {
			return await this.bulkAckMessages({
				userId,
				readStates: readStates.map((readState) => ({
					channelId: readState.channelId,
					messageId: readState.messageId,
				})),
			});
		}
		const results: Array<ReadState> = [];
		for (const readState of readStates) {
			results.push(
				await this.ackMessage({
					userId,
					channelId: readState.channelId,
					messageId: readState.messageId,
					mentionCount: readState.mentionCount ?? 0,
					manual: readState.manual,
				}),
			);
		}
		return results;
	}

	async bulkAckMessages({
		userId,
		readStates,
	}: {
		userId: UserID;
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>;
	}): Promise<Array<ReadState>> {
		if (readStates.length === 0) {
			return [];
		}
		try {
			const updatedReadStates = await this.repository.bulkAckMessages(userId, readStates);
			const readStatesByChannel = new Map(updatedReadStates.map((readState) => [readState.channelId, readState]));
			await this.invalidatePushBadgeCount(userId);
			await Promise.all(
				readStates.map(({channelId, messageId}) =>
					Promise.all([
						this.dispatchMessageAck({
							userId,
							channelId,
							messageId: readStatesByChannel.get(channelId)?.lastMessageId ?? messageId,
							mentionCount: readStatesByChannel.get(channelId)?.mentionCount ?? 0,
							version: readStatesByChannel.get(channelId)?.version,
						}).catch((error) => {
							Logger.error(
								{userId: userId.toString(), channelId: channelId.toString(), error},
								'Failed to dispatch MESSAGE_ACK for bulk ack',
							);
							return null;
						}),
						this.clearPushChannelNotifications({userId, channelId, messageId}),
					]),
				),
			);
			return updatedReadStates;
		} catch (error) {
			Logger.error({userId: userId.toString(), error}, 'Bulk ack messages failed');
			throw error;
		}
	}

	async deleteReadState({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		await this.repository.deleteReadState(userId, channelId);
		await this.invalidatePushBadgeCount(userId);
	}

	async incrementMentionCount({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		const readState = await this.repository.incrementReadStateMentions(userId, channelId, messageId, 1);
		if (readState == null) {
			return;
		}
		await this.invalidatePushBadgeCount(userId);
	}

	async bulkIncrementMentionCounts(
		updates: Array<{
			userId: UserID;
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<void> {
		if (updates.length === 0) {
			return;
		}
		try {
			const appliedUpdates = await this.repository.bulkIncrementMentionCounts(updates);
			const uniqueUserIds = Array.from(new Set(appliedUpdates.map((update) => update.userId)));
			if (uniqueUserIds.length === 0) {
				return;
			}
			await this.gatewayService.invalidatePushBadgeCounts({userIds: uniqueUserIds}).catch((error) => {
				Logger.error({userCount: uniqueUserIds.length, error}, 'Failed to invalidate push badge counts');
				return null;
			});
		} catch (error) {
			Logger.error({error}, 'Bulk increment mention counts failed');
			throw error;
		}
	}

	async ackPins(params: {userId: UserID; channelId: ChannelID; timestamp: Date}): Promise<void> {
		const {userId, channelId, timestamp} = params;
		await this.repository.upsertPinAck(userId, channelId, timestamp);
		await this.dispatchPinsAck({userId, channelId, timestamp});
	}

	private async invalidatePushBadgeCount(userId: UserID): Promise<void> {
		await this.gatewayService.invalidatePushBadgeCount({userId}).catch((error) => {
			Logger.error({userId: userId.toString(), error}, 'Failed to invalidate push badge count');
			return null;
		});
	}

	private async dispatchMessageAck(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
		version?: bigint;
	}): Promise<void> {
		const {userId, channelId, messageId, mentionCount, manual, version} = params;
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'MESSAGE_ACK',
			data: {
				channel_id: channelId.toString(),
				message_id: messageId.toString(),
				mention_count: mentionCount,
				manual,
				version: version?.toString(),
			},
		});
	}

	private async clearPushChannelNotifications(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		const {userId, channelId, messageId} = params;
		await this.gatewayService.clearPushChannelNotifications({userId, channelId, messageId}).catch((error) => {
			Logger.error(
				{userId: userId.toString(), channelId: channelId.toString(), messageId: messageId.toString(), error},
				'Failed to clear stale push notifications for read channel',
			);
			return null;
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
