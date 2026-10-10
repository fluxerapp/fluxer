// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {ReadState} from '@app/api/models/ReadState';
import type {
	IReadStateRepository,
	ReadStateMarker,
	ReadStateMentionUpdate,
} from '@app/api/read_state/IReadStateRepository';
import {type ReadStateChannelHint, resolveReadStateMarker} from '@app/api/read_state/ReadStateChannelMeta';
import {visibleReadStates} from '@app/api/read_state/ReadStateVisibility';

function threadScope(readState: ReadState | null | undefined): Record<string, unknown> {
	if (!readState?.isMarked) return {};
	return {flags: readState.flags ?? 0, __thread_scoped: readState.guildId!.toString()};
}

function hadUnreadThrough(previous: ReadState | null, messageId: MessageID, unreadThrough: MessageID | null): boolean {
	const previousMessageId = previous?.lastMessageId ?? null;
	if (previousMessageId !== null && previousMessageId >= messageId) {
		return false;
	}
	if (previous !== null && previous.mentionCount > 0) {
		return true;
	}
	return unreadThrough !== null && (previousMessageId === null || previousMessageId < unreadThrough);
}

export class ReadStateService {
	constructor(
		private repository: IReadStateRepository,
		private gatewayService: IGatewayService,
	) {}

	async getReadStates(userId: UserID): Promise<Array<ReadState>> {
		return await this.repository.listReadStates(userId);
	}

	async getReadState(userId: UserID, channelId: ChannelID): Promise<ReadState | null> {
		return await this.repository.getReadState(userId, channelId);
	}

	async ackMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
		implicit?: {unreadThrough: MessageID | null};
		emitGateway?: boolean;
		capable?: boolean;
		channel?: ReadStateChannelHint | null;
	}): Promise<ReadState> {
		const {userId, channelId, messageId, mentionCount, manual, implicit, emitGateway = true} = params;
		const marker = await resolveReadStateMarker({
			userId,
			channelId,
			capable: params.capable === true,
			channel: params.channel,
		});
		const {readState, previous} = await this.repository.upsertReadState(
			userId,
			channelId,
			messageId,
			mentionCount,
			undefined,
			manual ?? false,
			marker,
		);
		if (!implicit) {
			await this.clearPushChannelNotifications({userId, channelId, messageId});
		} else if (hadUnreadThrough(previous, messageId, implicit.unreadThrough)) {
			void this.clearPushChannelNotifications({userId, channelId, messageId});
		}
		if (emitGateway) {
			await this.dispatchMessageAck({
				userId,
				channelId,
				messageId: readState.lastMessageId ?? messageId,
				mentionCount: readState.mentionCount,
				manual,
				version: readState.version,
				readState,
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
		capable = false,
	}: {
		userId: UserID;
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			mentionCount?: number;
			manual?: boolean;
		}>;
		capable?: boolean;
	}): Promise<Array<ReadState>> {
		if (readStates.length === 0) {
			return [];
		}
		const canUseBulkAck = readStates.every(
			(readState) => !readState.manual && (readState.mentionCount == null || readState.mentionCount === 0),
		);
		if (canUseBulkAck) {
			const acked = await this.bulkAckMessages({
				userId,
				capable,
				readStates: readStates.map((readState) => ({
					channelId: readState.channelId,
					messageId: readState.messageId,
				})),
			});
			return visibleReadStates(acked, {userId, capable});
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
					capable,
				}),
			);
		}
		return visibleReadStates(results, {userId, capable});
	}

	async bulkAckMessages({
		userId,
		readStates,
		capable = false,
	}: {
		userId: UserID;
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>;
		capable?: boolean;
	}): Promise<Array<ReadState>> {
		if (readStates.length === 0) {
			return [];
		}
		try {
			const markers: Array<ReadStateMarker | null> = capable
				? await Promise.all(readStates.map(({channelId}) => resolveReadStateMarker({userId, channelId, capable})))
				: [];
			const updatedReadStates = await this.repository.bulkAckMessages(
				userId,
				markers.length > 0
					? readStates.map((readState, index) => ({...readState, marker: markers[index]}))
					: readStates,
			);
			const readStatesByChannel = new Map(updatedReadStates.map((readState) => [readState.channelId, readState]));
			await Promise.all(
				readStates.map(({channelId, messageId}) =>
					Promise.all([
						this.dispatchMessageAck({
							userId,
							channelId,
							messageId: readStatesByChannel.get(channelId)?.lastMessageId ?? messageId,
							mentionCount: readStatesByChannel.get(channelId)?.mentionCount ?? 0,
							version: readStatesByChannel.get(channelId)?.version,
							readState: readStatesByChannel.get(channelId),
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

	async bulkIncrementMentionCounts(updates: Array<ReadStateMentionUpdate>): Promise<void> {
		if (updates.length === 0) {
			return;
		}
		try {
			await this.repository.bulkIncrementMentionCounts(updates);
		} catch (error) {
			Logger.error({error}, 'Bulk increment mention counts failed');
			throw error;
		}
	}

	async ackPins(params: {
		userId: UserID;
		channelId: ChannelID;
		timestamp: Date;
		capable?: boolean;
		channel?: ReadStateChannelHint | null;
	}): Promise<void> {
		const {userId, channelId, timestamp} = params;
		const marker = await resolveReadStateMarker({
			userId,
			channelId,
			capable: params.capable === true,
			channel: params.channel,
		});
		await this.repository.upsertPinAck(userId, channelId, timestamp, marker);
		await this.dispatchPinsAck({userId, channelId, timestamp, marker});
	}

	private async dispatchMessageAck(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
		version?: bigint;
		readState?: ReadState | null;
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
				...threadScope(params.readState),
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

	private async dispatchPinsAck(params: {
		userId: UserID;
		channelId: ChannelID;
		timestamp: Date;
		marker: ReadStateMarker | null;
	}): Promise<void> {
		const {userId, channelId, timestamp, marker} = params;
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'CHANNEL_PINS_ACK',
			data: {
				channel_id: channelId.toString(),
				timestamp: timestamp.toISOString(),
				...(marker ? {__thread_scoped: marker.guildId.toString()} : {}),
			},
		});
	}
}
