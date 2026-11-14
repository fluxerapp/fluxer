// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {chunkEntries} from '@app/features/read_state/state/read_states/shared';
import {atPreviousMillisecond} from '@fluxer/snowflake/src/SnowflakeUtils';

const logger = new Logger('ReadStateCommands');

export interface BulkAckEntry {
	channelId: string;
	messageId: string;
}

const BULK_ACK_BATCH_SIZE = 100;

function previousReadableMessageId(channelId: string, messageId: string): string | null {
	const messagesArray = Messages.getMessages(channelId).toArray();
	const messageIndex = messagesArray.findIndex((message) => message.id === messageId);
	logger.debug(`Marking message ${messageId} as unread, index: ${messageIndex}, total: ${messagesArray.length}`);
	if (messageIndex < 0) {
		logger.debug('Message not found in cache; skipping mark-as-unread request');
		return null;
	}
	return messageIndex > 0 ? messagesArray[messageIndex - 1].id : atPreviousMillisecond(messageId);
}

function latestBulkAckEntry(channelId: string): BulkAckEntry | null {
	const messageId = ReadStates.lastMessageId(channelId) ?? Channels.getChannel(channelId)?.lastMessageId ?? null;
	if (messageId == null) {
		return null;
	}
	return {channelId, messageId};
}

export function ack(channelId: string, immediate = false, force = false): void {
	logger.debug(`Acking channel ${channelId}, immediate=${immediate}, force=${force}`);
	ReadStates.handleChannelAck({channelId, immediate, force});
}

export function ackWithStickyUnread(channelId: string): void {
	logger.debug(`Acking channel ${channelId} with sticky unread preservation`);
	ReadStates.handleChannelAckWithStickyUnread({channelId});
}

async function manualAck(channelId: string, messageId: string): Promise<void> {
	try {
		logger.debug(`Manual ack: ${messageId} in ${channelId}`);
		const mentionCount = ReadStates.getManualAckMentionCount(channelId, messageId);
		await ReadStates.sendManualAck(channelId, messageId, mentionCount);
		logger.debug(`Successfully manual acked ${messageId}`);
	} catch (error) {
		logger.error(`Failed to manual ack ${messageId}:`, error);
		throw error;
	}
}

export async function markAsUnread(channelId: string, messageId: string): Promise<void> {
	const ackMessageId = previousReadableMessageId(channelId, messageId);
	if (!ackMessageId || ackMessageId === '0') {
		logger.debug('Unable to determine a previous message to ack; skipping mark-as-unread request');
		return;
	}
	logger.debug(`Acking ${ackMessageId} to mark ${messageId} as unread`);
	await manualAck(channelId, ackMessageId);
}

export function clearManualAck(channelId: string): void {
	ReadStates.handleClearManualAck({channelId});
}

export function clearStickyUnread(channelId: string): void {
	logger.debug(`Clearing sticky unread for ${channelId}`);
	ReadStates.clearStickyUnread(channelId);
}

export async function bulkAckChannels(channelIds: Array<string>): Promise<void> {
	const entries = channelIds
		.map((channelId) => latestBulkAckEntry(channelId))
		.filter((entry): entry is BulkAckEntry => entry != null);
	await bulkAckEntries(entries);
}

export async function bulkAckEntries(entries: Array<BulkAckEntry>): Promise<void> {
	if (entries.length === 0) return;
	const chunks = chunkEntries(entries, BULK_ACK_BATCH_SIZE);
	for (const chunk of chunks) {
		ReadStates.handleBulkChannelAck(chunk);
		await ReadStates.flushPendingAcks();
	}
}
