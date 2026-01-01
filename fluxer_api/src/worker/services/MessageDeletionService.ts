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

import {type ChannelID, createChannelID, type MessageID, type UserID} from '~/BrandedTypes';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {purgeMessageAttachments} from '~/channel/services/message/MessageHelpers';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import {Logger} from '~/Logger';
import type {Message} from '~/Models';
import {extractTimestamp} from '~/utils/SnowflakeUtils';
import {chunkArray} from '../tasks/utils/messageDeletion';
import {ChannelEventDispatcher} from './ChannelEventDispatcher';

interface MessageDeletionServiceDeps {
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
	storageService: IStorageService;
	cloudflarePurgeQueue: ICloudflarePurgeQueue;
}

interface BulkDeleteOptions {
	beforeTimestamp?: number;
	onProgress?: (deleted: number) => void;
}

interface MessageWithChannel {
	channelId: ChannelID;
	messageId: MessageID;
	message: Message;
}

export class MessageDeletionService {
	private readonly eventDispatcher: ChannelEventDispatcher;
	private readonly FETCH_BATCH_SIZE = 100;
	private readonly DELETE_BATCH_SIZE = 100;

	constructor(private readonly deps: MessageDeletionServiceDeps) {
		this.eventDispatcher = new ChannelEventDispatcher({gatewayService: deps.gatewayService});
	}

	async deleteUserMessagesBulk(userId: UserID, options: BulkDeleteOptions = {}): Promise<number> {
		const {beforeTimestamp = Number.POSITIVE_INFINITY, onProgress} = options;

		Logger.debug({userId, beforeTimestamp}, 'Starting bulk user message deletion');

		const messagesByChannel = await this.collectUserMessages(userId, beforeTimestamp);

		let totalDeleted = 0;
		for (const [channelIdStr, messages] of messagesByChannel.entries()) {
			const deleted = await this.deleteMessagesInChannel(channelIdStr, messages);
			totalDeleted += deleted;
			onProgress?.(totalDeleted);
		}

		Logger.debug({userId, totalDeleted}, 'Bulk user message deletion completed');
		return totalDeleted;
	}

	private async collectUserMessages(
		userId: UserID,
		beforeTimestamp: number,
	): Promise<Map<string, Array<MessageWithChannel>>> {
		const messagesByChannel = new Map<string, Array<MessageWithChannel>>();

		let lastChannelId: ChannelID | undefined;
		let lastMessageId: MessageID | undefined;

		while (true) {
			const messageRefs = await this.deps.channelRepository.listMessagesByAuthor(
				userId,
				this.FETCH_BATCH_SIZE,
				lastChannelId,
				lastMessageId,
			);

			if (messageRefs.length === 0) {
				break;
			}

			for (const {channelId, messageId} of messageRefs) {
				const messageTimestamp = extractTimestamp(messageId);
				if (messageTimestamp > beforeTimestamp) {
					continue;
				}

				const message = await this.deps.channelRepository.getMessage(channelId, messageId);
				if (message && message.authorId === userId) {
					const channelIdStr = channelId.toString();
					if (!messagesByChannel.has(channelIdStr)) {
						messagesByChannel.set(channelIdStr, []);
					}
					messagesByChannel.get(channelIdStr)!.push({channelId, messageId, message});
				}
			}

			lastChannelId = messageRefs[messageRefs.length - 1].channelId;
			lastMessageId = messageRefs[messageRefs.length - 1].messageId;
		}

		return messagesByChannel;
	}

	private async deleteMessagesInChannel(channelIdStr: string, messages: Array<MessageWithChannel>): Promise<number> {
		if (messages.length === 0) {
			return 0;
		}

		const channelId = createChannelID(BigInt(channelIdStr));
		const channel = await this.deps.channelRepository.findUnique(channelId);

		if (!channel) {
			Logger.debug({channelId: channelIdStr}, 'Channel not found, skipping messages');
			return 0;
		}

		let deleted = 0;
		const batches = chunkArray(messages, this.DELETE_BATCH_SIZE);

		for (const batch of batches) {
			const messageIds = batch.map((m: MessageWithChannel) => m.messageId);
			const messageObjects = batch.map((m: MessageWithChannel) => m.message);

			await Promise.all(
				messageObjects.map((message) =>
					purgeMessageAttachments(message, this.deps.storageService, this.deps.cloudflarePurgeQueue),
				),
			);

			await this.deps.channelRepository.bulkDeleteMessages(channelId, messageIds);

			await this.eventDispatcher.dispatchBulkDelete(channel, messageIds);

			deleted += batch.length;
		}

		return deleted;
	}
}
