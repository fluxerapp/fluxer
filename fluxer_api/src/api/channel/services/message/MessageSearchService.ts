// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {Message} from '@app/api/models/Message';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {deleteMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

export function isMessageSearchIndexable(message: Message): boolean {
	return message.type !== MessageTypes.THREAD_CREATED && message.type !== MessageTypes.THREAD_STARTER_MESSAGE;
}

export class MessageSearchService {
	constructor(
		private userRepository: UserRepository,
		private workerService: IWorkerService<WorkerTaskName>,
	) {}

	async indexMessage(message: Message, authorIsBot: boolean): Promise<void> {
		if (!isMessageSearchIndexable(message)) return;
		try {
			const searchService = getMessageSearchService();
			if (!searchService) {
				return;
			}
			await searchService.indexMessage(message, authorIsBot);
		} catch (error) {
			Logger.error(
				{
					messageId: message.id,
					channelId: message.channelId,
					authorId: message.authorId,
					authorIsBot,
					error,
				},
				'Failed to index message in search',
			);
		}
	}

	async updateMessageIndex(message: Message): Promise<void> {
		if (!isMessageSearchIndexable(message)) return;
		try {
			const searchService = getMessageSearchService();
			if (!searchService) {
				return;
			}
			let authorIsBot = false;
			if (message.authorId != null) {
				const user = await this.userRepository.findUnique(message.authorId);
				authorIsBot = user?.isBot ?? false;
			}
			await searchService.updateMessage(message, authorIsBot);
		} catch (error) {
			Logger.error(
				{
					messageId: message.id,
					channelId: message.channelId,
					authorId: message.authorId,
					error,
				},
				'Failed to update message in search index',
			);
		}
	}

	async deleteMessageIndex(messageId: MessageID): Promise<void> {
		await this.deleteMessagesIndex([messageId]);
	}

	async deleteMessagesIndex(messageIds: Array<MessageID>): Promise<void> {
		await deleteMessageSearchDocuments(messageIds);
	}

	async triggerChannelIndexing(channelId: ChannelID): Promise<void> {
		await this.workerService.addJob('indexChannelMessages', {
			channelId: channelId.toString(),
		});
	}
}
