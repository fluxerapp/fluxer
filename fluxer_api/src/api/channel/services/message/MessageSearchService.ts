// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {Message} from '@app/api/models/Message';
import {getMessageSearchService} from '@app/api/SearchFactory';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import {deleteMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

interface MessageSearchIndexOptions {
	includeDefault?: boolean;
}

function getMessageIndexServices(options: MessageSearchIndexOptions = {}): Array<IMessageSearchService> {
	const services: Array<IMessageSearchService> = [];
	const includeDefault = options.includeDefault ?? true;
	const defaultService = getMessageSearchService();
	if (includeDefault && defaultService) {
		services.push(defaultService);
	}
	return services;
}

export function isMessageSearchIndexable(message: Message): boolean {
	return message.type !== MessageTypes.THREAD_CREATED && message.type !== MessageTypes.THREAD_STARTER_MESSAGE;
}

export class MessageSearchService {
	constructor(
		private userRepository: UserRepository,
		private workerService: IWorkerService<WorkerTaskName>,
	) {}

	async indexMessage(message: Message, authorIsBot: boolean, options?: MessageSearchIndexOptions): Promise<void> {
		if (!isMessageSearchIndexable(message)) return;
		try {
			const searchServices = getMessageIndexServices(options);
			if (searchServices.length === 0) {
				return;
			}
			await Promise.all(searchServices.map((searchService) => searchService.indexMessage(message, authorIsBot)));
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

	async updateMessageIndex(message: Message, options?: MessageSearchIndexOptions): Promise<void> {
		if (!isMessageSearchIndexable(message)) return;
		try {
			const searchServices = getMessageIndexServices(options);
			if (searchServices.length === 0) {
				return;
			}
			let authorIsBot = false;
			if (message.authorId != null) {
				const user = await this.userRepository.findUnique(message.authorId);
				authorIsBot = user?.isBot ?? false;
			}
			await Promise.all(searchServices.map((searchService) => searchService.updateMessage(message, authorIsBot)));
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

	async deleteMessageIndex(messageId: MessageID, options?: MessageSearchIndexOptions): Promise<void> {
		await this.deleteMessagesIndex([messageId], options);
	}

	async deleteMessagesIndex(messageIds: Array<MessageID>, options?: MessageSearchIndexOptions): Promise<void> {
		await Promise.all(
			getMessageIndexServices(options).map((searchService) =>
				deleteMessageSearchDocuments(messageIds, {searchService}),
			),
		);
	}

	async triggerChannelIndexing(channelId: ChannelID): Promise<void> {
		await this.workerService.addJob('indexChannelMessages', {
			channelId: channelId.toString(),
		});
	}
}
