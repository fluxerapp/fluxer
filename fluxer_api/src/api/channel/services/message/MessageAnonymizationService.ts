// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID, UserID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {Logger} from '@app/api/Logger';

export class MessageAnonymizationService {
	constructor(private channelRepository: IChannelRepositoryAggregate) {}

	async anonymizeMessagesByAuthor(originalAuthorId: UserID, newAuthorId: UserID): Promise<void> {
		const CHUNK_SIZE = 100;
		let lastMessageId: MessageID | undefined;
		let processedCount = 0;
		while (true) {
			const messagesToAnonymize = await this.channelRepository.messages.listMessagesByAuthor(
				originalAuthorId,
				CHUNK_SIZE,
				lastMessageId,
			);
			if (messagesToAnonymize.length === 0) {
				break;
			}
			for (const {channelId, messageId} of messagesToAnonymize) {
				await this.channelRepository.messages.anonymizeMessage(channelId, messageId, newAuthorId);
			}
			processedCount += messagesToAnonymize.length;
			lastMessageId = messagesToAnonymize[messagesToAnonymize.length - 1].messageId;
			Logger.debug(
				{originalAuthorId, processedCount, chunkSize: messagesToAnonymize.length},
				'Anonymized message chunk',
			);
			if (messagesToAnonymize.length < CHUNK_SIZE) {
				break;
			}
		}
		Logger.debug({originalAuthorId, newAuthorId, totalProcessed: processedCount}, 'Completed message anonymization');
	}
}
