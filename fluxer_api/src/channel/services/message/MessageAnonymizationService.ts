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
import {Logger} from '~/Logger';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';

export class MessageAnonymizationService {
	constructor(private channelRepository: IChannelRepositoryAggregate) {}

	async anonymizeMessagesByAuthor(originalAuthorId: UserID, newAuthorId: UserID): Promise<void> {
		const CHUNK_SIZE = 100;
		let lastChannelId: ChannelID | undefined;
		let lastMessageId: MessageID | undefined;
		let processedCount = 0;

		while (true) {
			const messagesToAnonymize = await this.channelRepository.messages.listMessagesByAuthor(
				originalAuthorId,
				CHUNK_SIZE,
				lastChannelId,
				lastMessageId,
			);

			if (messagesToAnonymize.length === 0) {
				break;
			}

			for (const {channelId, messageId} of messagesToAnonymize) {
				await this.channelRepository.messages.anonymizeMessage(channelId, messageId, newAuthorId);
			}

			processedCount += messagesToAnonymize.length;
			lastChannelId = messagesToAnonymize[messagesToAnonymize.length - 1].channelId;
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
