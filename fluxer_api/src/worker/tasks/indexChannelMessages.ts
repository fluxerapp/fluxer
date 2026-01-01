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

import type {Task} from 'graphile-worker';
import type {UserID} from '~/BrandedTypes';
import {createChannelID, createMessageID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {getMessageSearchService} from '~/Meilisearch';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface IndexChannelMessagesPayload {
	channelId: string;
	lastMessageId?: string | null;
}

const payloadSchema = {
	channelId: CommonFields.channelId(),
	lastMessageId: CommonFields.messageId('optional'),
};

const BATCH_SIZE = 100;

const indexChannelMessages: Task = async (payload, helpers) => {
	const validated = validatePayload<IndexChannelMessagesPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing indexChannelMessages task', {payload: validated});

	const searchService = getMessageSearchService();
	if (!searchService) {
		Logger.info('Search is disabled, skipping channel indexing');
		return;
	}

	const channelId = createChannelID(BigInt(validated.channelId));
	const {channelRepository, userRepository} = getWorkerDependencies();

	const channel = await channelRepository.findUnique(channelId);
	if (!channel) {
		Logger.warn({channelId: channelId.toString()}, 'Channel not found, skipping indexing');
		return;
	}

	try {
		const lastMessageId = validated.lastMessageId ? createMessageID(BigInt(validated.lastMessageId)) : undefined;
		const messages = await channelRepository.listMessages(channelId, lastMessageId, BATCH_SIZE);

		if (messages.length === 0) {
			Logger.debug({channelId: channelId.toString()}, 'Channel indexing complete');
			await channelRepository.upsert({
				...channel.toRow(),
				indexed_at: new Date(),
			});
			return;
		}

		const authorIds = new Set(messages.map((m) => m.authorId).filter((id): id is UserID => id !== null));
		const authorBotMap = new Map<UserID, boolean>();

		for (const authorId of authorIds) {
			const user = await userRepository.findUnique(authorId);
			if (user) {
				authorBotMap.set(authorId, user.isBot);
			}
		}

		await searchService.indexMessages(messages, authorBotMap);

		Logger.debug(
			{
				channelId: channelId.toString(),
				messagesIndexed: messages.length,
				hasMore: messages.length === BATCH_SIZE,
			},
			'Indexed message batch',
		);

		if (messages.length === BATCH_SIZE) {
			const oldestMessageId = messages[messages.length - 1].id;
			await helpers.addJob(
				'indexChannelMessages',
				{
					channelId: validated.channelId,
					lastMessageId: oldestMessageId.toString(),
				},
				{
					jobKey: `index-channel-${validated.channelId}-${oldestMessageId}`,
					maxAttempts: 3,
				},
			);
		} else {
			Logger.debug({channelId: channelId.toString()}, 'Channel indexing complete');
			const latestChannel = await channelRepository.findUnique(channelId);
			if (latestChannel) {
				await channelRepository.upsert({
					...latestChannel.toRow(),
					indexed_at: new Date(),
				});
			}
		}
	} catch (error) {
		Logger.error({error, channelId: channelId.toString()}, 'Failed to index channel messages');
		throw error;
	}
};

export default indexChannelMessages;
