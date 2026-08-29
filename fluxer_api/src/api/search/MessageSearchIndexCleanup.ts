// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createMessageID, type MessageID} from '../BrandedTypes';
import {Logger} from '../Logger';
import {getMessageSearchService} from '../SearchFactory';
import type {IMessageSearchService} from './IMessageSearchService';

const MESSAGE_DELETE_BATCH_SIZE = 1000;

interface CleanupOptions {
	searchService?: IMessageSearchService | null;
	context?: Record<string, unknown>;
}

function resolveMessageSearchService(searchService?: IMessageSearchService | null): IMessageSearchService | null {
	return searchService !== undefined ? searchService : getMessageSearchService();
}

export async function deleteMessageSearchDocuments(
	messageIds: ReadonlyArray<MessageID>,
	options: CleanupOptions = {},
): Promise<void> {
	if (messageIds.length === 0) {
		return;
	}
	const searchService = resolveMessageSearchService(options.searchService);
	if (!searchService) {
		return;
	}
	const uniqueIds = Array.from(new Set(messageIds.map((id) => id.toString())));
	try {
		for (let i = 0; i < uniqueIds.length; i += MESSAGE_DELETE_BATCH_SIZE) {
			await searchService.deleteMessages(
				uniqueIds.slice(i, i + MESSAGE_DELETE_BATCH_SIZE).map((id) => createMessageID(BigInt(id))),
			);
		}
	} catch (error) {
		Logger.error(
			{...options.context, messageIds: uniqueIds, error},
			'Failed to delete message documents from search index',
		);
	}
}

export async function deleteChannelMessageSearchDocuments(
	channelId: ChannelID,
	options: CleanupOptions = {},
): Promise<void> {
	const searchService = resolveMessageSearchService(options.searchService);
	if (!searchService) {
		return;
	}
	try {
		await searchService.deleteChannelMessages(channelId);
	} catch (error) {
		Logger.error(
			{...options.context, channelId: channelId.toString(), error},
			'Failed to delete channel message documents from search index',
		);
	}
}
