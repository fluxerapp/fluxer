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

import type {ChannelID, MessageID} from '~/BrandedTypes';
import type {MessageSearchRequest} from '~/channel/ChannelModel';
import {getMessageSearchService} from '~/Meilisearch';
import type {Message} from '~/Models';
import type {MessageSearchFilters} from '~/search/MessageSearchService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IWorkerService} from '~/worker/IWorkerService';

export class MessageSearchService {
	constructor(
		private userRepository: IUserRepository,
		private workerService: IWorkerService,
	) {}

	async indexMessage(message: Message, authorIsBot: boolean): Promise<void> {
		try {
			const searchService = getMessageSearchService();
			if (!searchService) return;

			await searchService.indexMessage(message, authorIsBot);
		} catch (error) {
			console.error('Failed to index message:', error);
		}
	}

	async updateMessageIndex(message: Message): Promise<void> {
		try {
			const searchService = getMessageSearchService();
			if (!searchService) return;

			let authorIsBot = false;
			if (message.authorId) {
				const user = await this.userRepository.findUnique(message.authorId);
				authorIsBot = user?.isBot ?? false;
			}

			await searchService.updateMessage(message, authorIsBot);
		} catch (error) {
			console.error('Failed to update message in search index:', error);
		}
	}

	async deleteMessageIndex(messageId: MessageID): Promise<void> {
		try {
			const searchService = getMessageSearchService();
			if (!searchService) return;

			await searchService.deleteMessage(messageId);
		} catch (error) {
			console.error('Failed to delete message from search index:', error);
		}
	}

	buildSearchFilters(channelId: ChannelID, searchParams: MessageSearchRequest): MessageSearchFilters {
		const filters: MessageSearchFilters = {};

		if (searchParams.max_id) filters.maxId = searchParams.max_id.toString();
		if (searchParams.min_id) filters.minId = searchParams.min_id.toString();

		if (searchParams.content) filters.content = searchParams.content;
		if (searchParams.contents) filters.contents = searchParams.contents;

		if (searchParams.channel_id) {
			filters.channelIds = Array.isArray(searchParams.channel_id)
				? searchParams.channel_id.map((id: bigint) => id.toString())
				: [(searchParams.channel_id as bigint).toString()];
		} else {
			filters.channelId = channelId.toString();
		}
		if (searchParams.exclude_channel_id) {
			filters.excludeChannelIds = Array.isArray(searchParams.exclude_channel_id)
				? searchParams.exclude_channel_id.map((id: bigint) => id.toString())
				: [(searchParams.exclude_channel_id as bigint).toString()];
		}

		if (searchParams.author_id) {
			filters.authorId = Array.isArray(searchParams.author_id)
				? searchParams.author_id.map((id: bigint) => id.toString())
				: [(searchParams.author_id as bigint).toString()];
		}
		if (searchParams.exclude_author_id) {
			filters.excludeAuthorIds = Array.isArray(searchParams.exclude_author_id)
				? searchParams.exclude_author_id.map((id: bigint) => id.toString())
				: [(searchParams.exclude_author_id as bigint).toString()];
		}
		if (searchParams.author_type) filters.authorType = searchParams.author_type;
		if (searchParams.exclude_author_type) filters.excludeAuthorType = searchParams.exclude_author_type;

		if (searchParams.mentions) filters.mentions = searchParams.mentions.map((id: bigint) => id.toString());
		if (searchParams.exclude_mentions)
			filters.excludeMentions = searchParams.exclude_mentions.map((id: bigint) => id.toString());
		if (searchParams.mention_everyone !== undefined) filters.mentionEveryone = searchParams.mention_everyone;

		if (searchParams.pinned !== undefined) filters.pinned = searchParams.pinned;

		if (searchParams.has) filters.has = searchParams.has;
		if (searchParams.exclude_has) filters.excludeHas = searchParams.exclude_has;

		if (searchParams.embed_type) filters.embedType = searchParams.embed_type;
		if (searchParams.exclude_embed_type) filters.excludeEmbedTypes = searchParams.exclude_embed_type;
		if (searchParams.embed_provider) filters.embedProvider = searchParams.embed_provider;
		if (searchParams.exclude_embed_provider) filters.excludeEmbedProviders = searchParams.exclude_embed_provider;

		if (searchParams.link_hostname) filters.linkHostname = searchParams.link_hostname;
		if (searchParams.exclude_link_hostname) filters.excludeLinkHostnames = searchParams.exclude_link_hostname;

		if (searchParams.attachment_filename) filters.attachmentFilename = searchParams.attachment_filename;
		if (searchParams.exclude_attachment_filename)
			filters.excludeAttachmentFilenames = searchParams.exclude_attachment_filename;
		if (searchParams.attachment_extension) filters.attachmentExtension = searchParams.attachment_extension;
		if (searchParams.exclude_attachment_extension)
			filters.excludeAttachmentExtensions = searchParams.exclude_attachment_extension;

		if (searchParams.sort_by) filters.sortBy = searchParams.sort_by;
		if (searchParams.sort_order) filters.sortOrder = searchParams.sort_order;

		if (searchParams.include_nsfw !== undefined) filters.includeNsfw = searchParams.include_nsfw;

		return filters;
	}

	async triggerChannelIndexing(channelId: ChannelID): Promise<void> {
		await this.workerService.addJob('indexChannelMessages', {
			channelId: channelId.toString(),
		});
	}
}
