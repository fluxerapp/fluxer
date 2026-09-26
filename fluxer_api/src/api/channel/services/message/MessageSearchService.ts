// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import type {Message} from '@app/api/models/Message';
import {getMessageSearchService} from '@app/api/SearchFactory';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import {deleteMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {MessageSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
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

export class MessageSearchService {
	constructor(
		private userRepository: IUserRepository,
		private workerService: IWorkerService<WorkerTaskName>,
	) {}

	async indexMessage(message: Message, authorIsBot: boolean, options?: MessageSearchIndexOptions): Promise<void> {
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
		if (searchParams.author_id != null) {
			filters.authorId = Array.isArray(searchParams.author_id)
				? searchParams.author_id.map((id: bigint) => id.toString())
				: [(searchParams.author_id as bigint).toString()];
		}
		if (searchParams.exclude_author_id != null) {
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
		if (searchParams.embed_type)
			filters.embedType = searchParams.embed_type as Array<'image' | 'video' | 'sound' | 'article'>;
		if (searchParams.exclude_embed_type)
			filters.excludeEmbedTypes = searchParams.exclude_embed_type as Array<'image' | 'video' | 'sound' | 'article'>;
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
		if (searchParams.sort_by) filters.sortBy = searchParams.sort_by as 'timestamp' | 'relevance';
		if (searchParams.sort_order) filters.sortOrder = searchParams.sort_order as 'asc' | 'desc';
		if (searchParams.include_nsfw !== undefined) filters.includeNsfw = searchParams.include_nsfw;
		return filters;
	}

	async triggerChannelIndexing(channelId: ChannelID): Promise<void> {
		await this.workerService.addJob('indexChannelMessages', {
			channelId: channelId.toString(),
		});
	}
}
