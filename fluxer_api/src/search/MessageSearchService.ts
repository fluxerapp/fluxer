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

import type {Index, MeiliSearch, SearchResponse} from 'meilisearch';
import type {GuildID, MessageID, UserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import type {Message} from '~/Models';
import {SEARCH_MAX_TOTAL_HITS} from '~/search/constants';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

const MESSAGE_INDEX_NAME = 'messages';

interface SearchableMessage {
	id: string;
	channelId: string;
	authorId: string | null;
	authorType: 'user' | 'bot' | 'webhook';
	content: string | null;
	createdAt: number;
	editedAt: number | null;
	isPinned: boolean;
	mentionedUserIds: Array<string>;
	mentionEveryone: boolean;
	hasLink: boolean;
	hasEmbed: boolean;
	hasPoll: boolean;
	hasFile: boolean;
	hasVideo: boolean;
	hasImage: boolean;
	hasSound: boolean;
	hasSticker: boolean;
	hasForward: boolean;
	embedTypes: Array<string>;
	embedProviders: Array<string>;
	linkHostnames: Array<string>;
	attachmentFilenames: Array<string>;
	attachmentExtensions: Array<string>;
}

export interface MessageSearchFilters {
	maxId?: string;
	minId?: string;

	content?: string;
	contents?: Array<string>;

	channelId?: string;
	channelIds?: Array<string>;
	excludeChannelIds?: Array<string>;

	authorId?: Array<string>;
	authorType?: Array<string>;
	excludeAuthorType?: Array<string>;
	excludeAuthorIds?: Array<string>;

	mentions?: Array<string>;
	excludeMentions?: Array<string>;
	mentionEveryone?: boolean;

	pinned?: boolean;

	has?: Array<string>;
	excludeHas?: Array<string>;

	embedType?: Array<'image' | 'video' | 'sound' | 'article'>;
	excludeEmbedTypes?: Array<'image' | 'video' | 'sound' | 'article'>;
	embedProvider?: Array<string>;
	excludeEmbedProviders?: Array<string>;

	linkHostname?: Array<string>;
	excludeLinkHostnames?: Array<string>;

	attachmentFilename?: Array<string>;
	excludeAttachmentFilenames?: Array<string>;
	attachmentExtension?: Array<string>;
	excludeAttachmentExtensions?: Array<string>;

	sortBy?: 'timestamp' | 'relevance';
	sortOrder?: 'asc' | 'desc';

	includeNsfw?: boolean;
}

export class MessageSearchService {
	private meilisearch: MeiliSearch;
	private index: Index<SearchableMessage> | null = null;

	constructor(meilisearch: MeiliSearch) {
		this.meilisearch = meilisearch;
	}

	async initialize(): Promise<void> {
		try {
			this.index = this.meilisearch.index<SearchableMessage>(MESSAGE_INDEX_NAME);

			await this.index.updateSettings({
				searchableAttributes: ['content'],
				filterableAttributes: [
					'channelId',
					'authorId',
					'authorType',
					'isPinned',
					'mentionedUserIds',
					'mentionEveryone',
					'hasLink',
					'hasEmbed',
					'hasPoll',
					'hasFile',
					'hasVideo',
					'hasImage',
					'hasSound',
					'hasSticker',
					'hasForward',
					'createdAt',
					'embedTypes',
					'embedProviders',
					'linkHostnames',
					'attachmentFilenames',
					'attachmentExtensions',
				],
				sortableAttributes: ['createdAt', 'editedAt'],
				rankingRules: ['words', 'typo', 'proximity', 'attribute', 'sort', 'exactness'],
				pagination: {
					maxTotalHits: SEARCH_MAX_TOTAL_HITS,
				},
			});

			Logger.debug('Message search index initialized successfully');
		} catch (error) {
			Logger.error({error}, 'Failed to initialize message search index');
			throw error;
		}
	}

	async indexMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		const searchableMessage = this.convertToSearchableMessage(message, authorIsBot);

		try {
			await this.index.addDocuments([searchableMessage], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({messageId: message.id, error}, 'Failed to index message');
			throw error;
		}
	}

	async indexMessages(messages: Array<Message>, authorBotMap?: Map<UserID, boolean>): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		if (messages.length === 0) return;

		const searchableMessages = messages.map((msg) => {
			const isBot = msg.authorId ? (authorBotMap?.get(msg.authorId) ?? false) : false;
			return this.convertToSearchableMessage(msg, isBot);
		});

		try {
			await this.index.addDocuments(searchableMessages, {primaryKey: 'id'});
		} catch (error) {
			Logger.error({count: messages.length, error}, 'Failed to index messages');
			throw error;
		}
	}

	async updateMessage(message: Message, authorIsBot?: boolean): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		const searchableMessage = this.convertToSearchableMessage(message, authorIsBot);

		try {
			await this.index.updateDocuments([searchableMessage], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({messageId: message.id, error}, 'Failed to update message in search index');
			throw error;
		}
	}

	async deleteMessage(messageId: MessageID): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		try {
			await this.index.deleteDocument(messageId.toString());
		} catch (error) {
			Logger.error({messageId, error}, 'Failed to delete message from search index');
			throw error;
		}
	}

	async deleteMessages(messageIds: Array<MessageID>): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		if (messageIds.length === 0) return;

		try {
			await this.index.deleteDocuments(messageIds.map((id) => id.toString()));
		} catch (error) {
			Logger.error({count: messageIds.length, error}, 'Failed to delete messages from search index');
			throw error;
		}
	}

	async deleteGuildMessages(guildId: GuildID): Promise<void> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		try {
			await this.index.deleteDocuments({
				filter: [`channelId IN ${guildId.toString()}`],
			});
		} catch (error) {
			Logger.error({guildId, error}, 'Failed to delete guild messages from search index');
			throw error;
		}
	}

	async searchMessages(
		query: string,
		filters: MessageSearchFilters,
		options?: {
			hitsPerPage?: number;
			page?: number;
		},
	): Promise<{hits: Array<SearchableMessage>; total: number}> {
		if (!this.index) {
			throw new Error('Message search index not initialized');
		}

		const searchQuery = this.buildSearchQuery(query, filters);
		const filterStrings = this.buildFilterStrings(filters);
		const sortField = this.buildSortField(filters);

		try {
			const rawResult = (await this.index.search<SearchableMessage>(searchQuery, {
				filter: filterStrings.length > 0 ? filterStrings : undefined,
				hitsPerPage: options?.hitsPerPage ?? 25,
				page: options?.page ?? 1,
				sort: sortField,
			})) as SearchResponse<SearchableMessage>;

			const totalHits = rawResult.totalHits ?? rawResult.estimatedTotalHits ?? rawResult.hits.length;

			return {
				hits: rawResult.hits,
				total: totalHits,
			};
		} catch (error) {
			Logger.error({query: searchQuery, filters, error}, 'Failed to search messages');
			throw error;
		}
	}

	private buildSearchQuery(query: string, filters: MessageSearchFilters): string {
		if (filters.contents && filters.contents.length > 0) {
			const contentTerms = filters.contents.join(' ');
			return query ? `${query} ${contentTerms}` : contentTerms;
		}

		if (filters.content) {
			return query ? `${query} ${filters.content}` : filters.content;
		}

		return query;
	}

	private buildFilterStrings(filters: MessageSearchFilters): Array<string> {
		const filterStrings: Array<string> = [];

		if (filters.channelId) {
			filterStrings.push(`channelId = "${filters.channelId}"`);
		}
		if (filters.channelIds && filters.channelIds.length > 0) {
			this.addOrFilter(filterStrings, 'channelId', filters.channelIds);
		}
		if (filters.excludeChannelIds && filters.excludeChannelIds.length > 0) {
			this.addExcludeFilter(filterStrings, 'channelId', filters.excludeChannelIds);
		}

		if (filters.authorId && filters.authorId.length > 0) {
			this.addOrFilter(filterStrings, 'authorId', filters.authorId);
		}
		if (filters.excludeAuthorIds && filters.excludeAuthorIds.length > 0) {
			this.addExcludeFilter(filterStrings, 'authorId', filters.excludeAuthorIds);
		}
		if (filters.authorType && filters.authorType.length > 0) {
			this.addAuthorTypeFilter(filterStrings, filters.authorType);
		}
		if (filters.excludeAuthorType && filters.excludeAuthorType.length > 0) {
			this.addExcludeFilter(filterStrings, 'authorType', filters.excludeAuthorType);
		}

		if (filters.mentions && filters.mentions.length > 0) {
			this.addOrFilter(filterStrings, 'mentionedUserIds', filters.mentions);
		}
		if (filters.excludeMentions && filters.excludeMentions.length > 0) {
			this.addExcludeFilter(filterStrings, 'mentionedUserIds', filters.excludeMentions);
		}
		if (filters.mentionEveryone !== undefined) {
			filterStrings.push(`mentionEveryone = ${filters.mentionEveryone}`);
		}

		if (filters.pinned !== undefined) {
			filterStrings.push(`isPinned = ${filters.pinned}`);
		}

		if (filters.has && filters.has.length > 0) {
			this.addHasFilter(filterStrings, filters.has);
		}
		if (filters.excludeHas && filters.excludeHas.length > 0) {
			this.addHasExcludeFilter(filterStrings, filters.excludeHas);
		}

		if (filters.embedType && filters.embedType.length > 0) {
			this.addOrFilter(filterStrings, 'embedTypes', filters.embedType);
		}
		if (filters.excludeEmbedTypes && filters.excludeEmbedTypes.length > 0) {
			this.addExcludeFilter(filterStrings, 'embedTypes', filters.excludeEmbedTypes);
		}
		if (filters.embedProvider && filters.embedProvider.length > 0) {
			this.addOrFilter(filterStrings, 'embedProviders', filters.embedProvider);
		}
		if (filters.excludeEmbedProviders && filters.excludeEmbedProviders.length > 0) {
			this.addExcludeFilter(filterStrings, 'embedProviders', filters.excludeEmbedProviders);
		}

		if (filters.linkHostname && filters.linkHostname.length > 0) {
			this.addOrFilter(filterStrings, 'linkHostnames', filters.linkHostname);
		}
		if (filters.excludeLinkHostnames && filters.excludeLinkHostnames.length > 0) {
			this.addExcludeFilter(filterStrings, 'linkHostnames', filters.excludeLinkHostnames);
		}

		if (filters.attachmentFilename && filters.attachmentFilename.length > 0) {
			this.addOrFilter(filterStrings, 'attachmentFilenames', filters.attachmentFilename);
		}
		if (filters.excludeAttachmentFilenames && filters.excludeAttachmentFilenames.length > 0) {
			this.addExcludeFilter(filterStrings, 'attachmentFilenames', filters.excludeAttachmentFilenames);
		}
		if (filters.attachmentExtension && filters.attachmentExtension.length > 0) {
			this.addOrFilter(filterStrings, 'attachmentExtensions', filters.attachmentExtension);
		}
		if (filters.excludeAttachmentExtensions && filters.excludeAttachmentExtensions.length > 0) {
			this.addExcludeFilter(filterStrings, 'attachmentExtensions', filters.excludeAttachmentExtensions);
		}

		if (filters.maxId) {
			const timestamp = Math.floor(extractTimestamp(BigInt(filters.maxId)) / 1000);
			filterStrings.push(`createdAt < ${timestamp}`);
		}
		if (filters.minId) {
			const timestamp = Math.floor(extractTimestamp(BigInt(filters.minId)) / 1000);
			filterStrings.push(`createdAt > ${timestamp}`);
		}

		return filterStrings;
	}

	private buildSortField(filters: MessageSearchFilters): Array<string> {
		const sortBy = filters.sortBy ?? 'timestamp';
		const sortOrder = filters.sortOrder ?? 'desc';
		return sortBy === 'relevance' ? [] : [`createdAt:${sortOrder}`];
	}

	private addOrFilter(filterStrings: Array<string>, field: string, values: Array<string>): void {
		const orFilters = values.map((value) => `${field} = "${value}"`).join(' OR ');
		filterStrings.push(`(${orFilters})`);
	}

	private addExcludeFilter(filterStrings: Array<string>, field: string, values: Array<string>): void {
		const notFilters = values.map((value) => `${field} != "${value}"`).join(' AND ');
		filterStrings.push(`(${notFilters})`);
	}

	private addAuthorTypeFilter(filterStrings: Array<string>, authorTypes: Array<string>): void {
		this.addOrFilter(filterStrings, 'authorType', authorTypes);
	}

	private addHasFilter(filterStrings: Array<string>, hasTypes: Array<string>): void {
		for (const hasType of hasTypes) {
			const field = this.getHasField(hasType);
			if (field) {
				filterStrings.push(`${field} = true`);
			}
		}
	}

	private addHasExcludeFilter(filterStrings: Array<string>, hasTypes: Array<string>): void {
		for (const hasType of hasTypes) {
			const field = this.getHasField(hasType);
			if (field) {
				filterStrings.push(`${field} = false`);
			}
		}
	}

	private getHasField(hasType: string): string | null {
		const mapping: Record<string, string> = {
			image: 'hasImage',
			sound: 'hasSound',
			video: 'hasVideo',
			file: 'hasFile',
			sticker: 'hasSticker',
			embed: 'hasEmbed',
			link: 'hasLink',
			poll: 'hasPoll',
			snapshot: 'hasForward',
		};
		return mapping[hasType] ?? null;
	}

	private getAuthorType(message: Message, authorIsBot?: boolean): 'user' | 'bot' | 'webhook' {
		if (message.webhookId) return 'webhook';
		if (authorIsBot) return 'bot';
		return 'user';
	}

	private extractAttachmentTypes(message: Message): {hasVideo: boolean; hasImage: boolean; hasSound: boolean} {
		const hasType = (prefix: string) =>
			message.attachments.some((att) => att.contentType.trim().toLowerCase().startsWith(prefix));

		return {
			hasVideo: hasType('video/'),
			hasImage: hasType('image/'),
			hasSound: hasType('audio/'),
		};
	}

	private extractEmbedTypes(message: Message): Array<string> {
		const types: Array<string> = [];
		for (const embed of message.embeds) {
			if (embed.type && !types.includes(embed.type)) {
				types.push(embed.type);
			}
		}
		return types;
	}

	private extractEmbedProviders(message: Message): Array<string> {
		const providers: Array<string> = [];
		for (const embed of message.embeds) {
			if (embed.provider?.name && !providers.includes(embed.provider.name)) {
				providers.push(embed.provider.name);
			}
		}
		return providers;
	}

	private extractLinkHostnames(message: Message): Array<string> {
		const hostnames: Array<string> = [];

		if (message.content) {
			const urlMatches = message.content.matchAll(/https?:\/\/([^/\s]+)/g);
			for (const match of urlMatches) {
				const hostname = match[1];
				if (hostname && !hostnames.includes(hostname)) {
					hostnames.push(hostname);
				}
			}
		}

		for (const embed of message.embeds) {
			if (embed.url) {
				try {
					const url = new URL(embed.url);
					if (!hostnames.includes(url.hostname)) {
						hostnames.push(url.hostname);
					}
				} catch {}
			}
		}

		return hostnames;
	}

	private extractAttachmentInfo(message: Message): {
		attachmentFilenames: Array<string>;
		attachmentExtensions: Array<string>;
	} {
		const filenames: Array<string> = [];
		const extensions: Array<string> = [];

		for (const att of message.attachments) {
			if (!filenames.includes(att.filename)) {
				filenames.push(att.filename);
			}

			const parts = att.filename.split('.');
			if (parts.length > 1) {
				const ext = parts[parts.length - 1]!.toLowerCase();
				if (ext.length > 0 && ext.length <= 10 && !extensions.includes(ext)) {
					extensions.push(ext);
				}
			}
		}

		return {attachmentFilenames: filenames, attachmentExtensions: extensions};
	}

	private convertToSearchableMessage(message: Message, authorIsBot?: boolean): SearchableMessage {
		const createdAt = Math.floor(extractTimestamp(BigInt(message.id)) / 1000);
		const editedAt = message.editedTimestamp ? Math.floor(message.editedTimestamp.getTime() / 1000) : null;

		const authorType = this.getAuthorType(message, authorIsBot);
		const {hasVideo, hasImage, hasSound} = this.extractAttachmentTypes(message);
		const hasLink = message.content !== null && /(https?:\/\/[^\s]+)/.test(message.content);
		const embedTypes = this.extractEmbedTypes(message);
		const embedProviders = this.extractEmbedProviders(message);
		const linkHostnames = this.extractLinkHostnames(message);
		const {attachmentFilenames, attachmentExtensions} = this.extractAttachmentInfo(message);

		return {
			id: message.id.toString(),
			channelId: message.channelId.toString(),
			authorId: message.authorId?.toString() ?? null,
			authorType,
			content: message.content,
			createdAt,
			editedAt,
			isPinned: message.pinnedTimestamp !== null,
			mentionedUserIds: Array.from(message.mentionedUserIds).map((id) => id.toString()),
			mentionEveryone: message.mentionEveryone,
			hasLink,
			hasEmbed: message.embeds.length > 0,
			hasPoll: false, // TODO: Implement when poll support is added
			hasFile: message.attachments.length > 0,
			hasVideo,
			hasImage,
			hasSound,
			hasSticker: message.stickers.length > 0,
			hasForward: message.reference?.type === 1,
			embedTypes,
			embedProviders,
			linkHostnames,
			attachmentFilenames,
			attachmentExtensions,
		};
	}
}
