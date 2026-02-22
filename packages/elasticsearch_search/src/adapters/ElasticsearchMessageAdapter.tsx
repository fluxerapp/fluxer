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

import type {Client} from '@elastic/elasticsearch';
import {ElasticsearchIndexAdapter} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {
	compactFilters,
	esAndTerms,
	esExcludeAny,
	esTermFilter,
	esTermsFilter,
} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

const DEFAULT_HITS_PER_PAGE = 25;
const FETCH_MULTIPLIER = 3;

const HAS_FIELD_MAP: Record<string, string> = {
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

function buildMessageFilters(filters: MessageSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];

	if (filters.guildId) {
		clauses.push(esTermFilter('guildId', filters.guildId));
	}

	if (filters.channelId) {
		clauses.push(esTermFilter('channelId', filters.channelId));
	}

	if (filters.channelIds && filters.channelIds.length > 0) {
		clauses.push(esTermsFilter('channelId', filters.channelIds));
	}

	if (filters.excludeChannelIds && filters.excludeChannelIds.length > 0) {
		clauses.push(...esExcludeAny('channelId', filters.excludeChannelIds));
	}

	if (filters.authorId && filters.authorId.length > 0) {
		clauses.push(esTermsFilter('authorId', filters.authorId));
	}

	if (filters.excludeAuthorIds && filters.excludeAuthorIds.length > 0) {
		clauses.push(...esExcludeAny('authorId', filters.excludeAuthorIds));
	}

	if (filters.authorType && filters.authorType.length > 0) {
		clauses.push(esTermsFilter('authorType', filters.authorType));
	}

	if (filters.excludeAuthorType && filters.excludeAuthorType.length > 0) {
		clauses.push(...esExcludeAny('authorType', filters.excludeAuthorType));
	}

	if (filters.mentions && filters.mentions.length > 0) {
		clauses.push(...esAndTerms('mentionedUserIds', filters.mentions));
	}

	if (filters.excludeMentions && filters.excludeMentions.length > 0) {
		clauses.push(...esExcludeAny('mentionedUserIds', filters.excludeMentions));
	}

	if (filters.mentionEveryone !== undefined) {
		clauses.push(esTermFilter('mentionEveryone', filters.mentionEveryone));
	}

	if (filters.pinned !== undefined) {
		clauses.push(esTermFilter('isPinned', filters.pinned));
	}

	if (filters.has && filters.has.length > 0) {
		for (const hasType of filters.has) {
			const field = HAS_FIELD_MAP[hasType];
			if (field) {
				clauses.push(esTermFilter(field, true));
			}
		}
	}

	if (filters.excludeHas && filters.excludeHas.length > 0) {
		for (const hasType of filters.excludeHas) {
			const field = HAS_FIELD_MAP[hasType];
			if (field) {
				clauses.push(esTermFilter(field, false));
			}
		}
	}

	if (filters.embedType && filters.embedType.length > 0) {
		clauses.push(...esAndTerms('embedTypes', filters.embedType));
	}

	if (filters.excludeEmbedTypes && filters.excludeEmbedTypes.length > 0) {
		clauses.push(...esExcludeAny('embedTypes', filters.excludeEmbedTypes));
	}

	if (filters.embedProvider && filters.embedProvider.length > 0) {
		clauses.push(...esAndTerms('embedProviders', filters.embedProvider));
	}

	if (filters.excludeEmbedProviders && filters.excludeEmbedProviders.length > 0) {
		clauses.push(...esExcludeAny('embedProviders', filters.excludeEmbedProviders));
	}

	if (filters.linkHostname && filters.linkHostname.length > 0) {
		clauses.push(...esAndTerms('linkHostnames', filters.linkHostname));
	}

	if (filters.excludeLinkHostnames && filters.excludeLinkHostnames.length > 0) {
		clauses.push(...esExcludeAny('linkHostnames', filters.excludeLinkHostnames));
	}

	if (filters.attachmentFilename && filters.attachmentFilename.length > 0) {
		clauses.push(...esAndTerms('attachmentFilenames', filters.attachmentFilename));
	}

	if (filters.excludeAttachmentFilenames && filters.excludeAttachmentFilenames.length > 0) {
		clauses.push(...esExcludeAny('attachmentFilenames', filters.excludeAttachmentFilenames));
	}

	if (filters.attachmentExtension && filters.attachmentExtension.length > 0) {
		clauses.push(...esAndTerms('attachmentExtensions', filters.attachmentExtension));
	}

	if (filters.excludeAttachmentExtensions && filters.excludeAttachmentExtensions.length > 0) {
		clauses.push(...esExcludeAny('attachmentExtensions', filters.excludeAttachmentExtensions));
	}

	return compactFilters(clauses);
}

function buildMessageSort(filters: MessageSearchFilters): Array<Record<string, unknown>> | undefined {
	const sortBy = filters.sortBy ?? 'timestamp';
	if (sortBy === 'relevance') {
		return undefined;
	}
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{createdAt: {order: sortOrder}}];
}

function getLimit(options?: SearchOptions): number {
	return options?.limit ?? options?.hitsPerPage ?? DEFAULT_HITS_PER_PAGE;
}

function getOffset(options?: SearchOptions): number {
	return options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? DEFAULT_HITS_PER_PAGE) : 0);
}

function applyMaxMinIdFilters(hits: Array<SearchableMessage>, filters: MessageSearchFilters): Array<SearchableMessage> {
	let filtered = hits;
	if (filters.maxId != null) {
		const maxId = BigInt(filters.maxId);
		filtered = filtered.filter((message) => BigInt(message.id) < maxId);
	}
	if (filters.minId != null) {
		const minId = BigInt(filters.minId);
		filtered = filtered.filter((message) => BigInt(message.id) > minId);
	}
	return filtered;
}

function applyExactPhraseFilter(hits: Array<SearchableMessage>, phrases: Array<string>): Array<SearchableMessage> {
	return hits.filter((hit) => {
		if (!hit.content) return false;
		return phrases.every((phrase) => hit.content!.includes(phrase));
	});
}

function applySortByIdTiebreaker(
	hits: Array<SearchableMessage>,
	filters: MessageSearchFilters,
): Array<SearchableMessage> {
	const sortBy = filters.sortBy ?? 'timestamp';
	if (sortBy === 'relevance') {
		return hits;
	}
	const sortOrder = filters.sortOrder ?? 'desc';
	return [...hits].sort((messageA, messageB) => {
		if (messageA.createdAt !== messageB.createdAt) {
			return sortOrder === 'asc' ? messageA.createdAt - messageB.createdAt : messageB.createdAt - messageA.createdAt;
		}
		const messageAId = BigInt(messageA.id);
		const messageBId = BigInt(messageB.id);
		if (sortOrder === 'asc') {
			return messageAId < messageBId ? -1 : messageAId > messageBId ? 1 : 0;
		}
		return messageBId < messageAId ? -1 : messageBId > messageAId ? 1 : 0;
	});
}

export interface ElasticsearchMessageAdapterOptions {
	client: Client;
}

export class ElasticsearchMessageAdapter extends ElasticsearchIndexAdapter<MessageSearchFilters, SearchableMessage> {
	constructor(options: ElasticsearchMessageAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.messages,
			searchableFields: ['content'],
			buildFilters: buildMessageFilters,
			buildSort: buildMessageSort,
		});
	}

	override async search(
		query: string,
		filters: MessageSearchFilters,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const limit = getLimit(options);
		const offset = getOffset(options);

		const fetchLimit = Math.max((limit + offset) * FETCH_MULTIPLIER, limit);

		const exactPhrases = filters.exactPhrases ?? [];
		const contents = filters.contents ?? [];

		if (contents.length > 0) {
			const resultMap = new Map<string, SearchableMessage>();
			const searchResults = await Promise.all(
				contents.map((term) =>
					super.search(
						term,
						{...filters, contents: undefined, exactPhrases: undefined},
						{...options, limit: fetchLimit, offset: 0},
					),
				),
			);
			for (const result of searchResults) {
				for (const hit of result.hits) {
					if (!resultMap.has(hit.id)) {
						resultMap.set(hit.id, hit);
					}
				}
			}

			let mergedHits = Array.from(resultMap.values());
			mergedHits = applyMaxMinIdFilters(mergedHits, filters);
			if (exactPhrases.length > 0) {
				mergedHits = applyExactPhraseFilter(mergedHits, exactPhrases);
			}
			const sorted = applySortByIdTiebreaker(mergedHits, filters);
			return {
				hits: sorted.slice(offset, offset + limit),
				total: mergedHits.length,
			};
		}

		if (exactPhrases.length > 0) {
			const result = await this.searchWithPhrases(query, exactPhrases, filters, {
				...options,
				limit: fetchLimit,
				offset: 0,
			});
			const filteredHits = applyMaxMinIdFilters(result.hits, filters);
			const sorted = applySortByIdTiebreaker(filteredHits, filters);
			return {
				hits: sorted.slice(offset, offset + limit),
				total: filteredHits.length,
			};
		}

		const result = await super.search(query, filters, {...options, limit: fetchLimit, offset: 0});
		const filtered = applyMaxMinIdFilters(result.hits, filters);
		const sorted = applySortByIdTiebreaker(filtered, filters);
		return {
			hits: sorted.slice(offset, offset + limit),
			total: filtered.length,
		};
	}

	private async searchWithPhrases(
		query: string,
		exactPhrases: Array<string>,
		filters: MessageSearchFilters,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const limit = options?.limit ?? DEFAULT_HITS_PER_PAGE;
		const offset = options?.offset ?? 0;

		const filterClauses = compactFilters(buildMessageFilters({...filters, exactPhrases: undefined}));
		const sort = buildMessageSort(filters);

		const must: Array<Record<string, unknown>> = [];

		if (query) {
			must.push({multi_match: {query, fields: ['content'], type: 'best_fields'}});
		}

		for (const phrase of exactPhrases) {
			must.push({match_phrase: {content: phrase}});
		}

		if (must.length === 0) {
			must.push({match_all: {}});
		}

		const searchParams: Record<string, unknown> = {
			index: 'messages',
			query: {
				bool: {
					must,
					filter: filterClauses.length > 0 ? filterClauses : undefined,
				},
			},
			from: offset,
			size: limit,
		};

		if (sort && sort.length > 0) {
			searchParams.sort = sort;
		}

		const result = await this.client.search<SearchableMessage>(searchParams);

		const totalValue = result.hits.total;
		const total = typeof totalValue === 'number' ? totalValue : (totalValue?.value ?? 0);
		const hits = result.hits.hits.map((hit) => ({...hit._source!, id: hit._id!}));

		return {hits, total};
	}
}
