// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {SearchOptions, SearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {MessageSearchFilters, SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {
	compactFilters,
	esAndTerms,
	esExcludeAny,
	esTermFilter,
	esTermsFilter,
} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

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
	clauses.push(...esExcludeAny('channelId', filters.excludeChannelIds ?? []));
	if (filters.authorId && filters.authorId.length > 0) {
		clauses.push(esTermsFilter('authorId', filters.authorId));
	}
	clauses.push(...esExcludeAny('authorId', filters.excludeAuthorIds ?? []));
	if (filters.authorType && filters.authorType.length > 0) {
		clauses.push(esTermsFilter('authorType', filters.authorType));
	}
	clauses.push(...esExcludeAny('authorType', filters.excludeAuthorType ?? []));
	clauses.push(...esAndTerms('mentionedUserIds', filters.mentions ?? []));
	clauses.push(...esExcludeAny('mentionedUserIds', filters.excludeMentions ?? []));
	if (filters.mentionEveryone !== undefined) {
		clauses.push(esTermFilter('mentionEveryone', filters.mentionEveryone));
	}
	if (filters.pinned !== undefined) {
		clauses.push(esTermFilter('isPinned', filters.pinned));
	}
	for (const hasType of filters.has ?? []) {
		const field = HAS_FIELD_MAP[hasType];
		if (field) {
			clauses.push(esTermFilter(field, true));
		}
	}
	for (const hasType of filters.excludeHas ?? []) {
		const field = HAS_FIELD_MAP[hasType];
		if (field) {
			clauses.push(esTermFilter(field, false));
		}
	}
	clauses.push(...esAndTerms('embedTypes', filters.embedType ?? []));
	clauses.push(...esExcludeAny('embedTypes', filters.excludeEmbedTypes ?? []));
	clauses.push(...esAndTerms('embedProviders', filters.embedProvider ?? []));
	clauses.push(...esExcludeAny('embedProviders', filters.excludeEmbedProviders ?? []));
	clauses.push(...esAndTerms('linkHostnames', filters.linkHostname ?? []));
	clauses.push(...esExcludeAny('linkHostnames', filters.excludeLinkHostnames ?? []));
	clauses.push(...esAndTerms('attachmentFilenames', filters.attachmentFilename ?? []));
	clauses.push(...esExcludeAny('attachmentFilenames', filters.excludeAttachmentFilenames ?? []));
	clauses.push(...esAndTerms('attachmentExtensions', filters.attachmentExtension ?? []));
	clauses.push(...esExcludeAny('attachmentExtensions', filters.excludeAttachmentExtensions ?? []));
	if (filters.maxId != null) {
		clauses.push({range: {id: {lt: filters.maxId}}});
	}
	if (filters.minId != null) {
		clauses.push({range: {id: {gt: filters.minId}}});
	}
	return compactFilters(clauses);
}

function buildMessageSort(filters: MessageSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'timestamp';
	if (sortBy === 'relevance') {
		return undefined;
	}
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{createdAt: {order: sortOrder}}];
}

export interface ElasticsearchMessageAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchMessageAdapter extends ElasticsearchIndexAdapter<MessageSearchFilters, SearchableMessage> {
	constructor(options: ElasticsearchMessageAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.messages,
			searchableFields: ['content', 'embedContent'],
			operator: 'and',
			buildFilters: buildMessageFilters,
			buildSort: buildMessageSort,
			lock: options.lock,
		});
	}

	override async search(
		query: string,
		filters: MessageSearchFilters,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const exactPhrases = filters.exactPhrases ?? [];
		const contents = filters.contents ?? [];
		if (contents.length > 0) {
			return this.searchMultipleContents(contents, exactPhrases, filters, options);
		}
		if (exactPhrases.length > 0) {
			return this.searchWithPhrases(query, exactPhrases, filters, options);
		}
		return super.search(query, filters, options);
	}

	private async searchMultipleContents(
		contents: Array<string>,
		exactPhrases: Array<string>,
		originalFilters: MessageSearchFilters,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const filterClauses = compactFilters(
			buildMessageFilters({...originalFilters, contents: undefined, exactPhrases: undefined}),
		);
		const must: Array<Record<string, unknown>> = contents.map((term) => ({
			multi_match: {query: term, fields: ['content', 'embedContent'], type: 'best_fields', operator: 'and'},
		}));
		for (const phrase of exactPhrases) {
			must.push({
				bool: {
					should: [{match_phrase: {content: phrase}}, {match_phrase: {embedContent: phrase}}],
					minimum_should_match: 1,
				},
			});
		}
		return this.runBoolSearch(must, filterClauses, buildMessageSort(originalFilters), options);
	}

	private async searchWithPhrases(
		query: string,
		exactPhrases: Array<string>,
		originalFilters: MessageSearchFilters,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const filterClauses = compactFilters(buildMessageFilters({...originalFilters, exactPhrases: undefined}));
		const must: Array<Record<string, unknown>> = [];
		if (query) {
			must.push({multi_match: {query, fields: ['content', 'embedContent'], type: 'best_fields', operator: 'and'}});
		}
		for (const phrase of exactPhrases) {
			must.push({
				bool: {
					should: [{match_phrase: {content: phrase}}, {match_phrase: {embedContent: phrase}}],
					minimum_should_match: 1,
				},
			});
		}
		if (must.length === 0) {
			must.push({match_all: {}});
		}
		return this.runBoolSearch(must, filterClauses, buildMessageSort(originalFilters), options);
	}

	private async runBoolSearch(
		must: Array<Record<string, unknown>>,
		filterClauses: Array<ElasticsearchFilter>,
		sort: Array<SortCombinations> | undefined,
		options?: SearchOptions,
	): Promise<SearchResult<SearchableMessage>> {
		const limit = options?.limit ?? options?.hitsPerPage ?? 25;
		const usesCursor = options?.cursor != null && options.cursor.length > 0;
		const searchParams: Record<string, unknown> = {
			index: this.indexDefinition.indexName,
			query: {
				bool: {
					must,
					filter: filterClauses.length > 0 ? filterClauses : undefined,
				},
			},
			size: limit,
			track_total_hits: true,
		};
		if (usesCursor) {
			searchParams.search_after = options!.cursor;
		} else {
			const offset = options?.offset ?? (options?.page ? (options.page - 1) * (options.hitsPerPage ?? 25) : 0);
			searchParams.from = offset;
		}
		const effectiveSort = sort && sort.length > 0 ? [...sort, {id: {order: 'desc'}}] : [{id: {order: 'desc'}}];
		searchParams.sort = effectiveSort;
		const result = await this.client.search<SearchableMessage>(searchParams);
		const totalValue = result.hits.total;
		const total = typeof totalValue === 'number' ? totalValue : (totalValue?.value ?? 0);
		const hits = result.hits.hits.map((hit) => ({...hit._source!, id: hit._id!}));
		const lastHit = result.hits.hits.at(-1);
		const cursor = lastHit?.sort?.map((v) => String(v));
		return {hits, total, cursor};
	}
}
