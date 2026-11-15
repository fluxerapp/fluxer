// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {
	SearchableThread,
	ThreadSearchCursor,
	ThreadSearchFilters,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {
	compactFilters,
	esAndTerms,
	esRangeFilter,
	esTermFilter,
	esTermsFilter,
} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

const SORT_FIELDS = {
	last_message_time: 'lastMessageAt',
	archive_time: 'archivedAt',
	creation_time: 'createdAt',
} as const;

function cursorFilter(cursor: ThreadSearchCursor, op: 'gt' | 'lt'): ElasticsearchFilter {
	return {
		bool: {
			should: [
				esRangeFilter('createdAt', {[op]: cursor.createdAt}),
				{
					bool: {
						filter: [
							esTermFilter('createdAt', cursor.createdAt),
							esRangeFilter('idSequence', {[op]: cursor.idSequence}),
						],
					},
				},
			],
			minimum_should_match: 1,
		},
	};
}

function buildThreadFilters(filters: ThreadSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [
		esTermFilter('guildId', filters.guildId),
		esTermFilter('parentId', filters.parentId),
	];
	if (filters.publicOnly) {
		clauses.push(
			filters.privateThreadIds && filters.privateThreadIds.length > 0
				? {
						bool: {
							should: [esTermsFilter('type', [10, 11]), esTermsFilter('id', filters.privateThreadIds)],
							minimum_should_match: 1,
						},
					}
				: esTermsFilter('type', [10, 11]),
		);
	}
	if (filters.archived !== undefined) clauses.push(esTermFilter('archived', filters.archived));
	if (filters.tagIds && filters.tagIds.length > 0) {
		if (filters.tagSetting === 'match_all') clauses.push(...esAndTerms('appliedTagIds', filters.tagIds));
		else clauses.push(esTermsFilter('appliedTagIds', filters.tagIds));
	}
	if (filters.after) clauses.push(cursorFilter(filters.after, 'gt'));
	if (filters.before) clauses.push(cursorFilter(filters.before, 'lt'));
	return compactFilters(clauses);
}

function buildThreadSort(filters: ThreadSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'last_message_time';
	if (sortBy === 'relevance') return undefined;
	const order = filters.sortOrder ?? 'desc';
	return [...new Set([SORT_FIELDS[sortBy], 'createdAt', 'idSequence'])].map((field) => ({[field]: {order}}));
}

export interface ElasticsearchThreadAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchThreadAdapter extends ElasticsearchIndexAdapter<ThreadSearchFilters, SearchableThread> {
	constructor(options: ElasticsearchThreadAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.threads,
			searchableFields: ['name'],
			buildFilters: buildThreadFilters,
			buildSort: buildThreadSort,
			lock: options.lock,
		});
	}
}
