// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {compactFilters, esAndTerms, esTermFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

function buildGuildFilters(filters: GuildSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];
	if (filters.ownerId) clauses.push(esTermFilter('ownerId', filters.ownerId));
	if (filters.verificationLevel !== undefined)
		clauses.push(esTermFilter('verificationLevel', filters.verificationLevel));
	if (filters.mfaLevel !== undefined) clauses.push(esTermFilter('mfaLevel', filters.mfaLevel));
	if (filters.nsfwLevel !== undefined) clauses.push(esTermFilter('nsfwLevel', filters.nsfwLevel));
	if (filters.hasFeature && filters.hasFeature.length > 0) {
		clauses.push(...esAndTerms('features', filters.hasFeature));
	}
	if (filters.isDiscoverable !== undefined) clauses.push(esTermFilter('isDiscoverable', filters.isDiscoverable));
	if (filters.discoveryCategory !== undefined)
		clauses.push(esTermFilter('discoveryCategory', filters.discoveryCategory));
	if (filters.discoveryPrimaryLanguage !== undefined)
		clauses.push(esTermFilter('discoveryPrimaryLanguage', filters.discoveryPrimaryLanguage));
	if (filters.discoveryTag !== undefined && filters.discoveryTag.length > 0)
		clauses.push(esTermFilter('discoveryTags.keyword', filters.discoveryTag.toLowerCase()));
	return compactFilters(clauses);
}

function buildGuildSort(filters: GuildSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchGuildAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchGuildAdapter extends ElasticsearchIndexAdapter<GuildSearchFilters, SearchableGuild> {
	constructor(options: ElasticsearchGuildAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.guilds,
			searchableFields: ['name^10', 'discoveryTags^5', 'vanityUrlCode^3', 'discoveryDescription'],
			searchType: 'bool_prefix',
			buildFilters: buildGuildFilters,
			buildSort: buildGuildSort,
			lock: options.lock,
		});
	}
}
