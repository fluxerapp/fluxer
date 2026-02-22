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
import {compactFilters, esAndTerms, esTermFilter} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

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

	return compactFilters(clauses);
}

function buildGuildSort(filters: GuildSearchFilters): Array<Record<string, unknown>> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchGuildAdapterOptions {
	client: Client;
}

export class ElasticsearchGuildAdapter extends ElasticsearchIndexAdapter<GuildSearchFilters, SearchableGuild> {
	constructor(options: ElasticsearchGuildAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.guilds,
			searchableFields: ['name', 'vanityUrlCode', 'discoveryDescription'],
			buildFilters: buildGuildFilters,
			buildSort: buildGuildSort,
		});
	}
}
