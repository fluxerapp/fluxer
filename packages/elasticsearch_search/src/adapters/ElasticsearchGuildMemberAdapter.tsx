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
	esRangeFilter,
	esTermFilter,
	esTermsFilter,
} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {
	GuildMemberSearchFilters,
	SearchableGuildMember,
} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

function buildGuildMemberFilters(filters: GuildMemberSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];

	clauses.push(esTermFilter('guildId', filters.guildId));

	if (filters.roleIds && filters.roleIds.length > 0) {
		clauses.push(...esAndTerms('roleIds', filters.roleIds));
	}

	if (filters.joinedAtGte !== undefined) clauses.push(esRangeFilter('joinedAt', {gte: filters.joinedAtGte}));
	if (filters.joinedAtLte !== undefined) clauses.push(esRangeFilter('joinedAt', {lte: filters.joinedAtLte}));

	if (filters.joinSourceType && filters.joinSourceType.length > 0) {
		clauses.push(esTermsFilter('joinSourceType', filters.joinSourceType));
	}

	if (filters.sourceInviteCode && filters.sourceInviteCode.length > 0) {
		clauses.push(esTermsFilter('sourceInviteCode', filters.sourceInviteCode));
	}

	if (filters.userCreatedAtGte !== undefined)
		clauses.push(esRangeFilter('userCreatedAt', {gte: filters.userCreatedAtGte}));
	if (filters.userCreatedAtLte !== undefined)
		clauses.push(esRangeFilter('userCreatedAt', {lte: filters.userCreatedAtLte}));

	if (filters.isBot !== undefined) clauses.push(esTermFilter('isBot', filters.isBot));

	return compactFilters(clauses);
}

function buildGuildMemberSort(filters: GuildMemberSearchFilters): Array<Record<string, unknown>> | undefined {
	const sortBy = filters.sortBy ?? 'joinedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchGuildMemberAdapterOptions {
	client: Client;
}

export class ElasticsearchGuildMemberAdapter extends ElasticsearchIndexAdapter<
	GuildMemberSearchFilters,
	SearchableGuildMember
> {
	constructor(options: ElasticsearchGuildMemberAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.guild_members,
			searchableFields: ['username', 'discriminator', 'globalName', 'nickname', 'userId'],
			buildFilters: buildGuildMemberFilters,
			buildSort: buildGuildMemberSort,
		});
	}
}
