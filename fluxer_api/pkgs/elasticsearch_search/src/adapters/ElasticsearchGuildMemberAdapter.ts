// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {
	GuildMemberSearchFilters,
	SearchableGuildMember,
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

function buildGuildMemberSort(filters: GuildMemberSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'joinedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchGuildMemberAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchGuildMemberAdapter extends ElasticsearchIndexAdapter<
	GuildMemberSearchFilters,
	SearchableGuildMember
> {
	constructor(options: ElasticsearchGuildMemberAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.guild_members,
			searchableFields: ['username', 'usernameSearch', 'discriminator', 'globalName', 'nickname', 'userId'],
			searchType: 'bool_prefix',
			buildFilters: buildGuildMemberFilters,
			buildSort: buildGuildMemberSort,
			lock: options.lock,
		});
	}
}
