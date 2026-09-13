// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {SearchableUser, UserSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {
	compactFilters,
	esAndTerms,
	esExistsFilter,
	esNotExistsFilter,
	esRangeFilter,
	esTermFilter,
} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

function buildUserFilters(filters: UserSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];
	if (filters.isBot !== undefined) clauses.push(esTermFilter('isBot', filters.isBot));
	if (filters.isSystem !== undefined) clauses.push(esTermFilter('isSystem', filters.isSystem));
	if (filters.emailVerified !== undefined) clauses.push(esTermFilter('emailVerified', filters.emailVerified));
	if (filters.emailBounced !== undefined) clauses.push(esTermFilter('emailBounced', filters.emailBounced));
	if (filters.hasPremium !== undefined) {
		clauses.push(filters.hasPremium ? esExistsFilter('premiumType') : esNotExistsFilter('premiumType'));
	}
	if (filters.isTempBanned !== undefined) {
		clauses.push(filters.isTempBanned ? esExistsFilter('tempBannedUntil') : esNotExistsFilter('tempBannedUntil'));
	}
	if (filters.isPendingDeletion !== undefined) {
		clauses.push(
			filters.isPendingDeletion ? esExistsFilter('pendingDeletionAt') : esNotExistsFilter('pendingDeletionAt'),
		);
	}
	if (filters.hasAcl && filters.hasAcl.length > 0) {
		clauses.push(...esAndTerms('acls', filters.hasAcl));
	}
	if (filters.minSuspiciousActivityFlags !== undefined) {
		clauses.push(esRangeFilter('suspiciousActivityFlags', {gte: filters.minSuspiciousActivityFlags}));
	}
	if (filters.createdAtGreaterThanOrEqual !== undefined) {
		clauses.push(esRangeFilter('createdAt', {gte: filters.createdAtGreaterThanOrEqual}));
	}
	if (filters.createdAtLessThanOrEqual !== undefined) {
		clauses.push(esRangeFilter('createdAt', {lte: filters.createdAtLessThanOrEqual}));
	}
	return compactFilters(clauses);
}

function buildUserSort(filters: UserSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchUserAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchUserAdapter extends ElasticsearchIndexAdapter<UserSearchFilters, SearchableUser> {
	constructor(options: ElasticsearchUserAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.users,
			searchableFields: ['username', 'email', 'phone', 'id'],
			buildFilters: buildUserFilters,
			buildSort: buildUserSort,
			lock: options.lock,
		});
	}
}
