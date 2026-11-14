// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {AuditLogSearchFilters, SearchableAuditLog} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {compactFilters, esTermFilter, esTermsFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

function buildAuditLogFilters(filters: AuditLogSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];
	if (filters.adminUserId) clauses.push(esTermFilter('adminUserId', filters.adminUserId));
	if (filters.targetType) clauses.push(esTermFilter('targetType', filters.targetType));
	if (filters.targetId) clauses.push(esTermFilter('targetId', filters.targetId));
	if (filters.action) clauses.push(esTermFilter('action', filters.action));
	if (filters.actions && filters.actions.length > 0) clauses.push(esTermsFilter('action.keyword', filters.actions));
	if (filters.excludeActions && filters.excludeActions.length > 0) {
		clauses.push({bool: {must_not: [esTermsFilter('action.keyword', filters.excludeActions)]}});
	}
	return compactFilters(clauses);
}

function buildAuditLogSort(filters: AuditLogSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{createdAt: {order: sortOrder}}];
}

export interface ElasticsearchAuditLogAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchAuditLogAdapter extends ElasticsearchIndexAdapter<AuditLogSearchFilters, SearchableAuditLog> {
	constructor(options: ElasticsearchAuditLogAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.audit_logs,
			searchableFields: ['action', 'targetType', 'targetId', 'auditLogReason'],
			buildFilters: buildAuditLogFilters,
			buildSort: buildAuditLogSort,
			lock: options.lock,
		});
	}
}
