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
import {compactFilters, esTermFilter} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {AuditLogSearchFilters, SearchableAuditLog} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

function buildAuditLogFilters(filters: AuditLogSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];

	if (filters.adminUserId) clauses.push(esTermFilter('adminUserId', filters.adminUserId));
	if (filters.targetType) clauses.push(esTermFilter('targetType', filters.targetType));
	if (filters.targetId) clauses.push(esTermFilter('targetId', filters.targetId));
	if (filters.action) clauses.push(esTermFilter('action', filters.action));

	return compactFilters(clauses);
}

function buildAuditLogSort(filters: AuditLogSearchFilters): Array<Record<string, unknown>> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{createdAt: {order: sortOrder}}];
}

export interface ElasticsearchAuditLogAdapterOptions {
	client: Client;
}

export class ElasticsearchAuditLogAdapter extends ElasticsearchIndexAdapter<AuditLogSearchFilters, SearchableAuditLog> {
	constructor(options: ElasticsearchAuditLogAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.audit_logs,
			searchableFields: ['action', 'targetType', 'targetId', 'auditLogReason'],
			buildFilters: buildAuditLogFilters,
			buildSort: buildAuditLogSort,
		});
	}
}
