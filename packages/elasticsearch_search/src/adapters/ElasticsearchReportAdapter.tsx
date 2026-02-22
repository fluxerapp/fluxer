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
	esExistsFilter,
	esNotExistsFilter,
	esTermFilter,
} from '@fluxer/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@fluxer/elasticsearch_search/src/ElasticsearchIndexDefinitions';
import type {ReportSearchFilters, SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

function buildReportFilters(filters: ReportSearchFilters): Array<ElasticsearchFilter | undefined> {
	const clauses: Array<ElasticsearchFilter | undefined> = [];

	if (filters.reporterId) clauses.push(esTermFilter('reporterId', filters.reporterId));
	if (filters.status !== undefined) clauses.push(esTermFilter('status', filters.status));
	if (filters.reportType !== undefined) clauses.push(esTermFilter('reportType', filters.reportType));
	if (filters.category) clauses.push(esTermFilter('category', filters.category));
	if (filters.reportedUserId) clauses.push(esTermFilter('reportedUserId', filters.reportedUserId));
	if (filters.reportedGuildId) clauses.push(esTermFilter('reportedGuildId', filters.reportedGuildId));
	if (filters.reportedMessageId) clauses.push(esTermFilter('reportedMessageId', filters.reportedMessageId));
	if (filters.guildContextId) clauses.push(esTermFilter('guildContextId', filters.guildContextId));
	if (filters.resolvedByAdminId) clauses.push(esTermFilter('resolvedByAdminId', filters.resolvedByAdminId));

	if (filters.isResolved !== undefined) {
		clauses.push(filters.isResolved ? esExistsFilter('resolvedAt') : esNotExistsFilter('resolvedAt'));
	}

	return compactFilters(clauses);
}

function buildReportSort(filters: ReportSearchFilters): Array<Record<string, unknown>> | undefined {
	const sortBy = filters.sortBy ?? 'reportedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchReportAdapterOptions {
	client: Client;
}

export class ElasticsearchReportAdapter extends ElasticsearchIndexAdapter<ReportSearchFilters, SearchableReport> {
	constructor(options: ElasticsearchReportAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.reports,
			searchableFields: ['category', 'additionalInfo', 'reportedGuildName', 'reportedChannelName'],
			buildFilters: buildReportFilters,
			buildSort: buildReportSort,
		});
	}
}
