// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Client} from '@elastic/elasticsearch';
import type {SortCombinations} from '@elastic/elasticsearch/lib/api/types';
import type {ReportSearchFilters, SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchIndexAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import type {ElasticsearchFilter} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {
	compactFilters,
	esExistsFilter,
	esNotExistsFilter,
	esTermFilter,
} from '@pkgs/elasticsearch_search/src/ElasticsearchFilterUtils';
import {ELASTICSEARCH_INDEX_DEFINITIONS} from '@pkgs/elasticsearch_search/src/ElasticsearchIndexDefinitions';

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

function buildReportSort(filters: ReportSearchFilters): Array<SortCombinations> | undefined {
	const sortBy = filters.sortBy ?? 'reportedAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [{[sortBy]: {order: sortOrder}}];
}

export interface ElasticsearchReportAdapterOptions {
	client: Client;
	lock?: ElasticsearchDistributedLock;
}

export class ElasticsearchReportAdapter extends ElasticsearchIndexAdapter<ReportSearchFilters, SearchableReport> {
	constructor(options: ElasticsearchReportAdapterOptions) {
		super({
			client: options.client,
			index: ELASTICSEARCH_INDEX_DEFINITIONS.reports,
			searchableFields: ['category', 'additionalInfo', 'reportedGuildName', 'reportedChannelName'],
			buildFilters: buildReportFilters,
			buildSort: buildReportSort,
			lock: options.lock,
		});
	}
}
