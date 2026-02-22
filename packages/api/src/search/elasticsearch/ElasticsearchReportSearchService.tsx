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

import type {GuildID, MessageID, ReportID, UserID} from '@fluxer/api/src/BrandedTypes';
import type {IARSubmission} from '@fluxer/api/src/report/IReportRepository';
import {ElasticsearchSearchServiceBase} from '@fluxer/api/src/search/elasticsearch/ElasticsearchSearchServiceBase';
import type {IReportSearchService} from '@fluxer/api/src/search/IReportSearchService';
import {convertToSearchableReport} from '@fluxer/api/src/search/report/ReportSearchSerializer';
import {
	ElasticsearchReportAdapter,
	type ElasticsearchReportAdapterOptions,
} from '@fluxer/elasticsearch_search/src/adapters/ElasticsearchReportAdapter';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {ReportSearchFilters, SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface ElasticsearchReportSearchServiceOptions extends ElasticsearchReportAdapterOptions {}

export class ElasticsearchReportSearchService
	extends ElasticsearchSearchServiceBase<ReportSearchFilters, SearchableReport, ElasticsearchReportAdapter>
	implements IReportSearchService
{
	constructor(options: ElasticsearchReportSearchServiceOptions) {
		super(new ElasticsearchReportAdapter({client: options.client}));
	}

	async indexReport(report: IARSubmission): Promise<void> {
		await this.indexDocument(convertToSearchableReport(report));
	}

	async indexReports(reports: Array<IARSubmission>): Promise<void> {
		if (reports.length === 0) return;
		await this.indexDocuments(reports.map(convertToSearchableReport));
	}

	async updateReport(report: IARSubmission): Promise<void> {
		await this.updateDocument(convertToSearchableReport(report));
	}

	async deleteReport(reportId: ReportID): Promise<void> {
		await this.deleteDocument(reportId.toString());
	}

	async deleteReports(reportIds: Array<ReportID>): Promise<void> {
		await this.deleteDocuments(reportIds.map((id) => id.toString()));
	}

	searchReports(
		query: string,
		filters: ReportSearchFilters,
		options?: {limit?: number; offset?: number},
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.search(query, filters, options);
	}

	listReportsByReporter(
		reporterId: UserID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reporterId: reporterId.toString()}, {limit, offset});
	}

	listReportsByStatus(status: number, limit?: number, offset?: number): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {status}, {limit, offset});
	}

	listReportsByType(
		reportType: number,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportType}, {limit, offset});
	}

	listReportsByReportedUser(
		reportedUserId: UserID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedUserId: reportedUserId.toString()}, {limit, offset});
	}

	listReportsByReportedGuild(
		reportedGuildId: GuildID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedGuildId: reportedGuildId.toString()}, {limit, offset});
	}

	listReportsByReportedMessage(
		reportedMessageId: MessageID,
		limit?: number,
		offset?: number,
	): Promise<SchemaSearchResult<SearchableReport>> {
		return this.searchReports('', {reportedMessageId: reportedMessageId.toString()}, {limit, offset});
	}
}
