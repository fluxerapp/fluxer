// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, MessageID, ReportID, UserID} from '@app/api/BrandedTypes';
import type {IARSubmission} from '@app/api/report/IReportRepository';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import {convertToSearchableReport} from '@app/api/search/report/ReportSearchSerializer';
import {SearchAdapterServiceBase} from '@app/api/search/SearchAdapterServiceBase';
import type {SearchResult as SchemaSearchResult} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {ReportSearchFilters, SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {
	ElasticsearchReportAdapter,
	type ElasticsearchReportAdapterOptions,
} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchReportAdapter';

interface ElasticsearchReportSearchServiceOptions extends ElasticsearchReportAdapterOptions {}

export class ElasticsearchReportSearchService
	extends SearchAdapterServiceBase<ReportSearchFilters, SearchableReport, ElasticsearchReportAdapter>
	implements IReportSearchService
{
	constructor(options: ElasticsearchReportSearchServiceOptions) {
		super(new ElasticsearchReportAdapter({client: options.client, lock: options.lock}));
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
		options?: {
			limit?: number;
			offset?: number;
		},
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
