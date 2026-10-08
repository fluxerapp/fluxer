// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID} from '@app/api/BrandedTypes';
import type {IARSubmission} from '@app/api/report/IReportRepository';
import type {
	ISearchAdapter as SchemaISearchAdapter,
	SearchResult as SchemaSearchResult,
} from '@fluxer/schema/src/contracts/search/SearchAdapterTypes';
import type {ReportSearchFilters, SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';

export interface IReportSearchService extends SchemaISearchAdapter<ReportSearchFilters, SearchableReport> {
	indexReport(report: IARSubmission): Promise<void>;
	indexReports(reports: Array<IARSubmission>): Promise<void>;
	updateReport(report: IARSubmission): Promise<void>;
	deleteReport(reportId: ReportID): Promise<void>;
	deleteReports(reportIds: Array<ReportID>): Promise<void>;
	searchReports(
		query: string,
		filters: ReportSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<SchemaSearchResult<SearchableReport>>;
	listReportsByStatus(status: number, limit?: number, offset?: number): Promise<SchemaSearchResult<SearchableReport>>;
}
