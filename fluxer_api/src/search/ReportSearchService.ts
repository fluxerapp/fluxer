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

import type {Index, MeiliSearch} from 'meilisearch';
import type {GuildID, MessageID, ReportID, UserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import type {IARSubmission} from '~/report/IReportRepository';
import {SEARCH_MAX_TOTAL_HITS} from '~/search/constants';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

const REPORT_INDEX_NAME = 'reports';

interface SearchableReport {
	id: string;
	reporterId: string;
	reportedAt: number;
	status: number;
	reportType: number;
	category: string;
	additionalInfo: string | null;
	reportedUserId: string | null;
	reportedGuildId: string | null;
	reportedGuildName: string | null;
	reportedMessageId: string | null;
	reportedChannelId: string | null;
	reportedChannelName: string | null;
	guildContextId: string | null;
	resolvedAt: number | null;
	resolvedByAdminId: string | null;
	publicComment: string | null;
	createdAt: number;
}

interface ReportSearchFilters {
	reporterId?: string;
	status?: number;
	reportType?: number;
	category?: string;
	reportedUserId?: string;
	reportedGuildId?: string;
	reportedMessageId?: string;
	guildContextId?: string;
	resolvedByAdminId?: string;
	isResolved?: boolean;
	sortBy?: 'createdAt' | 'reportedAt' | 'resolvedAt' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export class ReportSearchService {
	private meilisearch: MeiliSearch;
	private index: Index<SearchableReport> | null = null;

	constructor(meilisearch: MeiliSearch) {
		this.meilisearch = meilisearch;
	}

	async initialize(): Promise<void> {
		try {
			this.index = this.meilisearch.index<SearchableReport>(REPORT_INDEX_NAME);

			await this.index.updateSettings({
				searchableAttributes: [
					'id',
					'category',
					'additionalInfo',
					'reportedGuildName',
					'reportedChannelName',
					'publicComment',
				],
				filterableAttributes: [
					'reporterId',
					'status',
					'reportType',
					'category',
					'reportedUserId',
					'reportedGuildId',
					'reportedMessageId',
					'reportedChannelId',
					'guildContextId',
					'resolvedByAdminId',
					'reportedAt',
					'resolvedAt',
					'createdAt',
				],
				sortableAttributes: ['createdAt', 'reportedAt', 'resolvedAt'],
				rankingRules: ['words', 'typo', 'proximity', 'attribute', 'sort', 'exactness'],
				pagination: {
					maxTotalHits: SEARCH_MAX_TOTAL_HITS,
				},
			});

			Logger.debug('Report search index initialized successfully');
		} catch (error) {
			Logger.error({error}, 'Failed to initialize report search index');
			throw error;
		}
	}

	async indexReport(report: IARSubmission): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		const searchableReport = this.convertToSearchableReport(report);

		try {
			await this.index.addDocuments([searchableReport], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({reportId: report.reportId, error}, 'Failed to index report');
			throw error;
		}
	}

	async indexReports(reports: Array<IARSubmission>): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		if (reports.length === 0) return;

		const searchableReports = reports.map((report) => this.convertToSearchableReport(report));

		try {
			await this.index.addDocuments(searchableReports, {primaryKey: 'id'});
		} catch (error) {
			Logger.error({count: reports.length, error}, 'Failed to index reports');
			throw error;
		}
	}

	async updateReport(report: IARSubmission): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		const searchableReport = this.convertToSearchableReport(report);

		try {
			await this.index.updateDocuments([searchableReport], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({reportId: report.reportId, error}, 'Failed to update report in search index');
			throw error;
		}
	}

	async deleteReport(reportId: ReportID): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		try {
			await this.index.deleteDocument(reportId.toString());
		} catch (error) {
			Logger.error({reportId, error}, 'Failed to delete report from search index');
			throw error;
		}
	}

	async deleteReports(reportIds: Array<ReportID>): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		if (reportIds.length === 0) return;

		try {
			await this.index.deleteDocuments(reportIds.map((id) => id.toString()));
		} catch (error) {
			Logger.error({count: reportIds.length, error}, 'Failed to delete reports from search index');
			throw error;
		}
	}

	async searchReports(
		query: string,
		filters: ReportSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		const filterStrings = this.buildFilterStrings(filters);
		const sortField = this.buildSortField(filters);

		try {
			const result = await this.index.search(query, {
				filter: filterStrings.length > 0 ? filterStrings : undefined,
				limit: options?.limit ?? 50,
				offset: options?.offset ?? 0,
				sort: sortField,
			});

			return {
				hits: result.hits,
				total: result.estimatedTotalHits ?? 0,
			};
		} catch (error) {
			Logger.error({query, filters, error}, 'Failed to search reports');
			throw error;
		}
	}

	async listReportsByReporter(
		reporterId: UserID,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports(
			'',
			{reporterId: reporterId.toString(), sortBy: 'reportedAt', sortOrder: 'desc'},
			{limit, offset},
		);
	}

	async listReportsByStatus(
		status: number,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports('', {status, sortBy: 'reportedAt', sortOrder: 'desc'}, {limit, offset});
	}

	async listReportsByType(
		reportType: number,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports('', {reportType, sortBy: 'reportedAt', sortOrder: 'desc'}, {limit, offset});
	}

	async listReportsByReportedUser(
		reportedUserId: UserID,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports(
			'',
			{reportedUserId: reportedUserId.toString(), sortBy: 'reportedAt', sortOrder: 'desc'},
			{limit, offset},
		);
	}

	async listReportsByReportedGuild(
		reportedGuildId: GuildID,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports(
			'',
			{reportedGuildId: reportedGuildId.toString(), sortBy: 'reportedAt', sortOrder: 'desc'},
			{limit, offset},
		);
	}

	async listReportsByReportedMessage(
		reportedMessageId: MessageID,
		limit?: number,
		offset?: number,
	): Promise<{hits: Array<SearchableReport>; total: number}> {
		return this.searchReports(
			'',
			{reportedMessageId: reportedMessageId.toString(), sortBy: 'reportedAt', sortOrder: 'desc'},
			{limit, offset},
		);
	}

	async deleteAllDocuments(): Promise<void> {
		if (!this.index) {
			throw new Error('Report search index not initialized');
		}

		try {
			await this.index.deleteAllDocuments();
			Logger.debug('All report documents deleted from search index');
		} catch (error) {
			Logger.error({error}, 'Failed to delete all report documents');
			throw error;
		}
	}

	private buildFilterStrings(filters: ReportSearchFilters): Array<string> {
		const filterStrings: Array<string> = [];

		if (filters.reporterId) {
			filterStrings.push(`reporterId = "${filters.reporterId}"`);
		}

		if (filters.status !== undefined) {
			filterStrings.push(`status = ${filters.status}`);
		}

		if (filters.reportType !== undefined) {
			filterStrings.push(`reportType = ${filters.reportType}`);
		}

		if (filters.category) {
			filterStrings.push(`category = "${filters.category}"`);
		}

		if (filters.reportedUserId) {
			filterStrings.push(`reportedUserId = "${filters.reportedUserId}"`);
		}

		if (filters.reportedGuildId) {
			filterStrings.push(`reportedGuildId = "${filters.reportedGuildId}"`);
		}

		if (filters.reportedMessageId) {
			filterStrings.push(`reportedMessageId = "${filters.reportedMessageId}"`);
		}

		if (filters.guildContextId) {
			filterStrings.push(`guildContextId = "${filters.guildContextId}"`);
		}

		if (filters.resolvedByAdminId) {
			filterStrings.push(`resolvedByAdminId = "${filters.resolvedByAdminId}"`);
		}

		if (filters.isResolved !== undefined) {
			if (filters.isResolved) {
				filterStrings.push(`resolvedAt IS NOT NULL`);
			} else {
				filterStrings.push(`resolvedAt IS NULL`);
			}
		}

		return filterStrings;
	}

	private buildSortField(filters: ReportSearchFilters): Array<string> {
		const sortBy = filters.sortBy ?? 'reportedAt';
		const sortOrder = filters.sortOrder ?? 'desc';

		if (sortBy === 'relevance') {
			return [];
		}

		return [`${sortBy}:${sortOrder}`];
	}

	private convertToSearchableReport(report: IARSubmission): SearchableReport {
		const createdAt = Math.floor(extractTimestamp(BigInt(report.reportId)) / 1000);
		const reportedAt = Math.floor(report.reportedAt.getTime() / 1000);
		const resolvedAt = report.resolvedAt ? Math.floor(report.resolvedAt.getTime() / 1000) : null;

		return {
			id: report.reportId.toString(),
			reporterId: report.reporterId ? report.reporterId.toString() : 'anonymous',
			reportedAt,
			status: report.status,
			reportType: report.reportType,
			category: report.category,
			additionalInfo: report.additionalInfo,
			reportedUserId: report.reportedUserId?.toString() || null,
			reportedGuildId: report.reportedGuildId?.toString() || null,
			reportedGuildName: report.reportedGuildName,
			reportedMessageId: report.reportedMessageId?.toString() || null,
			reportedChannelId: report.reportedChannelId?.toString() || null,
			reportedChannelName: report.reportedChannelName,
			guildContextId: report.guildContextId?.toString() || null,
			resolvedAt,
			resolvedByAdminId: report.resolvedByAdminId?.toString() || null,
			publicComment: report.publicComment,
			createdAt,
		};
	}
}
