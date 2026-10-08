// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminReportService} from '@app/api/admin/services/AdminReportService';
import {createReportID, type ReportID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {type IARSubmission, ReportStatus, ReportType} from '@app/api/report/IReportRepository';
import type {ReportService} from '@app/api/report/ReportService';
import {setInjectedSearchProvider} from '@app/api/SearchFactory';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import type {ReportSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {afterEach, describe, expect, test, vi} from 'vitest';

const REPORT_A = createReportID(101n);
const REPORT_B = createReportID(102n);
const ORPHAN = createReportID(999n);
const NO_ACLS: ReadonlySet<string> = new Set();

interface SearchCall {
	query: string;
	filters: ReportSearchFilters;
	options: {limit?: number; offset?: number} | undefined;
}

function storedReport(reportId: ReportID): IARSubmission {
	return {
		reportId,
		reporterId: null,
		reporterEmail: null,
		reportedUserId: null,
		reportedAt: new Date('2026-10-01T12:00:00.000Z'),
		status: ReportStatus.PENDING,
		reportType: ReportType.GUILD,
		category: 'other',
		reportedGuildNsfw: false,
		reportedChannelEffectiveNsfw: false,
		reportedWebhookCreatorDiscriminator: null,
		reportedWebhookCreatorUsername: null,
	} as IARSubmission;
}

function createService(options: {
	hitIds: Array<ReportID>;
	total: number;
	getReport?: (reportId: ReportID) => Promise<IARSubmission>;
}): {service: AdminReportService; calls: Array<SearchCall>; lookups: Array<ReportID>} {
	const calls: Array<SearchCall> = [];
	const lookups: Array<ReportID> = [];
	const stored = new Map([REPORT_A, REPORT_B].map((reportId) => [reportId, storedReport(reportId)]));
	const getReport =
		options.getReport ??
		(async (reportId: ReportID) => {
			const report = stored.get(reportId);
			if (!report) {
				throw new UnknownReportError();
			}
			return report;
		});
	const reportSearchService = {
		searchReports: async (query: string, filters: ReportSearchFilters, searchOptions?: SearchCall['options']) => {
			calls.push({query, filters, options: searchOptions});
			return {hits: options.hitIds.map((reportId) => ({id: reportId.toString()})), total: options.total};
		},
	} as unknown as IReportSearchService;
	setInjectedSearchProvider({getReportSearchService: () => reportSearchService} as unknown as ISearchProvider);
	const reportService = {
		getReport: async (reportId: ReportID) => {
			lookups.push(reportId);
			return getReport(reportId);
		},
	} as unknown as ReportService;
	const service = new AdminReportService({reportService} as ConstructorParameters<typeof AdminReportService>[0]);
	return {service, calls, lookups};
}

describe('Admin report search totals', () => {
	afterEach(() => {
		setInjectedSearchProvider(undefined);
		vi.restoreAllMocks();
	});

	test('a hit with no stored report is skipped and the index total is returned as is', async () => {
		const warn = vi.spyOn(Logger.child({}), 'warn');
		const {service, calls, lookups} = createService({hitIds: [REPORT_A, ORPHAN, REPORT_B], total: 10});
		const result = await service.searchReports(
			{query: 'spam', limit: 3, offset: 6, sort_by: 'reportedAt', sort_order: 'desc'},
			NO_ACLS,
		);
		expect(result.reports.map((report) => report.report_id)).toEqual(['101', '102']);
		expect(result.total).toBe(10);
		expect(result.offset).toBe(6);
		expect(result.limit).toBe(3);
		expect(lookups).toEqual([REPORT_A, ORPHAN, REPORT_B]);
		expect(calls).toEqual([
			{query: 'spam', filters: {sortBy: 'reportedAt', sortOrder: 'desc'}, options: {limit: 3, offset: 6}},
		]);
		expect(warn).toHaveBeenCalledTimes(1);
		expect(warn.mock.calls[0]).toEqual([
			{orphanedReportIds: ['999']},
			'Report search index lists reports that are no longer stored, run refresh_search_index reports',
		]);
	});

	test('a page with every report stored logs nothing', async () => {
		const warn = vi.spyOn(Logger.child({}), 'warn');
		const {service} = createService({hitIds: [REPORT_B, REPORT_A], total: 2});
		const result = await service.searchReports(
			{limit: 50, offset: 0, sort_by: 'reportedAt', sort_order: 'desc'},
			NO_ACLS,
		);
		expect(result.reports.map((report) => report.report_id)).toEqual(['102', '101']);
		expect(result.total).toBe(2);
		expect(warn).not.toHaveBeenCalled();
	});

	test('a page of only missing reports is empty and keeps the index total', async () => {
		const {service} = createService({hitIds: [ORPHAN], total: 7});
		expect(
			await service.searchReports({limit: 1, offset: 0, sort_by: 'reportedAt', sort_order: 'desc'}, NO_ACLS),
		).toEqual({reports: [], total: 7, offset: 0, limit: 1});
	});

	test('a failed report lookup rejects the search', async () => {
		const failure = new Error('report storage is unavailable');
		const {service} = createService({
			hitIds: [REPORT_A, ORPHAN, REPORT_B],
			total: 10,
			getReport: async (reportId) => {
				if (reportId === ORPHAN) {
					throw failure;
				}
				return storedReport(reportId);
			},
		});
		await expect(
			service.searchReports({limit: 3, offset: 0, sort_by: 'reportedAt', sort_order: 'desc'}, NO_ACLS),
		).rejects.toBe(failure);
	});

	test('every supplied filter reaches the index under its typed name', async () => {
		const {service, calls} = createService({hitIds: [], total: 0});
		await service.searchReports(
			{
				limit: 50,
				offset: 0,
				reporter_id: 11n,
				status: 1,
				report_type: 0,
				category: 'harassment',
				reason: 'csam',
				reported_user_id: 12n,
				reported_webhook_id: 13n,
				reported_guild_id: 14n,
				reported_channel_id: 15n,
				guild_context_id: 16n,
				resolved_by_admin_id: 17n,
				sort_by: 'resolvedAt',
				sort_order: 'asc',
			},
			NO_ACLS,
		);
		const expected: Required<Omit<ReportSearchFilters, 'reportedMessageId' | 'isResolved'>> = {
			reporterId: '11',
			status: 1,
			reportType: 0,
			category: 'harassment',
			reason: 'csam',
			reportedUserId: '12',
			reportedWebhookId: '13',
			reportedGuildId: '14',
			reportedChannelId: '15',
			guildContextId: '16',
			resolvedByAdminId: '17',
			sortBy: 'resolvedAt',
			sortOrder: 'asc',
		};
		expect(calls).toEqual([{query: '', filters: expected, options: {limit: 50, offset: 0}}]);
	});

	test('the search is unavailable without a search service', async () => {
		const {service} = createService({hitIds: [], total: 0});
		setInjectedSearchProvider(undefined);
		await expect(
			service.searchReports({limit: 50, offset: 0, sort_by: 'reportedAt', sort_order: 'desc'}, NO_ACLS),
		).rejects.toBeInstanceOf(FeatureTemporarilyDisabledError);
	});
});
