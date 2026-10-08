// SPDX-License-Identifier: AGPL-3.0-or-later

import {createReportID, type ReportID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import {type IARSubmission, type IReportRepository, ReportStatus} from '@app/api/report/IReportRepository';
import {ReportService} from '@app/api/report/ReportService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {describe, expect, test} from 'vitest';

const REPORT_A = createReportID(101n);
const REPORT_B = createReportID(102n);
const ORPHAN = createReportID(999n);

interface ListCall {
	status: number;
	limit: number | undefined;
	offset: number | undefined;
}

function storedReport(reportId: ReportID): IARSubmission {
	return {reportId, status: ReportStatus.PENDING} as IARSubmission;
}

function createService(options: {
	hitIds: Array<ReportID>;
	total: number;
	getReport?: (reportId: ReportID) => Promise<IARSubmission | null>;
	search?: boolean;
}): {service: ReportService; calls: Array<ListCall>; lookups: Array<ReportID>} {
	const calls: Array<ListCall> = [];
	const lookups: Array<ReportID> = [];
	const stored = new Map([REPORT_A, REPORT_B].map((reportId) => [reportId, storedReport(reportId)]));
	const getReport = options.getReport ?? (async (reportId: ReportID) => stored.get(reportId) ?? null);
	const reportRepository = {
		getReport: async (reportId: ReportID) => {
			lookups.push(reportId);
			return getReport(reportId);
		},
	} as unknown as IReportRepository;
	const reportSearchService = {
		listReportsByStatus: async (status: number, limit?: number, offset?: number) => {
			calls.push({status, limit, offset});
			return {hits: options.hitIds.map((reportId) => ({id: reportId.toString()})), total: options.total};
		},
	} as unknown as IReportSearchService;
	const service = new ReportService(
		reportRepository,
		{} as IChannelRepository,
		{} as IGuildRepositoryAggregate,
		{} as IUserRepository,
		{} as IInviteRepository,
		{} as IEmailService,
		{} as IEmailDnsValidationService,
		{} as ISnowflakeService,
		{} as IStorageService,
		{} as IGatewayService,
		{} as IRateLimitService,
		{} as IWebhookRepository,
		options.search === false ? null : reportSearchService,
	);
	return {service, calls, lookups};
}

describe('Report listing totals', () => {
	test('a hit with no stored report is skipped and the index total is returned as is', async () => {
		const {service, calls, lookups} = createService({hitIds: [REPORT_A, ORPHAN, REPORT_B], total: 10});
		const result = await service.listReportsByStatus(ReportStatus.PENDING, 3, 6);
		expect(result.reports.map((report) => report.reportId)).toEqual([REPORT_A, REPORT_B]);
		expect(result.total).toBe(10);
		expect(calls).toEqual([{status: ReportStatus.PENDING, limit: 3, offset: 6}]);
		expect(lookups).toEqual([REPORT_A, ORPHAN, REPORT_B]);
	});

	test('the order of the index hits is kept', async () => {
		const {service} = createService({hitIds: [REPORT_B, REPORT_A], total: 2});
		const result = await service.listReportsByStatus(ReportStatus.PENDING);
		expect(result.reports.map((report) => report.reportId)).toEqual([REPORT_B, REPORT_A]);
		expect(result.total).toBe(2);
	});

	test('a page of only missing reports is empty and keeps the index total', async () => {
		const {service} = createService({hitIds: [ORPHAN], total: 7});
		expect(await service.listReportsByStatus(ReportStatus.RESOLVED, 1, 0)).toEqual({reports: [], total: 7});
	});

	test('a failed report lookup rejects the listing', async () => {
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
		await expect(service.listReportsByStatus(ReportStatus.PENDING)).rejects.toBe(failure);
	});

	test('the listing is unavailable without a search service', async () => {
		const {service} = createService({hitIds: [], total: 0, search: false});
		await expect(service.listReportsByStatus(ReportStatus.PENDING)).rejects.toBeInstanceOf(
			FeatureTemporarilyDisabledError,
		);
	});
});
