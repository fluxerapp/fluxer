// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createReportID} from '@app/api/BrandedTypes';
import {getReportRepository} from '@app/api/middleware/ServiceSingletons';
import {listReportReasons} from '@app/api/report/flows/ReportReasonCatalog';
import {createBuilder} from '@app/api/test/TestRequestBuilder';

interface FiledUserReport {
	reportId: string;
	reportedUserId: string;
}

const HOLD_UNTIL = '2099-01-01T00:00:00.000Z';
const PREVIOUS_HOLD_UNTIL = '2098-06-01T00:00:00.000Z';

async function fileUserReport({harness}: AdminAuditCoverageContext): Promise<FiledUserReport> {
	const reporter = await createTestAccount(harness);
	const reported = await createTestAccount(harness);
	const report = await createBuilder<{report_id: string}>(harness, reporter.token)
		.post('/reports/user')
		.body({user_id: reported.userId, category: 'harassment'})
		.execute();
	return {reportId: report.report_id, reportedUserId: reported.userId};
}

export const ReportAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/reports',
		name: 'search',
		search: 'enabled',
		async prepare(context) {
			const {reportedUserId} = await fileUserReport(context);
			await fileUserReport(context);
			return {
				request: {
					path: `/admin/reports?q=harassment&report_type=user&reported_user_id=${reportedUserId}&sort_by=created_at&sort_order=asc&limit=10`,
				},
				expected: {
					action: 'search_reports',
					targetType: 'report',
					targetId: '0',
					metadata: {
						has_query: 'true',
						report_type: 'user',
						reported_user_id: reportedUserId,
						sort_by: 'created_at',
						sort_order: 'asc',
						limit: '10',
						offset: '0',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/reports',
		name: 'status',
		search: 'enabled',
		async prepare(context) {
			await fileUserReport(context);
			await fileUserReport(context);
			return {
				request: {path: '/admin/reports?status=pending&limit=1&offset=1'},
				expected: {
					action: 'search_reports',
					targetType: 'report',
					targetId: '0',
					metadata: {
						status: 'pending',
						limit: '1',
						offset: '1',
						result_count: '1',
						total: '2',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/reports',
		name: 'reason',
		search: 'enabled',
		async prepare(context) {
			await fileUserReport(context);
			return {
				request: {path: '/admin/reports?status=pending&reason=csam'},
				expected: {
					action: 'search_reports',
					targetType: 'report',
					targetId: '0',
					metadata: {
						status: 'pending',
						reason: 'csam',
						sort_by: 'reported_at',
						sort_order: 'desc',
						limit: '50',
						offset: '0',
						result_count: '0',
						total: '0',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/reports',
		name: 'category',
		search: 'enabled',
		async prepare(context) {
			await fileUserReport(context);
			return {
				request: {path: '/admin/reports?category=harassment'},
				expected: {
					action: 'search_reports',
					targetType: 'report',
					targetId: '0',
					metadata: {
						category: 'harassment',
						sort_by: 'reported_at',
						sort_order: 'desc',
						limit: '50',
						offset: '0',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/report-reasons',
		async prepare() {
			return {
				request: {path: '/admin/report-reasons'},
				expected: {
					action: 'list_report_reasons',
					targetType: 'report',
					targetId: '0',
					metadata: {result_count: String(listReportReasons().length)},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/reports/:report_id',
		async prepare(context) {
			const {reportId} = await fileUserReport(context);
			return {
				request: {path: `/admin/reports/${reportId}`},
				expected: {
					action: 'get_report',
					targetType: 'report',
					targetId: reportId,
					metadata: {report_type: '1', status: '0'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/reports/:report_id',
		async prepare(context) {
			const {reportId} = await fileUserReport(context);
			return {
				request: {
					path: `/admin/reports/${reportId}`,
					body: {status: 'resolved', public_comment: 'We actioned the account.'},
				},
				expected: {
					action: 'resolve_report',
					targetType: 'report',
					targetId: reportId,
					metadata: {
						report_id: reportId,
						report_type: '1',
						notify_reporter: 'true',
						reporter_dm_sent: 'true',
						reporter_email_sent: 'true',
						reporter_dsa_email_sent: 'false',
					},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/reports/:report_id',
		name: 'generic notice',
		async prepare(context) {
			const {reportId} = await fileUserReport(context);
			return {
				request: {
					path: `/admin/reports/${reportId}`,
					body: {status: 'resolved', public_comment: 'Internal wording', notify_reporter: false},
				},
				expected: {
					action: 'resolve_report',
					targetType: 'report',
					targetId: reportId,
					metadata: {
						report_id: reportId,
						report_type: '1',
						notify_reporter: 'false',
						internal_comment: 'Internal wording',
						reporter_dm_sent: 'true',
						reporter_email_sent: 'true',
						reporter_dsa_email_sent: 'false',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/reports/:report_id/legal-hold',
		async prepare(context) {
			const {reportId} = await fileUserReport(context);
			return {
				request: {
					path: `/admin/reports/${reportId}/legal-hold`,
					body: {legal_hold_until: HOLD_UNTIL, legal_hold_reason: 'Preservation request 42'},
				},
				expected: {
					action: 'set_report_legal_hold',
					targetType: 'report',
					targetId: reportId,
					metadata: {
						report_id: reportId,
						report_type: '1',
						legal_hold_until: HOLD_UNTIL,
						legal_hold_reason: 'Preservation request 42',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/reports/:report_id/legal-hold',
		name: 'clear',
		async prepare(context) {
			const {reportId} = await fileUserReport(context);
			await getReportRepository().setReportLegalHold(
				createReportID(BigInt(reportId)),
				new Date(PREVIOUS_HOLD_UNTIL),
				'Preservation request 41',
			);
			return {
				request: {
					path: `/admin/reports/${reportId}/legal-hold`,
					body: {legal_hold_until: null},
				},
				expected: {
					action: 'clear_report_legal_hold',
					targetType: 'report',
					targetId: reportId,
					metadata: {
						report_id: reportId,
						report_type: '1',
						previous_legal_hold_until: PREVIOUS_HOLD_UNTIL,
					},
				},
			};
		},
	},
];
