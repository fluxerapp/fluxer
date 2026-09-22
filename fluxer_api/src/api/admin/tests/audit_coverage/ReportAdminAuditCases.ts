// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createBuilder} from '@app/api/test/TestRequestBuilder';

interface FiledUserReport {
	reportId: string;
	reportedUserId: string;
}

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
					metadata: {report_id: reportId, report_type: '1'},
				},
			};
		},
	},
];
