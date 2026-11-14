// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {expect} from 'vitest';

export const SearchAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/search/indexes/:index_name/refreshes',
		async prepare() {
			return {
				request: {path: '/admin/search/indexes/users/refreshes', body: {}},
				expected: {
					action: 'queue_refresh_index',
					targetType: 'search_index',
					targetId: '0',
					metadata: {index_type: 'users', job_id: expect.any(String)},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/search/index-refreshes/:job_id',
		exempt: true,
		async prepare() {
			return {request: {path: '/admin/search/index-refreshes/1501314428688998182'}};
		},
	},
];
