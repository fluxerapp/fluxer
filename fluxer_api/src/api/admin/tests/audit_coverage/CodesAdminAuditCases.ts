// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';

export const CodesAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/gift-codes',
		async prepare() {
			return {
				request: {
					path: '/admin/gift-codes',
					body: {count: 2, duration_type: 'months', duration_quantity: 3},
				},
				expected: {
					action: 'generate_gift_codes',
					targetType: 'gift_code',
					targetId: '0',
					metadata: {count: '2', duration_type: 'months', duration_quantity: '3'},
				},
			};
		},
	},
];
