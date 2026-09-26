// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';

export const SystemDmAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/system-dms',
		async prepare({harness}) {
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			return {
				request: {
					path: '/admin/system-dms',
					body: {content: 'Scheduled maintenance', user_ids: [first.userId, second.userId]},
				},
				expected: {
					action: 'system_dm.send',
					targetType: 'system_dm',
					targetId: '0',
					metadata: {recipient_count: '2', content_length: '21'},
				},
			};
		},
	},
];
