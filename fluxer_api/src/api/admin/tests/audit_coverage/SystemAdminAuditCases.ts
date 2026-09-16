// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {expect} from 'vitest';

export const SystemAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/system/heap-snapshots',
		async prepare() {
			return {
				request: {path: '/admin/system/heap-snapshots'},
				expected: {
					action: 'create_heap_snapshot',
					targetType: 'system',
					targetId: '0',
					metadata: {size_bytes: expect.any(String)},
				},
			};
		},
	},
];
