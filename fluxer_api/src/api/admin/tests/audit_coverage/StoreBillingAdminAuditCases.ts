// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {seedStorePurchase} from '@app/api/store_billing/tests/StoreBillingTestUtils';

export const StoreBillingAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/users/:user_id/store-purchases',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await seedStorePurchase(createUserID(BigInt(target.userId)));
			return {
				request: {path: `/admin/users/${target.userId}/store-purchases`},
				expected: {
					action: 'list_user_store_purchases',
					targetType: 'user',
					targetId: target.userId,
					metadata: {result_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/users/:user_id/store-purchases/refresh',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/store-purchases/refresh`},
				expected: {
					action: 'refresh_store_purchases',
					targetType: 'user',
					targetId: target.userId,
					metadata: {purchase_count: '0'},
				},
			};
		},
	},
];
