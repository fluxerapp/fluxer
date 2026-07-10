// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, createUniqueEmail, createUniqueUsername, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

interface PendingRegistrationResponse {
	user_id: string;
}

interface AuditLogsLogs {
	admin_user_id: string;
	target_type: string;
	target_id: string;
	action: string;
	audit_log_reason: string | null;
}

interface AuditLogsResponse {
	logs: Array<AuditLogsLogs>;
}

describe('pending registration audit logs', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it.each([
		['approve', 'approve_registration'],
		['reject', 'reject_registration'],
	] as const)('logs a pending registration %s decision', async (decision, action) => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.AUDIT_LOG_VIEW,
		]);

		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail(decision),
				username: createUniqueUsername(decision),
				global_name: 'The register man',
				password: 'approving-since-1999',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();

		await createBuilder(harness, admin.token)
			.post(`/admin/instance-config/pending-registrations/${decision}`)
			.header('X-Audit-Log-Reason', 'Registration review')
			.body({user_id: pending.user_id})
			.execute();

		const result = await createBuilder<AuditLogsResponse>(harness, admin.token)
			.post('/admin/audit-logs')
			.body({target_type: 'user', target_id: pending.user_id})
			.execute();

		expect(result.logs).toEqual([
			expect.objectContaining({
				admin_user_id: admin.userId,
				target_type: 'user',
				target_id: pending.user_id,
				action,
				audit_log_reason: 'Registration review',
			}),
		]);
	});
});
