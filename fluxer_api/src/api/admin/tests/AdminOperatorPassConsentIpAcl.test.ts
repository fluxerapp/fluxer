// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {AdminOperatorPassUserResponse} from '@fluxer/schema/src/domains/admin/AdminOperatorPassSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const AUDIT_REASON = 'operator pass consent access test';

async function seedPass(userId: string): Promise<void> {
	await new OperatorPassRepository().saveOperatorPass({
		user_id: createUserID(BigInt(userId)),
		granted_at: new Date('2026-11-05T10:00:00Z'),
		source: 'purchase',
		pass_count: 1,
		checkout_session_id: null,
		payment_intent_id: null,
		amount_minor: null,
		currency: null,
		paused_at: null,
		terms_version: null,
		terms_accepted_at: null,
		waiver_consent_at: new Date('2026-11-05T09:59:00Z'),
		waiver_consent_version: null,
		waiver_consent_ip: '203.0.113.7',
		waiver_consent_user_agent: 'TestBrowser/1.0',
		version: 1,
	});
}

describe('Admin Operator Pass consent details', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function readPass(acls: Array<string>) {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, acls);
		const buyer = await createTestAccount(harness);
		await seedPass(buyer.userId);
		return createBuilder<AdminOperatorPassUserResponse>(harness, admin.token)
			.get(`/admin/users/${buyer.userId}/operator-pass`)
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	test('a lookup-only admin gets no consent IP address or browser details', async () => {
		const response = await readPass([AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
		expect(response.pass?.waiver_consent_ip).toBeNull();
		expect(response.pass?.waiver_user_agent).toBeNull();
	});

	test('an admin who may see IP addresses gets the consent details', async () => {
		const response = await readPass([AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_IP]);
		expect(response.pass?.waiver_consent_ip).toBe('203.0.113.7');
		expect(response.pass?.waiver_user_agent).toBe('TestBrowser/1.0');
	});
});
