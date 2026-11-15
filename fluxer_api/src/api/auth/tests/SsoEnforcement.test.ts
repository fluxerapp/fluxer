// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createUniqueEmail,
	disableSso,
	enableSso,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {close, createStaff, suspend} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoCompleteBody {
	token?: string;
	user_id?: string;
	code?: string;
}

describe('SSO after enforcement ends', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		ipBanCache.resetCaches();
		await clearTestEmails(harness);
		staff = await createStaff(harness);
		await enableSso(harness, staff.token);
	});
	afterEach(async () => {
		await disableSso(harness, staff.token);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	function subject(label: string): string {
		return `itest-${label}-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	async function ssoSignIn(claims: Record<string, unknown>) {
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		return createBuilderWithoutAuth<SsoCompleteBody>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.executeRaw();
	}

	async function endDecision(decisionId: string): Promise<void> {
		await createBuilder(harness, staff.token)
			.post(`/admin/enforcement-decisions/${decisionId}/end`)
			.body({outcome: 'reversed'})
			.expect(200)
			.execute();
	}

	it('signs the same account in again after a closure is reversed', async () => {
		const claims = {sub: subject('closed'), email: createUniqueEmail('sso-closed')};
		const first = await ssoSignIn(claims);
		expect(first.response.status).toBe(200);
		const decisionId = await close(harness, staff, first.json.user_id!);
		const refused = await ssoSignIn(claims);
		expect(refused.response.status).toBe(403);
		await endDecision(decisionId);
		const again = await ssoSignIn(claims);
		expect(again.response.status).toBe(200);
		expect(again.json.user_id).toBe(first.json.user_id);
	});

	it('signs the same account in again after a suspension is lifted', async () => {
		const claims = {sub: subject('suspended'), email: createUniqueEmail('sso-suspended')};
		const first = await ssoSignIn(claims);
		const decisionId = await suspend(harness, staff, first.json.user_id!);
		expect((await ssoSignIn(claims)).response.status).toBe(403);
		await endDecision(decisionId);
		const again = await ssoSignIn(claims);
		expect(again.response.status).toBe(200);
		expect(again.json.user_id).toBe(first.json.user_id);
	});
});
