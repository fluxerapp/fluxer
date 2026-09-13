// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	loginUser,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function setUserSecurityFlags(harness: ApiTestHarness, userId: string, setFlags: Array<string>): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${userId}/security-flags`)
		.body({
			set_flags: setFlags,
		})
		.expect(200)
		.execute();
}

describe('Auth app store reviewer with other flags', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('allows login with APP_STORE_REVIEWER flag combined with other flags', async () => {
		const email = createUniqueEmail('reviewer-multi-flag');
		const username = createUniqueUsername('reviewer');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username,
			global_name: 'Multi Flag Reviewer',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await setUserSecurityFlags(harness, reg.user_id, ['APP_STORE_REVIEWER', 'STAFF']);
		const login = await loginUser(harness, {
			email,
			password,
		});
		expect('mfa' in login).toBe(false);
		if (!('mfa' in login)) {
			const nonMfaLogin = login as {
				user_id: string;
				token: string;
			};
			expect(nonMfaLogin.token).toBeTruthy();
			expect(nonMfaLogin.user_id).toBe(reg.user_id);
		}
	});
});
