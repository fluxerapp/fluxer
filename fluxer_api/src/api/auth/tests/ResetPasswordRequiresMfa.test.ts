// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	type TestEmailRecord,
	totpCodeNow,
} from './AuthTestUtils';
import type { AuthMfaMethod } from '@fluxer/schema/src/domains/auth/AuthSchemas';

interface MfaRequiredResponse {
	mfa: true;
	ticket: string;
	allowed_methods: Array<AuthMfaMethod>;
}

async function waitForEmail(harness: ApiTestHarness, type: string, recipient: string): Promise<TestEmailRecord> {
	const maxAttempts = 20;
	for (let i = 0; i < maxAttempts; i++) {
		await new Promise((resolve) => setTimeout(resolve, 100));
		const emails = await listTestEmails(harness, {recipient});
		const email = findLastTestEmail(emails, type);
		if (email) {
			return email;
		}
	}
	throw new Error(`Email not found: type=${type}, recipient=${recipient}`);
}

describe('Auth reset password requires MFA', () => {
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
	it('returns MFA ticket after password reset when MFA is enabled', async () => {
		const account = await createTestAccount(harness);
		await clearTestEmails(harness);
		const secret = 'JBSWY3DPEHPK3PXP';
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.execute();
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
		const email = await waitForEmail(harness, 'password_reset', account.email);
		const token = email.metadata['token'];
		expect(token).toBeDefined();
		const newPassword = 'new-strong-password-123';
		const resetResp = await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token, password: newPassword})
			.execute();
		expect(resetResp.mfa).toBe(true);
		expect(resetResp.ticket).toBeDefined();
		expect(resetResp.allowed_methods).toEqual(['totp']);
		const mfaResp = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({
				ticket: resetResp.ticket,
				code: totpCodeNow(secret),
			})
			.execute();
		expect(mfaResp.token).toBeDefined();
		const login = await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/login')
			.body({email: account.email, password: newPassword})
			.execute();
		expect(login.mfa).toBe(true);
		expect(login.ticket).toBeDefined();
		expect(login.allowed_methods).toEqual(['totp']);
	});
});
