// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	type LoginSuccessResponse,
	listTestEmails,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {generateUniquePassword, HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Password reset flow', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('allows forgot and reset password flow with token reuse rejection and session invalidation', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const resetEmail = findLastTestEmail(emails, 'password_reset');
		expect(resetEmail?.metadata?.token).toBeDefined();
		const token = resetEmail!.metadata!.token!;
		const newPassword = generateUniquePassword();
		const resetResp = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/reset')
			.body({token, password: newPassword})
			.execute();
		expect(resetResp.token.length).toBeGreaterThan(0);
		const login = await loginUser(harness, {email: account.email, password: newPassword});
		if ('mfa' in login && login.mfa) {
			throw new Error('Expected non-MFA login');
		}
		const nonMfaLogin = login as {
			user_id: string;
			token: string;
		};
		expect(nonMfaLogin.token.length).toBeGreaterThan(0);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const anotherPassword = generateUniquePassword();
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token, password: anotherPassword})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects invalid reset token', async () => {
		await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token: 'invalid-reset-token', password: generateUniquePassword()})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
