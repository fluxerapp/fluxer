// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	loginAccount,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ValidationErrorResponse {
	code: string;
	errors?: Array<{
		path?: string;
		code?: string;
	}>;
}

describe('Password change invalidates sessions', () => {
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
	it('invalidates all other sessions when password is changed', async () => {
		const account = await createTestAccount(harness);
		const session2 = await loginAccount(harness, account);
		const session3 = await loginAccount(harness, account);
		await createBuilder(harness, account.token).get('/users/@me').execute();
		await createBuilder(harness, session2.token).get('/users/@me').execute();
		await createBuilder(harness, session3.token).get('/users/@me').execute();
		const newPassword = `new-password-${Date.now()}`;
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				password: account.password,
				new_password: newPassword,
			})
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		await createBuilder(harness, session2.token).get('/users/@me').expect(401).execute();
		await createBuilder(harness, session3.token).get('/users/@me').expect(401).execute();
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
			.expect(400)
			.execute();
	});
	it('requires current password when changing password', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<ValidationErrorResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				new_password: `new-password-${Date.now()}`,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(response.errors?.some((error) => error.path === 'password' && error.code === 'PASSWORD_NOT_SET')).toBe(true);
	});
	it('invalidates all sessions after password reset', async () => {
		const account = await createTestAccount(harness);
		const session2 = await loginAccount(harness, account);
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const resetEmail = findLastTestEmail(emails, 'password_reset');
		expect(resetEmail?.metadata?.token).toBeDefined();
		const token = resetEmail!.metadata!.token!;
		const newPassword = `reset-password-${Date.now()}`;
		await createBuilderWithoutAuth(harness).post('/auth/reset').body({token, password: newPassword}).execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		await createBuilder(harness, session2.token).get('/users/@me').expect(401).execute();
		const login = await loginUser(harness, {email: account.email, password: newPassword});
		if ('mfa' in login && login.mfa) {
			throw new Error('Expected non-MFA login');
		}
		const nonMfaLogin = login as {
			user_id: string;
			token: string;
		};
		expect(nonMfaLogin.token.length).toBeGreaterThan(0);
	});
});
