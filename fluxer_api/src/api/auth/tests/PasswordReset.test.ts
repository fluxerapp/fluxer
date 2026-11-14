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
	it('invalidates every outstanding reset token once a reset completes', async () => {
		const account = await createTestAccount(harness);
		for (let i = 0; i < 2; i++) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/forgot')
				.body({email: account.email})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		const emails = await listTestEmails(harness, {recipient: account.email});
		const tokens = [
			...new Set(
				emails
					.filter((email) => email.type === 'password_reset')
					.map((email) => email.metadata?.token)
					.filter((token): token is string => typeof token === 'string'),
			),
		];
		expect(tokens).toHaveLength(2);
		const [earlierToken, laterToken] = tokens;
		const newPassword = generateUniquePassword();
		const resetResp = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/reset')
			.body({token: laterToken, password: newPassword})
			.execute();
		expect(resetResp.token.length).toBeGreaterThan(0);
		const check = await createBuilderWithoutAuth<{valid: boolean}>(harness)
			.get(`/auth/reset/${earlierToken}`)
			.execute();
		expect(check.valid).toBe(false);
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token: earlierToken, password: generateUniquePassword()})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilder(harness, resetResp.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		const login = await loginUser(harness, {email: account.email, password: newPassword});
		expect('token' in login && login.token.length > 0).toBe(true);
	});
	it('accepts a reset token for only one of several concurrent resets', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const token = findLastTestEmail(emails, 'password_reset')!.metadata!.token!;
		const results = await Promise.all(
			Array.from({length: 4}, () =>
				createBuilderWithoutAuth(harness)
					.post('/auth/reset')
					.body({token, password: generateUniquePassword()})
					.executeRaw(),
			),
		);
		const statuses = results.map(({response}) => response.status).sort();
		expect(statuses).toEqual(
			[HTTP_STATUS.OK, HTTP_STATUS.BAD_REQUEST, HTTP_STATUS.BAD_REQUEST, HTTP_STATUS.BAD_REQUEST].sort(),
		);
	});
	it('lets the inbox owner reset the password of an account holding the unconfirmed address', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const token = findLastTestEmail(emails, 'password_reset')!.metadata!.token!;
		const newPassword = generateUniquePassword();
		const reset = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/reset')
			.body({token, password: newPassword})
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const me = await createBuilder<{has_password: boolean}>(harness, reset.token).get('/users/@me').execute();
		expect(me.has_password).toBe(true);
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token, password: generateUniquePassword()})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const login = await loginUser(harness, {email: account.email, password: newPassword});
		expect('token' in login && login.token.length > 0).toBe(true);
	});
	it('refuses a new-IP approval requested before the password was reset', async () => {
		const account = await createTestAccount(harness);
		await clearTestEmails(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', '192.0.2.88')
			.body({email: account.email, password: account.password})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const approval = findLastTestEmail(await listTestEmails(harness, {recipient: account.email}), 'ip_authorization');
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({email: account.email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const token = findLastTestEmail(await listTestEmails(harness, {recipient: account.email}), 'password_reset')!
			.metadata!.token!;
		await createBuilderWithoutAuth(harness)
			.post('/auth/reset')
			.body({token, password: generateUniquePassword()})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/authorize-ip')
			.header('x-forwarded-for', '192.0.2.88')
			.body({token: approval!.metadata['token']})
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
