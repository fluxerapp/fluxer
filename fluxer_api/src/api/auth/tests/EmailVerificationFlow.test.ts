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
	loginAccount,
} from './AuthTestUtils';

interface UserPrivateResponse {
	verified: boolean;
	email_bounced: boolean;
}

describe('Email verification flow', () => {
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
	it('allows resending verification email and verifying email', async () => {
		const account = await createTestAccount(harness, {skipEmailVerification: true});
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.expect(200)
			.execute();
		await createBuilder(harness, account.token).post('/auth/verify/resend').body({}).expect(204).execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const verificationEmail = findLastTestEmail(emails, 'email_verification');
		expect(verificationEmail?.metadata?.token).toBeDefined();
		const token = verificationEmail!.metadata!.token!;
		await createBuilderWithoutAuth(harness).post('/auth/verify').body({token}).expect(204).execute();
		const login = await loginAccount(harness, account);
		expect(login.token).toBeDefined();
	});
	it('clears email_bounced when verifying email', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				email_bounced: true,
				email_verified: false,
			})
			.expect(200)
			.execute();
		await createBuilder(harness, account.token).post('/auth/verify/resend').body({}).expect(204).execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const verificationEmail = findLastTestEmail(emails, 'email_verification');
		expect(verificationEmail?.metadata?.token).toBeDefined();
		const token = verificationEmail!.metadata!.token!;
		await createBuilderWithoutAuth(harness).post('/auth/verify').body({token}).expect(204).execute();
		const me = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').expect(200).execute();
		expect(me.verified).toBe(true);
		expect(me.email_bounced).toBe(false);
	});
});
