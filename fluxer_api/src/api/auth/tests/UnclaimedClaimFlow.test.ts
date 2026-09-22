// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	listTestEmails,
	unclaimAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface EmailChangeStartResponse {
	ticket: string;
	require_original: boolean;
	original_proof?: string;
	original_code_expires_at?: string;
	resend_available_at?: string;
}

interface EmailChangeVerifyNewResponse {
	email_token: string;
}

interface UserPrivateResponse {
	id: string;
	email: string;
	phone?: string | null;
	username: string;
	discriminator: string;
	global_name: string;
	bio: string;
	verified: boolean;
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
	password_last_changed_at?: string;
}

interface UserPatchResponse {
	email: string;
	password_last_changed_at?: string;
}

describe('Auth unclaimed claim flow', () => {
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
	it('unclaimed users cannot change username before claim', async () => {
		const account = await createTestAccount(harness);
		await clearTestEmails(harness);
		await unclaimAccount(harness, account.userId);
		const newUsername = `forbidden${Date.now()}`;
		const {response, text} = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				username: newUsername,
			})
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(text).toContain('UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN');
	});
	it('unclaimed users claim via email code and password', async () => {
		const account = await createTestAccount(harness);
		await clearTestEmails(harness);
		await unclaimAccount(harness, account.userId);
		const start = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.execute();
		expect(start.require_original).toBe(false);
		expect(start.original_proof).toBeDefined();
		expect(start.original_proof!.length).toBeGreaterThan(0);
		const originalProof = start.original_proof!;
		const newEmail = createUniqueEmail('integration-claim');
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/request-new')
			.body({
				ticket: start.ticket,
				new_email: newEmail,
				original_proof: originalProof,
			})
			.execute();
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const newCode = newEmailData!.metadata!.code!;
		const verify = await createBuilder<EmailChangeVerifyNewResponse>(harness, account.token)
			.post('/users/@me/email-change/verify-new')
			.body({
				ticket: start.ticket,
				code: newCode,
				original_proof: originalProof,
			})
			.execute();
		const newPassword = `test-password-${Date.now()}`;
		const updated = await createBuilder<UserPatchResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: verify.email_token,
				new_password: newPassword,
			})
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.password_last_changed_at).toBeDefined();
		expect(updated.password_last_changed_at!.length).toBeGreaterThan(0);
		const me = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.email).toBe(newEmail);
		expect(me.verified).toBe(true);
	});
});
