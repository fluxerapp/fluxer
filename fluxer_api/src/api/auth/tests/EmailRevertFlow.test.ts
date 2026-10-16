// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	loginUser,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface EmailChangeStartResponse {
	ticket: string;
	require_original: boolean;
	original_proof?: string;
	original_code_expires_at?: string;
	resend_available_at?: string;
}

interface EmailChangeVerifyOriginalResponse {
	original_proof: string;
}

interface EmailChangeRequestNewResponse {
	ticket: string;
	new_email: string;
	new_code_expires_at: string;
	resend_available_at?: string;
}

interface EmailChangeVerifyNewResponse {
	email_token: string;
}

interface UserPrivateResponse {
	id: string;
	email: string;
	username: string;
	discriminator: string;
	global_name: string;
	bio: string;
	verified: boolean;
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
	password_last_changed_at?: string;
}

interface EmailRevertResponse {
	token: string;
}

async function startEmailChange(
	harness: ApiTestHarness,
	account: TestAccount,
	password: string,
): Promise<EmailChangeStartResponse> {
	return createBuilder<EmailChangeStartResponse>(harness, account.token)
		.post('/users/@me/email-change/start')
		.body({password})
		.execute();
}

async function verifyOriginalEmailChange(
	harness: ApiTestHarness,
	account: TestAccount,
	ticket: string,
	code: string,
	password: string,
): Promise<string> {
	const resp = await createBuilder<EmailChangeVerifyOriginalResponse>(harness, account.token)
		.post('/users/@me/email-change/verify-original')
		.body({ticket, code, password})
		.execute();
	return resp.original_proof;
}

async function requestNewEmailChange(
	harness: ApiTestHarness,
	account: TestAccount,
	ticket: string,
	newEmail: string,
	originalProof: string,
	password: string,
): Promise<EmailChangeRequestNewResponse> {
	return createBuilder<EmailChangeRequestNewResponse>(harness, account.token)
		.post('/users/@me/email-change/request-new')
		.body({
			ticket,
			new_email: newEmail,
			original_proof: originalProof,
			password,
		})
		.execute();
}

async function verifyNewEmailChange(
	harness: ApiTestHarness,
	account: TestAccount,
	ticket: string,
	code: string,
	originalProof: string,
	password: string,
): Promise<string> {
	const resp = await createBuilder<EmailChangeVerifyNewResponse>(harness, account.token)
		.post('/users/@me/email-change/verify-new')
		.body({
			ticket,
			code,
			original_proof: originalProof,
			password,
		})
		.execute();
	return resp.email_token;
}

function uniquePassword(): string {
	return `Sup3r-${Date.now()}-Pass!`;
}

describe('Email revert flow', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('restores original email and clears mfa', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/premium`)
			.body({stripe_customer_id: 'cus_email_revert_sync'})
			.expect(200)
			.execute();
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		const startResp = await startEmailChange(harness, account, account.password);
		let originalProof: string;
		if (startResp.require_original) {
			const emails = await listTestEmails(harness, {recipient: account.email});
			const originalEmail = findLastTestEmail(emails, 'email_change_original');
			expect(originalEmail?.metadata?.code).toBeDefined();
			const originalCode = originalEmail!.metadata!.code!;
			originalProof = await verifyOriginalEmailChange(
				harness,
				account,
				startResp.ticket,
				originalCode,
				account.password,
			);
		} else {
			originalProof = startResp.original_proof!;
		}
		const newEmail = `integration-revert-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const newCode = newEmailData!.metadata!.code!;
		const emailToken = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newCode,
			originalProof,
			account.password,
		);
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: emailToken,
				password: account.password,
			})
			.execute();
		const revertEmails = await listTestEmails(harness, {recipient: account.email});
		const revertEmail = findLastTestEmail(revertEmails, 'email_change_revert');
		expect(revertEmail?.metadata?.token).toBeDefined();
		const revertToken = revertEmail!.metadata!.token!;
		const newPassword = uniquePassword();
		addJob.mockClear();
		const revertResp = await createBuilderWithoutAuth<EmailRevertResponse>(harness)
			.post('/auth/email-revert')
			.body({
				token: revertToken,
				password: newPassword,
			})
			.execute();
		expect(revertResp.token.length).toBeGreaterThan(0);
		expect(addJob).toHaveBeenCalledWith('syncStripeCustomerEmail', {userId: account.userId});
		expect(Object.keys(revertResp).sort()).toEqual(['token', 'user', 'user_id']);
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		const user = await createBuilder<UserPrivateResponse>(harness, revertResp.token).get('/users/@me').execute();
		expect(user.email).toBe(account.email);
		expect(user.mfa_enabled).toBe(false);
		expect(user.authenticator_types?.length ?? 0).toBe(0);
		expect(user.password_last_changed_at).toBeDefined();
		const login = await loginUser(harness, {email: account.email, password: newPassword});
		expect('mfa' in login).toBe(false);
		const nonMfaLogin = login as {
			user_id: string;
			token: string;
		};
		expect(nonMfaLogin.token.length).toBeGreaterThan(0);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: account.password,
			})
			.expect(400)
			.execute();
	});
});
