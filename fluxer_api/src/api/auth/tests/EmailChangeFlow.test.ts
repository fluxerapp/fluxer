// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	findLastTestEmail,
	generateTotpCode,
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
	has_ever_purchased: boolean;
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

async function unclaimAccount(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilderWithoutAuth(harness).post(`/test/users/${userId}/unclaim`).body(null).expect(200).execute();
}

describe('Email change flow', () => {
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
	it('uses ticketed dual-code flow with sudo and proof token', async () => {
		const account = await createTestAccount(harness);
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
			expect(startResp.original_proof).toBeDefined();
			originalProof = startResp.original_proof!;
		}
		const newEmail = `integration-new-${Date.now()}@example.com`;
		const newReq = await requestNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmail,
			originalProof,
			account.password,
		);
		expect(newReq.new_email).toBe(newEmail);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const newCode = newEmailData!.metadata!.code!;
		const token = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newCode,
			originalProof,
			account.password,
		);
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: token,
				password: account.password,
			})
			.execute();
		expect(updated.email).toBe(newEmail);
	});
	it('rejects direct email field update', async () => {
		const account = await createTestAccount(harness);
		const newEmail = `integration-direct-${Date.now()}@example.com`;
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				email: newEmail,
				password: account.password,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('request-new fails without original_proof', async () => {
		const account = await createTestAccount(harness);
		const startResp = await startEmailChange(harness, account, account.password);
		const newEmail = `integration-no-proof-${Date.now()}@example.com`;
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/request-new')
			.body({
				ticket: startResp.ticket,
				new_email: newEmail,
				original_proof: 'invalid-proof-token',
				password: account.password,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('verify-new fails without original_proof', async () => {
		const account = await createTestAccount(harness);
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
		const newEmail = `integration-verify-no-proof-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const newCode = newEmailData!.metadata!.code!;
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/verify-new')
			.body({
				ticket: startResp.ticket,
				code: newCode,
				original_proof: 'invalid-proof-token',
				password: account.password,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('returns original_proof from start when require_original is false', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		const startResp = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.execute();
		expect(startResp.require_original).toBe(false);
		expect(startResp.original_proof).toBeDefined();
		expect(startResp.original_proof!.length).toBeGreaterThan(0);
	});
	it('verify-original returns original_proof for verified email accounts', async () => {
		const account = await createTestAccount(harness);
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
			expect(originalProof.length).toBeGreaterThan(0);
		} else {
			expect(startResp.original_proof).toBeDefined();
			expect(startResp.original_proof!.length).toBeGreaterThan(0);
			originalProof = startResp.original_proof!;
		}
		const newEmail = `integration-verify-flow-${Date.now()}@example.com`;
		const newReq = await requestNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmail,
			originalProof,
			account.password,
		);
		expect(newReq.new_email).toBe(newEmail);
	});
	it('keeps email_token valid when another account grabs the address before final apply', async () => {
		const account = await createTestAccount(harness);
		const startResp = await startEmailChange(harness, account, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		expect(startResp.require_original).toBe(true);
		expect(originalEmail?.metadata?.code).toBeDefined();
		const originalProof = await verifyOriginalEmailChange(
			harness,
			account,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-race-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const emailToken = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const conflictingAccount = await createTestAccount(harness, {email: newEmail});
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: emailToken,
				password: account.password,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${conflictingAccount.userId}/set-contact-info`)
			.body({email: null})
			.expect(200)
			.execute();
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: emailToken,
				password: account.password,
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
	});
	it('applies email changes for users who have ever purchased and syncs their Stripe customer', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/premium`)
			.body({has_ever_purchased: true, stripe_customer_id: 'cus_email_change_sync'})
			.expect(200)
			.execute();
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		const startResp = await startEmailChange(harness, account, account.password);
		let originalProof: string;
		if (startResp.require_original) {
			const emails = await listTestEmails(harness, {recipient: account.email});
			const originalEmail = findLastTestEmail(emails, 'email_change_original');
			expect(originalEmail?.metadata?.code).toBeDefined();
			originalProof = await verifyOriginalEmailChange(
				harness,
				account,
				startResp.ticket,
				originalEmail!.metadata!.code!,
				account.password,
			);
		} else {
			originalProof = startResp.original_proof!;
		}
		const newEmail = `integration-purchased-${Date.now()}@trusted.beauty`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const token = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: token,
				password: account.password,
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.has_ever_purchased).toBe(true);
		expect(addJob).toHaveBeenCalledWith('syncStripeCustomerEmail', {userId: account.userId});
	});
	it('applies ordinary claimed email changes', async () => {
		const account = await createTestAccount(harness);
		const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
		const startResp = await startEmailChange(harness, account, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		expect(startResp.require_original).toBe(true);
		expect(originalEmail?.metadata?.code).toBeDefined();
		const originalProof = await verifyOriginalEmailChange(
			harness,
			account,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-plain-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const token = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: token,
				password: account.password,
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.verified).toBe(true);
		expect(addJob).not.toHaveBeenCalledWith('syncStripeCustomerEmail', expect.anything());
	});
	it('requires MFA (not password) for email_token apply when user has TOTP enabled', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code: generateTotpCode(secret),
				password: account.password,
			})
			.expect(200)
			.execute();
		const login = await loginUser(harness, {email: account.email, password: account.password});
		if (!('mfa' in login)) {
			throw new Error('Expected MFA login challenge after enabling TOTP');
		}
		const mfaLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({code: generateTotpCode(secret), ticket: login.ticket})
			.execute();
		const mfaAccount: TestAccount = {...account, token: mfaLogin.token};
		const startResp = await startEmailChange(harness, mfaAccount, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		expect(startResp.require_original).toBe(true);
		expect(originalEmail?.metadata?.code).toBeDefined();
		const originalProof = await verifyOriginalEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-mfa-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, mfaAccount, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const emailToken = await verifyNewEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const noSudoResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, mfaAccount.token)
			.patch('/users/@me')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(true);
		expect(noSudoResp.methods).toEqual({totp: true, webauthn: false, backup_codes: true});
		const passwordOnlyResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, mfaAccount.token)
			.patch('/users/@me')
			.body({email_token: emailToken, password: account.password})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(passwordOnlyResp.has_mfa).toBe(true);
		expect(passwordOnlyResp.methods).toEqual({totp: true, webauthn: false, backup_codes: true});
		const updated = await createBuilder<UserPrivateResponse>(harness, mfaAccount.token)
			.patch('/users/@me')
			.body({
				email_token: emailToken,
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.mfa_enabled).toBe(true);
	});
	it('dedicated apply endpoint: MFA users see has_mfa + methods and can succeed with TOTP', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code: generateTotpCode(secret),
				password: account.password,
			})
			.expect(200)
			.execute();
		const login = await loginUser(harness, {email: account.email, password: account.password});
		if (!('mfa' in login)) {
			throw new Error('Expected MFA login challenge after enabling TOTP');
		}
		const mfaLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({code: generateTotpCode(secret), ticket: login.ticket})
			.execute();
		const mfaAccount: TestAccount = {...account, token: mfaLogin.token};
		const startResp = await startEmailChange(harness, mfaAccount, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		const originalProof = await verifyOriginalEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-apply-mfa-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, mfaAccount, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		const emailToken = await verifyNewEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const noSudoResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(true);
		expect(noSudoResp.methods).toEqual({totp: true, webauthn: false, backup_codes: true});
		const passwordOnlyResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken, password: account.password})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(passwordOnlyResp.has_mfa).toBe(true);
		expect(passwordOnlyResp.methods).toEqual({totp: true, webauthn: false, backup_codes: true});
		const updated = await createBuilder<UserPrivateResponse>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({
				email_token: emailToken,
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.mfa_enabled).toBe(true);
	});
	it('dedicated apply endpoint succeeds with password for non-MFA users', async () => {
		const account = await createTestAccount(harness);
		const startResp = await startEmailChange(harness, account, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		const originalProof = await verifyOriginalEmailChange(
			harness,
			account,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-apply-nomfa-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		const emailToken = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const noSudoResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, account.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(false);
		expect(noSudoResp.methods).toEqual({totp: false, webauthn: false, backup_codes: false});
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken, password: account.password})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
	});
	it('claims clean unclaimed accounts', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		const startResp = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.expect(200)
			.execute();
		expect(startResp.require_original).toBe(false);
		expect(startResp.original_proof).toBeDefined();
		const originalProof = startResp.original_proof!;
		const newEmail = `integration-claim-clean-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, account, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const token = await verifyNewEmailChange(
			harness,
			account,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.patch('/users/@me')
			.body({
				email_token: token,
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.verified).toBe(true);
	});
	it('e2e: reporter scenario — MFA user with TOTP completes "Use Different Email" recovery without sudo loop', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code: generateTotpCode(secret),
				password: account.password,
			})
			.expect(200)
			.execute();
		const login = await loginUser(harness, {email: account.email, password: account.password});
		if (!('mfa' in login)) {
			throw new Error('Expected MFA login challenge after enabling TOTP');
		}
		const mfaLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({code: generateTotpCode(secret), ticket: login.ticket})
			.execute();
		const mfaAccount: TestAccount = {...account, token: mfaLogin.token};
		const startResp = await startEmailChange(harness, mfaAccount, account.password);
		expect(startResp.require_original).toBe(true);
		const originalEmails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(originalEmails, 'email_change_original');
		expect(originalEmail?.metadata?.code).toBeDefined();
		const originalProof = await verifyOriginalEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-e2e-reporter-${Date.now()}@example.com`;
		await requestNewEmailChange(harness, mfaAccount, startResp.ticket, newEmail, originalProof, account.password);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const emailToken = await verifyNewEmailChange(
			harness,
			mfaAccount,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const discovery = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
				backup_codes?: boolean;
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(discovery.has_mfa).toBe(true);
		expect(discovery.methods).toEqual({totp: true, webauthn: false, backup_codes: true});
		const applied = await createBuilder<UserPrivateResponse>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({
				email_token: emailToken,
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(200)
			.execute();
		expect(applied.email).toBe(newEmail);
		expect(applied.mfa_enabled).toBe(true);
		const me = await createBuilder<UserPrivateResponse>(harness, mfaAccount.token)
			.get('/users/@me')
			.expect(200)
			.execute();
		expect(me.email).toBe(newEmail);
		expect(me.mfa_enabled).toBe(true);
	});
});
