// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	findLastTestEmail,
	type LoginSuccessResponse,
	listTestEmails,
	type TestAccount,
	type TestEmailRecord,
	totpCodeNow,
	unclaimAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createWebAuthnDevice,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnAuthenticationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface MfaRequiredResponse {
	mfa: true;
	ticket: string;
	allowed_methods: Array<string>;
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
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

async function claimEmailWithoutPassword(harness: ApiTestHarness, account: TestAccount): Promise<TestAccount> {
	await unclaimAccount(harness, account.userId);
	const start = await createBuilder<{ticket: string; original_proof?: string}>(harness, account.token)
		.post('/users/@me/email-change/start')
		.body({})
		.execute();
	const email = createUniqueEmail('passwordless-reset');
	await createBuilder(harness, account.token)
		.post('/users/@me/email-change/request-new')
		.body({ticket: start.ticket, new_email: email, original_proof: start.original_proof})
		.execute();
	const newEmail = await waitForEmail(harness, 'email_change_new', email);
	const verify = await createBuilder<{email_token: string}>(harness, account.token)
		.post('/users/@me/email-change/verify-new')
		.body({ticket: start.ticket, code: newEmail.metadata['code'], original_proof: start.original_proof})
		.execute();
	await createBuilder(harness, account.token).patch('/users/@me').body({email_token: verify.email_token}).execute();
	return {...account, email};
}

async function requestPasswordReset(harness: ApiTestHarness, email: string): Promise<string> {
	await clearTestEmails(harness);
	await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email}).expect(204).execute();
	const mail = await waitForEmail(harness, 'password_reset', email);
	const token = mail.metadata['token'];
	expect(token).toBeDefined();
	return token!;
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
		expect(resetResp.totp).toBe(true);
		expect(resetResp.webauthn).toBe(false);
		expect(resetResp.backup_codes).toBe(true);
		expect(resetResp.allowed_methods).toEqual(['totp', 'backup_codes']);
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
		expect(login.totp).toBe(true);
		expect(login.webauthn).toBe(false);
	});
	it('returns a session after password reset for a passkey user who left two-factor off', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await clearTestEmails(harness);
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
		const email = await waitForEmail(harness, 'password_reset', account.email);
		const token = email.metadata['token'];
		expect(token).toBeDefined();
		const resetResp = await createBuilderWithoutAuth<LoginSuccessResponse | MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token, password: 'new-strong-password-123'})
			.execute();
		expect('mfa' in resetResp).toBe(false);
		expect((resetResp as LoginSuccessResponse).token).toBeTruthy();
	});
	it('returns an MFA ticket after password reset for a passkey user who turned two-factor on', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		await clearTestEmails(harness);
		await createBuilderWithoutAuth(harness).post('/auth/forgot').body({email: account.email}).expect(204).execute();
		const email = await waitForEmail(harness, 'password_reset', account.email);
		const token = email.metadata['token'];
		expect(token).toBeDefined();
		const resetResp = await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token, password: 'new-strong-password-123'})
			.execute();
		expect(resetResp.mfa).toBe(true);
		expect(resetResp.totp).toBe(false);
		expect(resetResp.webauthn).toBe(true);
		expect(resetResp.allowed_methods).toContain('webauthn');
		const mfaOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/login/mfa/webauthn/authentication-options')
			.body({ticket: resetResp.ticket})
			.execute();
		if (mfaOptions.rpId) {
			device.rpId = mfaOptions.rpId;
		}
		const mfaResp = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/webauthn')
			.body({
				response: createAuthenticationResponse(device, mfaOptions),
				challenge: mfaOptions.challenge,
				ticket: resetResp.ticket,
			})
			.execute();
		expect(mfaResp.token).toBeDefined();
	});
	it('returns an MFA ticket after password reset for an account with no password that holds a passkey', async () => {
		const base = await createTestAccount(harness);
		const account = await claimEmailWithoutPassword(harness, base);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({}));
		const token = await requestPasswordReset(harness, account.email);
		const resetResp = await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token, password: 'new-strong-password-123'})
			.execute();
		expect(resetResp.mfa).toBe(true);
		expect(resetResp.totp).toBe(false);
		expect(resetResp.webauthn).toBe(true);
		expect(resetResp.allowed_methods).toContain('webauthn');
		const mfaOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/login/mfa/webauthn/authentication-options')
			.body({ticket: resetResp.ticket})
			.execute();
		if (mfaOptions.rpId) {
			device.rpId = mfaOptions.rpId;
		}
		const mfaResp = await createBuilderWithoutAuth<LoginSuccessResponse>(harness)
			.post('/auth/login/mfa/webauthn')
			.body({
				response: createAuthenticationResponse(device, mfaOptions),
				challenge: mfaOptions.challenge,
				ticket: resetResp.ticket,
			})
			.execute();
		expect(mfaResp.token).toBeTruthy();
		const me = await createBuilder<{id: string; authenticator_types: Array<number>}>(harness, mfaResp.token)
			.get('/users/@me')
			.execute();
		expect(me.id).toBe(account.userId);
		expect(me.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	});
	it('keeps demanding the passkey on a second password reset for an account that started with no password', async () => {
		const base = await createTestAccount(harness);
		const account = await claimEmailWithoutPassword(harness, base);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({}));
		const firstToken = await requestPasswordReset(harness, account.email);
		await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token: firstToken, password: 'new-strong-password-123'})
			.execute();
		const secondToken = await requestPasswordReset(harness, account.email);
		const secondReset = await createBuilderWithoutAuth<MfaRequiredResponse>(harness)
			.post('/auth/reset')
			.body({token: secondToken, password: 'another-strong-password-456'})
			.execute();
		expect(secondReset.mfa).toBe(true);
		expect(secondReset.webauthn).toBe(true);
		expect(secondReset.allowed_methods).toContain('webauthn');
	});
});
