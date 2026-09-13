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
import {setInjectedRegistrationRiskEvaluator} from '@app/api/middleware/ServiceMiddleware';
import {RecommendedAction, RiskConfidence, RiskDecisionMethod, RiskLevel} from '@app/api/risk/RiskTypes';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
	phone?: string | null;
	username: string;
	discriminator: string;
	global_name: string;
	bio: string;
	verified: boolean;
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
	password_last_changed_at?: string;
	required_actions?: Array<string> | null;
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

function createForcedRiskEvaluator(onEvaluate?: () => void) {
	return {
		async evaluate() {
			onEvaluate?.();
			return {
				assessment: {
					suspicious: true,
					level: RiskLevel.High,
					confidence: RiskConfidence.High,
					riskScore: 70,
					reasoning: 'forced test risk result',
					recommendedAction: RecommendedAction.RequireOutboundPhone,
					method: RiskDecisionMethod.Deterministic,
					modelUsed: 'test',
					rounds: 0,
					elapsedMs: 0,
					signals: {},
				},
				level: RiskLevel.High,
				recommendedAction: RecommendedAction.RequireOutboundPhone,
			};
		},
	};
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
	it('allows suspicious accounts to complete email change and clears email-related flags', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL', 'REQUIRE_VERIFIED_PHONE'],
			})
			.expect(200)
			.execute();
		const login = await loginUser(harness, {email: account.email, password: account.password});
		if ('mfa' in login) {
			throw new Error('Expected non-MFA login');
		}
		const suspiciousAccount: TestAccount = {...account, token: login.token};
		const startResp = await startEmailChange(harness, suspiciousAccount, account.password);
		const emails = await listTestEmails(harness, {recipient: account.email});
		const originalEmail = findLastTestEmail(emails, 'email_change_original');
		expect(startResp.require_original).toBe(true);
		expect(originalEmail?.metadata?.code).toBeDefined();
		const originalProof = await verifyOriginalEmailChange(
			harness,
			suspiciousAccount,
			startResp.ticket,
			originalEmail!.metadata!.code!,
			account.password,
		);
		const newEmail = `integration-suspicious-${Date.now()}@example.com`;
		await requestNewEmailChange(
			harness,
			suspiciousAccount,
			startResp.ticket,
			newEmail,
			originalProof,
			account.password,
		);
		const newEmails = await listTestEmails(harness, {recipient: newEmail});
		const newEmailData = findLastTestEmail(newEmails, 'email_change_new');
		expect(newEmailData?.metadata?.code).toBeDefined();
		const token = await verifyNewEmailChange(
			harness,
			suspiciousAccount,
			startResp.ticket,
			newEmailData!.metadata!.code!,
			originalProof,
			account.password,
		);
		const updated = await createBuilder<UserPrivateResponse>(harness, suspiciousAccount.token)
			.patch('/users/@me')
			.body({
				email_token: token,
				password: account.password,
			})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
		expect(updated.verified).toBe(true);
		expect(updated.required_actions).toContain('REQUIRE_VERIFIED_PHONE');
		expect(updated.required_actions).not.toContain('REQUIRE_REVERIFIED_EMAIL');
	});
	it('does not add suspicion on email change for users who have ever purchased', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/premium`)
			.body({has_ever_purchased: true})
			.expect(200)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			})
			.expect(200)
			.execute();
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
		expect(updated.required_actions).not.toContain('REQUIRE_REVERIFIED_EMAIL');
		expect(updated.required_actions).not.toContain('REQUIRE_VERIFIED_PHONE');
	});
	it('re-evaluates plus-tagged email changes on claimed accounts', async () => {
		const account = await createTestAccount(harness);
		let evaluateCalls = 0;
		setInjectedRegistrationRiskEvaluator(
			createForcedRiskEvaluator(() => {
				evaluateCalls += 1;
			}),
		);
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
		const newEmail = `integration-plus+${Date.now()}@example.com`;
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
		expect(evaluateCalls).toBe(1);
		expect(updated.required_actions).toContain('REQUIRE_VERIFIED_PHONE');
	});
	it('does not re-evaluate ordinary claimed email changes', async () => {
		const account = await createTestAccount(harness);
		let evaluateCalls = 0;
		setInjectedRegistrationRiskEvaluator(
			createForcedRiskEvaluator(() => {
				evaluateCalls += 1;
			}),
		);
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
		expect(evaluateCalls).toBe(0);
		expect(updated.required_actions ?? []).not.toContain('REQUIRE_VERIFIED_PHONE');
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
			};
		}>(harness, mfaAccount.token)
			.patch('/users/@me')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(true);
		expect(noSudoResp.methods).toEqual({totp: true, webauthn: false});
		const passwordOnlyResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
			};
		}>(harness, mfaAccount.token)
			.patch('/users/@me')
			.body({email_token: emailToken, password: account.password})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(passwordOnlyResp.has_mfa).toBe(true);
		expect(passwordOnlyResp.methods).toEqual({totp: true, webauthn: false});
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
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(true);
		expect(noSudoResp.methods).toEqual({totp: true, webauthn: false});
		const passwordOnlyResp = await createBuilder<{
			code: string;
			has_mfa?: boolean;
			methods?: {
				totp?: boolean;
				webauthn?: boolean;
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken, password: account.password})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(passwordOnlyResp.has_mfa).toBe(true);
		expect(passwordOnlyResp.methods).toEqual({totp: true, webauthn: false});
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
			};
		}>(harness, account.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(noSudoResp.has_mfa).toBe(false);
		expect(noSudoResp.methods).toEqual({totp: false, webauthn: false});
		const updated = await createBuilder<UserPrivateResponse>(harness, account.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken, password: account.password})
			.expect(200)
			.execute();
		expect(updated.email).toBe(newEmail);
	});
	it('does not re-evaluate clean unclaimed accounts when they are claimed', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		let evaluateCalls = 0;
		setInjectedRegistrationRiskEvaluator(
			createForcedRiskEvaluator(() => {
				evaluateCalls += 1;
			}),
		);
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
		expect(evaluateCalls).toBe(0);
		expect(updated.email).toBe(newEmail);
		expect(updated.verified).toBe(true);
		expect(updated.required_actions).not.toContain('REQUIRE_VERIFIED_PHONE');
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
			};
		}>(harness, mfaAccount.token)
			.post('/users/@me/email-change/apply')
			.body({email_token: emailToken})
			.expect(403, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(discovery.has_mfa).toBe(true);
		expect(discovery.methods).toEqual({totp: true, webauthn: false});
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
