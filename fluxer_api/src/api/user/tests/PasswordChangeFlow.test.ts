// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	loginUser,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface PasswordChangeStartResult {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string | null;
}

interface PasswordChangeVerifyResult {
	verification_proof: string;
}

interface PasswordChangeCompleteResult {
	token: string;
	auth_session_id_hash: string;
}

async function startPasswordChange(harness: ApiTestHarness, token: string): Promise<PasswordChangeStartResult> {
	return createBuilder<PasswordChangeStartResult>(harness, token)
		.post('/users/@me/password-change/start')
		.body({})
		.execute();
}

async function verifyPasswordChangeCode(
	harness: ApiTestHarness,
	token: string,
	ticket: string,
	code: string,
): Promise<PasswordChangeVerifyResult> {
	return createBuilder<PasswordChangeVerifyResult>(harness, token)
		.post('/users/@me/password-change/verify')
		.body({ticket, code})
		.execute();
}

async function completePasswordChange(
	harness: ApiTestHarness,
	token: string,
	ticket: string,
	verificationProof: string,
	newPassword: string,
): Promise<PasswordChangeCompleteResult> {
	return createBuilder<PasswordChangeCompleteResult>(harness, token)
		.post('/users/@me/password-change/complete')
		.body({ticket, verification_proof: verificationProof, new_password: newPassword})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function getVerificationCode(harness: ApiTestHarness, email: string): Promise<string> {
	const emails = await listTestEmails(harness, {recipient: email});
	const record = findLastTestEmail(emails, 'password_change_verification');
	if (!record) {
		throw new Error(`No password change verification email found for ${email}`);
	}
	return record.metadata.code;
}

async function runFullVerification(
	harness: ApiTestHarness,
	account: TestAccount,
): Promise<{
	ticket: string;
	verificationProof: string;
}> {
	const startResult = await startPasswordChange(harness, account.token);
	const code = await getVerificationCode(harness, account.email);
	const verifyResult = await verifyPasswordChangeCode(harness, account.token, startResult.ticket, code);
	return {ticket: startResult.ticket, verificationProof: verifyResult.verification_proof};
}

describe('PasswordChangeFlow', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	describe('start', () => {
		test('sends verification email and returns ticket', async () => {
			const account = await createTestAccount(harness);
			const result = await startPasswordChange(harness, account.token);
			expect(result.ticket).toBeDefined();
			expect(typeof result.ticket).toBe('string');
			expect(result.code_expires_at).toBeDefined();
			expect(result.resend_available_at).toBeDefined();
			const emails = await listTestEmails(harness, {recipient: account.email});
			const verificationEmail = findLastTestEmail(emails, 'password_change_verification');
			expect(verificationEmail).not.toBeNull();
			expect(verificationEmail!.metadata.code).toBeDefined();
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/password-change/start')
				.body({})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('resend', () => {
		test('rejects during cooldown period', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			await createBuilder(harness, account.token)
				.post('/users/@me/password-change/resend')
				.body({ticket: startResult.ticket})
				.expect(429)
				.execute();
		});
		test('rejects with invalid ticket', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/password-change/resend')
				.body({ticket: 'invalid-ticket-id'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/password-change/resend')
				.body({ticket: 'some-ticket'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('verify', () => {
		test('returns verification_proof with correct code', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			const code = await getVerificationCode(harness, account.email);
			const verifyResult = await verifyPasswordChangeCode(harness, account.token, startResult.ticket, code);
			expect(verifyResult.verification_proof).toBeDefined();
			expect(typeof verifyResult.verification_proof).toBe('string');
		});
		test('rejects with incorrect code', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/password-change/verify')
				.body({ticket: startResult.ticket, code: 'XXXX-YYYY'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
					path: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('INVALID_VERIFICATION_CODE');
		});
		test('rejects with invalid ticket', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/password-change/verify')
				.body({ticket: 'nonexistent-ticket', code: 'ABCD-1234'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('returns same proof on repeated verification with correct code', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			const code = await getVerificationCode(harness, account.email);
			const firstVerify = await verifyPasswordChangeCode(harness, account.token, startResult.ticket, code);
			const secondVerify = await verifyPasswordChangeCode(harness, account.token, startResult.ticket, code);
			expect(firstVerify.verification_proof).toBe(secondVerify.verification_proof);
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/password-change/verify')
				.body({ticket: 'some-ticket', code: 'ABCD-1234'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('complete', () => {
		test('succeeds with correct proof and passwords', async () => {
			const account = await createTestAccount(harness);
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			await completePasswordChange(harness, account.token, ticket, verificationProof, TEST_CREDENTIALS.ALT_PASSWORD_1);
		});
		test('succeeds when the user does not have a password set', async () => {
			const account = await createTestAccount(harness);
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			await createBuilderWithoutAuth(harness).post(`/test/users/${account.userId}/unclaim`).body({}).execute();
			await completePasswordChange(harness, account.token, ticket, verificationProof, TEST_CREDENTIALS.ALT_PASSWORD_1);
		});
		test('rejects with invalid verification_proof', async () => {
			const account = await createTestAccount(harness);
			const {ticket} = await runFullVerification(harness, account);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/password-change/complete')
				.body({
					ticket,
					verification_proof: 'fake-proof-value',
					new_password: TEST_CREDENTIALS.ALT_PASSWORD_1,
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
					path: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('INVALID_PROOF_TOKEN');
		});
		test('rejects when ticket has not been verified', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/password-change/complete')
				.body({
					ticket: startResult.ticket,
					verification_proof: 'some-proof',
					new_password: TEST_CREDENTIALS.ALT_PASSWORD_1,
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
					path: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('rejects reusing a completed ticket', async () => {
			const account = await createTestAccount(harness);
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			await completePasswordChange(harness, account.token, ticket, verificationProof, TEST_CREDENTIALS.ALT_PASSWORD_1);
			const freshLogin = await loginUser(harness, {
				email: account.email,
				password: TEST_CREDENTIALS.ALT_PASSWORD_1,
			});
			if ('mfa' in freshLogin) {
				throw new Error('Expected non-MFA login');
			}
			const newToken = (
				freshLogin as {
					token: string;
				}
			).token;
			await createBuilder(harness, newToken)
				.post('/users/@me/password-change/complete')
				.body({
					ticket,
					verification_proof: verificationProof,
					new_password: TEST_CREDENTIALS.ALT_PASSWORD_2,
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('invalidates existing sessions after password change', async () => {
			const account = await createTestAccount(harness);
			const originalToken = account.token;
			const otherLogin = await loginUser(harness, {
				email: account.email,
				password: account.password,
			});
			if ('mfa' in otherLogin) {
				throw new Error('Expected non-MFA login');
			}
			const otherToken = otherLogin.token;
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			const result = await completePasswordChange(
				harness,
				account.token,
				ticket,
				verificationProof,
				TEST_CREDENTIALS.ALT_PASSWORD_1,
			);
			const {response} = await createBuilder(harness, originalToken)
				.get('/users/@me')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.executeRaw();
			expect(response.status).toBe(HTTP_STATUS.UNAUTHORIZED);
			await createBuilder(harness, otherToken).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).executeRaw();
			expect(result.token).toBeTruthy();
			expect(result.auth_session_id_hash).toBeTruthy();
			await createBuilder(harness, result.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		});
		test('user can log in with new password after change', async () => {
			const account = await createTestAccount(harness);
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			await completePasswordChange(harness, account.token, ticket, verificationProof, TEST_CREDENTIALS.ALT_PASSWORD_1);
			const login = await loginUser(harness, {
				email: account.email,
				password: TEST_CREDENTIALS.ALT_PASSWORD_1,
			});
			expect('token' in login).toBe(true);
			expect('mfa' in login).toBe(false);
		});
		test('old password no longer works after change', async () => {
			const account = await createTestAccount(harness);
			const {ticket, verificationProof} = await runFullVerification(harness, account);
			await completePasswordChange(harness, account.token, ticket, verificationProof, TEST_CREDENTIALS.ALT_PASSWORD_1);
			await createBuilderWithoutAuth(harness)
				.post('/auth/login')
				.body({email: account.email, password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/password-change/complete')
				.body({
					ticket: 'some-ticket',
					verification_proof: 'some-proof',
					new_password: 'new-password',
				})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('full flow', () => {
		test('start, verify, and complete password change end-to-end', async () => {
			const account = await createTestAccount(harness);
			const startResult = await startPasswordChange(harness, account.token);
			expect(startResult.ticket).toBeDefined();
			expect(startResult.code_expires_at).toBeDefined();
			const code = await getVerificationCode(harness, account.email);
			expect(code).toMatch(/^[A-Z0-9]{4}-[A-Z0-9]{4}$/);
			const verifyResult = await verifyPasswordChangeCode(harness, account.token, startResult.ticket, code);
			expect(verifyResult.verification_proof).toBeDefined();
			await completePasswordChange(
				harness,
				account.token,
				startResult.ticket,
				verifyResult.verification_proof,
				TEST_CREDENTIALS.ALT_PASSWORD_1,
			);
			const login = await loginUser(harness, {
				email: account.email,
				password: TEST_CREDENTIALS.ALT_PASSWORD_1,
			});
			expect('token' in login).toBe(true);
		});
	});
});
