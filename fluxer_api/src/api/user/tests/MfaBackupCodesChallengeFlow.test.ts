// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {
	type BackupCodesResponse,
	createTestAccount,
	createTotpSecret,
	findLastTestEmail,
	listTestEmails,
	type TestAccount,
	totpCodeNow,
} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

interface ChallengeStartResult {
	ticket: string;
	code_expires_at: string;
	resend_available_at: string;
}

interface ChallengeVerifyResult {
	backup_codes: Array<{
		code: string;
		consumed: boolean;
	}>;
	verification_proof: string;
}

interface ChallengeRegenerateResult {
	backup_codes: Array<{
		code: string;
		consumed: boolean;
	}>;
}

interface ValidationErrorBody {
	errors?: Array<{
		code: string;
		path: string;
	}>;
}

async function enableTotpWithSecret(
	harness: ApiTestHarness,
	account: TestAccount,
	secret: string,
): Promise<Array<string>> {
	const enabled = await createBuilder<BackupCodesResponse>(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret, code: totpCodeNow(secret), password: account.password})
		.execute();
	return enabled.backup_codes.map((backupCode) => backupCode.code);
}

async function enableTotp(harness: ApiTestHarness, account: TestAccount): Promise<Array<string>> {
	return enableTotpWithSecret(harness, account, createTotpSecret());
}

async function createTotpAccount(harness: ApiTestHarness): Promise<{
	account: TestAccount;
	backupCodes: Array<string>;
}> {
	const account = await createTestAccount(harness);
	const backupCodes = await enableTotp(harness, account);
	return {account, backupCodes};
}

async function startChallenge(harness: ApiTestHarness, token: string): Promise<ChallengeStartResult> {
	return createBuilder<ChallengeStartResult>(harness, token)
		.post('/users/@me/mfa/backup-codes/challenge')
		.body({})
		.execute();
}

async function verifyChallenge(
	harness: ApiTestHarness,
	token: string,
	ticket: string,
	code: string,
): Promise<ChallengeVerifyResult> {
	return createBuilder<ChallengeVerifyResult>(harness, token)
		.post('/users/@me/mfa/backup-codes/challenge/verify')
		.body({ticket, code})
		.execute();
}

async function regenerateWithChallenge(
	harness: ApiTestHarness,
	token: string,
	ticket: string,
	verificationProof: string,
): Promise<ChallengeRegenerateResult> {
	return createBuilder<ChallengeRegenerateResult>(harness, token)
		.post('/users/@me/mfa/backup-codes/challenge/regenerate')
		.body({ticket, verification_proof: verificationProof})
		.execute();
}

async function getChallengeCode(harness: ApiTestHarness, email: string): Promise<string> {
	const emails = await listTestEmails(harness, {recipient: email});
	const record = findLastTestEmail(emails, 'mfa_backup_codes_view');
	if (!record) {
		throw new Error(`No backup codes challenge email found for ${email}`);
	}
	return record.metadata.code;
}

describe('MfaBackupCodesChallengeFlow', () => {
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
		test('returns a ticket and emails a verification code', async () => {
			const {account} = await createTotpAccount(harness);
			const result = await startChallenge(harness, account.token);
			expect(typeof result.ticket).toBe('string');
			expect(result.ticket.length).toBeGreaterThan(0);
			expect(result.code_expires_at).toBeDefined();
			expect(result.resend_available_at).toBeDefined();
			const emails = await listTestEmails(harness, {recipient: account.email});
			const challengeEmail = findLastTestEmail(emails, 'mfa_backup_codes_view');
			expect(challengeEmail).not.toBeNull();
			expect(challengeEmail!.metadata.code).toMatch(/^[A-Z0-9]{4}-[A-Z0-9]{4}$/);
		});
		test('rejects an account without TOTP enabled', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge')
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
				.execute();
			const emails = await listTestEmails(harness, {recipient: account.email});
			expect(findLastTestEmail(emails, 'mfa_backup_codes_view')).toBeNull();
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/mfa/backup-codes/challenge')
				.body({})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('resend', () => {
		test('rejects during the cooldown period', async () => {
			const {account} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/resend')
				.body({ticket: startResult.ticket})
				.expect(429)
				.execute();
		});
		test('rejects an unknown ticket', async () => {
			const {account} = await createTotpAccount(harness);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/resend')
				.body({ticket: 'nonexistent-ticket'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/mfa/backup-codes/challenge/resend')
				.body({ticket: 'some-ticket'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('verify', () => {
		test('returns the backup codes issued when TOTP was enabled', async () => {
			const {account, backupCodes} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const result = await verifyChallenge(harness, account.token, startResult.ticket, code);
			expect(result.backup_codes.length).toBe(backupCodes.length);
			expect(result.backup_codes.map((backupCode) => backupCode.code).sort()).toEqual([...backupCodes].sort());
			for (const backupCode of result.backup_codes) {
				expect(backupCode.consumed).toBe(false);
			}
		});
		test('rejects an incorrect code and returns no backup codes', async () => {
			const {account} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: startResult.ticket, code: 'XXXX-YYYY'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as ValidationErrorBody & {
				backup_codes?: unknown;
			};
			expect(body.errors?.[0]?.code).toBe('INVALID_VERIFICATION_CODE');
			expect(body.backup_codes).toBeUndefined();
		});
		test('accepts a code with different case and without dashes', async () => {
			const {account, backupCodes} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const mangled = code.toLowerCase().replaceAll('-', '');
			expect(mangled).not.toBe(code);
			const result = await verifyChallenge(harness, account.token, startResult.ticket, mangled);
			expect(result.backup_codes.map((backupCode) => backupCode.code).sort()).toEqual([...backupCodes].sort());
		});
		test('consumes the emailed code so it cannot be verified twice', async () => {
			const {account} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const first = await verifyChallenge(harness, account.token, startResult.ticket, code);
			expect(first.verification_proof).toBeTruthy();
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: startResult.ticket, code})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('VERIFICATION_CODE_NOT_ISSUED');
		});
		test('rejects an unknown ticket', async () => {
			const {account} = await createTotpAccount(harness);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: 'nonexistent-ticket', code: 'ABCD-1234'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('rejects a ticket belonging to another user', async () => {
			const owner = await createTotpAccount(harness);
			const other = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, owner.account.token);
			const code = await getChallengeCode(harness, owner.account.email);
			const {json} = await createBuilder(harness, other.account.token)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: startResult.ticket, code})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
			const result = await verifyChallenge(harness, owner.account.token, startResult.ticket, code);
			expect(result.backup_codes.map((backupCode) => backupCode.code).sort()).toEqual([...owner.backupCodes].sort());
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: 'some-ticket', code: 'ABCD-1234'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
	describe('regenerate', () => {
		test('replaces the backup codes using the verification proof', async () => {
			const {account, backupCodes} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const verified = await verifyChallenge(harness, account.token, startResult.ticket, code);
			const regenerated = await regenerateWithChallenge(
				harness,
				account.token,
				startResult.ticket,
				verified.verification_proof,
			);
			expect(regenerated.backup_codes.length).toBe(backupCodes.length);
			const before = new Set(backupCodes);
			expect(regenerated.backup_codes.every((backupCode) => !before.has(backupCode.code))).toBe(true);
			for (const backupCode of regenerated.backup_codes) {
				expect(backupCode.consumed).toBe(false);
			}
		});
		test('rejects an incorrect verification proof', async () => {
			const {account, backupCodes} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const verified = await verifyChallenge(harness, account.token, startResult.ticket, code);
			const wrongProof = `${verified.verification_proof.slice(0, -1)}${verified.verification_proof.endsWith('a') ? 'b' : 'a'}`;
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: startResult.ticket, verification_proof: wrongProof})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_PROOF_TOKEN');
			const reverify = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/verify')
				.body({ticket: startResult.ticket, code})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((reverify.json as ValidationErrorBody).errors?.[0]?.code).toBe('VERIFICATION_CODE_NOT_ISSUED');
			const regenerated = await regenerateWithChallenge(
				harness,
				account.token,
				startResult.ticket,
				verified.verification_proof,
			);
			expect(regenerated.backup_codes.map((backupCode) => backupCode.code).sort()).not.toEqual([...backupCodes].sort());
		});
		test('rejects a ticket that has not been verified yet', async () => {
			const {account} = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, account.token);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: startResult.ticket, verification_proof: 'not-a-real-proof'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('rejects a verified ticket belonging to another user', async () => {
			const owner = await createTotpAccount(harness);
			const other = await createTotpAccount(harness);
			const startResult = await startChallenge(harness, owner.account.token);
			const code = await getChallengeCode(harness, owner.account.email);
			const verified = await verifyChallenge(harness, owner.account.token, startResult.ticket, code);
			const {json} = await createBuilder(harness, other.account.token)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: startResult.ticket, verification_proof: verified.verification_proof})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('rejects an unknown ticket', async () => {
			const {account} = await createTotpAccount(harness);
			const {json} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: 'nonexistent-ticket', verification_proof: 'whatever'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expect((json as ValidationErrorBody).errors?.[0]?.code).toBe('INVALID_OR_EXPIRED_TICKET');
		});
		test('rejects a proof once TOTP has been disabled', async () => {
			const account = await createTestAccount(harness);
			const secret = createTotpSecret();
			const backupCodes = await enableTotpWithSecret(harness, account, secret);
			const startResult = await startChallenge(harness, account.token);
			const code = await getChallengeCode(harness, account.email);
			const verified = await verifyChallenge(harness, account.token, startResult.ticket, code);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code: backupCodes[0]!, mfa_method: 'totp', mfa_code: totpCodeNow(secret)})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: startResult.ticket, verification_proof: verified.verification_proof})
				.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
				.execute();
		});
		test('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/mfa/backup-codes/challenge/regenerate')
				.body({ticket: 'some-ticket', verification_proof: 'some-proof'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
});
