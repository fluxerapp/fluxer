// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	type LoginMfaResponse,
	loginUser,
	type TestAccount,
	totpCodeNow,
} from './AuthTestUtils';

interface BackupCodeEntry {
	code: string;
	consumed: boolean;
}

interface BackupCodeListResponse {
	backup_codes: Array<BackupCodeEntry>;
}

interface MfaLoginTokenResponse {
	user_id: string;
	token: string;
}

function stripBackupCode(code: string): string {
	return code.toLowerCase().replace(/[^a-z0-9]/g, '');
}

function unissuedBackupCode(codes: Array<string>): string {
	const issued = new Set(codes.map(stripBackupCode));
	for (let index = 0; index < 100; index++) {
		const candidate = `zzzz-${index.toString().padStart(4, '0')}`;
		if (!issued.has(stripBackupCode(candidate))) {
			return candidate;
		}
	}
	throw new Error('Unable to derive an unissued backup code');
}

async function enableTotp(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<Array<string>> {
	const enabled = await createBuilder<BackupCodeListResponse>(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret, code: totpCodeNow(secret), password: account.password})
		.execute();
	expect(enabled.backup_codes.length).toBeGreaterThan(0);
	return enabled.backup_codes.map((entry) => entry.code);
}

async function startMfaLogin(harness: ApiTestHarness, account: TestAccount): Promise<string> {
	const login = (await loginUser(harness, {email: account.email, password: account.password})) as LoginMfaResponse;
	expect(login.mfa).toBe(true);
	expect(login.ticket).toBeTruthy();
	return login.ticket;
}

async function completeMfaLogin(harness: ApiTestHarness, ticket: string, code: string): Promise<string> {
	const result = await createBuilderWithoutAuth<MfaLoginTokenResponse>(harness)
		.post('/auth/login/mfa/totp')
		.body({ticket, code})
		.execute();
	expect(result.token).toBeTruthy();
	return result.token;
}

async function expectMfaLoginRejected(harness: ApiTestHarness, ticket: string, code: string): Promise<void> {
	const result = await createBuilderWithoutAuth<{
		code: string;
		token?: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({ticket, code})
		.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
		.execute();
	expect(result.token).toBeUndefined();
}

async function assertSessionBelongsTo(harness: ApiTestHarness, token: string, userId: string): Promise<void> {
	const me = await createBuilder<{
		id: string;
	}>(harness, token)
		.get('/users/@me')
		.execute();
	expect(me.id).toBe(userId);
}

async function listBackupCodes(
	harness: ApiTestHarness,
	account: TestAccount,
	secret: string,
): Promise<Array<BackupCodeEntry>> {
	const listed = await createBuilder<BackupCodeListResponse>(harness, account.token)
		.post('/users/@me/mfa/backup-codes')
		.body({mfa_method: 'totp', mfa_code: totpCodeNow(secret), regenerate: false})
		.execute();
	return listed.backup_codes;
}

describe('MFA backup code login', () => {
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
	it('accepts a backup code exactly as issued', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		expect(codes[0]!).toMatch(/^[a-z0-9]{4}-[a-z0-9]{4}$/);
		const ticket = await startMfaLogin(harness, account);
		const token = await completeMfaLogin(harness, ticket, codes[0]!);
		await assertSessionBelongsTo(harness, token, account.userId);
	});
	it('accepts a backup code with the dash stripped as the web client sends it', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		const ticket = await startMfaLogin(harness, account);
		const token = await completeMfaLogin(harness, ticket, codes[0]!.replace('-', ''));
		await assertSessionBelongsTo(harness, token, account.userId);
		const listed = await listBackupCodes(harness, account, secret);
		expect(listed).toHaveLength(codes.length);
		const consumed = listed.filter((entry) => entry.consumed);
		expect(consumed).toHaveLength(1);
		expect(consumed[0]!.code).toBe(codes[0]!);
	});
	it('accepts uppercased and space padded backup codes', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		const upperTicket = await startMfaLogin(harness, account);
		const upperToken = await completeMfaLogin(harness, upperTicket, codes[0]!.toUpperCase());
		await assertSessionBelongsTo(harness, upperToken, account.userId);
		const spacedTicket = await startMfaLogin(harness, account);
		const spacedToken = await completeMfaLogin(harness, spacedTicket, `  ${codes[1]!.replace('-', ' ')}  `);
		await assertSessionBelongsTo(harness, spacedToken, account.userId);
		const listed = await listBackupCodes(harness, account, secret);
		expect(listed).toHaveLength(codes.length);
		const consumed = listed.filter((entry) => entry.consumed).map((entry) => entry.code);
		expect(consumed.sort()).toEqual([codes[0]!, codes[1]!].sort());
	});
	it('marks the issued row consumed and rejects every spelling of a used code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		const issued = codes[0]!;
		const firstTicket = await startMfaLogin(harness, account);
		const token = await completeMfaLogin(harness, firstTicket, issued.replace('-', ''));
		await assertSessionBelongsTo(harness, token, account.userId);
		const secondTicket = await startMfaLogin(harness, account);
		await expectMfaLoginRejected(harness, secondTicket, issued);
		await expectMfaLoginRejected(harness, secondTicket, issued.replace('-', ''));
		await expectMfaLoginRejected(harness, secondTicket, issued.toUpperCase());
		const listed = await listBackupCodes(harness, account, secret);
		expect(listed).toHaveLength(codes.length);
		expect(listed.map((entry) => entry.code).sort()).toEqual([...codes].sort());
		const consumed = listed.filter((entry) => entry.consumed);
		expect(consumed).toHaveLength(1);
		expect(consumed[0]!.code).toBe(issued);
	});
	it('rejects empty and whitespace only backup codes', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		const ticket = await startMfaLogin(harness, account);
		await expectMfaLoginRejected(harness, ticket, '');
		await expectMfaLoginRejected(harness, ticket, '   ');
		await expectMfaLoginRejected(harness, ticket, '-');
		const listed = await listBackupCodes(harness, account, secret);
		expect(listed).toHaveLength(codes.length);
		expect(listed.filter((entry) => entry.consumed)).toHaveLength(0);
		const token = await completeMfaLogin(harness, ticket, codes[0]!);
		await assertSessionBelongsTo(harness, token, account.userId);
	});
	it('rejects a well formed backup code that was never issued', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const codes = await enableTotp(harness, account, secret);
		const unissued = unissuedBackupCode(codes);
		const ticket = await startMfaLogin(harness, account);
		await expectMfaLoginRejected(harness, ticket, unissued);
		await expectMfaLoginRejected(harness, ticket, stripBackupCode(unissued));
		const listed = await listBackupCodes(harness, account, secret);
		expect(listed).toHaveLength(codes.length);
		expect(listed.filter((entry) => entry.consumed)).toHaveLength(0);
	});
});
