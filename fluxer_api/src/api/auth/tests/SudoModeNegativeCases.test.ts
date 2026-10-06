// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {createAuthHarness, createTestAccount, loginUser, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {TotpGenerator} from '@app/api/utils/TotpGenerator';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const SUDO_MODE_HEADER = 'X-Fluxer-Sudo-Mode-JWT';

interface ErrorResponse {
	code: string;
	message: string;
}

interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
		consumed: boolean;
	}>;
}

function generateTotpSecret(): string {
	const buffer = randomBytes(20);
	const base32Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
	let result = '';
	for (let i = 0; i < buffer.length; i += 5) {
		const bytes = [buffer[i]!, buffer[i + 1]!, buffer[i + 2]!, buffer[i + 3]!, buffer[i + 4]!];
		const n = (bytes[0]! << 24) | (bytes[1]! << 16) | (bytes[2]! << 8) | bytes[3]!;
		const indices = [
			(n >> 3) & 0x1f,
			((n >> 11) | ((bytes[4]! << 4) & 0xf)) & 0x1f,
			((n >> 19) | ((bytes[4]! << 2) & 0x3c)) & 0x1f,
			(bytes[4]! >> 1) & 0x1f,
		];
		result += base32Chars[indices[0]!];
		result += base32Chars[indices[1]!];
		result += base32Chars[indices[2]!];
		result += base32Chars[indices[3]!];
	}
	return result;
}

async function generateTotpCode(secret: string): Promise<string> {
	const totp = new TotpGenerator(secret);
	const codes = await totp.generateTotp();
	return codes[0]!;
}

async function enableTotpForAccount(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<void> {
	const code = await generateTotpCode(secret);
	await createBuilder(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({
			secret,
			code,
			password: account.password,
		})
		.execute();
}

async function loginWithTotp(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<TestAccount> {
	const login = await loginUser(harness, {email: account.email, password: account.password});
	if (!('mfa' in login)) {
		throw new Error('Expected MFA login');
	}
	const mfaLoginResp = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({
			ticket: login.ticket,
			code: await generateTotpCode(secret),
		})
		.execute();
	return {...account, token: mfaLoginResp.token};
}

async function getSudoTokenViaMfa(harness: ApiTestHarness, token: string, secret: string): Promise<string> {
	const {response} = await createBuilder<BackupCodesResponse>(harness, token)
		.post('/users/@me/mfa/backup-codes')
		.body({
			mfa_method: 'totp',
			mfa_code: await generateTotpCode(secret),
			regenerate: false,
		})
		.executeWithResponse();
	const sudoToken = response.headers.get(SUDO_MODE_HEADER);
	if (!sudoToken) {
		throw new Error('No sudo token returned');
	}
	return sudoToken;
}

describe('Sudo mode negative cases', () => {
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
	describe('invalid sudo token rejected', () => {
		it('rejects malformed sudo token', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const invalidTokens = [
				'invalid-token',
				'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.invalid.signature',
				'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoibm90LXN1ZG8ifQ.fakesignature',
			];
			for (const invalidToken of invalidTokens) {
				const {json: errResp} = await createBuilder<ErrorResponse>(harness, loggedIn.token)
					.post('/users/@me/disable')
					.header(SUDO_MODE_HEADER, invalidToken)
					.body({})
					.expect(HTTP_STATUS.FORBIDDEN)
					.executeWithResponse();
				expect(errResp.code).toBe('SUDO_MODE_REQUIRED');
			}
		});
		it('rejects empty sudo token header', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const {json: errResp} = await createBuilder<ErrorResponse>(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN)
				.executeWithResponse();
			expect(errResp.code).toBe('SUDO_MODE_REQUIRED');
		});
	});
	describe('wrong password rejected when verifying sudo', () => {
		it('rejects wrong password for password-only user', async () => {
			const account = await createTestAccount(harness);
			const wrongPasswords = ['wrong-password', `${account.password}extra`, 'password123'];
			for (const wrongPassword of wrongPasswords) {
				await createBuilder(harness, account.token)
					.post('/users/@me/disable')
					.body({
						password: wrongPassword,
					})
					.expect(HTTP_STATUS.BAD_REQUEST)
					.execute();
			}
		});
		it('requires password for password-only user - returns 403 without password', async () => {
			const account = await createTestAccount(harness);
			const {json: errResp} = await createBuilder<ErrorResponse>(harness, account.token)
				.post('/users/@me/disable')
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN)
				.executeWithResponse();
			expect(errResp.code).toBe('SUDO_MODE_REQUIRED');
		});
	});
	describe('wrong MFA code rejected', () => {
		it('rejects incorrect TOTP codes', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const wrongCodes = ['000000', '123456', '999999', '12345', '1234567', 'abcdef'];
			for (const wrongCode of wrongCodes) {
				await createBuilder(harness, loggedIn.token)
					.post('/users/@me/disable')
					.body({
						mfa_method: 'totp',
						mfa_code: wrongCode,
					})
					.expect(HTTP_STATUS.BAD_REQUEST)
					.execute();
			}
		});
	});
	describe('sudo token for wrong user rejected', () => {
		it('rejects sudo token from different user', async () => {
			const account1 = await createTestAccount(harness);
			const secret1 = generateTotpSecret();
			await enableTotpForAccount(harness, account1, secret1);
			const loggedIn1 = await loginWithTotp(harness, account1, secret1);
			const user1SudoToken = await getSudoTokenViaMfa(harness, loggedIn1.token, secret1);
			const account2 = await createTestAccount(harness);
			const secret2 = generateTotpSecret();
			await enableTotpForAccount(harness, account2, secret2);
			const loggedIn2 = await loginWithTotp(harness, account2, secret2);
			const {json: errResp} = await createBuilder<ErrorResponse>(harness, loggedIn2.token)
				.post('/users/@me/disable')
				.header(SUDO_MODE_HEADER, user1SudoToken)
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN)
				.executeWithResponse();
			expect(errResp.code).toBe('SUDO_MODE_REQUIRED');
		});
	});
	describe('password user requires password each time', () => {
		it('password user does not receive sudo token', async () => {
			const account = await createTestAccount(harness);
			const {response} = await createBuilder(harness, account.token)
				.post('/users/@me/disable')
				.body({
					password: account.password,
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.executeWithResponse();
			const sudoToken = response.headers.get(SUDO_MODE_HEADER);
			expect(sudoToken).toBeFalsy();
		});
		it('password user must provide password for subsequent sensitive operations', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/disable')
				.body({
					password: account.password,
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const login = await loginUser(harness, {email: account.email, password: account.password});
			if ('mfa' in login && login.mfa) {
				throw new Error('Expected non-MFA login');
			}
			const nonMfaLogin = login as {
				user_id: string;
				token: string;
			};
			const {json: errResp} = await createBuilder<ErrorResponse>(harness, nonMfaLogin.token)
				.post('/users/@me/disable')
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN)
				.executeWithResponse();
			expect(errResp.code).toBe('SUDO_MODE_REQUIRED');
		});
	});
	describe('MFA registration without token fails', () => {
		it('WebAuthn registration options do not issue sudo token', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const sudoToken = await getSudoTokenViaMfa(harness, loggedIn.token, secret);
			const {response} = await createBuilder(harness, loggedIn.token)
				.post('/users/@me/mfa/webauthn/credentials/registration-options')
				.header(SUDO_MODE_HEADER, sudoToken)
				.body({})
				.executeWithResponse();
			const newSudoToken = response.headers.get(SUDO_MODE_HEADER);
			expect(newSudoToken).toBeFalsy();
		});
	});
	describe('existing MFA token allows skipping MFA', () => {
		it('valid sudo token allows skipping MFA for subsequent requests', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const {response: firstResp} = await createBuilder<BackupCodesResponse>(harness, loggedIn.token)
				.post('/users/@me/mfa/backup-codes')
				.body({
					mfa_method: 'totp',
					mfa_code: await generateTotpCode(secret),
					regenerate: false,
				})
				.executeWithResponse();
			const sudoToken = firstResp.headers.get(SUDO_MODE_HEADER);
			expect(sudoToken).toBeTruthy();
			const backupCodes = await createBuilder<BackupCodesResponse>(harness, loggedIn.token)
				.post('/users/@me/mfa/backup-codes')
				.header(SUDO_MODE_HEADER, sudoToken!)
				.body({
					regenerate: true,
				})
				.execute();
			expect(backupCodes.backup_codes.length).toBeGreaterThan(0);
		});
		it('sudo token from MFA allows skipping MFA verification on sensitive endpoint', async () => {
			const account = await createTestAccount(harness);
			const secret = generateTotpSecret();
			await enableTotpForAccount(harness, account, secret);
			const loggedIn = await loginWithTotp(harness, account, secret);
			const sudoToken = await getSudoTokenViaMfa(harness, loggedIn.token, secret);
			const backupCodes = await createBuilder<BackupCodesResponse>(harness, loggedIn.token)
				.post('/users/@me/mfa/backup-codes')
				.header(SUDO_MODE_HEADER, sudoToken)
				.body({
					regenerate: false,
				})
				.execute();
			expect(backupCodes.backup_codes.length).toBeGreaterThan(0);
		});
	});
});
