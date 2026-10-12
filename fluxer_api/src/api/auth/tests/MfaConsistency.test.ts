// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, createTotpSecret, generateTotpCode, loginWithTotp} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface SudoMfaMethodsResponse {
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
	has_mfa: boolean;
}

interface SudoModeRequiredResponse {
	code: string;
	has_mfa?: boolean;
	methods?: {
		totp?: boolean;
		webauthn?: boolean;
		backup_codes?: boolean;
	};
}

describe('MFA Consistency Tests', () => {
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
	describe('Sudo MFA method availability', () => {
		test('mfa-methods agrees with the SUDO_MODE_REQUIRED body for an orphaned TOTP secret', async () => {
			const account = await createTestAccount(harness);
			const userRepository = new UserRepository();
			const userId = createUserID(BigInt(account.userId));
			const existing = await userRepository.findUnique(userId);
			await userRepository.patchUpsert(userId, {totp_secret: createTotpSecret()}, existing!.toRow());
			const methods = await createBuilder<SudoMfaMethodsResponse>(harness, account.token)
				.get('/users/@me/sudo/mfa-methods')
				.execute();
			expect(methods).toEqual({totp: false, webauthn: false, backup_codes: false, has_mfa: false});
			const errorResp = await createBuilder<SudoModeRequiredResponse>(harness, account.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403, 'SUDO_MODE_REQUIRED')
				.execute();
			expect(errorResp.has_mfa).toBe(methods.has_mfa);
			expect(errorResp.methods).toEqual({
				totp: methods.totp,
				webauthn: methods.webauthn,
				backup_codes: methods.backup_codes,
			});
		});
		test('mfa-methods agrees with the SUDO_MODE_REQUIRED body for an enrolled TOTP user', async () => {
			const account = await createTestAccount(harness);
			const secret = createTotpSecret();
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/enable')
				.body({
					secret,
					code: generateTotpCode(secret),
					password: account.password,
				})
				.execute();
			const loggedIn = await loginWithTotp(harness, account, secret);
			const methods = await createBuilder<SudoMfaMethodsResponse>(harness, loggedIn.token)
				.get('/users/@me/sudo/mfa-methods')
				.execute();
			expect(methods).toEqual({totp: true, webauthn: false, backup_codes: true, has_mfa: true});
			const errorResp = await createBuilder<SudoModeRequiredResponse>(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403, 'SUDO_MODE_REQUIRED')
				.execute();
			expect(errorResp.has_mfa).toBe(methods.has_mfa);
			expect(errorResp.methods).toEqual({
				totp: methods.totp,
				webauthn: methods.webauthn,
				backup_codes: methods.backup_codes,
			});
		});
	});
});
