// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';
import {createTestAccount, createTotpSecret, generateTotpCode, type TestAccount} from './AuthTestUtils';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from './WebAuthnTestUtils';

interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
	}>;
}

interface LoginMfaResponse {
	mfa: true;
	ticket: string;
	totp: boolean;
	webauthn: boolean;
}

interface SudoMfaMethodsResponse {
	totp: boolean;
	webauthn: boolean;
	has_mfa: boolean;
}

interface SudoModeRequiredResponse {
	code: string;
	has_mfa?: boolean;
	methods?: {
		totp?: boolean;
		webauthn?: boolean;
	};
}

async function loginWithTotp(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<TestAccount> {
	const login = await createBuilderWithoutAuth<LoginMfaResponse>(harness)
		.post('/auth/login')
		.body({
			email: account.email,
			password: account.password,
		})
		.execute();
	expect(login.mfa).toBe(true);
	const mfaLogin = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({
			code: generateTotpCode(secret),
			ticket: login.ticket,
		})
		.execute();
	return {...account, token: mfaLogin.token};
}

async function setupWebAuthnOnlyUser(
	harness: ApiTestHarness,
	account: TestAccount,
): Promise<{
	account: TestAccount;
	device: WebAuthnDevice;
}> {
	const device = createWebAuthnDevice();
	const secret = createTotpSecret();
	const backupCodes = await createBuilder<BackupCodesResponse>(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({
			secret,
			code: generateTotpCode(secret),
			password: account.password,
		})
		.execute();
	const login = await createBuilderWithoutAuth<LoginMfaResponse>(harness)
		.post('/auth/login')
		.body({
			email: account.email,
			password: account.password,
		})
		.execute();
	expect(login.mfa).toBe(true);
	const mfaLogin = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({
			code: backupCodes.backup_codes[0]!.code,
			ticket: login.ticket,
		})
		.execute();
	const updatedAccount = {...account, token: mfaLogin.token};
	const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, updatedAccount.token)
		.post('/users/@me/mfa/webauthn/credentials/registration-options')
		.body({
			mfa_method: 'totp',
			mfa_code: backupCodes.backup_codes[1]!.code,
		})
		.execute();
	const registrationResponse = createRegistrationResponse(device, registrationOptions, 'Test Passkey');
	await createBuilder(harness, updatedAccount.token)
		.post('/users/@me/mfa/webauthn/credentials')
		.body({
			response: registrationResponse,
			challenge: registrationOptions.challenge,
			name: 'Test Passkey',
			mfa_method: 'totp',
			mfa_code: backupCodes.backup_codes[2]!.code,
		})
		.expect(204)
		.execute();
	await createBuilder(harness, updatedAccount.token)
		.post('/users/@me/mfa/totp/disable')
		.body({
			code: backupCodes.backup_codes[3]!.code,
			mfa_method: 'totp',
			mfa_code: backupCodes.backup_codes[4]!.code,
		})
		.expect(204)
		.execute();
	const discoverableOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
		.post('/auth/webauthn/authentication-options')
		.body(null)
		.execute();
	const discoverableAssertion = createAuthenticationResponse(device, discoverableOptions);
	const passkeyLogin = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/webauthn/authenticate')
		.body({
			response: discoverableAssertion,
			challenge: discoverableOptions.challenge,
		})
		.execute();
	return {account: {...updatedAccount, token: passkeyLogin.token}, device};
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
	describe('WebAuthn sudo verification flow', () => {
		test('WebAuthn user can complete sudo verification with passkey', async () => {
			const account = await createTestAccount(harness);
			const {account: webauthnAccount, device} = await setupWebAuthnOnlyUser(harness, account);
			const sudoOptions = await createBuilder<WebAuthnAuthenticationOptions>(harness, webauthnAccount.token)
				.post('/users/@me/sudo/webauthn/authentication-options')
				.body(null)
				.execute();
			const sudoAssertion = createAuthenticationResponse(device, sudoOptions);
			await createBuilder(harness, webauthnAccount.token)
				.post('/users/@me/disable')
				.body({
					mfa_method: 'webauthn',
					webauthn_response: sudoAssertion,
					webauthn_challenge: sudoOptions.challenge,
				})
				.expect(204)
				.execute();
		});
		test('WebAuthn-only user cannot use password for sudo verification', async () => {
			const account = await createTestAccount(harness);
			const {account: webauthnAccount} = await setupWebAuthnOnlyUser(harness, account);
			const errorResp = await createBuilder<{
				code: string;
			}>(harness, webauthnAccount.token)
				.post('/users/@me/disable')
				.body({
					password: account.password,
				})
				.expect(403)
				.execute();
			expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
		});
	});
	describe('Password-only sudo flow for non-MFA users', () => {
		test('Non-MFA user can use password for sudo verification', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/disable')
				.body({
					password: account.password,
				})
				.expect(204)
				.execute();
		});
		test('Non-MFA user cannot perform sudo operation without password', async () => {
			const account = await createTestAccount(harness);
			const errorResp = await createBuilder<{
				code: string;
			}>(harness, account.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403)
				.execute();
			expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
		});
		test('Non-MFA user rejected with wrong password', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/users/@me/disable')
				.body({
					password: 'wrong-password-123!',
				})
				.expect(400)
				.execute();
		});
	});
	describe('TOTP sudo verification flow', () => {
		test('TOTP user can complete sudo verification with TOTP code', async () => {
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
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({
					mfa_method: 'totp',
					mfa_code: generateTotpCode(secret),
				})
				.expect(204)
				.execute();
		});
		test('TOTP user can use backup code for sudo verification', async () => {
			const account = await createTestAccount(harness);
			const secret = createTotpSecret();
			const backupCodes = await createBuilder<BackupCodesResponse>(harness, account.token)
				.post('/users/@me/mfa/totp/enable')
				.body({
					secret,
					code: generateTotpCode(secret),
					password: account.password,
				})
				.execute();
			const loggedIn = await loginWithTotp(harness, account, secret);
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({
					mfa_method: 'totp',
					mfa_code: backupCodes.backup_codes[0]!.code,
				})
				.expect(204)
				.execute();
		});
		test('TOTP user cannot use password for sudo verification', async () => {
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
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({
					password: account.password,
				})
				.expect(403)
				.execute();
		});
		test('TOTP user rejected with wrong TOTP code', async () => {
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
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({
					mfa_method: 'totp',
					mfa_code: '000000',
				})
				.expect(400)
				.execute();
		});
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
			expect(methods).toEqual({totp: false, webauthn: false, has_mfa: false});
			const errorResp = await createBuilder<SudoModeRequiredResponse>(harness, account.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403, 'SUDO_MODE_REQUIRED')
				.execute();
			expect(errorResp.has_mfa).toBe(methods.has_mfa);
			expect(errorResp.methods).toEqual({totp: methods.totp, webauthn: methods.webauthn});
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
			expect(methods).toEqual({totp: true, webauthn: false, has_mfa: true});
			const errorResp = await createBuilder<SudoModeRequiredResponse>(harness, loggedIn.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403, 'SUDO_MODE_REQUIRED')
				.execute();
			expect(errorResp.has_mfa).toBe(methods.has_mfa);
			expect(errorResp.methods).toEqual({totp: methods.totp, webauthn: methods.webauthn});
		});
	});
	describe('MFA requirement propagates to sensitive operations', () => {
		test('Account disable requires sudo for all users', async () => {
			const account = await createTestAccount(harness);
			const errorResp = await createBuilder<{
				code: string;
			}>(harness, account.token)
				.post('/users/@me/disable')
				.body({})
				.expect(403)
				.execute();
			expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
		});
		test('Account delete requires sudo for all users', async () => {
			const account = await createTestAccount(harness);
			const errorResp = await createBuilder<{
				code: string;
			}>(harness, account.token)
				.post('/users/@me/delete')
				.body({})
				.expect(403)
				.execute();
			expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
		});
		test('Session logout requires sudo for non-MFA user', async () => {
			const account = await createTestAccount(harness);
			const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
				.get('/auth/sessions')
				.execute();
			await createBuilder(harness, account.token)
				.post('/auth/sessions/logout')
				.body({
					session_id_hashes: [sessions[0]!.id_hash],
				})
				.expect(403)
				.execute();
			await createBuilder(harness, account.token)
				.post('/auth/sessions/logout')
				.body({
					session_id_hashes: [sessions[0]!.id_hash],
					password: account.password,
				})
				.expect(204)
				.execute();
		});
		test('Session logout requires MFA for TOTP user', async () => {
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
			const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
				.get('/auth/sessions')
				.execute();
			await createBuilder(harness, loggedIn.token)
				.post('/auth/sessions/logout')
				.body({
					session_id_hashes: [sessions[0]!.id_hash],
					password: account.password,
				})
				.expect(403)
				.execute();
			await createBuilder(harness, loggedIn.token)
				.post('/auth/sessions/logout')
				.body({
					session_id_hashes: [sessions[0]!.id_hash],
					mfa_method: 'totp',
					mfa_code: generateTotpCode(secret),
				})
				.expect(204)
				.execute();
		});
		test('TOTP disable requires MFA for sudo verification', async () => {
			const account = await createTestAccount(harness);
			const secret = createTotpSecret();
			const backupCodes = await createBuilder<BackupCodesResponse>(harness, account.token)
				.post('/users/@me/mfa/totp/enable')
				.body({
					secret,
					code: generateTotpCode(secret),
					password: account.password,
				})
				.execute();
			const loggedIn = await loginWithTotp(harness, account, secret);
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/mfa/totp/disable')
				.body({
					code: backupCodes.backup_codes[0]!.code,
				})
				.expect(403)
				.execute();
			await createBuilder(harness, loggedIn.token)
				.post('/users/@me/mfa/totp/disable')
				.body({
					code: backupCodes.backup_codes[0]!.code,
					mfa_method: 'totp',
					mfa_code: generateTotpCode(secret),
				})
				.expect(204)
				.execute();
		});
	});
});
