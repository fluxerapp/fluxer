// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createTestAccount, createTotpSecret, generateTotpCode} from './AuthTestUtils';
import {createAuthenticationResponse, createRegistrationResponse, createWebAuthnDevice} from './WebAuthnTestUtils';
import type {AuthMfaMethod} from '@fluxer/schema/src/domains/auth/AuthSchemas';

interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
	}>;
}

interface LoginMfaResponse {
	mfa: true;
	ticket: string;
	allowed_methods: Array<AuthMfaMethod>;
}

interface WebAuthnRegistrationOptions {
	challenge: string;
	rp: {
		id: string;
		name: string;
	};
	user: {
		id: string;
		name: string;
		displayName: string;
	};
	authenticatorSelection?: {
		residentKey?: string;
		requireResidentKey?: boolean;
		userVerification?: string;
	};
}

interface WebAuthnAuthenticationOptions {
	challenge: string;
	rpId: string;
	allowCredentials?: Array<{
		id: string;
		type: string;
	}>;
	userVerification: string;
}

describe('WebAuthn MFA Consistency Tests', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('WebAuthn-only user cannot use password for sudo - password rejected with 403', async () => {
		const account = await createTestAccount(harness);
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
		account.token = mfaLogin.token;
		const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({
				mfa_method: 'totp',
				mfa_code: backupCodes.backup_codes[1]!.code,
			})
			.execute();
		expect(registrationOptions.authenticatorSelection).toMatchObject({
			residentKey: 'preferred',
			requireResidentKey: false,
			userVerification: 'preferred',
		});
		const registrationResponse = createRegistrationResponse(device, registrationOptions, 'Test Passkey');
		await createBuilder(harness, account.token)
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
		await createBuilder(harness, account.token)
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
		account.token = passkeyLogin.token;
		const {json: errorResp} = await createBuilder<{
			code: string;
		}>(harness, account.token)
			.post('/users/@me/disable')
			.body({
				password: account.password,
			})
			.expect(403)
			.executeWithResponse();
		expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
	});
	test('WebAuthn-only user can use WebAuthn for sudo verification', async () => {
		const account = await createTestAccount(harness);
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
		account.token = mfaLogin.token;
		const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({
				mfa_method: 'totp',
				mfa_code: backupCodes.backup_codes[1]!.code,
			})
			.execute();
		const registrationResponse = createRegistrationResponse(device, registrationOptions, 'Test Passkey');
		await createBuilder(harness, account.token)
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
		await createBuilder(harness, account.token)
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
		account.token = passkeyLogin.token;
		const sudoOptions = await createBuilder<WebAuthnAuthenticationOptions>(harness, account.token)
			.post('/users/@me/sudo/webauthn/authentication-options')
			.body(null)
			.execute();
		expect(sudoOptions.userVerification).toBe('discouraged');
		const sudoAssertion = createAuthenticationResponse(device, sudoOptions);
		const {response: disableResp2} = await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({
				mfa_method: 'webauthn',
				webauthn_response: sudoAssertion,
				webauthn_challenge: sudoOptions.challenge,
			})
			.expect(204)
			.executeWithResponse();
		const sudoToken = disableResp2.headers.get('x-sudo-mode-token');
		expect(sudoToken).toBeNull();
	});
	test('WebAuthn-only user requires MFA when logging in with password', async () => {
		const account = await createTestAccount(harness);
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
		account.token = mfaLogin.token;
		const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({
				mfa_method: 'totp',
				mfa_code: backupCodes.backup_codes[1]!.code,
			})
			.execute();
		const registrationResponse = createRegistrationResponse(device, registrationOptions, 'Test Passkey');
		await createBuilder(harness, account.token)
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
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: backupCodes.backup_codes[3]!.code,
				mfa_method: 'totp',
				mfa_code: backupCodes.backup_codes[4]!.code,
			})
			.expect(204)
			.execute();
		const login2 = await createBuilderWithoutAuth<LoginMfaResponse>(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: account.password,
			})
			.execute();
		expect(login2.mfa).toBe(true);
		expect(login2.ticket).toBeTruthy();
		expect(login2.allowed_methods).toContain('webauthn');
	});
});
