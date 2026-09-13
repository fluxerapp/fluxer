// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	type LoginMfaResponse,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	type WebAuthnAuthenticationOptions,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('WebAuthn MFA login', () => {
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
	it('validates the WebAuthn MFA login flow', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		const regOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
			.execute();
		if (regOptions.rp.id) {
			device.rpId = regOptions.rp.id;
		}
		const registrationResponse = createRegistrationResponse(device, regOptions, 'MFA Passkey');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse,
				challenge: regOptions.challenge,
				name: 'MFA Passkey',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const loginResp = await loginUser(harness, {email: account.email, password: account.password});
		expect('mfa' in loginResp && loginResp.mfa).toBe(true);
		const loginMfaResp = loginResp as LoginMfaResponse;
		expect(loginMfaResp.ticket).toBeTruthy();
		expect(loginMfaResp.allowed_methods).toContain('webauthn');
		expect('token' in loginMfaResp).toBe(false);
		const mfaOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/login/mfa/webauthn/authentication-options')
			.body({ticket: loginMfaResp.ticket})
			.execute();
		expect(mfaOptions.challenge).toBeTruthy();
		expect(mfaOptions.rpId).toBeTruthy();
		expect(mfaOptions.userVerification).toBe('discouraged');
		expect(mfaOptions.allowCredentials).toBeTruthy();
		expect(mfaOptions.allowCredentials!.length).toBeGreaterThan(0);
		if (mfaOptions.rpId) {
			device.rpId = mfaOptions.rpId;
		}
		const mfaAssertion = createAuthenticationResponse(device, mfaOptions);
		const webauthnMfaLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/webauthn')
			.body({
				response: mfaAssertion,
				challenge: mfaOptions.challenge,
				ticket: (loginResp as LoginMfaResponse).ticket,
			})
			.execute();
		expect(webauthnMfaLogin.token).toBeTruthy();
		const userInfo = await createBuilder<{
			id: string;
		}>(harness, webauthnMfaLogin.token)
			.get('/users/@me')
			.execute();
		expect(userInfo.id).toBe(account.userId);
	});
});
