// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	type LoginMfaResponse,
	type LoginSuccessResponse,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnAuthenticationOptions,
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
	it('validates the WebAuthn MFA login flow when passkey two-factor is turned on', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(
			harness,
			account.token,
			device,
			() => ({mfa_method: 'totp', mfa_code: generateTotpCode(secret)}),
			'MFA Passkey',
		);
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
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
				ticket: loginMfaResp.ticket,
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
	it('issues a session token instead of an MFA ticket when passkey two-factor is left off', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(
			harness,
			account.token,
			device,
			() => ({mfa_method: 'totp', mfa_code: generateTotpCode(secret)}),
			'MFA Passkey',
		);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const loginResp = await loginUser(harness, {email: account.email, password: account.password});
		expect('mfa' in loginResp).toBe(false);
		const loginSuccessResp = loginResp as LoginSuccessResponse;
		expect(loginSuccessResp.token).toBeTruthy();
		const userInfo = await createBuilder<{
			id: string;
		}>(harness, loginSuccessResp.token)
			.get('/users/@me')
			.execute();
		expect(userInfo.id).toBe(account.userId);
	});
});
