// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	generateTotpCode,
	type LoginMfaResponse,
	type LoginSuccessResponse,
	loginUser,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createWebAuthnDevice,
	loginWithDiscoverablePasskey,
	registerWebAuthnCredential,
	type WebAuthnAuthenticationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('WebAuthn opt-in login', () => {
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
	it('does not offer webauthn at login to a TOTP user whose passkey is opted out', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		const login = (await loginUser(harness, {
			email: account.email,
			password: account.password,
		})) as LoginMfaResponse;
		expect(login.mfa).toBe(true);
		expect(login.totp).toBe(true);
		expect(login.webauthn).toBe(false);
		expect(login.allowed_methods).not.toContain('webauthn');
		expect(login.allowed_methods).toContain('totp');
	});
	it('rejects the WebAuthn MFA login route for a user who never turned passkey two-factor on', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		const login = (await loginUser(harness, {
			email: account.email,
			password: account.password,
		})) as LoginMfaResponse;
		const mfaOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/login/mfa/webauthn/authentication-options')
			.body({ticket: login.ticket})
			.execute();
		if (mfaOptions.rpId) {
			device.rpId = mfaOptions.rpId;
		}
		await createBuilderWithoutAuth(harness)
			.post('/auth/login/mfa/webauthn')
			.body({
				response: createAuthenticationResponse(device, mfaOptions),
				challenge: mfaOptions.challenge,
				ticket: login.ticket,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
			.execute();
		const totpLogin = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: login.ticket, code: generateTotpCode(secret)})
			.execute();
		expect(totpLogin.token).toBeTruthy();
	});
	it('completes the passwordless journey for an account that never enrolled TOTP or the toggle', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const me = await createBuilder<{
			authenticator_types: Array<number>;
			mfa_enabled: boolean;
		}>(harness, account.token)
			.get('/users/@me')
			.execute();
		expect(me.authenticator_types).toEqual([]);
		expect(me.mfa_enabled).toBe(false);
		const passwordLogin = await loginUser(harness, {email: account.email, password: account.password});
		expect('mfa' in passwordLogin).toBe(false);
		expect((passwordLogin as LoginSuccessResponse).token).toBeTruthy();
		const passkeyToken = await loginWithDiscoverablePasskey(harness, device);
		expect(passkeyToken).toBeTruthy();
		const passkeyMe = await createBuilder<{
			id: string;
			authenticator_types: Array<number>;
		}>(harness, passkeyToken)
			.get('/users/@me')
			.execute();
		expect(passkeyMe.id).toBe(account.userId);
		expect(passkeyMe.authenticator_types).not.toContain(UserAuthenticatorTypes.WEBAUTHN);
	});
});
