// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createRegistrationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	registerWebAuthnCredential,
	type WebAuthnCredentialMetadata,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('WebAuthn credential registration', () => {
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
	it('validates the WebAuthn credential registration flow', async () => {
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
		expect(regOptions.challenge).toBeTruthy();
		expect(regOptions.rp.id).toBeTruthy();
		expect(regOptions.user.id).toBeTruthy();
		expect(regOptions.authenticatorSelection).toEqual({
			residentKey: 'preferred',
			requireResidentKey: false,
			userVerification: 'preferred',
		});
		if (regOptions.rp.id) {
			device.rpId = regOptions.rp.id;
		}
		const registrationResponse = createRegistrationResponse(device, regOptions, 'Test Passkey');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse,
				challenge: regOptions.challenge,
				name: 'Test Passkey',
			})
			.expect(204)
			.execute();
		const credentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentials).toHaveLength(1);
		expect(credentials[0].name).toBe('Test Passkey');
		expect(credentials[0].id).toBe(device.credentialId.toString('base64url'));
	});
	it('does not turn passkeys into a second factor when a credential is registered', async () => {
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
		const me = await createBuilder<{
			authenticator_types: Array<number>;
		}>(harness, account.token)
			.get('/users/@me')
			.execute();
		expect(me.authenticator_types).toEqual([UserAuthenticatorTypes.TOTP]);
	});
});
