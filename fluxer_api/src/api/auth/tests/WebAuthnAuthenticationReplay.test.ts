// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
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

describe('WebAuthn authentication replay', () => {
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
	it('covers WebAuthn authentication challenge reuse rejection for passwordless flow', async () => {
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
		const registrationResponse = createRegistrationResponse(device, regOptions, 'ReplayTest');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse,
				challenge: regOptions.challenge,
				name: 'ReplayTest',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const authOptions = await createBuilderWithoutAuth<WebAuthnAuthenticationOptions>(harness)
			.post('/auth/webauthn/authentication-options')
			.body(null)
			.execute();
		expect(authOptions.challenge).toBeTruthy();
		expect(authOptions.rpId).toBeTruthy();
		if (authOptions.rpId) {
			device.rpId = authOptions.rpId;
		}
		const assertion = createAuthenticationResponse(device, authOptions);
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.body({
				response: assertion,
				challenge: authOptions.challenge,
			})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/webauthn/authenticate')
			.body({
				response: assertion,
				challenge: authOptions.challenge,
			})
			.expect(401)
			.execute();
	});
});
