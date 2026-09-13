// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createAuthenticationResponse,
	createRegistrationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	type WebAuthnAuthenticationOptions,
	type WebAuthnCredentialMetadata,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('WebAuthn credential delete', () => {
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
	it('validates deleting a WebAuthn credential including sudo verification with WebAuthn', async () => {
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
		const registrationResponse = createRegistrationResponse(device, regOptions, 'Passkey To Delete');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse,
				challenge: regOptions.challenge,
				name: 'Passkey To Delete',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const credentials1 = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentials1).toHaveLength(1);
		const credentialId = credentials1[0].id;
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const sudoOptions = await createBuilder<WebAuthnAuthenticationOptions>(harness, account.token)
			.post('/users/@me/sudo/webauthn/authentication-options')
			.body({})
			.execute();
		if (sudoOptions.rpId) {
			device.rpId = sudoOptions.rpId;
		}
		const sudoAssertion = createAuthenticationResponse(device, sudoOptions);
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credentialId}`)
			.body({
				mfa_method: 'webauthn',
				webauthn_response: sudoAssertion,
				webauthn_challenge: sudoOptions.challenge,
			})
			.expect(204)
			.execute();
		const credentials2 = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentials2).toHaveLength(0);
	});
});
