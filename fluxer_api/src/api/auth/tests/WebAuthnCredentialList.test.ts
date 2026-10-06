// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createRegistrationResponse,
	createTotpSecret,
	createWebAuthnDevice,
	generateTotpCode,
	type WebAuthnCredentialMetadata,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('WebAuthn credential list', () => {
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
	it('validates listing all WebAuthn credentials for a user', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		const emptyCredentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(emptyCredentials).toHaveLength(0);
		const device1 = createWebAuthnDevice();
		const regOptions1 = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
			.execute();
		if (regOptions1.rp.id) {
			device1.rpId = regOptions1.rp.id;
		}
		const registrationResponse1 = createRegistrationResponse(device1, regOptions1, 'First Passkey');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse1,
				challenge: regOptions1.challenge,
				name: 'First Passkey',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const device2 = createWebAuthnDevice();
		const regOptions2 = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
			.execute();
		if (regOptions2.rp.id) {
			device2.rpId = regOptions2.rp.id;
		}
		const registrationResponse2 = createRegistrationResponse(device2, regOptions2, 'Second Passkey');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse2,
				challenge: regOptions2.challenge,
				name: 'Second Passkey',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const credentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentials).toHaveLength(2);
		const firstCred = credentials.find(
			(c) => c.name === 'First Passkey' && c.id === device1.credentialId.toString('base64url'),
		);
		expect(firstCred).toBeDefined();
		const secondCred = credentials.find(
			(c) => c.name === 'Second Passkey' && c.id === device2.credentialId.toString('base64url'),
		);
		expect(secondCred).toBeDefined();
	});
});
