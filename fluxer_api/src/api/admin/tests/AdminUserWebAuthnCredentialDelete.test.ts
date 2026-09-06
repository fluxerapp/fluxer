// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, createTotpSecret, generateTotpCode, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnCredentialMetadata,
	type WebAuthnRegistrationOptions,
} from '../../auth/tests/WebAuthnTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AdminLookupResponse {
	users: Array<{
		id: string;
		authenticator_types: Array<number>;
	}>;
}

describe('Admin WebAuthn credential delete', () => {
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
	test('removes the WebAuthn authenticator type when admin deletes the last credential', async () => {
		let admin = await createTestAccount(harness);
		admin = await setUserACLs(harness, admin, [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_LOOKUP,
			AdminACLs.USER_UPDATE_MFA,
		]);
		const target = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, target.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: target.password})
			.execute();
		const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, target.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
			.execute();
		if (registrationOptions.rp.id) {
			device.rpId = registrationOptions.rp.id;
		}
		await createBuilder(harness, target.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: createRegistrationResponse(device, registrationOptions, 'Admin Delete Test Passkey'),
				challenge: registrationOptions.challenge,
				name: 'Admin Delete Test Passkey',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const credentialsBeforeDelete = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, target.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentialsBeforeDelete).toHaveLength(1);
		await createBuilder(harness, target.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const userBeforeDelete = await createBuilder<AdminLookupResponse>(harness, `${admin.token}`)
			.get(`/admin/users/${target.userId}`)
			.execute();
		expect(userBeforeDelete.users[0]?.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
		await createBuilder(harness, `${admin.token}`)
			.delete(`/admin/users/${target.userId}/webauthn-credentials/${credentialsBeforeDelete[0]!.id}`)
			.expect(204)
			.execute();
		const credentialsAfterDelete = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, target.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentialsAfterDelete).toHaveLength(0);
		const userAfterDelete = await createBuilder<AdminLookupResponse>(harness, `${admin.token}`)
			.get(`/admin/users/${target.userId}`)
			.execute();
		expect(userAfterDelete.users[0]?.authenticator_types).toEqual([]);
	});
});
