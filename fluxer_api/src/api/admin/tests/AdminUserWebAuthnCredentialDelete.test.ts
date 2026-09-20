// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, createTotpSecret, generateTotpCode, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {
	createWebAuthnDevice,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnCredentialMetadata,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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
	async function createAdmin() {
		const admin = await createTestAccount(harness);
		return await setUserACLs(harness, admin, [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_LOOKUP,
			AdminACLs.USER_UPDATE_MFA,
		]);
	}
	async function createPasskeyTarget(twoFactorEnabled: boolean) {
		const target = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		await createBuilder(harness, target.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: target.password})
			.execute();
		await registerWebAuthnCredential(
			harness,
			target.token,
			device,
			() => ({mfa_method: 'totp', mfa_code: generateTotpCode(secret)}),
			'Admin Delete Test Passkey',
		);
		if (twoFactorEnabled) {
			await setWebAuthnTwoFactor(harness, target.token, true, {
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			});
		}
		await createBuilder(harness, target.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: generateTotpCode(secret),
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		return target;
	}
	test('removes the WebAuthn authenticator type when admin deletes the last credential of a two-factor user', async () => {
		const admin = await createAdmin();
		const target = await createPasskeyTarget(true);
		const credentialsBeforeDelete = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, target.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentialsBeforeDelete).toHaveLength(1);
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
	test('leaves the authenticator types empty throughout for a user who never turned passkey two-factor on', async () => {
		const admin = await createAdmin();
		const target = await createPasskeyTarget(false);
		const credentialsBeforeDelete = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, target.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		expect(credentialsBeforeDelete).toHaveLength(1);
		const userBeforeDelete = await createBuilder<AdminLookupResponse>(harness, `${admin.token}`)
			.get(`/admin/users/${target.userId}`)
			.execute();
		expect(userBeforeDelete.users[0]?.authenticator_types).toEqual([]);
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
