// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createSudoWebAuthnBody,
	createWebAuthnDevice,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnCredentialMetadata,
	type WebAuthnTwoFactorResult,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface PrivateUserResponse {
	id: string;
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
}

describe('WebAuthn two-factor toggle', () => {
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
	it('reports an empty authenticator types array for an account with no second factor', async () => {
		const account = await createTestAccount(harness);
		const me = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.authenticator_types).toEqual([]);
		expect(me.mfa_enabled).toBe(false);
	});
	it('rejects enabling passkey two-factor when the account has no registered credential', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: true, password: account.password})
			.expect(HTTP_STATUS.BAD_REQUEST, 'NO_PASSKEYS_REGISTERED')
			.execute();
		const me = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.authenticator_types).toEqual([]);
	});
	it('round trips enabling and disabling passkey two-factor', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const enabled = await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		expect(enabled.user.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
		expect(enabled.user.mfa_enabled).toBe(true);
		const afterEnable = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(afterEnable.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const disabled = await setWebAuthnTwoFactor(harness, account.token, false, sudoBody);
		expect(disabled.user.authenticator_types).toEqual([]);
		expect(disabled.user.mfa_enabled).toBe(false);
		const afterDisable = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(afterDisable.authenticator_types).toEqual([]);
	});
	it('refuses to disable passkey two-factor without a sudo proof', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, password: account.password})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		const me = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	});
	it('accepts a passkey assertion as the sudo proof for disabling passkey two-factor', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const disabled = await createBuilder<WebAuthnTwoFactorResult>(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, ...sudoBody})
			.execute();
		expect(disabled.user.authenticator_types).toEqual([]);
	});
	it('leaves the account untouched when the requested state already matches', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		const firstEnable = await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		expect(firstEnable.backup_codes).not.toBeNull();
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const secondEnable = await setWebAuthnTwoFactor(harness, account.token, true, sudoBody);
		expect(secondEnable.backup_codes).toBeNull();
		expect(secondEnable.user.authenticator_types).toEqual([UserAuthenticatorTypes.WEBAUTHN]);
	});
	it('drops the passkey second factor when the last credential is deleted', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await setWebAuthnTwoFactor(harness, account.token, true, {password: account.password});
		const credentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, account.token)
			.get('/users/@me/mfa/webauthn/credentials')
			.execute();
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token)
			.delete(`/users/@me/mfa/webauthn/credentials/${credentials[0]!.id}`)
			.body(sudoBody)
			.expect(204)
			.execute();
		const me = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.authenticator_types).toEqual([]);
		expect(me.mfa_enabled).toBe(false);
	});
});
