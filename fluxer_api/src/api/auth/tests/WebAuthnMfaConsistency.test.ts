// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTestAccount,
	createTotpSecret,
	generateTotpCode,
	type LoginMfaResponse,
	type LoginSuccessResponse,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createSudoWebAuthnBody,
	createWebAuthnDevice,
	loginWithDiscoverablePasskey,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
	type WebAuthnAuthenticationOptions,
	type WebAuthnDevice,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

async function setupPasskeyOnlyAccount(
	harness: ApiTestHarness,
	twoFactorEnabled: boolean,
): Promise<{
	account: TestAccount;
	device: WebAuthnDevice;
}> {
	const account = await createTestAccount(harness);
	const device = createWebAuthnDevice();
	const secret = createTotpSecret();
	await createBuilder(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({
			secret,
			code: generateTotpCode(secret),
			password: account.password,
		})
		.execute();
	await registerWebAuthnCredential(harness, account.token, device, () => ({
		mfa_method: 'totp',
		mfa_code: generateTotpCode(secret),
	}));
	if (twoFactorEnabled) {
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
	}
	await createBuilder(harness, account.token)
		.post('/users/@me/mfa/totp/disable')
		.body({
			code: generateTotpCode(secret),
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		})
		.expect(204)
		.execute();
	const token = await loginWithDiscoverablePasskey(harness, device);
	return {account: {...account, token}, device};
}

describe('WebAuthn MFA Consistency Tests', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('passkey user with two-factor on cannot use password for sudo - password rejected with 403', async () => {
		const {account} = await setupPasskeyOnlyAccount(harness, true);
		const {json: errorResp} = await createBuilder<{
			code: string;
		}>(harness, account.token)
			.post('/users/@me/disable')
			.body({
				password: account.password,
			})
			.expect(403)
			.executeWithResponse();
		expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
	});
	test('passkey user with two-factor off can still use password for sudo', async () => {
		const {account} = await setupPasskeyOnlyAccount(harness, false);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({
				password: account.password,
			})
			.expect(204)
			.execute();
	});
	test('passkey user with two-factor on can use WebAuthn for sudo verification', async () => {
		const {account, device} = await setupPasskeyOnlyAccount(harness, true);
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		const {response: disableResp} = await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body(sudoBody)
			.expect(204)
			.executeWithResponse();
		const sudoToken = disableResp.headers.get('x-sudo-mode-token');
		expect(sudoToken).toBeNull();
	});
	test('passkey user with two-factor off can use WebAuthn for sudo verification', async () => {
		const {account, device} = await setupPasskeyOnlyAccount(harness, false);
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token).post('/users/@me/disable').body(sudoBody).expect(204).execute();
	});
	test('passkey user with two-factor on requires MFA when logging in with password', async () => {
		const {account} = await setupPasskeyOnlyAccount(harness, true);
		const login = await createBuilderWithoutAuth<LoginMfaResponse>(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: account.password,
			})
			.execute();
		expect(login.mfa).toBe(true);
		expect(login.ticket).toBeTruthy();
		expect(login.webauthn).toBe(true);
	});
	test('passkey user with two-factor off logs in with password and receives a session token', async () => {
		const {account} = await setupPasskeyOnlyAccount(harness, false);
		const login = await createBuilderWithoutAuth<LoginSuccessResponse | LoginMfaResponse>(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: account.password,
			})
			.execute();
		expect('mfa' in login).toBe(false);
		expect((login as LoginSuccessResponse).token).toBeTruthy();
		const userInfo = await createBuilder<{
			id: string;
		}>(harness, (login as LoginSuccessResponse).token)
			.get('/users/@me')
			.execute();
		expect(userInfo.id).toBe(account.userId);
	});
	test('sudo WebAuthn options stay available to a passkey user with two-factor off', async () => {
		const {account, device} = await setupPasskeyOnlyAccount(harness, false);
		const sudoOptions = await createBuilder<WebAuthnAuthenticationOptions>(harness, account.token)
			.post('/users/@me/sudo/webauthn/authentication-options')
			.body(null)
			.execute();
		expect(sudoOptions.userVerification).toBe('discouraged');
		expect(sudoOptions.allowCredentials?.length).toBeGreaterThan(0);
		expect(device.credentialId.length).toBeGreaterThan(0);
	});
});
