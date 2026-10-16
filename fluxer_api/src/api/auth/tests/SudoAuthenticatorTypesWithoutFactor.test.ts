// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	generateTotpCode,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {setWebAuthnTwoFactor} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface PrivateUserResponse {
	mfa_enabled: boolean;
	authenticator_types: Array<number>;
}

interface SudoModeRequiredResponse {
	code: string;
	has_mfa?: boolean;
}

async function setAuthenticatorTypes(account: TestAccount, types: Array<number>): Promise<void> {
	const users = getUserRepository();
	const user = (await users.findUnique(createUserID(BigInt(account.userId))))!;
	await users.patchUpsert(user.id, {authenticator_types: new Set<number>(types)}, user.toRow());
}

describe('Sudo mode for accounts whose authenticator types list no usable factor', () => {
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

	it('accepts the password and lets the account turn passkey two-factor off when no passkey remains', async () => {
		const account = await createTestAccount(harness);
		await setAuthenticatorTypes(account, [UserAuthenticatorTypes.WEBAUTHN]);
		const challenge = await createBuilder<SudoModeRequiredResponse>(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		expect(challenge.has_mfa).toBe(false);
		const disabled = await setWebAuthnTwoFactor(harness, account.token, false, {password: account.password});
		expect(disabled.user.authenticator_types).toEqual([]);
		const me = await createBuilder<PrivateUserResponse>(harness, account.token).get('/users/@me').execute();
		expect(me.mfa_enabled).toBe(false);
	});

	it('accepts the password when the TOTP type is listed without a stored secret', async () => {
		const account = await createTestAccount(harness);
		await setAuthenticatorTypes(account, [UserAuthenticatorTypes.TOTP]);
		await createBuilder(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, password: 'wrong-password'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await setWebAuthnTwoFactor(harness, account.token, false, {password: account.password});
	});

	it('still refuses the password from an account with TOTP enrolled', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		const challenge = await createBuilder<SudoModeRequiredResponse>(harness, account.token)
			.put('/users/@me/mfa/webauthn/two-factor')
			.body({enabled: false, password: account.password})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		expect(challenge.has_mfa).toBe(true);
	});
});
