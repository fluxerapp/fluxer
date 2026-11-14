// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, createTotpSecret, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	message: string;
}

describe('MFA requires verified email', () => {
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
	async function createUnverifiedAccount() {
		const account = await createTestAccount(harness, {skipEmailVerification: true});
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.expect(200)
			.execute();
		return account;
	}
	it('rejects TOTP enablement for unverified accounts', async () => {
		const account = await createUnverifiedAccount();
		const secret = createTotpSecret();
		const {json} = await createBuilder<ErrorResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
	});
	it('rejects WebAuthn registration setup for unverified accounts', async () => {
		const account = await createUnverifiedAccount();
		const {json} = await createBuilder<ErrorResponse>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({password: account.password})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
	});
	it('rejects new backup codes for unverified accounts and keeps the two-factor they already have', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.execute();
		await createBuilder(harness, '')
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: false})
			.expect(200)
			.execute();
		const {json} = await createBuilder<ErrorResponse>(harness, account.token)
			.post('/users/@me/mfa/backup-codes')
			.body({regenerate: true, mfa_method: 'totp', mfa_code: totpCodeNow(secret)})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
		expect(json.message).toBe('Confirm your email address to use two-factor authentication or passkeys.');
		const me = await createBuilder<{mfa_enabled: boolean}>(harness, account.token).get('/users/@me').execute();
		expect(me.mfa_enabled).toBe(true);
	});
});
