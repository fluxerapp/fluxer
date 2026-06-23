// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, createTotpSecret, totpCodeNow} from './AuthTestUtils';

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
});
