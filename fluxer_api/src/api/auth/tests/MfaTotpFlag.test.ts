// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BackupCodesResponse,
	createAuthHarness,
	createTestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth MFA TOTP flag matches authenticator types', () => {
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
	it('WebAuthn-only reports TOTP false', async () => {
		const account = await createTestAccount(harness);
		const secret = 'JBSWY3DPEHPK3PXP';
		const totpData = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.execute();
		expect(totpData.backup_codes.length).toBeGreaterThan(0);
		const totpLogin = await createBuilderWithoutAuth<{
			ticket: string;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect(totpLogin.ticket).toBeDefined();
		const totpResp = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({code: totpCodeNow(secret), ticket: totpLogin.ticket})
			.execute();
		account.token = totpResp.token;
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: totpData.backup_codes[0]!.code,
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
			})
			.expect(204)
			.execute();
		const loginResp = await createBuilderWithoutAuth<{
			token: string;
			user_id: string;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect('mfa' in loginResp).toBe(false);
		expect(loginResp.token).toBeDefined();
	});
});
