// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
		consumed: boolean;
	}>;
}

const SUDO_MODE_HEADER = 'X-Fluxer-Sudo-Mode-JWT';

async function loginWithTotp(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<TestAccount> {
	const loginResp = await createBuilderWithoutAuth<
		| {
				mfa: true;
				ticket: string;
		  }
		| {
				mfa: false;
				token: string;
		  }
	>(harness)
		.post('/auth/login')
		.body({
			email: account.email,
			password: account.password,
		})
		.execute();
	if (!loginResp.mfa) {
		throw new Error('Expected MFA login');
	}
	const mfaLoginResp = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({
			ticket: loginResp.ticket,
			code: totpCodeNow(secret),
		})
		.execute();
	return {...account, token: mfaLoginResp.token};
}

describe('Auth sudo TOTP verification', () => {
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
	it('logs out session with TOTP and returns sudo token', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		const enableResp = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
				password: account.password,
			})
			.execute();
		expect(enableResp.backup_codes.length).toBeGreaterThan(0);
		let loggedIn = await loginWithTotp(harness, account, secret);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		const {response: logoutResponse} = await createBuilder(harness, loggedIn.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
			})
			.expect(204)
			.executeWithResponse();
		const sudoToken = logoutResponse.headers.get(SUDO_MODE_HEADER);
		expect(sudoToken).toBeTruthy();
		await createBuilder(harness, loggedIn.token).get('/users/@me').expect(401).execute();
		loggedIn = await loginWithTotp(harness, loggedIn, secret);
		const sessions2 = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions2.length).toBeGreaterThan(0);
		await createBuilder(harness, loggedIn.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions2[0]!.id_hash],
				mfa_method: 'totp',
				mfa_code: '000000',
			})
			.expect(400)
			.execute();
		await createBuilder(harness, loggedIn.token).get('/users/@me').expect(200).execute();
	});
	it('logs out session with backup code and returns sudo token', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		const enableResp = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
				password: account.password,
			})
			.execute();
		expect(enableResp.backup_codes.length).toBeGreaterThan(0);
		const backupCode = enableResp.backup_codes[0]!.code;
		const loggedIn = await loginWithTotp(harness, account, secret);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		const {response: logoutResponse} = await createBuilder(harness, loggedIn.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				mfa_method: 'totp',
				mfa_code: backupCode,
			})
			.expect(204)
			.executeWithResponse();
		const sudoToken = logoutResponse.headers.get(SUDO_MODE_HEADER);
		expect(sudoToken).toBeTruthy();
		await createBuilder(harness, loggedIn.token).get('/users/@me').expect(401).execute();
	});
	it('rejects password when MFA is enabled with 403', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
				password: account.password,
			})
			.execute();
		const loggedIn = await loginWithTotp(harness, account, secret);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, loggedIn.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				password: account.password,
			})
			.expect(403)
			.execute();
	});
	it('rejects logout without MFA method when MFA is enabled with 403', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
				password: account.password,
			})
			.execute();
		const loggedIn = await loginWithTotp(harness, account, secret);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, loggedIn.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, loggedIn.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
			})
			.expect(403)
			.execute();
	});
});
