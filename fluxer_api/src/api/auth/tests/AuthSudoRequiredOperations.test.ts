// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	loginAccount,
	loginWithTotp,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BackupCodesResponse {
	backup_codes: Array<{
		code: string;
		consumed: boolean;
	}>;
}

interface OAuth2ApplicationResponse {
	id: string;
	client_id: string;
	client_secret: string;
	bot: {
		id: string;
		token: string;
	};
}

async function createOAuth2BotApplication(
	harness: ApiTestHarness,
	account: TestAccount,
	name: string,
	redirectUris: Array<string>,
): Promise<string> {
	const created = await createBuilder<OAuth2ApplicationResponse>(harness, account.token)
		.post('/oauth2/applications')
		.body({
			name,
			redirect_uris: redirectUris,
		})
		.execute();
	return created.id;
}

async function deleteOAuth2Application(harness: ApiTestHarness, account: TestAccount, appId: string): Promise<void> {
	await createBuilder(harness, account.token)
		.delete(`/oauth2/applications/${appId}`)
		.body({
			password: account.password,
		})
		.expect(204)
		.execute();
}

describe('Auth sudo required operations', () => {
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
	it('requires sudo for session logout', async () => {
		const account = await createTestAccount(harness);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
			})
			.expect(403)
			.execute();
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				password: account.password,
			})
			.expect(204)
			.execute();
		await loginAccount(harness, account);
	});
	it('requires sudo for delete OAuth2 application', async () => {
		const account = await createTestAccount(harness);
		const appName = `Test App ${Date.now()}`;
		const appId = await createOAuth2BotApplication(harness, account, appName, ['https://example.com/callback']);
		await createBuilder(harness, account.token).delete(`/oauth2/applications/${appId}`).body({}).expect(403).execute();
		await createBuilder(harness, account.token)
			.delete(`/oauth2/applications/${appId}`)
			.body({
				password: account.password,
			})
			.expect(204)
			.execute();
	});
	it('requires sudo for reset bot token', async () => {
		const account = await createTestAccount(harness);
		const appName = `Test App ${Date.now()}`;
		const appId = await createOAuth2BotApplication(harness, account, appName, ['https://example.com/callback']);
		await createBuilder(harness, account.token)
			.post(`/oauth2/applications/${appId}/bot/reset-token`)
			.body({})
			.expect(403)
			.execute();
		await createBuilder(harness, account.token)
			.post(`/oauth2/applications/${appId}/bot/reset-token`)
			.body({
				password: account.password,
			})
			.expect(200)
			.execute();
		await deleteOAuth2Application(harness, account, appId);
	});
	it('requires sudo for disable TOTP MFA', async () => {
		let account = await createTestAccount(harness);
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
		account = await loginWithTotp(harness, account, secret);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: 'invalid-code',
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: backupCode,
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
			})
			.expect(204)
			.execute();
	});
	it('requires sudo for enable TOTP MFA', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
			})
			.expect(403)
			.execute();
		await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({
				secret,
				code,
				password: account.password,
			})
			.execute();
	});
	it('requires sudo for disable account', async () => {
		const testUser = await createTestAccount(harness);
		await createBuilder(harness, testUser.token).post('/users/@me/disable').body({}).expect(403).execute();
		await createBuilder(harness, testUser.token)
			.post('/users/@me/disable')
			.body({
				password: testUser.password,
			})
			.expect(204)
			.execute();
	});
	it('requires sudo for delete account', async () => {
		const testUser = await createTestAccount(harness);
		await createBuilder(harness, testUser.token).post('/users/@me/delete').body({}).expect(403).execute();
		await createBuilder(harness, testUser.token)
			.post('/users/@me/delete')
			.body({
				password: testUser.password,
			})
			.expect(204)
			.execute();
	});
});
