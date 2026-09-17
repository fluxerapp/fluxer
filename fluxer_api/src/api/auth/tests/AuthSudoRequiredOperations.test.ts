// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {createAuthHarness, createTestAccount, loginAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {TotpGenerator} from '@app/api/utils/TotpGenerator';
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

function generateTotpSecret(): string {
	const buffer = randomBytes(20);
	const base32Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
	let result = '';
	for (let i = 0; i < buffer.length; i += 5) {
		const bytes = [buffer[i]!, buffer[i + 1]!, buffer[i + 2]!, buffer[i + 3]!, buffer[i + 4]!];
		const n = (bytes[0]! << 24) | (bytes[1]! << 16) | (bytes[2]! << 8) | bytes[3]!;
		const indices = [
			(n >> 3) & 0x1f,
			((n >> 11) | ((bytes[4]! << 4) & 0xf)) & 0x1f,
			((n >> 19) | ((bytes[4]! << 2) & 0x3c)) & 0x1f,
			(bytes[4]! >> 1) & 0x1f,
		];
		result += base32Chars[indices[0]!];
		result += base32Chars[indices[1]!];
		result += base32Chars[indices[2]!];
		result += base32Chars[indices[3]!];
	}
	return result;
}

async function generateTotpCode(secret: string): Promise<string> {
	const totp = new TotpGenerator(secret);
	const codes = await totp.generateTotp();
	return codes[0]!;
}

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
			code: await generateTotpCode(secret),
		})
		.execute();
	return {...account, token: mfaLoginResp.token};
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
		const secret = generateTotpSecret();
		const code = await generateTotpCode(secret);
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
				mfa_code: await generateTotpCode(secret),
			})
			.expect(204)
			.execute();
	});
	it('requires sudo for enable TOTP MFA', async () => {
		const account = await createTestAccount(harness);
		const secret = generateTotpSecret();
		const code = await generateTotpCode(secret);
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
	it('allows non-sudo operations without sudo', async () => {
		const account = await createTestAccount(harness);
		await loginAccount(harness, account);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, account.token).get('/auth/sessions').expect(200).execute();
	});
});
