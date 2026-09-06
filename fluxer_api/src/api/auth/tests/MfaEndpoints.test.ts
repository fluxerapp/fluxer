// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	type BackupCodesResponse,
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	totpCodeNow,
	type UserMeResponse,
} from './AuthTestUtils';

interface ValidationErrorResponse {
	code: string;
	errors: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

describe('Auth MFA endpoints', () => {
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
	it('handles TOTP enable, backup codes, login, and disable', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const enableData = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: totpCodeNow(secret), password: account.password})
			.execute();
		expect(enableData.backup_codes.length).toBeGreaterThan(0);
		const fetched = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/backup-codes')
			.body({
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
				regenerate: false,
			})
			.execute();
		expect(fetched.backup_codes.length).toBe(enableData.backup_codes.length);
		const login = await createBuilderWithoutAuth<{
			mfa: boolean;
			ticket: string;
			totp: boolean;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect(login.mfa).toBe(true);
		expect(login.ticket).toBeDefined();
		expect(login.totp).toBe(true);
		const backupResp = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({
				code: enableData.backup_codes[0]!.code,
				ticket: login.ticket,
			})
			.execute();
		expect(backupResp.token).toBeDefined();
		const me = await createBuilder<UserMeResponse>(harness, `Bearer ${backupResp.token}`).get('/users/@me').execute();
		expect(me.id).toBe(account.userId);
		account.token = backupResp.token;
		const regenerated = await createBuilder<BackupCodesResponse>(harness, account.token)
			.post('/users/@me/mfa/backup-codes')
			.body({
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
				regenerate: true,
			})
			.execute();
		expect(regenerated.backup_codes.length).toBeGreaterThan(0);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: regenerated.backup_codes[0]!.code,
				mfa_method: 'totp',
				mfa_code: totpCodeNow(secret),
			})
			.expect(204)
			.execute();
	});
	it('expired MFA ticket reports SESSION_TIMEOUT on ticket for both completion routes', async () => {
		const totpError = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/login/mfa/totp')
			.body({code: '000000', ticket: 'unknown'})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(totpError.errors[0]?.path).toBe('ticket');
		expect(totpError.errors[0]?.code).toBe(ValidationErrorCodes.SESSION_TIMEOUT);
		const webauthnError = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/login/mfa/webauthn')
			.body({
				response: {
					id: 'unknown',
					rawId: 'unknown',
					type: 'public-key',
					response: {clientDataJSON: '', authenticatorData: '', signature: ''},
					clientExtensionResults: {},
				},
				challenge: 'unknown',
				ticket: 'unknown',
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		expect(webauthnError.errors[0]?.path).toBe('ticket');
		expect(webauthnError.errors[0]?.code).toBe(ValidationErrorCodes.SESSION_TIMEOUT);
	});
});
