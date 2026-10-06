// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	generateTotpCode,
	seedMfaTicket,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createWebAuthnDevice,
	registerWebAuthnCredential,
	setWebAuthnTwoFactor,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth MFA TOTP without secret', () => {
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
	it('rejects TOTP login when secret is missing', async () => {
		const account = await createTestAccount(harness);
		const ticket = 'mfa-no-secret';
		await seedMfaTicket(harness, ticket, account.userId, 300);
		const login = await createBuilderWithoutAuth<{
			code: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket, code: '123456'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(login.code).toBe('INVALID_FORM_BODY');
	});
	it('rejects TOTP login when passkey two-factor is on and no TOTP secret remains', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		const totpData = await createBuilder<{
			backup_codes: Array<{
				code: string;
			}>;
		}>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		await setWebAuthnTwoFactor(harness, account.token, true, {
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		});
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: totpData.backup_codes[0]!.code,
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const login = await createBuilderWithoutAuth<{
			mfa: true;
			ticket: string;
			allowed_methods: Array<string>;
			totp: boolean;
			webauthn: boolean;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect(login.mfa).toBe(true);
		expect(login.totp).toBe(false);
		expect(login.webauthn).toBe(true);
		expect(login.allowed_methods).toContain('webauthn');
		expect(login.allowed_methods).not.toContain('totp');
		const bypassAttempt = await createBuilderWithoutAuth<{
			code: string;
		}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket: login.ticket, code: '123456'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(bypassAttempt.code).toBe('INVALID_FORM_BODY');
	});
	it('issues a session token when passkey two-factor is off and no TOTP secret remains', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const secret = createTotpSecret();
		const totpData = await createBuilder<{
			backup_codes: Array<{
				code: string;
			}>;
		}>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.execute();
		await registerWebAuthnCredential(harness, account.token, device, () => ({
			mfa_method: 'totp',
			mfa_code: generateTotpCode(secret),
		}));
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({
				code: totpData.backup_codes[0]!.code,
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
		const login = await createBuilderWithoutAuth<{
			mfa?: true;
			token: string;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect(login.mfa).toBeUndefined();
		expect(login.token).toBeTruthy();
	});
});
