// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, createTotpSecret, generateTotpCode, seedMfaTicket} from './AuthTestUtils';
import {createRegistrationResponse, createWebAuthnDevice, type WebAuthnRegistrationOptions} from './WebAuthnTestUtils';
import type { AuthMfaMethod } from '@fluxer/schema/src/domains/auth/AuthSchemas';

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
	it('rejects TOTP login when only WebAuthn is enabled', async () => {
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
		const regOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
			.execute();
		if (regOptions.rp.id) {
			device.rpId = regOptions.rp.id;
		}
		const registrationResponse = createRegistrationResponse(device, regOptions, 'Test Passkey');
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: registrationResponse,
				challenge: regOptions.challenge,
				name: 'Test Passkey',
				mfa_method: 'totp',
				mfa_code: generateTotpCode(secret),
			})
			.expect(204)
			.execute();
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
			allowed_methods: Array<AuthMfaMethod>;
		}>(harness)
			.post('/auth/login')
			.body({email: account.email, password: account.password})
			.execute();
		expect(login.mfa).toBe(true);
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
});
