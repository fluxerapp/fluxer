// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createSudoWebAuthnBody,
	createWebAuthnDevice,
	registerWebAuthnCredential,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SudoMfaMethodsResponse {
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
	has_mfa: boolean;
}

describe('Sudo mode for passwordless accounts holding a passkey', () => {
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
	it('still waves through a passwordless account that holds no credential at all', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	it('challenges a passwordless account that holds a passkey it never made a second factor', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await unclaimAccount(harness, account.userId);
		const methods = await createBuilder<SudoMfaMethodsResponse>(harness, account.token)
			.get('/users/@me/sudo/mfa-methods')
			.execute();
		expect(methods).toEqual({totp: false, webauthn: true, backup_codes: false, has_mfa: true});
		const errorResp = await createBuilder<{
			code: string;
		}>(harness, account.token)
			.post('/users/@me/disable')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		expect(errorResp.code).toBe('SUDO_MODE_REQUIRED');
	});
	it('lets the passwordless passkey holder clear the challenge with an assertion', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		await registerWebAuthnCredential(harness, account.token, device, () => ({password: account.password}));
		await unclaimAccount(harness, account.userId);
		const sudoBody = await createSudoWebAuthnBody(harness, account.token, device);
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body(sudoBody)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
});
