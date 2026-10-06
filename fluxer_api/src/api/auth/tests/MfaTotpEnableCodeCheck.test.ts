// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, createTotpSecret, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

function wrongCodeFor(code: string): string {
	return ((Number(code) + 500_000) % 1_000_000).toString().padStart(6, '0');
}

describe('Enabling TOTP checks the setup code before sudo', () => {
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
	it('rejects a wrong setup code on the code field without asking for a password', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const error = await createBuilder<ValidationErrorBody>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: wrongCodeFor(totpCodeNow(secret))})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(error.errors[0]?.path).toBe('code');
		expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_CODE);
	});
	it('asks for sudo for a valid setup code, then enables with the password', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const code = totpCodeNow(secret);
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
		const enabled = await createBuilder<{backup_codes: Array<{code: string}>}>(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code, password: account.password})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(enabled.backup_codes.length).toBeGreaterThan(0);
	});
});
