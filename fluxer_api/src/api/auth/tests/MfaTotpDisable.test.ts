// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BackupCodesResponse,
	createAuthHarness,
	createTestAccount,
	createTotpSecret,
	type TestAccount,
	totpCodeNow,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {Config} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const SUDO_MODE_HEADER = 'X-Fluxer-Sudo-Mode-JWT';

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

async function withTotpReplayProtection<T>(run: () => Promise<T>): Promise<T> {
	const previous = Config.dev.testModeEnabled;
	Config.dev.testModeEnabled = false;
	try {
		return await run();
	} finally {
		Config.dev.testModeEnabled = previous;
	}
}

function wrongCodeFor(code: string): string {
	return ((Number(code) + 500_000) % 1_000_000).toString().padStart(6, '0');
}

async function enableTotp(harness: ApiTestHarness, account: TestAccount, secret: string): Promise<Array<string>> {
	const response = await createBuilder<BackupCodesResponse>(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret, code: totpCodeNow(secret), password: account.password})
		.execute();
	return response.backup_codes.map((backupCode) => backupCode.code);
}

async function loginRequiresMfa(harness: ApiTestHarness, account: TestAccount): Promise<boolean> {
	const login = await createBuilderWithoutAuth<{mfa?: boolean}>(harness)
		.post('/auth/login')
		.body({email: account.email, password: account.password})
		.execute();
	return login.mfa === true;
}

describe('MFA TOTP disable', () => {
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

	it('disables when the sudo fields repeat the authenticator code', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			const code = totpCodeNow(secret);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code, mfa_method: 'totp', mfa_code: code})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(await loginRequiresMfa(harness, account)).toBe(false);
	});

	it('proves sudo mode with the sudo fields and skips code when mfa_method is set', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			const code = totpCodeNow(secret);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code: wrongCodeFor(code), mfa_method: 'totp', mfa_code: code})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(await loginRequiresMfa(harness, account)).toBe(false);
	});

	it('disables with an authenticator code alone', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code: totpCodeNow(secret)})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(await loginRequiresMfa(harness, account)).toBe(false);
	});

	it('disables with a backup code alone', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const backupCodes = await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code: backupCodes[0]!})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(await loginRequiresMfa(harness, account)).toBe(false);
	});

	it('rejects a wrong code without a sudo token and keeps TOTP enabled', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			const error = await createBuilder<ValidationErrorBody>(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code: wrongCodeFor(totpCodeNow(secret))})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors).toEqual([
				{path: 'mfa_code', code: ValidationErrorCodes.INVALID_MFA_CODE, message: expect.any(String)},
			]);
		});
		expect(await loginRequiresMfa(harness, account)).toBe(true);
	});

	it('rejects an authenticator code already spent on another sudo proof', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			const code = totpCodeNow(secret);
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes')
				.body({regenerate: false, mfa_method: 'totp', mfa_code: code})
				.expect(HTTP_STATUS.OK)
				.execute();
			const error = await createBuilder<ValidationErrorBody>(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.body({code})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors[0]?.path).toBe('mfa_code');
			expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_MFA_CODE);
		});
		expect(await loginRequiresMfa(harness, account)).toBe(true);
	});

	it('checks code when a sudo token proves sudo mode', async () => {
		const account = await createTestAccount(harness);
		const secret = createTotpSecret();
		const backupCodes = await enableTotp(harness, account, secret);
		await withTotpReplayProtection(async () => {
			const code = totpCodeNow(secret);
			const {response} = await createBuilder(harness, account.token)
				.post('/users/@me/mfa/backup-codes')
				.body({regenerate: false, mfa_method: 'totp', mfa_code: code})
				.expect(HTTP_STATUS.OK)
				.executeWithResponse();
			const sudoToken = response.headers.get(SUDO_MODE_HEADER);
			expect(sudoToken).toBeTruthy();
			for (const rejected of [wrongCodeFor(code), code]) {
				const error = await createBuilder<ValidationErrorBody>(harness, account.token)
					.post('/users/@me/mfa/totp/disable')
					.header(SUDO_MODE_HEADER, sudoToken!)
					.body({code: rejected})
					.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
					.execute();
				expect(error.errors[0]?.path).toBe('code');
				expect(error.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_CODE);
			}
			await createBuilder(harness, account.token)
				.post('/users/@me/mfa/totp/disable')
				.header(SUDO_MODE_HEADER, sudoToken!)
				.body({code: backupCodes[0]!})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		expect(await loginRequiresMfa(harness, account)).toBe(false);
	});

	it('still requires sudo mode when the account has no TOTP secret', async () => {
		const account = await createTestAccount(harness);
		const device = createWebAuthnDevice();
		const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials/registration-options')
			.body({password: account.password})
			.execute();
		if (registrationOptions.rp.id) {
			device.rpId = registrationOptions.rp.id;
		}
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/webauthn/credentials')
			.body({
				response: createRegistrationResponse(device, registrationOptions, 'Passkey'),
				challenge: registrationOptions.challenge,
				name: 'Passkey',
				password: account.password,
			})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/disable')
			.body({code: '123456'})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
	});
});
